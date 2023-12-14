#![feature(asm_const)]
#![feature(const_type_id)]
#![allow(named_asm_labels)]
// TODO: Properly test data for same type from different compilation units
// TODO: More platforms
// TODO: Benches

//! Quoting the [Rust Reference](https://doc.rust-lang.org/reference/items/static-items.html):
//!
//! "A static item defined in a generic scope (for example in a blanket or default implementation)
//! will result in exactly one static item being defined, as if the static definition was pulled
//! out of the current scope into the module. There will not be one item per monomorphization."
//!
//! One way to work around this is to use a `HashMap<TypeId,Data>`. This is a simple & usually the best solution.
//! If lookup performance is important, you can skip hashing the `TypeId` for minor gains as it [already contains](https://github.com/rust-lang/rust/blob/eeff92ad32c2627876112ccfe812e19d38494087/library/core/src/any.rs#L645) a good-quality hash. This is implemented in `TypeIdMap`.
//!
//! This crate aims to further fully remove the lookup by allocating the storage using inline
//! assembly.
//!
//! Currently only *x86-64* is supported! Unless you only target x86-64, you need to
//! fall back to a hashmap on other platforms. Additionally, different compilation units
//! may access different instances of the data.
//!
//! This crate requires the following unstable features: `asm_const`, `const_type_id`, `const_collections_with_hasher`
//!
//! # Examples
//! Static variables in a generic context:
//! ```
//! #![feature(const_collections_with_hasher)]
//! # use std::sync::atomic::{AtomicI32, Ordering};
//! # use generic_static_cache::generic_static;
//! fn get_and_inc<T>() -> i32 {
//!     generic_static!{
//!         static blub: &AtomicI32 = &AtomicI32::new(1);
//!     }
//!     let value = blub.load(Ordering::Relaxed);
//!     blub.fetch_add(1, Ordering::Relaxed);
//!     value
//! }
//! assert_eq!(get_and_inc::<bool>(), 1);
//! assert_eq!(get_and_inc::<bool>(), 2);
//! assert_eq!(get_and_inc::<String>(), 1);
//! assert_eq!(get_and_inc::<bool>(), 3);
//! ```
//! To support all platforms (keeping the performance benefits on supported platforms), change the above to
//! ```
//! # use std::sync::atomic::AtomicI32;
//! # use generic_static_cache::fallback_generic_static; /*
//! ...
//! # */ struct T;
//! fallback_generic_static!{
//!     T => static blub: &AtomicI32 = &AtomicI32::new(1);
//! }
//! # /*
//! ...
//! # */
//! ```
//! Associating data with a type:
//! ```
//! # #[derive(Debug)]
//! ##[derive(Copy, Clone, Eq, PartialEq)]
//! struct Metadata(&'static str);
//!
//! struct Cat;
//! struct Bomb;
//!
//! use generic_static_cache::{get, init};
//! init::<Cat, _>(Metadata("nya!")).unwrap();
//! init::<Bomb, _>(Metadata("boom!")).unwrap();
//!
//! assert_eq!(get::<Cat, _>(), Some(Metadata("nya!")));
//! assert_eq!(get::<Bomb, _>(), Some(Metadata("boom!")));
//! ```

use std::any::TypeId;
use std::arch::asm;
use std::collections::HashMap;
use std::hash::{BuildHasher, Hasher};
use std::mem::{align_of, size_of};
use std::sync::atomic::{AtomicPtr, Ordering};

use bytemuck::Zeroable;

#[derive(Debug)]
pub struct AlreadyInitialized;

/// Wrapper to prevent interfering with the user's `direct` calls
struct Heap<T>(AtomicPtr<T>);

unsafe impl<T> Zeroable for Heap<T> {}

/// Initialize the `Data`-storage of type `Type`.
/// Each `Type` can hold data for multiple different instantiations of `Data`.
///
/// If called multiple times, only the first call will succeed.
#[cfg(target_arch = "x86_64")]
pub fn init<Type: 'static, Data: Copy + 'static>(data: Data) -> Result<(), AlreadyInitialized> {
    let boxed = Box::into_raw(Box::new(data));
    match direct::<Type, Heap<Data>>().0.compare_exchange(
        std::ptr::null_mut(),
        boxed,
        Ordering::SeqCst,
        Ordering::SeqCst,
    ) {
        Ok(_) => Ok(()),
        Err(_) => {
            unsafe {
                drop(Box::from_raw(boxed));
            }
            Err(AlreadyInitialized)
        }
    }
}

/// Access the `Data`-storage of type `Type`.
/// Each `Type` can hold data for multiple different instantiations of `Data`.
#[cfg(target_arch = "x86_64")]
pub fn get<Type: 'static, Data: Copy + 'static>() -> Option<Data> {
    let data = direct::<Type, Heap<Data>>().0.load(Ordering::SeqCst);
    if data.is_null() {
        None
    } else {
        Some(unsafe { *data })
    }
}

/// Initialize & access the `Data`-storage of type `Type`.
/// Each `Type` can hold data for multiple different instantiations of `Data`.
#[cfg(target_arch = "x86_64")]
pub fn get_or_init<Type: 'static, Data: Copy + 'static>(cons: impl Fn() -> Data) -> Data {
    let data = direct::<Type, Heap<Data>>().0.load(Ordering::SeqCst);
    if data.is_null() {
        let _ = init::<Type, _>(cons());
        get::<Type, _>().unwrap()
    } else {
        unsafe { *data }
    }
}

/// Declare a static variable that is not shared across different monomorphizations
/// of the containing functions. Its type must be a shared reference to a type
/// that implements Sync.
///
/// Only available on supported targets; to support all platforms use [`fallback_generic_static`].
/// # Example
/// ```
/// #![feature(const_collections_with_hasher)]
/// # use std::sync::atomic::Ordering;
/// # use std::sync::Mutex;
/// # use generic_static_cache::generic_static;
/// generic_static!{
///     static name: &Mutex<String> = &Mutex::new("Ferris".to_string());
/// }
/// ```
#[cfg(target_arch = "x86_64")]
#[macro_export]
macro_rules! generic_static {
    {static $ident:ident : &$type:ty = &$init:expr;} => {
        let $ident: &'static $type = {
            fn assert_sync<T: Sync>() {}
            assert_sync::<$type>();
            trait Make<T>: Sized + 'static {
                fn make(self) -> &'static std::sync::atomic::AtomicPtr<T> {
                    $crate::direct::<Self, std::sync::atomic::AtomicPtr<T>>()
                }
            }
            impl<T: 'static, S> Make<S> for T {}
            let ptr = Make::<$type>::make(|| ());
            let data = ptr.load(std::sync::atomic::Ordering::SeqCst);
            if data.is_null() {
                let value: $type = (|| $init)();
                let boxed = Box::into_raw(Box::new(value));
                if ptr
                    .compare_exchange(
                        std::ptr::null_mut(),
                        boxed,
                        std::sync::atomic::Ordering::SeqCst,
                        std::sync::atomic::Ordering::SeqCst,
                    )
                    .is_err()
                {
                    unsafe {
                        drop(Box::from_raw(boxed));
                    }
                }
                unsafe { &*boxed }
            } else {
                unsafe { &*data }
            }
        };
    };
}

/// Declare a static variable, keyed by a type.
/// Its type must be a shared reference to a type that implements Sync.
///
/// Available on all targets (falls back to a fast HashMap on non-supported platforms).
/// # Example
/// ```
/// #![feature(const_collections_with_hasher)]
/// # use std::sync::atomic::Ordering;
/// # use std::sync::Mutex;
/// # use generic_static_cache::fallback_generic_static;
/// # struct T;
/// fallback_generic_static!(
///     T => static name: &Mutex<String> = &Mutex::new("Ferris".to_string());
/// );
/// ```
#[macro_export]
macro_rules! fallback_generic_static {
    {$key:ty => static $ident:ident : &$type:ty = &$init:expr;} => {
        let $ident: &'static $type = {
            #[cfg(target_arch = "x86_64")]
            {
                fn inner<T: 'static>(init: impl FnOnce() -> $type) -> &'static $type {
                    $crate::generic_static! {
                        static inner: &$type = &init();
                    }
                    inner
                }
                inner::<T>(||$init)
            }
            #[cfg(not(target_arch = "x86_64"))]
            {
                static MAP: std::sync::Mutex<$crate::TypeIdMap<&'static $type>> =
                    std::sync::Mutex::new($crate::TypeIdMap::<&'static $type>::with_hasher(
                        $crate::NoOpTypeIdBuildHasher,
                    ));
                MAP.lock()
                    .unwrap()
                    .entry(std::any::TypeId::of::<$key>())
                    .or_insert_with(|| Box::leak(Box::new((|| $init)())))
            }
        };
    };
}

/// Access data associated with Type without indirection. Requires interior mutability to be useful.
/// This data is independent of the data accessed via [`get`]/[`init`]/[`get_or_init`].
#[cfg(target_arch = "x86_64")]
pub fn direct<Type: 'static, Data: Zeroable + 'static>() -> &'static Data {
    // Work around "can't use generic parameter from outer function"
    trait Id: 'static {
        const ID: u128 = unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) };
    }
    impl<S: 'static> Id for S {}

    unsafe {
        let addr: usize;
        asm!(
            // Create static storage
            ".pushsection .data",
            ".balign {align}",
            "type_data_{id}:",
            ".skip {size}",
            ".popsection",
            // Position-independent address
            "lea {addr}, [rip+type_data_{id}]",
            addr = out(reg) addr,
            id = const <(Type, Data) as Id>::ID,
            align = const align_of::<Data>(),
            size = const size_of::<Data>(),
            options(pure, nomem)
        );
        &*(addr as *const _)
    }
}

/// Fast type map suitable for all platforms.
pub type TypeIdMap<T> = HashMap<TypeId, T, NoOpTypeIdBuildHasher>;

#[doc(hidden)]
#[derive(Default)]
pub struct NoOpTypeIdBuildHasher;

impl BuildHasher for NoOpTypeIdBuildHasher {
    type Hasher = NoOpTypeIdHasher;

    fn build_hasher(&self) -> Self::Hasher {
        NoOpTypeIdHasher(0)
    }
}

#[doc(hidden)]
#[derive(Default)]
pub struct NoOpTypeIdHasher(u64);

impl Hasher for NoOpTypeIdHasher {
    fn finish(&self) -> u64 {
        self.0
    }

    fn write(&mut self, _bytes: &[u8]) {
        unreachable!()
    }

    fn write_u64(&mut self, i: u64) {
        self.0 = i
    }
}

#[cfg(test)]
#[test]
fn test_heapless() {
    use crate::direct;
    use std::sync::atomic::{AtomicI64, Ordering};

    let a = direct::<Option<bool>, AtomicI64>();
    let b = direct::<Option<()>, AtomicI64>();
    assert_eq!(a.load(Ordering::Relaxed), 0);
    a.store(69, Ordering::Relaxed);
    assert_eq!(a.load(Ordering::Relaxed), 69);
    assert_eq!(b.load(Ordering::Relaxed), 0);
    assert_eq!(*direct::<Option<()>, i64>(), 0);

    // Check no duplicate symbol errors
    std::hint::black_box(direct::<Option<bool>, AtomicI64>());
}

#[cfg(test)]
#[test]
fn test_fallback_macro() {
    use std::sync::atomic::{AtomicI32, Ordering};
    fn get_and_inc<T: 'static>() -> i32 {
        fallback_generic_static!(
            T => static blub: &AtomicI32 = &AtomicI32::new(1);
        );
        let value = blub.load(Ordering::Relaxed);
        blub.fetch_add(1, Ordering::Relaxed);
        value
    }
    assert_eq!(get_and_inc::<bool>(), 1);
    assert_eq!(get_and_inc::<bool>(), 2);
    assert_eq!(get_and_inc::<String>(), 1);
    assert_eq!(get_and_inc::<bool>(), 3);
}
