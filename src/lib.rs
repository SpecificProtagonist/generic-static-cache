#![feature(asm_const)]
#![cfg_attr(
    not(any(target_arch = "x86_64", target_arch = "aarch64")),
    feature(const_collections_with_hasher)
)]
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
//! If lookup performance is important, you can skip hashing the `TypeId` for minor gains as it
//! [already contains](https://github.com/rust-lang/rust/blob/eeff92ad32c2627876112ccfe812e19d38494087/library/core/src/any.rs#L645)
//! a good-quality hash. This is implemented in `TypeIdMap`.
//!
//! This crate aims to further fully remove the lookup by allocating the storage using inline
//! assembly.
//!
//! Supported targets are **x86-64** and **aarch64**. On other targets, the `generic_static` macro
//! falls back to a hashmap and most other functionality is unavailable.
//!
//! Additionally, different compilation units may access different instances of the data!
//!
//! This crate requires the following unstable features: `asm_const` and
//! (on unsupported targets) `const_collections_with_hasher`.
//!
//! # Examples
//! Static variables in a generic context:
//! ```
//! #![feature(const_collections_with_hasher)]
//! # use std::sync::atomic::{AtomicI32, Ordering};
//! # use generic_static_cache::generic_static;
//! fn get_and_inc<T>() -> i32 {
//!     generic_static!{
//!         static COUNTER: &AtomicI32 = &AtomicI32::new(1);
//!     }
//!     COUNTER.fetch_add(1, Ordering::Relaxed)
//! }
//! assert_eq!(get_and_inc::<bool>(), 1);
//! assert_eq!(get_and_inc::<bool>(), 2);
//! assert_eq!(get_and_inc::<String>(), 1);
//! assert_eq!(get_and_inc::<bool>(), 3);
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
use std::collections::HashMap;
use std::hash::{BuildHasher, Hasher};
use std::sync::atomic::AtomicPtr;

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
///
/// Only available on supported targets.
#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
pub fn init<Type: 'static, Data: Copy + 'static>(data: Data) -> Result<(), AlreadyInitialized> {
    use std::sync::atomic::Ordering;
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
///
/// Only available on supported targets.
#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
pub fn get<Type: 'static, Data: Copy + 'static>() -> Option<Data> {
    use std::sync::atomic::Ordering;
    let data = direct::<Type, Heap<Data>>().0.load(Ordering::SeqCst);
    if data.is_null() {
        None
    } else {
        Some(unsafe { *data })
    }
}

/// Initialize & access the `Data`-storage of type `Type`.
/// Each `Type` can hold data for multiple different instantiations of `Data`.
///
/// Only available on supported targets.
#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
pub fn get_or_init<Type: 'static, Data: Copy + 'static>(cons: impl Fn() -> Data) -> Data {
    use std::sync::atomic::Ordering;
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
/// If this is executed for the first time in multiple threads simultaneously,
/// the initializing expression may get executed multiple times.
///
/// On unsupported targets, this falls back to a hashmap and generic types
/// from outer items may not be used.
///
/// On supported targets, the type annotation can be ommited.
///
/// # Example
/// ```
/// # use std::sync::Mutex;
/// # use generic_static_cache::generic_static;
/// generic_static!{
///     static NAME: &Mutex<String> = &Mutex::new("Ferris".to_string());
/// }
/// ```
#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
#[macro_export]
macro_rules! generic_static {
    {static $ident:ident $(: &$type:ty)? = &$init:expr;} => {
        #[allow(non_snake_case)]
        let $ident $(: &'static $type)? = {
            let init = ||$init;
            fn assert_sync<T: Sync>(_: &impl FnOnce() -> T) {}
            assert_sync(&init);

            // Use use empty closure to create a new type to use as a unique key,
            // use reference to initializer to infer type of static data
            fn make<Key: 'static, Value>(_: Key, _: &impl FnOnce()->Value)
            -> &'static std::sync::atomic::AtomicPtr<Value> {
                $crate::direct::<Key, std::sync::atomic::AtomicPtr<Value>>()
            }
            let ptr = make(||(), &init);

            let data = ptr.load(std::sync::atomic::Ordering::SeqCst);
            if data.is_null() {
                // Need to call initializer
                // This can be called multiple times if executed for the first time
                // in multiple threads simultaneously!
                let boxed = Box::into_raw(Box::new(init()));
                if ptr
                    .compare_exchange(
                        std::ptr::null_mut(),
                        boxed,
                        std::sync::atomic::Ordering::SeqCst,
                        std::sync::atomic::Ordering::SeqCst,
                    )
                    .is_err()
                {
                    // Was simultaneously initialized by another thread
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

/// Declare a static variable that is not shared across different monomorphizations
/// of the containing functions. Its type must be a shared reference to a type
/// that implements Sync.
///
/// If this is executed for the first time in multiple threads simultaneously,
/// the initializing expression may get executed multiple times.
///
/// On unsupported targets, this falls back to a hashmap.
///
/// On supported targets, the type annotation can be ommited.
///
/// # Example
/// ```
/// # use std::sync::Mutex;
/// # use generic_static_cache::generic_static;
/// generic_static!{
///     static NAME: &Mutex<String> = &Mutex::new("Ferris".to_string());
/// }
/// ```
#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
#[macro_export]
macro_rules! generic_static {
    {static $ident:ident: &$type:ty = &$init:expr;} => {
        #[allow(non_snake_case)]
        let $ident: &'static $type = {
            static MAP: std::sync::Mutex<$crate::TypeIdMap<&'static $type>> =
                std::sync::Mutex::new($crate::TypeIdMap::with_hasher(
                    $crate::NoOpTypeIdBuildHasher,
                ));
            MAP.lock()
                .unwrap()
                .entry({
                    fn id<T: 'static>(_: T) -> std::any::TypeId {
                        std::any::TypeId::of::<T>()
                    }
                    id(||())
                })
                .or_insert_with(|| Box::leak(Box::new((|| $init)())))
        };
    };
}

trait Carrier: 'static {
    unsafe fn storage(&self) -> TypeId;
}

impl<T: 'static, D: Zeroable + Sync + 'static> Carrier for (T, D) {
    /// THIS PLACE IS NOT A PLACE OF HONOR.  
    /// NO HIGHLY ESTEEMED DEED IS COMMEMORATED HERE.  
    /// NOTHING VALUED IS HERE.  
    /// WHAT IS HERE WAS DANGEROUS AND REPULSIVE TO US.  
    /// THE DANGER IS IN A PARTICULAR LOCATION.  
    /// THE DANGER IS STILL PRESENT, IN YOUR TIME, AS IT WAS IN OURS.  
    ///
    /// The useage of sym seems to prevent rustc from optimizing this out
    /// in release mode, even though it's never called.
    /// The inline is does the same, but for debug mode.
    ///
    /// Why? *shrug* Only 𒀭𒂗𒆠, the god of knowledge, knows
    /// and even he needs a minute to figure this out.
    #[inline(always)]
    unsafe fn storage(&self) -> TypeId {
        std::arch::asm!(
            ".pushsection .data",
            // TODO: Check at what align the .data section gets loaded,
            // ensure align is at most that large.
            // I believe llvm uses 4kb by default (probably more if larger pages are enabled),
            // but users could mess with the linker script, so add a note to the README?
            ".balign {align}",
            "type_data_{id}:",
            ".skip {size}",
            ".popsection",
            id = sym <(T, D) as Carrier>::storage,
            align = const std::mem::align_of::<D>(),
            size = const std::mem::size_of::<D>(),
            options(nomem)
        );
        TypeId::of::<(T, D)>()
    }
}

/// Access data associated with Type without indirection. Requires interior mutability to be useful.
/// This data is independent of the data accessed via [`get`]/[`init`]/[`get_or_init`].
///
/// Only available on supported targets.
#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
pub fn direct<Type: 'static, Data: Zeroable + Sync + 'static>() -> &'static Data {
    unsafe {
        // Tested both with position-independent code and with -C relocation-model=static
        let addr: usize;
        #[cfg(target_arch = "x86_64")]
        {
            std::arch::asm!(
                "lea {addr}, [rip+type_data_{id}]",
                addr = out(reg) addr,
                id = sym <(Type, Data) as Carrier>::storage,
                options(pure, nomem)
            );
        }
        #[cfg(target_arch = "aarch64")]
        {
            atd::arch::asm!(
                "adrp {addr}, type_data_{id}",
                "add {addr}, {addr}, :lo12:type_data_{id}",
                addr = out(reg) addr,
                id = sym <(Type, Data) as Carrier>::storage,
                options(pure, nomem)
            );
        }
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
        unimplemented!()
    }

    fn write_u64(&mut self, i: u64) {
        self.0 = i
    }
}

#[cfg(test)]
#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
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

    std::hint::black_box(direct::<Option<bool>, AtomicI64>());
}

#[cfg(test)]
#[test]
fn test_macro() {
    use std::sync::atomic::{AtomicI32, Ordering};
    fn get_and_inc<T: 'static>() -> i32 {
        generic_static!(
            static BLUB: &AtomicI32 = &AtomicI32::new(1);
        );
        let value = BLUB.load(Ordering::Relaxed);
        BLUB.fetch_add(1, Ordering::Relaxed);
        value
    }
    assert_eq!(get_and_inc::<bool>(), 1);
    assert_eq!(get_and_inc::<bool>(), 2);
    assert_eq!(get_and_inc::<String>(), 1);
    assert_eq!(get_and_inc::<bool>(), 3);

    generic_static!(
        static FOO_1: &AtomicI32 = &AtomicI32::new(0);
    );
    generic_static!(
        static FOO_2: &AtomicI32 = &AtomicI32::new(69);
    );
    assert_eq!(FOO_1.load(Ordering::Relaxed), 0);
    assert_eq!(FOO_2.load(Ordering::Relaxed), 69);
    FOO_1.store(1, Ordering::Relaxed);
    FOO_2.store(2, Ordering::Relaxed);
    assert_eq!(FOO_1.load(Ordering::Relaxed), 1);
    assert_eq!(FOO_2.load(Ordering::Relaxed), 2);
}

#[cfg(test)]
#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
#[test]
fn test_macro_type_inference() {
    generic_static! {
        static _FOO = &();
    }
}
