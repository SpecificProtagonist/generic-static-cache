#![feature(asm_const)]
#![feature(const_type_id)]
#![allow(named_asm_labels)]

//! Quoting the [Rust Reference](https://doc.rust-lang.org/reference/items/static-items.html):
//!
//! "A static item defined in a generic scope (for example in a blanket or default implementation)
//! will result in exactly one static item being defined, as if the static definition was pulled
//! out of the current scope into the module. There will not be one item per monomorphization."
//!
//! One way to work around this is to use a `HashMap<TypeId,Data>`. This is a simple & usually appropriate solution.
//! If lookup performance is important, you can skip hashing the `TypeId` for minor gains as it [already contains](https://github.com/rust-lang/rust/blob/eeff92ad32c2627876112ccfe812e19d38494087/library/core/src/any.rs#L645) a good-quality hash. This is implemented in `TypeIdMap`.
//!
//! This crate aims to further fully remove the lookup by allocating the storage using inline
//! assembly. Currently only amd64 is supported. Unless you only target amd64, you need to
//! fall back to a hashmap on other platforms. Additionally, different compilation units
//! may access different instances of the data.
//!
//! This crate requires the following unstable features: `asm_const`, `const_type_id`
//!
//! # Example
//! ```
//! # #[derive(Debug)]
//! ##[derive(Copy, Clone, Eq, PartialEq)]
//! struct Metadata(&'static str);
//!
//! struct Cat;
//! struct Bomb;
//!
//! generic_static_cache::init::<Cat, _>(Metadata("nya!")).unwrap();
//! generic_static_cache::init::<Bomb, _>(Metadata("boom!")).unwrap();
//!
//! assert_eq!(generic_static_cache::get::<Cat, _>(), Some(Metadata("nya!")));
//! assert_eq!(generic_static_cache::get::<Bomb, _>(), Some(Metadata("boom!")));
//! ```

use std::any::TypeId;
use std::arch::asm;
use std::collections::HashMap;
use std::hash::{BuildHasherDefault, Hasher};
use std::mem::{align_of, size_of};
use std::sync::atomic::{AtomicPtr, Ordering};

use bytemuck::Zeroable;

#[derive(Debug)]
pub struct AlreadyInitialized;

/// Wrapper to prevent interfering with the user's `direct` calls
struct Heap<T>(AtomicPtr<T>);

unsafe impl<T> Zeroable for Heap<T> {}

/// Initialize the `Data`-storage of type `Type`. If called multiple times, only the
/// first call will succeed.
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

#[cfg(target_arch = "x86_64")]
pub fn get<Type: 'static, Data: Copy + 'static>() -> Option<Data> {
    let data = direct::<Type, Heap<Data>>().0.load(Ordering::SeqCst);
    if data.is_null() {
        None
    } else {
        Some(unsafe { *data })
    }
}

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
            ".align {align}",
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
pub type TypeIdMap<T> = HashMap<TypeId, T, BuildHasherDefault<NoOpTypeIdHasher>>;

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
}
