#![feature(asm_const)]
#![feature(const_type_id)]
#![allow(named_asm_labels)]

/// Access data associated with Type, initialized to 0.
///
/// Multiple calls for the same type can return references to different instances
/// of said data if they originate in distinct compilation units.
#[cfg(target_arch = "x86_64")]
pub fn type_data<Type: 'static, Data: bytemuck::Zeroable + 'static>() -> &'static Data {
    use std::any::TypeId;
    use std::arch::asm;
    use std::mem::{align_of, size_of, transmute};

    // Work around "can't use generic parameter from outer function"
    trait Id: 'static {
        const ID: u128 = unsafe { transmute(TypeId::of::<Self>()) };
    }
    impl<S: 'static> Id for S {}

    unsafe {
        let addr: usize;
        asm!(
            // Create static storage
            ".pushsection .data",
            ".align {align}",
            "type_data_{type_id}_{data_id}:",
            ".skip {size}",
            ".popsection",
            // Position-independent address
            "lea {addr}, [rip+type_data_{type_id}_{data_id}]",
            addr = out(reg) addr,
            type_id = const <Type as Id>::ID,
            data_id = const <Data as Id>::ID,
            align = const align_of::<Data>(),
            size = const size_of::<Data>(),
            options(pure, nomem)
        );
        &*(addr as *const _)
    }
}

#[cfg(test)]
#[test]
fn test_primitive() {
    use crate::type_data;
    use std::sync::atomic::{AtomicI64, Ordering};

    let a = type_data::<Option<bool>, AtomicI64>();
    let b = type_data::<Option<()>, AtomicI64>();
    assert_eq!(a.load(Ordering::Relaxed), 0);
    a.store(69, Ordering::Relaxed);
    assert_eq!(a.load(Ordering::Relaxed), 69);
    assert_eq!(b.load(Ordering::Relaxed), 0);
    assert_eq!(*type_data::<Option<()>, i64>(), 0);
}
