⚠ Nightly ⚠

Quoting the [Rust Reference](https://doc.rust-lang.org/reference/items/static-items.html):

"A static item defined in a generic scope (for example in a blanket or default implementation)
will result in exactly one static item being defined, as if the static definition was pulled
out of the current scope into the module. There will not be one item per monomorphization."

One way to work around this is to use a `HashMap<TypeId,Data>`. This is a simple & usually appropriate solution.
If lookup performance is important, you can skip hashing the `TypeId` for minor gains as it [already contains](https://github.com/rust-lang/rust/blob/eeff92ad32c2627876112ccfe812e19d38494087/library/core/src/any.rs#L645) a good-quality hash. This is implemented in `TypeIdMap`.

This crate aims to further fully remove the lookup by allocating the storage using inline
assembly.

Currently only *x86-64* and **aarch64** are supported! Unless you only target this, you need to
fall back to a hashmap on other platforms. Additionally, different compilation units
may access different instances of the data!

This crate requires the following unstable features: `asm_const`, `const_type_id`, `const_collections_with_hasher`

# Examples
Static variables in a generic context:
```rust
#![feature(const_collections_with_hasher)]
fn get_and_inc<T>() -> i32 {
    generic_static!{
        static BLUB = &AtomicI32::new(1);
    }
    BLUB.fetch_add(1, Ordering::Relaxed)
}
assert_eq!(get_and_inc::<bool>(), 1);
assert_eq!(get_and_inc::<bool>(), 2);
assert_eq!(get_and_inc::<String>(), 1);
assert_eq!(get_and_inc::<bool>(), 3);
```
To support all platforms (keeping the performance benefits on supported platforms), change the above to
```rust
...
fallback_generic_static!{
    T => static BLUB = &AtomicI32::new(1);
}
...
```
Associating data with a type:
```rust
#[derive(Copy, Clone, Eq, PartialEq)]
struct Metadata(&'static str);

struct Cat;
struct Bomb;

use generic_static_cache::{get, init};
init::<Cat, _>(Metadata("nya!")).unwrap();
init::<Bomb, _>(Metadata("boom!")).unwrap();

assert_eq!(get::<Cat, _>(), Some(Metadata("nya!")));
assert_eq!(get::<Bomb, _>(), Some(Metadata("boom!")));
```
