Quoting the [Rust Reference](https://doc.rust-lang.org/reference/items/static-items.html):

"A static item defined in a generic scope (for example in a blanket or default implementation)
will result in exactly one static item being defined, as if the static definition was pulled
out of the current scope into the module. There will not be one item per monomorphization."

One way to work around this is to use a `HashMap<TypeId,Data>`. This is a simple & good solution.
If performance is important, you can skip hashing the `TypeId` as it [already contains](https://github.com/rust-lang/rust/blob/eeff92ad32c2627876112ccfe812e19d38494087/library/core/src/any.rs#L645) a good-quality hash.

This crate aims to further fully remove the lookup by allocating the storage using inline
assembly. Currently only amd64 is supported. Unless you only target amd64, you need to
fall back to a hashmap on other platforms.

Besides the arch, this has the following limitations:
- data must be [zeroable](https://docs.rs/bytemuck/latest/bytemuck/trait.Zeroable.html)
- different compilation units may access different instances of the data
