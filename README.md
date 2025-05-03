<!-- cargo-rdme start -->

Tools for implementing and transcribing C APIs.

[`&CStr`], [`CString`] and [`Box`] are not FFI safe.

```rust
#[deny(improper_ctypes_definitions)]
extern "C" fn bad(_: &CStr, _: Box<u8>) -> CString { todo!() }
```

[`&SeaStr`], [`SeaString`] and [`SeaBox`] are FFI-safe equivalents.

```rust
#[deny(improper_ctypes_definitions)]
extern "C" fn good(_: &SeaStr, _: SeaBox<u8>) -> SeaString { todo!() }
```

<details><summary>

All are pointer-wide, with a non-null niche filled by [`Option::None`].
</summary>

```rust
assert_eq!(size_of::<SeaBox<u8>>(),         size_of::<*mut u8>());
assert_eq!(size_of::<Option<SeaBox<u8>>>(), size_of::<*mut u8>());
assert_eq!(size_of::<SeaString>(),          size_of::<*mut c_char>());
assert_eq!(size_of::<Option<SeaString>>(),  size_of::<*mut c_char>());
```
</details>

[`trait@TransmuteFrom`] is the culmination of this crate,
for writing your own wrappers to C types.
See its documentation for more.

[`&CStr`]: core::ffi::CStr
[`&SeaStr`]: SeaStr
[`CString`]: alloc::ffi::CString
[`Box`]: alloc::boxed::Box

<!-- cargo-rdme end -->
