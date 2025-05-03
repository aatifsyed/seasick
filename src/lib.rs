//! Write and transcribe C APIs.
//!
//! [`&CStr`] and [`CString`] are not FFI safe.
//!
//! ```compile_fail
//! # use std::ffi::{CStr, CString};
//! #![deny(improper_ctypes)]
//! extern "C" fn concat(_: &CStr, _: &CStr) -> CString { todo!() }
//! ```
//! [`&SeaStr`] and [`SeaString`] are FFI-safe equivalents.
//! ```rust
//! # use seasick::*;
//! #[deny(improper_ctypes)]
//! extern "C" fn concat(_: &SeaStr, _: &SeaStr) -> SeaString { todo!() }
//! ```
//! They use the non-null niche which is filled by [`Option::None`].
//! ```c
//! /** may return null */
//! char *foo(void);
//! ```
//! ```
//! # stringify! {
//! extern "C" fn foo() -> Option<SeaString> { .. }
//! # };
//! # use {std::{ffi::c_char, mem::size_of}, seasick::SeaString};
//! assert_eq!(size_of::<Option<SeaString>>(), size_of::<*mut c_char>());
//! ```
//!
//! [`SeaArray`] wraps a `[c_char; N]` array, providing [`SeaStr`]-like capabilities.
//! [`SeaBox`] is an additional owned pointer type, with a pluggable [`Allocator`].
//! [`till_null`] contains iterators for nul-terminated arrays of pointers.
//! [`TransmuteFrom`] is a powerful trait and derive macro for writing wrappers
//! to C types.
//!
//! [`&CStr`]: core::ffi::CStr
//! [`&SeaStr`]: SeaStr
//! [`CString`]: alloc::ffi::CString

#![no_std]
#![cfg_attr(docsrs, feature(doc_cfg))]

#[cfg(feature = "std")]
extern crate std;

#[cfg(feature = "alloc")]
extern crate alloc;

mod _alloc;
mod _array;
mod _box;
mod _str;
mod _string;
#[cfg(feature = "alloc")]
mod _write_buffer;

pub use _alloc::*;
pub use _array::*;
pub use _box::*;
pub use _str::*;
pub use _string::*;
#[cfg(feature = "alloc")]
pub use _write_buffer::*;

pub mod till_null;

use core::fmt::{self, Write as _};

fn debug_bytes(bytes: &[u8], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("\"")?;
    for chunk in bytes.utf8_chunks() {
        f.write_fmt(format_args!("{}", chunk.valid().escape_default()))?;
        if !chunk.invalid().is_empty() {
            f.write_char(char::REPLACEMENT_CHARACTER)?
        }
    }
    f.write_str("\"")
}

fn display_bytes(bytes: &[u8], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for chunk in bytes.utf8_chunks() {
        f.write_str(chunk.valid())?;
        if !chunk.invalid().is_empty() {
            f.write_char(char::REPLACEMENT_CHARACTER)?
        }
    }
    Ok(())
}

/// Implement [`trait@TransmuteFrom`] between two types.
///
/// For each field in the given `struct`, compile-time assert that the following
/// match a corresponding field on the remote type:
/// - [size](core::mem::size_of).
/// - [alignment](core::mem::align_of).
/// - [offset](core::mem::offset_of).
///
/// The size and alignment of the outer structs are also checked.
/// The two structures can they be [transmuted](TransmuteFrom::transmute_from) between one another.
///
/// # Worked Example
///
/// Given this `C` file:
///
/// ```c
#[doc = include_str!("_doc/transmute_from/header.h")]
/// ```
///
/// [`bindgen`](https://docs.rs/bindgen) can generate the following Rust code:
///
/// ```
#[doc = include_str!("_doc/transmute_from/bindgen.rs")]
/// ```
///
/// We can then write a reinterpretation of that struct:
///
/// ```
/// # #![cfg(all(feature = "macros", feature = "libc"))]
/// # use core::{ffi::*, marker::PhantomData};
/// # use seasick::*;
/// # #[path = "_doc/transmute_from/bindgen.rs"] mod sys;
/// #[repr(C)]
/// #[derive(TransmuteFrom)]
/// #[transmute(from(sys::yak_shaver))]
/// struct YakShaver<'rt> {
///     // the names of all these match the remote.
///     pub id: c_uint,
///     pub name: SeaArray<64>,
///     pub owner: &'rt SeaStr,
///     pub description: Option<SeaString>,
///     pub children: Option<till_null::Iter<'rt, SeaStr>>,
///
///                                     // assert on the remote type
///     #[transmute(*mut sys::clothes)] // (useful for keeping definitions in sync)
///     pub clothes: SeaBox<sys::clothes>,
///
///     #[transmute(list: _)] // specify a different name on the remote
///     linked_list: [*mut Self; 2],
///
///     #[transmute(skip)] // skip ZST
///     phantom: PhantomData<()>,
/// }
/// ```
///
/// Bad field ordering, field [layout](core::alloc::Layout),
/// incorrect type assertions on the remote, and missing remote fields will
/// all cause a compilation failure:
///
/// ```compile_fail
/// # #![cfg(all(feature = "macros"))]
/// # use core::{ffi::*, marker::PhantomData};
/// # use seasick::*;
/// # #[path = "_doc/transmute_from/bindgen.rs"] mod sys;
/// #[repr(C)]
/// #[derive(TransmuteFrom)]
/// #[transmute(from(sys::yak_shaver))]
/// struct YakShaver<'rt> {
///     pub name: SeaArray<64>,
///     pub id: c_uint,
/// }
/// ```
///
/// By default, if the remote field type isn't specified,
/// no assertions are made on its _type_ (size, align and offset are always checked).
/// If `#[transmute(strict)]` is present on the `struct` definition,
/// the type will always be checked, defaulting to the type of the field on the
/// "local" `struct`.
///
/// ```
/// # use core::{ffi::*, marker::PhantomData};
/// # use seasick::*;
/// # #[path = "_doc/transmute_from/bindgen.rs"] mod sys;
/// #[repr(C)]
/// #[derive(TransmuteFrom)]
/// #[transmute(from(sys::yak_shaver), strict)]
/// //                                 ^^^^^^
/// // requires that remote types are present and correct
/// struct YakShaver<'rt> {
///     // name and type already match
///     pub id: c_uint,
///
///     #[transmute([c_char; 64])] // need to specify type
///     pub name: SeaArray<64>,
///     #[transmute(*const c_char)]
///     pub owner: &'rt SeaStr,
///     #[transmute(*mut c_char)]
///     pub description: Option<SeaString>,
///     #[transmute(*mut *mut c_char)]
///     pub children: Option<till_null::Iter<'rt, SeaStr>>,
///     #[transmute(*mut sys::clothes)]
///     pub clothes: SeaBox<sys::clothes>,
///
///     #[transmute(list: sys::yak_shaver__bindgen_ty_1)] // need to specify name and type
///     linked_list: [*mut Self; 2],
///
///     #[transmute(skip)]
///     phantom: PhantomData<()>,
/// }
/// ```
///
/// <div class="warning">
/// This macro can only check the size, alignment and offsets of types.
/// It is still up to you to ensure that usage is sound.
/// </div>
#[cfg(feature = "macros")]
pub use seasick_macros::TransmuteFrom;

/// Transmute between two types that have the same ABI.
///
/// Implemented with [`derive@TransmuteFrom`].
///
/// # Safety
/// - Must be safe to transmute between the given types.
/// - Blanket impls of [`TransmuteRefFrom`] and [`TransmuteMutFrom`] must be safe.
pub unsafe trait TransmuteFrom<T: Sized>: Sized {
    /// # Safety
    /// - Must be safe to transmute between the given objects.
    unsafe fn transmute_from(src: T) -> Self;
}

unsafe impl<T, U: TransmuteFrom<T>> TransmuteRefFrom<T> for U {
    unsafe fn transmute_ref(src: &T) -> &Self {
        let src = src as *const T as *const U;
        unsafe { &*src }
    }
}
unsafe impl<T, U: TransmuteFrom<T>> TransmuteMutFrom<T> for U {
    unsafe fn transmute_mut(src: &mut T) -> &mut Self {
        let src = src as *mut T as *mut U;
        unsafe { &mut *src }
    }
}

/// Transmute between references of two types that have the same ABI.
///
/// # Safety
/// - Must be safe to transmute between the given types
pub unsafe trait TransmuteRefFrom<T: ?Sized> {
    /// # Safety
    /// - Must be safe to transmute between the given objects
    unsafe fn transmute_ref(src: &T) -> &Self;
}

/// Transmute between mutable references of two types that have the same ABI.
///
/// # Safety
/// - Must be safe to transmute between the given types
pub unsafe trait TransmuteMutFrom<T: ?Sized> {
    /// # Safety
    /// - Must be safe to transmute between the given objects
    unsafe fn transmute_mut(src: &mut T) -> &mut Self;
}

/// Assert that the ABI of two functions are compatible.
///
/// Compile-time assert that the parameters and return types of the given
/// functions have the same [size](core::mem::size_of) and [alignment](core::mem::align_of).
///
/// # Worked example
///
/// ## `C` Source
///
/// ```c
#[doc = include_str!("_doc/assert_abi/header.h")]
/// ```
///
/// ## [`bindgen`](https://docs.rs/bindgen) output.
///
/// ```rust
#[doc = include_str!("_doc/assert_abi/bindgen.rs")]
/// ```
///
/// ## User code
///
/// ```
/// # #![cfg(all(feature = "macros", feature = "libc"))]
/// # use seasick::*;
/// # use core::ffi::c_char;
/// # #[path = "_doc/assert_abi/bindgen.rs"] mod sys;
/// extern "C" fn concat(_: &SeaStr, _: &SeaStr) -> Option<SeaString> { todo!() }
///
/// assert_abi! {
///     sys::concat as unsafe extern "C" fn(*const c_char, *const c_char) -> *mut c_char
///     == concat as extern "C" fn(&SeaStr, &SeaStr) -> Option<SeaString>
/// }
/// ```
///
/// If the abi is mismatched, you will get a compile error:
///
/// ```compile_fail
/// # #![cfg(all(feature = "macros"))]
/// # extern crate alloc;
/// # use seasick::*;
/// # use core::ffi::c_char;
/// # use alloc::vec::Vec;
/// # #[path = "_doc/assert_abi/bindgen.rs"] mod sys;
/// extern "C" fn concat(_: &SeaStr, _: &SeaStr) -> Vec<u8> { todo!() }
///
/// assert_abi! {
///     sys::concat as unsafe extern "C" fn(*const c_char, *const c_char) -> *mut c_char
///     == concat as extern "C" fn(&SeaStr, &SeaStr) -> Vec<u8>;
/// }
/// ```
#[macro_export]
#[cfg(feature = "macros")]
#[cfg_attr(docsrs, doc_cfg(feature = "macros"))]
macro_rules! assert_abi {
    ($($tt:tt)*) => {
        $crate::__private::assert_abi! {
            crate = $crate;
            $($tt)*
        }
    };
}

#[doc(hidden)]
pub mod __private {
    use core::alloc::Layout;

    pub use core;

    pub const fn layout_of_field<T, U>(_: fn(&T) -> &U) -> Layout {
        Layout::new::<U>()
    }

    #[cfg(feature = "macros")]
    pub use seasick_macros::assert_abi;
}

#[cfg(all(test, feature = "std"))]
mod doc {
    use super::*;

    #[test]
    fn transmute_from() {
        let mut v = std::vec::Vec::new();
        bindgen::builder()
            .header_contents("header.h", include_str!("_doc/transmute_from/header.h"))
            .use_core()
            .layout_tests(false)
            .derive_copy(false)
            .derive_debug(false)
            .generate_comments(false)
            .allowlist_type("yak_shaver")
            .allowlist_type("clothes")
            .generate()
            .unwrap()
            .write(std::boxed::Box::new(&mut v))
            .unwrap();
        let s = std::string::String::from_utf8(v).unwrap();
        expect_test::expect_file!["_doc/transmute_from/bindgen.rs"].assert_eq(&s);
    }

    #[test]
    fn abi_eq() {
        let mut v = std::vec::Vec::new();
        bindgen::builder()
            .header_contents("header.h", include_str!("_doc/assert_abi/header.h"))
            .use_core()
            .generate_comments(false)
            .generate()
            .unwrap()
            .write(std::boxed::Box::new(&mut v))
            .unwrap();
        let s = std::string::String::from_utf8(v).unwrap();
        expect_test::expect_file!["_doc/assert_abi/bindgen.rs"].assert_eq(&s);
    }
}
