//! FFI-safe types for writing and transcribing C APIs.
//!
//! [`&CStr`] and [`CString`] are not FFI safe.
//! ```compile_fail
//! # use std::ffi::{CStr, CString};
//! #[deny(improper_ctypes)]
//! extern "C" {
//!     fn concat(_: &CStr, _: &CStr) -> CString;
//! }
//! ```
//! [`&SeaStr`] and [`SeaString`] are FFI-safe equivalents.
//! ```rust
//! # use seasick::{SeaStr, SeaString};
//! # #[deny(improper_ctypes)]
//! unsafe extern "C" {
//!     fn concat(_: &SeaStr, _: &SeaStr) -> SeaString;
//! }
//! ```
//! They use the non-null niche which is filled by [`Option::None`].
//! ```c
//! /** may return null */
//! char *foo(void);
//! ```
//! ```rust
//! # stringify! {
//! extern "C" fn foo() -> Option<SeaString> { .. }
//! # };
//! # use std::{ffi::c_char, mem::size_of}; use seasick::SeaString;
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
#[doc = include_str!("_doc/header.h")]
/// ```
///
/// [`bindgen`](https://docs.rs/bindgen) can generate the following Rust code:
///
/// ```
#[doc = include_str!("_doc/bindgen.rs")]
/// ```
///
/// We can then write a reinterpretation of that struct:
///
/// ```
/// # use core::{ffi::*, marker::PhantomData};
/// # use seasick::*;
/// # #[path = "_doc/bindgen.rs"] mod sys;
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
/// # use core::{ffi::*, marker::PhantomData};
/// # use seasick::*;
/// # #[path = "_doc/bindgen.rs"] mod sys;
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
/// # #[path = "_doc/bindgen.rs"] mod sys;
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
#[cfg(feature = "macros")]
pub use seasick_macros::TransmuteFrom;

#[cfg(feature = "std")]
#[test]
fn _doc() {
    let mut v = std::vec::Vec::new();
    bindgen::builder()
        .header_contents("bind.h", include_str!("_doc/header.h"))
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
    expect_test::expect_file!["_doc/bindgen.rs"].assert_eq(&s);
}

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

/// Compile-time assertions of equality for arity, offset, size and alignment
/// of struct members and function parameters.
///
/// Suppose you are implementing a C header file:
/// ```c
/// struct args
/// {
///     const char *left;
///     const char *right;
/// };
/// char *concat(struct args);
/// ```
///
/// You could use [`bindgen`](https://docs.rs/bindgen) to create simple bindings,
/// and then write nice rust APIs separately,
/// asserting that the two are ABI compatible:
///
/// ```
/// use seasick::{SeaStr, SeaString, assert_abi};
///
/// struct Args<'a> {
///     front: &'a SeaStr,
///     back: &'a SeaStr,
/// }
///
/// #[unsafe(no_mangle)]
/// # extern "C" fn concat(_: Args) -> Option<SeaString> { todo!() }
/// # const _: &str = stringify! {
/// extern "C" fn concat(args: Args) -> Option<SeaString> { .. }
/// # };
///
/// assert_abi! {
///     struct Args = bindings::args { front = left, back = right };
///     fn concat = bindings::concat as unsafe extern "C" fn (_) -> _;
/// }
///
/// mod bindings {
///     /* automatically generated by rust-bindgen */
///     #[repr(C)]
///     #[derive(Debug, Copy, Clone)]
///     pub struct args {
///         pub left: *const ::std::os::raw::c_char,
///         pub right: *const ::std::os::raw::c_char,
///     }
///     unsafe extern "C" {
///         pub fn concat(arg1: args) -> *mut ::std::os::raw::c_char;
///     }
/// }
/// ```
///
/// Compilation will fail if the ABI drifts out of sync.
///
/// ```compile_fail
/// # use bindings::Args;
/// # #[expect(improper_ctypes_definitions)]
/// extern "C" fn concat(args: Args) -> String { String::new() }
///                                  // ^^^^^^ different size and alignment
/// assert_abi! {
///     fn concat = bindings::concat as unsafe extern "C" fn(_) -> _;
/// }
/// # mod bindings { #[repr(C)] pub struct Args(()); extern "C" { pub fn concat(args: Args) -> *mut ::std::os::raw::c_char; } }
/// ```
///
/// <div class="warning">
///
/// This macro only detects ABI changes (e.g size, alignment), and cannot distinguish
/// e.g `Box` from `SeaString` - it is still up to you to write (or generate) your type mappings appropriately.
///
/// </div>
#[macro_export]
macro_rules! assert_abi {
    (fn $left:path = $right:path as $ty:ty $(; $($tt:tt)*)?) => {
        const _: () = {
            let _ = $left as $ty;
            let _ = $right as $ty;
            $crate::__private::assert!($crate::__private::abi_eq($left as $ty, $right as $ty));
        };
        $(
            $crate::assert_abi!($($tt)*);
        )?
    };
    (#[non_exhaustive] struct $left_ty:path = $right_ty:path {
        $($left_field:ident = $right_field:ident),* $(,)?
    } $(; $($tt:tt)*)?) => {
        $crate::assert_abi! {
            __struct $left_ty = $right_ty {
                $($left_field = $right_field,)*
            }
        }
        $(
            $crate::assert_abi!($($tt)*);
        )?
    };
    (struct $left_ty:path = $right_ty:path {
        $($left_field:ident = $right_field:ident),* $(,)?
    } $(; $($tt:tt)*)?) => {
        $crate::assert_abi! {
            __struct $left_ty = $right_ty {
                $($left_field = $right_field,)*
            }
        }
        const _: () = {
            fn exhaustive($left_ty { $($left_field: _),* }: $left_ty, $right_ty { $($right_field: _),* }: $right_ty) {}
            //             ^^^^^^^ must be :path not :ty
        };
        $(
            $crate::assert_abi!($($tt)*);
        )?
    };
    (__struct $left_ty:path = $right_ty:path {
        $($left_field:ident = $right_field:ident),* $(,)?
    } $(; $($tt:tt)*)?) => {
        const _: () = {
            use $crate::__private::*;

            let left = Layout::new::<$left_ty>();
            let right = Layout::new::<$right_ty>();

            assert! {
                left.size() == right.size(),
                concat!("size mismatch between ", stringify!($left_ty), " and ", stringify!($right_ty))
            };
            assert! {
                left.align() == right.align(),
                concat!("aligment mismatch between ", stringify!($left_ty), " and ", stringify!($right_ty))
            };

            $(
                assert! {
                    offset_of!($left_ty, $left_field) == offset_of!($right_ty, $right_field),
                    concat!("mismatched offsets between ", stringify!($left_field), " and ", stringify!($right_field))
                };

                let left = layout_of_field(|it: &$left_ty| &it.$left_field);
                let right = layout_of_field(|it: &$right_ty| &it.$right_field);

                assert! {
                    left.size() == right.size(),
                    concat!("size mismatch between ", stringify!($left_field), " and ", stringify!($right_field))
                };
                assert! {
                    left.align() == right.align(),
                    concat!("aligment mismatch between ", stringify!($left_field), " and ", stringify!($right_field))
                };
            )*
        };
    };
    ($(;)?) => {}; // trailing semi
}

#[doc(hidden)]
pub mod __private {

    pub use core;

    pub const fn layout_of_field<T, U>(_: fn(&T) -> &U) -> Layout {
        Layout::new::<U>()
    }

    pub use ::core::{alloc::Layout, assert, concat, mem::offset_of, stringify};

    pub trait AbiEq<T> {
        const ABI_EQ: bool;
    }

    macro_rules! define {
        ($($l:ident $r:ident)*) => {
            impl<LR, RR, $($l, $r),*> AbiEq<fn($($l),*) -> LR> for fn($($r),*) -> RR {
                const ABI_EQ: bool = layout_eq::<LR, RR>() $(&& layout_eq::<$l, $r>())*;
            }
            impl<LR, RR, $($l, $r),*> AbiEq<unsafe fn($($l),*) -> LR> for unsafe fn($($r),*) -> RR {
                const ABI_EQ: bool = layout_eq::<LR, RR>() $(&& layout_eq::<$l, $r>())*;
            }
            impl<LR, RR, $($l, $r),*> AbiEq<extern "C" fn($($l),*) -> LR> for extern "C" fn($($r),*) -> RR {
                const ABI_EQ: bool = layout_eq::<LR, RR>() $(&& layout_eq::<$l, $r>())*;
            }
            impl<LR, RR, $($l, $r),*> AbiEq<unsafe extern "C" fn($($l),*) -> LR> for unsafe extern "C" fn($($r),*) -> RR {
                const ABI_EQ: bool = layout_eq::<LR, RR>() $(&& layout_eq::<$l, $r>())*;
            }
        };
    }

    define!();
    define!(L0 R0);
    define!(L0 R0 L1 R1);
    define!(L0 R0 L1 R1 L2 R2);
    define!(L0 R0 L1 R1 L2 R2 L3 R3);
    define!(L0 R0 L1 R1 L2 R2 L3 R3 L4 R4);
    define!(L0 R0 L1 R1 L2 R2 L3 R3 L4 R4 L5 R5);
    define!(L0 R0 L1 R1 L2 R2 L3 R3 L4 R4 L5 R5 L6 R6);
    define!(L0 R0 L1 R1 L2 R2 L3 R3 L4 R4 L5 R5 L6 R6 L7 R7);
    define!(L0 R0 L1 R1 L2 R2 L3 R3 L4 R4 L5 R5 L6 R6 L7 R7 L8 R8);
    define!(L0 R0 L1 R1 L2 R2 L3 R3 L4 R4 L5 R5 L6 R6 L7 R7 L8 R8 L9 R9);
    define!(L0 R0 L1 R1 L2 R2 L3 R3 L4 R4 L5 R5 L6 R6 L7 R7 L8 R8 L9 R9 L10 R10);
    define!(L0 R0 L1 R1 L2 R2 L3 R3 L4 R4 L5 R5 L6 R6 L7 R7 L8 R8 L9 R9 L10 R10 L11 R11);
    define!(L0 R0 L1 R1 L2 R2 L3 R3 L4 R4 L5 R5 L6 R6 L7 R7 L8 R8 L9 R9 L10 R10 L11 R11 L12 R12);
    define!(L0 R0 L1 R1 L2 R2 L3 R3 L4 R4 L5 R5 L6 R6 L7 R7 L8 R8 L9 R9 L10 R10 L11 R11 L12 R12 L13 R13);
    define!(L0 R0 L1 R1 L2 R2 L3 R3 L4 R4 L5 R5 L6 R6 L7 R7 L8 R8 L9 R9 L10 R10 L11 R11 L12 R12 L13 R13 L14 R14);
    define!(L0 R0 L1 R1 L2 R2 L3 R3 L4 R4 L5 R5 L6 R6 L7 R7 L8 R8 L9 R9 L10 R10 L11 R11 L12 R12 L13 R13 L14 R14 L15 R15);

    const fn layout_eq<L, R>() -> bool {
        let left = Layout::new::<L>();
        let right = Layout::new::<R>();
        left.size() == right.size() && left.align() == right.align()
    }

    pub const fn abi_eq<L, R>(_: L, _: R) -> bool
    where
        L: AbiEq<R>,
        L: Copy,
        R: Copy,
    {
        L::ABI_EQ
    }
}
