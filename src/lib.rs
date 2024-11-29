#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]

#[cfg(feature = "alloc")]
extern crate alloc;

mod _alloc;
mod _box;
mod _str;
mod _string;

pub use _alloc::*;
pub use _box::*;
pub use _str::*;
pub use _string::*;
