#![no_main]

use std::{
    ffi::{c_char, CString},
    ptr::{self, NonNull},
};

use seasick::{nul_terminated, SeaStr};

fn do_fuzz(inputs: Vec<CString>) {
    let ptrs = inputs
        .iter()
        .map(|it| it.as_ptr())
        .chain([ptr::null()])
        .collect::<Vec<_>>();
    let it = NonNull::<[*const c_char]>::from(&*ptrs).cast::<*const SeaStr>();
    let ours = unsafe { nul_terminated::Iter::new(it) }.collect::<Vec<_>>();
    assert_eq!(ours.len(), inputs.len());
    assert!(ours
        .iter()
        .map(|it| it.as_cstr())
        .eq(inputs.iter().map(|it| it.as_c_str())));
}

libfuzzer_sys::fuzz_target!(|inputs: Vec<CString>| {
    do_fuzz(inputs);
});
