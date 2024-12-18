#![no_main]

use std::ffi::CString;

use seasick::SeaString;

fn do_fuzz(theirs: CString) {
    let ours = seasick::SeaString::new(&theirs);
    assert_eq!(ours.len(), theirs.as_bytes().len());
    assert_eq!(ours.len_with_nul(), theirs.as_bytes_with_nul().len());
    assert_eq!(ours.bytes(), theirs.as_bytes());
    assert_eq!(ours.bytes_with_nul(), theirs.as_bytes_with_nul());
    assert_eq!(ours.is_empty(), theirs.is_empty());
    let mut ours = unsafe { SeaString::from_ptr(ours.into_raw()) };
    ours.fill(1);
    assert_eq!(ours.len(), theirs.as_bytes().len());
    ours.fill(0);
    assert!(ours.is_empty());
}

libfuzzer_sys::fuzz_target!(|input: CString| {
    do_fuzz(input);
});
