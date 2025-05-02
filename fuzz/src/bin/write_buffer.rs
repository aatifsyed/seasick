#![no_main]

use std::fmt;

use seasick::WriteBuffer;

fn do_fuzz(theirs: String) {
    let mut buffer = WriteBuffer::new();
    fmt::write(&mut buffer, format_args!("{}", &theirs)).unwrap();
    dbg!(buffer.as_cstr(), buffer.as_str());
    assert_eq!(buffer.as_cstr().to_bytes().len(), buffer.as_str().len())
}

libfuzzer_sys::fuzz_target!(|input: String| {
    do_fuzz(input);
});
