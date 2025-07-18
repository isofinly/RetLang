pub fn byte_offset_to_line_col(src: &str, offset: usize) -> (usize, usize) {
    let clamped_offset = offset.min(src.len());

    let newline_count = src[..clamped_offset]
        .bytes()
        .filter(|&b| b == b'\n')
        .count();
    let line = newline_count + 1;

    let last_nl = src[..clamped_offset]
        .rfind('\n')
        .map(|i| i + 1)
        .unwrap_or(0);
    let column = clamped_offset - last_nl + 1; // 1-based

    (line, column)
}
