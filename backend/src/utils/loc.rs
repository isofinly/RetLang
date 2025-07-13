/// Convert 1-based (line, column, width) into an absolute `SourceSpan`.
///
///     line / col are 1-based to match typical compiler diagnostics.
///     width is the number of bytes to underline.
pub fn span_from_linecol(src: &str, line: usize, col: usize, width: usize) -> miette::SourceSpan {
    let mut offset = 0usize;
    for (i, l) in src.lines().enumerate() {
        if i + 1 == line {
            offset += col - 1;
            break;
        }
        offset += l.len() + 1;
    }
    miette::SourceSpan::from((offset, width))
}

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
