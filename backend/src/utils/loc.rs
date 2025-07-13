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
