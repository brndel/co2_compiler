pub fn align_bytes(bytes: usize, align: usize) -> usize {
    if align == 0 || bytes % align == 0 {
        return bytes;
    } else {
        ((bytes / align) + 1) * align
    }
}
