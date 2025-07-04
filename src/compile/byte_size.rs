

#[derive(Debug, Clone, Copy)]
pub enum ByteSize {
    B1,
    B2,
    B4,
    B8,
}

impl TryFrom<usize> for ByteSize {
    type Error = ();
    
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::B1),
            2 => Ok(Self::B2),
            4 => Ok(Self::B4),
            8 => Ok(Self::B8),
            _ => Err(())
        }
    }
}

pub struct WithByteSize<T> {
    pub value: T,
    pub size: ByteSize
}

pub trait WithByteSizeExt: Sized {
    fn with_size(&self, size: ByteSize) -> WithByteSize<Self>;
}

impl<T: Copy> WithByteSizeExt for T {
    fn with_size(&self, size: ByteSize) -> WithByteSize<Self> {
        WithByteSize { value: *self, size }
    }
}