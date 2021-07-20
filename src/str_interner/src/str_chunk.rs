use std::ptr;

#[derive(Debug)]
pub struct StrChunk {
    data: Vec<u8>,
    ptr: *mut u8,
}

impl StrChunk {
    pub fn with_capacity(cap: usize) -> Self {
        let mut chunk = StrChunk {
            data: vec![0; cap],
            ptr: ptr::null_mut(),
        };
        chunk.ptr = chunk.data.as_mut_ptr();
        chunk
    }

    pub fn remain(&self) -> usize {
        4096 - (self.ptr as usize - self.data.as_ptr() as usize)
    }

    pub fn alloc(&mut self, size: usize) -> Option<*mut u8> {
        if self.remain() < size {
            return None;
        }

        let ptr = self.ptr;
        unsafe { self.ptr.add(size) };
        Some(ptr)
    }
}
