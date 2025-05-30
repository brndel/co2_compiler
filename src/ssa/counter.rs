#[derive(Debug, Default)]
pub struct Counter {
    count: usize,
}

impl Counter {
    pub fn next<T: From<usize>>(&mut self) -> T {
        let reg = self.count;
        self.count += 1;
        T::from(reg)
    }
}
