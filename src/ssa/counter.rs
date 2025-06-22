#[derive(Debug)]
pub struct Counter<'a> {
    func: &'a str,
    count: usize,
}

impl<'a> Counter<'a> {
    pub fn new(func: &'a str) -> Self {
        Counter { func, count: 0 }
    }
    pub fn next<T: From<usize>>(&mut self) -> T {
        let reg = self.count;
        self.count += 1;
        T::from(reg)
    }

    pub fn func(&self) -> &'a str {
        self.func
    }
}
