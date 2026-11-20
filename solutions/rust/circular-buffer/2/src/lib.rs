pub struct CircularBuffer<T> {
    buffer: Vec<Option<T>>,
    start: usize,
    end: usize,
    capacity: usize,
    count: usize
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    EmptyBuffer,
    FullBuffer,
}

impl<T> CircularBuffer<T>
where
    T: std::clone::Clone,
{
    pub fn new(capacity: usize) -> Self {
        let mut vec = Vec::new();
        for _ in 0..capacity {
            vec.push(None);
        }

        Self {
            buffer: vec,
            start: 0,
            end: 0,
            count: 0,
            capacity,
        }
    }

    pub fn write(&mut self, _element: T) -> Result<(), Error> {
        if self.count == self.capacity {
            return Err(Error::FullBuffer);
        }

        self.buffer[self.end] = Some(_element);
        self.end = (self.end + 1) % self.capacity;
        self.count += 1;
        Ok(())
    }

    pub fn read(&mut self) -> Result<T, Error> {
        if self.count == 0 {
            return Err(Error::EmptyBuffer);
        }

        let val = self.buffer[self.start].as_ref().cloned().unwrap();
        self.buffer[self.start] = None;
        self.start = (self.start + 1) % self.capacity;
        self.count -= 1;
        Ok(val)
    }

    pub fn clear(&mut self) {
        self.start = 0;
        self.end = 0;
        self.count = 0;
        for i in 0..self.capacity {
            self.buffer[i] = None;
        }
    }

    pub fn overwrite(&mut self, _element: T) {
        if self.count == self.capacity {
            self.read().unwrap();
        }

        self.write(_element).unwrap();
    }
}
