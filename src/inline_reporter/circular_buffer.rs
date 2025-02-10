use std::{iter::Chain, slice::Iter as IterSlice};

pub struct CircularBuffer<T> {
    buffer: Vec<T>,
    write_idx: usize,
}

pub type Iter<'a, T> = Chain<IterSlice<'a, T>, IterSlice<'a, T>>;

#[allow(dead_code)]
impl<T> CircularBuffer<T> {
    pub fn new(capacity: usize) -> Self {
        Self {
            buffer: Vec::with_capacity(capacity),
            write_idx: 0,
        }
    }

    pub fn push(&mut self, item: T) {
        if self.is_full() {
            let _ = std::mem::replace(&mut self.buffer[self.write_idx], item);
        } else {
            self.buffer.insert(self.write_idx, item);
        }
        self.write_idx = (self.write_idx + 1) % self.capacity();
    }

    pub fn len(&self) -> usize {
        self.buffer.len()
    }

    pub fn is_full(&self) -> bool {
        self.buffer.len() == self.capacity()
    }

    pub fn capacity(&self) -> usize {
        self.buffer.capacity()
    }

    pub fn iter(&self) -> Iter<T> {
        let (a, b) = self.buffer.split_at(self.write_idx);
        b.iter().chain(a.iter())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_iterator_over_circular_in_order_when_buffer_is_not_full() {
        let capacity = 5;

        let mut ring: CircularBuffer<usize> = CircularBuffer::new(capacity);

        ring.push(1);
        ring.push(2);
        ring.push(3);

        assert_eq!(ring.iter().map(|&n| n).collect::<Vec<_>>(), [1, 2, 3]);
    }

    #[test]
    fn test_iterator_over_circular_in_order_when_buffer_is_full() {
        let capacity = 5;

        let mut ring: CircularBuffer<usize> = CircularBuffer::new(capacity);

        ring.push(1);
        ring.push(2);
        ring.push(3);
        ring.push(4);
        ring.push(5);

        assert_eq!(ring.iter().map(|&n| n).collect::<Vec<_>>(), [1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_iterator_over_circular_in_order_when_buffer_is_roll_over() {
        let capacity = 5;

        let mut ring: CircularBuffer<usize> = CircularBuffer::new(capacity);

        ring.push(1);
        ring.push(2);
        ring.push(3);
        ring.push(4);
        ring.push(5);
        ring.push(6);
        ring.push(7);

        assert_eq!(ring.iter().map(|&n| n).collect::<Vec<_>>(), [3, 4, 5, 6, 7]);
    }

    #[test]
    fn test_len_at_zero_initially() {
        let capacity = 5;
        let ring: CircularBuffer<usize> = CircularBuffer::new(capacity);
        assert_eq!(ring.len(), 0);
    }

    #[test]
    fn test_set_capacity() {
        let capacity = 5;
        let ring: CircularBuffer<usize> = CircularBuffer::new(capacity);
        assert_eq!(ring.capacity(), capacity);
    }

    #[test]
    fn test_can_push_bellow_capacity() {
        let capacity = 5;
        let mut ring: CircularBuffer<usize> = CircularBuffer::new(capacity);
        ring.push(1);
        ring.push(1);
        assert_eq!(ring.len(), 2);
    }

    #[test]
    fn test_can_push_over_capacity() {
        let capacity = 5;
        let until = capacity * 10;
        let mut ring: CircularBuffer<usize> = CircularBuffer::new(capacity);

        for i in 0..until {
            ring.push(i);
        }

        assert_eq!(ring.len(), capacity);
    }

    #[test]
    fn test_len() {
        let capacity = 20;
        let mut ring = CircularBuffer::new(capacity);
        assert_eq!(ring.len(), 0);
        ring.push(1);
        assert_eq!(ring.len(), 1);
        ring.push(1);
        assert_eq!(ring.len(), 2);
    }
}
