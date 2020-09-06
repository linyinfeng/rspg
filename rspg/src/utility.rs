/// Create a Vec with size and all element in the Vec is the clone of item.
pub fn vec_with_size<T>(size: usize, item: T) -> Vec<T>
where
    T: Clone,
{
    vec![item; size]
}

#[cfg(test)]
mod tests {
    use super::vec_with_size;

    #[test]
    fn vec_with_size_zero_size() {
        let v = vec_with_size(0, ());
        assert_eq!(v.len(), 0);
    }

    #[test]
    fn vec_with_size_100() {
        let item = 10;
        let v = vec_with_size(100, item);
        assert_eq!(v.len(), 100);
        assert!(v.into_iter().all(|x| x == item));
    }
}
