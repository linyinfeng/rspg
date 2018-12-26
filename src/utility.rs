/// create a Vec with size and all element in the Vec is the clone of item
pub fn vec_with_size<T>(size: usize, item: T) -> Vec<T>
where
    T: Clone,
{
    let mut vec = Vec::new();
    if size != 0 {
        for _ in 0..size - 1 {
            vec.push(item.clone());
        }
        vec.push(item);
    }
    vec
}
