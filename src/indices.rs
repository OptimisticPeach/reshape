pub struct Indices {
    pub indices: Vec<u32>,
}

impl Indices {
    pub fn from<T: Into<u32> + Clone>(data: &[T]) -> Self {
        Self {
            indices: data.iter().cloned().map(Into::into).collect(),
        }
    }
}
