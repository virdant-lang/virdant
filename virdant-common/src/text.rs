use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct Text(Arc<[u8]>);

impl<S: ToString> From<S> for Text {
    fn from(s: S) -> Self {
        Text(Arc::from(s.to_string().into_bytes().into_boxed_slice()))
    }
}

impl std::ops::Deref for Text {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}
