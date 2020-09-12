use crate::into_colour::IntoVec4Colour;
use glam::Vec2;
use glam::Vec3;
use glam::Vec4;

#[non_exhaustive]
pub enum VertexAttribute {
    Position(Vec<Vec3>),
    Colour(Vec<Vec4>),
    Normal(Vec<Vec3>),
    UV(Vec<Vec2>),
}

impl VertexAttribute {
    pub fn from_pos<T: Into<Vec3> + Clone>(data: &[T]) -> Self {
        VertexAttribute::Position(data.iter().cloned().map(Into::into).collect())
    }

    pub fn from_colour<T: IntoVec4Colour + Clone>(data: &[T]) -> Self {
        VertexAttribute::Colour(
            data.iter()
                .cloned()
                .map(IntoVec4Colour::into_colour)
                .collect(),
        )
    }

    pub fn from_uv<T: Into<Vec2> + Clone>(data: &[T]) -> Self {
        VertexAttribute::UV(data.iter().cloned().map(Into::into).collect())
    }

    pub fn from_normal<T: Into<Vec3> + Clone>(data: &[T]) -> Self {
        VertexAttribute::Normal(data.iter().cloned().map(Into::into).collect())
    }
}

impl VertexAttribute {
    pub fn len(&self) -> usize {
        match self {
            VertexAttribute::Position(x) => x.len(),
            VertexAttribute::Colour(x) => x.len(),
            VertexAttribute::Normal(x) => x.len(),
            VertexAttribute::UV(x) => x.len(),
            _ => unreachable!(),
        }
    }

    pub(crate) fn unroll_indices(&mut self, indices: &[u32]) {
        fn unroll<T: Copy>(original: &mut Vec<T>, indices: &[u32]) {
            let mut new_val = Vec::with_capacity(indices.len());

            new_val.extend(indices.iter().map(|&i| original[i as usize]));

            *original = new_val;
        }

        match self {
            VertexAttribute::Position(pos) => unroll(pos, indices),
            VertexAttribute::Colour(col) => unroll(col, indices),
            VertexAttribute::Normal(norm) => unroll(norm, indices),
            VertexAttribute::UV(uv) => unroll(uv, indices),
        }
    }
}
