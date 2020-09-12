pub use glam::{Vec2, Vec4};

// TODO: When publishing, remember to use Vec3A.
// This is done to make code inference work.
pub use glam::Vec3;
// pub use glam::Vec3A as Vec3;
use indices::Indices;
pub use into_colour::IntoVec4Colour;
use topology::Topology;
use vertex_attribute::VertexAttribute;
use crate::reshaper::{DefaultImpl, ReshapeImpl};
use std::marker::PhantomData;

mod indices;
mod into_colour;
mod topology;
mod vertex_attribute;
mod reshaper;

pub trait ShapeSupplier3D {
    fn attributes(&self) -> Vec<VertexAttribute>;
    fn topology(&self) -> Topology;
    fn indices(&self) -> Option<Indices>;
}

pub enum ShapeValidationError {
    InsufficientAttributes,
    UnevenAttributeLengths,
    AttributeIndexOverflow,
    LengthTopologyMismatch,
    ZeroVertices,
}

pub struct Shape {
    pub attributes: Vec<VertexAttribute>,
    pub topology: Topology,
    pub indices: Option<Vec<u32>>,
}

impl Shape {
    pub fn from<T: ShapeSupplier3D>(shape: &T) -> Self {
        Self {
            attributes: shape.attributes(),
            topology: shape.topology(),
            indices: shape.indices().map(|x| x.indices),
        }
    }

    pub fn validate_shape(&self) -> Result<(), ShapeValidationError> {
        if self.attributes.len() == 0 {
            return Err(ShapeValidationError::InsufficientAttributes);
        }

        let len = self.attributes[0].len();
        for attrib in self.attributes.iter().skip(1) {
            if len != attrib.len() {
                return Err(ShapeValidationError::UnevenAttributeLengths);
            }
        }

        if len == 0 {
            return Err(ShapeValidationError::ZeroVertices);
        }

        match self.topology {
            Topology::TriangleListCW | Topology::TriangleListCCW => {
                if len % 3 != 0 {
                    return Err(ShapeValidationError::LengthTopologyMismatch);
                }
            }
            Topology::LineList => {
                if len % 2 != 0 {
                    return Err(ShapeValidationError::LengthTopologyMismatch);
                }
            }
        }

        if let Some(indices) = &self.indices {
            if indices.iter().any(|x| *x as usize >= len) {
                return Err(ShapeValidationError::AttributeIndexOverflow);
            }
        }

        Ok(())
    }
}

#[non_exhaustive]
#[derive(Clone, Debug, Default)]
pub enum Operation {
    CreateDefaultIndices,
    UnrollIndices,
}

#[derive(Default, PartialEq, Hash, Clone, Debug)]
pub struct Reshaper<I: ReshapeImpl = DefaultImpl> {
    commands: Vec<Operation>,
    _phantom: PhantomData<I>
}

impl<I: ReshapeImpl> Reshaper<I> {
    pub fn new() -> Self {
        Self {
            commands: Vec::new(),
            _phantom: PhantomData,
        }
    }

    pub fn create_default_indices(&mut self) -> &mut Self {
        self.commands.push(Operation::CreateDefaultIndices);
        self
    }

    pub fn unroll_indices(&mut self) -> &mut Self {
        self.commands.push(Operation::UnrollIndices);
        self
    }

    pub fn apply(&self, shape: &mut Shape) where I: Default {
        let mut implementation = I::default();

        self.apply_custom(shape, &mut implementation);
    }

    pub fn apply_custom(&self, shape: &mut Shape, implementation: &mut I) {
        implementation.execute_all(&self.operations, shape);
    }
}
