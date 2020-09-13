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
pub use crate::interpolation::{Vector, LerpParams};

mod indices;
mod into_colour;
mod topology;
mod vertex_attribute;
mod reshaper;
mod interpolation;

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
#[derive(Debug)]
pub enum Operation {
    CreateDefaultIndices,
    UnrollIndices,
    Subdivide {
        subdivisions: usize,
        position_interpolation: LerpParams<Vec3>,
        colour_interpolation: LerpParams<Vec4>,
        uv_interpolation: LerpParams<Vec2>,
        ///
        /// If set to None, this will try to recalculate
        /// the normals of a triangle mesh with the correct
        /// topology, and if unable, this will use linear
        /// interpolation and normalization to calculate the
        /// new normals.
        ///
        normal_interpolation: Option<LerpParams<Vec3>>,
    }
}

#[derive(Default, Debug)]
pub struct Reshaper {
    commands: Vec<Operation>,
}

impl Reshaper {
    pub fn new() -> Self {
        Self {
            commands: Vec::new(),
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

    pub fn apply(&self, shape: &mut Shape) {
        self.apply_custom(shape, &mut DefaultImpl);
    }

    pub fn apply_custom(&self, shape: &mut Shape, implementation: &mut impl ReshapeImpl) {
        implementation.execute_all(&self.commands, shape);
    }
}
