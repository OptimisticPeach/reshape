pub use glam::{Vec2, Vec4};

// TODO: When publishing, remember to use Vec3A.
// This is done to make code inference work.
pub use glam::Vec3;
// pub use glam::Vec3A as Vec3;
pub use crate::interpolation::{LerpParams, Vector};
use crate::reshaper::{DefaultImpl, ReshapeImpl};
use indices::Indices;
pub use into_colour::IntoVec4Colour;
use topology::Topology;
use vertex_attribute::VertexAttribute;
use std::collections::HashSet;

mod indices;
mod interpolation;
mod into_colour;
mod reshaper;
mod topology;
mod vertex_attribute;

pub trait ShapeSupplier3D {
    fn attributes(&self) -> Vec<VertexAttribute>;
    fn topology(&self) -> Topology;
    fn indices(&self) -> Option<Indices>;
}

pub enum ShapeValidationError {
    ///
    /// `attributes.len() == 0`
    ///
    InsufficientAttributes,
    ///
    /// Not all attributes have same number of elements
    ///
    UnevenAttributeLengths,
    ///
    /// Indices will try and index outside of length of attributes
    ///
    AttributeIndexOverflow,
    ///
    /// Number of items in shape does not work with topology
    ///
    LengthTopologyMismatch,
    ///
    /// There are zero attributes in the shape
    ///
    ZeroVertices,
    ///
    /// A triangle shares an edge with a triangle that is wound
    /// opposite to it.
    ///
    /// ## Note
    /// This may cause unexpected results when subdividing, but
    /// will not cause a panic. Open an issue on the repository
    /// if you wish for different behaviour on finding this case.
    ///
    /// Additionally, this is the last check run when running a
    /// shape validation, so if this is what is returned, and is
    /// expected of the shape you're processing, then you can
    /// continue knowing that the rest of the parts of the shape
    /// are valid.
    ///
    TriangleNeighbourWindingMismatch,
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

    ///
    /// Note: Any operation in this library can be written
    /// assuming this returns `Ok(())`. Failure to ensure
    /// may result in a panic or incorrect results.
    ///
    pub fn validate_shape(&self) -> Result<(), ShapeValidationError> {
        if self.attributes.len() == 0 {
            return Err(ShapeValidationError::InsufficientAttributes);
        }

        let len = self.attributes[0].len();

        if len == 0 {
            return Err(ShapeValidationError::ZeroVertices);
        }

        for attrib in self.attributes.iter().skip(1) {
            if len != attrib.len() {
                return Err(ShapeValidationError::UnevenAttributeLengths);
            }
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

        if let (Some(indices), Topology::TriangleListCCW) | (Some(indices), Topology::TriangleListCW) = (&self.indices, self.topology) {
            let mut edge_set = HashSet::new();
            for triangle in indices.chunks_exact(3) {
                let a = triangle[0];
                let b = triangle[1];
                let c = triangle[2];

                if edge_set.contains(&(a, b)) ||
                    edge_set.contains(&(b, c)) ||
                    edge_set.contains(&(c, a)) {
                    return Err(ShapeValidationError::TriangleNeighbourWindingMismatch);
                }

                edge_set.insert((a, b));
                edge_set.insert((b, c));
                edge_set.insert((c, a));
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
    },
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

    pub fn apply(&mut self, shape: &mut Shape) {
        self.apply_custom(shape, &mut DefaultImpl);
    }

    pub fn apply_custom(&mut self, shape: &mut Shape, implementation: &mut impl ReshapeImpl) {
        implementation.execute_all(&mut self.commands, shape);
    }
}
