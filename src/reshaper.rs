use crate::{LerpParams, Operation, Shape, Topology};
use crate::{Vec2, Vec3, Vec4};

mod subdivide;

use subdivide::TriangleSubdivideParams;

pub trait ReshapeImpl {
    fn execute_all(&mut self, operations: &mut [Operation], shape: &mut Shape) {
        for operation in operations {
            match operation {
                Operation::CreateDefaultIndices => self.create_default_indices(shape),
                Operation::UnrollIndices => self.unroll_indices(shape),
                &mut Operation::Subdivide {
                    subdivisions,
                    ref mut position_interpolation,
                    ref mut colour_interpolation,
                    ref mut uv_interpolation,
                    ref mut normal_interpolation,
                } => self.subdivide(
                    shape,
                    subdivisions,
                    position_interpolation,
                    colour_interpolation,
                    uv_interpolation,
                    normal_interpolation.as_mut(),
                ),
            }
        }
    }

    fn unroll_indices(&mut self, shape: &mut Shape);

    fn create_default_indices(&mut self, shape: &mut Shape);

    fn subdivide(
        &mut self,
        shape: &mut Shape,
        subdivisions: usize,
        position_interpolation: &mut LerpParams<Vec3>,
        colour_interpolation: &mut LerpParams<Vec4>,
        uv_interpolation: &mut LerpParams<Vec2>,
        normal_interpolation: Option<&mut LerpParams<Vec3>>,
    );
}

#[derive(Copy, Clone, Debug, PartialEq, Hash)]
pub struct DefaultImpl;

impl ReshapeImpl for DefaultImpl {
    fn unroll_indices(&mut self, shape: &mut Shape) {
        let indices = shape.indices.take();
        let indices = if let Some(x) = indices {
            x
        } else {
            return;
        };

        shape
            .attributes
            .iter_mut()
            .for_each(|x| x.unroll_indices(&indices));
    }

    fn create_default_indices(&mut self, shape: &mut Shape) {
        if shape.indices.is_some() {
            return;
        }

        shape.indices = Some((0..shape.attributes[0].len() as u32).collect());
    }

    fn subdivide(
        &mut self,
        shape: &mut Shape,
        subdivisions: usize,
        position_interpolation: &mut LerpParams<Vec3>,
        colour_interpolation: &mut LerpParams<Vec4>,
        uv_interpolation: &mut LerpParams<Vec2>,
        normal_interpolation: Option<&mut LerpParams<Vec3>>,
    ) {
        match shape.topology {
            Topology::TriangleListCW => subdivide::subdivide_triangles(TriangleSubdivideParams {
                shape,
                subdivisions,
                position_interpolation,
                colour_interpolation,
                uv_interpolation,
                normal_interpolation,
                is_ccw: true,
            }),
            Topology::TriangleListCCW => subdivide::subdivide_triangles(TriangleSubdivideParams {
                shape,
                subdivisions,
                position_interpolation,
                colour_interpolation,
                uv_interpolation,
                normal_interpolation,
                is_ccw: false,
            }),
            Topology::LineList => subdivide::subdivide_lines(
                shape,
                subdivisions,
                position_interpolation,
                colour_interpolation,
                uv_interpolation,
                normal_interpolation,
            )
        }
    }
}
