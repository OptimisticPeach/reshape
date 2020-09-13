use crate::{LerpParams, Operation, Shape, Topology, Vector, VertexAttribute};
use crate::{Vec2, Vec3, Vec4};

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
                } => match shape.topology {
                    Topology::LineList => self.subdivide_line_list(
                        shape,
                        subdivisions,
                        position_interpolation,
                        colour_interpolation,
                        uv_interpolation,
                        normal_interpolation.as_mut(),
                    ),
                    Topology::TriangleListCW => self.subdivide_triangles_cw(
                        shape,
                        subdivisions,
                        position_interpolation,
                        colour_interpolation,
                        uv_interpolation,
                        normal_interpolation.as_mut(),
                    ),
                    Topology::TriangleListCCW => self.subdivide_triangles_ccw(
                        shape,
                        subdivisions,
                        position_interpolation,
                        colour_interpolation,
                        uv_interpolation,
                        normal_interpolation.as_mut(),
                    ),
                },
            }
        }
    }

    fn unroll_indices(&mut self, shape: &mut Shape);

    fn create_default_indices(&mut self, shape: &mut Shape);

    fn subdivide_line_list(
        &mut self,
        shape: &mut Shape,
        subdivisions: usize,
        position_interpolation: &mut LerpParams<Vec3>,
        colour_interpolation: &mut LerpParams<Vec4>,
        uv_interpolation: &mut LerpParams<Vec2>,
        normal_interpolation: Option<&mut LerpParams<Vec3>>,
    );

    fn subdivide_triangles_cw(
        &mut self,
        shape: &mut Shape,
        subdivisions: usize,
        position_interpolation: &mut LerpParams<Vec3>,
        colour_interpolation: &mut LerpParams<Vec4>,
        uv_interpolation: &mut LerpParams<Vec2>,
        normal_interpolation: Option<&mut LerpParams<Vec3>>,
    );

    fn subdivide_triangles_ccw(
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

    fn subdivide_line_list(
        &mut self,
        shape: &mut Shape,
        subdivisions: usize,
        position_interpolation: &mut LerpParams<Vec3>,
        colour_interpolation: &mut LerpParams<Vec4>,
        uv_interpolation: &mut LerpParams<Vec2>,
        normal_interpolation: Option<&mut LerpParams<Vec3>>,
    ) {
        if subdivisions == 0 {
            return;
        }
        debug_assert_eq!(shape.topology, Topology::LineList);
        let mut default_normal_interpolation = Default::default();

        let normal_interpolation =
            normal_interpolation.unwrap_or(&mut default_normal_interpolation);

        if let Some(indices) = &mut shape.indices {
            let new_items = (indices.len() / 2) * subdivisions;
            shape
                .attributes
                .iter_mut()
                .for_each(|x| x.reserve(new_items));

            for i in 0..indices.len() / 2 {
                let a_idx = i * 2;
                let b_idx = i * 2 + 1;
                let (a, b) = (chunk[a_idx], chunk[b_idx]);

                let old_len = shape.attributes[0].len() as u32;

                for attrib in &mut shape.attributes {
                    fn subdivide_line<T: Vector + Default>(
                        subdivisions: usize,
                        interpolation: &mut LerpParams<T>,
                        attributes: &mut Vec<T>,
                        a: u32,
                        b: u32,
                    ) {
                        attributes
                            .extend(std::iter::repeat_with(Default::default).take(subdivisions));
                        interpolation.interpolate_multiple(
                            attributes[a as usize].clone(),
                            attributes[b as usize].clone(),
                            attributes.len() - subdivisions..attributes.len(),
                            attributes,
                        );
                    }
                    match attrib {
                        VertexAttribute::Colour(colours) => {
                            subdivide_line(subdivisions, colour_interpolation, colours, a, b)
                        }
                        VertexAttribute::Position(positions) => {
                            subdivide_line(subdivisions, position_interpolation, positions, a, b)
                        }
                        VertexAttribute::UV(uvs) => {
                            subdivide_line(subdivisions, uv_interpolation, uvs, a, b)
                        }
                        VertexAttribute::Normal(normals) => {
                            subdivide_line(subdivisions, normal_interpolation, normals, a, b);
                        }
                    }
                }

                let new_len = shape.attributes[0].len() as u32;
                let a_idx = a_idx as u32;
                let b_idx = b_idx as u32;

                indices.extend_from_slice(&[a_idx, old_len, b_idx, new_len - 1]);

                for i in old_len..new_len {
                    indices.extend_from_slice(&[i, i + 1]);
                }
            }
        } else {
            // TODO
            unimplemented!();
        }
    }

    fn subdivide_triangles_cw(
        &mut self,
        shape: &mut Shape,
        subdivisions: usize,
        position_interpolation: &mut LerpParams<Vec3>,
        colour_interpolation: &mut LerpParams<Vec4>,
        uv_interpolation: &mut LerpParams<Vec2>,
        normal_interpolation: Option<&mut LerpParams<Vec3>>,
    ) {
        unimplemented!()
    }

    fn subdivide_triangles_ccw(
        &mut self,
        shape: &mut Shape,
        subdivisions: usize,
        position_interpolation: &mut LerpParams<Vec3>,
        colour_interpolation: &mut LerpParams<Vec4>,
        uv_interpolation: &mut LerpParams<Vec2>,
        normal_interpolation: Option<&mut LerpParams<Vec3>>,
    ) {
        unimplemented!()
    }
}
