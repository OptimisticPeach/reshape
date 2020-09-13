use crate::{LerpParams, Operation, Shape, Topology, VertexAttribute, Vector};
use crate::{Vec2, Vec3, Vec4};

pub trait ReshapeImpl {
    fn execute_all(&mut self, operations: &mut [Operation], shape: &mut Shape) {
        for operation in operations {
            match operation {
                Operation::CreateDefaultIndices => self.create_default_indices(shape),
                Operation::UnrollIndices => self.unroll_indices(shape),
                &Operation::Subdivide {
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
        debug_assert_eq!(shape.topology, Topology::LineList);

        if let Some(indices) = &mut shape.indices {
            let new_items = (indices.len() / 2) * subdivisions;
            shape
                .attributes
                .iter_mut()
                .for_each(|x| x.reserve(new_items));

            let len = shape.attributes[0].len();

            for chunk in indices.chunks_exact(2) {
                let [a, b] = [chunk[0], chunk[1]];

                for attrib in &mut shape.attributes {
                    fn subdivide_line<T: Vector + Default>(subdivisions: usize, interpolation: &mut LerpParams<T>, attributes: &mut Vec<T>) {
                        attributes.extend(
                            std::iter::repeat_with(Default::default).take(subdivisions),
                        );
                        colour_interpolation.interpolate_multiple(
                            colours[a as usize],
                            colours[b as usize],
                            colours.len() - subdivisions..colours.len(),
                            colours,
                        );
                    }
                    match attrib {
                        VertexAttribute::Colour(colours) => {

                        },
                        VertexAttribute::Position(position) => {
                            position.extend(
                                std::iter::repeat_with(Default::default).take(subdivisions),
                            );
                            position_interpolation.interpolate_multiple(
                                position[a as usize],
                                position[b as usize],
                                position.len() - subdivisions..position.len(),
                                position,
                            );
                        },
                        VertexAttribute::Colour(colours) => {
                            colours.extend(
                                std::iter::repeat_with(Default::default).take(subdivisions),
                            );
                            colour_interpolation.interpolate_multiple(
                                colours[a as usize],
                                colours[b as usize],
                                colours.len() - subdivisions..colours.len(),
                                colours,
                            );
                        },
                        VertexAttribute::Colour(colours) => {
                            colours.extend(
                                std::iter::repeat_with(Default::default).take(subdivisions),
                            );
                            colour_interpolation.interpolate_multiple(
                                colours[a as usize],
                                colours[b as usize],
                                colours.len() - subdivisions..colours.len(),
                                colours,
                            );
                        },
                    }
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
