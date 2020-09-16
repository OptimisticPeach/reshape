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
                let (a, b) = (indices[a_idx], indices[b_idx]);

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
                            .extend((0..subdivisions).map(|_| Default::default()));
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

                indices.extend_from_slice(&[a, old_len, b, new_len - 1]);

                for i in old_len..new_len {
                    indices.extend_from_slice(&[i, i + 1]);
                }
            }
        } else {
            let new_items = (shape.attributes[0].len() / 2) * subdivisions;
            shape
                .attributes
                .iter_mut()
                .for_each(|x| x.reserve(new_items));
            for i in 0..shape.attributes[0].len() / 2 {
                let a_idx = i * 2 + i * subdivisions * 2;

                for attrib in &mut shape.attributes {
                    fn subdivide_line_noindices<T: Vector + Default + Copy>(
                        subdivisions: usize,
                        interpolation: &mut LerpParams<T>,
                        attributes: &mut Vec<T>,
                        a: usize,
                    ) {
                        attributes
                            .splice(a + 1..a + 1, (0..subdivisions * 2).map(|_| Default::default()))
                            .for_each(drop);
                        interpolation.interpolate_multiple(
                            attributes[a].clone(),
                            attributes[a + subdivisions + 1].clone(),
                            a + 1..a + subdivisions + 1,
                            attributes,
                        );
                        copy_interlaced(&mut attributes[a + 1..a + 1 + subdivisions * 2]);
                    }
                    match attrib {
                        VertexAttribute::Colour(colours) => {
                            subdivide_line_noindices(subdivisions, colour_interpolation, colours, a_idx)
                        }
                        VertexAttribute::Position(positions) => {
                            subdivide_line_noindices(subdivisions, position_interpolation, positions, a_idx)
                        }
                        VertexAttribute::UV(uvs) => {
                            subdivide_line_noindices(subdivisions, uv_interpolation, uvs, a_idx)
                        }
                        VertexAttribute::Normal(normals) => {
                            subdivide_line_noindices(subdivisions, normal_interpolation, normals, a_idx);
                        }
                    }
                }
            }
        }
    }

    fn subdivide_triangles_cw(
        &mut self,
        _shape: &mut Shape,
        _subdivisions: usize,
        _position_interpolation: &mut LerpParams<Vec3>,
        _colour_interpolation: &mut LerpParams<Vec4>,
        _uv_interpolation: &mut LerpParams<Vec2>,
        _normal_interpolation: Option<&mut LerpParams<Vec3>>,
    ) {
        unimplemented!()
    }

    fn subdivide_triangles_ccw(
        &mut self,
        _shape: &mut Shape,
        _subdivisions: usize,
        _position_interpolation: &mut LerpParams<Vec3>,
        _colour_interpolation: &mut LerpParams<Vec4>,
        _uv_interpolation: &mut LerpParams<Vec2>,
        _normal_interpolation: Option<&mut LerpParams<Vec3>>,
    ) {
        unimplemented!()
    }
}

///
/// Copies `[A, B, _, _]` to `[A, A, B, B]` in place
/// for an arbitrary size of `[A, B, C, .., _, _, _, ..]`
/// really fast.
///
fn copy_interlaced<T: Copy>(slice: &mut [T]) {
    if slice.len() == 1 {
        return;
    }
    unsafe {
        #[inline]
        unsafe fn interlace_mul4<T: Copy>(slice: &mut [T]) {
            for i in (0..slice.len() / 4).rev() {
                *slice.get_unchecked_mut(i * 4 + 3) = *slice.get_unchecked(i * 2 + 1);
                *slice.get_unchecked_mut(i * 4 + 2) = *slice.get_unchecked(i * 2 + 1);
                *slice.get_unchecked_mut(i * 4 + 1) = *slice.get_unchecked(i * 2 + 0);
                *slice.get_unchecked_mut(i * 4 + 0) = *slice.get_unchecked(i * 2 + 0);
            }
        }
        if slice.len() % 4 == 2 {
            let old_slice = slice;
            let slice = &mut old_slice[2..];
            interlace_mul4(slice);
            *old_slice.get_unchecked_mut(1) = *old_slice.get_unchecked(0);
        } else {
            interlace_mul4(slice)
        }
    }
}
