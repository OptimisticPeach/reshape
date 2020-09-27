use crate::{LerpParams, Operation, Shape, Topology, Vector, VertexAttribute};
use crate::{Vec2, Vec3, Vec4};
use std::ops::{Index, Range};
use std::collections::HashMap;

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
        shape: &mut Shape,
        subdivisions: usize,
        position_interpolation: &mut LerpParams<Vec3>,
        colour_interpolation: &mut LerpParams<Vec4>,
        uv_interpolation: &mut LerpParams<Vec2>,
        normal_interpolation: Option<&mut LerpParams<Vec3>>,
    ) {
        subdivide_triangles(TriangleSubdivideParams {
            shape,
            subdivisions,
            position_interpolation,
            colour_interpolation,
            uv_interpolation,
            normal_interpolation,
            is_ccw: true,
        })
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
        subdivide_triangles(TriangleSubdivideParams {
            shape,
            subdivisions,
            position_interpolation,
            colour_interpolation,
            uv_interpolation,
            normal_interpolation,
            is_ccw: false,
        })
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

struct TriangleSubdivideParams<'a> {
    pub is_ccw: bool,
    pub shape: &'a mut Shape,
    pub subdivisions: usize,
    pub position_interpolation: &'a mut LerpParams<Vec3>,
    pub colour_interpolation: &'a mut LerpParams<Vec4>,
    pub uv_interpolation: &'a mut LerpParams<Vec2>,
    pub normal_interpolation: Option<&'a mut LerpParams<Vec3>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Hash)]
enum RevRange {
    Forward(u32, u32),
    Backward(u32, u32),
}

impl RevRange {
    pub fn forward(range: Range<u32>, ccw: bool) -> Self {
        if ccw {
            RevRange::Forward(range.start, range.end)
        } else {
            RevRange::Backward(range.start, range.end)
        }
    }

    pub fn backward(range: Range<u32>, ccw: bool) -> Self {
        if ccw {
            RevRange::Backward(range.start, range.end)
        } else {
            RevRange::Forward(range.start, range.end)
        }
    }

    pub fn index<T>(&self, slice: &[T], index: usize) -> &[T] {
        match self {
            RevRange::Forward(start, end) => {
                &slice[start as usize..end as usize][index]
            },
            RevRange::Backward(start, end) => {
                &slice[start as usize..end as usize][(end as usize - start as usize) - index - 1]
            },
        }
    }

    pub fn index_mut<T>(&self, slice: &mut [T], index: usize) -> &mut [T] {
        match self {
            RevRange::Forward(start, end) => {
                &mut slice[start as usize..end as usize][index]
            },
            RevRange::Backward(start, end) => {
                &mut slice[start as usize..end as usize][(end as usize - start as usize) - index - 1]
            },
        }
    }

    pub fn rev(self) -> Self {
        match self {
            RevRange::Forward(start, end) => RevRange::Backward(start, end),
            RevRange::Backward(start, end) => RevRange::Forward(start, end),
        }
    }

    pub fn len(&self) -> u32 {
        match self {
            RevRange::Forward(start, end) | RevRange::Backward(start, end) => (*end - *start) as u32
        }
    }

    pub fn first(&self) -> u32 {
        match self {
            RevRange::Forward(start, _) => *start,
            RevRange::Backward(_, start) => *start,
        }
    }

    pub fn last(&self) -> u32 {
        match self {
            RevRange::Forward(_, end) => *end - 1,
            RevRange::Backward(end, _) => *end + 1,
        }
    }

    pub fn get(&self, n: usize) -> u32 {
        if n >= self.len() as usize {
            panic!("Index {} is out of range", n);
        }
        match self {
            RevRange::Forward(start, _) => *start + n,
            RevRange::Backward(_, start) => *start - n,
        }
    }
}

impl<'a> Iterator for &'a mut RevRange {
    type Item = u32;
    fn next(&mut self) -> Option<u32> {
        match self {
            RevRange::Forward(start, end) => {
                if *start == *end {
                    return None;
                }

                let temp = *start;
                *start += 1;
                Some(temp)
            },
            RevRange::Backward(end, start) => {
                if *start == *end {
                    return None;
                }

                let temp = *start;
                *start -= 1;
                Some(temp)
            },
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl<'a> DoubleEndedIterator for &'a mut RevRange {
    fn next_back(&mut self) -> Option<u32> {
        match self {
            RevRange::Forward(start, end) => {
                if *start == *end {
                    return None;
                }

                let temp = *end;
                *end -= 1;
                Some(temp)
            },
            RevRange::Backward(end, start) => {
                if *start == *end {
                    return None;
                }

                let temp = *end;
                *end += 1;
                Some(temp)
            },
        }
    }
}

impl<'a> ExactSizeIterator for &'a mut RevRange {
    fn len(&self) -> usize {
        RevRange::len(&*self) as usize
    }

    fn is_empty(&self) -> bool {
        match self {
            RevRange::Forward(start, end) | RevRange::Backward(start, end) => *start == *end
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
struct IndexedTriangle {
    a: u32,
    b: u32,
    c: u32,
    ab: RevRange,
    bc: RevRange,
    ca: RevRange,
}

macro_rules! indexed_triangle_idx {
    ($name:ident, $first:ident, $last:ident, $both:ident) => {
        fn $name(&self, idx: usize) -> u32 {
            if idx == 0 {
                self.$first
            } else if idx == 1 + self.$both.len() {
                self.$last
            } else {
                self.$both.nth(idx - 1)
            }
        }
    };
}

impl IndexedTriangle {
    fn new(a: u32, b: u32, c: u32, ab: RevRange, bc: RevRange, ca: RevRange) -> Self {
        Self {
            a,
            b,
            c,
            ab,
            bc,
            ca,
        }
    }
    indexed_triangle_idx!(idx_ab, a, b, ab);
    indexed_triangle_idx!(idx_bc, b, c, bc);
    indexed_triangle_idx!(idx_ca, c, a, ca);
}

fn subdivide_line<T: Vector + Default>(a: u32, b: u32, interpolation: &mut LerpParams<T>, subdivisions: usize, data: &mut Vec<T>) {
    data.extend((0..subdivisions).map(|_| Default::default()));

    interpolation.interpolate_multiple(data[a as usize].clone(), data[b as usize].clone(), data.len() - 1 - subdivisions..data.len(), &mut *data);
}

fn subdivide_half<T: Vector + Default>(a: u32, b: u32, interpolation: &mut LerpParams<T>, data: &mut Vec<T>) {
    let temp = interpolation.interpolate_half(data[a as usize].clone(), data[b as usize].clone());
    data.push(temp);
}

macro_rules! subdivide_all {
    (half: $a:expr, $b:expr, $params:expr) => {
        for attribute in &mut $params.shape.attributes {
            use VertexAttribute::*;
            match attribute {
                Colour(x) => subdivide_half($a, $b, &mut $params.colour_interpolation, x),
                Position(x) => subdivide_half($a, $b, &mut $params.position_interpolation, x),
                UV(x) => subdivide_half($a, $b, &mut $params.uv_interpolation, x),
                Normal(x) => {
                    if let Some(normal_interpolation) = &mut $params.normal_interpolation {
                        subdivide_half($a, $b, normal_interpolation, x);
                    } else {
                        x.push(Default::default());
                    }
                }
            }
        }
    };
    (line: $a:expr, $b:expr, $subdivisions:expr, $params:expr) => {
        for attribute in &mut $params.shape.attributes {
            use VertexAttribute::*;
            match attribute {
                Colour(x) => subdivide_line($a, $b, &mut $params.colour_interpolation, $subdivisions, x),
                Position(x) => subdivide_half($a, $b, &mut $params.position_interpolation, $subdivisions, x),
                UV(x) => subdivide_half($a, $b, &mut $params.uv_interpolation, $subdivisions, x),
                Normal(x) => {
                    if let Some(normal_interpolation) = &mut $params.normal_interpolation {
                        subdivide_half($a, $b, normal_interpolation, $subdivisions, x);
                    } else {
                        x.extend((0..$subdivisions).map(|_| Default::default()));
                    }
                }
            }
        }
    };
}

macro_rules! len {
    ($shape:expr) => {
        $shape.attributes[0].len()
    };
}

impl IndexedTriangle {
    fn add_indices(&self, next: Option<&Self>, indices: &mut Vec<u32>) {
        match (self.ab.len(), next) {
            (0, None) => {
                indices.extend_from_slice(&[self.a, self.b, self.c]);
            },
            (1, None) => {
                indices.extend_from_slice(
                    &[
                        // Triangle A
                        self.a,
                        self.ab.first(),
                        self.ca.first(),
                        // Triangle B
                        self.b,
                        self.bc.first(),
                        self.ab.first(),
                        // Triangle C
                        self.c,
                        self.ca.first(),
                        self.bc.first(),
                    ]
                );
            },
            (
                2,
                Some(
                    IndexedTriangle {
                        a: x,
                        b,
                        c,
                        ..
                     }
                )
            ) if *x == *b && *b == *c => {
                indices.extend_from_slice(
                    &[
                        // Triangle A
                        self.a,
                        self.ab.first(),
                        self.ca.last(),
                        // Triangle B
                        self.b,
                        self.bc.first(),
                        self.ab.last(),
                        // Triangle C
                        self.c,
                        self.ca.first(),
                        self.bc.last(),

                        // Adjacent to outer triangles:
                        // Adj. A
                        self.ca.last(),
                        self.ab.first(),
                        x,
                        // Adj. B
                        self.ab.last(),
                        self.bc.first(),
                        x,
                        // Adj. C
                        self.bc.last(),
                        self.ca.first(),
                        x,

                        // Opposite to outer triangles:
                        // Opp. A
                        self.bc.first(),
                        self.bc.last(),
                        x,
                        // Opp. B
                        self.ca.first(),
                        self.ca.last(),
                        x,
                        // Opp. C
                        self.ab.first(),
                        self.ab.last(),
                        x
                    ]
                );
            },
            (side_length, Some(inner)) => {
                indices.extend_from_slice(
                    &[
                        // Triangle A
                        self.a,
                        self.ab.first(),
                        self.ca.last(),
                        // Triangle B
                        self.b,
                        self.bc.first(),
                        self.ab.last(),
                        // Triangle C
                        self.c,
                        self.ca.first(),
                        self.bc.last(),

                        // Adjacent to outer triangles:
                        // Adj. A
                        self.ca.last(),
                        self.ab.first(),
                        inner.idx_ab(0),
                        // Adj. B
                        self.ab.last(),
                        self.bc.first(),
                        inner.idx_bc(0),
                        // Adj. C
                        self.bc.last(),
                        self.ca.first(),
                        inner.idx_ca(0),

                        // Err, not sure how to explain this one,
                        // it's adjacent to the triangles adjacent
                        // to the outer triangles, while sharing a
                        // vertex with the outer triangle, and is
                        // in the backwards direction.
                        self.ca.last(),
                        inner.idx_ab(0),
                        (&mut self.ca.clone()).nth_back(1).unwrap(),

                        self.ab.last(),
                        inner.idx_bc(0),
                        (&mut self.ab.clone()).nth_back(1).unwrap(),

                        self.bc.last(),
                        inner.idx_ca(0),
                        (&mut self.bc.clone()).nth_back(1).unwrap(),
                    ],
                );
                debug_assert!(side_length >= 3);

                indices.reserve((self.ab.len() as usize - 2) * 18);
                for idx in 0..self.ab.len() as usize - 2 {
                    indices.extend_from_slice(
                        &[
                            self.ab.get(idx),
                            inner.idx_ab(idx),
                            self.ab.get(idx + 1),

                            self.ab.get(idx + 1),
                            inner.idx_ab(idx),
                            inner.idx_ab(idx + 1),
                            //
                            self.bc.get(idx),
                            inner.idx_bc(idx),
                            self.bc.get(idx + 1),

                            self.bc.get(idx + 1),
                            inner.idx_bc(idx),
                            inner.idx_bc(idx + 1),
                            //
                            self.ca.get(idx),
                            inner.idx_ca(idx),
                            self.ca.get(idx + 1),

                            self.ca.get(idx + 1),
                            inner.idx_ca(idx),
                            inner.idx_ca(idx + 1),
                        ]
                    );
                }
            },
            _ => panic!("Invalid arguments to `add_indices`"),
        }
    }

    fn make_next_triangle(&self, params: &mut TriangleSubdivideParams) -> Option<Self> {
        match self.ab.len() {
            0 | 1 => None,
            2 => {
                subdivide_all!(half: self.ab.first(), self.bc.last(), params);
                let len = len!(params.shape) as u32;
                Some(
                    Self {
                        a: len - 1,
                        b: len - 1,
                        c: len - 1,
                        ab: RevRange::forward(0..0, params.is_ccw),
                        bc: RevRange::forward(0..0, params.is_ccw),
                        ca: RevRange::forward(0..0, params.is_ccw),
                    }
                )
            },
            3 => {
                subdivide_all!(half: self.ca.get(1), self.ab.get(1), params);
                subdivide_all!(half: self.ab.get(1), self.bc.get(1), params);
                subdivide_all!(half: self.bc.get(1), self.ca.get(1), params);
                let len = len!(params.shape) as u32;
                Some(
                    Self {
                        a: len - 3,
                        b: len - 2,
                        c: len - 1,
                        ab: RevRange::forward(0..0, params.is_ccw),
                        bc: RevRange::forward(0..0, params.is_ccw),
                        ca: RevRange::forward(0..0, params.is_ccw),
                    }
                )
            },
            n => {
                let a = len!(params.shape) as u32;
                subdivide_all!(half: self.ca.clone().rev().get(1), self.ab.get(1), params);
                let b = len!(params.shape) as u32;
                subdivide_all!(half: self.ab.clone().rev().get(1), self.bc.get(1), params);
                let c = len!(params.shape) as u32;
                subdivide_all!(half: self.bc.clone().rev().get(1), self.ca.get(1), params);

                let inner_length = n - 3;

                let ab_start = len!(params.shape) as u32;
                subdivide_all!(line: a, b, inner_length, params);
                let ab = RevRange::forward(ab_start..ab_start + inner_length, params.is_ccw);

                let bc_start = len!(params.shape) as u32;
                subdivide_all!(line: b, c, inner_length, params);
                let bc = RevRange::forward(bc_start..bc_start + inner_length, params.is_ccw);

                let ca_start = len!(params.shape) as u32;
                subdivide_all!(line: c, a, inner_length, params);
                let ca = RevRange::forward(ca_start..ca_start + inner_length, params.is_ccw);

                Some(
                    Self {
                        a,
                        b,
                        c,
                        ab,
                        bc,
                        ca,
                    }
                )
            },
        }
    }
}

fn subdivide_triangles(mut triangle_params: TriangleSubdivideParams) {
    // Created from a counter clockwise schematic, but
    // is agnostic over the winding because triangle_params
    // creates slices with the correct direction.

    if let Some(old_indices) = triangle_params.shape.indices.take() {
        let mut indices = Vec::new();

        let mut edge_hashmap = HashMap::<(u32, u32), RevRange>::new();

        for triangle_indices in old_indices.chunks_exact(3) {
            let a = triangle_indices[0];
            let b = triangle_indices[1];
            let c = triangle_indices[2];

            let ab = if let Some(x) = edge_hashmap.get(&(a, b)).or(edge_hashmap.get(&(b, a))).cloned() {
                x.rev()
            } else {
                let ab_start = len!(triangle_params.shape) as u32;
                subdivide_all!(line: a, b, triangle_params.subdivisions, triangle_params);
                let range = RevRange::forward(ab_start..ab_start + triangle_params.subdivisions as u32, triangle_params.is_ccw);
                edge_hashmap.insert((a, b), range);

                range
            };

            let bc = if let Some(x) = edge_hashmap.get(&(b, c)).or(edge_hashmap.get(&(c, b))).cloned() {
                x.rev()
            } else {
                let bc_start = len!(triangle_params.shape) as u32;
                subdivide_all!(line: b, c, triangle_params.subdivisions, triangle_params);
                let range = RevRange::forward(bc_start..bc_start + triangle_params.subdivisions as u32, triangle_params.is_ccw);
                edge_hashmap.insert((b, c), range);

                range
            };

            let ca = if let Some(x) = edge_hashmap.get(&(c, a)).or(edge_hashmap.get(&(a, c))).cloned() {
                x
            } else {
                let ca_start = len!(triangle_params.shape) as u32;
                subdivide_all!(line: c, a, triangle_params.subdivisions, triangle_params);
                let range = RevRange::forward(ca_start..ca_start + triangle_params.subdivisions as u32, triangle_params.is_ccw);
                edge_hashmap.insert((c, a), range);

                range
            };

            let mut triangle = IndexedTriangle::new(a, b, c, ab, bc, ca);
            let mut next = triangle.make_next_triangle(&mut triangle_params);

            loop {
                triangle.add_indices(next.as_ref(), &mut indices);
                if let Some(old_next) = &mut next {
                    let new_next = old_next.make_next_triangle(&mut triangle_params);
                    std::mem::swap(&mut triangle, old_next);
                    next = new_next;
                } else {
                    break;
                }
            }
        }

        triangle_params.shape.indices = Some(indices);
    } else {
        todo!();
    }
}
