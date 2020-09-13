use core::ops::{FnMut, Add, Mul};
use core::clone::Clone;

pub trait Vector: Clone + Add<Self, Output = Self> + Mul<f32, Output = Self> {
    fn dot(self, other: Self) -> f32;
    fn normalize(self) -> Self;
}

impl Vector for crate::Vec2 {
    fn dot(self, other: Self) -> f32 {
        self.dot(other)
    }
    fn normalize(self) -> Self {
        self.normalize()
    }
}

impl Vector for crate::Vec3 {
    fn dot(self, other: Self) -> f32 {
        self.dot(other)
    }
    fn normalize(self) -> Self {
        self.normalize()
    }
}

impl Vector for crate::Vec4 {
    fn dot(self, other: Self) -> f32 {
        self.dot(other)
    }
    fn normalize(self) -> Self {
        self.normalize()
    }
}

pub enum LerpParams<T: Vector> {
    Lerp,
    Slerp,
    NormalizedLerp,
    CustomFn {
        interpolate: fn(T, T, f32) -> T,
        interpolate_half: Option<fn(T, T) -> T>,
        interpolate_multiple: Option<fn(T, T, &[u32], &mut [T])>,
    },
    CustomFnBox {
        interpolate: Box<dyn FnMut(T, T, f32) -> T>,
        interpolate_half: Option<Box<dyn FnMut(T, T) -> T>>,
        interpolate_multiple: Option<Box<dyn FnMut(T, T, &[u32], &mut [T])>>,
    }
}

impl<T> LerpParams<T> where T: Vector {
    pub fn interpolate(&mut self, a: T, b: T, p: f32) -> T {
        use LerpParams::*;
        match self {
            Lerp => lerp(a, b, p),
            Slerp => geometric_slerp(a, b, p),
            NormalizedLerp => normalized_lerp(a, b, p),
            CustomFn {
                interpolate,
                ..
            } => interpolate(a, b, p),
            CustomFnBox {
                interpolate,
                ..
            } => interpolate(a, b, p),
        }
    }

    pub fn interpolate_half(&mut self, a: T, b: T) -> T {
        use LerpParams::*;
        match self {
            Lerp => lerp_half(a, b),
            Slerp => geometric_slerp_half(a, b),
            NormalizedLerp => normalized_lerp_half(a, b),
            CustomFn {
                interpolate_half: Some(interpolate),
                ..
            } => interpolate(a, b),
            CustomFn {
                interpolate,
                interpolate_half: None,
                ..
            } => interpolate(a, b, 0.5),
            CustomFnBox {
                interpolate_half: Some(interpolate),
                ..
            } => interpolate(a, b),
            CustomFnBox {
                interpolate,
                interpolate_half: None,
                ..
            } => interpolate(a, b, 0.5),
        }
    }

    pub fn interpolate_multiple(&self, a: T, b: T, indices: &[u32], )
}

impl<T: Vector> Default for LerpParams<T> {
    fn default() -> Self {
        LerpParams::Lerp
    }
}

impl<T: Vector> std::fmt::Debug for LerpParams<T> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LerpParams::Lerp => write!(fmt, "Lerp"),
            LerpParams::Slerp => write!(fmt, "Slerp"),
            LerpParams::NormalizedLerp => write!(fmt, "NormalizedLerp"),
            LerpParams::CustomFn { .. } => write!(fmt, "CustomFn {{ Custom Implementation }}"),
            LerpParams::CustomFnBox { .. } => write!(fmt, "CustomFnBox {{ Custom Implementation }}"),
        }
    }
}

///
/// Implements spherical interpolation along the great arc created by
/// the initial points. This returns a new point `p` percent of the way
/// along that arc.
///
/// Note: `a` and `b` should both be normalized for normalized results.
///
pub fn geometric_slerp<T: Vector>(a: T, b: T, p: f32) -> T {
    let angle = a.clone().dot(b.clone()).acos();

    let sin = angle.sin().recip();
    a * (((1.0 - p) * angle).sin() * sin) + b * ((p * angle).sin() * sin)
}

///
/// This is an optimization for the `geometric_slerp` in the case where `p`
/// is `0.5` or 50%.
///
/// Note: `a` and `b` should both be normalized for normalized results.
///
pub fn geometric_slerp_half<T: Vector>(a: T, b: T) -> T {
    (a.clone() + b.clone()) * (2.0 * (1.0 + a.dot(b))).sqrt().recip()
}

///
/// This is an optimization for the case where multiple points require the
/// calculation of varying values of `p` for the same start and end points.
///
/// See the intended use in [`BaseShape::interpolate_multiple`].
///
/// Note: `a` and `b` should both be normalized for normalized results.
///
pub fn geometric_slerp_multiple<T: Vector>(a: T, b: T, indices: &[u32], points: &mut [T]) {
    let angle = a.clone().dot(b.clone()).acos();
    let sin = angle.sin().recip();

    for (percent, index) in indices.iter().enumerate() {
        let percent = (percent + 1) as f32 / (indices.len() + 1) as f32;

        points[*index as usize] =
            a.clone() * (((1.0 - percent) * angle).sin() * sin) + b.clone() * ((percent * angle).sin() * sin);
    }
}

///
/// Performs normalized linear interpolation. This creates distortion when
/// compared with spherical interpolation along an arc, however this is most
/// likely faster, as though this avoids expensive sin and acos calculations.
///
pub fn normalized_lerp<T: Vector>(a: T, b: T, p: f32) -> T {
    lerp(a, b, p).normalize()
}

///
/// This is an optimization of `normalized_lerp` which avoids a multiplication.
///
pub fn normalized_lerp_half<T: Vector>(a: T, b: T) -> T {
    lerp_half(a, b).normalize()
}

///
/// This is provided as a plug in for people who need it, but this implements
/// essentially the same algorithm as `BaseShape` would without ever being
/// reimplemented.
///
pub fn normalized_lerp_multiple<T: Vector>(a: T, b: T, indices: &[u32], points: &mut [T]) {
    for (percent, index) in indices.iter().enumerate() {
        let percent = (percent + 1) as f32 / (indices.len() + 1) as f32;

        points[*index as usize] = (a.clone() * (1.0 - percent) + b.clone() * percent).normalize();
    }
}

///
/// Simple linear interpolation. No weirdness here.
///
pub fn lerp<T: Vector>(a: T, b: T, p: f32) -> T {
    a * (1.0 - p) + b * p
}

///
/// Gives the average of the two points.
///
pub fn lerp_half<T: Vector>(a: T, b: T) -> T {
    (a + b) * 0.5
}

///
/// This is provided as a plug in for people who need it, but this implements
/// essentially the same algorithm as `BaseShape` would without ever being
/// reimplemented.
///
pub fn lerp_multiple<T: Vector>(a: T, b: T, indices: &[u32], points: &mut [T]) {
    for (percent, index) in indices.iter().enumerate() {
        let percent = (percent + 1) as f32 / (indices.len() + 1) as f32;

        points[*index as usize] = a.clone() * (1.0 - percent) + b.clone() * percent;
    }
}
