use glam::Vec4;

pub trait IntoVec4Colour {
    fn into_colour(self) -> Vec4;
}

impl IntoVec4Colour for [u8; 3] {
    fn into_colour(self) -> Vec4 {
        Vec4::new(
            self[0] as f32 / 255.0,
            self[1] as f32 / 255.0,
            self[2] as f32 / 255.0,
            1.0,
        )
    }
}

impl IntoVec4Colour for [u8; 4] {
    fn into_colour(self) -> Vec4 {
        Vec4::new(
            self[0] as f32 / 255.0,
            self[1] as f32 / 255.0,
            self[2] as f32 / 255.0,
            self[3] as f32 / 255.0,
        )
    }
}

impl IntoVec4Colour for [f32; 3] {
    fn into_colour(self) -> Vec4 {
        Vec4::new(self[0], self[1], self[2], 1.0)
    }
}

impl IntoVec4Colour for [f32; 4] {
    fn into_colour(self) -> Vec4 {
        self.into()
    }
}

impl IntoVec4Colour for glam::Vec3 {
    fn into_colour(self) -> Vec4 {
        self.extend(1.0)
    }
}

impl IntoVec4Colour for glam::Vec3A {
    fn into_colour(self) -> Vec4 {
        self.extend(1.0)
    }
}

impl IntoVec4Colour for Vec4 {
    fn into_colour(self) -> Vec4 {
        self
    }
}
