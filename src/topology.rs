#[non_exhaustive]
#[derive(Copy, Clone, Debug, PartialEq, Hash)]
pub enum Topology {
    TriangleListCCW,
    TriangleListCW,
    LineList,
    // TODO: Implement these
    // TriangleStripCCW,
    // TriangleStripCW,
    // LineStrip,
    // PointList,
}
