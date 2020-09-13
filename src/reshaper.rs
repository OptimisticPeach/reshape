use crate::{Operation, Shape};

pub trait ReshapeImpl {
    fn execute_all(&mut self, operations: &[Operation], shape: &mut Shape) {
        for operation in operations {
            match operation {
                Operation::CreateDefaultIndices => self.create_default_indices(shape),
                Operation::UnrollIndices => self.unroll_indices(shape),
                Operation::Subdivide {
                    ..
                } => {}
            }
        }
    }

    fn unroll_indices(&mut self, shape: &mut Shape);

    fn create_default_indices(&mut self, shape: &mut Shape);
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
}
