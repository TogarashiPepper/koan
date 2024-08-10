use inkwell::values::{FloatValue, StructValue};

pub enum LLValue<'a> {
    Float(FloatValue<'a>),
    Array(StructValue<'a>),
}

impl<'a> LLValue<'a> {
    pub(crate) fn map_flt<F>(mut self, op: F) -> Self
    where
        // >>=
        F: FnOnce(FloatValue<'a>) -> LLValue<'a>
    {
        if let LLValue::Float(fvalue) = self {
            self = op(fvalue);
        }

        self
    }


    pub(crate) fn map_arr<F>(mut self, op: F) -> Self
    where
        F: FnOnce(StructValue<'a>) -> LLValue<'a>
    {
        if let LLValue::Array(avalue) = self {
            self = op(avalue);
        }

        self
    }
}
