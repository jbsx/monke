#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    INT(i32),
    BOOL(bool),
    NULL(),
    RETURN(Box<Object>),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Object::INT(val) => return write!(f, "{}", val),
            Object::BOOL(val) => return write!(f, "{}", val),
            Object::NULL() => return write!(f, "NULL"),
            Object::RETURN(val) => return write!(f, "{}", *val),
        }
    }
}
