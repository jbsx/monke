use crate::object::Object;
use std::collections::HashMap;
use std::{cell::RefCell, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    pub store: HashMap<String, Object>,
    pub outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        return Self {
            store: HashMap::new(),
            outer: None,
        };
    }

    pub fn get(&self, name: &String) -> Option<&Object> {
        let res = self.store.get(name);
        match res {
            Some(val) => return Some(val),
            None => match &self.outer {
                Some(val) => {
                    return val.get(name).clone();
                }
                None => return None,
            },
        }
    }

    pub fn set(&mut self, name: String, obj: Object) {
        self.store.insert(name, obj);
    }

    pub fn new_enclosed_env(outer: Rc<RefCell<Environment>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(Box::new(outer.borrow().clone())),
        }
    }
}
