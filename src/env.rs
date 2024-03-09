use crate::object::Object;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new_env() -> Environment {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: &String) -> Option<&Object> {
        return self.store.get(name);
    }

    pub fn set(&mut self, name: String, obj: Object) {
        self.store.insert(name, obj);
    }
}
