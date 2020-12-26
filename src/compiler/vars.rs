use super::Err;
use crate::ast::*;
use crate::primitives::{Atom, AtomContent};
use std::collections::HashMap;
use std::path::PathBuf;

pub(super) struct Propergation;

impl super::Pass for Propergation {
    const NAME: &'static str = "vars-propergation";

    fn exec<'a>(&mut self, config: &mut Vec<Item<'a>>, _path: PathBuf) -> Result<(), Err> {
        let mut namespace: HashMap<String, SetVar> = HashMap::new();

        for i in config.iter_mut() {
            let namespace = &mut namespace;
            i.visit::<Err, _>(move |i| {
                match i {
                    Item::Set(s) => {
                        let name = if let AtomContent::Var(name) = &s.variable.content {
                            name
                        } else {
                            panic!("s.variable was not of the Var invariant");
                        };

                        // First, propergate any variables the assignment might
                        // itself depend on.
                        let resolved: Vec<Atom> = s
                            .values
                            .iter()
                            .map(|v| {
                                if let AtomContent::Var(var) = &v.content {
                                    if let Some(out_val) = namespace.get(var) {
                                        out_val.resolved_values.clone().unwrap()
                                    } else {
                                        vec![v.clone()]
                                    }
                                } else {
                                    vec![v.clone()]
                                }
                            })
                            .flatten()
                            .collect();
                        s.resolved_values = Some(resolved);

                        // Last, store the resolved values against the running namespace.
                        namespace.insert(name.clone(), s.clone());
                        Ok(())
                    }

                    _ => Ok(()),
                }
            })
            .map_err(|e| match e {
                TraversalError::Visit(e) => e,
            })?;
        }
        Ok(())
    }
}
