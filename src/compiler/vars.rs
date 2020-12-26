use super::Err;
use crate::ast::*;
use crate::primitives::{Atom, AtomContent};
use std::collections::HashMap;
use std::path::PathBuf;

pub(super) struct Propergation;

impl Propergation {
    fn exec<'a>(
        mut namespace: &mut HashMap<String, SetVar>,
        config: &mut Vec<Item<'a>>,
        path: PathBuf,
    ) -> Result<(), Err> {
        for i in config.iter_mut() {
            let namespace = &mut namespace;
            let path = path.clone();
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

                    Item::BindSym(b) => {
                        let resolved: Vec<bind::Key> = b
                            .keys
                            .iter()
                            .map(|v| {
                                if let bind::Key::Var(var) = &v {
                                    if let Some(out_val) = namespace.get(var) {
                                        // TODO: Parse each bit of the variable properly, so
                                        // as to support values like '+ Control'.
                                        out_val
                                            .resolved_values
                                            .clone()
                                            .unwrap()
                                            .into_iter()
                                            .map(|a| bind::Key::Key(a.content.into()))
                                            .collect()
                                    } else {
                                        vec![v.clone()]
                                    }
                                } else {
                                    vec![v.clone()]
                                }
                            })
                            .flatten()
                            .collect();

                        b.resolved_keys = Some(resolved);

                        // TODO: Handle unresolved args.
                        Ok(())
                    }

                    Item::Exec(e) => {
                        let resolved: Vec<Atom> = e
                            .args
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
                        e.resolved_args = Some(resolved);
                        Ok(())
                    }

                    Item::RuntimeResolvable(r) => {
                        if let AtomContent::Var(v) = &r.cmd.content {
                            if let Some(val) = namespace.get(v) {
                                let mut args: Vec<Atom> = r
                                    .atoms
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

                                let mut resolved = val.resolved_values.clone().unwrap();
                                resolved.append(&mut args);

                                if let Ok(item) = parse_line(r.line.clone(), resolved) {
                                    let mut tmp = vec![item];
                                    Propergation::exec(namespace, &mut tmp, path.clone())?;
                                    r.resolved_item = Some(Box::new(tmp.pop().unwrap()));
                                }
                            }
                        }
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

impl super::Pass for Propergation {
    const NAME: &'static str = "vars-propergation";

    fn exec<'a>(&mut self, config: &mut Vec<Item<'a>>, path: PathBuf) -> Result<(), Err> {
        let mut namespace: HashMap<String, SetVar> = HashMap::new();

        Propergation::exec(&mut namespace, config, path)
    }
}
