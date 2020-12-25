use crate::{layout, primitives};
use primitives::{Atom, AtomContent, Token};
use serde::{Deserialize, Serialize};

pub mod bind;

/// A fatal error returned by the parser.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Err<'a> {
    #[serde(borrow)]
    pub stanza: layout::Stanza<'a>,
    pub err: String,
}

/// A line which was not understood by the parser.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct Unknown<'a> {
    pub line: primitives::Line<'a>,
}

/// An expression which sets a variable to a specific value.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct SetVar {
    pub cmd: Atom,
    pub variable: Atom,
    pub values: Vec<Atom>,
    pub resolved_values: Option<Vec<Atom>>,
}

impl SetVar {
    /// Indicates if the variable should be expanded at runtime.
    pub fn expand_at_runtime(&self) -> bool {
        if let AtomContent::Var(v) = &self.variable.content {
            return v.starts_with("$");
        }
        unreachable!();
    }

    /// The name of the variable, with leading dollar sign tokens removed.
    pub fn name(&self) -> String {
        if let AtomContent::Var(v) = &self.variable.content {
            if self.expand_at_runtime() {
                return v[1..].to_string();
            } else {
                return v.to_string();
            }
        }
        unreachable!();
    }
}

/// An expression runs the specified arguments in a shell subprocess.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct Exec {
    pub cmd: Atom,
    pub args: Vec<Atom>,
    pub resolved_args: Option<Vec<Atom>>,
}

impl Exec {
    /// Returns the atoms making up the subshell invocation.
    pub fn command_atoms(&self) -> &Vec<Atom> {
        &self.args
    }
}

/// An expression symbolizing switching to a specific mode.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct SwitchMode {
    pub cmd: Atom,
    pub mode: Atom,
}

/// An expression describing a key binding.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct BindSym<'a> {
    pub cmd: Atom,
    pub flags: bind::FLAGS,
    pub keys: Vec<bind::Key>,
    pub resolved_keys: Option<Vec<bind::Key>>,
    pub args: Subset<'a>,
}

/// An expression describing a subcommand or an unresolved set of tokens.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub enum Subset<'a> {
    Unresolved(Vec<Atom>),
    Item(Box<Item<'a>>),
}

/// An expression whose command must be resolved at runtime.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct RuntimeResolvable<'a> {
    pub cmd: Atom,
    pub atoms: Vec<Atom>,
    pub resolved_item: Option<Box<Item<'a>>>,
}

impl RuntimeResolvable<'_> {
    /// The name of the variable, with leading dollar sign tokens removed.
    pub fn name(&self) -> String {
        if let AtomContent::Var(v) = &self.cmd.content {
            v.to_string()
        } else {
            unreachable!();
        }
    }
}

/// A subtree from a successfully-included configuration file.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct ResolvedInclude<'a> {
    pub path: std::path::PathBuf,
    pub ast: Vec<Item<'a>>,
}

/// An expression symbolizing inclusion of config from other files.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct Include<'a> {
    pub cmd: Atom,
    pub glob: Atom,
    pub resolved: Vec<ResolvedInclude<'a>>,
}

/// Represents a sway command. This may not directly correspond to a
/// line in the config, due to possible line continuations or blocks.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case", tag = "type")]
pub enum Item<'a> {
    /// A comment, leading with a pound symbol.
    #[serde(borrow)]
    Comment(primitives::Line<'a>),
    /// A 'set' command.
    Set(SetVar),
    /// An 'exec' command.
    Exec(Exec),
    /// A command to switch to the specified mode.
    SwitchMode(SwitchMode),
    /// A command which maps a key combination to a sway command.
    BindSym(BindSym<'a>),
    /// A set of commands within a block.
    Nested {
        l_brace: Token<'a>,
        r_brace: Token<'a>,
        nested: Vec<Item<'a>>,
    },
    /// A command which can only be resolved at runtime (because
    /// the first atom references a variable).
    RuntimeResolvable(RuntimeResolvable<'a>),
    /// A command to include configuration from another file.
    Include(Include<'a>),
    /// A stanza which is invalid or not yet supported.
    Unknown(layout::Stanza<'a>),
}

#[derive(Debug)]
pub enum TraversalError<E> {
    Visit(E),
}

impl<'a> Item<'a> {
    fn traverse_inner<'b, E, F>(&mut self, mut visitor: F) -> Result<F, TraversalError<E>>
    where
        F: FnMut(&mut Item) -> Result<(), E> + 'b,
    {
        match self {
            // Invariants with no interior nodes
            Item::Comment(_) => Ok(visitor),
            Item::Set(_) => Ok(visitor),
            Item::Exec(_) => Ok(visitor),
            Item::SwitchMode(_) => Ok(visitor),
            Item::RuntimeResolvable(_) => Ok(visitor),
            Item::Unknown(_) => Ok(visitor),
            Item::Include(_) => Ok(visitor),

            Item::BindSym(bind_sym) => match &mut bind_sym.args {
                Subset::Unresolved(_) => Ok(visitor),
                Subset::Item(i) => {
                    visitor(i.as_mut()).map_err(|e| TraversalError::Visit(e))?;
                    Ok(visitor)
                }
            },
            Item::Nested { nested, .. } => {
                for i in &mut nested.iter_mut() {
                    visitor = i.traverse_inner(visitor)?;
                    visitor(i).map_err(|e| TraversalError::Visit(e))?;
                }
                Ok(visitor)
            }
        }
    }

    /// Calls the provided closure on all nested items and then itself.
    pub fn visit<'b, E, F>(&mut self, visitor: F) -> Result<(), TraversalError<E>>
    where
        F: FnMut(&mut Item) -> Result<(), E> + 'b,
    {
        let mut visitor = self.traverse_inner(visitor)?;

        visitor(self).map_err(|e| TraversalError::Visit(e))?;
        Ok(())
    }
}

macro_rules! cmd_matcher {
    ($cmd:literal, at_least=2) => {
        ($cmd, true, _)
    };
    ($cmd:literal, at_least=1) => {
        ($cmd, _, _)
    };
    ($cmd:literal, exact=$sz:literal) => {
        ($cmd, _, $sz)
    };
}

pub(crate) fn parse_line<'a>(
    line: primitives::Line<'a>,
    mut atoms: Vec<Atom>,
) -> Result<Item<'a>, Err> {
    let cmd = if atoms.len() > 0 {
        match &atoms[0].content {
            AtomContent::Arg(s) => s.clone().to_lowercase(),
            AtomContent::Var(_) => {
                let cmd = atoms.remove(0);
                return Ok(Item::RuntimeResolvable(RuntimeResolvable {
                    cmd,
                    atoms,
                    resolved_item: None,
                }));
            }
            _ => {
                return Err(Err {
                    stanza: layout::Stanza::Line { line, atoms },
                    err: "command was invalid".to_string(),
                })
            }
        }
    } else {
        return Err(Err {
            stanza: layout::Stanza::Line { line, atoms },
            err: "no command present".to_string(),
        });
    };

    // (cmd as lowercase, at-least-2-args, exact-len)
    match (cmd.as_str(), atoms.len() >= 2, atoms.len()) {
        cmd_matcher!("set", at_least = 2) => {
            let cmd = atoms.remove(0);
            if let AtomContent::Var(_) = &atoms[0].content {
                let variable = atoms.remove(0);
                Ok(Item::Set(SetVar {
                    cmd,
                    variable,
                    values: atoms,
                    resolved_values: None, // Populated during compilation
                }))
            } else {
                Err(Err {
                    stanza: layout::Stanza::Line { line, atoms },
                    err: "1st argument to set command must be a variable".to_string(),
                })
            }
        }
        cmd_matcher!("bindsym", at_least = 2) => bind::parse(line, atoms),
        cmd_matcher!("exec", at_least = 1) => Ok(Item::Exec(Exec {
            cmd: atoms.remove(0),
            args: atoms,
            resolved_args: None, // Populated during compilation
        })),
        cmd_matcher!("mode", exact = 2) => Ok(Item::SwitchMode(SwitchMode {
            cmd: atoms.remove(0),
            mode: atoms.remove(0),
        })),
        cmd_matcher!("include", exact = 2) => Ok(Item::Include(Include {
            cmd: atoms.remove(0),
            glob: atoms.remove(0),
            resolved: Vec::new(), // Populated during compilation
        })),
        _ => Ok(Item::Unknown(layout::Stanza::Line { line, atoms })),
    }

    //Ok(Item::Comment(l))
}

fn parse_inner<'a>(
    stanza: layout::Stanza<'a>,
    outer_prefix_atoms: Vec<Atom>,
) -> Result<Item<'a>, Err> {
    match stanza {
        layout::Stanza::Comment(l) => Ok(Item::Comment(l)),
        layout::Stanza::Line { line, atoms } => {
            let mut inner_atoms = outer_prefix_atoms;
            inner_atoms.extend_from_slice(&atoms);
            parse_line(line, inner_atoms)
        }
        layout::Stanza::Block {
            prefix_atoms,
            nested,
            l_brace,
            r_brace,
            ..
        } => {
            let ast = nested
                .into_iter()
                .map(|s| {
                    let mut atoms = outer_prefix_atoms.clone();
                    atoms.extend_from_slice(&prefix_atoms);
                    parse_inner(s, atoms)
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Item::Nested {
                l_brace,
                r_brace,
                nested: ast,
            })
        }
    }
}

/// Maps config stanzas into their more-detailed AST equivalents.
pub fn parse(stanzas: Vec<layout::Stanza<'_>>) -> Result<Vec<Item<'_>>, Err> {
    let ast = stanzas
        .into_iter()
        .map(|s| parse_inner(s, Vec::new()))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(ast)
}

#[cfg(test)]
mod tests {
    use super::*;
    use layout::parse_stanza;

    use nom::multi::many0;
    // use pretty_assertions::assert_eq as assert_pretty;
    // use std::path::PathBuf;
    // use test_case::test_case;

    #[test]
    fn comments() {
        let input = primitives::Span::new("  #1\n#2nd comment");
        let output = many0(parse_stanza)(input);
        assert!(
            output.is_ok(),
            "layout parse failed! {:?}",
            output.err().unwrap()
        );
        let output = output.unwrap().1;

        let ast = parse(output);
        assert!(ast.is_ok(), "ast parse failed! {:?}", ast.err().unwrap());
        let ast = ast.unwrap();

        assert_eq!(ast.len(), 2);
        assert!(matches!(&ast[0], Item::Comment(l) if l.line == "1"));
        assert!(matches!(&ast[1], Item::Comment(l) if l.line == "2nd comment"));
    }

    macro_rules! parse_to_ast {
        ($input:literal) => {{
            let input = primitives::Span::new($input);
            let output = many0(parse_stanza)(input);
            assert!(
                output.is_ok(),
                "layout parse failed! {:?}",
                output.err().unwrap()
            );
            let output = output.unwrap().1;

            let ast = parse(output);
            assert!(ast.is_ok(), "ast parse failed! {:?}", ast.err().unwrap());
            ast.unwrap()
        }};
    }

    mod set {
        use super::*;

        #[test]
        fn cmd() {
            let ast = parse_to_ast!("  set $mod Mod1+Control");

            assert_eq!(ast.len(), 1);
            assert!(matches!(
                &ast[0],
                Item::Set(SetVar {
                    variable: Atom { content: AtomContent::Var(v), .. },
                    values,
                    ..
                }) if v == "mod" && values.len() == 1
            ));
            assert!(matches!(&ast[0], Item::Set(sv) if !sv.expand_at_runtime()));
            assert!(matches!(&ast[0], Item::Set(s) if s.name() == "mod"));
        }
        #[test]
        fn runtime_expansion() {
            let ast = parse_to_ast!("  set $$mod $(expand_thingy)");

            assert_eq!(ast.len(), 1);
            assert!(matches!(&ast[0], Item::Set(sv) if sv.expand_at_runtime()));
            assert!(matches!(&ast[0], Item::Set(sv) if sv.name() == "mod"));
        }

        #[test]
        fn block() {
            let ast = parse_to_ast!("  set { $mod Mod1+Control\n $neow wwmm }");

            assert_eq!(ast.len(), 1);
            assert!(matches!(&ast[0], Item::Nested{ nested, .. } if nested.len() == 2));
            if let Item::Nested { nested, .. } = &ast[0] {
                assert!(matches!(
                    &nested[0],
                    Item::Set(SetVar {
                        variable: Atom { content: AtomContent::Var(v), .. },
                        values,
                        ..
                    }) if v == "mod" && values.len() == 1
                ));
                assert!(matches!(&nested[0], Item::Set(sv) if !sv.expand_at_runtime()));
                assert!(matches!(&nested[0], Item::Set(s) if s.name() == "mod"));

                assert!(matches!(
                    &nested[1],
                    Item::Set(SetVar {
                        variable: Atom { content: AtomContent::Var(v), .. },
                        values,
                        ..
                    }) if v == "neow" && values.len() == 1
                ));
                assert!(matches!(&nested[1], Item::Set(sv) if !sv.expand_at_runtime()));
                assert!(matches!(&nested[1], Item::Set(s) if s.name() == "neow"));
            }
        }
    }

    mod exec {
        use super::*;

        #[test]
        fn cmd() {
            let ast =
                parse_to_ast!("\nexec systemd-cat --stderr-priority=warning -t mako $something");

            assert_eq!(ast.len(), 1);
            assert!(matches!(&ast[0], Item::Exec(Exec { args, .. }) if {
                let s: Vec<String> = args.iter().map(|a| a.content.clone().into()).collect();
                s == vec!["systemd-cat", "--stderr-priority=warning", "-t", "mako", "$something"]
            }));
        }

        #[test]
        fn block() {
            let ast =
                parse_to_ast!("\nexec systemd-cat --stderr-priority=warning {\n -t mako $something\n -t ye ./ye.sh\n }");

            assert_eq!(ast.len(), 1);
            assert!(matches!(&ast[0], Item::Nested{ nested, .. } if nested.len() == 2));
            if let Item::Nested { nested, .. } = &ast[0] {
                assert!(matches!(&nested[0], Item::Exec(Exec { args, .. }) if {
                    let s: Vec<String> = args.iter().map(|a| a.content.clone().into()).collect();
                    s == vec!["systemd-cat", "--stderr-priority=warning", "-t", "mako", "$something"]
                }));
                assert!(matches!(&nested[1], Item::Exec(Exec { args, .. }) if {
                    let s: Vec<String> = args.iter().map(|a| a.content.clone().into()).collect();
                    s == vec!["systemd-cat", "--stderr-priority=warning", "-t", "ye", "./ye.sh"]
                }));
            }
        }
    }

    mod mode {
        use super::*;

        #[test]
        fn cmd() {
            let ast = parse_to_ast!("\nmode \"blueberry pie\"");

            assert_eq!(ast.len(), 1);
            assert!(
                matches!(&ast[0], Item::SwitchMode(SwitchMode { mode, .. }) if {
                    let m: String = mode.content.clone().into();
                    m == "blueberry pie"
                })
            );
        }
    }

    mod bindsym {
        use super::*;

        #[test]
        fn cmd() {
            let ast = parse_to_ast!("\nbindsym $mod+Shift+r exec yeet");
            assert_eq!(ast.len(), 1);
            assert!(
                matches!(&ast[0], Item::BindSym(BindSym { args: Subset::Item(i), flags, .. })
                    if flags.is_empty() && matches!(&**i, Item::Exec(Exec { args, .. }) if {
                        let s: Vec<String> = args.iter().map(|a| a.content.clone().into()).collect();
                        s == vec!["yeet"]
                    })
                )
            );
        }

        #[test]
        fn block() {
            let ast = parse_to_ast!(
                "\nbindsym  {
                $mod+Shift+r exec {
                    yeet
                }
            }"
            );

            assert_eq!(ast.len(), 1);
            assert!(matches!(&ast[0], Item::Nested{ nested, .. } if nested.len() == 1));
            if let Item::Nested { nested, .. } = &ast[0] {
                assert_eq!(nested.len(), 1);
                if let Item::Nested { nested, .. } = &nested[0] {
                    assert_eq!(nested.len(), 1);
                    assert!(
                        matches!(&nested[0], Item::BindSym(BindSym { args: Subset::Item(i), flags, .. })
                            if flags.is_empty() && matches!(&**i, Item::Exec(Exec { args, .. }) if {
                                let s: Vec<String> = args.iter().map(|a| a.content.clone().into()).collect();
                                s == vec!["yeet"]
                            })
                        )
                    );
                }
            }
        }
    }

    #[test]
    fn visit() {
        let mut ast = parse_to_ast!(
            "\nbindsym  {
            $mod+Shift+r exec {
                yeet
            }
        }"
        );

        assert_eq!(ast.len(), 1);
        let mut seen_exec = false;
        ast[0]
            .visit::<(), _>(|i| {
                if let Item::Exec(e) = i {
                    seen_exec = true;
                    assert_eq!(
                        e.args
                            .iter()
                            .map(|a| a.content.clone().into())
                            .collect::<Vec<String>>(),
                        vec!["yeet"]
                    );
                };
                Ok(())
            })
            .expect("success");

        assert!(seen_exec);
    }
}
