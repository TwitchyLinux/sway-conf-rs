use crate::{layout, primitives};
use primitives::{Atom, AtomContent};
pub mod bind;
use serde::{Deserialize, Serialize};

/// A fatal error returned by the parser.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Err<'a> {
    #[serde(borrow)]
    stanza: layout::Stanza<'a>,
    err: String,
}

/// A line which was not understood by the parser.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Unknown<'a> {
    line: primitives::Line<'a>,
}

/// An expression which sets a variable to a specific value.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct SetVar<'a> {
    line: primitives::Line<'a>,
    variable: Atom,
    values: Vec<Atom>,
}

impl SetVar<'_> {
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
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Exec<'a> {
    line: primitives::Line<'a>,
    args: Vec<Atom>,
}

impl<'a> Exec<'a> {
    /// Returns the atoms making up the subshell invocation.
    pub fn command_atoms(&'a self) -> &'a Vec<Atom> {
        &self.args
    }
}

/// An expression symbolizing switching to a specific mode.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct SwitchMode<'a> {
    line: primitives::Line<'a>,
    mode: Atom,
}

/// An expression describing a key binding.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct BindSym<'a> {
    line: primitives::Line<'a>,
    flags: bind::FLAGS,
    keys: Vec<bind::Key>,
    args: Vec<Atom>,
}

// impl<'a> BindSym<'a> {
//
// }

/// Represents a sway command. This may not directly correspond to a
/// line in the config, due to possible line continuations or blocks.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "type")]
pub enum Item<'a> {
    /// A comment, leading with a pound symbol.
    #[serde(borrow)]
    Comment(primitives::Line<'a>),
    /// A 'set' command.
    Set(SetVar<'a>),
    /// An 'exec' command.
    Exec(Exec<'a>),
    /// A command to switch to the specified mode.
    SwitchMode(SwitchMode<'a>),
    /// A command which maps a key combination to a sway command.
    BindSym(BindSym<'a>),
    Unknown(Unknown<'a>),
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

fn parse_line<'a>(line: primitives::Line<'a>, mut atoms: Vec<Atom>) -> Result<Item<'a>, Err> {
    let cmd = if atoms.len() > 0 {
        if let AtomContent::Arg(s) = &atoms[0].content {
            s.clone().to_lowercase()
        } else {
            return Err(Err {
                stanza: layout::Stanza::Line { line, atoms },
                err: "command was invalid".to_string(),
            });
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
            if let AtomContent::Var(_) = &atoms[1].content {
                let variable = atoms.remove(1);
                Ok(Item::Set(SetVar {
                    line,
                    variable,
                    values: atoms[1..].to_vec(),
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
            line,
            args: atoms[1..].to_vec(),
        })),
        cmd_matcher!("mode", exact = 2) => Ok(Item::SwitchMode(SwitchMode {
            line,
            mode: atoms[1].clone(),
        })),
        _ => Err(Err {
            stanza: layout::Stanza::Line { line, atoms },
            err: "unhandled line".to_string(),
        }),
    }

    //Ok(Item::Comment(l))
}

fn parse_inner<'a>(stanza: layout::Stanza<'a>) -> Result<Item<'a>, Err> {
    match stanza {
        layout::Stanza::Comment(l) => Ok(Item::Comment(l)),
        layout::Stanza::Line { line, atoms } => parse_line(line, atoms),
        layout::Stanza::Block { .. } => Err(Err {
            stanza: stanza,
            err: "blocks are not yet handled".to_string(),
        }),
    }
}

/// Maps config stanzas into their more-detailed AST equivalents.
pub fn parse(stanzas: Vec<layout::Stanza<'_>>) -> Result<Vec<Item<'_>>, Err> {
    let ast = stanzas
        .into_iter()
        .map(|s| parse_inner(s))
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
            eprintln!("{:?}\n\n\n", ast);

            assert_eq!(ast.len(), 1);
            assert!(
                matches!(&ast[0], Item::BindSym(BindSym { args, flags, keys, .. }) if {
                    let s: Vec<String> = args.iter().map(|a| a.content.clone().into()).collect();
                    let k: Vec<String> = keys.iter().map(|a| a.clone().into()).collect();
                    s == vec!["exec", "yeet"] && flags.is_empty() && k == vec!["$mod", "Shift", "r"]
                })
            );
        }
    }
}
