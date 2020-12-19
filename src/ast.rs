use crate::{layout, primitives};
use primitives::{Atom, AtomContent};

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

    /// The name of the variable.
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

/// Represents a sway command. This may not correspond to a line
/// in the config, in the event of line continuations or blocks.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "type")]
pub enum Item<'a> {
    /// A comment, leading with a pound symbol.
    #[serde(borrow)]
    Comment(primitives::Line<'a>),
    /// A 'set' command.
    Set(SetVar<'a>),
    Unknown(Unknown<'a>),
}

macro_rules! cmd_matcher {
    ($cmd:literal, at_least=2) => {
        ($cmd, true)
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

    // (cmd as lowercase, at-least-2-args)
    match (cmd.as_str(), atoms.len() >= 2) {
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
}
