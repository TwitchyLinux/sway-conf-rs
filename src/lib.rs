extern crate nom;
extern crate nom_locate;

mod primitives;
use primitives::*;

use nom::character::complete::multispace0;
use nom::IResult;
use nom::{multi::many_till, sequence::tuple};

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "type")]
pub enum Stanza<'a> {
    #[serde(borrow)]
    Comment(Line<'a>),
    #[serde(borrow)]
    Line(Line<'a>),
    Block {
        prefix: Line<'a>,
        l_brace: Token<'a>,
        r_brace: Token<'a>,
        nested: Vec<Stanza<'a>>,
    },
}

pub fn parse_stanza(i: Span) -> IResult<Span, Stanza> {
    let (i, _) = multispace0(i)?;

    // Match end-of-line comments first
    if let Ok((s, c)) = unary_comment(i) {
        return Ok((s, Stanza::Comment(c)));
    }

    let (s, l) = line(i)?;
    if let Ok((s, lb)) = open_brace(s) {
        let (s, (set, (_, rb))) = many_till(parse_stanza, tuple((multispace0, close_brace)))(s)?;
        Ok((
            s,
            Stanza::Block {
                prefix: l,
                l_brace: lb,
                r_brace: rb,
                nested: set,
            },
        ))
    } else {
        Ok((s, Stanza::Line(l)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::multi::many0;
    use pretty_assertions::assert_eq as assert_pretty;
    use std::path::PathBuf;
    use test_case::test_case;

    #[test]
    fn comments() {
        let input = Span::new("  #1\n#2");
        let output = parse_stanza(input);
        assert!(output.is_ok(), "1st parse failed!  {:?}", output);
        let (remaining, c1) = output.unwrap();
        if let Stanza::Comment(c) = c1 {
            assert_eq!(c.line, "1");
        }

        let output = parse_stanza(remaining);
        assert!(output.is_ok(), "2nd parse failed!  {:?}", output);
        let (_, c2) = output.unwrap();
        if let Stanza::Comment(c) = c2 {
            assert_eq!(c.line, "2");
        }
    }

    #[test]
    fn basic() {
        let input = Span::new("set $mod Mod4\nset $bindsym bindsym --to-code # lulz");
        let output = parse_stanza(input);
        assert!(output.is_ok(), "1st parse failed!  {:?}", output);
        let (remaining, c1) = output.unwrap();
        if let Stanza::Line(c) = c1 {
            assert_eq!(c.line, "set $mod Mod4");
        }

        let output = parse_stanza(remaining);
        assert!(output.is_ok(), "2nd parse failed!  {:?}", output);
        let (_, c2) = output.unwrap();
        if let Stanza::Line(c) = c2 {
            assert_eq!(c.line, "set $bindsym bindsym --to-code ");
        }
    }

    #[test]
    fn basic_brace() {
        let input = Span::new("cmd{a\nb}");
        let output = parse_stanza(input);
        assert!(output.is_ok(), "parse failed!  {:?}", output);
        let (_remaining, c) = output.unwrap();
        if let Stanza::Block { prefix, nested, .. } = c {
            assert_eq!(prefix.line, "cmd");
            assert_eq!(2, nested.len());
        }
    }

    #[test]
    fn nested_brace() {
        let input = Span::new("cmd{  a\nb {}}");
        let output = parse_stanza(input);
        assert!(output.is_ok(), "parse failed!  {:?}", output);
        let (_remaining, c) = output.unwrap();
        if let Stanza::Block { prefix, nested, .. } = c {
            assert_eq!(prefix.line, "cmd");
            assert_eq!(2, nested.len());

            if let Stanza::Line(c) = &nested[0] {
                assert_eq!(c.line, "a");
            }
            assert!(matches!(nested[0].clone(), Stanza::Line(_)));

            if let Stanza::Block { prefix, nested, .. } = &nested[1] {
                assert_eq!(prefix.line, "b ");
                assert_eq!(0, nested.len());
            }
            assert!(matches!(nested[1].clone(), Stanza::Block{ .. }));
        }
    }

    #[test_case( "tc1" ; "simple directives")]
    #[test_case( "tc2" ; "simple block")]
    fn test_config(name: &'static str) {
        let mut c = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let mut j = c.clone();
        c.push(format!("testdata/{}.sway", name));
        j.push(format!("testdata/{}.json", name));

        let sway = std::fs::read_to_string(c).expect("opening config");
        let expected = std::fs::read_to_string(j).expect("opening result");

        let input = Span::new(&sway);
        let output = many0(parse_stanza)(input);
        assert!(output.is_ok(), "Failed to parse input! Got: {:?}", output);

        let d = serde_json::to_string_pretty(&output.unwrap().1).expect("json encode");

        let mut s = String::from(d.clone());
        s.push('\n');
        if s != expected {
            println!("{}\n\n\n\n", d);
            assert_pretty!(
                d.split("\n").collect::<Vec<_>>(),
                expected.split("\n").collect::<Vec<_>>()
            );
        }
    }
}
