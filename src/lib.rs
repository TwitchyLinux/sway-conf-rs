extern crate nom;
extern crate nom_locate;

mod primitives;
use primitives::*;

use nom::bytes::complete::{tag, take_while};
use nom::character::complete::multispace0;
use nom::IResult;
use nom::{
    branch::alt,
    combinator::value,
    multi::{many0, many_till},
    sequence::terminated,
};
use nom_locate::position;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Stanza<'a> {
    Comment(Line<'a>),
    Line(Line<'a>),
    Block(Line<'a>, Span<'a>, Vec<Stanza<'a>>),
}

pub fn parse_stanza(i: Span) -> IResult<Span, Stanza> {
    let (i, _) = multispace0(i)?;

    // Match end-of-line comments first
    if let Ok((s, c)) = unary_comment(i) {
        return Ok((s, Stanza::Comment(c)));
    }

    let (s, l) = line(i)?;
    if let Ok((s, b)) = open_brace(s) {
        let (s, (set, _)) = many_till(parse_stanza, tag("}"))(s)?;
        Ok((s, Stanza::Block(l, b, set)))
    } else {
        Ok((s, Stanza::Line(l)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        if let Stanza::Block(cmd, _brace, inners) = c {
            assert_eq!(cmd.line, "cmd");
            assert_eq!(2, inners.len());
        }
    }

    #[test]
    fn nested_brace() {
        let input = Span::new("cmd{  a\nb {}}");
        let output = parse_stanza(input);
        assert!(output.is_ok(), "parse failed!  {:?}", output);
        let (_remaining, c) = output.unwrap();
        if let Stanza::Block(cmd, _brace, inners) = c {
            assert_eq!(cmd.line, "cmd");
            assert_eq!(2, inners.len());

            if let Stanza::Line(c) = &inners[0] {
                assert_eq!(c.line, "a");
            }
            assert!(matches!(inners[0].clone(), Stanza::Line(_)));

            if let Stanza::Block(cmd, _, inner) = &inners[1] {
                assert_eq!(cmd.line, "b ");
                assert_eq!(0, inner.len());
            }
            assert!(matches!(inners[1].clone(), Stanza::Block(_, _, _)));
        }
    }

    #[test]
    fn tc1() {
        let input = Span::new(include_str!("testdata/tc1.sway"));
        let output = many0(parse_stanza)(input);
        assert!(output.is_ok(), "{:?}", output);
    }
}
