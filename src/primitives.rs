use nom::bytes::complete::{tag, take_while};
use nom::character::complete::multispace0;
use nom::IResult;
use nom::{
    branch::alt,
    combinator::value,
    sequence::{preceded, tuple},
};
use nom_locate::{position, LocatedSpan};

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Line<'a> {
    pub position: Span<'a>,
    pub line: &'a str,
}

#[inline(always)]
fn is_cmd_char(chr: char) -> bool {
    chr != '\n' && chr != '#' && chr != '{' && chr != '}'
}

pub fn line(i: Span) -> IResult<Span, Line> {
    let (s, pos) = position(i)?;
    let (s, v) = take_while(is_cmd_char)(s)?;

    Ok((
        s,
        Line {
            position: pos,
            line: v.fragment(),
        },
    ))
}

pub fn open_brace(i: Span) -> IResult<Span, Span> {
    let (s, pos) = position(i)?;
    let (s, _) = tag("{")(s)?;
    Ok((s, pos))
}

pub fn unary_comment(i: Span) -> IResult<Span, Line> {
    let (s, pos) = position(i)?;
    let (s, _) = tag("#")(s)?;
    let (s, v) = take_while(|chr| chr != '\n')(s)?;

    Ok((
        s,
        Line {
            position: pos,
            line: v.fragment(),
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn line_basic() {
        let input = Span::new("Lorem ipsum \n foobar");
        let output = line(input);
        let line = output.as_ref().unwrap().1.line;
        let position = output.as_ref().unwrap().1.position;
        assert_eq!(line, "Lorem ipsum ");
        assert_eq!(position.get_column(), 1);
    }

    #[test]
    fn line_no_newline() {
        let input = Span::new("Lorem ipsum ");
        let output = line(input);
        let line = output.as_ref().unwrap().1.line;
        let position = output.as_ref().unwrap().1.position;
        assert_eq!(line, "Lorem ipsum ");
        assert_eq!(position.get_column(), 1);
    }

    #[test]
    fn line_with_comment() {
        let input = Span::new("set $mod Mod4 # Yeeeeee");
        let output = line(input);
        let line = output.as_ref().unwrap().1.line;
        let position = output.as_ref().unwrap().1.position;
        assert_eq!(line, "set $mod Mod4 ");
        assert_eq!(position.get_column(), 1);
    }

    #[test]
    fn line_with_brace() {
        let input = Span::new("Lorem ipsum { YE");
        let output = line(input);
        let line = output.as_ref().unwrap().1.line;
        let position = output.as_ref().unwrap().1.position;
        assert_eq!(line, "Lorem ipsum ");
        assert_eq!(position.get_column(), 1);
    }

    #[test]
    fn unary_comment_basic() {
        let input = Span::new("#Blueberries");
        let output = unary_comment(input);
        let line = output.as_ref().unwrap().1.line;
        let position = output.as_ref().unwrap().1.position;
        assert_eq!(line, "Blueberries");
        assert_eq!(position.get_column(), 1);
    }
    #[test]
    fn unary_comment_err() {
        let input = Span::new("set $mod Mod4");
        let output = unary_comment(input);
        assert!(
            !output.is_ok(),
            "parser did not error, returned {:?}",
            output
        );
    }

    #[test]
    fn open_brace_basic() {
        let input = Span::new("    {\nsomething");
        let (input, _) = multispace0::<_, ()>(input).unwrap();
        let output = open_brace(input);
        let position = output.as_ref().unwrap().1;
        assert_eq!(position.get_column(), 5);
        assert_eq!(output.as_ref().unwrap().0.fragment(), &"\nsomething");
    }
}
