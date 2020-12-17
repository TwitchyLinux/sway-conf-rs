use nom::bytes::complete::{tag, take_while};
use nom::character::complete::multispace0;
use nom::sequence::tuple;
use nom::IResult;
use nom_locate::{position, LocatedSpan};
use serde::{ser::*, Deserialize, Serialize};

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub struct SpanOffset {
    from: usize,
    offset: usize,
    line: u32,
}

impl SpanOffset {
    fn adjust_with_prefix(self, offset: usize) -> Self {
        Self {
            from: self.from + offset,
            ..self
        }
    }
}

fn span_serialize<S>(x: &Option<Span<'_>>, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    match *x {
        Some(ref x) => {
            let mut sp = s.serialize_struct("Span", 2)?;
            sp.serialize_field("column", &x.get_utf8_column())?;
            sp.serialize_field("line", &x.location_line())?;
            sp.end()
        }
        None => s.serialize_none(),
    }
}

/// Token describes the type and location of some character
/// that has meaning when parsing.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub struct Token<'a> {
    #[serde(serialize_with = "span_serialize")]
    #[serde(skip_deserializing)]
    pub position: Option<Span<'a>>,
    pub tok: char,
}

/// Line describes a single unit of configuration.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Line<'a> {
    #[serde(serialize_with = "span_serialize")]
    #[serde(skip_deserializing)]
    pub start: Option<Span<'a>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub continuation_spans: Option<Vec<SpanOffset>>,
    pub line: String,
}

#[inline(always)]
fn is_cmd_char(chr: char) -> bool {
    chr != '\n' && chr != '#' && chr != '{' && chr != '}'
}

pub fn line(i: Span) -> IResult<Span, Line> {
    let (s, pos) = position(i)?;
    let (s, v) = take_while(is_cmd_char)(s)?;

    if v.fragment().len() == 0 {
        return Err(nom::Err::Error(nom::error::Error::new(
            s,
            nom::error::ErrorKind::Tag,
        )));
    }

    // Handle potential line continuation
    if v.fragment().ends_with('\\') {
        if let Ok((mut s2, _newline)) = tag::<_, _, ()>("\n")(s) {
            let mut line_tmp = v.fragment().to_string();
            line_tmp.pop(); // Drop the trailing backslash

            if let Ok((s3, space)) = multispace0::<_, ()>(s2) {
                s2 = s3;
                line_tmp.push_str(&space);
            }

            let (s2, eol_pos) = position(s2)?;
            return Ok(match line(s2) {
                Ok((s2, l)) => {
                    let mut offsets: Vec<SpanOffset> = Vec::new();
                    let inner_pos = l.start.unwrap();
                    offsets.push(SpanOffset {
                        from: line_tmp.len(),
                        offset: eol_pos.location_offset(),
                        line: eol_pos.location_line(),
                    });
                    if let Some(o) = l.continuation_spans {
                        offsets.extend_from_slice(
                            &o.into_iter()
                                .map(|so| so.adjust_with_prefix(line_tmp.len()))
                                .collect::<Vec<_>>(),
                        );
                    }

                    line_tmp.push_str(&l.line);
                    (
                        s2,
                        Line {
                            start: Some(pos),
                            continuation_spans: Some(offsets),
                            line: line_tmp,
                        },
                    )
                }
                _ => (
                    s2,
                    Line {
                        start: Some(pos),
                        continuation_spans: None,
                        line: line_tmp,
                    },
                ),
            });
        }
    }

    Ok((
        s,
        Line {
            start: Some(pos),
            continuation_spans: None,
            line: v.fragment().to_string(),
        },
    ))
}

pub fn open_brace(i: Span) -> IResult<Span, Token> {
    let (s, pos) = position(i)?;
    let (s, _) = tag("{")(s)?;
    Ok((
        s,
        Token {
            position: Some(pos),
            tok: '{',
        },
    ))
}

pub fn close_brace(i: Span) -> IResult<Span, Token> {
    let (s, pos) = position(i)?;
    let (s, _) = tag("}")(s)?;
    Ok((
        s,
        Token {
            position: Some(pos),
            tok: '}',
        },
    ))
}

pub fn unary_comment(i: Span) -> IResult<Span, Line> {
    let (s, pos) = position(i)?;
    let (s, _) = tag("#")(s)?;
    let (s, v) = take_while(|chr| chr != '\n')(s)?;

    Ok((
        s,
        Line {
            start: Some(pos),
            continuation_spans: None,
            line: v.fragment().to_string(),
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::character::complete::multispace0;

    #[test]
    fn line_basic() {
        let input = Span::new("Lorem ipsum \n foobar");
        let output = line(input);
        let line = &output.as_ref().unwrap().1.line;
        let position = output.as_ref().unwrap().1.start.unwrap();
        assert_eq!(line, "Lorem ipsum ");
        assert_eq!(position.get_column(), 1);
    }

    #[test]
    fn line_no_newline() {
        let input = Span::new("Lorem ipsum ");
        let output = line(input);
        let line = &output.as_ref().unwrap().1.line;
        let position = output.as_ref().unwrap().1.start.unwrap();
        assert_eq!(line, "Lorem ipsum ");
        assert_eq!(position.get_column(), 1);
    }

    #[test]
    fn line_with_comment() {
        let input = Span::new("set $mod Mod4 # Yeeeeee");
        let output = line(input);
        let line = &output.as_ref().unwrap().1.line;
        let position = output.as_ref().unwrap().1.start.unwrap();
        assert_eq!(line, "set $mod Mod4 ");
        assert_eq!(position.get_column(), 1);
    }

    #[test]
    fn line_with_brace() {
        let input = Span::new("Lorem ipsum { YE");
        let output = line(input);
        let line = &output.as_ref().unwrap().1.line;
        let position = output.as_ref().unwrap().1.start.unwrap();
        assert_eq!(line, "Lorem ipsum ");
        assert_eq!(position.get_column(), 1);
    }

    #[test]
    fn unary_comment_basic() {
        let input = Span::new("#Blueberries");
        let output = unary_comment(input);
        let line = &output.as_ref().unwrap().1.line;
        let position = output.as_ref().unwrap().1.start.unwrap();
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
        let tok = output.as_ref().unwrap().1;
        assert_eq!(tok.position.unwrap().get_column(), 5);
        assert_eq!(output.as_ref().unwrap().0.fragment(), &"\nsomething");
    }

    #[test]
    fn line_continuation_spans() {
        let input = Span::new("Something \\\n that  do\\\nes\\\n  cross lines!");
        let output = line(input);
        let line = &output.as_ref().unwrap().1.line;
        let position = output.as_ref().unwrap().1.start.unwrap();
        assert_eq!(line, "Something  that  does  cross lines!");
        assert_eq!(position.get_column(), 1);

        for (i, span) in output
            .as_ref()
            .unwrap()
            .1
            .continuation_spans
            .as_ref()
            .unwrap()
            .iter()
            .enumerate()
        {
            match i {
                0 => {
                    assert_eq!(&line[..span.from], "Something  ");
                    assert_eq!(span.line, 2);
                    assert_eq!(span.offset, 13);
                }
                1 => {
                    assert_eq!(&line[11..span.from], "that  do");
                    assert_eq!(span.line, 3);
                    assert_eq!(span.offset, 23);
                }
                2 => {
                    assert_eq!(&line[19..span.from], "es  ");
                    assert_eq!(span.line, 4);
                    assert_eq!(span.offset, 29);

                    assert_eq!(&line[span.from..], "cross lines!");
                }
                _ => assert!(false, "Unexpected span index: {}", i),
            }
        }
    }
}
