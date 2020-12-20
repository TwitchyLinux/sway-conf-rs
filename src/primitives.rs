use nom::branch::alt;
use nom::bytes::complete::{escaped, is_not, tag, take_while, take_while_m_n};
use nom::character::complete::{multispace0, none_of, one_of};
use nom::combinator::eof;
use nom::multi::{fold_many1, many0};
use nom::sequence::{delimited, preceded, tuple};
use nom::IResult;
use nom_locate::{position, LocatedSpan};
use serde::{ser::*, Deserialize, Serialize};

/// Describes a section of text at a known location in a source file.
pub type Span<'a> = LocatedSpan<&'a str>;

/// Describes a location in a source file starting from an offset.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub struct SpanOffset {
    from: usize,
    offset: usize,
    line: u32,
    column: usize,
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

fn hex2(input: Span) -> IResult<Span, u8> {
    let (s, b) = take_while_m_n(2, 2, |c: char| c.is_digit(16))(input)?;

    Ok((
        s,
        u8::from_str_radix(&b, 16)
            .map_err(|_| nom::Err::Error(nom::error::Error::new(s, nom::error::ErrorKind::Tag)))?,
    ))
}

fn parse_hex_color(input: Span) -> IResult<Span, (u8, u8, u8)> {
    let (input, _) = tag("#")(input)?;
    let (input, (red, green, blue)) = tuple((hex2, hex2, hex2))(input)?;

    Ok((input, (red, green, blue)))
}

fn parse_single_quoted(input: Span) -> IResult<Span, Span> {
    let esc = escaped(none_of("\\\'"), '\\', tag("'"));
    let esc_or_empty = alt((esc, tag("")));

    delimited(tag("'"), esc_or_empty, tag("'"))(input)
}

fn parse_double_quoted(input: Span) -> IResult<Span, Span> {
    let esc = escaped(none_of("\\\""), '\\', tag("\""));
    let esc_or_empty = alt((esc, tag("")));

    delimited(tag("\""), esc_or_empty, tag("\""))(input)
}

fn parse_bracket(input: Span) -> IResult<Span, Span> {
    let esc = escaped(none_of("\\[]"), '\\', one_of("[]"));
    let esc_or_empty = alt((esc, tag("")));

    delimited(tag("["), esc_or_empty, tag("]"))(input)
}

fn arg(s: Span) -> IResult<Span, (Span, String)> {
    let (s, pos) = position(s)?;
    let (s, bits) = fold_many1(
        alt((parse_single_quoted, parse_double_quoted, is_not(" '\"\\[]"))),
        Vec::new(),
        |mut acc: Vec<_>, item| {
            acc.push(item);
            acc
        },
    )(s)?;

    Ok((
        s,
        bits.into_iter()
            .fold((pos, String::with_capacity(32)), |acc, x| {
                let (pos, mut s) = acc;
                s.push_str(&x);
                (pos, s)
            }),
    ))
}

fn var(s: Span) -> IResult<Span, Span> {
    let (s, val) = preceded(tag("$"), is_not(" '\"\\[],\n;+"))(s)?;
    let mut ending = alt((tag(" "), tag("\t"), tag("\n"), tag("\r"), eof));

    match ending(s) {
        Ok(_) => Ok((s, val)),
        Err(e) => Err(e),
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "kind", content = "atom")]
pub enum AtomContent {
    Arg(String),
    Var(String),
    Bracket(String),
    Rgb(u8, u8, u8),
}

impl Into<String> for AtomContent {
    fn into(self) -> String {
        match self {
            AtomContent::Arg(a) => a,
            AtomContent::Var(v) => {
                let mut out = String::with_capacity(v.len() + 1);
                out.push('$');
                out.push_str(&v);
                out
            }
            AtomContent::Bracket(v) => {
                let mut out = String::with_capacity(v.len() + 2);
                out.push('[');
                out.push_str(&v);
                out.push(']');
                out
            }
            AtomContent::Rgb(r, g, b) => String::from(format!("#{:02X}{:02X}{:02X}", r, g, b)),
        }
    }
}

/// The smallest unit of information that makes up a command.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Atom {
    pub content: AtomContent,
    pub start_offset: usize,
    pub end_offset: usize,
    pub line: u32,
    pub column: usize,
}

/// Parses an atom.
pub fn atom(mut s: Span) -> IResult<Span, Atom> {
    if let Ok((i, _)) = multispace0::<_, ()>(s) {
        s = i;
    }
    let (s, p) = position(s)?;

    if let Ok((s, var)) = var(s) {
        let str = var.fragment().to_string();
        return Ok((
            s,
            Atom {
                start_offset: var.location_offset() - 1,
                end_offset: s.location_offset(),
                content: AtomContent::Var(str),
                line: 0,
                column: 0,
            },
        ));
    }

    if let Ok((s, b)) = parse_bracket(s) {
        return Ok((
            s,
            Atom {
                start_offset: p.location_offset(),
                end_offset: s.location_offset(),
                content: AtomContent::Bracket(b.to_string()),
                line: 0,
                column: 0,
            },
        ));
    }

    if let Ok((s, (r, g, b))) = parse_hex_color(s) {
        return Ok((
            s,
            Atom {
                start_offset: p.location_offset(),
                end_offset: s.location_offset(),
                content: AtomContent::Rgb(r, g, b),
                line: 0,
                column: 0,
            },
        ));
    }

    if let Ok((s, arg)) = arg(s) {
        let (pos, arg) = arg;
        return Ok((
            s,
            Atom {
                start_offset: pos.location_offset(),
                end_offset: s.location_offset(),
                content: AtomContent::Arg(arg),
                line: 0,
                column: 0,
            },
        ));
    }

    return Err(nom::Err::Error(nom::error::Error::new(
        s,
        nom::error::ErrorKind::Tag,
    )));
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

impl<'a> Line<'a> {
    pub(crate) fn atoms(&'a self) -> Result<Vec<Atom>, nom::Err<nom::error::Error<Span<'a>>>> {
        let input = Span::new(&self.line);
        let span = self.start.unwrap();

        let (_, mut atoms) = many0(atom)(input)?;

        // Compute the offsets/locations for each atom.
        for atom in atoms.iter_mut() {
            let (s, e) = (atom.start_offset, atom.end_offset);
            let (start, end) = (span.location_offset() + s, span.location_offset() + e);
            *atom = Atom {
                start_offset: start,
                end_offset: end,
                line: span.location_line(),
                column: span.get_utf8_column() + atom.start_offset,
                ..atom.clone()
            };

            // There may be continuation spans present, which would mean we
            // need to update our offset and line information.
            for span in self.continuation_spans.iter().flatten() {
                // println!("span={:?}, atom={:?}\n", span, atom);
                if s >= span.from {
                    *atom = Atom {
                        start_offset: span.offset + (s - span.from),
                        end_offset: span.offset + (e - span.from),
                        line: span.line,
                        column: span.column + (s - span.from),
                        ..atom.clone()
                    };
                }
                if e >= span.from {
                    *atom = Atom {
                        end_offset: span.offset + (e - span.from),
                        ..atom.clone()
                    };
                }
            }
        }

        Ok(atoms)
    }
}

#[inline(always)]
fn is_cmd_char(chr: char) -> bool {
    chr != '\n' && chr != '{' && chr != '}'
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
                    offsets.push(SpanOffset {
                        from: line_tmp.len(),
                        offset: eol_pos.location_offset(),
                        line: eol_pos.location_line(),
                        column: eol_pos.get_utf8_column(),
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

/// Token describes the type and location of some character
/// that has meaning when parsing.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub struct Token<'a> {
    #[serde(serialize_with = "span_serialize")]
    #[serde(skip_deserializing)]
    pub position: Option<Span<'a>>,
    pub tok: char,
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

#[cfg(test)]
mod tests {
    use super::*;
    use nom::character::complete::multispace0;
    use pretty_assertions::assert_eq as assert_pretty;

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
                    assert_eq!(span.column, 2);
                    assert_eq!(span.offset, 13);
                }
                1 => {
                    assert_eq!(&line[11..span.from], "that  do");
                    assert_eq!(span.line, 3);
                    assert_eq!(span.column, 1);
                    assert_eq!(span.offset, 23);
                }
                2 => {
                    assert_eq!(&line[19..span.from], "es  ");
                    assert_eq!(span.line, 4);
                    assert_eq!(span.column, 3);
                    assert_eq!(span.offset, 29);

                    assert_eq!(&line[span.from..], "cross lines!");
                }
                _ => assert!(false, "Unexpected span index: {}", i),
            }
        }
    }

    #[test]
    fn atoms_basic() {
        let input = Span::new("set $alt   Mod1");
        let want = vec![
            Atom {
                content: AtomContent::Arg("set".to_string()),
                start_offset: 0,
                end_offset: 3,
                line: 1,
                column: 1,
            },
            Atom {
                content: AtomContent::Var("alt".to_string()),
                start_offset: 4,
                end_offset: 8,
                line: 1,
                column: 5,
            },
            Atom {
                content: AtomContent::Arg("Mod1".to_string()),
                start_offset: 11,
                end_offset: 15,
                line: 1,
                column: 12,
            },
        ];

        let atoms = line(input).unwrap().1.atoms().expect("atom parsing failed");
        assert_pretty!(atoms, want);
    }

    #[test]
    fn atoms_strings_single_quote() {
        let input = Span::new("cmd 'swiggity swooty' 'Yolo swaggins'");
        let want = vec![
            Atom {
                content: AtomContent::Arg("cmd".to_string()),
                start_offset: 0,
                end_offset: 3,
                line: 1,
                column: 1,
            },
            Atom {
                content: AtomContent::Arg("swiggity swooty".to_string()),
                start_offset: 4,
                end_offset: 21,
                line: 1,
                column: 5,
            },
            Atom {
                content: AtomContent::Arg("Yolo swaggins".to_string()),
                start_offset: 22,
                end_offset: 37,
                line: 1,
                column: 23,
            },
        ];

        let atoms = line(input).unwrap().1.atoms().expect("atom parsing failed");
        assert_pretty!(atoms, want);
    }

    #[test]
    fn atoms_strings_double_quote() {
        let input = Span::new("cmd \"swiggity swooty\" \"I'm \\\"quoted\\\"\"");
        let want = vec![
            Atom {
                content: AtomContent::Arg("cmd".to_string()),
                start_offset: 0,
                end_offset: 3,
                line: 1,
                column: 1,
            },
            Atom {
                content: AtomContent::Arg("swiggity swooty".to_string()),
                start_offset: 4,
                end_offset: 21,
                line: 1,
                column: 5,
            },
            Atom {
                content: AtomContent::Arg("I'm \\\"quoted\\\"".to_string()),
                start_offset: 22,
                end_offset: 38,
                line: 1,
                column: 23,
            },
        ];

        let atoms = line(input).unwrap().1.atoms().expect("atom parsing failed");
        assert_pretty!(atoms, want);
    }

    #[test]
    fn atoms_multiline_in_quotes() {
        let input = Span::new("cmd 'ye\\\now' \\\narg");
        let want = vec![
            Atom {
                content: AtomContent::Arg("cmd".to_string()),
                start_offset: 0,
                end_offset: 3,
                line: 1,
                column: 1,
            },
            Atom {
                content: AtomContent::Arg("yeow".to_string()),
                start_offset: 4,
                end_offset: 12,
                line: 1,
                column: 5,
            },
            Atom {
                content: AtomContent::Arg("arg".to_string()),
                start_offset: 15,
                end_offset: 18,
                line: 3,
                column: 1,
            },
        ];

        let atoms = line(input).unwrap().1.atoms().expect("atom parsing failed");
        assert_pretty!(atoms, want);
    }

    #[test]
    fn escaped_in_braces() {
        let input = Span::new(" [cmd=\"\\[\\]\"]");
        let want = vec![Atom {
            content: AtomContent::Bracket("cmd=\"\\[\\]\"".to_string()),
            start_offset: 1,
            end_offset: 13,
            line: 1,
            column: 2,
        }];

        let atoms = line(input).unwrap().1.atoms().expect("atom parsing failed");
        assert_pretty!(atoms, want);
    }

    #[test]
    fn atoms_bindings() {
        let input = Span::new("set $alt+$mod+t");
        let want = vec![
            Atom {
                content: AtomContent::Arg("set".to_string()),
                start_offset: 0,
                end_offset: 3,
                line: 1,
                column: 1,
            },
            Atom {
                content: AtomContent::Arg("$alt+$mod+t".to_string()),
                start_offset: 4,
                end_offset: 15,
                line: 1,
                column: 5,
            },
        ];

        let atoms = line(input).unwrap().1.atoms().expect("atom parsing failed");
        assert_pretty!(atoms, want);
    }
}
