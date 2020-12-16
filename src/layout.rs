use crate::primitives::*;

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
