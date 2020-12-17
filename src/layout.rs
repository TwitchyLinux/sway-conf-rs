use crate::primitives::*;

use nom::character::complete::multispace0;
use nom::IResult;
use nom::{multi::many_till, sequence::tuple};

use serde::{Deserialize, Serialize};

/// A lexical section in a sway config.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "type")]
pub enum Stanza<'a> {
    /// A comment, leading with a pound symbol.
    #[serde(borrow)]
    Comment(Line<'a>),
    /// A sequence of atoms representing a single line.
    Line { line: Line<'a>, atoms: Vec<Atom> },
    /// A sequence of lines within a block, and the prefix of
    /// the block.
    Block {
        prefix: Line<'a>,
        prefix_atoms: Vec<Atom>,
        l_brace: Token<'a>,
        r_brace: Token<'a>,
        nested: Vec<Stanza<'a>>,
    },
}

/// Decodes the high-level layout of a sway configuration file, keeping track of file
/// offsets and line numbers.
pub fn parse_stanza(i: Span) -> IResult<Span, Stanza> {
    let (i, _) = multispace0(i)?;

    // Match end-of-line comments first
    if let Ok((s, c)) = unary_comment(i) {
        return Ok((s, Stanza::Comment(c)));
    }

    let (s, l) = line(i)?;
    let atoms = l
        .atoms()
        .map_err(|_e| nom::Err::Error(nom::error::Error::new(s, nom::error::ErrorKind::Satisfy)))?
        .clone();

    if let Ok((s, lb)) = open_brace(s) {
        let (s, (set, (_, rb))) = many_till(parse_stanza, tuple((multispace0, close_brace)))(s)?;
        Ok((
            s,
            Stanza::Block {
                prefix: l,
                prefix_atoms: atoms,
                l_brace: lb,
                r_brace: rb,
                nested: set,
            },
        ))
    } else {
        Ok((s, Stanza::Line { line: l, atoms }))
    }
}
