use super::*;
use crate::layout;
use crate::primitives::{self, Atom, AtomContent};
use serde::Serialize;

bitflags::bitflags! {
    /// Modifies how a binding works.
    #[derive(Serialize)]
    pub struct FLAGS: u32 {
        const RELEASE = (1 << 0);
        const LOCKED = (1 << 1); // keyboard only
        const BORDER = (1 << 2); // mouse only; trigger on container border
        const CONTENTS = (1 << 3); // mouse only; trigger on container contents
        const TITLEBAR = (1 << 4); // mouse only; trigger on container titlebar
        const CODE = (1 << 5); // keyboard only; convert keysyms into keycodes
        const RELOAD = (1 << 6); // switch only; (re)trigger binding on reload
        const INHIBITED = (1 << 7); // keyboard only: ignore shortcut inhibitor
        const NOREPEAT = (1 << 8); // keyboard only; do not trigger when repeating a held key

        const EXCLUDE_TITLEBAR = (1 << 20);
        const NOWARN = (1 << 21);
    }
}

/// A key in a key binding.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Key {
    Var(String),
    Key(String),
}

impl Into<String> for Key {
    fn into(self) -> String {
        match self {
            Key::Var(v) => {
                let mut out = String::with_capacity(v.len() + 1);
                out.push('$');
                out.push_str(&v);
                out
            }
            Key::Key(k) => k,
        }
    }
}

fn parse_keyset<'a>(atom: Atom) -> Result<Vec<Key>, &'static str> {
    match atom.content {
        AtomContent::Arg(s) => Ok(s
            .split("+")
            .map(|s| {
                if s.starts_with("$") {
                    Key::Var(s[1..].to_string())
                } else {
                    Key::Key(s.to_string())
                }
            })
            .collect()),
        _ => Err("invalid atom content for keyset"),
    }
}

/// Parses a bindsym line.
pub(super) fn parse<'a>(line: primitives::Line<'a>, mut atoms: Vec<Atom>) -> Result<Item<'a>, Err> {
    let mut flags = FLAGS::empty();
    let mut args: Vec<Atom> = Vec::with_capacity(atoms.len());
    let cmd = atoms.remove(0);

    for atom in atoms.iter() {
        // If we have already finished parsing the switches,
        // just fill in the args vec.
        if args.len() > 0 {
            args.push(atom.clone());
            continue;
        }

        match &atom.content {
            AtomContent::Arg(a) => match a.to_lowercase().as_str() {
                "--release" => {
                    flags |= FLAGS::RELEASE;
                    continue;
                }
                "--locked" => {
                    flags |= FLAGS::LOCKED;
                    continue;
                }
                "--inhibited" => {
                    flags |= FLAGS::INHIBITED;
                    continue;
                }
                "--whole-window" => {
                    flags |= FLAGS::BORDER | FLAGS::CONTENTS | FLAGS::TITLEBAR;
                    continue;
                }
                "--border" => {
                    flags |= FLAGS::BORDER;
                    continue;
                }
                "--to-code" => {
                    flags |= FLAGS::CODE;
                    continue;
                }
                "--exclude-titlebar" => {
                    flags |= FLAGS::EXCLUDE_TITLEBAR;
                    continue;
                }
                "--no-warn" => {
                    flags |= FLAGS::NOWARN;
                    continue;
                }
                "--no-repeat" => {
                    flags |= FLAGS::NOREPEAT;
                    continue;
                }
                _ => {}
            },
            _ => {}
        }
        args.push(atom.clone());
    }

    if args.len() < 2 {
        return Err(Err {
            stanza: layout::Stanza::Line { line, atoms },
            err: "bindsym: expected at least 2 non-option arguments".to_string(),
        });
    }

    let keys = match parse_keyset(args[0].clone()) {
        Ok(keyset) => keyset,
        Err(e) => {
            return Err(Err {
                stanza: layout::Stanza::Line { line, atoms },
                err: e.to_string(),
            });
        }
    };
    args.remove(0);

    Ok(Item::BindSym(BindSym {
        cmd,
        flags,
        keys,
        mode: None,
        resolved_keys: None,
        args: match super::parse_line(line, args.clone()) {
            Ok(item) => Subset::Item(Box::new(item)),
            Err(_) => Subset::Unresolved(args),
        },
    }))
}
