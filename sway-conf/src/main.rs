use std::fs::read_to_string;
use std::io::stdout;
use std::path::PathBuf;
use structopt::StructOpt;
use sway_conf::*;

#[macro_use]
extern crate prettytable;
use prettytable::{Cell, Row, Table};

fn main() {
    pretty_env_logger::init();
    let args = Opt::from_args();

    if let Err(e) = match args.cmd {
        Cmd::dumpLayout { file } => dump_layout(file),
        Cmd::dumpAST { file } => dump_ast(file),
        Cmd::dumpCompiledAST { file } => dump_compiled_ast(file),
        Cmd::dumpBindings { file } => dump_bindings(file),
        Cmd::dumpShorthands { file } => dump_shorthands(file),
    } {
        eprintln!("Error: {:?}", e);
    }
}

fn dump_layout(file: PathBuf) -> Result<(), String> {
    let content = read_to_string(file).map_err(|e| format!("IO error: {:?}", e))?;
    let c = parse_layout(&content).map_err(|e| format!("parse error: {:?}", e))?;
    serde_json::to_writer_pretty(stdout(), &c).map_err(|e| format!("output error: {:?}", e))?;
    println!();
    Ok(())
}

fn dump_ast(file: PathBuf) -> Result<(), String> {
    let content = read_to_string(file).map_err(|e| format!("IO error: {:?}", e))?;
    let c = parse_layout(&content).map_err(|e| format!("parse error: {:?}", e))?;
    let ast = ast::parse(c).map_err(|e| format!("AST build error: {:?}", e))?;
    serde_json::to_writer_pretty(stdout(), &ast).map_err(|e| format!("output error: {:?}", e))?;
    println!();
    Ok(())
}

fn dump_compiled_ast(file: PathBuf) -> Result<(), String> {
    let content = read_to_string(file.clone()).map_err(|e| format!("IO error: {:?}", e))?;
    let c = parse_layout(&content).map_err(|e| format!("parse error: {:?}", e))?;
    let mut ast = ast::parse(c).map_err(|e| format!("AST build error: {:?}", e))?;
    compiler::compile(&mut ast, file).map_err(|e| format!("compilation error: {:?}", e))?;

    serde_json::to_writer_pretty(stdout(), &ast).map_err(|e| format!("output error: {:?}", e))?;
    println!();
    Ok(())
}

fn dump_bindings(file: PathBuf) -> Result<(), String> {
    let content = read_to_string(file).map_err(|e| format!("IO error: {:?}", e))?;
    let c = parse_layout(&content).map_err(|e| format!("parse error: {:?}", e))?;
    let ast = ast::parse(c).map_err(|e| format!("AST build error: {:?}", e))?;

    let mut table = Table::new();
    table.set_titles(row!["keys", "action"]);
    for item in ast {
        if let ast::Item::BindSym(ast::BindSym { keys, args, .. }) = item {
            table.add_row(Row::new(vec![
                Cell::new(&linebreak(
                    keys.into_iter()
                        .fold(String::with_capacity(32), |mut acc, k| {
                            if acc.len() > 0 {
                                acc.push_str(" + ");
                            }
                            let k: String = k.into();
                            acc.push_str(&k);
                            acc
                        }),
                    18,
                )),
                Cell::new(&linebreak(
                    match args {
                        ast::Subset::Item(item) => serde_json::to_string_pretty(&item).unwrap(),
                        ast::Subset::Unresolved(args) => {
                            args.into_iter()
                                .fold(String::with_capacity(32), |mut acc, k| {
                                    if acc.len() > 0 {
                                        acc.push_str(" ");
                                    }
                                    let k: String = k.content.into();
                                    acc.push_str(&k);
                                    acc
                                })
                        }
                    },
                    55,
                )),
            ]));
        }
    }

    table.print_tty(false);
    Ok(())
}

fn linebreak(input: String, max_width: usize) -> String {
    let mut out = String::with_capacity(input.len() + 8);
    let mut width = 0;
    for word in input.split(' ') {
        width += word.len() + 1;
        if let Some(amt) = word.find('\n') {
            width = word.len() - amt;
        }
        if width > max_width {
            out.push('\n');
            width = 0;
        }
        out.push_str(word);
        out.push(' ');
    }

    out
}

fn dump_shorthands(file: PathBuf) -> Result<(), String> {
    let content = read_to_string(file).map_err(|e| format!("IO error: {:?}", e))?;
    let c = parse_layout(&content).map_err(|e| format!("parse error: {:?}", e))?;
    let ast = ast::parse(c).map_err(|e| format!("AST build error: {:?}", e))?;

    let mut table = Table::new();
    table.set_titles(row!["name", "values"]);
    for item in ast {
        if let ast::Item::Set(s) = item {
            let v: String = s.variable.content.into();
            table.add_row(Row::new(vec![
                Cell::new(&v),
                Cell::new(&linebreak(
                    s.values
                        .into_iter()
                        .fold(String::with_capacity(32), |mut acc, k| {
                            if acc.len() > 0 {
                                acc.push_str(" ");
                            }
                            let k: String = k.content.into();
                            acc.push_str(&k);
                            acc
                        }),
                    55,
                )),
            ]));
        }
    }

    table.print_tty(false);
    Ok(())
}

#[allow(non_camel_case_types)]
#[derive(StructOpt, Debug, PartialEq)]
pub enum Cmd {
    /// Dump the layout of the given sway config.
    dumpLayout { file: PathBuf },
    /// Dump the AST of the given sway config.
    dumpAST { file: PathBuf },
    /// Dump the compiled AST of the given sway config.
    dumpCompiledAST { file: PathBuf },
    /// Dump the key bindings of the given sway config.
    dumpBindings { file: PathBuf },
    /// Dump the variables in the given sway config.
    dumpShorthands { file: PathBuf },
    // /// flash binary, note includes a verify and reset into app
    // flash {
    //     #[structopt(short = "f", name = "file", long = "file")]
    //     file: PathBuf,
    //     #[structopt(short = "a", name = "address", long = "address", parse(try_from_str = parse_hex_32))]
    //     address: u32,
    // },
}

#[derive(Debug, StructOpt)]
#[structopt(name = "sway-conf", about = "Sway configurator")]
struct Opt {
    #[structopt(subcommand)]
    cmd: Cmd,
    // #[structopt(short = "p", name = "pid", long = "pid", parse(try_from_str = parse_hex_16))]
    // pid: Option<u16>,
    // #[structopt(short = "v", name = "vid", long = "vid", parse(try_from_str = parse_hex_16))]
    // vid: Option<u16>,
}
