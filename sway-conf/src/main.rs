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
    let ast = ast::parse(c).map_err(|e| format!("AST parse error: {:?}", e))?;
    serde_json::to_writer_pretty(stdout(), &ast).map_err(|e| format!("output error: {:?}", e))?;
    println!();
    Ok(())
}

fn dump_bindings(file: PathBuf) -> Result<(), String> {
    let content = read_to_string(file).map_err(|e| format!("IO error: {:?}", e))?;
    let c = parse_layout(&content).map_err(|e| format!("parse error: {:?}", e))?;
    let ast = ast::parse(c).map_err(|e| format!("AST parse error: {:?}", e))?;

    let mut table = Table::new();
    table.set_titles(row!["keys", "action"]);
    for item in ast {
        if let ast::Item::BindSym(b) = item {
            table.add_row(Row::new(vec![
                Cell::new(
                    &b.keys
                        .into_iter()
                        .fold(String::with_capacity(32), |mut acc, k| {
                            if acc.len() > 0 {
                                acc.push_str(" + ");
                            }
                            let k: String = k.into();
                            acc.push_str(&k);
                            acc
                        }),
                ),
                Cell::new(
                    &b.args
                        .into_iter()
                        .fold(String::with_capacity(32), |mut acc, k| {
                            if acc.len() > 0 {
                                acc.push_str(" ");
                            }
                            let k: String = k.content.into();
                            acc.push_str(&k);
                            acc
                        }),
                ),
            ]));
        }
    }

    table.printstd();
    Ok(())
}

fn dump_shorthands(file: PathBuf) -> Result<(), String> {
    let content = read_to_string(file).map_err(|e| format!("IO error: {:?}", e))?;
    let c = parse_layout(&content).map_err(|e| format!("parse error: {:?}", e))?;
    let ast = ast::parse(c).map_err(|e| format!("AST parse error: {:?}", e))?;

    let mut table = Table::new();
    table.set_titles(row!["name", "values"]);
    for item in ast {
        if let ast::Item::Set(s) = item {
            let v: String = s.variable.content.into();
            table.add_row(Row::new(vec![
                Cell::new(&v),
                Cell::new(
                    &s.values
                        .into_iter()
                        .fold(String::with_capacity(32), |mut acc, k| {
                            if acc.len() > 0 {
                                acc.push_str(" ");
                            }
                            let k: String = k.content.into();
                            acc.push_str(&k);
                            acc
                        }),
                ),
            ]));
        }
    }

    table.printstd();
    Ok(())
}

#[allow(non_camel_case_types)]
#[derive(StructOpt, Debug, PartialEq)]
pub enum Cmd {
    /// Dump the layout of the given sway config.
    dumpLayout { file: PathBuf },
    /// Dump the AST of the given sway config.
    dumpAST { file: PathBuf },
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
