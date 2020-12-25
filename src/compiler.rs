use crate::ast::*;
use conch_parser::ast::builder::{AtomicDefaultBuilder, Builder};
use conch_parser::parse::ParseError;
use std::path::PathBuf;

mod includes;

#[derive(Debug)]
pub enum Err {
    /// Failed to build the layout for a file resolved from an include statement.
    IncludeParse(PathBuf),
    /// Failed to build the AST for a file resolved from an include statement.
    IncludeBadAST {
        path: PathBuf,
        err: crate::ast::Err<'static>,
    },
    /// IO error, typically when reading an include file.
    IO(std::io::Error),
    /// Failed to interpret the path, command, or glob argument of an include statement.
    IncludeBadGlob(ParseError<<AtomicDefaultBuilder<String> as Builder>::Error>),
}

/// A single iteration over the AST, populating one class of information.
pub(crate) trait Pass {
    const NAME: &'static str;

    fn exec<'a>(&mut self, config: &mut Vec<Item<'a>>, path: PathBuf) -> Result<(), Err>;
}

fn do_pass<'a, P: Pass>(mut pass: P, config: &mut Vec<Item<'a>>, path: PathBuf) -> Result<(), Err> {
    pass.exec(config, path)
}

/// Fills in information about the provided config in the context of the running system.
///
/// Note: As part of resolving include files, compile may temporarily switch the current
/// working directory of the process. Callers should not perform any operation which relies
/// on a stable working directory concurrently to `compile()`. Unfortunately, this means
/// that compile() may erroneously fail if run in parallel.
pub fn compile<'a>(config: &mut Vec<Item<'a>>, path: PathBuf) -> Result<(), Err> {
    do_pass(includes::ResolverPass, config, path.clone())?;
    do_pass(includes::InlinerPass, config, path)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::*;

    use std::sync::Mutex;
    lazy_static::lazy_static! {
        static ref TEST_MUTEX: Mutex<()> = Mutex::new(());
    }

    #[test]
    fn include_resolver() {
        let _guard = TEST_MUTEX.lock().unwrap();

        let cwd = std::env::current_dir()
            .expect("failed reading CWD")
            .to_path_buf();

        {
            let stanzas = parse_layout("include ../../testdata/*.sway").expect("parsing failed");
            let mut ast = ast::parse(stanzas).expect("AST build failed");

            super::do_pass(
                super::includes::ResolverPass,
                &mut ast,
                "src/compiler/test.sway".into(),
            )
            .expect("relative includes pass failed");

            assert_eq!(ast.len(), 1);
            assert!(
                matches!(&ast[0], ast::Item::Include(ast::Include { resolved, .. }) if resolved.len() >= 6)
            );
        }

        assert_eq!(
            cwd,
            std::env::current_dir()
                .expect("failed reading CWD")
                .to_path_buf()
        );

        {
            let stanzas = parse_layout(
                "include ../../testdata/*.sway\ninclude ../compiler/../../testdata/*.sway",
            )
            .expect("parsing failed");
            let mut ast = ast::parse(stanzas).expect("AST build failed");

            super::do_pass(
                super::includes::ResolverPass,
                &mut ast,
                cwd.join("src/compiler/test.sway"),
            )
            .expect("absolute includes pass failed");

            assert_eq!(ast.len(), 2);
            assert!(
                matches!(&ast[0], ast::Item::Include(ast::Include { resolved, .. }) if resolved.len() >= 6)
            );
            assert!(
                matches!(&ast[1], ast::Item::Include(ast::Include { resolved, .. }) if resolved.len() >= 6)
            );
        }

        {
            let mut inc = String::from("include ");
            inc.push_str(cwd.clone().to_str().unwrap());
            inc.push_str("/testdata/*.sway");
            let stanzas = parse_layout(&inc).expect("parsing failed");
            let mut ast = ast::parse(stanzas).expect("AST build failed");

            super::do_pass(
                super::includes::ResolverPass,
                &mut ast,
                "src/compiler/test.sway".into(),
            )
            .expect("absolute glob includes pass failed");

            assert_eq!(ast.len(), 1);
            assert!(
                matches!(&ast[0], ast::Item::Include(ast::Include { resolved, .. }) if resolved.len() >= 6)
            );
        }

        assert_eq!(
            cwd,
            std::env::current_dir()
                .expect("failed reading CWD")
                .to_path_buf()
        );
    }

    #[test]
    fn include_inliner() {
        let _guard = TEST_MUTEX.lock().unwrap();
        let stanzas = parse_layout("include inc_*.sway").expect("parsing failed");
        let mut ast = ast::parse(stanzas).expect("AST build failed");

        super::compile(&mut ast, "testdata/inc_base.sway".into()).expect("compilation failed");

        eprintln!("ast: {:?}\n\n", ast);
        assert_eq!(ast.len(), 1);
        assert!(
            matches!(&ast[0], ast::Item::Include(ast::Include { resolved, .. }) if resolved.len() == 2 &&
                resolved[0].ast.len() == 0 && resolved[1].ast.len() > 0
            )
        );
    }
}
