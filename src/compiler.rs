use crate::ast::*;
use conch_parser::ast::builder::{AtomicDefaultBuilder, Builder};
use conch_parser::parse::ParseError;
use std::path::PathBuf;

mod includes;

#[derive(Debug)]
pub enum Err {
    BadInclude(ParseError<<AtomicDefaultBuilder<String> as Builder>::Error>),
    IO(std::io::Error),
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
pub fn compile<'a>(config: &mut Vec<Item<'a>>, path: PathBuf) -> Result<(), Err> {
    do_pass(includes::ResolverPass, config, path)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn include_resolver() {
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
            let stanzas = parse_layout("include ../../testdata/*.sway\ninclude ../compiler/../../testdata/*.sway").expect("parsing failed");
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
}
