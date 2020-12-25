use super::Err;
use crate::ast::*;

// use conch_parser::ast::builder::{Builder, RcBuilder};
// use conch_parser::lexer::Lexer;
// use conch_parser::parse::Parser;
use glob::glob as enumerate_glob;
use std::path::PathBuf;

pub(super) struct ResolverPass;

impl super::Pass for ResolverPass {
    const NAME: &'static str = "includes";

    fn exec<'a>(&mut self, config: &mut Vec<Item<'a>>, path: PathBuf) -> Result<(), Err> {
        let conf_dir = path.parent();

        let cwd = std::env::current_dir()
            .map_err(|e| Err::IO(e))?
            .to_path_buf();

        for i in config.iter_mut() {
            let cwd = cwd.clone();
            i.visit::<Err, _>(move |i| {
                if let Item::Include(i) = i {
                    let glob: String = i.glob.content.clone().into();

                    // // Parse as a shell command. Works just fine for globs, so we can safely
                    // // bail here if parsing fails.
                    // let lexer = Lexer::new(glob.chars());
                    // let mut parser = Parser::with_builder(lexer, RcBuilder::new());
                    // let include_ast = parser.complete_command().map_err(|e| Err::BadInclude(e))?;

                    if let Some(parent_dir) = conf_dir {
                        std::env::set_current_dir({
                            if parent_dir.is_relative() && parent_dir != PathBuf::from("") {
                                cwd.clone().join(parent_dir)
                            } else {
                                parent_dir.to_path_buf()
                            }
                        })
                        .map_err(|e| Err::IO(e))?;
                    }
                    let undo_wd = |cwd| {
                        std::env::set_current_dir(cwd).ok();
                    };

                    // Interpret as a glob if its a valid glob.
                    let files: Result<Vec<_>, _> = if let Ok(files) = enumerate_glob(&glob) {
                        files
                            .map(|f| f.map(|f| f.canonicalize().unwrap()))
                            .collect()
                    } else {
                        Ok(Vec::new())
                    };
                    if let Ok(files) = files {
                        for f in files {
                            i.resolved.push(ResolvedInclude {
                                path: f,
                                ast: Vec::new(), // Populated by another pass
                            });
                        }
                    }

                    undo_wd(cwd.clone());
                };
                Ok(())
            })
            .map_err(|e| match e {
                TraversalError::Visit(e) => e,
            })?;
        }

        Ok(())
    }
}
