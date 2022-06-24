use std::{path::{PathBuf, Path}, str::FromStr};

use crate::parse::*;

use super::schema::TokenProvider;

impl<'lexer> Parser<'lexer> {
    pub fn parse_spec(&mut self, t: &TokenProvider, current_path: &Path) -> ParseResult<Spec> {
        let mut t = parse_header!(t, [Token::Use => 1]);

        let mut res = Vec::new();
        
        while t.lh.la(0).is_ok() {
            res.push(self.parse_spec_declaration(&t, current_path).catch(&mut t).join_hard(&mut t)?);
        }

        t.success(Spec { entries: res })
    }

    /// I know all of this is horribly ugly and hacky, I just threw it together to get something
    /// that works well enough for now for building out project structure without needing
    /// to define a whole other lexer/parser for the spec files
    pub fn parse_spec_declaration(&mut self, t: &TokenProvider, current_path: &Path) -> ParseResult<(MountPoint, FileRole)> {
        let mut t = parse_header!(t,
            [Token::Identifier => 1,
            Token::Identifier => 1,
            Token::StringLiteral => 1,
            Token::Dot => 1,
            Token::Identifier => 1,
            Token::As => 1,
            Token::Identifier => 1]);

        //t.take(Token::Use).join()?;

        let dec_type = t.take(Token::Identifier).join()?;

        let frd = match dec_type.slice.resolve() {
            "source" => FileRoleDescriminant::Source,
            "spec" => FileRoleDescriminant::Spec,
            "data" => FileRoleDescriminant::Data,
            other => t.failure(ParseResultError::SemanticIssue("expected source, spec, or data, found alternative value", dec_type.start, dec_type.end)).catch(&mut t).join_hard(&mut t)?,
        };

        let file_name = t.take(Token::StringLiteral).join()?;
        let file_name = file_name.slice.resolve();
        let file_name = &file_name[1..file_name.len() - 2]; // strip " from literal
        println!("Filename after cleaning: {file_name}");

        // TODO: nicer handling of this entire file honestly
        let file_path = PathBuf::from_str(file_name).unwrap().canonicalize().expect("bad path in spec");

        //let file_path = current_path.join(file_name);

        let get_mount = match frd {
            FileRoleDescriminant::Data => {
                t.take(Token::As).join()?;
                true
            },
            FileRoleDescriminant::Source | FileRoleDescriminant::Spec => {
                t.try_take(Token::As).is_some()
            }
        };

        let mount_point = match get_mount {
            false => MountPoint::Here(),
            true => {
                t.take(Token::As).join()?;
                let mount_point = t.take(Token::Identifier).join()?;
                MountPoint::Nest(mount_point.slice)
            }
        };

        let inner = match frd {
            FileRoleDescriminant::Data => FileRole::Data { path: file_path },
            FileRoleDescriminant::Spec => FileRole::Spec { path: file_path },
            FileRoleDescriminant::Source => FileRole::Source { path: file_path },
        };

        t.success((mount_point, inner))
    }
}
