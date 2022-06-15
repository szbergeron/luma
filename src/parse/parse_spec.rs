use std::path::{PathBuf, Path};

use crate::parse::*;

use super::schema::TokenProvider;

impl<'lexer> Parser<'lexer> {
    pub fn parse_spec(&mut self, t: &TokenProvider, current_path: Path) -> ParseResult<Spec> {
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
    pub fn parse_spec_declaration(&mut self, t: &TokenProvider, current_path: Path) -> ParseResult<FileRole> {
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
        let file_name = file_name[1..file_name.len() - 2]; // strip " from literal
        println!("Filename after cleaning: {file_name}");

        let file_path = current_path.join(file_name);

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
            FileRoleDescriminant::Data => FileRole::Data { path: file_path, mount_at: mount_point },
            FileRoleDescriminant::Spec => FileRole::Spec { path: file_path, mount_at: mount_point },
            FileRoleDescriminant::Source => FileRole::Source { path: file_path, mount_at: mount_point },
        };

        t.success(inner)
    }
}

struct Spec {
    entries: Vec<FileRole>,
}

enum MountPoint {
    /// Acts as basically an "include" statement,
    /// the spec or source elements from the target
    /// module are dumped directly into the current module
    Here(),

    /// States that the referenced element should be mounted
    /// within a module named <.0>
    Nest(IStr),
}

enum FileRole {
    Data { path: PathBuf, mount_at: MountPoint },
    Spec { path: PathBuf, mount_at: MountPoint },
    Source { path: PathBuf, mount_at: MountPoint },
}

enum FileRoleDescriminant {
    Data,
    Spec,
    Source,
}
