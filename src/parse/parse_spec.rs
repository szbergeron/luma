use std::path::Path;

use crate::{parse::*, lex::TokenWrapper};

use super::schema::TokenProvider;

#[derive(Clone, Copy, Debug)]
pub enum FileRoleDescriminant {
    Data,
    Spec,
    Source,
}

impl<'lexer> Parser<'lexer> {
    pub fn parse_spec(&mut self, t: &TokenProvider, current_path: &Path) -> ParseResult<Spec> {
        let mut t = parse_header!(t, [Token::Use => 1]);

        let mut res = Vec::new();
        
        //while t.lh.la(0).is_ok() {
        while let Ok(TokenWrapper { token: Token::Identifier, .. }) = t.lh.la(0) {
            println!("La is: {:?}", t.lh.la(0));
            println!("Trying to get a spec");
            res.push(self.parse_spec_declaration(&t, current_path).catch(&mut t).join_hard(&mut t)?);
            println!("Got a spec");
        }

        t.success(Spec { entries: res })
    }

    /// I know all of this is horribly ugly and hacky, I just threw it together to get something
    /// that works well enough for now for building out project structure without needing
    /// to define a whole other lexer/parser for the spec files
    pub fn parse_spec_declaration(&mut self, t: &TokenProvider, current_path: &Path) -> ParseResult<(Vec<IStr>, FileRole)> {
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
            _other => t.failure(ParseResultError::SemanticIssue("expected source, spec, or data, found alternative value", dec_type.start, dec_type.end)).catch(&mut t).join_hard(&mut t)?,
        };

        let file_name = t.take(Token::StringLiteral).join()?;
        let file_name = file_name.slice.resolve();
        let file_name = &file_name[1..file_name.len() - 1]; // strip " from literal
        let file_base = current_path.parent().expect("we weren't in a directory somehow?");
        let file_path = file_base.join(file_name);
        println!("P: {file_path:?}");
        let file_path = file_path.canonicalize().expect("bad path in spec");
        //let file_path = current_path.join(file_name).canonicalize().expect("bad path in spec");
        println!("Filename after cleaning: {file_name}");

        // TODO: nicer handling of this entire file honestly
        //let file_path = PathBuf::from_str(file_name).unwrap().canonicalize().expect("bad path in spec");

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

        println!("get_mount is {get_mount}");

        let mount_point = match get_mount {
            false => {
                vec![]
            },
            true => {
                //t.take(Token::As).join()?;
                //let mount_point = t.take(Token::Identifier).join()?;
                let point = self.parse_scope(&t).join_hard(&mut t).catch(&mut t)?;
                let mut point = point.scope;
                let last = t.take(Token::Identifier).join()?;
                point.push(last.slice);
                //t.take(Token::Semicolon).join()?;
                point
                //MountPoint::Nest(mount_point.slice)
            }
        };

        t.take(Token::Semicolon).join()?;

        let inner = match frd {
            FileRoleDescriminant::Data => FileRole::Data(DataFile {location: file_path}),
            FileRoleDescriminant::Spec => FileRole::Spec(SpecFile {location: file_path}),
            FileRoleDescriminant::Source => FileRole::Source(SourceFile {location: file_path}),
        };

        println!("Returns from parse_spec, val: {inner:?}");

        t.success((mount_point, inner))
    }
}
