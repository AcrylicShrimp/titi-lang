use crate::ASTTopLevel;

#[derive(Default, Debug, Clone)]
pub struct AST {
    top_levels: Vec<ASTTopLevel>,
}

impl AST {
    pub fn top_levels(&self) -> &Vec<ASTTopLevel> {
        &self.top_levels
    }
}
