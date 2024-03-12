use std::collections::HashMap;

use crate::{
    parser::{Ast, InfixOp},
    typecheck::TypeInfo,
    util::idvec::{Id, IdVec},
};

use super::compiler::CompileError;

#[derive(Default, Clone, Debug)]
pub struct Scope {
    pub variables: HashMap<String, VReg>,
}

/// Type name or literal type
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum NamedType {
    Named(String),
    Value(TypeInfo),
}

impl<T: Into<String>> From<T> for NamedType {
    fn from(value: T) -> Self {
        Self::Named(value.into())
    }
}

impl From<TypeInfo> for NamedType {
    fn from(value: TypeInfo) -> Self {
        Self::Value(value)
    }
}

#[derive(Debug, Clone)]
pub struct ValueCell {
    pub typ: NamedType,
}

pub type VReg = Id<ValueCell>;

#[derive(Clone, Debug)]
pub enum IrOp {
    /// IAdd.0 = IAdd.1 + IAdd.2
    IAdd(VReg, VReg, VReg),

    /// ISub.0 = ISub.1 - ISub.2
    ISub(VReg, VReg, VReg),

    /// IDiv.0 = IDiv.1 / IDiv.2
    IDiv(VReg, VReg, VReg),

    /// IMul.0 = IMul.1 * IMul.2
    IMul(VReg, VReg, VReg),

    /// store an immediate integer in a virtual register
    IStoreImm(VReg, i32),

    /// Using params from Call.2 invoke the function in call.1
    /// storing the return value in  Call.0
    Call(VReg, String, Vec<VReg>),

    /// Mark Eq.0 equal to Eq.1 at this point in the program
    Eq(VReg, VReg),
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub paramaters: Vec<(String, TypeInfo)>,
    pub returns: TypeInfo,
}

impl Default for FunctionDeclaration {
    fn default() -> Self {
        FunctionDeclaration {
            paramaters: Vec::new(),
            returns: TypeInfo::Unit,
        }
    }
}

#[derive(Debug, Default)]
pub struct Declarations {
    pub types: HashMap<String, TypeInfo>,
    pub functions: HashMap<String, FunctionDeclaration>,
}

#[derive(Debug, Default)]
pub struct IrCompiler {
    pub declarations: Declarations,
    pub functions: HashMap<String, FrameData>,
}

impl IrCompiler {
    pub fn new() -> IrCompiler {
        IrCompiler::default()
    }

    pub fn scan_declarations<'a>(
        &mut self,
        program: impl Iterator<Item = &'a Ast>,
    ) -> Result<(), CompileError> {
        for toplevel_ast_node in program {
            match toplevel_ast_node {
                Ast::DefFunction {
                    name,
                    params,
                    return_type,
                    body: _body,
                } => {
                    // TODO: params, return type
                    self.declarations.functions.insert(
                        name.clone(),
                        FunctionDeclaration {
                            paramaters: params
                                .iter()
                                .map(|p| (p.name.clone(), TypeInfo::from_ast(&p.typ).into()))
                                .collect(),
                            returns: TypeInfo::from_ast(return_type),
                        },
                    );
                }

                // enumerate ignored items to make force future additions to the ast to be considered here before compiling
                Ast::Number(_)
                | Ast::Ident(_)
                | Ast::Error
                | Ast::Repaired(_)
                | Ast::Block { .. }
                | Ast::StmtIf { .. }
                | Ast::ExprCall { .. }
                | Ast::DefType { .. }
                | Ast::StmtLet { .. }
                | Ast::Expr { .. }
                | Ast::Program { .. } => continue,
            }
        }

        Ok(())
    }

    pub fn compile_functions<'a>(
        &mut self,
        program: impl Iterator<Item = &'a Ast>,
    ) -> Result<(), CompileError> {
        for toplevel_ast_node in program {
            match toplevel_ast_node {
                Ast::DefFunction {
                    name,
                    params: _params,
                    return_type: _return_type,
                    body,
                } => {
                    // TODO: params, return type
                    let decl = self.declarations.functions.get(name).unwrap();
                    let mut framecc = FrameCompiler::from_function_declaration(decl);
                    let _ = framecc.compile(body)?;
                    // TODO handle output
                    self.functions.insert(name.clone(), framecc.into_frame());
                }
                // enumerate ignored items to make force future additions to the ast to be considered here before compiling
                Ast::Number(_)
                | Ast::Ident(_)
                | Ast::Error
                | Ast::Repaired(_)
                | Ast::Block { .. }
                | Ast::StmtIf { .. }
                | Ast::ExprCall { .. }
                | Ast::StmtLet { .. }
                | Ast::DefType { .. }
                | Ast::Expr { .. }
                | Ast::Program { .. } => continue,
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Unit,
    Integer(i32),
}

/// Intermediate representation of a stack frame
#[derive(Debug, Clone)]
pub struct FrameData {
    pub registers: IdVec<ValueCell>,

    /// Registers with their values known at compile time
    pub constants: HashMap<VReg, Value>,
    pub inputs: Vec<VReg>,
    pub output: VReg,
    pub operations: Vec<IrOp>,
}

impl FrameData {
    pub fn resolve_type<'a>(&'a self, typename: &'a NamedType) -> &'a TypeInfo {
        match typename {
            NamedType::Value(t) => t,
            NamedType::Named(_) => todo!("removed name type"),
        }
    }

    pub fn get_type(&self, vreg: VReg) -> &TypeInfo {
        self.resolve_type(&self.registers.get(vreg).typ)
    }
}

#[derive(Debug, Clone)]
pub struct FrameCompiler {
    frame: FrameData,
    scopes: Vec<Scope>,
    unit_register: VReg,
}

impl FrameCompiler {
    pub fn from_function_declaration(decl: &FunctionDeclaration) -> FrameCompiler {
        let mut registers = IdVec::default();
        let unit_register = registers.push(ValueCell {
            typ: TypeInfo::Unit.into(),
        });
        let output = if decl.returns == TypeInfo::Unit {
            unit_register
        } else {
            registers.push(ValueCell {
                typ: decl.returns.clone().into(),
            })
        };

        let mut frame = FrameData {
            registers,
            output,
            constants: HashMap::new(),
            inputs: Vec::with_capacity(decl.paramaters.len()),
            operations: Vec::new(),
        };

        let mut top_scope = Scope::default();
        for (name, typ) in &decl.paramaters {
            let reg = frame.registers.push(ValueCell {
                typ: typ.clone().into(),
            });
            top_scope.variables.insert(name.clone(), reg);
            frame.inputs.push(reg);
        }

        FrameCompiler {
            scopes: vec![top_scope],
            frame,
            unit_register,
        }
    }

    pub fn current_scope(&self) -> &Scope {
        self.scopes.last().expect("frame compiler has no scopes")
    }

    pub fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes
            .last_mut()
            .expect("frame compiler has no scopes")
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    pub fn pop_scope(&mut self) {
        assert!(self.scopes.len() != 1);

        self.scopes.pop();
    }

    pub fn unit(&self) -> VReg {
        self.unit_register
    }

    pub fn get_frame(&self) -> &FrameData {
        &self.frame
    }

    pub fn into_frame(self) -> FrameData {
        self.frame
    }

    pub fn compile_expr(&mut self, expr: &Ast) -> Result<VReg, CompileError> {
        // TODO maybe pass in the result reg, so we're not constantly allocating registers?
        match expr {
            Ast::Number(x) => Ok(self.add_store_integer_imm(*x)),
            Ast::Ident(varname) => Ok(*self
                .current_scope()
                .variables
                .get(varname)
                .expect("no such variable")), // TODO error checking
            Ast::Expr { lhs, op, rhs } => {
                let lhs_vreg = self.compile_expr(lhs)?;
                let rhs_vreg = self.compile_expr(rhs)?;
                self.add_op(*op, lhs_vreg, rhs_vreg)
            }

            Ast::Error => todo!(),
            Ast::Repaired(_) => todo!(),
            Ast::DefFunction { .. } => todo!(),
            Ast::Block {
                statements,
                returns,
            } => {
                let mut last_result = None;
                for stmt in statements {
                    last_result = Some(self.compile_expr(stmt)?);
                }
                if let Some(last_result) = last_result {
                    if *returns {
                        return Ok(last_result);
                    }
                }
                Ok(self.unit())
            }
            Ast::StmtIf { .. } => todo!(),
            Ast::ExprCall { function_name, .. } => {
                let r = self.unit();
                self.frame
                    .operations
                    .push(IrOp::Call(r, function_name.clone(), vec![]));
                Ok(r)
            }
            Ast::StmtLet { .. } => todo!(),
            Ast::DefType { .. } => todo!(),
            Ast::Program { .. } => todo!(),
        }
    }

    pub fn compile(&mut self, expr: &Ast) -> Result<(), CompileError> {
        let result = self.compile_expr(expr)?;
        self.frame
            .operations
            .push(IrOp::Eq(self.frame.output, result));
        Ok(())
    }

    pub fn allocate_register(&mut self, typ: TypeInfo) -> VReg {
        self.frame.registers.push(ValueCell { typ: typ.into() })
    }

    pub fn add_store_integer_imm(&mut self, value: i32) -> VReg {
        let r = self.allocate_register(TypeInfo::integer(value, value).into());
        self.frame.operations.push(IrOp::IStoreImm(r, value));
        r
    }

    pub fn add_op(&mut self, op: InfixOp, lhs: VReg, rhs: VReg) -> Result<VReg, CompileError> {
        let ltyp = self.frame.get_type(lhs);
        let rtyp = self.frame.get_type(rhs);
        let result_type = match (ltyp, rtyp) {
            (TypeInfo::Scalar(lhs), TypeInfo::Scalar(rhs)) => match op {
                InfixOp::Add => lhs.add(rhs),
                _ => todo!(),
            },
            _ => None,
        }
        .ok_or(CompileError::TypeError {
            left: ltyp.clone(),
            right: rtyp.clone(),
        })?;

        let result = self.allocate_register(TypeInfo::Scalar(result_type));

        match op {
            InfixOp::Add => self.frame.operations.push(IrOp::IAdd(result, lhs, rhs)),
            InfixOp::Sub => self.frame.operations.push(IrOp::ISub(result, rhs, lhs)),
            InfixOp::Div => self.frame.operations.push(IrOp::IDiv(result, rhs, lhs)),
            InfixOp::Mul => self.frame.operations.push(IrOp::IMul(result, rhs, lhs)),
            InfixOp::CmpNotEqual => self.frame.operations.push(IrOp::ISub(result, rhs, lhs)),
            InfixOp::CmpEqual => todo!(),
        }

        Ok(result)
    }
}

#[cfg(test)]
mod test {
    use crate::ir::ircompiler::{FrameCompiler, FunctionDeclaration, IrOp};
    use crate::parser::lexer::Lexer;
    use crate::parser::InfixOp;
    use crate::parser::{grammar, Ast, ParseError};
    use crate::typecheck::TypeInfo;

    #[cfg(test)]
    pub fn must_parse_expr(s: &str) -> Ast {
        let lexer = Lexer::new(s);
        let mut recovered_errors = Vec::new();
        let result = grammar::ExprParser::new().parse(&mut recovered_errors, lexer);

        let mut errors: Vec<_> = recovered_errors
            .into_iter()
            .map(|e| ParseError::from(e.error))
            .collect();
        match result {
            Ok(v) => return v,
            Err(e) => {
                errors.push(ParseError::from(e));
            }
        };

        panic!("parser encountered errors: {:?}", errors);
    }

    #[test]
    fn add_arith_op() {
        let mut frame_compiler = FrameCompiler::from_function_declaration(&FunctionDeclaration {
            paramaters: vec![],
            returns: TypeInfo::Unit,
        });
        let l = frame_compiler.allocate_register(TypeInfo::integer(0, 100).into());
        let r = frame_compiler.allocate_register(TypeInfo::integer(0, 100).into());

        let result = frame_compiler.add_op(InfixOp::Add, l, r).unwrap();
        let t = frame_compiler.get_frame().get_type(result);
        assert_eq!(t.clone(), TypeInfo::integer(0, 100));
    }

    #[test]
    fn compile_simple_expr() {
        let mut frame_compiler = FrameCompiler::from_function_declaration(&FunctionDeclaration {
            paramaters: vec![],
            returns: TypeInfo::Unit,
        });

        frame_compiler
            .compile_expr(&must_parse_expr("1 + 2"))
            .expect("compile expr failed");
        assert!(matches!(
            &frame_compiler.get_frame().operations[..],
            &[
                IrOp::IStoreImm(a, 1),
                IrOp::IStoreImm(b, 2),
                IrOp::IAdd(_, a_, b_)
            ] if a_ == a && b_ == b
        ));
    }
}
