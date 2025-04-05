use crate::parser::{Ast, Definition, Expression, Function, Statement, Type, Variable};

impl Ast {
    pub fn generate(self) -> String {
        let mut output = String::new();
        let includes = vec!["stdio.h"];

        add_includes(&mut output, &includes);

        for def in self.definitions {
            output += &match def {
                Definition::Struct(_) => todo!(),
                Definition::Function(f) => format_fn(f),
            };
        }

        output
    }
}

fn format_fn(f: Function) -> String {
    let is_main = f.name == "main" && f.return_type == Type::Unit;

    let return_type = if is_main {
        "int"
    } else {
        &format!("{:?}", format_type(f.return_type))
    };

    format!(
        "{} {}({}) {{ {} }}",
        return_type,
        f.name,
        format_params(f.params),
        format_body(f.body, is_main),
    )
}

fn format_params(params: Vec<Variable>) -> String {
    let mut output = vec![];

    for p in params {
        output.push(format!("{} {}", format_type(p.var_type), p.name));
    }

    output.join(",")
}

fn format_body(stmts: Vec<Statement>, is_main: bool) -> String {
    let mut output = vec![];

    for s in stmts {
        output.push(format_stmt(s) + "\n");
    }

    if is_main {
        output.push("return 0".to_string());
    }

    output.join(",")
}

fn format_stmt(s: Statement) -> String {
    match s {
        Statement::Expression(expression) => format_expression(expression) + ";",
        Statement::Let(_, expression) => todo!(),
    }
}

fn format_expression(expression: Expression) -> String {
    match expression {
        Expression::I32(n) => format!("{}", n),
        Expression::Char(c) => format!("{}", c),
        Expression::String(s) => s,
        Expression::Bool(_) => todo!(),
        Expression::UnaryOp(unary_op, expression) => todo!(),
        Expression::BinaryOp(expression, binary_op, expression1) => todo!(),
        Expression::FunctionCall(id, args) | Expression::DeclMacroCall(id, args) => {
            format!("{}({})", id, format_fn_args(args))
        }
        Expression::Variable(_) => todo!(),
    }
}

fn format_fn_args(args: Vec<Expression>) -> String {
    let mut output = vec![];

    for a in args {
        output.push(format_expression(a));
    }

    output.join(",")
}

fn format_type(t: Type) -> String {
    todo!()
}

fn add_includes(output: &mut String, includes: &[&str]) {
    for include in includes {
        output.push_str(&format!("#include <{}>", include));
    }

    output.push_str("\n\n");
}
