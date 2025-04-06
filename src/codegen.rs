use crate::parser::{Ast, Definition, Expression, Function, Identifier, Statement, Variable};

impl Ast {
    pub fn generate(mut self) -> String {
        let mut output = String::new();

        let includes = vec!["stdio.h"];
        write_includes(&mut output, &includes);

        edit_main_signature(self.definitions.as_mut_slice());

        for def in self.definitions {
            output += &match def {
                Definition::Struct(_) => todo!(),
                Definition::Function(f) => format_fn(&f),
            };
        }

        output
    }
}

fn edit_main_signature(defs: &mut [Definition]) {
    let main = defs
        .iter_mut()
        .find_map(|d| match d {
            Definition::Function(f) if f.id.expect_single() == "main" => Some(f),
            _ => None,
        })
        .expect("Program doens't contain main function");

    main.return_type = "i32".to_string();
    main.body.push(Statement::Expression(Expression::I32(0)));
}

fn format_fn(f: &Function) -> String {
    format!(
        "{} {}({}) {{\n{}\n}}",
        translate_type(&f.return_type),
        f.id.expect_single(),
        format_params(&f.params),
        format_body(&f.body, &f.return_type),
    )
}

fn format_params(params: &[Variable]) -> String {
    let mut output = vec![];

    for p in params {
        output.push(format!("{} {}", translate_type(&p.var_type), p.name));
    }

    output.join(",")
}

fn format_body(stmts: &[Statement], return_type: &str) -> String {
    let mut output = Vec::new();

    let is_void = return_type == "()";

    for (i, s) in stmts.iter().enumerate() {
        let s = if !is_void && i == stmts.len() - 1 {
            format!("return {}", format_stmt(s))
        } else {
            format_stmt(s)
        };

        output.push(s);
    }

    output.join("\n")
}

fn format_stmt(s: &Statement) -> String {
    let stmt = match s {
        Statement::Expression(expression) => format_expression(expression),
        Statement::Assignment(_, expression) => todo!(),
    };

    stmt + ";"
}

fn format_expression(expression: &Expression) -> String {
    match expression {
        Expression::I32(n) => format!("{}", n),
        Expression::Char(c) => format!("{}", c),
        Expression::String(s) => format!("\"{}\"", s),
        Expression::Bool(_) => todo!(),
        Expression::UnaryOp(unary_op, expression) => todo!(),
        Expression::BinaryOp(expression, binary_op, expression1) => todo!(),
        Expression::FunctionCall(id, args) | Expression::DeclMacroCall(id, args) => {
            format!("{}({})", translate_fn(id), format_fn_args(args))
        }
        Expression::Variable(_) => todo!(),
        Expression::MethodCall(_, _, _) => todo!(),
        Expression::Ref(_) => todo!(),
    }
}

fn translate_fn(id: &Identifier) -> &'static str {
    match id.segments.as_slice() {
        [x] if x == "println" || x == "print" => "printf",
        _ => panic!("Translation for function {:?} not found", id),
    }
}

fn translate_type(t: &str) -> &'static str {
    match t {
        "()" => "void",
        "i32" => "int",
        "i64" => "long",
        "u32" => "unsigned int",
        "u64" => "unsigned long",
        _ => panic!("Translation for type {t} not found"),
    }
}

fn format_fn_args(args: &[Expression]) -> String {
    let mut output = vec![];

    for a in args {
        output.push(format_expression(a));
    }

    output.join(",")
}

fn write_includes(output: &mut String, includes: &[&str]) {
    for include in includes {
        output.push_str(&format!("#include <{}>", include));
    }

    output.push_str("\n\n");
}
