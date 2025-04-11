use std::collections::HashMap;

use crate::parser::{
    Ast, BinaryOp, Definition, Expression, For, Function, Identifier, Param, Range, Statement,
    Type, Variable,
};

macro_rules! expect_expr_type {
    ($a:expr, $s:expr) => {
        get_type($a, $s).unwrap_or_else(|| panic!("Could not define type for: {:?}", $a))
    };
}

pub fn generate(ast: Ast) -> String {
    let symbol_table = construct_symbol_table(&ast);

    let mut generator = Gen { symbol_table, ast };

    generator.generate()
}

fn construct_symbol_table(ast: &Ast) -> HashMap<String, HashMap<String, String>> {
    let mut symbols = HashMap::new();

    for def in &ast.definitions {
        match def {
            Definition::Function(f) => {
                let id = get_fn_key(def);
                let fn_symbols = construct_fn(f);
                symbols.insert(id, fn_symbols);
            }
            Definition::Struct(_) => todo!(),
            Definition::ImplBlock(_) => todo!(),
        }
    }

    symbols
}

fn get_fn_key(def: &Definition) -> String {
    let key = match def {
        Definition::Struct(_) => "struct",
        Definition::Function(_) => "fn",
        Definition::ImplBlock(_) => "impl",
    };

    format!("{}:{}", key, def.id())
}

fn construct_fn(f: &Function) -> HashMap<String, String> {
    let mut symbols = HashMap::new();

    for stmt in &f.body {
        if let Statement::VarDeclaration(name, expr, typ) = stmt {
            let typ = get_type(expr, &symbols)
                .or(typ.as_deref())
                .unwrap_or_else(|| panic!("Variable '{name}' type not defined"));

            symbols.insert(name.clone(), typ.to_owned());
        }
    }

    for param in &f.params {
        let typ = match &param.param_type {
            Type::Owned(n) => n,
            Type::Ref(n) => n,
            Type::MutRef(n) => n,
        };

        let name = match &param.name {
            Variable::Const(n) => n,
            Variable::Mutable(n) => n,
        };

        symbols.insert(name.clone(), typ.to_owned());
    }

    symbols
}

struct Gen {
    symbol_table: HashMap<String, HashMap<String, String>>,
    ast: Ast,
}

impl Gen {
    pub fn generate(&mut self) -> String {
        let mut output = String::new();

        let includes = vec!["stdio.h", "stdlib.h"];
        write_includes(&mut output, &includes);
        write_lib(&mut output);

        edit_main(self.ast.definitions.as_mut_slice());

        for def in &self.ast.definitions {
            let def_key = get_fn_key(def);

            let symbols = self
                .symbol_table
                .get(&def_key)
                .unwrap_or_else(|| panic!("Symbol table not found for {}", def_key));

            output += &match def {
                Definition::Function(f) => self.fmt_fn_def(f, symbols),
                Definition::Struct(_) => todo!(),
                Definition::ImplBlock(_) => todo!(),
            };

            output.push_str("\n\n");
        }

        output
    }

    fn fmt_fn_def(&self, f: &Function, symbols: &HashMap<String, String>) -> String {
        format!(
            "{} {}({}) {{\n{}\n}}",
            fmt_type(&f.return_type),
            f.name,
            self.fmt_params(&f.params),
            self.fmt_fn_body(f, symbols),
        )
    }

    fn fmt_params(&self, params: &[Param]) -> String {
        let mut output = vec![];

        for p in params {
            let t = fmt_type(&p.param_type);

            let p = match &p.name {
                Variable::Const(n) => format!("{} const {}", t, n),
                Variable::Mutable(n) => format!("{} {}", t, n),
            };

            output.push(p);
        }

        output.join(",")
    }

    fn fmt_fn_body(&self, f: &Function, symbols: &HashMap<String, String>) -> String {
        let mut output = Vec::new();

        let is_void = matches!(&f.return_type, Type::Owned(t) if t == "()");

        for (i, s) in f.body.iter().enumerate() {
            let ret = if !is_void && i == f.body.len() - 1 {
                "return "
            } else {
                ""
            };

            output.push(format!("{}{}", ret, self.fmt_stmt(s, symbols)));
        }

        output.join("\n")
    }

    fn fmt_stmt(&self, s: &Statement, symbols: &HashMap<String, String>) -> String {
        let stmt = match s {
            Statement::Expression(expr) => self.fmt_expression(expr, symbols),
            Statement::VarDeclaration(id, expr, _) => self.fmt_var_declaration(id, expr, symbols),
        };

        stmt + ";\n"
    }

    fn fmt_expression(&self, expression: &Expression, symbols: &HashMap<String, String>) -> String {
        match expression {
            Expression::For(f) => self.fmt_for(f, symbols),
            Expression::I32(n) => format!("{}", n),
            Expression::Char(c) => format!("'{}'", c.escape_default()),
            Expression::String(s) => format!("\"{}\"", s),
            Expression::Bool(_) => todo!(),
            Expression::UnaryOp(_, _) => todo!(),
            Expression::BinaryOp(l, o, r) => {
                format!(
                    "{} {} {}",
                    self.fmt_expression(l, symbols),
                    fmt_bop(o),
                    self.fmt_expression(r, symbols)
                )
            }
            Expression::FunctionCall(id, args) | Expression::DeclMacroCall(id, args) => {
                self.format_fn_call(id, args, symbols)
            }
            Expression::Variable(id) => id.clone(),
            Expression::MethodCall(target, id, args) => {
                let t = get_type(target, symbols)
                    .unwrap_or_else(|| panic!("Cannot find type for {:?}", target));

                format!(
                    "{}_{}({})",
                    t,
                    id,
                    self.fmt_method_args(target, args, symbols)
                )
            }
            Expression::Ref(e) | Expression::MutRef(e) => {
                format!("&{}", self.fmt_expression(e, symbols))
            }
        }
    }

    fn format_fn_call(
        &self,
        id: &Identifier,
        args: &[Expression],
        symbols: &HashMap<String, String>,
    ) -> String {
        let (id, args) = match id.segments.as_slice() {
            [x] if x == "println" || x == "print" => self.format_print(x, args, symbols),
            _ => (id.segments.join("_"), self.fmt_fn_args(args, symbols)),
        };

        format!("{}({})", id, args)
    }

    fn fmt_method_args(
        &self,
        target: &Expression,
        args: &[Expression],
        symbols: &HashMap<String, String>,
    ) -> String {
        let mut output = vec![self.fmt_expression(target, symbols)];

        for a in args {
            output.push(self.fmt_expression(a, symbols));
        }

        output.join(",")
    }

    fn fmt_fn_args(&self, args: &[Expression], symbols: &HashMap<String, String>) -> String {
        let mut output = vec![];

        for a in args {
            output.push(self.fmt_expression(a, symbols));
        }

        output.join(",")
    }

    fn fmt_template(
        &self,
        template: &Expression,
        args: &[Expression],
        symbols: &HashMap<String, String>,
    ) -> String {
        let mut template = match template {
            Expression::String(s) => s.clone(),
            _ => panic!("print first argument must be a template string litteral"),
        };

        let mut args = args.iter();

        while template.contains("{}") {
            let arg = args
                .next()
                .expect("Mismatch on args number on template string");

            let typ = expect_expr_type!(arg, symbols);

            let fmt = get_template_specifier(typ);

            template = template.replacen("{}", fmt, 1)
        }

        template
    }

    fn fmt_var_declaration(
        &self,
        id: &str,
        expr: &Expression,
        symbols: &HashMap<String, String>,
    ) -> String {
        format!(
            "{} {} = {}",
            translate_qualified_type(expr, id, symbols),
            id,
            self.fmt_expression(expr, symbols)
        )
    }

    fn format_print(
        &self,
        id: &str,
        args: &[Expression],
        symbols: &HashMap<String, String>,
    ) -> (String, String) {
        let ln = if id == "println" { "\\n" } else { "" };

        let mut template = args
            .first()
            .map(|t| self.fmt_template(t, &args[1..], symbols))
            .unwrap_or("".to_string());

        template.push_str(ln);

        let mut final_args = vec![self.fmt_expression(&Expression::String(template), symbols)];

        for a in &args[1..] {
            let expr = self.fmt_expression(a, symbols);

            let expr = if expect_expr_type!(a, symbols) == "String" {
                format!("String_fmt(&{})", expr)
            } else {
                expr
            };

            final_args.push(expr);
        }

        ("printf".to_string(), final_args.join(","))
    }

    fn fmt_for(&self, f: &For, symbols: &HashMap<String, String>) -> String {
        let range = match f.range.2 {
            Range::Inclusive => "<=",
            Range::Exclusive => "<",
        };

        format!(
            "for ({} {} = {}; {} {} {}; ++{}) {{\n{}\n}}",
            translate_type(expect_expr_type!(&f.range.0, symbols)),
            f.indexer_name,
            self.fmt_expression(&f.range.0, symbols),
            f.indexer_name,
            range,
            self.fmt_expression(&f.range.1, symbols),
            f.indexer_name,
            self.fmt_body(&f.body, symbols)
        )
    }

    fn fmt_body(&self, body: &[Statement], symbols: &HashMap<String, String>) -> String {
        let mut output = Vec::new();

        for s in body {
            output.push(self.fmt_stmt(s, symbols));
        }

        output.join("\n")
    }
}

fn write_lib(output: &mut String) {
    let vec = &include_str!("templates/vec.ctempl").replace("{T}", "char");
    let string = include_str!("templates/str.cstand");
    let io = include_str!("templates/io.cstand");

    for t in [vec, string, io] {
        output.push_str(t);
        output.push('\n');
    }
}

fn edit_main(defs: &mut [Definition]) {
    defs.reverse();

    let main = defs
        .iter_mut()
        .find_map(|d| match d {
            Definition::Function(f) if f.name == "main" => Some(f),
            _ => None,
        })
        .expect("Program doens't contain main function");

    main.return_type = Type::Owned("i32".to_string());
    main.body.push(Statement::Expression(Expression::I32(0)));
}

fn write_includes(output: &mut String, includes: &[&str]) {
    for include in includes {
        output.push_str(&format!("#include <{}>\n", include));
    }

    output.push('\n');
}

fn translate_qualified_type<'a>(
    expr: &Expression,
    id: &str,
    symbols: &'a HashMap<String, String>,
) -> &'a str {
    let t = get_type(expr, symbols)
        .or_else(|| symbols.get(id).map(String::as_str))
        .unwrap_or_else(|| panic!("Variable {id} type not defined"));

    translate_type(t)
}

// todo maybe this fn should return enum Type
fn get_type<'a>(e: &Expression, symbols: &'a HashMap<String, String>) -> Option<&'a str> {
    match e {
        Expression::I32(_) => Some("i32"),
        Expression::Char(_) => Some("char"),
        Expression::String(_) => Some("String"),
        Expression::Bool(_) => Some("bool"),
        Expression::For(_) => Some("()"),
        Expression::UnaryOp(_, _) => todo!(),
        Expression::BinaryOp(_, _, _) => todo!(),
        Expression::FunctionCall(id, _) => Some(get_fn_type(id)),
        Expression::MethodCall(_, name, _) => get_method_type(name),
        Expression::DeclMacroCall(_, _) => todo!(),
        Expression::Variable(name) => symbols.get(name).map(|n| n.as_str()),
        Expression::Ref(e) => get_type(e, symbols),
        Expression::MutRef(e) => get_type(e, symbols),
    }
}

fn fmt_bop(o: &BinaryOp) -> &str {
    match o {
        BinaryOp::Plus => "+",
        BinaryOp::Minus => "-",
        BinaryOp::Mul => todo!(),
        BinaryOp::Div => todo!(),
        BinaryOp::Mod => todo!(),
        BinaryOp::And => todo!(),
        BinaryOp::Or => todo!(),
    }
}

fn get_method_type(name: &str) -> Option<&'static str> {
    match name {
        "collect" => None,
        _ => None,
    }
}

fn fmt_type(t: &Type) -> String {
    match t {
        Type::Owned(t) => translate_type(t).to_string(),
        Type::Ref(t) => format!("const {}*", translate_type(t)),
        Type::MutRef(t) => format!("{}*", translate_type(t)),
    }
}

fn translate_type(t: &str) -> &str {
    match t {
        "()" => "void",
        "u8" => "char",
        "char" => "char",
        "i32" => "int",
        "i64" => "long",
        "u32" => "unsigned",
        "u64" => "unsigned long",
        "usize" => "size_t",
        "String" => "String",
        _ => panic!("Translation for type {t} not found"),
    }
}

fn get_fn_type(id: &Identifier) -> &'static str {
    match id.segments.as_slice() {
        [c, m] if c == "String" && m == "new" => "String",
        [f] if f == "read_line" => "String",
        [f] if f == "read_usize" => "usize",
        _ => panic!("Translation for function {:?} not found", id),
    }
}

fn get_template_specifier(t: &str) -> &str {
    match t {
        "String" => "%s",
        "u8" => "%c",
        "i32" => "%d",
        "i64" => "%ld",
        "u32" => "u",
        "u64" => "%lu",
        _ => panic!("Template specifier for {t} not found"),
    }
}
