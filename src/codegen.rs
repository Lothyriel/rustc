use std::collections::HashMap;

use crate::parser::{Ast, Definition, Expression, Function, Identifier, Statement, Variable};

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
        if let Statement::Assignment(name, expr) = stmt {
            let typ = get_type(expr, &symbols).to_owned();
            symbols.insert(name.clone(), typ);
        }
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
        self.drop_dummy_defs();

        let includes = vec!["stdio.h", "stdlib.h"];
        write_includes(&mut output, &includes);
        write_lib(&mut output);

        edit_main_signature(self.ast.definitions.as_mut_slice());

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
        }

        output
    }

    #[allow(clippy::match_like_matches_macro)]
    fn drop_dummy_defs(&mut self) {
        self.ast.definitions.retain(|def| match def.id() {
            "read_line" => false,
            _ => true,
        })
    }

    fn fmt_fn_def(&self, f: &Function, symbols: &HashMap<String, String>) -> String {
        format!(
            "{} {}({}) {{\n{}\n}}",
            translate_type(&f.return_type),
            f.name,
            self.fmt_params(&f.params),
            self.fmt_fn_body(f, symbols),
        )
    }

    fn fmt_params(&self, params: &[Variable]) -> String {
        let mut output = vec![];

        for p in params {
            output.push(format!("{} {}", translate_type(&p.var_type), p.name));
        }

        output.join(",")
    }

    fn fmt_fn_body(&self, f: &Function, symbols: &HashMap<String, String>) -> String {
        let mut output = Vec::new();

        let is_void = f.return_type == "()";

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
            Statement::Assignment(id, expr) => self.fmt_assignment(id, expr, symbols),
        };

        stmt + ";"
    }

    fn fmt_expression(&self, expression: &Expression, symbols: &HashMap<String, String>) -> String {
        match expression {
            Expression::I32(n) => format!("{}", n),
            Expression::Char(c) => format!("{}", c),
            Expression::String(s) => format!("\"{}\"", s),
            Expression::Bool(_) => todo!(),
            Expression::UnaryOp(_, _) => todo!(),
            Expression::BinaryOp(_, _, _) => todo!(),
            Expression::FunctionCall(id, args) | Expression::DeclMacroCall(id, args) => {
                self.format_fn_call(id, args, symbols)
            }
            Expression::Variable(id) => id.clone(),
            Expression::MethodCall(target, id, _) => todo!("Calling {id} on {target:?}"),
            Expression::Ref(_) => todo!(),
            Expression::MutRef(_) => todo!(),
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

            let fmt = get_template_specifier(get_type(arg, symbols));

            template = template.replacen("{}", fmt, 1)
        }

        template
    }

    fn fmt_assignment(
        &self,
        id: &str,
        expr: &Expression,
        symbols: &HashMap<String, String>,
    ) -> String {
        format!(
            "{} {} = {}",
            get_type(expr, symbols),
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

            let expr = if get_type(a, symbols) == "String" {
                format!("String_fmt(&{})", expr)
            } else {
                expr
            };

            final_args.push(expr);
        }

        ("printf".to_string(), final_args.join(","))
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

fn edit_main_signature(defs: &mut [Definition]) {
    let main = defs
        .iter_mut()
        .find_map(|d| match d {
            Definition::Function(f) if f.name == "main" => Some(f),
            _ => None,
        })
        .expect("Program doens't contain main function");

    main.return_type = "i32".to_string();
    main.body.push(Statement::Expression(Expression::I32(0)));
}

fn write_includes(output: &mut String, includes: &[&str]) {
    for include in includes {
        output.push_str(&format!("#include <{}>\n", include));
    }

    output.push('\n');
}

fn get_type<'a>(e: &Expression, symbols: &'a HashMap<String, String>) -> &'a str {
    match e {
        Expression::I32(_) => "i32",
        Expression::Char(_) => "char",
        Expression::String(_) => "String",
        Expression::Bool(_) => "bool",
        Expression::UnaryOp(_, _) => todo!(),
        Expression::BinaryOp(_, _, _) => todo!(),
        Expression::FunctionCall(id, _) => get_fn_type(id),
        Expression::MethodCall(_, _, _) => todo!(),
        Expression::DeclMacroCall(_, _) => todo!(),
        Expression::Variable(name) => symbols
            .get(name)
            .unwrap_or_else(|| panic!("Variable '{name}' not defined {symbols:?}")),
        Expression::Ref(e) => get_type(e, symbols),
        Expression::MutRef(e) => get_type(e, symbols),
    }
}

fn translate_type(t: &str) -> &str {
    match t {
        "()" => "void",
        "u8" => "char",
        "i32" => "int",
        "i64" => "long",
        "u32" => "unsigned",
        "u64" => "unsigned long",
        _ => panic!("Translation for type {t} not found"),
    }
}

fn get_fn_type(id: &Identifier) -> &'static str {
    match id.segments.as_slice() {
        [c, m] if c == "String" && m == "new" => "String",
        [f] if f == "read_line" => "String",
        _ => panic!("Translation for function {:?} not found", id),
    }
}

fn get_fn_template_specifier(id: &Identifier) -> &'static str {
    match id.segments.as_slice() {
        [c, m] if c == "String" && m == "new" => "%s",
        [f] if f == "read_line" => "%s",
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
