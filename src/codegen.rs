use std::{collections::HashMap, rc::Rc};

use crate::parser::*;

pub fn generate(ast: Ast) -> String {
    Gen::new(ast).generate()
}

type Symbols = HashMap<Rc<str>, Type>;

fn construct_symbol_table(ast: &Ast) -> HashMap<String, Symbols> {
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

fn construct_fn(f: &Function) -> Symbols {
    let mut symbols = HashMap::new();

    for stmt in &f.body {
        if let Statement::VarDeclaration(name, expr, _) = stmt {
            let typ = get_type(expr, &symbols);

            symbols.insert(name.clone(), typ.to_owned());
        }
    }

    for param in &f.params {
        let name = match &param.name {
            Variable::Const(n) => n,
            Variable::Mutable(n) => n,
        };

        symbols.insert(name.clone(), param.param_type.clone());
    }

    symbols
}

struct Gen {
    symbol_table: HashMap<String, Symbols>,
    ast: Ast,
}

impl Gen {
    fn new(mut ast: Ast) -> Self {
        let symbol_table = construct_symbol_table(&ast);

        edit_main(&mut ast.definitions);

        Self { symbol_table, ast }
    }

    fn generate(&self) -> String {
        let mut output = String::new();

        let includes = vec!["stdio.h", "stdlib.h"];
        write_includes(&mut output, &includes);
        write_lib(&mut output);

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

    fn fmt_fn_def(&self, f: &Function, symbols: &Symbols) -> String {
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

    fn fmt_fn_body(&self, f: &Function, symbols: &Symbols) -> String {
        let mut output = Vec::new();

        let is_void = matches!(&f.return_type, Type::Owned(_) if f.return_type.name() == "()");

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

    fn fmt_stmt(&self, s: &Statement, symbols: &Symbols) -> String {
        let stmt = match s {
            Statement::Expression(expr) => self.fmt_expression(expr, symbols),
            Statement::VarDeclaration(id, expr, _) => self.fmt_var_declaration(id, expr, symbols),
        };

        stmt + ";\n"
    }

    fn fmt_expression(&self, expression: &Expression, symbols: &Symbols) -> String {
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
                self.fmt_fn_call(id, args, symbols)
            }
            Expression::Variable(id) => id.to_string(),
            Expression::MethodCall(target, id, args) => {
                self.fmt_method_call(target, id, args, symbols)
            }
            Expression::Ref(e) | Expression::MutRef(e) => {
                format!("&{}", self.fmt_expression(e, symbols))
            }
        }
    }

    fn fmt_fn_call(&self, id: &Identifier, args: &[Expression], symbols: &Symbols) -> String {
        let (id, args) = match id.segments.as_slice() {
            [x] if x.as_ref() == "println" || x.as_ref() == "print" => {
                self.fmt_print(x, args, symbols)
            }
            _ => (id.segments.join("_"), self.fmt_fn_args(args, symbols)),
        };

        format!("{}({})", id, args)
    }

    fn fmt_method_args(&self, target: String, args: &[Expression], symbols: &Symbols) -> String {
        let mut output = vec![target];

        for a in args {
            output.push(self.fmt_expression(a, symbols));
        }

        output.join(",")
    }

    fn fmt_fn_args(&self, args: &[Expression], symbols: &Symbols) -> String {
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
        symbols: &Symbols,
    ) -> String {
        let mut template = match template {
            Expression::String(s) => s.to_string(),
            _ => panic!("print first argument must be a template string litteral"),
        };

        let mut args = args.iter();

        while template.contains("{}") {
            let arg = args
                .next()
                .expect("Mismatch on args number on template string");

            let typ = get_type(arg, symbols);

            let fmt = get_template_specifier(typ.name());

            template = template.replacen("{}", fmt, 1)
        }

        template
    }

    fn fmt_var_declaration(&self, id: &str, expr: &Expression, symbols: &Symbols) -> String {
        format!(
            "{} {} = {}",
            translate_qualified_type(expr, symbols),
            id,
            self.fmt_expression(expr, symbols)
        )
    }

    fn fmt_print(&self, id: &str, args: &[Expression], symbols: &Symbols) -> (String, String) {
        let ln = if id == "println" { "\\n" } else { "" };

        let mut template = args
            .first()
            .map(|t| self.fmt_template(t, &args[1..], symbols))
            .unwrap_or("".to_string());

        template.push_str(ln);

        let mut final_args =
            vec![self.fmt_expression(&Expression::String(template.into()), symbols)];

        for a in &args[1..] {
            let expr = self.fmt_expression(a, symbols);

            let expr = if get_type(a, symbols).name() == "String" {
                format!("String_fmt(&{})", expr)
            } else {
                expr
            };

            final_args.push(expr);
        }

        ("printf".to_string(), final_args.join(","))
    }

    fn fmt_for(&self, f: &For, symbols: &Symbols) -> String {
        let range = match f.range.2 {
            Range::Inclusive => "<=",
            Range::Exclusive => "<",
        };

        format!(
            "for ({} {} = {}; {} {} {}; ++{}) {{\n{}\n}}",
            translate_type(get_type(&f.range.0, symbols).name()),
            f.indexer_name,
            self.fmt_expression(&f.range.0, symbols),
            f.indexer_name,
            range,
            self.fmt_expression(&f.range.1, symbols),
            f.indexer_name,
            self.fmt_body(&f.body, symbols)
        )
    }

    fn fmt_body(&self, body: &[Statement], symbols: &Symbols) -> String {
        let mut output = Vec::new();

        for s in body {
            output.push(self.fmt_stmt(s, symbols));
        }

        output.join("\n")
    }

    fn fmt_method_call(
        &self,
        target: &Expression,
        name: &str,
        args: &[Expression],
        symbols: &Symbols,
    ) -> String {
        let target_type = get_type(target, symbols);

        let method_type = match (target_type.name(), name) {
            ("String", "push") => CType::Pointer,
            ("String", "replace") => CType::Value,
            _ => panic!("Translation for method {} not found", name),
        };

        let modifier = match (&target_type, method_type) {
            (Type::Owned(_), CType::Pointer) => "&",
            (Type::Owned(_), CType::Value) => "",
            (Type::Ref(_) | Type::MutRef(_), CType::Pointer) => "",
            (Type::Ref(_) | Type::MutRef(_), CType::Value) => "*",
        };

        let target = format!("{}{}", modifier, self.fmt_expression(target, symbols));

        let args = self.fmt_method_args(target, args, symbols);

        format!("{}_{}({})", target_type.name(), name, args)
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
    let main = defs
        .iter_mut()
        .find_map(|d| match d {
            Definition::Function(f) if f.name.as_ref() == "main" => Some(f),
            _ => None,
        })
        .expect("Program doens't contain main function");

    main.return_type = Type::Owned("i32".into());
    main.body.push(Statement::Expression(Expression::I32(0)));

    defs.reverse();
}

fn write_includes(output: &mut String, includes: &[&str]) {
    for include in includes {
        output.push_str(&format!("#include <{}>\n", include));
    }

    output.push('\n');
}

fn translate_qualified_type(expr: &Expression, symbols: &Symbols) -> &'static str {
    let t = get_type(expr, symbols);

    translate_type(t.name())
}

fn get_type(e: &Expression, symbols: &Symbols) -> Type {
    match e {
        Expression::I32(_) => Type::Owned("i32".into()),
        Expression::Char(_) => Type::Owned("char".into()),
        Expression::String(_) => Type::Owned("String".into()),
        Expression::Bool(_) => Type::Owned("bool".into()),
        Expression::For(_) => Type::Owned("()".into()),
        Expression::UnaryOp(_, _) => todo!(),
        Expression::BinaryOp(_, _, _) => todo!(),
        Expression::FunctionCall(id, _) => Type::Owned(get_fn_type(id).into()),
        Expression::MethodCall(_, _, _) => todo!(),
        Expression::DeclMacroCall(_, _) => todo!(),
        Expression::Variable(name) => symbols
            .get(name)
            .cloned()
            .unwrap_or_else(|| panic!("Type for {name} not found")),
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

fn fmt_type(t: &Type) -> String {
    match t {
        Type::Owned(t) => translate_type(t).to_string(),
        Type::Ref(t) => format!("const {}*", translate_type(t)),
        Type::MutRef(t) => format!("{}*", translate_type(t)),
    }
}

fn translate_type(t: &str) -> &'static str {
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

enum CType {
    Pointer,
    Value,
}

fn get_fn_type(id: &Identifier) -> &'static str {
    match id.segments.as_slice() {
        [c, m] if c.as_ref() == "String" && m.as_ref() == "new" => "String",
        [f] if f.as_ref() == "read_line" => "String",
        [f] if f.as_ref() == "read_usize" => "usize",
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
