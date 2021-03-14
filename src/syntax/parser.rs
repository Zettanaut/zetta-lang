use super::tokens::*;
use super::ast::*;
use crate::span::Span;

use std::collections::HashMap;

struct Scope {
    symbols: HashMap<String, Type>,
    parent: usize,
}

struct ParsingContext {
    current: usize,
    tokens: Vec<Token>,
    types: HashMap<String, Type>,
    current_scope_arena: usize,
    scope_arena: Vec<Scope>,
}

fn initialize_scope(ctx: &mut ParsingContext) {
    let new_scope = Scope { symbols: HashMap::new(), parent: ctx.current_scope_arena } ;
    ctx.scope_arena.push(new_scope);
    ctx.current_scope_arena = ctx.scope_arena.len()-1;
}

fn finalize_scope(ctx: &mut ParsingContext) {
    ctx.current_scope_arena = ctx.scope_arena[ctx.current_scope_arena].parent;
}

fn declare_symbol(name: &String, t: &Type, ctx: &mut ParsingContext) {
    let current_scope = &mut ctx.scope_arena[ctx.current_scope_arena].symbols;
    /*if current_scope.contains_key(name) {
        panic!("Redeclaring symbol {}", name);
    }*/
    current_scope.insert(name.clone(), t.clone());
}

fn lookup_symbol(name: &String, ctx: &mut ParsingContext) -> Type {
    let mut reached_top = false;

    let mut current = ctx.current_scope_arena;
    while !reached_top {
        if current == 0 {
            reached_top = true;
        }
        let current_scope = &ctx.scope_arena[current];
        if let Some(t) = current_scope.symbols.get(name) {
            return t.clone();
        } else {
            current = current_scope.parent;
        }
    }
    panic!("Failed to find symbol {}", name);
}

fn is_done(ctx: &mut ParsingContext) -> bool {
    ctx.current == ctx.tokens.len()
}

fn accept(ctx: &mut ParsingContext, kind: TokenKind) -> bool {
    if !is_done(ctx) && ctx.tokens[ctx.current].kind == kind {
        ctx.current += 1;
        true
    } else {
        false
    }
}

fn expect(ctx: &mut ParsingContext, token: TokenKind) -> bool {
    if accept(ctx, token.clone()) {
        true
    } else {
        panic!("Expected {:?} but got {:?}", token, ctx.tokens[ctx.current]);
    }
}

fn look_ahead(ctx: &mut ParsingContext, offset: usize) -> Token {
    if ctx.current == ctx.tokens.len() { Token { kind: TokenKind::EOF, span: Span::new(0, 0)} }
    else { ctx.tokens[ctx.current + offset].clone() }
}

fn consume(ctx: &mut ParsingContext) -> Token {
    ctx.current += 1;
    ctx.tokens[ctx.current-1].clone()
}

fn parse_if(ctx: &mut ParsingContext) -> Expr {
    let condition = parse_expression(ctx, 0);
    let then = parse_block(ctx);
    let otherwise = if accept(ctx, TokenKind::Else) {
        if accept(ctx, TokenKind::If) {
            let nested = parse_if(ctx);
            let block = Block { stmts: vec![ Stmt { node: StmtKind::Expr(Box::new(nested)) }] };
            Some(Box::new(block))
        } else {
            Some(Box::new(parse_block(ctx)))
        }
    } else {
        None
    };

    Expr::new(ExprKind::If(Box::new(condition), Box::new(then), otherwise), Type::Infer)
}

fn get_precedence(operator: BinaryOperatorKind) -> u32 {
    use BinaryOperatorKind::*;

    match operator {
        Product => 10,
        Division => 10,
        Modulus => 10,
        Addition => 9,
        Subtraction => 9,
        LeftShift => 8,
        RightShift => 8,
        Xor => 6,
        Less => 4,
        LessEq => 4,
        Greater => 4,
        GreaterEq => 4,
        Equality => 4,
        NotEq => 4,
        And => 3,
        Or => 2,
    }
}

fn parse_identifier(ctx: &mut ParsingContext, ident: String) -> Expr {

    let t = lookup_symbol(&ident, ctx);

    Expr::new(ExprKind::Identifier(ident), t)
}

fn parse_prefix_operator(ctx: &mut ParsingContext, token: Token) -> Expr {
    use TokenKind::*;

    let operation = match token.kind {
        Minus => UnaryOperatorKind::Negation,
        Bang => UnaryOperatorKind::Complement,
        And => UnaryOperatorKind::Refer,
        Star => UnaryOperatorKind::Deref,
        _ => panic!("{:?} is not a valid prefix operator!", token.kind),
    };

    let operand = parse_expression(ctx, 11);
    Expr::new(ExprKind::Unary(operation, Box::new(operand)), Type::Infer)
}

fn convert_token_to_binary_operator(token: &TokenKind) -> Option<BinaryOperatorKind> {
    use TokenKind::*;

    match *token {
        Plus => Some(BinaryOperatorKind::Addition),
        Minus => Some(BinaryOperatorKind::Subtraction),
        Star => Some(BinaryOperatorKind::Product),
        Slash => Some(BinaryOperatorKind::Division),
        Percent => Some(BinaryOperatorKind::Modulus),
        Less => Some(BinaryOperatorKind::Less),
        LessEqual => Some(BinaryOperatorKind::LessEq),
        Greater => Some(BinaryOperatorKind::Greater),
        GreaterEqual => Some(BinaryOperatorKind::GreaterEq),
        EqualEqual => Some(BinaryOperatorKind::Equality),
        BangEqual => Some(BinaryOperatorKind::NotEq),
        AndAnd => Some(BinaryOperatorKind::And),
        OrOr => Some(BinaryOperatorKind::Or),
        Hat => Some(BinaryOperatorKind::Xor),
        LessLess => Some(BinaryOperatorKind::LeftShift),
        GreaterGreater => Some(BinaryOperatorKind::RightShift),
        _ => None,
    }
}

fn parse_binary_operator(ctx: &mut ParsingContext, left: Expr, operator: BinaryOperatorKind) -> Expr {

    let precedence = get_precedence(operator);
    let right = parse_expression(ctx, precedence);
    let t = left.t.clone();
    Expr::new(ExprKind::Binary(operator, Box::new(left), Box::new(right)), t)
}


fn parse_member_access(ctx: &mut ParsingContext, left: Expr) -> Expr {
    let field_token = consume(ctx);
    let field_name =
        if let TokenKind::Identifier(ident) = field_token.kind {
            ident
        } else {
            panic!("Token {:?} is not a valid struct field", field_token);
        };

    let mut find_type = |struct_name: String, search_name: String| {
        let search_fields: Vec<(String, Type)> = match ctx.types.get(&struct_name) {
            Some(Type::Struct(_, xs)) => xs.clone(),
            _ => panic!("Unable to access field \"{}\" in {}", search_name, struct_name),
        };

        for (f_n, f_t) in search_fields {
            if f_n == search_name {
                return Some(f_t);
            }
        }
        return None;
    };

    let t = match left.t.clone() {
        Type::Struct(name, _) => find_type(name.clone(), field_name.clone()),
        Type::Ptr(inner) => match *inner {
            Type::Struct(name, _) => find_type(name.clone(), field_name.clone()),
            _ => panic!("Unable to access field \"{}\" in {:?}", field_name, left),
        }
        _ => panic!("Unable to access field \"{}\" in {:?}", field_name, left),
    };

    if t.is_none() {
        panic!("No such field \"{}\" in {:?}", field_name, left.t);
    }

    Expr::new(ExprKind::Member(Box::new(left), field_name), t.unwrap())
}

fn parse_indexing(ctx: &mut ParsingContext, left: Expr) -> Expr {
    let index = parse_expression(ctx, 0);
    expect(ctx, TokenKind::CloseBracket);

    Expr::new(ExprKind::Index(Box::new(left), Box::new(index)), Type::Infer)
}

fn parse_infix_operator(ctx: &mut ParsingContext, left: Expr, token: Token) -> Expr {
    use TokenKind::*;

    if token.kind == OpenParen {
        parse_call(ctx, left)
    } else if token.kind == Dot {
        parse_member_access(ctx, left)
    } else if token.kind == OpenBracket {
        parse_indexing(ctx, left)
    } else if let Some(operator) = convert_token_to_binary_operator(&token.kind) {
        parse_binary_operator(ctx, left, operator)
    } else {
        panic!("Unsupported infix operator: {:?}", token.kind);
    }
}

fn parse_call(ctx: &mut ParsingContext, left: Expr) -> Expr {
    use TokenKind::*;

    let mut args = Vec::new();

    if let ExprKind::Identifier(xs) = left.kind.clone() {
        if xs == "sizeof" {
            let arg = parse_type(ctx);
            expect(ctx, CloseParen);
            let type_name = if let Type::Struct(struct_name, _) = arg.clone() {
                format!("struct {}", struct_name)
            } else {
                panic!("Was unable to sizeof type {:?}", arg);
            };
            return Expr::new(ExprKind::Call(Box::new(left), vec![Box::new(Expr::new(ExprKind::Identifier(type_name),arg))]), Type::Unsigned(IntegerSize::I64));
        }
    }

    if !accept(ctx, CloseParen) {
        loop {
            let expr = parse_expression(ctx, 0);
            args.push(Box::new( expr));
            if !accept(ctx, Comma) { break; }
        }
        expect(ctx, CloseParen);
    }

    Expr::new(ExprKind::Call(Box::new(left),  args), Type::Infer)
}

fn parse_cast(ctx: &mut ParsingContext) -> Expr {
    expect(ctx, TokenKind::OpenParen);
    let target = parse_type(ctx);
    expect(ctx, TokenKind::Comma);
    let inner = parse_expression(ctx, 0);
    expect(ctx, TokenKind::CloseParen);
    Expr::new(ExprKind::Cast(target.clone(), Box::new(inner)), target)
}

fn get_current_precedence(ctx: &mut ParsingContext) -> u32 {
    use TokenKind::*;

    if ctx.tokens.len() <= ctx.current {
        0
    } else {
        let token = ctx.tokens[ctx.current].kind.clone();
        if let Some(op) = convert_token_to_binary_operator(&token) {
            get_precedence(op)
        } else if token == OpenParen || token == Dot || token == OpenBracket {
            12
        } else {
            0
        }
    }
}


fn parse_expression(ctx: &mut ParsingContext, precedence: u32) -> Expr {
    use TokenKind::*;

    let token = consume(ctx);

    let mut left = match token.kind {
        Cast => parse_cast(ctx),
        Identifier(ident) => parse_identifier(ctx, ident.clone()),
        Lit(lit) => Expr::new(ExprKind::Literal(lit), Type::Infer),
        Minus | Bang | And | Star => parse_prefix_operator(ctx, token.clone()),
        OpenParen => {
            let inner = parse_expression(ctx, 0);
            expect(ctx, CloseParen);
            inner
        }
        If => parse_if(ctx),
        _ => panic!("{:?} is not a valid expression prefix", token.kind),
    };

    while precedence < get_current_precedence(ctx) {
        let token = consume(ctx);
        left = parse_infix_operator(ctx, left, token);
    }

    left

}

fn primitive_type_by_name(name: &String, ctx: &ParsingContext) -> Type {
    use Type::*;

    match name.as_ref() {
        "int" => Signed(IntegerSize::Unspecified),
        "float" => Float(FloatingSize::Unspecified),
        "s8" => Signed(IntegerSize::I8),
        "s16" => Signed(IntegerSize::I16),
        "s32" => Signed(IntegerSize::I32),
        "s64" => Signed(IntegerSize::I64),
        "u8" => Unsigned(IntegerSize::I8),
        "u16" => Unsigned(IntegerSize::I16),
        "u32" => Unsigned(IntegerSize::I32),
        "u64" => Unsigned(IntegerSize::I64),
        "f32" => Float(FloatingSize::F32),
        "f64" => Float(FloatingSize::F64),
        "bool" => Bool,
        "char" => Char,
        "void" => Void,
        _ => {
            panic!("{} is not a primitive type", name)
        },
    }
}

fn parse_type(ctx: &mut ParsingContext) -> Type {
    let token = consume(ctx);
    if let TokenKind::Identifier(type_name) = token.kind {
        if let Some(adt) = ctx.types.get(&type_name) {
            adt.clone()
        } else {
            primitive_type_by_name(&type_name, ctx)
        }
    } else if token.kind == TokenKind::Star {
        let inner = parse_type(ctx);
        Type::Ptr(Box::new(inner))
    } else if token.kind == TokenKind::OpenBracket {
        expect(ctx, TokenKind::CloseBracket);
        let inner = parse_type(ctx);
        Type::Slice(Box::new(inner))
    } else {
        panic!("Expected type but got {:?}", token.kind);
    }
}

fn parse_variable_decl(ctx: &mut ParsingContext) -> Item {
    let identifier_token = consume(ctx);
    let identifier =
        if let TokenKind::Identifier(ident) = identifier_token.kind {
            ident
        } else {
            panic!("Tried to parse variable decl starting with token {:?}", identifier_token.kind)
        };
    expect(ctx, TokenKind::Colon);
    let _type = if accept(ctx, TokenKind::Equal) {
        Type::Infer
    } else {
        let t = parse_type(ctx);
        t
    };
    let expr = if accept(ctx, TokenKind::Equal) {
        Some(Box::new(parse_expression(ctx, 0)))
    } else {
        None
    };
    let name = identifier;
    declare_symbol(&name, &_type, ctx);
    let node = ItemKind::VariableDecl(_type, expr);
    Item {name, node, span: identifier_token.span }
}

fn parse_const_decl(ctx: &mut ParsingContext) -> Item {
    panic!("Not yet implemented");
}

fn parse_assignment(place: Expr, ctx: &mut ParsingContext) -> Stmt {
    expect(ctx, TokenKind::Equal);
    let value = parse_expression(ctx, 0);
    Stmt { node: StmtKind::Assignment(Box::new(place), Box::new(value)) }
}

fn parse_stmt(ctx: &mut ParsingContext) -> Stmt {
    use TokenKind::*;

    let mut semicolon_exception = false;

    let result = if accept(ctx, Break) {
        Stmt { node: StmtKind::Break }
    } else if accept(ctx, Continue) {
        Stmt { node: StmtKind::Continue }
    } else if accept(ctx, Return) {
        let expr = parse_expression(ctx, 0);
        Stmt { node: StmtKind::Return(Box::new(expr)) }
    } else if accept(ctx, Defer) {
        let expr = parse_expression(ctx, 0);
        Stmt { node: StmtKind::Defer(Box::new(expr)) }
    } else if accept(ctx, While) {
        let expr = parse_expression(ctx, 0);
        let block = parse_block(ctx);
        semicolon_exception = true;
        Stmt { node: StmtKind::While(Box::new(expr), Box::new(block)) }
    } else if accept(ctx, Semicolon) {
        Stmt { node: StmtKind::Empty }
    } else {

        if let (Identifier(_),Colon) = (look_ahead(ctx, 0).kind, look_ahead(ctx, 1).kind) {
            Stmt { node: StmtKind::Item(Box::new(parse_variable_decl(ctx))) }
        } else {
            let left = parse_expression(ctx, 0);
            let next = look_ahead(ctx, 0);

            //Semicolon exception on if statements to conform to regular C syntax
            match left.kind.clone() {
                ExprKind::If(_,_,_) => semicolon_exception = true,
                _ => {}
            }

            if next.kind == Equal {
                parse_assignment(left, ctx)
            } else if semicolon_exception || next.kind == Semicolon || next.kind == CloseCurly {
                Stmt { node: StmtKind::Expr(Box::new(left)) }
            } else {
                panic!("Unexpected token {:?} ", next);
            }
        }
    };
    if !semicolon_exception && look_ahead(ctx, 0).kind != CloseCurly {
        expect(ctx, Semicolon);
    }
    result
}

fn parse_block(ctx: &mut ParsingContext) -> Block {
    use TokenKind::*;

    let mut stmts = Vec::new();

    if look_ahead(ctx, 0).kind == TokenKind::OpenCurly {
        expect(ctx, OpenCurly);
        while ! accept(ctx, CloseCurly) {
            let stmt = parse_stmt(ctx);
            stmts.push(stmt);
        }
        Block { stmts }
    } else {
        Block { stmts: vec![parse_stmt(ctx)] }
    }
}

fn parse_signature(ctx: &mut ParsingContext) -> Signature {
    use TokenKind::*;

    let mut inputs = Vec::new();

    expect(ctx, OpenParen);
    if !accept(ctx, CloseParen) {
        loop {
            let arg_name_token = consume(ctx);
            let arg_name = if let Identifier(ident) = arg_name_token.kind {
                ident
            } else {
                panic!("Unexpected {:?} token in function signature", arg_name_token.kind);
            };
            expect(ctx, Colon);
            let arg_type = parse_type(ctx);
            inputs.push((arg_type, arg_name));
            if !accept(ctx, Comma) { break; }
        }
        expect(ctx, CloseParen);
    }

    let output = if accept(ctx, Arrow) {
        parse_type(ctx)
    } else {
        Type::Void
    };

    Signature { inputs, output }
}

fn signature_to_function_type(signature: &Signature) -> Type {
    let mut types = Vec::new();
    for (t,n) in &signature.inputs {
        types.push(t.clone());
    }
    Type::Function(types, Box::new(signature.output.clone()))
}

fn parse_function_decl(ctx: &mut ParsingContext) -> Item {
    use TokenKind::*;

    let function_identifier_token = consume(ctx);
    let function_identifier = if let Identifier(ident) = function_identifier_token.kind {
        ident
    } else {
        panic!("Unexpected {:?} token in function declaration", function_identifier_token.kind);
    };
    expect(ctx, ColonColon);
    let signature = parse_signature(ctx);

    declare_symbol(&function_identifier, &signature_to_function_type(&signature), ctx);

    initialize_scope(ctx);

    for (input_type, input_name) in &signature.inputs {
        declare_symbol(input_name, input_type, ctx);
    }

    let block = if look_ahead(ctx, 0).kind == OpenCurly {
        Some(Box::new(parse_block(ctx)))
    } else {
        None
    };


    finalize_scope(ctx);

    let node = ItemKind::FunctionDecl(Box::new(signature), block);

    Item {name: function_identifier, node, span: function_identifier_token.span }
}



fn parse_enum_decl(ctx: &mut ParsingContext) -> Item {
    use TokenKind::*;
    let enum_identifier_token = consume(ctx);
    let type_name = if let Identifier(ident) = enum_identifier_token.kind {
        ident
    } else {
        panic!("Unexpected {:?} token in enum declaration", enum_identifier_token.kind);
    };
    expect(ctx, ColonColon);
    expect(ctx, Enum);
    expect(ctx, OpenCurly);

    let mut variants = Vec::new();

    while !accept(ctx, CloseCurly) {
        let name_token = consume(ctx);
        let variant_name = if let Identifier(ident) = name_token.kind {
            ident
        } else {
            panic!("Expected variant identifier but got {:?}", name_token);
        };
        variants.push(variant_name);
        expect(ctx, TokenKind::Comma);
    }

    let type_def = Type::Enum(type_name.clone(), variants.clone());
    if ctx.types.contains_key(&type_name) {
        panic!("Type {} defined multiple times!", type_name);
    }
    ctx.types.insert(type_name.clone(), type_def.clone());

    for var in variants {
        declare_symbol(&var, &type_def, ctx);
    }

    Item {name: type_name, node: ItemKind::EnumDecl(type_def), span: enum_identifier_token.span }
}

fn parse_struct_decl(ctx: &mut ParsingContext) -> Item {
    use TokenKind::*;
    let struct_identifier_token = consume(ctx);
    let type_name = if let Identifier(ident) = struct_identifier_token.kind {
        ident
    } else {
        panic!("Unexpected {:?} token in struct declaration", struct_identifier_token.kind);
    };
    expect(ctx, ColonColon);
    expect(ctx, Struct);
    expect(ctx, OpenCurly);

    let mut fields = Vec::new();

    while !accept(ctx, CloseCurly) {
        let name_token = consume(ctx);
        let field_name = if let Identifier(ident) = name_token.kind {
            ident
        } else {
            panic!("Expected field identifier but got {:?}", name_token);
        };
        expect(ctx, Colon);
        let field_type = parse_type(ctx);
        fields.push((field_name, field_type));
        expect(ctx, Comma);
    }

    let type_def = Type::Struct(type_name.clone(), fields);
    /*if ctx.types.contains_key(&type_name) {
        panic!("Type {} defined multiple times!", type_name);
    }*/
    ctx.types.insert(type_name.clone(), type_def.clone());

    Item {name: type_name, node: ItemKind::StructDecl(type_def), span: struct_identifier_token.span }
}

fn parse_item(ctx: &mut ParsingContext) -> Item {
    use TokenKind::*;
    let token = look_ahead(ctx, 0);
    if let Identifier(_) = token.kind {}
    else {
        panic!("Tried to parse a item starting with a {:?}", token.kind);
    }
    let result = match look_ahead(ctx, 1).kind {
        Colon => parse_variable_decl(ctx),
        ColonColon => match look_ahead(ctx, 2).kind {
            Enum => parse_enum_decl(ctx),
            Struct => parse_struct_decl(ctx),
            Identifier(_) | Equal => parse_const_decl(ctx),
            OpenParen => parse_function_decl(ctx),
            _ => panic!("Unexpected token {:?} in item", token.kind)
        }
        _ => panic!("Unexpected token {:?} in item", token.kind)
    };
    accept(ctx, Semicolon);
    result
}

pub fn parse(tokens: Vec<Token>) -> Vec<Item> {

    let global_scope = Scope { symbols: HashMap::new(), parent: 0};

    let mut ctx = ParsingContext {current: 0, tokens, types: HashMap::new(), current_scope_arena: 0, scope_arena: vec![global_scope]};

    declare_symbol(&String::from("sizeof"), &Type::Function(vec![Type::Void], Box::new(Type::Unsigned(IntegerSize::I64))), &mut ctx);
    declare_symbol(&String::from("printf"), &Type::Function(vec![Type::Void], Box::new(Type::Void)), &mut ctx);
    declare_symbol(&String::from("sprintf"), &Type::Function(vec![Type::Void], Box::new(Type::Void)), &mut ctx);
    declare_symbol(&String::from("fprintf"), &Type::Function(vec![Type::Void], Box::new(Type::Signed(IntegerSize::I32))), &mut ctx);

    let mut ast = Vec::new();
    while !is_done(&mut ctx) {
        ast.push(parse_item(&mut ctx));
    }
    ast
}
