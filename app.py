import streamlit as st
import re

# --------------------------
# AST Nodes
# --------------------------
class Expr: pass

class Var(Expr):
    def __init__(self, name): 
        self.name = name
    def __str__(self): 
        return self.name

class Abs(Expr):
    def __init__(self, param, body): 
        self.param, self.body = param, body
    def __str__(self): 
        return f"(λ{self.param}.{self.body})"

class App(Expr):
    def __init__(self, func, arg): 
        self.func, self.arg = func, arg
    def __str__(self): 
        return f"({self.func} {self.arg})"

class BoolLit(Expr):
    def __init__(self, val): 
        self.val = val
    def __str__(self): 
        return "true" if self.val else "false"

class IntLit(Expr):
    def __init__(self, val): 
        self.val = val
    def __str__(self): 
        return str(self.val)

class If(Expr):
    def __init__(self, cond, then, els): 
        self.cond, self.then, self.els = cond, then, els
    def __str__(self): 
        return f"(if {self.cond} then {self.then} else {self.els})"

class Succ(Expr):
    def __init__(self, n): 
        self.n = n
    def __str__(self): 
        return f"(succ {self.n})"

class Pred(Expr):
    def __init__(self, n): 
        self.n = n
    def __str__(self): 
        return f"(pred {self.n})"

class IsZero(Expr):
    def __init__(self, n): 
        self.n = n
    def __str__(self): 
        return f"(iszero {self.n})"

# --------------------------
# Types
# --------------------------
class Type: 
    pass

class BoolType(Type):  
    def __str__(self): 
        return "Bool"

class IntType(Type):   
    def __str__(self): 
        return "Int"

class FuncType(Type):
    def __init__(self, t1, t2): 
        self.t1, self.t2 = t1, t2
    def __str__(self): 
        return f"({self.t1} → {self.t2})"

# --------------------------
# Type Checking
# --------------------------
def typeof(expr, env):
    if isinstance(expr, Var):
        return env.get(expr.name, None)

    elif isinstance(expr, Abs):
        param_type = env.get(expr.param, None)
        if not param_type: 
            return None
        body_type = typeof(expr.body, env)
        return FuncType(param_type, body_type)

    elif isinstance(expr, App):
        ftype = typeof(expr.func, env)
        atype = typeof(expr.arg, env)
        if isinstance(ftype, FuncType) and str(ftype.t1) == str(atype):
            return ftype.t2
        return None

    elif isinstance(expr, BoolLit): 
        return BoolType()
    elif isinstance(expr, IntLit): 
        return IntType()

    elif isinstance(expr, If):
        cond_t = typeof(expr.cond, env)
        if str(cond_t) != "Bool": 
            return None
        then_t = typeof(expr.then, env)
        else_t = typeof(expr.els, env)
        return then_t if str(then_t) == str(else_t) else None

    elif isinstance(expr, Succ) or isinstance(expr, Pred):
        return IntType() if str(typeof(expr.n, env)) == "Int" else None

    elif isinstance(expr, IsZero):
        return BoolType() if str(typeof(expr.n, env)) == "Int" else None

    return None

# --------------------------
# Evaluation
# --------------------------
def is_value(expr):
    return isinstance(expr, (Abs, BoolLit, IntLit))

def substitute(expr, var, val):
    if isinstance(expr, Var):
        return val if expr.name == var else expr
    elif isinstance(expr, Abs):
        return Abs(expr.param, substitute(expr.body, var, val))
    elif isinstance(expr, App):
        return App(substitute(expr.func, var, val), substitute(expr.arg, var, val))
    elif isinstance(expr, If):
        return If(substitute(expr.cond, var, val),
                  substitute(expr.then, var, val),
                  substitute(expr.els, var, val))
    elif isinstance(expr, Succ): 
        return Succ(substitute(expr.n, var, val))
    elif isinstance(expr, Pred): 
        return Pred(substitute(expr.n, var, val))
    elif isinstance(expr, IsZero): 
        return IsZero(substitute(expr.n, var, val))
    return expr

def step(expr):
    if isinstance(expr, App) and isinstance(expr.func, Abs) and is_value(expr.arg):
        return substitute(expr.func.body, expr.func.param, expr.arg)
    elif isinstance(expr, App):
        if not is_value(expr.func): 
            return App(step(expr.func), expr.arg)
        elif not is_value(expr.arg): 
            return App(expr.func, step(expr.arg))

    elif isinstance(expr, If):
        if isinstance(expr.cond, BoolLit): 
            return expr.then if expr.cond.val else expr.els
        else: 
            return If(step(expr.cond), expr.then, expr.els)

    elif isinstance(expr, Succ):
        if isinstance(expr.n, IntLit): 
            return IntLit(expr.n.val + 1)
        else: 
            return Succ(step(expr.n))

    elif isinstance(expr, Pred):
        if isinstance(expr.n, IntLit): 
            return IntLit(max(0, expr.n.val - 1))
        else: 
            return Pred(step(expr.n))

    elif isinstance(expr, IsZero):
        if isinstance(expr.n, IntLit): 
            return BoolLit(expr.n.val == 0)
        else: 
            return IsZero(step(expr.n))

    return expr

def normalize(expr):
    steps = []
    current = expr
    while True:
        nxt = step(current)
        if str(nxt) == str(current): 
            break
        steps.append(str(nxt))
        current = nxt
    return steps

# --------------------------
# Tokenizer + Parser
# --------------------------
token_pattern = r"(λ|\\|\.|\(|\)|if|then|else|succ|pred|iszero|true|false|[a-zA-Z_][a-zA-Z0-9_]*|[0-9]+)"
def tokenize(s):
    return re.findall(token_pattern, s)

class Parser:
    def __init__(self, tokens): 
        self.tokens, self.pos = tokens, 0
    def peek(self): 
        return self.tokens[self.pos] if self.pos < len(self.tokens) else None
    def eat(self, tok=None):
        cur = self.peek()
        if tok and cur != tok: 
            raise ValueError(f"Expected {tok}, got {cur}")
        self.pos += 1
        return cur

    def parse_expr(self):
        if self.peek() == "if":
            self.eat("if"); cond = self.parse_expr()
            self.eat("then"); then = self.parse_expr()
            self.eat("else"); els = self.parse_expr()
            return If(cond, then, els)
        return self.parse_app()

    def parse_app(self):
        expr = self.parse_atom()
        while True:
            nxt = self.peek()
            if nxt and nxt not in [")", "then", "else"]:
                arg = self.parse_atom()
                expr = App(expr, arg)
            else: 
                break
        return expr

    def parse_atom(self):
        tok = self.peek()
        if tok == "(":
            self.eat("("); e = self.parse_expr(); self.eat(")"); return e
        if tok in ["λ", "\\"]:
            self.eat(); param = self.eat(); self.eat("."); body = self.parse_expr(); return Abs(param, body)
        if tok == "true": self.eat(); return BoolLit(True)
        if tok == "false": self.eat(); return BoolLit(False)
        if tok.isdigit(): self.eat(); return IntLit(int(tok))
        if tok == "succ": self.eat(); return Succ(self.parse_atom())
        if tok == "pred": self.eat(); return Pred(self.parse_atom())
        if tok == "iszero": self.eat(); return IsZero(self.parse_atom())
        if re.match(r"[a-zA-Z_][a-zA-Z0-9_]*", tok): 
            self.eat(); return Var(tok)
        raise ValueError(f"Unexpected token {tok}")

def parse(s):
    tokens = tokenize(s)
    return Parser(tokens).parse_expr()

# --------------------------
# Streamlit UI
# --------------------------
st.title("⚡ Advanced Type Systems Playground (FI1969)")
st.write("Experiment with Untyped & Typed Lambda Calculus, Arithmetic & Boolean Expressions")

expr_input = st.text_input("Enter expression:",
                           "if iszero (pred (succ 0)) then true else false")

env = {"x": IntType(), "y": IntType()}  # Example environment

if st.button("Run"):
    try:
        expr = parse(expr_input)
    except Exception as e:
        st.error(f"Parse Error: {e}")
        st.stop()

    st.subheader("Expression")
    st.code(str(expr))

    st.subheader("Reduction Steps")
    steps = normalize(expr)
    if steps:
        for i, s in enumerate(steps): 
            st.write(f"➡️ {s}")
    else:
        st.write("Already in normal form!")

    st.subheader("Type Checking")
    t = typeof(expr, env)
    if t: 
        st.success(f"Type: {t}")
    else: 
        st.error("Type Error!")
