//A linguage
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>  

/* =================== Config =================== */
#define MAX_TOKEN_LENGTH 128
#define MAX_TOKENS       1024
#define MAX_VARS         512
#define MAX_LINE         2048

/* =================== Diagnóstico =================== */
typedef enum { ERR_NONE, ERR_LEX, ERR_PARSE, ERR_RUNTIME } ErrKind;

typedef struct {
    ErrKind kind;
    int line;
    int col;
    char msg[256];
} Error;

static Error g_error = { ERR_NONE, 0, 0, "" };

static void set_error(ErrKind kind, int line, int col, const char* fmt, ...) {
    if (g_error.kind != ERR_NONE) return; // mantém o primeiro erro
    g_error.kind = kind;
    g_error.line = line;
    g_error.col  = col;
    va_list ap; va_start(ap, fmt);
    vsnprintf(g_error.msg, sizeof(g_error.msg), fmt, ap);
    va_end(ap);
}

static void clear_error(void) { g_error.kind = ERR_NONE; g_error.msg[0] = 0; }

static void print_error_and_flush(const char* phase) {
    fprintf(stderr, "[%s error] line %d, col %d: %s\n",
            phase, g_error.line, g_error.col, g_error.msg);
}

/* =================== Tokens =================== */
typedef enum {
    T_EOF = 0,
    T_UNKNOWN,
    T_NUMBER,
    T_STRING,
    T_IDENTIFIER,

    // símbolos
    T_PLUS, T_MINUS, T_STAR, T_SLASH,
    T_ASSIGN,            // =
    T_EQ, T_NE,          // == !=
    T_LT, T_LE, T_GT, T_GE, // < <= > >=
    T_LPAREN, T_RPAREN,  // ( )
    T_LBRACE, T_RBRACE,  // { }
    T_SEMI, T_COMMA,     // ; ,

    // lógicos
    T_AND, T_OR, T_NOT,  // && || !

    // palavras-chave
    T_KW_IF, T_KW_ELSE, T_KW_WHILE, T_KW_PRINT, T_KW_INPUT
} TokenType;

typedef struct {
    TokenType type;
    char lexeme[MAX_TOKEN_LENGTH];
    int line;
    int col;
} Token;

typedef struct {
    Token data[MAX_TOKENS];
    int count;
} TokenVec;

/* =================== Lexer =================== */
typedef struct {
    const char* src;
    int i;
    int line;
    int col;
    TokenVec* out;
} Lexer;

static int lex_peek(Lexer* L) { return L->src[L->i]; }
static int lex_peek2(Lexer* L) { return L->src[L->i+1]; }
static int lex_advance(Lexer* L) {
    int c = L->src[L->i++];
    if (c == '\n') { L->line++; L->col = 1; } else { L->col++; }
    return c;
}
static void emit(Lexer* L, TokenType t, const char* lex, int line, int col) {
    if (L->out->count >= MAX_TOKENS) {
        set_error(ERR_LEX, line, col, "token buffer overflow");
        return;
    }
    Token* tk = &L->out->data[L->out->count++];
    tk->type = t;
    tk->line = line;
    tk->col  = col;
    if (lex) strncpy(tk->lexeme, lex, MAX_TOKEN_LENGTH-1), tk->lexeme[MAX_TOKEN_LENGTH-1]=0;
    else tk->lexeme[0]=0;
}

static int is_ident_start(int c){ return isalpha(c) || c=='_'; }
static int is_ident_part (int c){ return isalnum(c) || c=='_'; }

static int match(Lexer* L, int expected) {
    if (lex_peek(L) == expected) { lex_advance(L); return 1; }
    return 0;
}

static void lex_all(const char* src, TokenVec* out) {
    Lexer L = { src, 0, 1, 1, out };
    out->count = 0;
    clear_error();

    while (lex_peek(&L)) {
        int c = lex_peek(&L);
        if (isspace(c)) { lex_advance(&L); continue; }

        int line = L.line, col = L.col;

        // números
        if (isdigit(c)) {
            char buf[MAX_TOKEN_LENGTH]; int j=0;
            while (isdigit(lex_peek(&L))) {
                if (j < MAX_TOKEN_LENGTH-1) buf[j++] = lex_advance(&L); else lex_advance(&L);
            }
            buf[j]=0;
            emit(&L, T_NUMBER, buf, line, col);
            if (g_error.kind) return;
            continue;
        }

        // identificadores / palavras-chave
        if (is_ident_start(c)) {
            char buf[MAX_TOKEN_LENGTH]; int j=0;
            while (is_ident_part(lex_peek(&L))) {
                if (j < MAX_TOKEN_LENGTH-1) buf[j++] = lex_advance(&L); else lex_advance(&L);
            }
            buf[j]=0;
            TokenType t = T_IDENTIFIER;
            if      (strcmp(buf,"if")==0)    t=T_KW_IF;
            else if (strcmp(buf,"else")==0)  t=T_KW_ELSE;
            else if (strcmp(buf,"while")==0) t=T_KW_WHILE;
            else if (strcmp(buf,"print")==0) t=T_KW_PRINT;
            else if (strcmp(buf,"input")==0) t=T_KW_INPUT;
            emit(&L, t, buf, line, col);
            if (g_error.kind) return;
            continue;
        }

        // strings
        if (c=='"') {
            lex_advance(&L);
            char buf[512]; int j=0;
            int escaped = 0;
            while (lex_peek(&L)) {
                int ch = lex_advance(&L);
                if (!escaped && ch=='"') break;
                if (!escaped && ch=='\\') { escaped=1; continue; }
                if (escaped) {
                    if      (ch=='n') ch='\n';
                    else if (ch=='t') ch='\t';
                    else if (ch=='r') ch='\r';
                    // caso contrário, mantém o char
                    escaped=0;
                }
                if (j< (int)sizeof(buf)-1) buf[j++]= (char)ch;
            }
            if (L.src[L.i-1] != '"') {
                set_error(ERR_LEX, line, col, "string no closure");
                return;
            }
            buf[j]=0;
            emit(&L, T_STRING, buf, line, col);
            if (g_error.kind) return;
            continue;
        }

        // operadores e pontuação
        switch (c) {
            case '+': lex_advance(&L); emit(&L, T_PLUS, "+", line, col); break;
            case '-': lex_advance(&L); emit(&L, T_MINUS,"-", line, col); break;
            case '*': lex_advance(&L); emit(&L, T_STAR, "*", line, col); break;
            case '/': lex_advance(&L); emit(&L, T_SLASH,"/", line, col); break;
            case '!':
                lex_advance(&L);
                if (match(&L,'=')) emit(&L, T_NE, "!=", line, col);
                else emit(&L, T_NOT, "!", line, col);
                break;
            case '=':
                lex_advance(&L);
                if (match(&L,'=')) emit(&L, T_EQ, "==", line, col);
                else emit(&L, T_ASSIGN, "=", line, col);
                break;
            case '<':
                lex_advance(&L);
                if (match(&L,'=')) emit(&L, T_LE, "<=", line, col);
                else emit(&L, T_LT, "<", line, col);
                break;
            case '>':
                lex_advance(&L);
                if (match(&L,'=')) emit(&L, T_GE, ">=", line, col);
                else emit(&L, T_GT, ">", line, col);
                break;
            case '&':
                lex_advance(&L);
                if (match(&L,'&')) emit(&L, T_AND, "&&", line, col);
                else { set_error(ERR_LEX, line, col, "expected '&' to '&&'"); return; }
                break;
            case '|':
                lex_advance(&L);
                if (match(&L,'|')) emit(&L, T_OR, "||", line, col);
                else { set_error(ERR_LEX, line, col, "expected'|' to'||'"); return; }
                break;
            case '(': lex_advance(&L); emit(&L, T_LPAREN,"(", line, col); break;
            case ')': lex_advance(&L); emit(&L, T_RPAREN,")", line, col); break;
            case '{': lex_advance(&L); emit(&L, T_LBRACE,"{", line, col); break;
            case '}': lex_advance(&L); emit(&L, T_RBRACE,"}", line, col); break;
            case ';': lex_advance(&L); emit(&L, T_SEMI,  ";", line, col); break;
            case ',': lex_advance(&L); emit(&L, T_COMMA, ",", line, col); break;
            default:
                set_error(ERR_LEX, line, col, "unknown caracter'%c'", c);
                return;
        }
        if (g_error.kind) return;
    }
    emit(&L, T_EOF, "", L.line, L.col);
}

/* =================== AST =================== */
typedef enum {
    N_INT, N_STRING, N_VAR,
    N_UNARY,      // op, left
    N_BINARY,     // op, left, right
    N_ASSIGN,     // var-name em value, left=expr
    N_PRINT,      // extra = lista ligada por right
    N_INPUT,      // value = nome var
    N_IF,         // left=cond, extra=then, right=else?
    N_WHILE,      // left=cond, extra=body
    N_BLOCK       // extra = primeiro stmt; encadeado via right
} NodeType;

typedef enum {
    OP_PLUS, OP_MINUS, OP_MUL, OP_DIV,
    OP_EQ, OP_NE, OP_LT, OP_LE, OP_GT, OP_GE,
    OP_AND, OP_OR, OP_NOT,
} OpType;

typedef struct Node {
    NodeType type;
    OpType   op;
    char value[128]; // número em texto, string, ou nome de variável
    struct Node* left;
    struct Node* right;
    struct Node* extra;
    int line, col;
} Node;

static Node* node_new(NodeType t, int line, int col) {
    Node* n = (Node*)calloc(1, sizeof(Node));
    n->type = t;
    n->line = line;
    n->col  = col;
    return n;
}

static void node_free(Node* n) {
    if (!n) return;
    node_free(n->left);
    node_free(n->right);
    node_free(n->extra);
    free(n);
}

/* =================== Parser =================== */
typedef struct {
    Token* toks;
    int pos;
    int count;
} Parser;

static Token* P_peek(Parser* P) {
    if (P->pos < P->count) return &P->toks[P->pos];
    return &P->toks[P->count-1];
}
static Token* P_prev(Parser* P) {
    if (P->pos>0) return &P->toks[P->pos-1];
    return &P->toks[0];
}
static int P_match(Parser* P, TokenType t) {
    if (P_peek(P)->type == t) { P->pos++; return 1; }
    return 0;
}
static Token* P_consume(Parser* P, TokenType t, const char* msg) {
    Token* tk = P_peek(P);
    if (tk->type == t) { P->pos++; return tk; }
    set_error(ERR_PARSE, tk->line, tk->col, "%s (i found '%s')", msg, tk->lexeme);
    return NULL;
}

/* Forward decls */
static Node* parse_statement(Parser* P);
static Node* parse_block(Parser* P);
static Node* parse_expression(Parser* P);

/* precedência: || -> && -> igualdade -> relacional -> aditivo -> multiplicativo -> unário -> primário */
static Node* parse_primary(Parser* P) {
    Token* tk = P_peek(P);
    if (tk->type == T_NUMBER) {
        P->pos++;
        Node* n = node_new(N_INT, tk->line, tk->col);
        strncpy(n->value, tk->lexeme, sizeof(n->value)-1);
        return n;
    }
    if (tk->type == T_STRING) {
        P->pos++;
        Node* n = node_new(N_STRING, tk->line, tk->col);
        strncpy(n->value, tk->lexeme, sizeof(n->value)-1);
        return n;
    }
    if (tk->type == T_IDENTIFIER) {
        P->pos++;
        Node* n = node_new(N_VAR, tk->line, tk->col);
        strncpy(n->value, tk->lexeme, sizeof(n->value)-1);
        return n;
    }
    if (P_match(P, T_LPAREN)) {
        Node* e = parse_expression(P);
        if (!e) return NULL;
        if (!P_consume(P, T_RPAREN, "expected ')'")) { node_free(e); return NULL; }
        return e;
    }
    set_error(ERR_PARSE, tk->line, tk->col, "invalid primary expression");
    return NULL;
}

static Node* make_unary(OpType op, Node* a, int line, int col) {
    Node* n = node_new(N_UNARY, line, col); n->op = op; n->left = a; return n;
}
static Node* make_binary(OpType op, Node* a, Node* b, int line, int col) {
    Node* n = node_new(N_BINARY, line, col); n->op = op; n->left = a; n->right = b; return n;
}

static Node* parse_unary(Parser* P) {
    Token* tk = P_peek(P);
    if (tk->type == T_NOT || tk->type == T_MINUS || tk->type == T_PLUS) {
        P->pos++;
        Node* rhs = parse_unary(P);
        if (!rhs) return NULL;
        OpType op = (tk->type==T_NOT)? OP_NOT : (tk->type==T_MINUS? OP_MINUS : OP_PLUS);
        return make_unary(op, rhs, tk->line, tk->col);
    }
    return parse_primary(P);
}

static Node* parse_mul(Parser* P) {
    Node* left = parse_unary(P);
    if (!left) return NULL;
    for (;;) {
        Token* tk = P_peek(P);
        if (tk->type == T_STAR || tk->type == T_SLASH) {
            P->pos++;
            Node* right = parse_unary(P);
            if (!right) { node_free(left); return NULL; }
            left = make_binary(tk->type==T_STAR?OP_MUL:OP_DIV, left, right, tk->line, tk->col);
        } else break;
    }
    return left;
}

static Node* parse_add(Parser* P) {
    Node* left = parse_mul(P);
    if (!left) return NULL;
    for (;;) {
        Token* tk = P_peek(P);
        if (tk->type == T_PLUS || tk->type == T_MINUS) {
            P->pos++;
            Node* right = parse_mul(P);
            if (!right) { node_free(left); return NULL; }
            left = make_binary(tk->type==T_PLUS?OP_PLUS:OP_MINUS, left, right, tk->line, tk->col);
        } else break;
    }
    return left;
}

static Node* parse_rel(Parser* P) {
    Node* left = parse_add(P);
    if (!left) return NULL;
    for (;;) {
        Token* tk = P_peek(P);
        OpType op;
        int match = 1;
        switch (tk->type) {
            case T_LT: op=OP_LT; break;
            case T_LE: op=OP_LE; break;
            case T_GT: op=OP_GT; break;
            case T_GE: op=OP_GE; break;
            default: match=0; break;
        }
        if (!match) break;
        P->pos++;
        Node* right = parse_add(P);
        if (!right) { node_free(left); return NULL; }
        left = make_binary(op, left, right, tk->line, tk->col);
    }
    return left;
}

static Node* parse_eq(Parser* P) {
    Node* left = parse_rel(P);
    if (!left) return NULL;
    for (;;) {
        Token* tk = P_peek(P);
        if (tk->type == T_EQ || tk->type == T_NE) {
            P->pos++;
            Node* right = parse_rel(P);
            if (!right) { node_free(left); return NULL; }
            left = make_binary(tk->type==T_EQ?OP_EQ:OP_NE, left, right, tk->line, tk->col);
        } else break;
    }
    return left;
}

static Node* parse_and(Parser* P) {
    Node* left = parse_eq(P);
    if (!left) return NULL;
    for (;;) {
        Token* tk = P_peek(P);
        if (tk->type == T_AND) {
            P->pos++;
            Node* right = parse_eq(P);
            if (!right) { node_free(left); return NULL; }
            left = make_binary(OP_AND, left, right, tk->line, tk->col);
        } else break;
    }
    return left;
}

static Node* parse_or(Parser* P) {
    Node* left = parse_and(P);
    if (!left) return NULL;
    for (;;) {
        Token* tk = P_peek(P);
        if (tk->type == T_OR) {
            P->pos++;
            Node* right = parse_and(P);
            if (!right) { node_free(left); return NULL; }
            left = make_binary(OP_OR, left, right, tk->line, tk->col);
        } else break;
    }
    return left;
}

static Node* parse_expression(Parser* P) { return parse_or(P); }

static Node* parse_assignment_or_expr_stmt(Parser* P) {
    // lookahead para "IDENT = ..."
    if (P_peek(P)->type == T_IDENTIFIER && P->toks[P->pos+1].type == T_ASSIGN) {
        Token* id = P_consume(P, T_IDENTIFIER, "expected indentifier");
        if (!id) return NULL;
        P_consume(P, T_ASSIGN, "expected '='");
        Node* expr = parse_expression(P);
        if (!expr) return NULL;
        Node* n = node_new(N_ASSIGN, id->line, id->col);
        strncpy(n->value, id->lexeme, sizeof(n->value)-1);
        n->left = expr;
        P_consume(P, T_SEMI, "expected';' after assignment");
        return n;
    } else {
        // expressão solta (ex.: função que não retorna? aqui só avaliamos e descartamos)
        Node* e = parse_expression(P);
        if (!e) return NULL;
        P_consume(P, T_SEMI, "expected';' after expression");
        return e; // como "stmt expr;" (sem efeito prático)
    }
}

static Node* parse_print(Parser* P) {
    Token* kw = P_consume(P, T_KW_PRINT, "expected 'print'");
    if (!kw) return NULL;
    P_consume(P, T_LPAREN, "expected'(' after 'print'");
    Node* first=NULL, *prev=NULL;
    if (P_peek(P)->type != T_RPAREN) {
        for (;;) {
            Node* e = parse_expression(P);
            if (!e) { node_free(first); return NULL; }
            if (!first) first=e; else prev->right=e;
            prev = e;
            if (P_match(P, T_COMMA)) continue;
            break;
        }
    }
    P_consume(P, T_RPAREN, "expected')'");
    P_consume(P, T_SEMI,   "expected ';'  após 'print(...)'");
    Node* n = node_new(N_PRINT, kw->line, kw->col);
    n->extra = first;
    return n;
}

static Node* parse_input(Parser* P) {
    Token* kw = P_consume(P, T_KW_INPUT, "expected'input'");
    if (!kw) return NULL;
    P_consume(P, T_LPAREN, "expected '(' after 'input'");
    Token* id = P_consume(P, T_IDENTIFIER, "expected indentifier ininput(var)");
    if (!id) return NULL;
    P_consume(P, T_RPAREN, "expected ')'");
    P_consume(P, T_SEMI,   "expected after 'input(...)'");
    Node* n = node_new(N_INPUT, kw->line, kw->col);
    strncpy(n->value, id->lexeme, sizeof(n->value)-1);
    return n;
}

static Node* parse_if(Parser* P) {
    Token* kw = P_consume(P, T_KW_IF, "expected 'if'");
    if (!kw) return NULL;
    P_consume(P, T_LPAREN, "expected after '(' após 'if'");
    Node* cond = parse_expression(P);
    if (!cond) return NULL;
    P_consume(P, T_RPAREN, "expected ')'");
    Node* thenB = parse_block(P);
    if (!thenB) { node_free(cond); return NULL; }
    Node* elseB = NULL;
    if (P_match(P, T_KW_ELSE)) {
        elseB = parse_block(P);
        if (!elseB) { node_free(cond); node_free(thenB); return NULL; }
    }
    Node* n = node_new(N_IF, kw->line, kw->col);
    n->left = cond; n->extra = thenB; n->right = elseB;
    return n;
}

static Node* parse_while(Parser* P) {
    Token* kw = P_consume(P, T_KW_WHILE, "expected 'while'");
    if (!kw) return NULL;
    P_consume(P, T_LPAREN, "expected '(' after 'while'");
    Node* cond = parse_expression(P);
    if (!cond) return NULL;
    P_consume(P, T_RPAREN, "expected ')'");
    Node* body = parse_block(P);
    if (!body) { node_free(cond); return NULL; }
    Node* n = node_new(N_WHILE, kw->line, kw->col);
    n->left = cond; n->extra = body;
    return n;
}

static Node* parse_statement(Parser* P) {
    Token* tk = P_peek(P);
    switch (tk->type) {
        case T_KW_PRINT: return parse_print(P);
        case T_KW_INPUT: return parse_input(P);
        case T_KW_IF:    return parse_if(P);
        case T_KW_WHILE: return parse_while(P);
        case T_LBRACE:   return parse_block(P);
        default:         return parse_assignment_or_expr_stmt(P);
    }
}

static Node* parse_block(Parser* P) {
    if (!P_match(P, T_LBRACE)) {
        // bloco reduzido a um único statement (permitimos 'stmt' sem chaves)
        return parse_statement(P);
    }
    Node* first=NULL, *prev=NULL;
    while (P_peek(P)->type != T_RBRACE && P_peek(P)->type != T_EOF) {
        Node* s = parse_statement(P);
        if (!s) { node_free(first); return NULL; }
        if (!first) first=s; else prev->right=s;
        prev=s;
    }
    if (!P_consume(P, T_RBRACE, "expected '}' to close block")) { node_free(first); return NULL; }
    Node* blk = node_new(N_BLOCK, P_prev(P)->line, P_prev(P)->col);
    blk->extra = first;
    return blk;
}

/* =================== VM =================== */
typedef enum { V_INT, V_STRING } ValType;

typedef struct {
    ValType type;
    int i;
    char s[512];
} Value;

typedef struct { char name[64]; Value val; } Var;
static Var g_vars[MAX_VARS]; static int g_varc=0;

static Var* var_find(const char* name) {
    for (int i=0;i<g_varc;i++) if (strcmp(g_vars[i].name, name)==0) return &g_vars[i];
    return NULL;
}
static Var* var_ensure(const char* name) {
    Var* v = var_find(name);
    if (v) return v;
    if (g_varc>=MAX_VARS) return NULL;
    strncpy(g_vars[g_varc].name, name, sizeof(g_vars[g_varc].name)-1);
    g_vars[g_varc].name[sizeof(g_vars[g_varc].name)-1]=0;
    g_vars[g_varc].val.type=V_INT; g_vars[g_varc].val.i=0; g_vars[g_varc].val.s[0]=0;
    return &g_vars[g_varc++];
}

static int truthy(Value v) {
    if (v.type==V_INT) return v.i!=0;
    return v.s[0]!=0;
}

static void print_value(Value v) {
    if (v.type==V_INT) printf("%d", v.i);
    else printf("%s", v.s);
}

static Value V_int(int x){ Value v; v.type=V_INT; v.i=x; v.s[0]=0; return v; }
static Value V_str(const char* s){ Value v; v.type=V_STRING; v.i=0; strncpy(v.s,s,sizeof(v.s)-1); v.s[sizeof(v.s)-1]=0; return v; }

static Value eval(Node* n); // fwd
static void exec_block(Node* n);

/* Helpers de runtime para tipos */
static Value bin_num_num(Node* n, Value a, Value b, OpType op) {
    if (a.type!=V_INT || b.type!=V_INT) {
        set_error(ERR_RUNTIME, n->line, n->col, "operator aritimatic is not int");
        return V_int(0);
    }
    switch (op) {
        case OP_PLUS:  return V_int(a.i + b.i);
        case OP_MINUS: return V_int(a.i - b.i);
        case OP_MUL:   return V_int(a.i * b.i);
        case OP_DIV:
            if (b.i==0) { set_error(ERR_RUNTIME, n->line, n->col, "division by zero"); return V_int(0); }
            return V_int(a.i / b.i);
        default: return V_int(0);
    }
}

static Value cmp_any(Node* n, Value a, Value b, OpType op) {
    // Comparações: se ambos inteiros, compare numericamente; se ambos strings, lexicográfico; senão, erro.
    if (a.type==V_INT && b.type==V_INT) {
        int A=a.i, B=b.i, r=0;
        switch (op){
            case OP_EQ: r=(A==B); break; case OP_NE: r=(A!=B); break;
            case OP_LT: r=(A< B); break; case OP_LE: r=(A<=B); break;
            case OP_GT: r=(A> B); break; case OP_GE: r=(A>=B); break;
            default: r=0; break;
        }
        return V_int(r);
    }
    if (a.type==V_STRING && b.type==V_STRING) {
        int cmp = strcmp(a.s, b.s), r=0;
        switch (op){
            case OP_EQ: r=(cmp==0); break; case OP_NE: r=(cmp!=0); break;
            case OP_LT: r=(cmp< 0); break; case OP_LE: r=(cmp<=0); break;
            case OP_GT: r=(cmp> 0); break; case OP_GE: r=(cmp>=0); break;
            default: r=0; break;
        }
        return V_int(r);
    }
    set_error(ERR_RUNTIME, n->line, n->col, "incompatibles");
    return V_int(0);
}

static Value add_any(Node* n, Value a, Value b) {
    // Se ambos int -> soma; se qualquer é string -> concatena (coerção simples para int->string)
    if (a.type==V_INT && b.type==V_INT) return V_int(a.i + b.i);
    char buf[1024]; buf[0]=0;
    if (a.type==V_STRING) snprintf(buf, sizeof(buf), "%s", a.s);
    else snprintf(buf, sizeof(buf), "%d", a.i);
    size_t len = strlen(buf);
    if (b.type==V_STRING) snprintf(buf+len, sizeof(buf)-len, "%s", b.s);
    else snprintf(buf+len, sizeof(buf)-len, "%d", b.i);
    return V_str(buf);
}

static Value eval(Node* n) {
    if (!n || g_error.kind) return V_int(0);

    switch (n->type) {
        case N_INT:    return V_int(atoi(n->value));
        case N_STRING: return V_str(n->value);
        case N_VAR: {
            Var* v = var_find(n->value);
            if (!v) { set_error(ERR_RUNTIME, n->line, n->col, "var '%s' not defined", n->value); return V_int(0); }
            return v->val;
        }
        case N_UNARY: {
            Value a = eval(n->left);
            if (g_error.kind) return V_int(0);
            switch (n->op) {
                case OP_NOT:   return V_int(!truthy(a));
                case OP_MINUS: if (a.type!=V_INT){ set_error(ERR_RUNTIME, n->line,n->col,"- unary is not int"); return V_int(0);} return V_int(-a.i);
                case OP_PLUS:  if (a.type!=V_INT){ set_error(ERR_RUNTIME, n->line,n->col,"+ unáry is not int"); return V_int(0);} return V_int(+a.i);
                default: return V_int(0);
            }
        }
        case N_BINARY: {
            // curto-circuito em && e ||
            if (n->op==OP_AND) {
                Value L = eval(n->left); if (g_error.kind) return V_int(0);
                if (!truthy(L)) return V_int(0);
                Value R = eval(n->right); if (g_error.kind) return V_int(0);
                return V_int(truthy(R)!=0);
            }
            if (n->op==OP_OR) {
                Value L = eval(n->left); if (g_error.kind) return V_int(0);
                if (truthy(L)) return V_int(1);
                Value R = eval(n->right); if (g_error.kind) return V_int(0);
                return V_int(truthy(R)!=0);
            }
            // demais binários
            Value L = eval(n->left);  if (g_error.kind) return V_int(0);
            Value R = eval(n->right); if (g_error.kind) return V_int(0);
            switch (n->op) {
                case OP_PLUS:  return add_any(n, L, R);
                case OP_MINUS: case OP_MUL: case OP_DIV: return bin_num_num(n, L, R, n->op);
                case OP_EQ: case OP_NE: case OP_LT: case OP_LE: case OP_GT: case OP_GE:
                    return cmp_any(n, L, R, n->op);
                default: return V_int(0);
            }
        }
        case N_ASSIGN: {
            Value v = eval(n->left);
            if (g_error.kind) return V_int(0);
            Var* slot = var_ensure(n->value);
            if (!slot) { set_error(ERR_RUNTIME, n->line, n->col, "var limit"); return V_int(0); }
            slot->val = v;
            return v;
        }
        case N_PRINT: {
            Node* a = n->extra; int first=1;
            while (a && !g_error.kind) {
                Value v = eval(a);
                if (g_error.kind) break;
                if (!first) printf(" ");
                print_value(v);
                first=0;
                a = a->right;
            }
            if (!g_error.kind) printf("\n");
            return V_int(0);
        }
        case N_INPUT: {
            Var* slot = var_ensure(n->value);
            if (!slot) { set_error(ERR_RUNTIME, n->line, n->col, "var limit"); return V_int(0); }
            char buf[512];
            printf("> "); fflush(stdout);
            if (!fgets(buf, sizeof(buf), stdin)) { set_error(ERR_RUNTIME, n->line, n->col, "error in len"); return V_int(0); }
            buf[strcspn(buf,"\n")]=0;
            slot->val = V_str(buf);
            return slot->val;
        }
        case N_IF: {
            Value c = eval(n->left); if (g_error.kind) return V_int(0);
            if (truthy(c)) exec_block(n->extra);
            else if (n->right) exec_block(n->right);
            return V_int(0);
        }
        case N_WHILE: {
            int guard = 1000000; // evita loop infinito acidental
            while (guard-- > 0) {
                Value c = eval(n->left);
                if (g_error.kind) return V_int(0);
                if (!truthy(c)) break;
                exec_block(n->extra);
                if (g_error.kind) return V_int(0);
            }
            if (guard<=0) set_error(ERR_RUNTIME, n->line, n->col, "While error");
            return V_int(0);
        }
        case N_BLOCK: {
            exec_block(n);
            return V_int(0);
        }
        default: return V_int(0);
    }
}

static void exec_block(Node* stmt) {
    Node* cur = (stmt && stmt->type==N_BLOCK)? stmt->extra : stmt;
    while (cur && !g_error.kind) {
        (void)eval(cur);
        cur = cur->right;
    }
}

/* =================== REPL =================== */
static void run_line(const char* line_text, int base_line) {
    TokenVec tv;
    lex_all(line_text, &tv);
    if (g_error.kind) { print_error_and_flush("lexer"); clear_error(); return; }

    Parser P = { tv.data, 0, tv.count };

    // Permitimos: um bloco { ... } OU uma única statement.
    Node* root = NULL;
    if (P_peek(&P)->type == T_LBRACE) {
        root = parse_block(&P);
    } else {
        root = parse_statement(&P);
    }

    if (!root) {
        if (g_error.kind) { print_error_and_flush("parser"); clear_error(); }
        return;
    }

    // Consumir lixo até EOF
    if (P_peek(&P)->type != T_EOF) {
        set_error(ERR_PARSE, P_peek(&P)->line, P_peek(&P)->col, "extra tokens!");
        print_error_and_flush("parser"); clear_error(); node_free(root); return;
    }

    exec_block(root);
    if (g_error.kind) { print_error_and_flush("runtime"); clear_error(); }
    node_free(root);
}

// mini_lang.c - Interpretador simples com sintaxe C-like, erros detalhados e REPL.
// Compile: gcc mini_lang.c -o mini_lang
// Execução: ./mini_lang


 // necessário para va_list em set_error



int process_import(const char *filename, int interactive) {
    char path[512];
    char line[MAX_LINE];

    // monta caminho: libs/<filename>
    snprintf(path, sizeof(path), "libs/%s", filename);

    FILE *f = fopen(path, "r");
    if (!f) {
        fprintf(stderr, "Error: module not found '%s'\n", path);
        return 1;
    }

    while (fgets(line, sizeof(line), f)) {
        line[strcspn(line, "\n")] = 0;
        if (line[0] == 0) continue;
        run_line(line, interactive);
    }

    fclose(f);
    return 0;
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define MAX_LINE 1024

// função para exportar/configurar/escrever em pinos GPIO
void gpio_write(int pin, int value) {
    char path[64];
    FILE *f;

    // exporta o pino (se não existir)
    snprintf(path, sizeof(path), "/sys/class/gpio/gpio%d", pin);
    if (access(path, F_OK) != 0) {
        f = fopen("/sys/class/gpio/export", "w");
        if (f) {
            fprintf(f, "%d", pin);
            fclose(f);
            usleep(100000); // espera 0.1s para sysfs criar o diretório
        }
    }

    // define como saída
    snprintf(path, sizeof(path), "/sys/class/gpio/gpio%d/direction", pin);
    f = fopen(path, "w");
    if (f) {
        fprintf(f, "out");
        fclose(f);
    }

    // escreve valor
    snprintf(path, sizeof(path), "/sys/class/gpio/gpio%d/value", pin);
    f = fopen(path, "w");
    if (f) {
        fprintf(f, "%d", value);
        fclose(f);
    }
}

// processa o comando "com <pin> <state>"
void process_com(char *args) {
    int pin;
    char state[16];

    if (sscanf(args, "%d %15s", &pin, state) == 2) {
        if (strcasecmp(state, "high") == 0) {
            gpio_write(pin, 1);
            printf("[GPIO] Pin %d -> HIGH\n", pin);
        } else if (strcasecmp(state, "low") == 0) {
            gpio_write(pin, 0);
            printf("[GPIO] Pino %d -> LOW\n", pin);
        } else {
            printf("com error:invalid state'\n");
        }
    } else {
        printf("Uso: com <pin> <high|low>\n");
    }
}

int main(int argc, char *argv[]) {
    char line[MAX_LINE];

    if (argc > 1) {
        FILE *f = fopen(argv[1], "r");
        if (!f) {
            perror("Error in open file");
            return 1;
        }
        while (fgets(line, sizeof(line), f)) {
            line[strcspn(line, "\n")] = 0;
            if (line[0] == 0) continue;

            if (strncmp(line, "import ", 7) == 0) {
                process_import(line + 7, 0);
            }
            else if (strncmp(line, "door ", 5) == 0) {
                system(line + 5);
            }
            else if (strncmp(line, "com ", 4) == 0) {
                process_com(line + 4);
            }
            else {
                run_line(line, 0);
            }
        }
        fclose(f);
        return 0;
    }

    printf("A version 6.75 ('exit' to quit).\n");
    for (;;) {
        printf(">>> ");
        if (!fgets(line, sizeof(line), stdin)) break;
        line[strcspn(line, "\n")] = 0;
        if (strcmp(line, "exit") == 0) break;
        if (line[0] == 0) continue;

        if (strncmp(line, "import ", 7) == 0) {
            process_import(line + 7, 1);
        }
        else if (strncmp(line, "door ", 5) == 0) {
            system(line + 5);
        }
        else if (strncmp(line, "com ", 4) == 0) {
            process_com(line + 4);
        }
        else {
            run_line(line, 1);
        }
    }

    return 0;
}
