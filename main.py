import ply.lex as lex
import ply.yacc as yacc

reserved = {
    'print': 'PRINT'
}

#----------
#--LEXER---
#----------

tokens = [
    'NUMBER','NAME','PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'ASSIGN','LPAREN', 'RPAREN','SEMICOLON'
]+ list(reserved.values())

t_PLUS      = r'\+'
t_MINUS     = r'-'
t_TIMES     = r'\*'
t_DIVIDE    = r'/'
t_ASSIGN    = r'='
t_LPAREN    = r'\('
t_RPAREN    = r'\)'
t_SEMICOLON = r';'
t_ignore    = ' \t'

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'NAME')
    return t

def t_NUMBER(t): 
    r'\d+' 
    t.value = int(t.value) 
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")
    
def t_error(t):
    print("Caractère illégal'%s'" % t.value[0])
    t.lexer.skip(1)
    
lexer = lex.lex()

#-----------
#--PARSER---
#-----------

precedence = (
    ('left','PLUS','MINUS'),
    ('left','TIMES','DIVIDE')
)

def p_start(p):
    'start : bloc'
    p[0] = p[1]

def p_bloc(p):
    '''bloc : bloc statement SEMICOLON
    | statement SEMICOLON'''
    if len(p) == 4:
        p[0] = ('bloc', p[1],p[2])
    else:
        p[0] = ('bloc','empty',p[1])
        
def p_statement_assign(p):
    'statement : NAME ASSIGN expression'
    p[0] = ('assign',p[1],p[3])
    
def p_statement_print(p):
    'statement : PRINT LPAREN expression RPAREN'
    p[0] = ('print', p[3])
    
def p_expression_binop_plus(p):
    'expression : expression PLUS expression'
    p[0] = ('+',p[1],p[3])

def p_expression_binop_minus(p):
    'expression : expression MINUS expression'
    p[0] = ('-',p[1],p[3])

def p_expression_binop_times(p):
    'expression : expression TIMES expression'
    p[0] = ('*',p[1],p[3])

def p_expression_binop_divide(p):
    'expression : expression DIVIDE expression'
    p[0] = ('/',p[1],p[3])
    
def p_expression_group(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]

def p_expression_number(p):
    'expression : NUMBER'
    p[0] = p[1]
    
def p_expression_var(p):
    'expression : NAME'
    p[0] = ('var', p[1])
    
def p_error(p):
    print("Erreur de syntaxe")


# Fonction Eval
 
variables = {}

def evalInst(t):
    if t == 'empty': return
    assert type(t) is tuple
    
    if t[0] == 'bloc':
        evalInst(t[1])
        evalInst(t[2])
        
    if t[0] == 'assign':
        variables[t[1]] = evalExpr(t[2])
        
    if t[0] == 'print': 
        print('CALC>', evalExpr(t[1]))

def evalExpr(t):
    if type(t) is int or type(t) is float: 
        return t
        
    if type(t) is tuple: 
        if t[0] == 'var':
            return variables.get(t[1], 0)
            
        if t[0] == '+': return evalExpr(t[1]) + evalExpr(t[2])
        if t[0] == '-': return evalExpr(t[1]) - evalExpr(t[2])
        if t[0] == '*': return evalExpr(t[1]) * evalExpr(t[2])
        if t[0] == '/': return evalExpr(t[1]) / evalExpr(t[2])
        
    return 0

#--------------
#--TEST CODE---
#--------------

# s1 : affectation, print
s1='x=4;x=x+3;print(x);' 

parser = yacc.yacc()
ast = parser.parse(s1)
print(ast)
evalInst(ast) 
