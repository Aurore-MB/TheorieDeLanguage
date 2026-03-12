import ply.lex as lex
import ply.yacc as yacc
from genereTreeGraphviz2 import printTreeGraph
reserved = {
    'print': 'PRINT',
    'if': 'IF',
    'else': 'ELSE',
    'elif': 'ELIF',
    'for': 'FOR',
    'while': 'WHILE'
}

#----------
#--LEXER---
#----------

tokens = [
    'NUMBER','NAME','PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'EGAL','LPAREN', 'RPAREN','SEMICOLON',
    'EGALEGAL', 'INF', 'SUP', 'INFEG', 'SUPFEG','LACC', 'RACC'
]+ list(reserved.values())

t_PLUS      = r'\+'
t_MINUS     = r'-'
t_TIMES     = r'\*'
t_DIVIDE    = r'/'
t_EGAL    = r'='
t_LPAREN    = r'\('
t_RPAREN    = r'\)'
t_SEMICOLON = r';'
t_EGALEGAL   = r'\=='
t_INF       = r'<'
t_SUP       = r'>'
t_INFEG     = r'<='
t_SUPFEG    = r'>='
t_LACC      = r'\{'
t_RACC      = r'\}'
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
        
def p_statement_egal(p):
    'statement : NAME EGAL expression'
    p[0] = ('egal',p[1],p[3])
    
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
    
def p_statement_group(p):
    'statement : LACC bloc RACC'
    p[0] = p[2]

def p_statement_if(p):
    '''statement : IF LPAREN expression RPAREN statement 
    | IF LPAREN expression RPAREN statement ELSE statement'''
    if len(p) == 6:
        p[0] = ('if', p[3], p[5], 'empty')
    else:
        p[0] = ('if', p[3], p[5], p[7])

def p_statement_while(p):
    'statement : WHILE LPAREN expression RPAREN statement'
    p[0] = ('while', p[3], p[5])
    
def p_statement_for(p):
    'statement : FOR LPAREN statement SEMICOLON expression SEMICOLON statement RPAREN statement'
    p[0] = ('for', p[3], p[5], p[7], p[9])

def p_expression_compare_egalegal(p):
    'expression : expression EGALEGAL expression'
    p[0] = ('==', p[1], p[3])

def p_expression_compare_inf(p):
    'expression : expression INF expression'
    p[0] = ('<', p[1], p[3])

def p_expression_compare_sup(p):
    'expression : expression SUP expression'
    p[0] = ('>', p[1], p[3])

def p_expression_compare_infeg(p):
    'expression : expression INFEG expression'
    p[0] = ('<=', p[1], p[3])
    
def p_expression_compare_supfeg(p):
    'expression : expression SUPFEG expression'
    p[0] = ('>=', p[1], p[3])

    

#---------
#--EVAL---
#---------
 
variables = {}

def evalInst(t):
    if t == 'empty': 
        return
    assert type(t) is tuple
    
    if t[0] == 'bloc':
        evalInst(t[1])
        evalInst(t[2])
        
    if t[0] == 'egal':
        variables[t[1]] = evalExpr(t[2])
        
    if t[0] == 'print': 
        print('CALC>', evalExpr(t[1]))
        
    if t[0] == 'if':
        if evalExpr(t[1]):
            evalInst(t[2])
        else:
            evalInst(t[3])
    
    if t[0] == 'while':
        while evalExpr(t[1]):
            evalInst(t[2])
    
    if t[0] == 'for':
        evalInst(t[1])
        while evalExpr(t[2]):
            evalInst(t[4])
            evalInst(t[3])
        

def evalExpr(t):
    if type(t) == int: 
        return t
    if type(t) == float:
        return t
        
    if type(t) == tuple: 
        if t[0] == 'var':
            return variables.get(t[1], 0)
            
        if t[0] == '+': return evalExpr(t[1]) + evalExpr(t[2])
        if t[0] == '-': return evalExpr(t[1]) - evalExpr(t[2])
        if t[0] == '*': return evalExpr(t[1]) * evalExpr(t[2])
        if t[0] == '/': return evalExpr(t[1]) / evalExpr(t[2])
        if t[0] == '==': return evalExpr(t[1]) == evalExpr(t[2])
        if t[0] == '<': return evalExpr(t[1]) < evalExpr(t[2])
        if t[0] == '>': return evalExpr(t[1]) > evalExpr(t[2])
        if t[0] == '<=': return evalExpr(t[1]) <= evalExpr(t[2])
        if t[0] == '>=': return evalExpr(t[1]) >= evalExpr(t[2])
        
        
    return 0

#--------------
#--TEST CODE---
#--------------
s1='x=4;x=x+3;print(x);'
s3='''x=4;
    while(x<30){
        x=x+3;
        print(x);
    }; 
    for(i=0 ;i<4 ;i=i+1){
        print(i*i);
    };'''
parser = yacc.yacc()
print("TEST 1 : ", s1) 
ast = parser.parse(s1)
printTreeGraph(ast)
evalInst(ast)


print("TEST 3 : ", s3)
ast = parser.parse(s3)
printTreeGraph(ast)
evalInst(ast)
