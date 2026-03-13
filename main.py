import ply.lex as lex
import ply.yacc as yacc
from genereTreeGraphviz2 import printTreeGraph

reserved = {
    'print': 'PRINT',
    'if': 'IF',
    'else': 'ELSE',
    'for': 'FOR',
    'while': 'WHILE',
    'def': 'DEF',       
    'return': 'RETURN', 
}

tokens = [
    'NUMBER', 'NAME', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'EGAL', 'LPAREN', 'RPAREN', 'SEMICOLON', 'COMMA',
    'EGALEGAL', 'INF', 'SUP', 'INFEG', 'SUPFEG', 'LACC', 'RACC',
    'PLUSPLUS', 'MINUSMINUS', 'PLUSEGAL', 'MINUSEGAL', 'TIMESEGAL', 'DIVIDEEGAL',  
    'STRING', 'LBRACKET', 'RBRACKET'                                               
] + list(reserved.values())

t_PLUSPLUS   = r'\+\+'
t_MINUSMINUS = r'--'
t_PLUSEGAL   = r'\+='
t_MINUSEGAL  = r'-='
t_TIMESEGAL  = r'\*='
t_DIVIDEEGAL = r'/='
t_PLUS      = r'\+'
t_MINUS     = r'-'
t_TIMES     = r'\*'
t_DIVIDE    = r'/'
t_EGAL      = r'='
t_LPAREN    = r'\('
t_RPAREN    = r'\)'
t_SEMICOLON = r';'
t_COMMA     = r','
t_EGALEGAL  = r'\=='
t_INF       = r'<'
t_SUP       = r'>'
t_INFEG     = r'<='
t_SUPFEG    = r'>='
t_LACC      = r'\{'
t_RACC      = r'\}'
t_LBRACKET  = r'\['
t_RBRACKET  = r'\]'
t_ignore    = ' \t'

def t_STRING(t):
    r'"[^"]*"|\'[^\']*\''
    t.value = t.value[1:-1] 
    return t

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
    print("Caractère illégal '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

precedence = ( ('left', 'PLUS', 'MINUS'), ('left', 'TIMES', 'DIVIDE'))

def p_start(p):
    'start : bloc'
    p[0] = p[1]

def p_bloc(p):
    '''bloc : bloc statement SEMICOLON
    | bloc definition
    | bloc if_block
    | statement SEMICOLON
    | definition
    | if_block
    | empty'''
    if len(p) == 4:
        p[0] = ('bloc', p[1], p[2])
    elif len(p) == 3:
        p[0] = ('bloc', p[1], p[2])
    elif len(p) == 2:
        p[0] = (p[1])

def p_if_block(p):
    '''if_block : IF LPAREN expression RPAREN LACC bloc RACC
    | IF LPAREN expression RPAREN LACC bloc RACC ELSE LACC bloc RACC'''
    if len(p) == 8:
        p[0] = ('if', p[3], p[6], 'empty')
    else:
        p[0] = ('if', p[3], p[6], p[10])

def p_def(p):
    'definition : DEF NAME LPAREN params RPAREN LACC bloc RACC'
    p[0] = ('def', p[2], p[4], p[7])

def p_params(p):
    '''params : params COMMA NAME
    | NAME
    | empty'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    elif len(p) == 2 and p[1] != 'empty':
        p[0] = [p[1]]
    else:
        p[0] = []

def p_expression_call(p):
    'expression : NAME LPAREN args RPAREN'
    p[0] = ('call', p[1], p[3])

def p_args(p):
    '''args : args COMMA expression
    | expression
    | empty'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    elif len(p) == 2 and p[1] != 'empty':
        p[0] = [p[1]]
    else:
        p[0] = []

def p_statement_call(p):
    'statement : NAME LPAREN args RPAREN'
    p[0] = ('call_stmt', p[1], p[3])

# return coupe-circuit
def p_statement_return(p):
    'statement : RETURN expression'
    p[0] = ('return', p[2])

# affectation
def p_statement_egal(p):
    'statement : NAME EGAL expression'
    p[0] = ('egal', p[1], p[3])

# print multiple
def p_statement_print(p):
    'statement : PRINT LPAREN args RPAREN'
    p[0] = ('print', p[3])

# chaine de caracteres 
def p_expression_string(p):
    'expression : STRING'
    p[0] = p[1]

# tab = [1, 2, 3];
def p_statement_tab_init(p):
    'statement : NAME EGAL LBRACKET args RBRACKET'
    p[0] = ('tab_init', p[1], p[4])

# tab[0]
def p_expression_tab_get(p):
    'expression : NAME LBRACKET expression RBRACKET'
    p[0] = ('tab_get', p[1], p[3])

# tab[1] = 99;
def p_statement_tab_set(p):
    'statement : NAME LBRACKET expression RBRACKET EGAL expression'
    p[0] = ('tab_set', p[1], p[3], p[6])

# expression arithmetique

def p_expression_binop_plus(p):
    'expression : expression PLUS expression'
    p[0] = ('+', p[1], p[3])

def p_expression_binop_minus(p):
    'expression : expression MINUS expression'
    p[0] = ('-', p[1], p[3])

def p_expression_binop_times(p):
    'expression : expression TIMES expression'
    p[0] = ('*', p[1], p[3])

def p_expression_binop_divide(p):
    'expression : expression DIVIDE expression'
    p[0] = ('/', p[1], p[3])

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

def p_empty(p):
    'empty :'
    pass

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

def p_statement_plusplus(p):
    'statement : NAME PLUSPLUS'
    p[0] = ('egal', p[1], ('+', ('var', p[1]), 1))

def p_statement_minusminus(p):
    'statement : NAME MINUSMINUS'
    p[0] = ('egal', p[1], ('-', ('var', p[1]), 1))

def p_statement_plusegal(p):
    'statement : NAME PLUSEGAL expression'
    p[0] = ('egal', p[1], ('+', ('var', p[1]), p[3]))

def p_statement_minusegal(p):
    'statement : NAME MINUSEGAL expression'
    p[0] = ('egal', p[1], ('-', ('var', p[1]), p[3]))

def p_statement_timesegal(p):
    'statement : NAME TIMESEGAL expression'
    p[0] = ('egal', p[1], ('*', ('var', p[1]), p[3]))

def p_statement_divideegal(p):
    'statement : NAME DIVIDEEGAL expression'
    p[0] = ('egal', p[1], ('/', ('var', p[1]), p[3]))


variables = {}
fonctions = {}

# exception pour le return coupe-circuit
class ReturnException(Exception):
    def __init__(self, value):
        self.value = value

def evalInst(t):
    if t == 'empty':
        return
    if type(t) != tuple:
        return

    # bloc
    if t[0] == 'bloc':
        evalInst(t[1])
        evalInst(t[2])

    # definition de fonction
    if t[0] == 'def':
        fonctions[t[1]] = (t[2], t[3])

    # appel de fonction comme instruction
    if t[0] == 'call_stmt':
        evalExpr(('call', t[1], t[2]))

    # affectation
    if t[0] == 'egal':
        variables[t[1]] = evalExpr(t[2])

    # print multiple
    if t[0] == 'print':
        valeurs = [str(evalExpr(arg)) for arg in t[1]]
        print('calc >', ' '.join(valeurs))

    # tableaux
    if t[0] == 'tab_init':
        variables[t[1]] = [evalExpr(e) for e in t[2]]

    if t[0] == 'tab_set':
        variables[t[1]][evalExpr(t[2])] = evalExpr(t[3])

    # return coupe-circuit
    if t[0] == 'return':
        raise ReturnException(evalExpr(t[1]))

    # if / if-else
    if t[0] == 'if':
        if evalExpr(t[1]):
            evalInst(t[2])
        else:
            evalInst(t[3])

    # while
    if t[0] == 'while':
        while evalExpr(t[1]):
            evalInst(t[2])

    # for
    if t[0] == 'for':
        evalInst(t[1])
        while evalExpr(t[2]):
            evalInst(t[4])
            evalInst(t[3])


def evalExpr(t):
    # nombre entier
    if type(t) == int:
        return t
    if type(t) == float:
        return t
    # chaine de caracteres
    if type(t) == str:
        return t

    if type(t) == tuple:

        # variable avec gestion erreur
        if t[0] == 'var':
            return variables[t[1]]

        # acces tableau tab[i]
        if t[0] == 'tab_get':
            return variables[t[1]][evalExpr(t[2])]

        # operations arithmetiques
        if t[0] == '+': return evalExpr(t[1]) + evalExpr(t[2])
        if t[0] == '-': return evalExpr(t[1]) - evalExpr(t[2])
        if t[0] == '*': return evalExpr(t[1]) * evalExpr(t[2])
        if t[0] == '/': return evalExpr(t[1]) / evalExpr(t[2])

        # comparaisons
        if t[0] == '==': return evalExpr(t[1]) == evalExpr(t[2])
        if t[0] == '<':  return evalExpr(t[1]) < evalExpr(t[2])
        if t[0] == '>':  return evalExpr(t[1]) > evalExpr(t[2])
        if t[0] == '<=': return evalExpr(t[1]) <= evalExpr(t[2])
        if t[0] == '>=': return evalExpr(t[1]) >= evalExpr(t[2])

        # appel de fonction avec scope et return
        if t[0] == 'call':
            nom_fonction = t[1]
            if nom_fonction in fonctions:
                params_nom, bloc_fonction = fonctions[nom_fonction]
                arguments_donnes = t[2]
                sauvegarde = variables.copy()  
                for i in range(len(params_nom)):
                    variables[params_nom[i]] = evalExpr(arguments_donnes[i])
                return_value = 0
                try:
                    evalInst(bloc_fonction)
                except ReturnException as r:  
                    return_value = r.value
                variables.clear()
                variables.update(sauvegarde)   
                return return_value

# affectation + print
s1 = 'x=4; x=x+3; print(x);'

# x++ et x+=
s2 = 'x=10; x++; print(x); x+=9; print(x);'

# while + for
s3 = 'x=4; while(x<30){ x=x+3; print(x); }; for(i=0; i<4; i=i+1){ print(i*i); };'

# fonctionVoid sans return
s4 = 'def carre(n){ res=n*n; print(res); } carre(5); carre(10);'

# fonctionValue avec return explicite
s5 = 'def add(a, b){ return a+b; } x=add(3,5); print(x);'

# return coupe-circuit 
s6 = 'def toto(a, b){ c=a+b; return c; print(666); } x=toto(3, 5); print(x);'

# scope + recursivite 
s7 = 'def toto(a, b){ if(a==0){ return b; } c=toto(a-1, b-1); return c; } x=toto(3, 5); print(x);'

# chaines + print multiple
s8 = 'nom="Merai"; print(nom, "a", 42, "ans");'

# tableaux
s9 = 'tab=[10, 20, 30]; print(tab[0]); tab[1]=99; print(tab[1]);'

parser = yacc.yacc()

print("------------------------------------------------------------------------------------------------------")
print("test 1 : affectation + print : ", s1)
ast = parser.parse(s1)
printTreeGraph(ast)
print(ast)
evalInst(ast)

variables = {}
fonctions = {}
print("------------------------------------------------------------------------------------------------------")
print("test 2 : (x++ et x+=) : ", s2)
ast = parser.parse(s2)
printTreeGraph(ast)
print(ast)
evalInst(ast)

variables = {}
fonctions = {}
print("------------------------------------------------------------------------------------------------------")
print("test 3 : while + for : ", s3)
ast = parser.parse(s3)
printTreeGraph(ast)
print(ast)
evalInst(ast)

variables = {}
fonctions = {}
print("------------------------------------------------------------------------------------------------------")
print("test 4 : fonctionVoid : ", s4)
ast = parser.parse(s4)
printTreeGraph(ast)
print(ast)
evalInst(ast)

variables = {}
fonctions = {}
print("------------------------------------------------------------------------------------------------------")
print("test 5 : fonctionValue return : ", s5)
ast = parser.parse(s5)
printTreeGraph(ast)
print(ast)
evalInst(ast)

variables = {}
fonctions = {}
print("------------------------------------------------------------------------------------------------------")
print("test 6 : return coupe-circuit : ", s6)
ast = parser.parse(s6)
printTreeGraph(ast)
print(ast)
evalInst(ast)

variables = {}
fonctions = {}
print("------------------------------------------------------------------------------------------------------")
print("test 7 : scope + recursivite : ", s7)
ast = parser.parse(s7)
printTreeGraph(ast)
print(ast)
evalInst(ast)

variables = {}
fonctions = {}
print("------------------------------------------------------------------------------------------------------")
print("test 8 : chaines + print multiple : ", s8)
ast = parser.parse(s8)
printTreeGraph(ast)
print(ast)
evalInst(ast)

variables = {}
fonctions = {}
print("------------------------------------------------------------------------------------------------------")
print("test 9 : tableaux : ", s9)
ast = parser.parse(s9)
printTreeGraph(ast)
print(ast)
evalInst(ast)

