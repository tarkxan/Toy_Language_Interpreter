
""" Oleg Lando - Toy Language Interpreter """

#############################################
#                                           #
#               Lexer                       #
#                                           #
#############################################

import re
import sys

# Token types
# EOF (end-of-file) token is used to indicate that
# there is no more input left for lexical analysis
(INTEGER, PLUS, MINUS, MULTIPLICATION, OPENPARENTHESIS,
 CLOSEPARENTHESIS, ID, ASSIGN, SEMICOLON, EOF) = (
    'INTEGER', 'PLUS', 'MINUS', 'MULTIPLICATION', 'OPENPARENTHESIS',
    'CLOSEPARENTHESIS', 'ID', 'ASSIGN', 'SEMICOLON', 'EOF'
)

# Exceptions
class LexerSyntError(Exception):

    def __init__(self,
                 errMessage):
        
        self.errMessage = errMessage

    def __str__(self):
        return repr(self.errMessage)

class ParserSyntError(Exception):

    def __init__(self,
                 errMessage):
        
        self.errMessage = errMessage

    def __str__(self):
        return repr(self.errMessage)

class VariableNameError(Exception):

    def __init__(self,
                 errMessage):
        
        self.errMessage = errMessage

    def __str__(self):
        return repr(self.errMessage)

    

#-----------------------
#       Token
#-----------------------
class Token(object):

    def __init__(self,
                 type,
                 value = None,
                 line = None,
                 column = None):
        
        # token attributes
        self.type = type
        self.value = value
        self.line = line
        self.column = column

    def __str__(self):
        return 'Token({type},{value})'.format(type = self.type, value = repr(self.value))

    def __repr__(self):
        return self.__str__()

#-----------------------
#       Lexer
#-----------------------
class Lexer(object):

    def __init__(self):
        
        self.tokenLst = []
        self.currentPos = -1

        self.token_specification = [
            ('INTEGER', r'\b\d\b|[1-9]+([0-9])*'),    # Integer or decimal number
            ('ASSIGN',  r'='),                        # Assignment operator
            ('SEMICOLON', r'[;]'),                    # Statement terminator
            ('ID',      r'[A-Za-z]([0-9A-Za-z_])*'),  # Identifier
            ('PLUS',    r'[+]'),                      # Addition
            ('MINUS',   r'[\-]'),                     # Substraction
            ('MULTIPLICATION',    r'[*]'),            # Multiplication
            ('OPENPARENTHESIS',  r'[(]'),             # Open Parenthesis
            ('CLOSEPARENTHESIS',  r'[)]'),            # Close Parenthesis   
            ('NEWLINE', r'\n'),                       # Line endings
            ('SKIP',    r'[ \t]'),                    # Skip over spaces and tabs
        ]

        self.tok_regex = '|'.join('(?P<%s>%s)' % pair for pair in self.token_specification)
        self.get_token = re.compile(self.tok_regex).match

    # tokenizes an input string to lexemes
    def scan(self,
             inputProg):

        self.currentPos = -1
        self.tokenLst = []

        pos = 0
        line = 1
        line_start = 0

        mo = self.get_token(inputProg)

        while mo is not None:

            typ = mo.lastgroup

            if typ == 'NEWLINE':

                line_start = pos
                line += 1

            elif typ != 'SKIP':

                val = mo.group(typ)
                self.tokenLst.append(Token(typ, val, line, mo.start() - line_start))

            pos = mo.end()
            mo = self.get_token(inputProg,
                                pos)

        if pos != len(inputProg):
            raise LexerSyntError('Syntax Error found by Lexer.scan()')

        if len(self.tokenLst) > 0:
            self.currentPos = 0

    # return next token until the end of input reached
    def getNextToken(self):

        while self.currentPos >= 0 and self.currentPos < len(self.tokenLst):

            token = self.tokenLst[self.currentPos]
            self.currentPos += 1

            return token

        return Token(EOF)
    

#############################################
#                                           #
#               Parser                      #
#                                           #
#############################################

#-- AST = Abstract Syntax Tree Class
class AST(object):
    pass

#--  BinOp = Binary Operator Class, ex: x + y, a + 5
class BinOp(AST):

    def __init__(self,
                 left,
                 op,
                 right):

        self.left = left           # variable
        self.token = self.op = op  # operator
        self.right = right         # expression

#-- Num
class Num(AST):

    def __init__(self,
                 token):

        self.token = token
        self.value = token.value

#-- UnaryOp = Unary Operator applied to the following operand, ex: -x, -5
class UnaryOp(AST):

    def __init__(self,
                 op,
                 expr):

        self.token = self.op = op
        self.expr = expr

#-- Compound = list of statements separated by semicolon
class Compound(AST):
    
    def __init__(self):
        self.children = []

#-- Assign
class Assign(AST):
    def __init__(self,
                 left,
                 op,
                 right):
        
        self.left = left
        self.token = self.op = op
        self.right = right

#-- Var - the Var node is constructed out of ID token
class Var(AST):
    
    def __init__(self,
                 token):

        self.token = token
        self.value = token.value

#-- NoOp
class NoOp(AST):
    pass

#-----------------------
#       Parser
#-----------------------
class Parser(object):

    def __init__(self,
                 lexer):

        self.lexer = lexer

        # set current token to the first token taken from the input
        self.current_token = self.lexer.getNextToken()

    def setToken(self):
        
        self.current_token = self.lexer.getNextToken()

    ''' Compare the current token type with the passed token type and if they
        match then "eat" the current token and assign the next token to
        the self.current_token
        Otherwise raise an exception '''
    def eat(self,
            token_type):

        if self.current_token.type == token_type:
            self.current_token = self.lexer.getNextToken()

        else:
            raise ParserSyntError('Syntax Error found by Parcer.eat(), token type: ' + token_type)

    # error message
    def error(self,
              token_type = None):
        
        raise ParserSyntError('Syntax Error found')

    #----------------- Grammar Recursive Methods -----------------------

    """    THE GRAMMAR:
        -------------------------------
        Prog      -> Prog Assign |
                     Epsylon
        Assign    -> Id = Exp;
        Exp       -> Term ExpPrime
        ExpPrime  -> +Term ExpPrime |
                     -Term ExpPrime |
                     Epsylon           
        Epsylon   ->
        Term      -> Fact TermPrime
        TermPrime -> * Fact TermPrime |
                     Epsylon
        Fact      -> (Exp) |
                     - Fact |
                     + Fact |
                     Integer |
                     Id
        """

    ''' Prog -> Prog Assign | Epsylon '''
    def program(self):
        
        nodes = self.assignsList()

        root = Compound()

        # add assignment expressions to the list
        for node in nodes:
            root.children.append(node)

        return root

    ''' AssignsList -> Assign ; AssignsList '''
    def assignsList(self):

        node = self.assign()

        # list of assignments
        results = [node] 
        
        while self.current_token.type == ID:
            
            results.append(self.assign())

        if self.current_token.type == ID:
            
            raise ParserSyntError('Syntax Error found by Parcer.assignsList()')

        return results

    ''' Assign -> Id = Exp | Epsylon '''
    def assign(self):

        if self.current_token.type == ID:
            
            left = self.id()
            token = self.current_token
            self.eat(ASSIGN)
            right = self.expr()
            self.eat(SEMICOLON)

            node = Assign(left,
                          token,
                          right)

        else:
            node = self.empty()
            
        return node

    ''' Id -> Letter Alphanum '''
    def id(self):

        node = Var(self.current_token)
        self.eat(ID)

        return node

    ''' Epsylon '''
    def empty(self):

        return NoOp()

    ''' Exp -> Tem ExpPrime '''
    def expr(self):

        node = self.term()
        node = self.exprPrime(node)

        return node

    ''' ExpPrime -> + Term ExpPrime | - Tem ExpPrime'''
    def exprPrime(self,
                  node):

        token = self.current_token
        
        if token.type == PLUS:

            self.eat(PLUS)
            node = BinOp(left = node,
                         op = token,
                         right = self.term())
            
            node = self.exprPrime(node)

        elif token.type == MINUS:

            self.eat(MINUS)
            node = BinOp(left = node,
                         op = token,
                         right = self.term())
            
            node = self.exprPrime(node)
            
        return node
        
    ''' Term -> Fact TermPrime '''
    def term(self):

        node = self.factor()
        node = self.termPrime(node)

        return node

    ''' TermPrime -> * Fact TermPrime '''
    def termPrime(self,
                  node):

        token = self.current_token

        if token.type == MULTIPLICATION:

            self.eat(MULTIPLICATION)
            node = BinOp(left=node,
                         op=token,
                         right=self.term())
            node = self.termPrime(node)

        return node

    ''' Fact -> (Expr) | -Fact | +Fact| Integer|  |Id '''
    def factor(self):

        token = self.current_token

        if token.type == OPENPARENTHESIS:
            
            self.eat(OPENPARENTHESIS)
            node = self.expr()
            self.eat(CLOSEPARENTHESIS)
            return node

        elif token.type == MINUS:

            self.eat(MINUS)
            node = UnaryOp(token,
                           self.factor())
            return node

        elif token.type == PLUS:

            self.eat(PLUS)
            node = UnaryOp(token,
                           self.factor())
            return node

        elif token.type == INTEGER:

            self.eat(INTEGER)
            return Num(token) 

        else:

            node = self.id()
            return node

    # construct a parse tree (Abstract Syntax Tree)
    def parse(self):

        node = self.program()

        if self.current_token.type != EOF:
            raise ParserSyntError('Syntax Error found by Parcer.parse()')

        return node


#############################################
#                                           #
#               Interpreter                 #
#                                           #
#############################################

#-----------------------
#       NodeVisitor
#-----------------------
class NodeVisitor(object):

    def visit(self,
              node):
        
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self,
                          method_name,
                          self.generic_visit)

        return visitor(node)

    def generic_visit(self,
                      node):
        
        raise Exception('No visit_{} method'.format(type(node).__name__))

#-----------------------
#       Interpreter
#-----------------------
class Interpreter(NodeVisitor):

    # constructor
    def __init__(self,
                 parser):

        self.parser = parser
        self.outputDict = {}

    # supports +, -, * operations
    def visit_BinOp(self,
                    node):

        if node.op.type == PLUS:
            return self.visit(node.left) + self.visit(node.right)

        elif node.op.type == MINUS:
            return self.visit(node.left) - self.visit(node.right)

        elif node.op.type == MULTIPLICATION:
            return self.visit(node.left) * self.visit(node.right)

    def visit_Num(self,
                  node):

        return int(node.value)

    def visit_UnaryOp(self,
                      node):

        op = node.op.type

        if op == PLUS:
            return +self.visit(node.expr)

        elif op == MINUS:
            return -self.visit(node.expr)

    def visit_Compound(self,
                       node):

        for child in node.children:

            self.visit(child)

    def visit_Assign(self,
                     node):
        
        var_name = node.left.value
        self.outputDict [var_name] = self.visit(node.right)

    def visit_Var(self,
                  node):

        var_name = node.value
        val = self.outputDict.get(var_name)

        if val is None:
            #raise NameError(repr(var_name))
            print ('Error Message: Invalid variable: ' + repr(var_name))
            sys.exit(0)

        else:
            return val

    def visit_NoOp(self, node):
        pass

    def interpret(self,
                  inputProg):

        # call tokenizer
        try:
            self.parser.lexer.scan(inputProg)

        except LexerSyntError as errMessage:

            print(errMessage)
            print ('\nCheck the input:\n' + inputProg)
            sys.exit(0)

        except VariableNameError as errMessage:

            print ('\nUnexpected Error\n')
            sys.exit(0)

        # set current toke to 1-st token
        self.parser.setToken()

        # 
        try:
            tree = self.parser.parse()

        except ParserSyntError as errMessage:

            print(errMessage)
            print ('\nCheck the input:\n' + inputProg)
            sys.exit(0)

        except Exception:

            print ('\nUnexpected Error\n')
            sys.exit(0)

        if tree is None:
            return ''

        return self.visit(tree)

    def printValues(self,
                    inputProg):

        if inputProg is not None:

            print ('Input:\n--------\n' + inputProg)

        if self.outputDict is not None:

            print ('\nOutput:\n--------')

            for key in self.outputDict:
                print (key + ' = ' + str(self.outputDict[key]))
                


#############################################
#                                           #
#               main                        #
#                                           #
#############################################
def main():

    inputProg = '''x = 1;
y = 2;
z = ---(x+y)*(x * y);'''

    #inputProg = input('Input>\n')

    if len(inputProg) == 0:
        print ('Empty Input')

    lexer = Lexer()
    parser = Parser(lexer)
    interpreter = Interpreter(parser)

    # parse
    interpreter.interpret(inputProg)

    # print evaluated values from output dictionary
    interpreter.printValues(inputProg)

if __name__ == '__main__':
    main()
