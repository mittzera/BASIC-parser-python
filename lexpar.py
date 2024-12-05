    DIGITS = '0123456789'
    TT_INT = 'INT'
    TT_FLOAT = 'FLOAT'
    TT_IDENTIFIER = 'IDENTIFIER'
    TT_KEYWORD = 'KEYWORD'
    TT_ASSIGN = 'ASSIGN'
    TT_PLUS = 'PLUS'
    TT_MINUS = 'MINUS'
    TT_MUL = 'MUL'
    TT_DIV = 'DIV'
    TT_LPAREN = 'LPAREN'
    TT_RPAREN = 'RPAREN'
    TT_SEMICOLON = 'SEMICOLON'
    TT_EOF = 'EOF'

    KEYWORDS = ['var']

    # Classes de Erros
    class Error:
        def __init__(self, pos_start, pos_end, error_name, details):
            self.pos_start = pos_start
            self.pos_end = pos_end
            self.error_name = error_name
            self.details = details

        def as_string(self):
            result = f'{self.error_name}: {self.details}\n'
            result += f"File {self.pos_start.fn}, line {self.pos_start.ln + 1}, column {self.pos_start.col + 1}"
            return result

    class IllegalCharError(Error):
        def __init__(self, pos_start, pos_end, details):
            super().__init__(pos_start, pos_end, 'Illegal Character', details)

    class InvalidSyntaxError(Error):
        def __init__(self, pos_start, pos_end, details):
            super().__init__(pos_start, pos_end, 'Invalid Syntax', details)

    # Classe de posição para rastreamento de tokens
    class Position:
        def __init__(self, idx, ln, col, fn, ftxt):
            self.idx = idx
            self.ln = ln
            self.col = col
            self.fn = fn
            self.ftxt = ftxt

        def advance(self, current_char=None):
            self.idx += 1
            self.col += 1
            if current_char == '\n':
                self.ln += 1
                self.col = 0
            return self

        def copy(self):
            return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)

    # Definição de Token
    class Token:
        def __init__(self, type_, value=None, pos_start=None, pos_end=None):
            self.type = type_
            self.value = value
            if pos_start:
                self.pos_start = pos_start.copy()
                self.pos_end = pos_start.copy()
                self.pos_end.advance()
            if pos_end:
                self.pos_end = pos_end

        def __repr__(self):
            return f'{self.type}:{self.value}' if self.value else f'{self.type}'

    # Implementação do Lexer
    class Lexer:
        def __init__(self, fn, text):
            self.fn = fn
            self.text = text
            self.pos = Position(-1, 0, -1, fn, text)
            self.current_char = None
            self.advance()

        def advance(self):
            self.pos.advance(self.current_char)
            self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None

        def make_tokens(self):
            tokens = []
            while self.current_char is not None:
                if self.current_char in ' \t':  # Ignorar espaços em branco e tabulações
                    self.advance()
                elif self.current_char == '\n':  # Ignorar quebras de linha
                    self.advance()
                elif self.current_char in DIGITS:
                    tokens.append(self.make_number())
                elif self.current_char.isalpha():
                    tokens.append(self.make_identifier())
                elif self.current_char == '=':
                    tokens.append(Token(TT_ASSIGN, pos_start=self.pos))
                    self.advance()
                elif self.current_char == '+':
                    tokens.append(Token(TT_PLUS, pos_start=self.pos))
                    self.advance()
                elif self.current_char == '-':
                    tokens.append(Token(TT_MINUS, pos_start=self.pos))
                    self.advance()
                elif self.current_char == '*':
                    tokens.append(Token(TT_MUL, pos_start=self.pos))
                    self.advance()
                elif self.current_char == '/':
                    tokens.append(Token(TT_DIV, pos_start=self.pos))
                    self.advance()
                elif self.current_char == '(':
                    tokens.append(Token(TT_LPAREN, pos_start=self.pos))
                    self.advance()
                elif self.current_char == ')':
                    tokens.append(Token(TT_RPAREN, pos_start=self.pos))
                    self.advance()
                elif self.current_char == ';':
                    tokens.append(Token(TT_SEMICOLON, pos_start=self.pos))
                    self.advance()
                else:
                    pos_start = self.pos.copy()
                    char = self.current_char
                    self.advance()
                    return [], IllegalCharError(pos_start, self.pos, f"'{char}'")
            tokens.append(Token(TT_EOF, pos_start=self.pos))
            return tokens, None

        def make_number(self):
            num_str = ''
            dot_count = 0
            pos_start = self.pos.copy()

            while self.current_char is not None and self.current_char in DIGITS + '.':
                if self.current_char == '.':
                    if dot_count == 1: break
                    dot_count += 1
                num_str += self.current_char
                self.advance()

            if dot_count == 0:
                return Token(TT_INT, int(num_str), pos_start, self.pos)
            else:
                return Token(TT_FLOAT, float(num_str), pos_start, self.pos)

        def make_identifier(self):
            id_str = ''
            pos_start = self.pos.copy()

            while self.current_char is not None and self.current_char.isalnum():
                id_str += self.current_char
                self.advance()

            tok_type = TT_KEYWORD if id_str in KEYWORDS else TT_IDENTIFIER
            return Token(tok_type, id_str, pos_start, self.pos)

    # Definição de Nós da AST
    class NumberNode:
        def __init__(self, tok):
            self.tok = tok

        def __repr__(self):
            return f'{self.tok}'

    class VarAssignNode:
        def __init__(self, var_name_tok, value_node):
            self.var_name_tok = var_name_tok
            self.value_node = value_node

        def __repr__(self):
            return f'({self.var_name_tok} = {self.value_node})'

    class BinOpNode:
        def __init__(self, left_node, op_tok, right_node):
            self.left_node = left_node
            self.op_tok = op_tok
            self.right_node = right_node

        def __repr__(self):
            return f'({self.left_node} {self.op_tok} {self.right_node})'

    # Classe de Resultados do Parser
    class ParseResult:
        def __init__(self):
            self.error = None
            self.node = None

        def register(self, res):
            if isinstance(res, ParseResult) and res.error:
                self.error = res.error
            return res

        def success(self, node):
            self.node = node
            return self

        def failure(self, error):
            self.error = error
            return self

    # Parser Principal
    class Parser:
        def __init__(self, tokens):
            self.tokens = tokens
            self.tok_idx = -1
            self.current_tok = None
            self.advance()

        def advance(self):
            self.tok_idx += 1
            self.current_tok = self.tokens[self.tok_idx] if self.tok_idx < len(self.tokens) else None
            return self.current_tok

        def parse(self):
            res = self.statements()
            if res.error:
                return res
            if self.current_tok is not None and self.current_tok.type != TT_EOF:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected '+', '-', '*' or '/'"
                ))
            return res

        def statements(self):
            res = ParseResult()
            statements = []

            while self.current_tok is not None:
                if self.current_tok.type == TT_KEYWORD and self.current_tok.value == 'var':
                    statement = self.var_assign()
                    if statement.error:
                        return res.failure(statement.error)
                    statements.append(statement.node)
                elif self.current_tok.type in (TT_IDENTIFIER, TT_INT, TT_FLOAT):
                    expression = self.expr()
                    if expression.error:
                        return res.failure(expression.error)
                    statements.append(expression.node)
                else:
                    break
                self.advance()

            if not statements:
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected statement"))
            return res.success(statements)

        def var_assign(self):
            res = ParseResult()

            if self.current_tok.type != TT_KEYWORD or self.current_tok.value != 'var':
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected 'var'"
                ))

            self.advance()

            if self.current_tok.type != TT_IDENTIFIER:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected identifier"
                ))

            var_name_tok = self.current_tok
            self.advance()

            if self.current_tok.type != TT_ASSIGN:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected '='"
                ))

            self.advance()

            value_res = self.expr()
            if value_res.error:
                return res.failure(value_res.error)

            return res.success(VarAssignNode(var_name_tok, value_res.node))

        def expr(self):
            return self.bin_op(self.term, [TT_PLUS, TT_MINUS])

        def term(self):
            return self.bin_op(self.factor, [TT_MUL, TT_DIV])

        def bin_op(self, func, ops):
            res = func()
            if res.error:
                return res

            while self.current_tok is not None and any(self.current_tok.type == op for op in ops):
                op_tok = self.current_tok
                self.advance()
                right_res = func()
                if right_res.error:
                    return right_res
                res.node = BinOpNode(res.node, op_tok, right_res.node)
            return res

        def factor(self):
            tok = self.current_tok
            if tok.type in (TT_PLUS, TT_MINUS):
                self.advance()
                factor_res = self.factor()
                if factor_res.error:
                    return factor_res
                return ParseResult().success(UnaryOpNode(tok, factor_res.node))
            elif tok.type == TT_INT or tok.type == TT_FLOAT:
                self.advance()
                return ParseResult().success(NumberNode(tok))
            elif tok.type == TT_LPAREN:
                self.advance()
                expr_res = self.expr()
                if expr_res.error:
                    return expr_res
                if self.current_tok.type == TT_RPAREN:
                    self.advance()
                    return expr_res
                return res.failure(InvalidSyntaxError(
                    tok.pos_start, tok.pos_end, "Expected ')'"
                ))
            return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected number"))

    # Função de execução
    # Função de execução
    def run(filename, text):
        lexer = Lexer(filename, text)
        tokens, error = lexer.make_tokens()
        if error:
            return None, error

        parser = Parser(tokens)
        result = parser.parse()
        return result.node, result.error

    # Exemplo de Implementação com entrada do usuário
    # Exemplo de Implementação com loop contínuo
    if __name__ == '__main__':
        while True:
            # Solicitar input do usuário
            text = input("Digite seu código (ou 'sair' para sair): ")

            # Condição de saída do loop
            if text.lower() == 'sair':
                print("Saindo...")
                break
            
            # Definir o nome do arquivo (se necessário)
            filename = 'example.txt'

            # Executar o código inserido pelo usuário
            result, error = run(filename, text)

            # Verificar se ocorreu erro
            if error:
                print(f"Erro: {error.as_string()}")
            else:
                print(f"Resultado: {result}")
