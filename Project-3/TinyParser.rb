#
#  Parser Class with Extra Credit Part 1 - correcting operand order
#
load "TinyLexer.rb"
load "TinyToken.rb"
load "AST.rb"
class Parser < Lexer
    def initialize(filename)
        super(filename)
        consume()
    end
    
    def consume()
        @lookahead = nextToken()
        while(@lookahead.type == Token::WS)
            @lookahead = nextToken()
        end
    end
    
    def match(dtype)
        if (@lookahead.type != dtype)
            puts "Expected #{dtype} found #{@lookahead.text}"
            @errors_found+=1
        end
        consume()
    end
    
    def program()
        @errors_found = 0
        
        # Create program node as the root of the AST
        p = AST.new(Token.new("program","program"))
        
        while(@lookahead.type != Token::EOF)
            p.addChild(statement())
        end
        
        puts "There were #{@errors_found} parse errors found."
      
        return p
    end
    
    def statement()
        if (@lookahead.type == Token::PRINT)
            # Create print node
            stmt = AST.new(@lookahead)
            match(Token::PRINT)
            stmt.addChild(exp())
            return stmt
        else
            # Handle assignment
            return assign()
        end
    end
    
    def assign()
        if (@lookahead.type == Token::ID)
            # Create node for the ID
            idtok = AST.new(@lookahead)
            match(Token::ID)
            
            if (@lookahead.type == Token::ASSGN)
                # Create assignment node with the assignment token
                assgn = AST.new(@lookahead)
                # Add the ID as the first child
                assgn.addChild(idtok)
                match(Token::ASSGN)
                # Add the expression as the second child
                assgn.addChild(exp())
                return assgn
            else
                # Error - expected assignment operator
                match(Token::ASSGN)
                # Create an assignment node for error recovery
                assgn = AST.new(Token.new("assignment","assignment"))
                return assgn
            end
        else
            # Error
            match(Token::ID)
            # Create an assignment node for error recovery
            assgn = AST.new(Token.new("assignment","assignment"))
            return assgn
        end
    end
    
    def exp()
        # Start with a term
        t = term()
        
        # Check for any additional terms
        return etail(t)
    end
    
    def term()
        # Start with a factor
        f = factor()
        
        # Check for any additional factors 
        return ttail(f)
    end
    
    def factor()
        if (@lookahead.type == Token::LPAREN)
            match(Token::LPAREN)
            exp_node = exp()
            if (@lookahead.type == Token::RPAREN)
                match(Token::RPAREN)
                return exp_node
            else
                match(Token::RPAREN)
                # Error case
                return AST.new(Token.new("error","error"))
            end
        elsif (@lookahead.type == Token::INT)
            int_node = AST.new(@lookahead)
            match(Token::INT)
            return int_node
        elsif (@lookahead.type == Token::ID)
            id_node = AST.new(@lookahead)
            match(Token::ID)
            return id_node
        else
            puts "Expected ( or INT or ID found #{@lookahead.text}"
            @errors_found+=1
            consume()
            # Error case
            return AST.new(Token.new("error","error"))
        end
    end
    
    def ttail(left)
        if (@lookahead.type == Token::MULTOP)
            # Create a node for the multiplication operation
            mult_node = AST.new(@lookahead)
            match(Token::MULTOP)
            
            mult_node.addAsFirstChild(left)
            
            right = factor()
            mult_node.addChild(right)
            
            return ttail(mult_node)
        elsif (@lookahead.type == Token::DIVOP)
            div_node = AST.new(@lookahead)
            match(Token::DIVOP)
            
            div_node.addAsFirstChild(left)
            
            right = factor()
            div_node.addChild(right)
            
            return ttail(div_node)
        else
            # No more multiplication or division
            return left
        end
    end
    
    def etail(left)
        if (@lookahead.type == Token::ADDOP)
            add_node = AST.new(@lookahead)
            match(Token::ADDOP)
            
            add_node.addAsFirstChild(left)
            
            right = term()
            add_node.addChild(right)
            
            return etail(add_node)
        elsif (@lookahead.type == Token::SUBOP)
            sub_node = AST.new(@lookahead)
            match(Token::SUBOP)
            
            sub_node.addAsFirstChild(left)
            
            right = term()
            sub_node.addChild(right)
            
            return etail(sub_node)
        else
            return left
        end
    end
end
