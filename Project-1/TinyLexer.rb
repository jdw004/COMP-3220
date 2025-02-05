#
#  Class Lexer - Reads a TINY program and emits tokens
#
class Lexer
# Constructor - Is passed a file to scan and outputs a token
#               each time nextToken() is invoked.
#   @c        - A one character lookahead 
		def initialize(filename)
		# Need to modify this code so that the program
		# doesn't abend if it can't open the file but rather
		# displays an informative message
		
		# Go ahead and read in the first character in the source
		# code file (if there is one) so that you can begin
		# lexing the source code file 
		if (File.file?(filename))
            @f = File.open(filename,'r:utf-8')
            @c = @f.getc()  
        else
            puts "File does not exist"
        
        end
 
    end       

	
	# Method nextCh() returns the next character in the file
	def nextCh()
		if (! @f.eof?)
			@c = @f.getc()
		else
			@c = "eof"
		end
		
		return @c
	end

	# Method nextToken() reads characters in the file and returns
	# the next token
	def nextToken() 
		if @c == "eof"
			return Token.new(Token::EOF,"eof")
				
		elsif (whitespace?(@c))
			str =""
		
			while whitespace?(@c)
				str += @c
				nextCh()
			end
		
			tok = Token.new(Token::WS,str)
			puts "Token: #{tok.type}   Lexeme: #{tok.text}"
			File.open("tokens.txt", "a") { |file| file.puts "Token: #{tok.type}   Lexeme: #{tok.text}" }
			return tok

		elsif (letter?(@c))
			str = ""
			while letter?(@c)
				str += @c
				nextCh()
			end
			# Check if the identifier is a reserved keyword
			if (str == "print")
				tok = Token.new(Token::PRINT, str)
			else
				tok = Token.new(Token::ID, str)
			end
			puts "Token: #{tok.type}   Lexeme: #{tok.text}"
			File.open("tokens.txt", "a") { |file| file.puts "Token: #{tok.type}   Lexeme: #{tok.text}" }
			return tok

		elsif (numeric?(@c))
			str = ""
			while numeric?(@c)
				str += @c
				nextCh()
			end
			tok = Token.new(Token::INT, str)
			puts "Token: #{tok.type}   Lexeme: #{tok.text}"
			File.open("tokens.txt", "a") { |file| file.puts "Token: #{tok.type}   Lexeme: #{tok.text}" }
			return tok

		else
			case @c
			when '('
                tok = Token.new(Token::LPAREN, @c)
            when ')'
                tok = Token.new(Token::RPAREN, @c)
            when '+'
                tok = Token.new(Token::PLUS, @c)
            when '-'
                tok = Token.new(Token::MINUS, @c)
            when '*'
                tok = Token.new(Token::MULT, @c)
            when '/'
                tok = Token.new(Token::DIV, @c)
            when '='
                tok = Token.new(Token::EQUAL, @c) 
            when '<'
                tok = Token.new(Token::LT, @c)
            when '>'
                tok = Token.new(Token::GT, @c)
            when '&'
                tok = Token.new(Token::AMPERSAND, @c)
            else
                tok = Token.new(Token::UNKWN,@c)
            end
			nextCh()
			puts "Token: #{tok.type}   Lexeme: #{tok.text}"
			File.open("tokens.txt", "a") { |file| file.puts "Token: #{tok.type}   Lexeme: #{tok.text}" }
			return tok
		end
		
		# don't want to give back nil token!
		# Fallback for any unrecognized token:
		tok = Token.new(Token::UNKWN,@c)
	end
	
end
#
# Helper methods for Scanner
#
def letter?(lookAhead)
	lookAhead =~ /^[a-z]|[A-Z]$/
end

def numeric?(lookAhead)
	lookAhead =~ /^(\d)+$/
end

def whitespace?(lookAhead)
	lookAhead =~ /^(\s)+$/
end
