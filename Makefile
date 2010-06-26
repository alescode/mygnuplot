all: Parser.hs

mygnuplot: Main.hs
	ghc --make Main.hs -o mygnuplot

Parser.hs: Parser.y Lexer.hs
	happy -a -c -g -s Parser.y; ghc --make Parser.hs -o Parser

Lexer.hs: Lexer.x
	alex -g Lexer.x

#Parser.hs: Parser.y Lexer.hs
#	happy -a -c -g -s Parser.y
#
#
clean:
	rm -f *.o Lexer.hs Parser.hs *.hi mygnuplot
