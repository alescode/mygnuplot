all: GNULex.hs

mygnuplot: Main.hs
	ghc --make Main.hs -o mygnuplot

GNULex.hs: GNULex.x
	alex -g GNULex.x; ghc --make GNULex.hs -o GNULex
#
#Parser.hs: Parser.y Lexer.hs
#	happy -a -c -g -s Parser.y
#
#
clean:
	-rm -f *.o GNULex.hs *.hi mygnuplot
