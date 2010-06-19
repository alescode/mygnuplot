mygnuplot: Main.hs
	ghc --make Main.hs -o mygnuplot

#Lexer.hs: Lexer.x
#	alex -g Lexer.x
#
#Parser.hs: Parser.y Lexer.hs
#	happy -a -c -g -s Parser.y
#
#
clean:
	-rm -f *.o *.hi mygnuplot
