HERRAMIENTAS = Lexer.hs Parser.hs
FUENTES = Main.hs AS.hs

all: mygnuplot

mygnuplot: $(FUENTES) $(HERRAMIENTAS)
	ghc --make -w Main.hs -o mygnuplot

Lexer.hs: Lexer.x
	alex -g Lexer.x

Parser.hs: Parser.y
	happy -acgsi Parser.y

clean:
	rm -f *.o Lexer.hs Parser.hs *.hi mygnuplot
