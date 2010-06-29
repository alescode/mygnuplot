HERRAMIENTAS = Lexer.x Parser.y
FUENTES = Main.hs AS.hs

all: mygnuplot

mygnuplot: $(FUENTES) $(HERRAMIENTAS)
	alex -g Lexer.x
	happy -acgsi Parser.y
	ghc --make -w Main.hs -o mygnuplot

clean:
	rm -f *.o Lexer.hs Parser.hs *.hi mygnuplot
