HERRAMIENTAS = Lexer.hs Parser.hs
FUENTES = Main.hs AS.hs GeneracionCodigo.hs TablaSimbolos.hs

# El flag -w se debe a que Happy no es totalmente consistente
# con algunas versiones recientes de GHC,
# y puede producir codigo que arroja advertencias.
# Esto no afecta la estabilidad del analizador sintactico.
#
all: mygnuplot

mygnuplot: $(FUENTES) $(HERRAMIENTAS)
	ghc --make -w Main.hs -o mygnuplot

Lexer.hs: Lexer.x
	alex -g Lexer.x

Parser.hs: Parser.y
	happy -acgsi Parser.y

clean:
	rm -f *.o Lexer.hs Parser.hs *.hi mygnuplot
