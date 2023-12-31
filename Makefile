DEPEND += Tokens.hs Grammar.hs myinterpreter.hs

all: $(DEPEND) Grammar.hs Tokens.hs myinterpreter

myinterpreter: $(DEPEND) myinterpreter.hs
	ghc -o myinterpreter myinterpreter.hs

Grammar.hs : Grammar.y
	@rm -f Grammar.hs
	happy Grammar.y
	@chmod -w Grammar.hs

Tokens.hs : Tokens.x
	@rm -f Tokens.hs
	alex Tokens.x
	@chmod -w Tokens.hs

clean::
	rm -rf Tokens.hs Grammar.hs *.hi *.o *.info