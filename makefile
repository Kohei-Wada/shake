target=Main


all : $(target)

$(target): Main.hs
	stack ghc $<

Main.hs : Game.hs
	stack ghc $<

Game.hs : Snake.hs Food.hs Types.hs Options.hs

Food.hs : Utils.hs Types.hs Options.hs

Snake.hs : Types.hs Utils.hs

Types.hs : 
	stack ghc $@

Utils.hs : 
	stack ghc $@

Options.hs :
	stack ghc $@

clean :
	rm -rf *o
	rm -rf *hi
	rm -rf $(target) 


test : all
	./$(target)


