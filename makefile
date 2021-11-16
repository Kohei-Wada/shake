target=Main

$(target): Main.hs
	stack ghc $<

Main.hs : Game.hs
	stack ghc $<

Game.hs : Snake.hs
	stack ghc $<


clean :
	rm -rf *o
	rm -rf *hi
	rm -rf $(target) 
