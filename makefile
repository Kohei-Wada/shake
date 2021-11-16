target=Main

$(target): Main.hs
	stack ghc $<

test: $(target)
	./$(target)


clean :
	rm -rf *o
	rm -rf *hi
	rm -rf $(target) 
