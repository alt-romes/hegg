view: egraph.gv
	dot -Tpng egraph.gv > egraph.png

egraph.gv:
	cabal run vizdot

.PHONY=clean
clean:
	rm egraph.gv
