egraph.png: egraph.gv
	dot -Tpng egraph.gv > egraph.png

egraph.gv:
	cabal run vizdot

t: egraph-test.png

egraph-test.png: egraph-test.gv
	dot -Tpng egraph-test.gv > egraph-test.png

.PHONY=egraph-test.gv
egraph-test.gv:
	cabal run hegg-test

.PHONY=clean viewtest
clean:
	rm -f egraph.gv egraph.png egraph-test.gv egraph-test.png
