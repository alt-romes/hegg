egraph.png: egraph.gv
	dot -Tpng egraph.gv > egraph.png

egraph.gv:
	cabal run vizdot

.PHONY=demo
demo demo.png: demo.gv
	dot -Tpng $< > demo.png


.PHONY=clean viewtest
clean:
	rm -f egraph.gv egraph.png egraph-test.gv egraph-test.png
