
site: _site

Main: Main.hs
	ghc --make Main -optl -w

_site: Main css/*.css js/*.js posts/*
	./Main rebuild

clean: 
	find . -name '*~' | xargs rm
	find . -name '#*#' | xargs rm
	find . -name '*.o' | xargs rm
	rm Main.hi