
site: _site

Main: Main.hs
	ghc --make Main -optl -w

_site: Main css/*.css js/*.js posts/*
	./Main rebuild

sync: _site
	s3cmd -P sync _site/ s3://dave.fayr.am/

clean: 
	find . -name '*~' | xargs rm
	find . -name '#*#' | xargs rm
	find . -name '*.o' | xargs rm
	rm Main.hi
