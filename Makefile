watch:
	stack build --file-watch --fast

exec:
	stack exec TinyScheme-exe

exec-scm:
	stack exec TinyScheme-exe lisp/test.scm
