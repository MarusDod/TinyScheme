watch:
	stack build --file-watch --fast

exec:
	stack exec TinyScheme-exe

exec-scm:
	stack exec TinyScheme-exe lisp/test.scm

exec-define:
	stack exec TinyScheme-exe lisp/define.scm

exec-factorial:
	stack exec TinyScheme-exe lisp/factorial.scm

ramdisk:
	mkdir /dev/shm/.stack-work
	ln -s /dev/shm/.stack-work .stack-work
