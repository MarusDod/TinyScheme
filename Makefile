watch:
	stack build --file-watch --fast

exec:
	stack exec TinyScheme

exec-scm:
	stack exec -- TinyScheme --file=lisp/test.scm

exec-define:
	stack exec -- TinyScheme --file=lisp/define.scm

exec-factorial:
	stack exec -- TinyScheme --file=lisp/factorial.scm

ramdisk:
	mkdir /dev/shm/.stack-work
	ln -s /dev/shm/.stack-work .stack-work
