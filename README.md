# TinyScheme
Scheme interpreter written in haskell. Contains:
- first class lambdas
- define and set vars
- let syntax
- many common lisp functions (examples in lisp folder)
- repl
- **call/cc support**
 
#### TODO
- macro rules
- vectors
- ...

### Installation Steps
    curl -sSL https://get.haskellstack.org/ | sh

    git clone https://github.com/MarusDod/TinyScheme.git

    cd TinyScheme

    stack build

    stack install

    TinyScheme
    TinyScheme ...FILE