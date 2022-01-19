# TinyScheme
Scheme interpreter written in haskell.
- includes many common scheme functions
- first class lambdas
- can define and set vars
- can pretty much define the rest of the standard library without changing source code
- **call/cc support**
 
#### TODO
- let syntax
- macro rules
- vectors
- ...

### Installation Steps
    curl -sSL https://get.haskellstack.org/ | sh

    git clone https://github.com/MarusDod/TinyScheme.git

    cd TinyScheme

    stack build

    stack install

    TinyScheme-exe ...ARGS