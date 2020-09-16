# Haskell-3 Project-  Programming in Felix

Implemented a programming language called FELIX. Felix has all the basic features such as operators , data types such
as double , string, bool. It supports loops and functions which makes it a Turing Complete language.

For more details about the language you can see Language description page

![Felix Language Description](https://github.com/IITH-SBJoshi/haskell-3/wiki/Language-Description)

Here are some Sample Programs implemented in Felix

![Felix Sample Programs](https://github.com/IITH-SBJoshi/haskell-3/wiki/Sample-Programs-in-FELIX)


# FELIX
[![TravisCI](https://travis-ci.com/IITH-SBJoshi/haskell-3.svg?token=UGPoQA6bQYmpzzso7vXa&branch=master)]
[![CircleCI](https://circleci.com/gh/IITH-SBJoshi/haskell-3.svg?style=svg&circle-token=6fd50174569492d7a709ca648b86697bf80349e8)](https://circleci.com/gh/IITH-SBJoshi/haskell-3)
![Felix Felix the programmming language](./imgs/felix.png)

## Introduction
* FELIX is a simple toy programming language. A person familiar with javascipt will find it to be a subset of it.
* Our aim is to provide an interpreter for FELIX.
* FELIX supports basic constructs of if-else, definition-declaration, conditional loops, and simple data types of double, string and boolean.
* The interpreter supports input as well as repl inputs.
* For more information, see the github wiki.

## Team Members
1. Atharva Sarage C17BTECH11005
2. Yash Khasbage CS17BTECH11044
3. Rushikesh Tammewar CS17BTECH11041
4. Maulikkumar Ravat CS17BTECH11031

## Documentation
* [FELIX-docs](https://iith-sbjoshi.github.io/haskell-3/ "docs")
* docs/ directory (offline)
* language tutorial is also given in WIKI!!!

## Setup
1. install ghc and cabal(project has only been tested on ubuntu 18.* and 16.* )
```bash
sudo apt install ghc cabal-install
```

2. clone repository
```bash
git clone https://github.com/IITH-SBJoshi/haskell-3.git
```

3. move into haskell-3 and install dependencies
```bash
cabal install parsers
cabal install parsec
cabal install text
cabal install HUnit
cabal configure -v --enable-tests
```
or simply run
```bash
bash install_dependencies.sh
```

4. build app
```bash
cabal build
```

5. run:
* to simply get parse-tree as output
```bash
cabal run --verbose=0 -- -P <filepath>
```
* to actually run code
```bash
cabal run --verbose=0 <filepath>
```
* for those who have to run multiple times, it is better to do
```bash
alias felix='cabal run --verbose=0 '
```
Now, simply do
```bash
felix <filepath>
```
everytime. Note that alias remains only for single bash session.

## Contribution

We expect traditional ways of contribution. Few preferences we choose are:
* always branch from *dev* branch and merge into *dev* branch. This dev branch will then be merged with master depending on build status and other criteria.
* Always create test cases for every new feature.

## Testing

* Minor ways of testing involve writing tests in test/ directory.
This involves testing of small routines.
* The major, difficult and important way of testing is creating felix programs in test/ directory. The parser output and runtime output of test case should be included in testanswers directory.
Parser output and runtime output are to be named as <testname>.trueparse and <testname>.trueout. Similarly, testfile should be named <testname>.felix .
* running test:
```bash
bash runtests.sh
```
* once you have added a new .felix testfile, the testname should be appended in FileNameArray list of runtests.sh

Note:
There are no special error repoting or strong semantic checking routines. So if your code is wrong, there is a probablility that it will run yielding every result as <undefined>

## Project dependencies
* parsers
* parsec
* text
* HUnit(for testing only)

## References:
* [ALSU](https://en.wikipedia.org/wiki/Compilers:_Principles,_Techniques,_and_Tools "Compilers: Principles, Techniques, and Tools")
* [llvm](http://www.stephendiehl.com/llvm/ "Compiler in Haskell")
