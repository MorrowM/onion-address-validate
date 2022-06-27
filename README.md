# Onion Address Validator

A simple tool to test if an onion address has a valid checksum.

> **Warning**
> This is a toy project written in a couple of hours. Do not use this for any serious needs!

## Installation

Install GHC 9.2.3 and the latest version of Cabal using the GHCup toolchain installer:

<https://www.haskell.org/ghcup/>

Clone this repository and build it with Cabal:

```sh
git clone https://github.com/MorrowM/onion-address-validate.git
```

```sh
cabal install -w ghc-9.2.3 exe:onion-address-validate
```

## Usage

`onion-address-validate` reads an onion address from the standard input and will return an exit code of 0 if the address is valid and a nonzero exit code otherwise. For example:

```sh
$ echo "pg6mmjiyjmcrsslvykfwnntlaru7p5svn6y2ymmju6nubxndf4pscryd.onion" | onion-address-validate
$ echo $?                
0
$ echo "qg6mmjiyjmcrsslvykfwnntlaru7p5svn6y2ymmju6nubxndf4pscryd.onion" | onion-address-validate
error: checksum mismatch
$ echo $?
1
```
