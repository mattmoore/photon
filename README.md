# Photon

Photon is a command-line client utility, written in Haskell, similar to curl that also integrates API-Auth authentication for RESTable API endpoints.

## Installation

Via good old [Homebrew](https://brew.sh):

```shell
brew tap mattmoore/brew https://github.com/mattmoore/homebrew-formulas
brew install photon
```

```shell
photon -X GET --client clientid --key thekey -H "x-custom-header-1: 1" -H "x-custom-header-2: blah" http://somesite.com
```

If you don't specify the protocol ("http://"), photon will assume "http".

Likewise, if you don't specify the verb ("-X GET"), photon will assume you mean "GET".

```shell
photon somesite.com
```
