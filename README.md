# Photon

Photon is a command-line client utility, written in Haskell, similar to curl that also integrates API-Auth authentication for RESTable API endpoints.

## Installation

### macOS Sierra

Via good old [Homebrew](https://brew.sh):

```shell
brew tap mattmoore/brew https://github.com/mattmoore/homebrew-formulas
brew install photon
```

### Arch Linux

A package is available in the AUR. It can be installed with Yaourt:

```shell
yaourt -S photon
```

Or, you can install it the old-fashioned way without Yaourt:

```shell
curl -LO https://aur.archlinux.org/cgit/aur.git/snapshot/photon.tar.gz
tar xf photon.tar.gz
cd photon
makepkg
sudo pacman -U photon-0.1.10-1-x86_64.pkg.tar.xz
```

### Ubuntu/Debian

To install the standard Debian package locally:

```shell
# Install
curl -LO https://github.com/mattmoore/photon/releases/download/0.1.10/photon_0.1.10-1_amd64.deb
sudo dpkg -i photon_0.1.10-1_amd64.deb

# Uninstall
sudo dpkg -r photon
```

Eventually, I plan to investigate the PPA issues and use that instead.

### Fedora/openSUSE

```shell
# Download the same RPM for Fedora or openSUSE
curl -LO https://github.com/mattmoore/photon/releases/download/0.1.10/photon-0.1.10-1.fc26.x86_64.rpm

## Fedora
# Install
sudo dnf install -y photon-0.1.10-1.fc26.x86_64.rpm
# Uninstall
sudo dnf remove -y photon

## openSUSE
# Install
sudo zypper in ./photon-0.1.10-1.fc26.x86_64.rpm
# Uninstall
sudo zypper remove photon
```

## Usage

```shell
photon -X GET --client clientid --key thekey -H "x-custom-header-1: 1" -H "x-custom-header-2: blah" http://somesite.com
```

If you don't specify the protocol ("http://"), photon will assume "http".

Likewise, if you don't specify the verb ("-X GET"), photon will assume you mean "GET".

```shell
photon somesite.com
```
