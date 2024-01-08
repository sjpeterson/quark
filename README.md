# Quark - the editor the world doesn't need

## Introduction

Quark is an editor written in Haskell that started as a learning project with
the aim to replace nano with something similarly simple and clean, but somewhat
more powerful. It is slightly inspired by [Atom](https://atom.io), hence the
name (in other words quark as in the elementary particle, not the dairy
product), as well as a few other editors and IDEs.

The project has been abandoned.

## Installation

To build and install quark from source:

    git clone https://github.com/sjpet/quark.git
    cd quark
    stack build

After this, you may wish to copy the man page to some suitable location, for example:

    sudo cp doc/quark.1 /usr/local/man/man1
