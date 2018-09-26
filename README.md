# Rebar3 Erlang

[![License: MIT][MIT Badge]][MIT]
[![GitHub Release Badge]][GitHub Releases]

A [Rebar3 Plugin] for creating [Erlang] release for [Escriptized] application.

## Overview

The [Rebar3 Releases] feature prepares a normal Erlang release with all needed
beam files and Erlang runtime included. In case of escriptized application
all the external dependencies are usually baked into the binary and not needed
to be a part of the release itself. This plugin prepares an Erlang release
without those dependencies assuming they are part of the executable.

In addition a handy Makefile is generated for easier installation.

## Usage

Specify the plugin in your "rebar.config" file:

```
{plugins, [
    {r3erlang,
     {git, "https://github.com/aialferov/r3erlang", {branch, "master"}}}
]}.
```

Build you application and run the following command:

```
$ rebar3 erlang
```

Find the Erlang release in a profile directory. By default:

```
$ cd _build/default/erl
```

You can install this release using a [Make] target:

```
$ make install
```

The installation directories used are the following:

```
$(DESTDIR)/$(PREFIX)/lib/erlang
$(DESTDIR)/$(PREFIX)/bin
```

To change the target installation directory use "PREFIX" (default: "usr/local").
To prepare local installation for packaging and further redistribution use
"DESTDIR". In final installations "DESTDIR" should always be empty.

<!-- Links -->
[MIT]: https://opensource.org/licenses/MIT
[GitHub Releases]: https://github.com/aialferov/r3erlang/releases
[Make]: https://www.gnu.org/software/make
[Erlang]: http://erlang.org
[Rebar3 Plugin]: https://www.rebar3.org/docs/using-available-plugins
[Rebar3 Releases]: https://www.rebar3.org/docs/releases
[Escriptized]: https://www.rebar3.org/docs/commands#section-escriptize

<!-- Badges -->
[MIT Badge]: https://img.shields.io/badge/License-MIT-yellow.svg?style=flat-square
[GitHub Release Badge]: https://img.shields.io/github/release/aialferov/r3erlang/all.svg?style=flat-square
