<div align="center">

# Navi

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/navi?include_prereleases&sort=semver)](https://github.com/tbidne/navi/releases/)
[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/navi/ci.yaml?branch=main)](https://github.com/tbidne/navi/actions/workflows/ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/navi?color=blue)](https://opensource.org/licenses/MIT)

![linux](https://img.shields.io/static/v1?label=&message=linux&logo=linux&logoColor=white&labelColor=2f353e&color=blue)
![apple](https://img.shields.io/static/v1?label=&message=osx&logo=apple&labelColor=2f353e&color=blue)

</div>

### Table of Contents
- [Introduction](#introduction)
- [Motivation](#motivation)
- [Requirements](#requirements)
- [Configuration](#configuration)
- [Installation](#installation)
- [Building](#building)
  - [Cabal](#cabal)
  - [Nix](#nix)

# Introduction

Navi is an application for defining desktop notifications in terms of a **running notification system**. That is, Navi does not implement desktop notifications from the ground up. Rather, given a running compatible notifications system, Navi provides a simple interface for defining notifications that hook into the running system.

There are built-in services for sending specific notifications, along with functionality to send custom notifications.

# Motivation

Navi is useful for when we have a running notification server and want to define custom notification events. For example, we may want a notification for cpu temperature. We can define a "service" that includes:

* The command to run.
* The command output that should trigger a notification.
* The notification to send.

```toml
# requires lm-sensors
[[single]]
command = """
  temp_res=$(sensors | grep "Core 0")
  regex="Core 0:\\s*\\+([0-9]+)\\.[0-9]{0,2}°[C|F].*"

  if [[ $temp_res =~ $regex ]]; then
    temp="${BASH_REMATCH[1]}"
    # not actually that hot...
    if [[ $temp -gt 20 ]]; then
      echo "true"
    else
      echo "false"
    fi
  else
    echo "couldn't parse: ${temp_res}"
    exit 1
  fi
"""
trigger = "true"

[single.note]
summary = "Temperature"
body = "We're hot!"
urgency = "critical"
timeout = 10
```

This allows us to define arbitrary notification services based on the current system. In other words, as long as we can query for a particular bit of information (e.g. bash code), then navi will take care of the rest: running this query every N seconds, sending notifications, caching previous values to avoid repeats, and error handling.

# Requirements

Navi currently supports:

* DBus

  If there is a DBus-compatible notification server running, navi can hook in directly. This has been tested with:

    * [deadd-notification-center](https://github.com/phuhl/linux_notification_center)
    * [dunst](https://dunst-project.org/)
    * [GNOME](https://www.gnome.org/)
    * [KDE Plasma](https://kde.org/plasma-desktop/)

* Libnotify

  Navi can also use the `notify-send` tool directly. This is largely redundant since `notify-send` itself requires a running DBus notification server, but this option is provided as an alternative.

* AppleScript

  MacOS can use AppleScript.

# Configuration

Navi is configured via a toml file, by default located at `<xdg-config>/navi/config.toml`. Full examples can be found in [examples](./examples).

# Installation

The [releases](https://github.com/tbidne/navi/releases) page has binaries built for several platforms. If there are no binaries for your platform, it is possible to [build navi](#building) yourself.

# Building

If you have never built a haskell program before, [Cabal](#cabal) is probably the best choice.

## Cabal

### Prerequisites

* [`cabal 3.8+`](https://www.haskell.org/cabal/download.html)
* [`ghc 9.6 - 9.12`](https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC%20Status)

The easiest way to install these is generally [`ghcup`](https://www.haskell.org/ghcup/).

The current "blessed" version is `ghc-9.10.2`.

### Build Navi

Once you have `cabal` and `ghc`, `navi` can be built locally with `cabal build` or installed globally (e.g. `~/.local/bin/navi`) with `cabal install`.

> [!IMPORTANT]
>
> Navi requires git information to be available at build time, for the purposes of including some data in the binary (e.g. commit hash). Cabal's vanilla install method interfers with this, though we have a workaround that relies on passing the current directory as an environment variable:
>
> ```sh
> $ export NAVI_HOME=$(pwd); cabal install exe:navi
> ```
>
> Nix does not require such a workaround.

For further reproducibility, an optional freeze files can be used for the "blessed" compiler.

```sh
cabal build --project-file cabal.ghc<XYZ>.project
```

## Nix

### Prerequisites

* [nix](https://nixos.org/download.html)

### Manually

Building with `nix` uses [flakes](https://nixos.wiki/wiki/Flakes). `navi` can be built with `nix build`, which will compile and run the tests.

### Nix expression

Because Navi is a flake, it can be built as part of a nix expression. For instance, if you want to add Navi to `NixOS`, your `flake.nix` might look something like:

```nix
# flake.nix
{
  inputs.navi.url = "github:tbidne/navi/main";
}
```

Then include this in the `systemPackages`:

```nix
# wherever your global packages are defined
{
  environment.systemPackages = [
    navi.packages."${system}".default
  ];
}
```
