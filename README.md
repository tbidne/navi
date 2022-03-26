<div align="center">

# Navi

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/navi?include_prereleases&sort=semver)](https://github.com/tbidne/navi/releases/)
[![MIT](https://img.shields.io/github/license/tbidne/navi?color=blue)](https://opensource.org/licenses/MIT)

[![nix](https://img.shields.io/github/workflow/status/tbidne/navi/nix/main?label=nix&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/navi/actions/workflows/nix_ci.yaml)
[![stack](https://img.shields.io/github/workflow/status/tbidne/navi/stack/main?label=stack%20lts-19.0&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/navi/actions/workflows/stack_ci.yaml)
[![style](https://img.shields.io/github/workflow/status/tbidne/navi/style/main?label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/navi/actions/workflows/style_ci.yaml)

[![8.10.7](https://img.shields.io/github/workflow/status/tbidne/navi/8.10.7/main?label=8.10.7&logo=haskell&logoColor=904d8c&labelColor=2f353c)](https://github.com/tbidne/navi/actions/workflows/ghc_8-10-7.yaml)

</div>

### Table of Contents
- [Introduction](#introduction)
- [Requirements](#requirements)
- [Command-Line Args](#command-line-args)
  - [Config File](#config-file)
  - [Config Directory](#config-directory)
- [Configuration](#configuration)
  - [General Options](#general-options)
  - [Notification Options](#notification-options)
  - [Service Options](#service-options)
    - [Predefined](#predefined)
      - [Battery Charging](#battery-charging)
      - [Battery Level](#battery-state)
      - [Network Connectivity](#network-connectivity)
    - [Custom](#custom)
      - [Single](#single)
      - [Multiple](#multiple)
- [Building](#building)
  - [Prerequisites](#prerequisites)
  - [Cabal](#cabal)
  - [Stack](#stack)
  - [Nix](#nix)

# Introduction

Navi is an application for defining desktop notifications in terms of a **running notification system**. That is, Navi does not implement desktop notifications from the ground up. Rather, given a running compatible notifications system, Navi provides a simple interface for defining notifications that hook into the running system.

There are built-in services for sending specific notifications, along with functionality to send custom notifications.

# Requirements

Navi sends [freedesktop.org](https://specifications.freedesktop.org/notification-spec/notification-spec-latest.html)-compatible notifications via [D-Bus](https://en.wikipedia.org/wiki/D-Bus).

There are plans to generalize navi to other notification systems, but for now Navi requires a running notification service compatible with freedesktop.org. For example, Navi has been tested with:

- [deadd-notification-center](https://github.com/phuhl/linux_notification_center)

# Command-Line Args

Navi has the following usage:

```
Usage: navi [-f|--config-file PATH] [-d|--config-dir PATH] [-v|--version]

Available options:
  -f,--config-file PATH    Path to config file. Overrides default
                           <config-dir>/config.toml if <config-dir> is
                           specified.
  -d,--config-dir PATH     Path to config directory. Determines where we look
                           for config.toml and output log file.
  -v,--version             Displays the version number.
  -h,--help                Show this help text
```

## Config File

This argument overrides where Navi searches for the configuration file.

The default path to the config file is based on the [XDG base directory](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html). Given `xdgBase`, by default, Navi will look for `xdgBase/navi/config.toml`, e.g., `~/.config/navi/config.toml`.

## Config Directory

Navi uses `xdgBase` to:

* Search for `config.toml` (unless overridden by `--config-file`).
* Write to `navi.log`.

Thus overriding `--config-dir` will affect both of the above (again, `--config-file` takes priority over `--config-dir`).

# Configuration

Navi is configured via a toml file, by default located at `xdgBase/navi/config.toml`. Full examples can be found in [examples](./examples).

## General Options

* `poll-interval`: Mandatory. Non-negative integer, determines how often we query the system in seconds.
* `logging.severity`: Optional. One of `[debug|info|error]`. Controls the logging level. Defaults to `error`.
* `logging.location`: Optional. Either `stdout` or `<filename>`. Defaults to a file, `xdgBase/navi/navi.log`.

Example:

```toml
poll-interval = 30

[logging]
severity = "debug"
location = "stdout"
```

## Notification Options

The full list of notification options are:

* `summary`: Text summary.
* `body`: (Optional). Text body.
* `urgency`: (Optional). One of `[low|normal|critical]`.
* `timeout`: (Optional). One of `[never|<seconds>]`. Determines how long notifications persist. Defaults to 10 seconds.

## Service Options

Individual services have their own options, but there are a few that are common to most.

* `repeat-events`: One of `[true|false>`. Determines if we send off the same notification twice in a row. Defaults to `false` (i.e., no repeats) unless stated otherwise.
* `error-events`: One of `["none"|"repeats"|"no-repeats">`. Determines if we send off notifications for errors, and how we handle repeats. Defaults to `"no-repeats"` unless stated otherwise, i.e., we send error notifications but no repeats.

### Predefined

#### Battery Charging

This service sends notifications based on the current battery charging status. Options include:

##### Specific Options

* `battery-charging.type`: One of `[upower|<custom command>>`.
  * `upower` requires the `UPower` utility.
  * A custom command allows one to pass an arbitrary shell command. The requirement is that its output contains a line:
    * `state: <charge status>`, where `<charge status>` is one of `[charging|discharging|fully-charged]`.


##### General Options

* `battery-charging.repeat-events`
* `battery-charging.error-events`
* `battery-charging.timeout`

Example:

```toml
[battery-charging]
repeat-events = false
error-events = "repeats"
```

#### Battery State

This service sends notifications based on the current battery level when it is discharging. Options include:

##### Specific Options

* `battery-state.alert.level`: integer in `[0, 100]`. Sends a notification once the battery level drops below the level.

* `battery-state.type`: One of `[upower|<custom command>]`.
  * `upower` requires the `UPower` utility.
  * A custom command allows one to pass an arbitrary shell command. The requirement is that its output contains two lines:
    * `percentage: N%`, where `N` is in `[0, 100]`.
    * `state: <charge status>`, where `<charge status>` is one of `[charging|discharging|fully-charged]`.

##### General Options

* `battery-state.repeat-events`
* `battery-state.error-events`
* `battery-state.alert.urgency`
* `battery-state.alert.timeout`

Example:

```toml
[battery-state]
repeat-events = false
error-events = "repeats"

[[battery-state.alert]]
level = 80

[[battery-state.alert]]
level = 20
urgency = "critical"
```

#### Network Connectivity

This service sends notifications based on the network connectivity for given devices.

##### Specific Options

* `network-connectivity.device`: The name of the network device to monitor (e.g. `wlp0s20f3`).
* `network-connectivity.type`: One of `[networkmanager|<custom command>]`.
  * `networkmanager` requires the `NetworkManager` utility.
  * A custom command allows one to pass an arbitrary shell command. The requirement is that its output contains the following four lines for each device (i.e., there should be `4n` lines for the number of total network devices, `n`):
    * `DEVICE: <device>`
    * `TYPE: <type>`: One of `[wifi|wifi-p2p|ethernet|loopback|tun|<other>]`.
    * `STATE: <state>`: One of `[connected|disconnected|unavailable|unmanaged|<other>]`.
    * `CONNECTION: <name>`: One of `[--|<some name>]`.

##### General Options

* `network-connectivity.repeat-events`
* `network-connectivity.error-events`
* `network-connectivity.alert.urgency`
* `network-connectivity.alert.timeout`


##### Example

```toml
[[network-connectivity]]
device = "wlp0s20f3"

[[network-connectivity]]
device = "enp0s31f6"
```

### Custom

#### Single

This service allows one to create a single notification based on an arbitrary command.

##### Specific Options

* `single.command`: Command literal or path to a script.
* `single.trigger`: Result that triggers the notification.

##### General Options

* `single.repeat-events`
* `single.error-events`
* `single.note.summary`
* `single.note.body`
* `single.note.urgency`
* `single.note.timeout`

***** Example



```toml
# Send alert when the current minute is even
[[single]]
command = """
  min=`date +%M`;
  if [[ \"$min % 2\" -eq 0 ]]; then
    echo true
  else
    echo false
  fi
"""
trigger = "true"

[single.note]
summary = "Even/Odd"
body = "We're even, yay!"
```

#### Multiple

This service allows one to create multiple notifications based on an arbitrary command.

##### Specific Options

* `multiple.command`: Command literal or path to a script.
* `multiple.trigger-note.trigger`: Result that triggers the notification.

##### General Options

* `multiple.repeat-events`
* `multiple.error-events`
* `multiple.trigger-note.summary`
* `multiple.trigger-note.body`
* `multiple.trigger-note.urgency`
* `multiple.trigger-note.timeout`

Example

```toml
# Manual battery level alerts
[[multiple]]
command = """
  regex="([0-9]{1,3})%"
  power=$(upower -i `upower -e | grep 'BAT'` | grep percentage | awk '{print $2}')

  if [[ $power =~ $regex ]]; then
      power_num="${BASH_REMATCH[1]}"

      if [[ $power_num -lt 5 ]]; then
          echo 5
      elif [[ $power_num -lt 40 ]]; then
          echo 40
      elif [[ $power_num -lt 80 ]]; then
          echo 80
      else
          echo 100
      fi
  else
      echo "Error reading battery: $power"
  fi
"""

[[multiple.trigger-note]]
trigger = "80"

[multiple.trigger-note.note]
summary = "Multi"
body = "Battery < 80"
timeout = "10"

[[multiple.trigger-note]]
trigger = "40"

[multiple.trigger-note.note]
summary = "Multi"
body = "Battery < 40"
urgency = "critical"
timeout = "10"
```

# Building

## Prerequisites

You will need one of:

* [cabal-install 2.4+](https://www.haskell.org/cabal/download.html) and one of:
  * [ghc 8.10.7](https://www.haskell.org/ghc/download_ghc_8_10_7.html)
* [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
* [nix](https://nixos.org/download.html)

If you have never built a haskell program before, `stack` is probably the best choice.

## Cabal

You will need `ghc` and `cabal-install`. From there Navi can be built with `cabal build` or installed globally (i.e. `~/.cabal/bin/`) with `cabal install`.

The project is set to build with `-Werror` in `cabal.project`, so if for some reason that's a problem, you can disable this with `cabal build --ghc-options="-Wwarn"`.

## Stack


Like `cabal`, Navi can be built locally or installed globally (e.g. `~/.local/bin/`) with `stack build` and `stack install`, respectively.

## Nix

Because Navi is a flake, it can be built as part of a nix expression. For instance, if you want to add Navi to `NixOS`, your `flake.nix` might look something like:

```nix
{
  description = "My flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    navi-src.url= "github:tbidne/navi/main";
    navi-src.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, navi-src, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        system = system;
      };
      navi = navi-src.defaultPackage.${system};
    in
    {
      nixosConfigurations = {
        nixos = nixpkgs.lib.nixosSystem {
          system = system;
          modules = [
            (import ./configuration.nix { inherit pkgs navi; })
          ];
        };
      };
    };
}
```

Then in `configuration.nix` you can simply have:

```nix
{ pkgs, navi, ... }:

{
  environment.systemPackages = [
    navi
  ];
}
```