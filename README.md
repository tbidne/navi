<div align="center">

# Navi

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/navi?include_prereleases&sort=semver)](https://github.com/tbidne/navi/releases/)
[![MIT](https://img.shields.io/github/license/tbidne/navi?color=blue)](https://opensource.org/licenses/MIT)

[![nix](https://img.shields.io/github/workflow/status/tbidne/navi/nix/main?label=nix%209.0.2&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/navi/actions/workflows/nix_ci.yaml)
[![stack](https://img.shields.io/github/workflow/status/tbidne/navi/stack/main?label=stack%2019.4&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/navi/actions/workflows/stack_ci.yaml)
[![style](https://img.shields.io/github/workflow/status/tbidne/navi/style/main?label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/navi/actions/workflows/style_ci.yaml)

[![8.10.7](https://img.shields.io/github/workflow/status/tbidne/navi/8.10.7/main?label=8.10.7&logo=haskell&logoColor=904d8c&labelColor=2f353c)](https://github.com/tbidne/navi/actions/workflows/ghc_8-10.yaml)
[![9.0.2](https://img.shields.io/github/workflow/status/tbidne/navi/9.0.2/main?label=9.0.2&logo=haskell&logoColor=904d8c&labelColor=2f353c)](https://github.com/tbidne/navi/actions/workflows/ghc_9-0.yaml)

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
      - [Battery Status](#battery-status)
      - [Battery Percentage](#battery-percentage)
      - [Network Interface](#network-interface)
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
Usage: navi [-f|--config-file PATH] [-v|--version]

Available options:
  -f,--config-file PATH    Path to config file. Defaults to
                           <xdgConfig>/navi/config.toml.
  -h,--help                Show this help text
```

## Config File

This argument overrides where Navi searches for the configuration file.

The default path to the config file is based on the [XDG base directory](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html). Given `xdgBase`, by default, Navi will look for `xdgBase/navi/config.toml` e.g. `~/.config/navi/config.toml`.

# Configuration

Navi is configured via a toml file, by default located at `xdgBase/navi/config.toml`. Full examples can be found in [examples](./examples).

## General Options

* `logging.severity`: Optional. One of `[debug|info|error]`. Controls the logging level. Defaults to `error`.
* `logging.location`: Optional. Either `default`, `stdout` or `<filename>`. No option or `default` uses `xdgBase/navi/navi.log`.

##### Example

```toml
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

* `poll-interval`: Optional. Non-negative integer, determines how often we query the system for the particular service, in seconds. Each service defines its own default.
* `repeat-events`: One of `[true|false]`. Determines if we send off the same notification twice in a row. Defaults to `false` (i.e. no repeats) unless stated otherwise.
* `error-events`: One of `["none"|"repeats"|"no-repeats">`. Determines if we send off notifications for errors, and how we handle repeats. Defaults to `"no-repeats"` unless stated otherwise i.e. we send error notifications but no repeats.

### Predefined

These are services that are built-in, in the sense that no custom script is required.

#### Battery Status

This service sends notifications based on the current battery status. Options include:

##### Specific Options

* `battery-status.app`: (Optional). One of `[sysfs | acpi | upower]`.
  * `sysfs` reads `/sys` or `/sysfs` directly.
  * `acpi` requires the `acpi` utility.
  * `upower` requires the `upower` utility.
  * If no option is given then we will try each of the above in the given order, if they are supported.


##### General Options

* `battery-status.poll-interval`: Defaults to 30 seconds.
* `battery-status.repeat-events`
* `battery-status.error-events`
* `battery-status.timeout`

##### Example

```toml
[battery-status]
repeat-events = false
error-events = "repeats"
```

#### Battery Percentage

This service sends notifications based on the current battery percentage when it is discharging. Options include:

##### Specific Options

* `battery-percentage.alert.level`: integer in `[0, 100]`. Sends a notification once the battery level drops below the level.
* `battery-percentage.app`: (Optional). One of `[sysfs | acpi | upower]`.
  * `sysfs` reads `/sys` or `/sysfs` directly.
  * `acpi` requires the `acpi` utility.
  * `upower` requires the `upower` utility.
  * If no option is given then we will try each of the above in the given order, if they are supported.

##### General Options

* `battery-percentage.poll-interval`: Defaults to 30 seconds.
* `battery-percentage.repeat-events`
* `battery-percentage.error-events`
* `battery-percentage.alert.urgency`
* `battery-percentage.alert.timeout`

##### Example

```toml
[battery-percentage]
repeat-events = false
error-events = "repeats"

[[battery-percentage.alert]]
percent = 80

[[battery-percentage.alert]]
percent = 20
urgency = "critical"
```

#### Network Interface

This service sends notifications based on the network connectivity for given devices.

##### Specific Options

* `net-interface.device`: The name of the network device to monitor (e.g. `wlp0s20f3`).
* `net-interface.app`: One of `[nmcli | ip]`.
  * `nmcli` requires the `nmcli` (`NetworkManager cli`) utility.
  * `ip` requires the `ip` utility.
  * If no option is given then we will try each of the above in the given order, if they are supported.

##### General Options

* `net-interface.poll-interval`: Defaults to 30 seconds.
* `net-interface.repeat-events`
* `net-interface.error-events`
* `net-interface.alert.urgency`
* `net-interface.alert.timeout`

##### Example

```toml
[[net-interface]]
device = "wlp0s20f3"

[[net-interface]]
device = "enp0s31f6"
```

### Custom

#### Single

This service allows one to create a single notification based on an arbitrary command.

##### Specific Options

* `single.command`: Command literal or path to a script.
* `single.trigger`: Result that triggers the notification.

##### General Options

* `single.poll-interval`: Defaults to 30 seconds.
* `single.repeat-events`
* `single.error-events`
* `single.note.summary`
* `single.note.body`
* `single.note.urgency`
* `single.note.timeout`

##### Example

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

* `multiple.poll-interval`: Defaults to 30 seconds.
* `multiple.repeat-events`
* `multiple.error-events`
* `multiple.trigger-note.summary`
* `multiple.trigger-note.body`
* `multiple.trigger-note.urgency`
* `multiple.trigger-note.timeout`

##### Example

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
  * [ghc 9.0.2](https://www.haskell.org/ghc/download_ghc_9_0_2.html)
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