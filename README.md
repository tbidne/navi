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
  temp_res=$(sensors | head -n 3 | tail -n 1)
  regex="temp1:\\s*\\+([0-9]+)\\.[0-9]{0,2}°[C|F]"

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
    * [KDE Plasma](https://kde.org/plasma-desktop/)

* Libnotify

  Navi can also use the `notify-send` tool directly. This is largely redundant since `notify-send` itself requires a running DBus notification server, but this option is provided as an alternative.

# Configuration

Navi is configured via a toml file, by default located at `<xdg-config>/navi/config.toml`. Full examples can be found in [examples](./examples).

## General Options

* `note-system`: Optional. One of `["dbus"|"notify-send"]`. Defaults to `"dbus"`.
* `logging.severity`: Optional. One of `["debug"|"info"|"error"]`. Controls the logging level. Defaults to `error`.
* `logging.location`: Optional. Either `"default"`, `"stdout"` or `"<filename>"`. No option or `default` uses `<xdg-state>/navi/<timestamp>.log` e.g. `~/.local/state/navi/<timestamp>.log`.
* `logging.size-mode`: Optional. Sets a size threshold for the file log directory, upon which we either print a warning or delete all prior logs, if the threshold is exceeded. The `SIZE` should include the value and units e.g. `warn 10 mb`, `warn 5 gigabytes`, `delete 20.5B`. Defaults to `delete 50 mb`. This only affects the _default_ log path e.g. `~/.local/state/navi`.

##### Example

```toml
note-system = "dbus"

[logging]
severity = "debug"
location = "stdout"
size-mode = "warn 10 mb"
```

## Notification Options

The full list of notification options are:

* `summary`: Text summary.
* `body`: (Optional). Text body.
* `urgency`: (Optional). One of `["low"|"normal"|"critical"]`.
* `timeout`: (Optional). One of `["never"|<seconds>]`. Determines how long notifications persist. Defaults to 10 seconds.

## Service Options

Individual services have their own options, but there are a few that are common to most.

* `poll-interval`: Optional. One of `[NATURAL | STRING]`. The provided interval be either a raw natural (interpreted as seconds), or a "time string" e.g. `1d2m3h4s`, `3h20s`. Determines how often a service is polled.
* `repeat-events`: One of `[true|false]`. Determines if we send off the same notification twice in a row. Defaults to `false` (i.e. no repeats) unless stated otherwise.
* `error-events`: One of `["none"|"repeats"|"no-repeats"]`. Determines if we send off notifications for errors, and how we handle repeats. Defaults to `"no-repeats"` unless stated otherwise i.e. we send error notifications but no repeats.

### Predefined

These are services that are built-in, in the sense that no custom script is required.

#### Battery Status

This service sends notifications based on the current battery status. Options include:

##### Specific Options

* `battery-status.app`: One of `["sysfs" | "acpi" | "upower"]`.
  * `sysfs` reads `/sys` or `/sysfs` directly.
  * `acpi` requires the `acpi` utility.
  * `upower` requires the `upower` utility.


##### General Options

* `battery-status.poll-interval`: Defaults to 30 seconds.
* `battery-status.repeat-events`
* `battery-status.error-events`
* `battery-status.timeout`

##### Example

```toml
[battery-status]
app = "sysfs"
repeat-events = false
error-events = "repeats"
```

#### Battery Percentage

This service sends notifications based on the current battery percentage when it is discharging. Options include:

##### Specific Options

* `battery-percentage.alert.percent`: integer in `[0, 100]`. Sends a notification once the battery level drops to this level.
* `battery-percentage.app`: One of `["sysfs" | "acpi" | "upower"]`.
  * `sysfs` reads `/sys` or `/sysfs` directly.
  * `acpi` requires the `acpi` utility.
  * `upower` requires the `upower` utility.

##### General Options

* `battery-percentage.poll-interval`: Defaults to 30 seconds.
* `battery-percentage.repeat-events`
* `battery-percentage.error-events`
* `battery-percentage.alert.urgency`
* `battery-percentage.alert.timeout`

##### Example

```toml
[battery-percentage]
app = "upower"
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
* `net-interface.app`: One of `["nmcli" | "ip"]`.
  * `nmcli` requires the `nmcli` (`NetworkManager cli`) utility.
  * `ip` requires the `ip` utility.

##### General Options

* `net-interface.poll-interval`: Defaults to 30 seconds.
* `net-interface.repeat-events`
* `net-interface.error-events`
* `net-interface.alert.urgency`
* `net-interface.alert.timeout`

##### Example

```toml
[[net-interface]]
app = "nmcli"
device = "wlp0s20f3"

[[net-interface]]
app = "ip"
device = "enp0s31f6"
```

### Custom

#### Single

This service sends a single notification based on an arbitrary command.

##### Specific Options

* `single.command`: Command literal or path to a script.
* `single.name`: Optional name to be used in logging.
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
poll-interval = 10
command = """
  min=`date +%M`;
  if [[ \"$min % 2\" -eq 0 ]]; then
    echo -n "true"
  else
    echo -n "false"
  fi
"""
trigger = "true"

[single.note]
summary = "Even/Odd"
body = "We're even, yay!"
timeout = 10
```

#### Multiple

This service sends multiple notifications based on an arbitrary command.

##### Specific Options

* `multiple.command`: Command literal or path to a script.
* `multiple.name`: Optional name to be used in logging.
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
name = "battery-manual"
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
trigger = "100"

[multiple.trigger-note.note]
summary = "Battery Percentage"
body = "Full"
timeout = 10

[[multiple.trigger-note]]
trigger = 80

[multiple.trigger-note.note]
summary = "Battery Percentage"
body = "< 80"
timeout = 10

[[multiple.trigger-note]]
trigger = "40"

[multiple.trigger-note.note]
summary = "Battery Percentage"
body = "< 40"
urgency = "critical"
timeout = 10
```

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
