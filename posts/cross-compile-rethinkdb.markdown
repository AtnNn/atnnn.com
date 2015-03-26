---
title: Cross compiling RethinkDB for ARM
date: 2015-03-06
category: code
---

![](/images/cross/thinker-build.png)

Building [RethinkDB](http://rethinkdb.com) on a Raspberry Pi takes a
very long time. However we can speed it up using cross-compilation.

## Terminology

Cross-compilation allows us to build ARM software on a PC. There is a
common terminology used when cross-compiling that can be confusing:

* The **build** architecture is the architecture we are building on

* The **host** architecture is the architecture the code will run in

* When building or referencing a compiler or related tools, the
  **target** architecture is the architecture that it will build for

* The **toolchain** is the set of compilers and tools for a given
  architecture

* The host toolchain is also known as the **cross-compiler**. It runs on the build
  architecture and targets the host architecture.

In this post, We will walk through the steps required to **build** on
a Linux PC a version of RethinkDB that will run, or be **host**ed, on
ARM.

## The cross-compiler

The [raspberrypi/tools](https://github.com/raspberrypi/tools)
repository on github contains a pre-compiled cross-compiler for the
raspberry pi, with no installation necessary other than downloading
the repository.

```shell
git clone --depth 1 https://github.com/raspberrypi/tools.git pi-tools
```

If that compiler doesn't work, tools such as
[crossdev](https://www.gentoo.org/proj/en/base/embedded/handbook/?part=1&chap=2)
or [crossdev-ng](http://crosstool-ng.org/) can be used to build a
cross-compiler from scratch.

## The environment

RethinkDB makes two assumptions when cross-compiling:

* The cross-compiler tools are available in the `CC`, `CXX`, `LD`, `AR` and `RANLIB` variables

* The build toolchain is available in the `PATH`

This script will setup such an environment for us:

```bash
CROSS=~/pi-tools/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian-x64/bin/arm-linux-gnueabihf
export CC=$CROSS-gcc
export CXX=$CROSS-g++
export LD=$CROSS-ld
export AR=$CROSS-ar
export RANLIB=$CROSS-ranlib
```

We can pass the `-dumpmachine` argument to GCC to check the target architecture:

```bash
$ uname -om
x86_64 GNU/Linux
$ gcc -dumpmachine
x86_64-linux-gnu
$ $CXX -dumpmachine
arm-linux-gnueabihf
```

## The build

We can now fetch and build RethinkDB for the Raspberry Pi.

```bash
git clone -b v2.0.x https://github.com/rethinkdb/rethinkdb
cd rethinkdb
rm -rf external/v8*
./configure --allow-fetch --with-system-malloc
make -j `nproc`
```

* Cross-compiling has been tested in the v2.0.x branch.

* The bundled version of v8 does not include a copy of the ICU
  library, which it requires for cross-compiling, so it has to be
  removed

* The `--allow-fetch` flag tells RethinkDB to fetch missing
  libraries. The RethinkDB build system includes a tiny package
  manager that can download and cross-compile all the required
  dependencies.

* The `--with-system-malloc` flag disables the use of `jemalloc`

* The `` -j `nproc` `` flag parallelizes the build by taking advantage
  of the multiple cores that the Pi does not have

When the build completes, we will have version of RethinkDB that runs
on the Raspberry Pi:

```
$ scp build/release_system/rethinkdb pi:
$ ssh pi ./rethinkdb
Running rethinkdb 1.16.0-1-447-g4400d8 (GCC 4.8.3)...
Running on Linux 3.18.7+ armv6l
```
