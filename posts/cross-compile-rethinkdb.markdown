---
title: Cross compiling RethinkDB for ARM
date: 2015-03-06
category: code
---

![](/images/cross/thinker-build.png)

Building [RethinkDB](http://rethinkdb.com) on a Raspberry Pi takes a
very long time. There is however a way to speed it up: cross-compilation.

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

## Overview

Cross-compilation is very error-prone because it requires many
factors to be set up correctly. For RethinkDB, this means:

* A build toolchain for building code that needs to run during the
  build. This toolchain should be easily available, it is composed of
  your everyday compiler

* A cross-compiler for the actual build, targeting the correct host
  architecture and properly installed in the build environment. 

* Development files for all dependencies. This can includes header
  files, host libraries and sometimes even build tools, libraries and
  data

* Passing around the correct flags and paths. Although some flags are
  [somewhat
  standard](https://www.gnu.org/software/autoconf/manual/autoconf-2.69/html_node/Hosts-and-Cross_002dCompilation.html)
  each library has their own way of dealing with cross-compilation, if
  they do at all

For testing or simplifying the build, an emulator such as
[qemu](http://wiki.qemu.org/Main_Page) can be very useful, especially
when combined with
[binfmt_misc](https://packages.debian.org/sid/binfmt-support).

## The cross-compiler

The [raspberrypi/tools](https://github.com/raspberrypi/tools)
repository on github contains a pre-compiled cross-compiler for the
raspberry pi, with no installation necessary other than downloading
the repository.

```shell
git clone --depth 1 https://github.com/raspberrypi/tools.git pi-tools
```

Other options include:

* Gentoo's
  [crossdev](https://www.gentoo.org/proj/en/base/embedded/handbook/?part=1&chap=2)
  tool, my favourite method of installing a cross-compiler.

* It should be possible to build a cross-compiler using
  [crossdev-ng](http://crosstool-ng.org/) but I haven't tried their approach yet.

* On some versions of linux, you may find that a package with a name
  such as
  [g++-4.6-arm-linux-gnueabi](http://packages.ubuntu.com/precise/devel/g++-4.6-arm-linux-gnueabi)
  is available. I have not had any success getting RethinkDB to run
  after building with this cross-compiler.

## The environment

RethinkDB makes two assumptions when cross-compiling:

* The cross-compiler tools are available in the `CC`, `CXX`, `LD`, `AR` and `RANLIB` variables

* The build toolchain is available in the `PATH`

This script will setup such an environment for us:

```bash
CROSS=~/pi-tools/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian-x64/bin/arm-linux-gnueabihf
export CC=$CROSS-cc
export CXX=$CROSS-c++
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
git clone -b atnnn/cross https://github.com/rethinkdb/rethinkdb
cd rethinkdb
rm -rf external/v8*
./configure --allow-fetch --with-system-malloc
make -j `nproc`
```

* Cross-compiling now requires the patches from the `atnnn/cross`
  branch, but these will be merged into `HEAD` for RethinkDB 2.0

* The bundled version of v8 does not include a copy of the ICU
  library, which it requires for cross-compiling, so it has to be
  removed

* The `--allow-fetch` flag tells RethinkDB to fetch all the missing
  dependencies. The RethinkDB build system includes a tiny package
  manager that can download and cross-compile all the required
  dependencies, such as OpenSSL, Protobuf, ICU, V8 and Curl

* The `--with-system-malloc` flag disables the use of `jemalloc`

* The ``-j `nproc``` flag parallelizes the build by taking advantage
  of the multiple cores that the Pi does not have

When the build completes, we will have version of RethinkDB that runs
on the Raspberry Pi.

But how does it all work?

## Behind the scenes

The RethinkDB `./configure` script and make files talk to a tiny
package manager called
[pkg.sh](https://github.com/rethinkdb/rethinkdb/blob/next/mk/support/pkg/pkg.sh). This
script