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

* The `` -j `nproc` `` flag parallelizes the build by taking advantage
  of the multiple cores that the Pi does not have

When the build completes, we will have version of RethinkDB that runs
on the Raspberry Pi.

## Cross-compiling dependencies

We love RethinkDB's dependencies. It wouldn't be a very good database
without them. To show our love, we will abuse their build systems in
ways that were never intended. We could complain, but RethinkDB's
build system isn't any better.

Behind the scenes, the RethinkDB build system runs a different build
script for each dependency. Let's examine how these scripts handle
cross-compilation.

### zlib

zlib requires no special consideration when cross-compiling. A simple
`./configure && make` suffice.

### OpenSSL

The first thing we notice when trying to build OpenSSL is that it has
no `./configure` script. Instead, it has both a `./config` and a
`./Configure` script. The `./config` script tries to auto-detect the
host, but fails when cross-compiling.

Calling `./Configure insomnia` will list all the supported build
architectures, one of which is `linux-armv4`, which we use to
cross-compile RethinkDB for the Pi (`insomnia` is an alias for
`list`).

Although RethinkDB will statically link to OpenSSL, it also builds the
OpenSSL shared libraries. More on this later.

The build commands are therefor

```bash
./Configure linux-armv4 -shared
make -j1
```

With an extra `-j1` to slow us down, because the OpenSSL build doesn't
parallelize very well.

### Protocol Buffers

This is a higher-level library that acts as a buffer between us and
the underlying protocol. It works so well that although RethinkDB
occasionally uses Protocol Buffers (also known as Protobufs) to pass
messages around, these messages get serialized as JSON over the
network.

Protobuf is one of the libraries that we build twice: once for the
build architecture and once for the host. We first build Protobuf for
the build environment in the `cross_build/`. This gives us the
`protoc` executable, which we can use to compile `.proto` files
containing protobuf definitions. We then build Protobuf again with a
few extra arguments:

```bash
cp -a . cross_build/
( cd cross_build/
  cross_build_env
  ./configure
  make )
./configure --with-protoc=cross_build/src/protoc --host=$($CXX -dumpmachine)
make
```

`cross_bin_env` is a poorly named shell function that removes the
cross-compiler from the environment.

We also save the `protoc` from the first build to pass it down to
RethinkDB so it can also compile `.proto` files.

### ICU

ICU is built twice, just like protoc, but the `--with-cross-build`
flag is used instead of `--with-protoc`.

### V8

Even when not cross-compiling, building V8 can be very fun for
us. Luckily it has no `./configure` script nor any equivalent of
one. Instead, it has a top-level Makefile, which calls gyp, which
evaluates the instructions in the gypfiles, which generates output
makefiles, which get called to build V8.

We can pass make targets and variable assignments to the top-level
Makefile, but these aren't the same as those accepted by the ouput
makefiles, and they don't give full control over gyp. So we can pass
variables to gyp using `GYP_DEFINES`. Or is it `GYPFLAGS`? It's
both.

Like most other unusual build system, it solves problems that
autotools doesn't, and when poked the right way, everything builds
just fine.

And when poking doesn't work, patching starts to look like a viable
solution:

```bash
sed -i.bak '/unittests/d;/cctest/d' "$build_dir/build/all.gyp" # don't build the tests
```

### libcurl and libidn

These last two libraries simply require passing the `--host=` flag to
`./configure` to cross-compile.

However libcurl's `./configure` sometimes fails if it can't find a
shared openssl library, even whith `--disable-shared`, so we need a
shared version of the OpenSSL library.

## The build is done

If you are a slow reader or have a very fast PC, our build should be
complete by now. Enjoy playing with RethinkDB on your Raspberry Pi.
