---
title: How to use GHC's LLVM backend
---

Japanese version: [GHCのLLVMバックエンドの使い方](https://zenn.dev/mod_poppo/articles/ghc-llvm-backend) *This article was translated from Japanese with help of AI. Of course, the translation was checked and corrected by the human.*

---

GHC is a compiler capable of generating native code.
It provides three methods for generating native code: its own NCG (Native Code Generator) backend, the LLVM backend, and the via-C backend that generates code via C sources.

Typically, the NCG backend is used to generate code for x86 or AArch64, but there may be situations where you want to use the LLVM backend.
This article explains how to enable and use the LLVM backend in GHC.

<!-- GHC introduced the LLVM backend in version 7.0, released in 2010. -->

Here are the features of the LLVM backend compared to the NCG backend:

* Supports more architectures: As of this writing, ARM, AArch64, LoongArch, RISC-V, System Z, x86, and x86-64. (Some of these are also supported by NCG, but others are only supported via the LLVM backend.)
* Supports SIMD primitives (Though NCG supports some SIMD primitives for x86-64 starting from GHC 9.12, it is still in development.)
* Leverages optimizations implemented in LLVM.

For certain targets, using the LLVM backend is mandatory.
For example:

* To generate code for AArch64 with GHC <= 9.0, you need the LLVM backend.
* Similarly, generating RISC-V code with GHC <= 9.10 requires the LLVM backend.

Note: The sections "Installing LLVM" and "Configuring GHC to Use LLVM" do not apply to Windows. For Windows-specific instructions, see the "For Windows" section below.

## Installing LLVM

For non-Windows systems, LLVM is not bundled with GHC and must be installed separately using your system's package manager.

The version of LLVM you need depends on the GHC version.
The following table summarizes the compatibility:

| GHC | LLVM |
|:-|:-|
| GHC 8.10.7 | LLVM >= 9 && <= 12 |
| GHC 9.0.2 | LLVM >= 9 && <= 12 |
| GHC 9.2.8 | LLVM >= 9 && <= 12 |
| GHC 9.4.8 | LLVM >= 10 && <= 13 (if you are using `-mavx`, you need LLVM <= 12) |
| GHC 9.6 | LLVM >= 11 && <= 15 (if you are using `-mavx`, you need LLVM <= 12) |
| GHC 9.8 | LLVM >= 11 && <= 15 (if you are using `-mavx`, you need LLVM <= 12) |
| GHC 9.10 | LLVM >= 13 && <= 15 |
| GHC 9.12 | LLVM >= 13 && <= 19 |

Note: Use LLVM 12 for GHC <= 9.8 if you plan to use the `-mavx` option ([GHC #23870](https://gitlab.haskell.org/ghc/ghc/-/issues/23870)).

Here, I describe the procedures to install LLVM 15.

For Ubuntu or similar systems using `apt`:

```
$ sudo apt install llvm-15 clang-15
```

For macOS using Homebrew:

```
$ brew install llvm@15
```

For macOS using MacPorts:

```
$ sudo port install llvm-15 clang-15
```

Among the libraries and commands provided by LLVM, the ones required by GHC are these:

* The `opt` command: The optimizer for LLVM IR.
* The `llc` command: The tool to generate assembly output from LLVM IR.
* The `clang` command: GHC 9.10+ uses it as an assembler.

Let's see where these commands are located.

On Ubuntu, commands have the suffix `-15`:

```
$ which opt-15 llc-15 clang-15
/usr/bin/opt-15
/usr/bin/llc-15
/usr/bin/clang-15
```

Alternatively, `/usr/lib/llvm-15/bin` contains the commands without suffix:

```
$ ls /usr/lib/llvm-15/bin/{opt,llc,clang}
/usr/lib/llvm-15/bin/clang
/usr/lib/llvm-15/bin/llc
/usr/lib/llvm-15/bin/opt
```

On macOS with Homebrew, the commands are in `$(brew --prefix llvm@15)/bin`:

```
$ ls $(brew --prefix llvm@15)/bin/{opt,llc,clang}
/opt/homebrew/opt/llvm@15/bin/clang
/opt/homebrew/opt/llvm@15/bin/llc
/opt/homebrew/opt/llvm@15/bin/opt
```

On macOS with MacPorts, commands have the suffix `-mp-15`:

```
$ which opt-mp-15 llc-mp-15 clang-mp-15
/opt/local/bin/opt-mp-15
/opt/local/bin/llc-mp-15
/opt/local/bin/clang-mp-15
```

Alternatively, `/opt/local/libexec/llvm-15/bin` contains the commands without suffix:

```
$ ls /opt/local/libexec/llvm-15/bin/{opt,llc,clang}
/opt/local/libexec/llvm-15/bin/clang
/opt/local/libexec/llvm-15/bin/llc
/opt/local/libexec/llvm-15/bin/opt
```

Adding the directory containing the suffix-less commands to your PATH makes it easier for GHC to locate them, which is convenient.
However, since different versions of GHC may require different versions of LLVM, I do not recommend adding the suffix-less commands to your PATH if you need to manage multiple GHC/LLVM versions simultaneously.

## Configuring GHC to Use LLVM

GHC needs to know the location of the LLVM tools (`opt`, `llc`, and `clang`).
There are two ways to configure this:

* Specify the paths each time you compile.
* Set the paths when installing GHC.

### Specify Paths at Compile Time

Use the `-pgmlo`, `-pgmlc`, and `-pgmlas` flags to specify the locations of `opt`, `llc`, and `clang`, respectively.
For example, if you want to use GHC 9.12.1 with LLVM 15 installed by Homebrew, you type:

```
$ ghc-9.12.1 -fllvm -pgmlo $(brew --prefix llvm@15)/bin/opt \
    -pgmlc $(brew --prefix llvm@15)/bin/llc \
    -pgmlas $(brew --prefix llvm@15)/bin/clang Main.hs
```

### Configure Paths During GHC Installation

If you specify the location of LLVM tools during GHC installation, you won't need to specify options like `-pgmlo` each time.

First, if the LLVM tools are in a directory included in your PATH, and the command names follow one of the formats below:

```
opt-15 opt-15.0 opt15 opt
llc-15 llc-15.0 llc15 llc
clang-15 clang-15.0 clang15 clang
```

GHC's `configure` command will automatically detect and record the LLVM toolchain names during installation.

If LLVM wasn't installed at the time of GHC installation, you will need to reinstall or reconfigure GHC after installing LLVM.
For example. using GHCup, you can run:

```
$ ghcup install ghc 9.12.1 --force
```

With Stack, you can use:

```
$ stack setup 9.12.1 --reinstall
```

If the LLVM tools are not in the PATH or their names are non-standard (e.g. when using Homebrew or MacPorts), you need to manually specify the locations of `opt`, `llc`, and `clang` when running GHC's `configure` command.
This is done by setting the `OPT`, `LLC`, and `LLVMAS` environment variables.

For GHCup, the process looks like this:

```
$ # Using Homebrew
$ env OPT=$(brew --prefix llvm@15)/bin/opt \
      LLC=$(brew --prefix llvm@15)/bin/llc \
      LLVMAS=$(brew --prefix llvm@15)/bin/clang \
      ghcup install ghc 9.12.1 --force

$ # Using MacPorts
$ env OPT=opt-mp-15 LLC=llc-mp-15 LLVMAS=clang-mp-15 \
      ghcup install ghc 9.12.1 --force
```

If you installed GHC using Stack, follow these steps:

```
$ # Using Homebrew
$ env OPT=$(brew --prefix llvm@15)/bin/opt \
      LLC=$(brew --prefix llvm@15)/bin/llc \
      LLVMAS=$(brew --prefix llvm@15)/bin/clang \
      stack setup 9.12.1 --reinstall

$ # Using MacPorts
$ env OPT=opt-mp-15 LLC=llc-mp-15 LLVMAS=clang-mp-15 \
      stack setup 9.12.1 --reinstall
```

To install GHC directly from a tarball:

```
$ curl -LO https://downloads.haskell.org/~ghc/9.12.1/ghc-9.12.1-aarch64-apple-darwin.tar.xz
$ tar xf ghc-9.12.1-aarch64-apple-darwin.tar.xz
$ cd ghc-9.12.1-aarch64-apple-darwin

$ # Using Homebrew
$ env OPT=$(brew --prefix llvm@15)/bin/opt \
      LLC=$(brew --prefix llvm@15)/bin/llc \
      LLVMAS=$(brew --prefix llvm@15)/bin/clang \
      ./configure --prefix=/opt/ghc-9.12.1

$ # Using MacPorts
$ env OPT=opt-mp-15 LLC=llc-mp-15 LLVMAS=clang-mp-15 \
      ./configure --prefix=/opt/ghc-9.12.1

$ sudo make install
```

The names of the detected or manually specified LLVM commands (`opt`, `llc`, `clang`) are recorded in the `lib/settings` file.
You can check the content like this:

```
$ # For GHCup
$ grep LLVM ~/.ghcup/ghc/9.12.1/lib/ghc-9.12.1/lib/settings
,("LLVM target", "arm64-apple-darwin")
,("LLVM llc command", "/opt/homebrew/opt/llvm@15/bin/llc")
,("LLVM opt command", "/opt/homebrew/opt/llvm@15/bin/opt")
,("LLVM llvm-as command", "/opt/homebrew/opt/llvm@15/bin/clang")

$ # For Stack
$ grep LLVM ~/.stack/programs/aarch64-osx/ghc-9.12.1/lib/ghc-9.12.1/lib/settings
,("LLVM target", "arm64-apple-darwin")
,("LLVM llc command", "/opt/homebrew/opt/llvm@15/bin/llc")
,("LLVM opt command", "/opt/homebrew/opt/llvm@15/bin/opt")
,("LLVM llvm-as command", "/opt/homebrew/opt/llvm@15/bin/clang")
```

Note:
If you temporarily add `PATH=$(brew --prefix llvm@15)/bin:$PATH` to include the LLVM commands during setup, only the command names (e.g. `opt`) are recorded in the `lib/settings` file instead of the full paths.
For this reason, it is better to use environment variables like `OPT` when specifying the locations of the commands.

## For Windows

On Windows, GHC includes LLVM starting from version 9.4.
With GHC 9.12 and later, the LLVM bundled with GHC is automatically used.

However, for GHC versions <= 9.10, attempting to use the LLVM backend with floating-point numbers can lead to linker errors, making it effectively unusable.
For more details, refer to [GHC #22487](https://gitlab.haskell.org/ghc/ghc/-/issues/22487).

## Using the LLVM Backend

If the setup is successful, you can use the LLVM backend by passing the `-fllvm` option to GHC:

```
$ ghc -fllvm hello.hs
```

Of course, if you chose to the "specify paths at compile time" method, you'll also need to provide options such as `-pgmlo` each time.

## Using the LLVM Backend in GitHub Actions

There may be scenarios where you want to enable the LLVM backend in GitHub Actions.
With the knowledge gained so far, configuring the LLVM backend in GitHub Actions should not be difficult.
For a practical example, see [ghc-llvm-backend-test/.github/workflows/build.yaml](https://github.com/minoki/ghc-llvm-backend-test/blob/main/.github/workflows/build.yaml).

One point to note is that the runner image might already include a version of GHC.
In such cases, you need to pass the `--force` option to GHCup.
Since the current `haskell-actions/setup` does not support passing the `--force` option, you can either install GHCup directly or use the [haskell/ghcup-setup](https://github.com/marketplace/actions/ghcup-setup) action.

## Known Issues with the LLVM Backend

Here are some known issues related to the LLVM backend, including ones already mentioned:

* GHC <= 9.8 && LLVM >= 13: [#23870: LLVM 13+ doesn't recognize `-stack-alignment` option · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/23870) This issue arises when the `-mavx` option is specified.

* GHC <= 9.10 on Windows: [#22487: Link error when using LLVM backend on Windows: undefined symbol: `_fltused` · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/22487) This occurs whe floating-point numbers are used.

* GHC 9.10.1 on macOS: [#24999: LLVM version detection logic in configure doesn't work on macOS · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/24999)

## Conclusion

Start using GHC's LLVM backend to take full advantage of its optimizations and architecture support.
Happy coding!
