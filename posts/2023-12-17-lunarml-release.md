---
title: "Introducing LunarML: The Standard ML compiler that produces Lua/JavaScript"
---

Today I would like to announce the initial release of LunarML, the new Standard ML compiler I have been developing for several years.

* [minoki/LunarML: The Standard ML compiler that produces Lua/JavaScript](https://github.com/minoki/LunarML)

## Introduction

It is hard to create large software in an untyped language.
However, there are situations where the use of untyped languages is unavoidable due to the constraints of the runtime environment.
This situation can be remedied by a compiler, which converts programs written in statically-typed languages to code in untyped languages.
Such compilers are also called transpilers.

Since JavaScript used to be the only programming language that could be used on the Web, many compilers that compile to JavaScript have appeared.
However, there are still few compilers that output other scripting languages, such as Lua.
Therefore, I decided to create a new compiler that can convert from a statically-typed language to Lua.

As for the input language, I decided to use an existing language instead of creating a new one.
I like ML languages, so after considering several languages in the ML family, I chose Standard ML, which has the following features:

* Powerful type inference
* Strict evaluation
* The module system, which allows encapsulation and code reuse
* The Definition and multiple conforming implementations

## Features of LunarML

LunarML implements all the features of SML '97 (including the module system) and some features of Successor ML.
It also implements some extensions.

The standard library is still incomplete, but it is functional enough to compile LunarML itself.

To support multi-file projects, LunarML implements ML Basis system (MLB files) compatible with MLton and MLKit.

Of course, a program can interact with the Lua and JavaScript worlds.

Some backends support delimited continuations, which allows integration with async runtimes like Node.js.

## Build and Install

LunarML is developed on GitHub:
<https://github.com/minoki/LunarML>

You need MLton and Lua to build it.

```
$ git clone https://github.com/minoki/LunarML.git
$ cd LunarML
$ make
$ bin/lunarml compile example/hello.sml
$ lua example/hello.lua
Hello world!
```

To install LunarML in the system, use `make install`.
The destination directory can be configured by `PREFIX` variable (it defaults to `/usr/local`).

```
$ make install prefix=/opt/lunarml
$ export PATH=/opt/lunarml/bin:$PATH
$ lunarml compile example/hello.sml
```

You might not have MLton installed in the system.
In that case, you can follow the alternative method: The Docker image.

```
$ docker pull ghcr.io/minoki/lunarml:latest
$ docker run --rm --platform linux/amd64 -v "$(pwd)":/work -w /work ghcr.io/minoki/lunarml:latest lunarml compile example/hello.sml
$ lua example/hello.lua
Hello world!
```

If you do not have Docker installed, there is yet another method: The precompiled script.
If you have Node.js installed, you can download the tarball and run `install-precompiled-node` target.

```
$ curl -LO https://github.com/minoki/LunarML/releases/download/v0.1.0/lunarml-0.1.0.tar.gz
$ tar xf lunarml-0.1.0.tar.gz
$ cd lunarml-0.1.0
$ make install-precompiled-node PREFIX=/opt/lunarml
$ export PATH=/opt/lunarml/bin:$PATH
$ lunarml --help
```

Warning: The script-compiled LunarML is very slow and you should use native binary for serious use.

## Compiling some code

"Hello world" in Standard ML might look like:

```sml
print "Hello world!\n";
```

Let's compile this.
You need Lua 5.3 or 5.4 to run the compiled code.

```
$ lunarml compile hello.sml
$ lua hello.lua
Hello world!
```

You can also get JavaScript code for Node.js.
Pass the `--nodejs` or `--nodejs-cps` option:

```
$ lunarml compile --nodejs-cps hello.sml
$ node hello.mjs
Hello world!
```

A (slow) program to compute Fibonacci numbers might look like:

```sml
fun fib 0 = 0
  | fib 1 = 1
  | fib n = fib (n - 1) + fib (n - 2);
print ("fib 10 = " ^ Int.toString (fib 10) ^ "\n");
```

```
$ lunarml compile --lua fib10.sml
$ lua fib10.lua                      
fib 10 = 55
$ lunarml compile --nodejs fib10.sml
$ node fib10.mjs
fib 10 = 55
```

You can use multiple-precision integers (called `IntInf` in Standard ML).
For Lua target, LunarML uses its own implementation.
For JavaScript target, LunarML uses `BigInt`.

```sml
fun fact 0 : IntInf.int = 1
  | fact n = n * fact (n - 1);
print ("50! = " ^ IntInf.toString (fact 50) ^ "\n");
```

```
$ lunarml compile --lua fact50.sml
$ lua fact50.lua
50! = 30414093201713378043612608166064768844377641568960512000000000000
$ lunarml compile --nodejs fact50.sml
$ node fact50.mjs
50! = 30414093201713378043612608166064768844377641568960512000000000000
```

## Compiling HaMLet

[HaMLet](https://people.mpi-sws.org/~rossberg/hamlet/) is another implementation of Standard ML, written in Standard ML.
Let's compile HaMLet to Lua.

```
$ git clone https://github.com/rossberg/hamlet.git
$ cd hamlet/
$ make hamlet.mlb SYSTEM=mlton
$ lunarml compile --lua-continuations hamlet.mlb
$ lua hamlet.lua
HaMLet 2.0.0 - To Be Or Not To Be Standard ML
[loading standard basis library]
- 1 + 1;
val it = 2 : int
- OS.Process.exit OS.Process.success : unit;
```

Of course, you can also compile it to JavaScript.

```
$ lunarml compile --nodejs-cps hamlet.mlb
$ node hamlet.mjs
HaMLet 2.0.0 - To Be Or Not To Be Standard ML
[loading standard basis library]
- "Hello " ^ "world!"; 
val it = "Hello world!" : string
- OS.Process.exit OS.Process.success : unit;
```

## Generating Lua code

By default, LunarML produces code for Lua 5.3/5.4.
You can be explicit with the `--lua` option.

To produce code for LuaJIT, you need to set the `--luajit` option.

The functionality of Lua can be called via `Lua` API.
Currently it is not very easy to use, so I may introduce a more convenient way in the future.

* [LunarML/doc/LuaInterface.md at master · minoki/LunarML](https://github.com/minoki/LunarML/blob/master/doc/LuaInterface.md)

You can also generate a Lua module with the `--lib` option.
Define a variable or module named `export`, and its content will become accessible from Lua.

A module defined like this

```sml
structure export = struct
  val hello = "Hello world!"
  val print = TextIO.print
  fun add (x, y) = x + y
  val fun' = "fun!"
end;
```

will compile to something like this:

```lua
return {
  hello = "Hello world!",
  print = ...,
  add = ...,
  fun = "fun!"
}
```

The generated code is not very human readable.
Producing human-readable code is not one of my goals, but I would still like to generate code that is more readable.

## Generating JavaScript code

LunarML can also output JavaScript code.
Currently it requires Node.js for execution and does not work in a browser.
Use the `--nodejs` or `--nodejs-cps` option.

Many APIs of Node.js are asynchronous, whereas Standard ML's input/output functions are synchronous.
To cope with this difference, LunarML performs what is called the CPS conversion on the program when the `--nodejs-cps` option is used.
If the `--nodejs` option is specified, it does not perform CPS conversion, but instead restricts the input/output functions.

The functionality of JavaScript can be called via `JavaScript` API.
I may introduce a more convenient way in the future.

* [LunarML/doc/JavaScriptInterface.md at master · minoki/LunarML](https://github.com/minoki/LunarML/blob/master/doc/JavaScriptInterface.md)

You can generate an ES module via the `--lib` option.
Define a variable or module named `export`, and it will be exported.

A module defined like this

```sml
structure export = struct
  val foo = 42 : int
  val bar = "Hello world!"
  val baz = "Goodbye world!" : WideString.string
end;
```

will compile to something like this:

```js
let foo = 42;
let bar = Uint8Array.of(...);
let baz = "Goodbye world!";
export { foo, bar, baz };
```

SML's standard string type `string` is represented by `Uint8Array` in JavaScript.
You can use `WideString.string` type to manipulate JavaScript's 16-bit string.

## Future plans

LunarML is still a work in progress and there are many features that I would like to implement in the future.
Some of them are listed below:

* More complete standard library
* More features of Successor ML
* REPL and interpreter
* Online compiler
* More backends
    * JavaScript for browsers
    * PHP
    * WebAssembly with GC
* Package manager

Lastly, I would be happy to receive a star on the GitHub repository:

* [minoki/LunarML: The Standard ML compiler that produces Lua/JavaScript](https://github.com/minoki/LunarML)

Thank you for reading!
