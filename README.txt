The Xera programming language

___( what's this? )__________________________________________________________

Xera is an experimental programming language, very much a work in progress
(how could it be otherwise? ;-), etc.

tl;dr: Not much to see here unless you're into programming language geekery
and/or Scheme.


___( requirements & installation )___________________________________________

- Chicken Scheme 4.8 [http://call-cc.org/]
- eggs: regex
        test

Building Xera:

- Run make.sh. This will create a directory xera/ with the executable.
- Run deploy.sh. This will locally install any necessary eggs as dependencies.
- Run xera/xera to get a (very primitive) REPL.


___( the language )__________________________________________________________

Inspired by Io, I wanted to create a language that consists of only a very
few, simple parts. In this case, Xera has

- atoms (numbers, strings, symbols, etc)
- blocks
- method calls

That's it. Everything else can be built with these tools. 

- Everything is an object, including namespaces
- No operators

[...to be continued...]

