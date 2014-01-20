Macros are programs that can be invoked in user programs, similarly to functions, but which
- are executed at compile-time, that is, at runtime of the compiler
- manipulate a part of the compiler state, that is the program.

Programs which manipulate other programs are called *metaprograms*. Macros are an example.

In compiled languages, macros act at compile-time, so sometimes macros can be used in contexts where
functions are not allowed because that context contains something that must be
known at compile-time (declarations of types/methods/etc.).
