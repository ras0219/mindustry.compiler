# Language Support

## Supported Constructs

- Functions
- Integer types
- `const`
- Pointers
- Arrays of fixed size
- Recursion
- Arithmetic
- Indexing (`[]`)
- Relations
- `for`, `while`, `do`
- `goto` and labels
- `#include`

## Unsupported Constructs

- `#define`, `#if`, `#ifdef`
- `switch`, `case`
- `static`
- `struct`, `union`, `enum` (with a limited exception for structs as types)
- Casts
- `sizeof()`
- `auto`
- Conversions to and from integer types and `void*`
- `float`/`double`
- Array initializer syntax
- String manipulation

## Memory

External memory is required for the following operations:

- Recursion / Reentrancy
- Dereferencing pointers & arrays

To configure external memory, use `#pragma memory <memory>`.

```c
#pragma memory memory1
```

## Attributes

### Symbol Name

This can be used to handle constant identifiers, such as linked device names.

```c
struct unit __attribute__((sym("memory1"))) *memory;
```

### Assembly String

This can be used to expose underlying ISA instructions.

Use `%r` for the return register position and `%N` (starting at 0) for the arguments.

```c
int __attribute__((asmstr("ucontrol within %0 %1 %2 %r 0")))
    unit_within(int x, int y, int radius);
```
