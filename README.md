# titi-lang

A tiny language with minimal features!

```
fn main() {
    print("hello, world!");
}
```

## Primitive types

| Keyword | Size                             | Desc                   |
| ------- | -------------------------------- | ---------------------- |
| bool    | 1 bits (traited as 8 bits)       | Boolean                |
| byte    | 8 bits                           | Unsigned integer       |
| char    | 32 bits                          | Unsigned integer       |
| i64     | 64 bits                          | Signed integer         |
| u64     | 64 bits                          | Unsigned integer       |
| isize   | 32/64(follows pointer size) bits | Signed integer         |
| usize   | 32/64(follows pointer size) bits | Unsigned integer       |
| f64     | 64 bits                          | IEEE754 floating point |

## Pointers and References

| Keyword | Desc             | Usage                |
| ------- | ---------------- | -------------------- |
| cptr    | Constant pointer | `let foo = cptr i8;` |
| mptr    | Mutable pointer  | `let foo = mptr i8;` |

## Tuples

```
let foo = (0usize);
let bar = (f32, f64);

bar = (3.14f32, 3.14f64);

print("{}", foo.0);
print("{}, {}", bar.0, bar.1);
```

## Static arrays

```
let foo = cptr u64;
foo = [0u64, 1u64, 2u64];

let bar = [0f32, 0.5f32, 1f32, 1.5f32];
```

## Syntaxes

### Comment

There is a line comment only.

```
# This is a line comment!
```

### Literals

#### Bool literals

- `true`
- `false`

#### Byte literals

Any number between [0, 255]

#### Char literals

A single character enclosed with '. It supports escape sequences.

| Escape | Desc             |
| ------ | ---------------- |
| `\n`   | Constant pointer |
| mptr   | Mutable pointer  |

### Variables

### Functions

### Flow controls

#### If statement

```
if some_function() == 'yes' {

} else if some_function() == 'no' {

} else {

}
```

#### Loop statements

##### Infinite loop

```
for {

}
```

##### While loop

```
for some_condition {

}
```

##### Foreach loop

```
for index in 0..1 {

}
```

```
fn fill_zero(dest=mptr usize) {
    dest = 0;
}
```
