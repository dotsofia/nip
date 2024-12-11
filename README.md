# Getting Started

1. Install CMake 3.20 or newer <br>
2. Install LLVM and Clang 20 <br>
3. ```git clone github.com/dotsofia/nip``` <br>
4. ```cd nip && mkdir build && cd build``` <br>
5. ```cmake ../ && cmake --build .``` 

# Features

## Static Types

```
fn main(): void  {
  let x: number = 123;
}
```

## Type inference

```
fn main(): void {
  let inferred = 1;
}
```

## Immutability
```
fn main(): void {
  const immutable = 3;

  immutable = 4;
}
```
```
main.nip:4:7: error: 'immutable' cannot be mutated
```
