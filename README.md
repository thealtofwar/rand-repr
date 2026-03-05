# rand-repr
Randomizes the representation of a macro as a countermeasure in embedded security

Ex:

```rust
#[randomize_repr(u32)]
enum Status {
    NoLogin,
    LoggedIn,
    SuperUser
}
```
becomes 
```rust
#[repr(u32)]
enum USER_STATE {
    NoLogin = 92476339u32,
    LoggedIn = 2671715106u32,
    SuperUser = 714567915u32,
}
```