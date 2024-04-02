# rust-ndis-examples

ndisprot-kmdf and mux examples ported to Rust.

## Compiling With or Without LTO

By default, we try to compile using cross-language LTO in order to inline the macro wrapper functions, but this requires that `clang-cl` and `llvm-lib` be in `PATH` (see [Usage with clang-cl and x86_64-pc-windows-msvc] in the rustc book).
If building without LTO is desired (e.g. if you don't want to add LLVM tools to `PATH`), you can specify `NO_LTO` when building a specific driver package:
```shell
cargo make --cwd driver/<driver_name> --env NO_LTO=1 dist
```

## License

Licensed under either of

- Apache License, Version 2.0 (see [LICENSE-APACHE](LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)
- MIT License (see [LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.

[Usage with clang-cl and x86_64-pc-windows-msvc]: https://doc.rust-lang.org/rustc/linker-plugin-lto.html#usage-with-clang-cl-and-x86_64-pc-windows-msvc