# rspg - Rust Simple Parser Generator

[![Build Status](https://travis-ci.com/linyinfeng/rspg.svg?branch=master)](https://travis-ci.com/linyinfeng/rspg)

A simple **experimental** rust parser generator library for syntactic analysis learning.

```toml
[dependencies]
rspg = { git = "https://github.com/linyinfeng/rspg" }
```

## Status

- [x] Grammar
- [x] FIRST Set
- [x] FOLLOW Set
- [ ] LL(1) Parser
- [ ] LL(1) Table Generator
- [ ] SLR(1) Parser
- [ ] SLR(1) Table Generator
- [x] LR(1) Parser
- [x] LR(1) Table Generator
- [ ] Tests
- [ ] Documentation
- [ ] ...

## Documentation

The API documentation of the master branch is hosted on GitHub Pages. Please refer to https://www.linyinfeng.com/rspg.

Or build and view documents with [Cargo](https://github.com/rust-lang/cargo).

```bash
cargo doc --open
```