# rspg - Rust Simple Parser Generator

[![Build Status](https://travis-ci.com/linyinfeng/rspg.svg?branch=master)](https://travis-ci.com/linyinfeng/rspg)
[![Coverage Status](https://img.shields.io/coveralls/github/linyinfeng/rspg.svg)](https://coveralls.io/github/linyinfeng/rspg)
[![Documentation](https://img.shields.io/badge/doc-master-blue.svg)](https://www.linyinfeng.com/rspg)
[![License: MIT](https://img.shields.io/github/license/linyinfeng/rspg.svg)](https://github.com/linyinfeng/rspg/blob/master/LICENSE)

A simple **experimental** rust parser generator library for syntactic analysis learning.

```toml
[dependencies]
rspg = { git = "https://github.com/linyinfeng/rspg" }
```

## Status

- [x] Grammar
- [x] FIRST Set
- [x] FOLLOW Set
- [x] LR(1) Parser
- [x] LR(1) Table Generator
- [ ] Tests
- [ ] Documentation
- [x] Basic ability to use LR(1) parse result
- [ ] More elegant and faster compile time parser generator interface
- [ ] ...

## Documentation

The API documentation of the master branch is hosted on GitHub Pages. Please refer to https://www.linyinfeng.com/rspg.

Or build and view documents with [Cargo](https://github.com/rust-lang/cargo).

```bash
cargo doc --open
```