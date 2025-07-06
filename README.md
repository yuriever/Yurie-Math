# Yurie/Math

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Wolfram Language](https://img.shields.io/badge/Wolfram%20Language-14.2%2B-red.svg)](https://www.wolfram.com/language/)

Math utilities for Wolfram Language.


## Features


### Core modules

* `Quest.wl` - logical functions

* `OperatorForm.wl` - operator form of built-in functions

* `DLMF.wl` - partial implementation of [DLMF](https://dlmf.nist.gov/)

* `Gamma.wl` - Gamma functions

* `Hyper.wl` - hypergeometric-related functions

* `Diff.wl` - partial derivative, integration, and summation

* `Simplify.wl`, `SimplifyUnsafe.wl` - enhanced symbolic simplification routines

* `Relation.wl` - relation generators for simplification

* `Matrix.wl` - matrix utilities

* `Dye.wl` - mathematical expression display utilities

* `Label.wl` - variable labeling utilities


### Specialized modules

* `Lie.wl` - data of simple Lie algebras


## Usage

1. Clone or download this repository

2. Move the entire folder to the user paclet directory:

   ```wl
   $UserBasePacletsDirectory
   ```

3. Rebuild the paclet data:

   ```wl
   PacletDataRebuild[]
   ```

4. Load the paclet

    ```wl
    Needs["Yurie`Math`"]

    Needs["Yurie`Math`Lie`"]
    ```


### Uninstallation

```wl
PacletUninstall["Yurie/Math"]
```


### Installation checking

```wl
PacletFind["Yurie/Math"]
```


## Development


### Testing

The paclet includes comprehensive test suites in the `Test/` directory. Test source notebooks are available in `TestSource/` for interactive development.


### Documentation

Usage documentation guidelines are provided in `Dev/UsageDoc.md`. All functions follow consistent documentation standards with proper usage messages.
