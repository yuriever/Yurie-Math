# [Yurie/Math](https://github.com/yuriever/Yurie-Math)

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Wolfram Language](https://img.shields.io/badge/Wolfram%20Language-14.3%2B-red.svg)](https://www.wolfram.com/language/)

Math utilities for Wolfram Language.


## [Documentation](https://yuriever.github.io/symbolic/Yurie-Math/doc/)


## Features


### Core modules

* `Quest` - logical functions

* `OperatorForm` - operator form of built-in functions

* `DLMF` - partial implementation of [Digital Library of Mathematical Functions](https://dlmf.nist.gov/)

* `Gamma` - Gamma functions

* `Hyper` - hypergeometric-related functions

* `Diff` - partial derivative, integration, and summation

* `Simplify`, `SimplifyUnsafe` - enhanced symbolic simplification routines

* `Relation` - relation generators for simplification

* `Matrix` - matrix utilities

* `Dye` - mathematical expression display utilities

* `Label` - variable labeling utilities


### Specialized modules

* `Lie` - data of simple Lie algebras


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


* The paclet includes comprehensive test suites in the `Test/` directory. Test source notebooks are available in `TestSource/` for interactive development.

* Usage documentation guidelines are provided in `Dev/UsageDoc.md`. All functions follow consistent documentation standards with proper usage messages.
