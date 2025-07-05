# Usage Documentation Guidelines

This document outlines the formatting conventions for `Usage` messages in Mathematica paclets.


## Formatting Rules

All `Usage` messages must adhere to the following requirements:

* Begin with `functionName::usage =` on the first line

* Indent the usage message content with four spaces

* Structure each line as a string `"..."` containing one of the supported field types (see below)

* Fields may be arranged in any order as needed

* All fields are optional, though `signature` is recommended for clarity


## Supported Field Types

The following field formats are available:

* **`signature: description`** — Function signature with description showing typical usage patterns

* **`Sketch: definition`** — Brief definition or conceptual overview of the function

* **`Hint: explanation`** — Additional guidance on proper usage and best practices

* **`Example: example`** — Code example demonstrating typical applications

* **`Def[parameter]: description`** — Detailed description of a specific function parameter

* **`Value[parameter]: values`** — Enumeration of allowed values for a specific function parameter

* **`Default[parameter]: value`** — Default value for a specific function parameter


## Multi-line Usage Format

For usage messages that span multiple lines, follow this pattern:

* Begin with `functionName::usage =` on the first line

* Indent the usage message content with four spaces

* End each line except the last with `<>`

* Separate lines using `"\n"<>`

* Terminate the final line with `;`


### Example

``` wl
fun::usage =
    "fun[parameter]: ..."<>
    "\n"<>
    "Second line of usage message"<>
    "\n"<>
    "Final line of usage message";
```

