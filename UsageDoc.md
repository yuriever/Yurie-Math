# Usage Documentation Guidelines

This document defines the formatting conventions for `Usage` messages in Mathematica paclets.


## Example

``` wl
fun::usage =
    "fun[parameter]: try to process the given parameter."<>
    "\n"<>
    "Hint: may produce unexpected results in edge cases."<>
    "\n"<>
    "Example: fun[x] -> result."<>
    "\n"<>
    "Default[parameter]: defaultValue.";
```


## Formatting Rules

All `Usage` messages must follow these requirements:

* Start with `functionName::usage =` on the first line.

* Indent message content with four spaces beginning on the second line.

* Write each content line as a string `"..."` containing one of the supported field types (see below).

* For multi-line messages, use this pattern:
    * End each line except the last with `<>`
    * Connect lines using `"\n"<>`
    * Terminate the final line with `;`

* End field content after the colon with periods.

* Fields may appear in any order.

* All fields are optional, though function signature is recommended for clarity.

* When rewriting existing usage messages, preserve only the fields that are already present.


## Supported Field Types

* **`signature: description`** — Function signature with description of typical usage patterns.

* **`Sketch: definition`** — Brief function definition (e.g., `fun1 + fun2` where `fun1` and `fun2` are other functions).

* **`Hint: explanation`** — Additional guidance on usage.

* **`Example: example`** — Code example demonstrating typical usage.

* **`Def[parameter]: description`** — Detailed description of a function parameter.

* **`Value[parameter]: values`** — Enumeration of allowed values for a function parameter.

* **`Default[parameter]: value`** — Default value for a function parameter.

