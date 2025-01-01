# JSON Scheme Generator

This is a small commandline tool, which converts a JSON output to
the corresponding JSON scheme.

For example 

```json
{
    "a": 1,
    "b": [2, null, true],
    "c": [
        {
            "d": 1,
            "e", true
        },
        {
            "f": false
        },
        {
            "f": null
        }
    ],
    "d" [
        [
            {
                "a": false
            },
            {
                "a": null
            }
        ],
        [
            {
                "a": false
            }
        ]
    ]
}

```

returns

```json-scheme
{
    "a": number,
    "b": [
        null | boolean | number
    ],
    "c": [
        {
                "d": number,
                "e": boolean
            } | {
                "f": null | boolean
            }
    ],
    "d": [
        [
            {
                "a": null | boolean
            }
        ]
    ]
}
```

## Installation

You can install this script with `cabal install`.

## Usage

```
Usage: json-scheme-gen [-p|--prettify] ((-f|--file FILENAME) | --stdin)

  Returns the JSON Scheme of a JSON input
```
