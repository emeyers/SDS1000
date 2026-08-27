# Determine what kind of answer a poll expects

Polls are multiple choice unless the instructor set the poll's `choices`
to the single word `"Numeric"` or `"String"`.

## Usage

``` r
poll_response_type(poll)
```

## Arguments

- poll:

  A parsed poll returned by the poll web app.

## Value

One of `"choice"`, `"numeric"`, or `"text"`.
