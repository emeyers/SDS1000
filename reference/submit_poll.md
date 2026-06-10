# Submit a poll answer

Students use this function to submit their answer to a poll question.
The instructor will provide a `poll_name` for each question.

## Usage

``` r
submit_poll(poll_name, answer)
```

## Arguments

- poll_name:

  Character. The poll identifier provided by your instructor (e.g.
  `"q1"`).

- answer:

  Your answer to the poll question (character or numeric).

## Value

Invisibly returns `NULL`. Called for its side effect of submitting your
answer.
