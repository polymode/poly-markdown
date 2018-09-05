---
layout: post
date: 2015-08-13 11:35:25 EST
---

Polymode Markdown Test Cases
============================

## Intro

Fragments taken from `markdown-mode/tests/test-cases.text` and interspersed with
various code blocks.

1. Lists
--------

Unordered lists:

- This is a `el (defvar bullet-point "bullet-point")`.
- This is a `ada "Sub bullet point"`.
- This is `python ["*" + x + "*" for x in ["another", "bullet", "point"]]`

```fortran
*     euclid.f (FORTRAN 77)
*     Find greatest common divisor using the Euclidean algorithm
      PROGRAM EUCLID
      PRINT *, 'A?'
      READ *, NA
      IF (NA.LE.0) THEN
         PRINT *, 'A must be a positive integer.'
         STOP
      END IF
      PRINT *, 'B?'
      READ *, NB
      IF (NB.LE.0) THEN
         PRINT *, 'B must be a positive integer.'
         STOP
      END IF
      PRINT *, 'The GCD of', NA, ' and', NB, ' is', NGCD(NA, NB), '.'
      STOP
      END
```
