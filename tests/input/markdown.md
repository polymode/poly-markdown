
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

Ordered lists

1. This is an ordered list
2. With a second element.
44. And a forty-fourth element.
3. Remember, Markdown doesn't care which number you use.

```perl
# Scan a file and print all the URL's it links to.
sub scan {
    my ($fn) = @_;
    
    open(IN, $fn) or return 0;
    
    # Go through each line in the file.
    while(<IN>) {
        # Repeatedly match URLs in the line.  Each one is removed by
        # replacing it with the empty string.  The loop body will execute
        # once for each match/replace, and prints the URL part of the
        # matched text.
        while(s/<\s*A\s+[^>]*HREF\s*\=\s*"([^"]+)"//i) {
            print "   $1\n";
        }
    }
    
    close IN;
    return 1;
}
```
List items with bold and italic

> * This is a list item *in italics*, just a test.
> * *List item in italics.*
> * This is a list item **in bold**, just a test.
> * **List item in bold.**

Bold and italic phrases at the beginning of lines:

```el
(defmacro thread-last (&rest forms)
  "Thread FORMS elements as the last argument of their successor.
Example:
    (thread-last
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
Is equivalent to:
    (+ 40 (- (/ 25 (+ 20 5))))
Note how the single `-' got converted into a list before
threading."
  (declare (indent 1) (debug thread-first))
  `(internal--thread-argument nil ,@forms))
```

*not a list*
**also not a list**


2. Blockquotes
--------------

> this is a test
> of the blockquote mechanism

```pascal

type
   pNode = ^Node;
   Node = record
             a :integer;
             b : char;
             c : pNode  {extra semicolon not strictly required}
          end;
var
   NodePtr : pNode;
   IntPtr  : ^integer;
   
```


3. Two Inline Links on One Line
-------------------------------

I did notice a minor bug. if there are two inline links in the same line, e.g.
[foo](bar) baz [foo](bar), it colors the text between the links (baz) as well.


4. Empty Inline Links
---------------------

[]()
[](asdf)
[asdf]()

```python
def insertion_sort_bin(seq):
    for i in range(1, len(seq)):
        key = seq[i]
        # invariant: ``seq[:i]`` is sorted        
        # find the least `low' such that ``seq[low]`` is not less then `key'.
        #   Binary search in sorted sequence ``seq[low:up]``:
        low, up = 0, i
        while up > low:
            middle = (low + up) // 2
            if seq[middle] < key:
                low = middle + 1             
            else:
                up = middle
        # insert key at position ``low`` // no-indent-test
        seq[:] = seq[:low] + [key] + seq[low:i] + seq[i + 1:]
```

## 5. Bold and Italics on the Same Line

**foo and doo** or *ziddle zop*

```el

(defun delete-dups (list)
  "Destructively remove `equal' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list.
Of several `equal' occurrences of an element in LIST, the first
one is kept."
  (let ((l (length list)))
    (if (> l 100)
        (let ((hash (make-hash-table :test #'equal :size l))
              (tail list) retail)
          (puthash (car list) t hash)
          (while (setq retail (cdr tail))
            (let ((elt (car retail)))
              (if (gethash elt hash)
                  (setcdr tail (cdr retail))
                (puthash elt t hash)
                (setq tail retail)))))
      (let ((tail list))
        (while tail
          (setcdr tail (delete (car tail) (cdr tail)))
          (setq tail (cdr tail))))))
  list)

```

Indentation of modes with simple `indent-line-function` like `indent-relative`

```sql
SELECT * FROM table;
SELECT * FROM table;
SELECT * FROM table;
```

The end.
