# parpy

Generating Python using Racket to get some actual syntactic abstraction


For now, this is just a bin with a few hundred lines of Racket that can be
used to assemble Python code. This code hasn't yet emerged from the
primordial swamp of what-abstractions-are-actually-useful?

Here's an example of currently working code:

```
#lang racket

(require parpy)

(py-file
 "/tmp/maxval.py"
 '((import unittest)
   (%% "A FloatBinTree is one of
- None, or
- Node(float, FloatBinTree, FloatBinTree)")
   (class Node (val left right))
   (%% "A MaybeFloat is one of
- None, or
- a float")
   (%%
    "FloatBinTree -> MaybeFloat
Return the largest number in a FloatBinTree"
    (define (maxval t)
      (cond [(== t None)
             None]
            [else
             (larger t.val (larger (maxval t.left) (maxval t.right)))])))
   (%%
    "MaybeFloat MaybeFloat -> MaybeFloat
return the larger of two maybefloats, or None if they're both None"
    (define (larger a b)
      (cond [(== a None) b]
            [(== b None) a]
            [(< a b) b]
            [else a])))
   (testclass
    MaxvalTests
    [maxval
     (self.assertEqual (maxval None) None)
     (self.assertEqual (maxval (Node 13 (Node 6 None None)
                                     (Node 20 (Node 79 None None) None)))
                       79.0)])
   (unittest.main)))

(run-py-file
 "/tmp/maxval.py")
 ```
 
