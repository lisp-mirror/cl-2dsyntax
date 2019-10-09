# cl-2dsyntax - Significant whitespace lisp

See http://ql-goheecus.goheeca.ga/lisp/cl-2dsyntax/

---

A factorial function:

```lisp
!defun fact !n
  if !zerop n
    1
    !* n !fact !1- n
```
