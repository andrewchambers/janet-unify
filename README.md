# janet-unify

A simple unification library.

Quick Example, A code simplifier engine

```
(use ./unify)

(def simplifications
  [['(= (length ?d) 0) '(empty? ?d)]
   ['(set ?n (+ ?n 1)) '(++ ?n)]])

(defn simplify-form
  [f]
  (var f f)
  (each [p r] simplifications
    (set f
      (if-let [subst (unify p f)]
        (apply-subst subst r)
        f)))
    f)

(defn fix-point
  [f v1]
  (def v2 (f v1))
  (if (deep= v1 v2)
    v2
    (fix-point f v2)))

(defn simplify
  [code]
  (fix-point |(prewalk simplify-form $) code))

(def code
  '(while (= (length ds) 0)
     (set v (+ v 1))
     (foo)))

(simplify code)
> (while (empty? ds) (++ v) (foo))
```


