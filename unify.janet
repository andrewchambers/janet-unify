# Original reference:
# https://eli.thegreenplace.net/2018/unification/ 

(defn- uvar?
  [v]
  (and (symbol? v) (string/has-prefix? "?" v)))

(var unify2 nil)

(defn- occurs-check
  [v term subst]
  (cond
    (= v term)
      true
    (and (uvar? term) (in subst term))
      (occurs-check v (first (get subst term)) subst)
    (or (tuple? term) (array? term))
      (some |(occurs-check v $ subst) term)
    (or (table? term) (struct? term))
      (do
        (defn check1 [x] (occurs-check v x subst))
        (defn check2 [[x y]] (or (check1 x) (check1 y)))
        (some check2 (pairs term)))
    false))

(defn- unify-var
  [v x subst]
  (cond
    (in subst v)
      (unify2 (first (get subst v)) x subst)
    (and (uvar? x) (in subst v))
      (unify2 v (first (get subst v)))
    (occurs-check v x subst)
      nil
    (table/to-struct (merge subst {v [x]}))))

(varfn unify2
  [x y subst]
  (var subst subst)
  (cond
    (nil? subst)
      nil
    (deep= x y)
      subst
    (uvar? x)
      (unify-var x y subst)
    (uvar? y)
      (unify-var y x subst)
    (or (and (tuple? x)  (tuple? y))
        (and (array? x)  (array? y)))
      (when (= (length x) (length y))
        (loop [k :keys x :when subst]
          (set subst (unify2 (x k) (y k) subst)))
        subst)
    (or (and (table? x) (table? y))
        (and (struct? x) (struct? y)))
      (do
        (def ix (invert x))
        (def iy (invert y))
        (defn unify-keys [a b ib]
          (loop [[k v] :pairs a :when subst]
            (set subst
              (cond
                (b k)
                  (unify2 v (b k) subst)
                (ib v)
                  (unify2 k (ib v) subst)
                nil))))
          (unify-keys x y iy)
          (unify-keys y x ix)
        subst)
    nil))

(defn unify
  [x y &opt subst]
  (default subst {})
  (unify2 x y subst))

(defn lookup-uvar
  [subst v]
  (def r (get subst v))
  (if (nil? r)
    v
    (lookup-uvar subst (r 0))))

(defn apply-subst
  [subst form]
  (defn f
    [v]
    (if (uvar? v)
      (lookup-uvar subst v)
      v))
  (prewalk f form))

