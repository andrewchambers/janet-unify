# Original reference:
# https://eli.thegreenplace.net/2018/unification/ 

(defn- uvar?
  [v]
  (and (symbol? v) (string/has-prefix? "?" v)))

(defn- has [ds k] (not (nil? (in ds k))))

(var unify2 nil)

(defn- occurs-check
  [v term subst]
  (cond
    (= v term)
      true
    (and (uvar? term) (has subst term))
      (occurs-check v (subst term) subst)
    (or (tuple? term) (struct? term) (table? term) (array? term))
      (loop [k :keys term]
        (occurs-check v (term k) subst))
    false))

(defn- unify-var
  [v x subst]
  (cond
    (has subst v)
      (unify2 (get subst v) x subst)
    (and (uvar? x) (has subst v))
      (unify2 v (subst v))
    (occurs-check v x subst)
      nil
    (table/to-struct (merge subst {v x}))))

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
        (and (struct? x) (struct? y))
        (and (table? x)  (table? y))
        (and (array? x)  (array? y)))
      (when (= (length x) (length y))
        (do
          (loop [k :keys x :when subst]
            (set subst (unify2 (x k) (y k) subst)))
          subst))
    nil))

(defn unify
  [x y &opt subst]
  (default subst {})
  (unify2 x y subst))
