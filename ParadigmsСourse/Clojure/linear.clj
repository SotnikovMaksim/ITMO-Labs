(defn v+ [v1 v2]
  (mapv + v1 v2))

(defn v- [v1 v2]
  (mapv - v1 v2))

(defn v* [v1 v2]
  (mapv * v1 v2))

(defn vd [v1 v2]
  (mapv / v1 v2))

(defn scalar [v1 v2]
  (apply + (v* v1 v2)))

(defn det2 [v1 v2 v3 v4]
  (- (* v1 v4) (* v2 v3)))

(defn vect [[a1 a2 a3] [b1 b2 b3]]
  (vector (det2 a2 a3 b2 b3)
           (- (det2 a1 a3 b1 b3))
           (det2 a1 a2 b1 b2)))

(defn v*s [v s]
  (mapv * v (vec (repeat (count v) s))))

(defn transpose [m]
  (apply mapv vector m))

(defn m*m [m1 m2]
  (def tm2 (transpose m2))
  (mapv (fn [m1row] (mapv #(scalar m1row %) tm2)) m1))

(defn m+ [m1 m2]
  (mapv v+ m1 m2))

(defn m- [m1 m2]
  (mapv v- m1 m2))

(defn m* [m1 m2]
  (mapv v* m1 m2))

(defn md [m1 m2]
  (mapv vd m1 m2))

(defn m*s [m s]
  (mapv #(v*s % s) m))

(defn m*v [m v]
  (mapv #(scalar % v) m))

(defn c+ [m1 m2]
  (mapv m+ m1 m2))

(defn c- [m1 m2]
  (mapv m- m1 m2))

(defn c* [m1 m2]
  (mapv m* m1 m2))

(defn cd [m1 m2]
  (mapv md m1 m2))

(defn x+ [x1 x2]
  (if (number? (first x1))
    (v+ x1 x2)
    (mapv (fn[x1r x2r] (if (vector? (first x1r)) (x+ x1r x2r) (v+ x1r x2r))) x1 x2)))

(defn x- [x1 x2]
  (if (number? (first x1))
    (v- x1 x2)
    (mapv (fn[x1r x2r] (if (vector? (first x1r)) (x- x1r x2r) (v- x1r x2r))) x1 x2)))

(defn x* [x1 x2]
  (if (number? (first x1))
    (v* x1 x2)
    (mapv (fn[x1r x2r] (if (vector? (first x1r)) (x* x1r x2r) (v* x1r x2r))) x1 x2)))

(defn xd [x1 x2]
  (if (number? (first x1))
    (vd x1 x2)
  (mapv (fn[x1r x2r] (if (vector? (first x1r)) (if (number? (first x1r)) (vd x1 x2) (xd x1r x2r)) (vd x1r x2r))) x1 x2)))
