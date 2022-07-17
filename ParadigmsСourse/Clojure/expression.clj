(load-file "parser.clj")

(defn make-op [op]
  (fn [& args]
    (fn [m] (apply op (map #(% m) args)))))

(def add (make-op +))

(def subtract (make-op -))

(def multiply (make-op *))

(def negate (make-op -))

(defn divide [& args]
  (fn[m]
    (let [args (mapv #(% m) args)]
      (if (or (and (= (count args) 1) (== (first args) 0))(boolean (some #(== 0.0 %) (rest args))))
        ##Inf
        (apply / args)))))

(defn constant [v]
  (fn[_] v))

(defn variable [v]
  (fn[m] (get m v)))

(defn sumexp [& args]
  (fn[m] (apply + (map #(Math/exp (% m)) args))))

(defn exp [arg]
  (fn[m] (Math/exp (arg m))))

(defn ln [arg]
  (fn[m] (Math/log (arg m))))

(defn softmax [& args]
  (fn[m] (/ (Math/exp ((first args) m)) ((apply sumexp args) m))))

(def operationsFunction {'+ add, '- subtract, '* multiply, '/ divide, 'negate negate, 'softmax softmax, 'sumexp sumexp, 'exp exp, 'ln, ln})

(defn parseToFunction [expr]
  (cond
    (number? expr) (constant expr)
    (symbol? expr) (variable (name expr))
    :else (apply (get operationsFunction (first expr)) (map parseToFunction (rest expr)))))

(defn parseFunction [expr]
  (let [unparsed (read-string (clojure.string/trim expr))]
    (parseToFunction unparsed)))

; ====================================================================================================

(definterface Proto
  (^String toString [])
  (^String toStringSuffix [])
  (^Object diff [var])
  (^Number evaluate [varMap]))

(declare Add Subtract Multiply Divide Constant Negate Sumexp Softmax Exp)

(defn diffTable [name args var]
  (case name
    "negate" (apply Negate (map #(.diff % var) args))
    "+" (apply Add (map #(.diff % var) args))
    "-" (apply Subtract (map #(.diff % var) args))
    "*" (apply Add (map #(apply Multiply %) (map (fn [ext] (map #(if (= % ext) (.diff % var) %) args)) args)))
    "sumexp" (apply Add (map #(.diff (Exp %) var) args))
    "softmax" (.diff (Divide (Exp (first args)) (apply Sumexp args)) var)
    "exp" (Multiply (.diff (first args) var) (Exp (first args)))
    "ln" (Multiply (.diff (first args) var) (Divide (Constant 1) (first args)))
    "/" (let [num (if (= 1 (count args)) (Constant 1) (first args)) denom (if (= 1 (count args)) (first args) (apply Multiply (rest args)))]
          (Divide (Subtract (Multiply (.diff num var) denom) (Multiply num (.diff denom var))) (Multiply denom denom)))))

(defn evalTable [name op varMap args]
  (case name
    "simple" (apply op (map #(.evaluate % varMap) args))
    "sumexp" (apply + (map #(Math/exp (.evaluate % varMap)) args))
    "softmax" (/ (Math/exp (.evaluate (first args) varMap)) (.evaluate (apply Sumexp args) varMap))
    "exp" (Math/exp (.evaluate (first args) varMap))
    "ln" (Math/log (.evaluate (first args) varMap))
    "div" (let [args (mapv #(.evaluate % varMap) args)]
            (if (or (and (= (count args) 1) (== (first args) 0)) (boolean (some #(== 0.0 %) (rest args))))
              ##Inf
              (apply / args)))))

(deftype Expression [op opName opType args]
  Proto
  (toString [this] (str "(" opName " " (clojure.string/join " " (map #(.toString %) args)) ")"))
  (toStringSuffix [this] (str "(" (clojure.string/join " " (map #(.toStringSuffix %) args)) " " opName ")"))
  (diff [this var] (diffTable opName args var))
  (evaluate [this varMap] (evalTable opType op varMap args)))

(deftype Const [value]
  Proto
  (toString [this] (str value))
  (toStringSuffix [this] (str value))
  (diff [this var] (Const. 0))
  (evaluate [this varMap] value))

(deftype Var [name]
  Proto
  (toString [this] name)
  (toStringSuffix [this] name)
  (diff [this var] (let [varName (clojure.string/lower-case (first name))] (if (= varName var) (Const. 1) (Const. 0))))
  (evaluate [this varMap] (get varMap (clojure.string/lower-case (first name)))))

(defn diff [expr var]
  (.diff expr var))

(defn Add [& args]
    (Expression. + "+" "simple" args))

(defn Subtract [& args]
  (Expression. - "-" "simple" args))

(defn Multiply [& args]
  (Expression. * "*" "simple" args))

(defn Divide [& args]
  (Expression. / "/" "div" args))

(defn Sumexp [& args]
  (Expression. nil "sumexp" "sumexp" args))

(defn Softmax [& args]
  (Expression. nil "softmax" "softmax" args))

(defn Negate [& arg]
  (Expression. - "negate" "simple" arg))

(defn Exp [& arg]
  (Expression. nil "exp" "exp" arg))

(defn Ln [& arg]
  (Expression. nil "ln" "ln" arg))

(defn Constant [value]
  (Const. value))

(defn Variable [name]
  (Var. name))

(defn evaluate [expr varMap]
  (.evaluate expr varMap))

(defn toString [expr]
  (.toString expr))

(defn toStringSuffix [expr]
  (.toStringSuffix expr))

(def operationsObject {'+ Add, '- Subtract, '* Multiply, '/ Divide, 'negate Negate, 'sumexp Sumexp, 'softmax Softmax, 'exp Exp, 'ln Ln})
(def nameToFunc {"+" Add, "-" Subtract, "*" Multiply, "/" Divide})

(defn parseToObject [expr]
  (cond
    (number? expr) (Const. expr)
    (symbol? expr) (Var. (name expr))
    :else (apply (get operationsObject (first expr)) (map parseToObject (rest expr)))))

(defn parseObject [expr]
  (let [unparsed (read-string (clojure.string/trim expr))]
    (parseToObject unparsed)))

; ====================================================================================================

(def *all-chars (mapv char (range 0 128)))

(def *digit (+char (apply str (char 45) (char 46) (filter #(Character/isDigit %) *all-chars))))

(def *space (+char (apply str (filter #(Character/isWhitespace %) *all-chars))))

(def *letter (+char (apply str (filter #(Character/isLetter %) *all-chars))))

(def *ws (+ignore (+star *space)))

(def *number (+map (comp Constant read-string) (+str (+plus *digit))))

(def *variable (+map (comp Variable str) (+str (+plus *letter))))

(def *op (+map (comp nameToFunc str) (+char "+-*/")))

(def *negOp (+seqf (constantly Negate) (+char "n") (+char "e") (+char "g") (+char "a") (+char "t") (+char "e")))

(declare *unary)
(def *binary (+seqf #(apply (last %&) (butlast %&)) *ws
                    (+ignore (+char "(")) *ws
                    (+or *number *variable (delay *binary) (delay *unary)) *ws
                    (+or *number *variable (delay *binary) (delay *unary)) *ws
                    *op *ws (+ignore (+char ")")) *ws))
(def *unary (+seqf #(apply (last %&) (butlast %&)) *ws
                   (+ignore (+char "(")) *ws
                   (+or *number *variable (delay *binary) (delay *unary)) *ws
                   *negOp *ws (+ignore (+char ")")) *ws))
(def parseObjectSuffix (+parser (+seqn 0 *ws (+or *number *variable *binary *unary) *ws)))