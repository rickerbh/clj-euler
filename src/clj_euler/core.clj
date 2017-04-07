(ns clj-euler.core
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as math]
            [clojure.core.reducers :as r]))

(defn multiple-of
  [n x]
  (= 0 (mod n x)))

(defn is-multiple?
  [x]
  (or
   (multiple-of x 3)
   (multiple-of x 5)))

(defn p01
  []
  (let [numbers (range 1 1000)
        multiples (filter is-multiple? numbers)]
    (println (reduce + 0 multiples))))

(defn fib
  ([]
   (fib 1 1))
  ([a b]
   (lazy-seq (cons a (fib b (+ a b))))))

(defn p02
  []
  (let [numbers (take-while (partial > 4000000) (fib))
        evens (filter even? numbers)]
    println (reduce + 0 evens)))

(defn lazy-primes
  ([] (cons 2 (lazy-primes 3 [])))
  ([current known-primes]
   (if (not-any? #(zero? (mod current %)) 
                 (take-while #(<= (*' % %) current) known-primes))
     (lazy-seq (cons current
                     (lazy-primes 
                      (+' current 2) 
                      (conj known-primes current))))
     (recur (+' current 2) known-primes))))

(defn prime-factors [num]
  (loop [num num
         acc [1]
         primes (lazy-primes)]
    (if (= num 1)
      acc
      (let [factor (first primes)]
        (if (= 0 (mod num factor))
          (recur (quot num factor) (conj acc factor) primes)
          (recur num acc (rest primes)))))))

(defn p03
  []
  (let [number 600851475143]
    (println (str "Checking for prime factors of " number ))
    (println (prime-factors number))))

(defn palindromic?
  [n]
  (let [sn (str n)
        reversed (reduce str "" (reverse sn))]
    (= sn reversed)))

(defn multiples
  [r s]
  (map
   (fn [x]
     (map #(* x %) s))
   r))

(defn p04
  []
  (let [three-digit-range (range 100 1000)]
    (->>
     (multiples three-digit-range three-digit-range)
     (flatten)
     ((partial filter palindromic?))
     (sort)
     (last))))

(defn divisible-by-range
  [n r]
  (= 0 (reduce + 0 (map #(mod n %) r))))

(defn p05
  ([]
   (p05 20))
  ([x]
   (loop [n x]
     (let [r (range 1 21)]
       (if (divisible-by-range n r)
         (println (str "Answer is " n))
         (recur (+ n 20)))))))

(defn square-of
  [n]
  (math/expt n 2))

(defn sum-of-squares
  [r]
  (reduce #(+ %1 (square-of %2)) r))

(defn square-of-sum
  [r]
  (->
   (reduce + 0 r)
   (square-of)))

(defn p06
  []
  (let [r (range 1 101)]
    (- (square-of-sum r) (sum-of-squares r))))

(defn p07
  ([]
   (p07 10001))
  ([n]
   (first (drop (- n 1) (lazy-primes)))))

(def p8number
  7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)

(defn str->int
  [s]
  (Integer/parseInt (re-find #"\d+" s))) ;; \A-?\d+ (also handles negative numbers

(defn str->bigint
  [s]
  (BigInteger. (re-find #"\d+" s) 10))

(defn p08
  ([]
   (p08 13))
  ([x]
   (let [windows (map #(map (fn [x] (str->int (str x))) %) (partition x 1 (str p8number)))] 
     (last (sort (map #(reduce * 1 %) windows))))))

(defn pythagorean-triplet?
  [t]
  (let [a (first t)
        b (second t)
        c (last t)]
    (and (< a b c)
         (= (+ (square-of a) (square-of b)) (square-of c)))))

(defn triplet-sums-to
  [sum t] 
  (= (apply + t) sum))

(defn p09
  ([]
   (p09 1000))
  ([n]
   (let [r (range 1 998) ;; real range needs to go to 997 + 1
         products (combo/cartesian-product r r r)]
     (apply * (first (filter #(and (triplet-sums-to n %) (pythagorean-triplet? %)) products ))))))

(defn p10
  ([]
   (p10 2000000))
  ([n]
   (reduce + 0 (take-while #(< % n) (lazy-primes)))))

(defn factors
  [n]
  (into (sorted-set)
        (mapcat (fn [x] [x (/ n x)])
                (filter #(zero? (rem n %)) (range 1 (inc (Math/sqrt n)))) )))

(defn triangle-numbers
  ([]
   (triangle-numbers 1))
  ([n]
   (let [next (inc n)
         result (reduce + (range 1 next))]
     (lazy-seq (cons result (triangle-numbers next))))))

(defn p12
  ([]
   (p12 500))
  ([n] 
   (->> (triangle-numbers)
        (map factors)
        (drop-while #(< (count %) n))
        (first)
        (last))))

(defn split-digits
  [n]
  (->>
   n
   (str)
   (partition 1 1)
   (flatten)
   (map #(str->int (str %)))))

(defn p13
  []
  (let [contents (slurp "resources/p013.txt")]
    (->
     contents
     (str/split #"\n")
     (->> (map str->bigint)
          (reduce +))
     (split-digits)
     (->> (take 10)
          (map str)
          (String/join "")))))

(defn- collatz-g
  [n] 
  (if (even? n)
    (let [x (/ n 2)]
      (if (= x 1)
        '(1)
        (cons x (collatz-g x))))
    (let [x (+ 1 (* 3 n))]
      (cons x (collatz-g x)))))

(defn collatz
  [n]
  (cons n (collatz-g n)))

(defn p14
  []
  (let [max 1000000]
    (->> max
         (range 1)
         (reduce (fn [acc n]
                   (let [c (collatz n)
                         size (count c)
                         acc-size (count (:collatz acc))]
                     (if (< (count (:collatz acc)) size) 
                       { :number n :collatz c }
                       acc)))
                 { :number 0 :collatz '()})
         :number)))

(defn p16
  ([]
   (p16 1000))
  ([e]
   (->>
    (math/expt 2 e)
    (split-digits)
    (apply +))))

(defn p20
  ([]
   (p20 100))
  ([n]
   (->>
    n
    (+ 1)
    (range 1)
    (reverse)
    (map bigint)
    (reduce *)
    (split-digits)
    (reduce +))))

(defn proper-divisors
  [n]
  (->> n
       (factors)
       (drop-last)))

(defn amicable-numbers?
  [n]
  (let [s1 (reduce + (proper-divisors n))
        s2 (reduce + (proper-divisors s1))]
    (and (= s2 n)
         (not= s1 s2))))

(defn p21
  []
  (let [max 10000]
    (->> max
         (range 1)
         (filter amicable-numbers?)
         (reduce +))))

(defn p22-name-score
  [name]
  (let [letters (seq (char-array name))]
    (reduce #(+ %1 (- (int %2) 64)) 0 letters)))

(defn p22
  []
  (let [contents (str/split (str/replace (slurp "resources/p022_names.txt") #"\"" "") #",")]
    (->> contents
         (sort)
         (map p22-name-score)
         (map-indexed #(* (+ 1 %1) %2))
         (reduce +))))

(defn divisors-sum
  [n]
  (->> n
       (proper-divisors)
       (reduce +)))

(defn perfect-number?
  [n]
  (= (divisors-sum n) n))

(defn deficient-number?
  [n]
  (< (divisors-sum n) n))

(defn abundant-number?
  [n]
  (> (divisors-sum n) n))

(defn natural-numbers
  ([]
   (natural-numbers 1))
  ([n]
   (lazy-seq (cons n (natural-numbers (inc n))))))

(defn abundant-numbers
  []
  (filter abundant-number? (natural-numbers)))

(defn contains-val?
  [coll val]
  (reduce #(if (= val %2) (reduced true) %1) false coll))

(defn p23
  []
  (let [max (inc 28123)
        abundant-range (take-while #(< % max) (abundant-numbers))
        summed-abundants (-> (into abundant-range abundant-range)
                             (combo/combinations 2)
                             (->> (map (partial apply +))) 
                             (set)
                             (sort))]
    (->> (filter #(not (contains-val? summed-abundants %)) (range 1 (inc max))) 
         (reduce +))))

(defn p24
  ([]
   (p24 (range 0 10) 1000000))
  ([r n]
   (take 1 (drop (dec n) (combo/permutations r)))))

(defn indexed-fib
  ([]
   (indexed-fib  1 (BigInteger/valueOf 1) (BigInteger/valueOf 1)))
  ([i a b]
   (lazy-seq (cons {:index i :value a} (indexed-fib (inc i) b (+ a b))))))

(defn p25
  ([]
   (p25 1000))
  ([d]
   (->> (indexed-fib)
        (drop-while #(< (count (str (:value %))) d))
        (first)
        (:index))))
