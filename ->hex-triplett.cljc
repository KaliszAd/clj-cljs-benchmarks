;; Convert to hex string using the built in function
;; Pad so the length is always 3.
(defn ->hex-triplett-padding
  [input]
  (letfn [(#?(:clj  ^String hex-group
              :cljs ^string hex-group) [num]
            (let [unpadded-hex #?(:clj  ^String (format "%x" num)
                                  :cljs ^string (.toString num 16))]
              (case (count unpadded-hex)
                1 (str "00" unpadded-hex)
                2 (str "0" unpadded-hex)
                unpadded-hex)))]
    (hex-group input)))

;; On Intel Core i7-1260P, OpenJDK 17, Debian 12 Bookworm AMD64
;; (crit/quick-bench (->hex-triplett-padding 15))
;; Evaluation count : 3414900 in 6 samples of 569150 calls.
;; Execution time mean : 210.473043 ns
;; Execution time std-deviation : 45.128104 ns
;; Execution time lower quantile : 178.059591 ns ( 2.5%)
;; Execution time upper quantile : 282.366970 ns (97.5%)
;; Overhead used : 5.930294 ns

;; Consider type definitions for Clojure/ type hints for ClojureScript
;; The form where type hint is before the argument vector does not work
;; with this reader macro. Probably need to wrap whole function.
(defn #?(:clj  ^String hex->char-as-str
         :cljs ^string hex->char-as-str)
  [num]
  (case num
    0 "0"
    1 "1"
    2 "2"
    3 "3"
    4 "4"
    5 "5"
    6 "6"
    7 "7"
    8 "8"
    9 "9"
    10 "a"
    11 "b"
    12 "c"
    13 "d"
    14 "e"
    15 "f"))

;; Convert to hex using bit-shift and bit masking + string concatenation.
(defn ->hex-triplett-bit-shift
  [input]
  (letfn [(#?(:clj  ^String bin-hex-group
              :cljs ^string bin-hex-group) [num]
            (when (< num 0xfff)
              (str (hex->char-as-str (bit-and 0xf (bit-shift-right num 8)))
                   (hex->char-as-str (bit-and 0xf (bit-shift-right num 4)))
                   (hex->char-as-str (bit-and 0xf num)))))]
    (bin-hex-group input)))

;; Or optionally for ClojureScript, add better type hints in string concatenation.
(defn ->hex-triplett-bit-shift
  [input]
  (letfn [(#?(:clj  ^String bin-hex-group
              :cljs ^string bin-hex-group) [num]
            (when (< num 0xfff)
              (str ^string (hex->char-as-str (bit-and 0xf (bit-shift-right num 8)))
                   ^string (hex->char-as-str (bit-and 0xf (bit-shift-right num 4)))
                   ^string (hex->char-as-str (bit-and 0xf num)))))]
    (bin-hex-group input)))

;; (crit/quick-bench (->hex-triplett-bit-shift 15))
;; Evaluation count : 6076602 in 6 samples of 1012767 calls.
;; Execution time mean : 81.918362 ns
;; Execution time std-deviation : 20.873176 ns
;; Execution time lower quantile : 63.328467 ns ( 2.5%)
;; Execution time upper quantile : 112.579955 ns (97.5%)
;; Overhead used : 5.930294 ns

;; In Chromium version 112.0.5615.138, without advanced compilation
; (simple-benchmark [] (->hex-triplett-bit-shift 15) 1e6)
; => 130 ms (with type hints for string concatenation 73 ms)
; (simple-benchmark [] (->hex-triplett-bit-shift 3844) 1e6)
; => 130 ms (with type hints for string concatenation 75 ms)

; (simple-benchmark [] (->hex-triplett-padding 15) 1e6)
; => 54 ms
; (simple-benchmark [] (->hex-triplett-padding 3844) 1e6)
; => 22 msec
