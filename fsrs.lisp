;; Implementation of FSRS-6 (Free Spaced Repetition Scheduler)
;;
;; r: retrievability - probability of a successful recall of a card at the current time
;; s: stability - number of days for the retrievability to drop from 100% to 90%
;; d: difficulty - number between 1 and 10 describing its difficulty
;; g: grade - 1: again, 2: hard, 3: good, 4: easy
(let*
  ;; weights
  ([w [0.212 1.2931 2.3065 8.2956 6.4133 0.8334 3.0194 0.001 1.8722 0.1666 0.796 1.4835 0.0614 0.2629 1.6483 0.6014 1.8729 0.5425 0.0912 0.0658 0.1542]]
  [wi (fn (i) (vector-get w i))]
  [clamp (fn (x from to) (max (min x to) from))]
  ;; initial stability update
  [stability-initial (fn (g) (max (wi (- g 1)) 0.1))]
  ;; initial difficulty update
  [difficulty-initial (fn (g)
    (clamp
      (+ (- (wi 4) (exp (* (wi 5) (- g 1)))) 1)
      1.0
      10.0))]
  ;; stability after recall
  [stability-recall (fn (d s r g)
    (* s
      (+
        (*
          (exp (wi 8))
          (- 11 d)
          (expt s (- (wi 9)))
          (exp (- (* (wi 10) (- 1 r)) 1))
          (if (= g 2)
            (wi 15)
            (if (= g 4)
              (wi 16)
              1.0)))
      1)))]
  ;; stability after forgetting
  [stability-failure (fn (d s r)
    (*
      (wi 11)
      (expt d (- (wi 12)))
      (- (expt (+ s 1) (wi 13)) 1)
      (exp (* (- 1 r) (wi 14)))))]
  ;; difficulty after review
  [difficulty (fn (d g)
    (clamp
      (+
        d
        (*
          (*
            (- (wi 6))
            (- g 3))
          (/
            (- 10 d)
            9)))
      1.0
      10.0))]
  ;; interval for the next review based on the current r and stability
  [interval (fn (dr s)
    (*
      (/ s
        (-
          (expt 0.9 (- (/ 1 (wi 20))))
          1))
      (-
        (expt dr (- (/ 1 (wi 20))))
        1)))]
  [update-card (fn (card s d due)
    (map (fn (x)
      (cond
        [(eq? (car x) "s") (cons "s" s)]
        [(eq? (car x) "d") (cons "d" d)]
        [(eq? (car x) "t") (cons "t" (date))]
        [(eq? (car x) "due") (cons "due" (+ (date) (* due 24 60 60 1000)))]
        [#t x]))
      card))])
  ;; retrievability at the current moment for a given stability from t days ago
  (defun retrievability (fn (t s)
    (expt
      (+
        (*
          (- (expt 0.9 (- (/ 1 (wi 20)))) 1)
          (/ (/ (- (date) t) 1000 60 60 24) s))
        1)
      (- (wi 20)))))
  ;; performs a card review with a given grade
  (defun review (card g)
    (let*
      ([s (cdr-assoc "s" card)]
       [d (cdr-assoc "d" card)]
       [r (retrievability (cdr-assoc "t" card) s)])
      (cond
        [(= s 0.0)
          (begin
            (set! s (stability-initial g))
            (set! d (difficulty-initial g)))]
        [(= g 1)
          (begin
            (set! s (stability-failure d s r))
            (set! d (difficulty d g)))]
        [#t
          (begin
            (set! s (stability-recall d s r g))
            (set! d (difficulty d g)))])
      (update-card card s d (interval 0.9 s)))))

;; vim: set ts=2 sw=2 et :
