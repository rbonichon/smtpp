(set-logic QF_LIA)
; success

(declare-fun w () Int)
; success (assert (> z x))
(declare-fun x () Int)
; success (check-sat)
; unsat
(declare-fun y () Int)

; (:time 0.01 :memory 0.2)
(declare-fun z () Int)

(assert (> x y))
; success
(assert (> y z))
; success
(check-sat)


(set-option :print-success false)
(push 1)
(assert (> z z))
(check-sat)

(get-info :all-statistics)
; success (get-info :all-statistics)

(pop 1)
; success (pop 1)
(push 1)
(check-sat)
; sat

(exit)