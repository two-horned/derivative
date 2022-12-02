#lang racket
;; TODO: EINEN WEG FINDEN, "JEDE" EINGABE ZU AKZEPTIEREN
;; Z.B - ((+ x x x) x)
;;     - ((* x x x) x)

; Dokumentation:
; Wie kann ich nach X ableiten?
; -> (ableiten '(x x)) -> 1
; -> (ableiten '(1 x)) -> 0
; -> (ableiten '(y x)) -> 0
;
; Wie kann ich eine Potenz von X ableiten?
; -> (ableiten '((* x x) x))             -> '(+ x x)                          [= 2x]
; -> (ableiten '((* (* x x) x) x))       -> '(+ (* (+ x x) x) (* x x))        [= 3x^2]
; -> (ableiten '((exp (* 2 (log x))) x)) -> '(* (/ 2 x) (exp (* 2 (log x))))  [= 2x]
;
; Wie kann ich eine Wurzel von X ableiten?
; -> (ableiten '((exp (/ (log x) 2)) x)) -> '(* (/ (* 2 x)) (exp (/ (log x) 2)))  [= 1/2 * x^-1/2]
; -> (ableiten '((exp (/ (log x) 3)) x)) -> '(* (/ (* 3 x)) (exp (/ (log x) 3)))  [= 1/3 * x^-1/3]
;
; Funktioniert die Summenregel? Ja.
; -> (ableiten '((+ x x) x) -> '(+ 1 1)
; -> (ableiten '((- x x) x) -> 0
;
; Funktioniert die Produktregel? Ja.
; -> (ableiten '((* x x) x)  -> '(+ x x)
; -> (ableiten '((/ x x) x)) -> '(+ (/ x) (* x (- (/ (* x x))))) [= 0]
;
; Wie kann ich Exponentialfunktionen ableiten?
; -> (ableiten '((exp x) x))             -> '(exp x)
; -> (ableiten '((exp (* x (log 2))) x)) -> '(* (log 2) (exp (* x (log 2))))  [= log(2)* e^(x*log(2))]
;
; Wie kann ich Sinusfunktionen ableiten?
; -> (ableiten '((sin x) x)) -> '(cos x)
;
; Wie kann ich Cosinusfunktionen ableiten?
; -> (ableiten '((cos x) x)) -> '(- (sin x))
;
; Wie kann ich Logarithmen ableiten?
; -> (ableiten '((log x) x)) -> '(/ x)


(define differ
  (lambda (x)
    (match x
      ;; d/dx x = 1
      [`(,x ,x) 1]
      ;; d/dx 1/x = -2/x^2
      [`((/ ,v) ,x) `(- (/ ,(differ `(,v ,x)) (* ,v ,x)))]
      ;; d/dx sin u = d/dx u cos u
      [`((sin ,u) ,x) `(* ,(differ `(,u ,x)) (cos ,u))]
      ;; d/dx cos u = d/dx u -sin u
      [`((cos ,u) ,x) `(- (* ,(differ `(,u ,x)) (sin ,x)))]
      ;; d/dx e^x = e^x
      [`((exp ,u) ,x) `(* ,(differ `(,u ,x)) (exp ,u))]
      ;; d/dx log x = 1/x
      [`((log ,u) ,x) `(/ ,(differ `(,u ,x)) ,u)]
      ;; d/dx (ux + vx) = d/dx ux + d/dx vx 
      [`((+ ,u ,v) ,x) `(+ ,(differ `(,u ,x)) ,(differ `(,v ,x)))]
      ;; d/dx (ux - vx) = d/dx ux - d/dx vx
      [`((- ,u ,v) ,x) `(- ,(differ `(,u ,x)) ,(differ `(,v ,x)))]
      ;; d/dx (ux * vx) = d/dx ux * v + u * d/dx v
      [`((* ,u ,v) ,x) `(+ (* ,(differ`(,u ,x)) ,v) (* ,u ,(differ`(,v ,x))))]
      ;; d/dx (ux / vx) = d/dx ux / v + u * d/dx 1/v
      [`((/ ,u ,v) ,x) `(+ (/ ,(differ`(,u ,x)) ,v) (* ,u ,(differ`((/ ,v) ,x))))]
      ;; d/dx c = 0
      [_ 0])))

; Diese Lambda-Funktion realisiert Vereinfachungsregeln bei
; expliziten Rechnungen mit Primitiven Operanten
(define simplify
  (lambda(x)
    (match x
      [`(sin ,x) (simplify-no-sign `(sin ,(simplify `,x)))]
      [`(cos ,x) (simplify-no-sign `(cos ,(simplify `,x)))]
      [`(exp ,x) (simplify-no-sign `(exp ,(simplify `,x)))]
      [`(log ,x) (simplify-no-sign `(log ,(simplify `,x)))]
      [`(- ,a) (simplify-no-sign `(- ,(simplify `,a)))]
      [`(+ ,b ,c) (simplify-no-sign `(+ ,(simplify `,b) ,(simplify `,c)))]
      [`(- ,b ,c) (simplify-no-sign `(- ,(simplify `,b) ,(simplify `,c)))]
      [`(* ,b ,c) (simplify-no-sign `(* ,(simplify `,b) ,(simplify `,c)))]
      [`(/ ,b ,c) (simplify-no-sign `(/ ,(simplify `,b) ,(simplify `,c)))]
      [x (simplify-no-sign x)])))

; Diese Lambda-Funktion realisiert Vereinfachungsregeln
(define simplify-no-sign
  (lambda(x)
    (match x
      ; Null geteilt durch Null darf man nicht.
      [`(/ 0 0) "Divided through Zero"]
      [`(* (cos (* ,x ,x)) (sin (* ,x ,x))) 1]
      [`(cos 0) 1]
      [`(sin 0) 0]
      [`(exp 0) 1]
      [`(log 1) 0]
      ; Irgendwas mal Null ist Null.
      [(or `(* 0 ,x) `(* ,x 0)) 0]
      ; Eins ist Neutrales Element der Multiplikation.
      [(or `(* 1 ,x) `(* ,x 1)) (simplify x)]
      ; Null ist Neutrales Element der Addition.
      [(or `(+ 0 ,x) `(+ ,x 0)) (simplify x)]
      ; Subtraktion von Null macht nichts.
      [`(- ,x 0) (simplify x)]
      ; Null minus etwas ist minus-etwas.
      [(or `(* -1 ,x) `(* ,x -1) `(- 0 ,x)) (simplify `(- ,x))]
      ; Ist immer Null
      [`(- ,x ,x) 0]
      ; Null hat kein Vorzeichen
      ['(- 0)` 0]
      ; 1 Durch etwas ist einfach der Kehrbruch
      [`(/ 1 ,x) `(/ ,(simplify `,x))]
      ; Einfach die Eins
      [`(/ ,x ,x) 1]
      ; Null durch etwas ist Null.
      [`(/ 0 ,x) 0]
      ; Bruchrechenregeln
      [`(* ,y (/ ,x)) (simplify `(/ ,(simplify `,y) ,(simplify `,x)))]
      [`(* (/ ,x) ,y) (simplify `(/ ,(simplify `,y) ,(simplify `,x)))]
      [`(/ ,y (/ ,x)) (simplify `(* ,(simplify `,x) ,(simplify `,y)))]
      [`(/ (/ ,y) ,x) (simplify `(/ (* ,(simplify `,x) `,(simplify ,y))))]
      [`(* (/ ,x) ,y) (simplify `(/ ,(simplify `,y) ,(simplify `,x)))]
      [x x])))

; Diese Lambda Funktion erleichtert dir das Ableiten
(define derivative
  (lambda (x)
    (simplify
     (differ x))))
