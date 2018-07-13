#lang teachlog

(relation has-type 3)

;; Γ
(data mt)
(data extend 3)

;; Numbers
(data Num)
(data zero)
(data one)
(:- (has-type Γ zero Num))
(:- (has-type Γ one Num))

;; Addition
(data plus 2)
(:- (has-type Γ (plus lhs rhs) Num)
    (has-type Γ lhs Num)
    (has-type Γ rhs Num))

;; Booleans
(data Bool)
(data true)
(data false)
(:- (has-type Γ true Bool))
(:- (has-type Γ false Bool))

;; If
(data if 3)
(:- (has-type Γ (if Test Then Else) Tau)
    (has-type Γ Test Bool)
    (has-type Γ Then Tau)
    (has-type Γ Else Tau))

;; Functions & Variables
(data Arr 2)
(data var 1)
(data fun 2)
(data app 2)

(:- (has-type Γ (fun Var Body) (Arr Dom Rng))
    (has-type (extend Var Dom Γ) Body Rng))
(:- (has-type Γ (app Fun Arg) Rng)
    (has-type Γ Fun (Arr Dom Rng))
    (has-type Γ Arg Dom))

(:- (has-type (extend V1 T1 Γ) (var V1) T1))
(:- (has-type (extend V1 T1 Γ) (var V2) T2)
    (has-type Γ (var V2) T2))

;; Examples

(? (has-type mt zero Num))
(? (has-type mt (if true one zero) Num))
(? (has-type mt (if false one true) Num))

(? (has-type mt true T))
(? (has-type mt (if true one zero) T))

(? (has-type mt T Num))

(? (has-type mt (fun 'x (if (var 'x) zero true)) T))
(? (has-type mt (fun "x" (if (var "x") one zero)) T))

(? (has-type mt
             (app (fun "x" (if (var "x") one zero))
                  true)
             T))

(? (has-type mt (fun "x" (var "x")) T))

(? (has-type mt
             (app (fun "id"
                       (if (app (var "id") false)
                         (app (var "id") true)
                         (app (var "id") true)))
                  (fun "x" (var "x")))
             T))

(? (has-type mt
             (app (fun "id"
                       (if (app (var "id") false)
                         (app (var "id") one)
                         (app (var "id") zero)))
                  (fun "x" (var "x")))
             T))
