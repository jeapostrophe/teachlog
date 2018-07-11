#lang teachlog

(relation has-type 3)

;; Types
(data num 0)
(data bool 0)
(data arr 2)

;; Programs
(data if 3)
(data var 1)
(data fun 2)
(data app 2)
(data true 0)
(data false 0)
(data zero 0)
(data one 0)
(data plus 2)

;; Environments
(data mt 0)
(data bind 3)

(:- (has-type Γ zero num))
(:- (has-type Γ one num))
(:- (has-type Γ true bool))
(:- (has-type Γ false bool))

(:- (has-type Γ (plus lhs rhs) num)
    (has-type Γ lhs num)
    (has-type Γ rhs num))

(:- (has-type Γ (if Test Then Else) Tau)
    (has-type Γ Test bool)
    (has-type Γ Then Tau)
    (has-type Γ Else Tau))

(? (has-type mt zero num)) ; => yes
(? (has-type mt (if true one zero) num)) ; => yes
(? (has-type mt (if false one true) num)) ; => no

(? (has-type mt true T)) ; => T=bool
(? (has-type mt (if true one zero) T)) ; => T=num
(? (has-type mt (if true one false) T)) ; => no

(? (has-type mt T num)) ; => T=zero

(:- (has-type (bind V T Γ) (var V) T))
(:- (has-type (bind V_ T_ Γ) (var V) T)
    (has-type Γ (var V) T))

(? (has-type (bind "w" bool (bind "v" num mt))
             (var "v")
             T))
; => T=num

(:- (has-type Γ (fun Var Body) (arr T1 T2))
    (has-type (bind Var T1 Γ) Body T2))

(? (has-type mt (fun "x" (if (var "x") zero true)) T)) ; => no

(? (has-type mt (fun "x" (if (var "x") one zero)) T)) ; => T=(bool -> num)

(:- (has-type Γ (app Fun Arg) T2)
    (has-type Γ Fun (arr T1 T2))
    (has-type Γ Arg T1))

(? (has-type mt
             (app (fun "x" (if (var "x") one zero))
                  true)

             T))
; => T=num

(? (has-type mt (fun "x" (var "x")) T)) ; => T=(T1213424 -> T1213424)

(? (has-type mt
             (app (fun "id"
                       (if (app (var "id") false)
                         (app (var "id") true)
                         (app (var "id") true)))
                  (fun "x" (var "x")))
             T))
; => T=bool

(? (has-type mt
             (app (fun "id"
                       (if (app (var "id") false)
                         (app (var "id") one)
                         (app (var "id") zero)))
                  (fun "x" (var "x")))
             T))
; => no

(? (has-type mt (fun "x" (app (var "x") (var "x"))) num)) ; => no
