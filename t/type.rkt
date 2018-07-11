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
(data numConst 0)
(data boolConst 0)

;; Environments
(data mt 0)
(data bind 3)

(:- (has-type Γ numConst num))
(:- (has-type Γ boolConst bool))

(:- (has-type Γ (if Test Then Else) Tau)
    (has-type Γ Test bool)
    (has-type Γ Then Tau)
    (has-type Γ Else Tau))

(? (has-type mt numConst num)) ; => yes
(? (has-type mt (if boolConst numConst numConst) num)) ; => yes
(? (has-type mt (if boolConst numConst boolConst) num)) ; => no

(? (has-type mt boolConst T)) ; => T=bool
(? (has-type mt (if boolConst numConst numConst) T)) ; => T=num
(? (has-type mt (if boolConst numConst boolConst) T)) ; => no

(? (has-type mt T num)) ; => T=numConst

(:- (has-type (bind V T Γ) (var V) T))
(:- (has-type (bind V_ T_ Γ) (var V) T)
    (has-type Γ (var V) T))

(? (has-type (bind "w" bool (bind "v" num mt))
             (var "v")
             T))
; => T=num

(:- (has-type Γ (fun Var Body) (arr T1 T2))
    (has-type (bind Var T1 Γ) Body T2))

(? (has-type mt (fun "x" (if (var "x") numConst boolConst)) T)) ; => no

(? (has-type mt (fun "x" (if (var "x") numConst numConst)) T)) ; => T=(bool -> num)

(:- (has-type Γ (app Fun Arg) T2)
    (has-type Γ Fun (arr T1 T2))
    (has-type Γ Arg T1))

(? (has-type mt
             (app (fun "x" (if (var "x") numConst numConst))
                  boolConst)

             T))
; => T=num

(? (has-type mt (fun "x" (var "x")) T))
; => T=(T1213424 -> T1213424)

(? (has-type mt
             (app (fun "id"
                       (if (app (var "id") boolConst)
                         (app (var "id") boolConst)
                         (app (var "id") boolConst)))
                  (fun "x" (var "x")))
             T))
; => T=bool

(? (has-type mt
             (app (fun "id"
                       (if (app (var "id") boolConst)
                         (app (var "id") numConst)
                         (app (var "id") numConst)))
                  (fun "x" (var "x")))
             T))
; => no

(? (has-type mt (fun "x" (app (var "x") (var "x"))) num))
; => no

; Infinite loop:
#;(? (has-type mt (fun x (app (var x) (var x))) T))
