(declare-datatype Color (
  (Red)
  (Green)
  (Blue)
))

(declare-datatype Shape  (
  (Circle)
  (Triangle)
  (Square)
))

(declare-datatype Polarity (
  (Yes)
  (No)
))

(declare-datatype ObjectProps (par (color shape) (
  (Object (color color) (shape shape))
)))


(declare-datatype Constraint (par (a) (
  (Is (polarity Polarity) (prop a))
  (Unconstrained)
)))

(declare-datatype Constraint2 (
  (Same)
  (Different)
  (Unspecified)
))

(define-sort Object () (ObjectProps Color Shape))
(define-sort Prop   () (ObjectProps (Constraint Color) (Constraint Shape)))
(define-sort Prop2  () (ObjectProps Constraint2 Constraint2))



(declare-datatype Op (
  (Eq)
  (Lt)
  (Leq)
))



(declare-datatype Term (
  (Const (tConst Int))
  (Count (tCount Prop))
))

(declare-datatype PosRule (
  (Exist (eProp Prop))
  (ExistAdjacent (eaPropL Prop) (eaPropR Prop) (eaProp2 Prop2))
  (ExistBefore   (ebPropL Prop) (ebPropR Prop) (ebProp2 Prop2))
  (Compare (op Op) (termL Term) (termR Term))
))

(declare-datatype Rule (
  (Rule (rPol Polarity) (rPos PosRule))
))


; Semantics

(declare-datatype Thing (
  (Empty)
  (Full (object Object))
))

; The model

(declare-datatype Model (
  (Model (place_1 Thing)
         (place_2 Thing)
         (place_3 Thing)
         (place_4 Thing)
         (place_5 Thing))
))



(define-fun semPol ((p Polarity) (x bool)) bool (
  match p (
    (Yes x)
    (No  (not x))
  )
))

(define-fun checkColor ((expect (Constraint Color)) (x Color)) bool (
  match expect (
    (Unconstrained true)
    ((Is pol prop) (semPol pol (= prop x)))
  )
))

(define-fun checkShape ((expect (Constraint Shape)) (x Shape)) bool (
  match expect (
    (Unconstrained true)
    ((Is pol prop) (semPol pol (= prop x)))
  )
))

(define-fun checkProp ((expect Prop) (o Object)) bool (
  and (checkColor (color expect) (color o))
      (checkShape (shape expect) (shape o))
))

(define-fun checkThing ((p Prop) (t Thing)) bool (
  match t (
    (Empty false)
    ((Full o) (checkProp p o))
  )
))

(define-fun checkSameColor ((c Constraint2) (x Color) (y Color)) bool (
  match c (
    (Same        (= x y))
    (Different   (not (= x y)))
    (Unspecified true)
  )
))

(define-fun checkSameShape ((c Constraint2) (x Shape) (y Shape)) bool (
  match c (
    (Same        (= x y))
    (Different   (not (= x y)))
    (Unspecified true)
  )
))

(define-fun checkProp2 ((c Prop2) (x Object) (y Object)) bool (
  and
    (checkSameColor (color c) (color x) (color y))
    (checkSameShape (shape c) (shape x) (shape y))
))


(define-fun checkThing2 ((p Prop) (q Prop) (c Prop2) (x Thing) (y Thing)) bool (
  match x (
    (Empty false)
    ((Full o1)
      (match y (
        (Empty false)
        ((Full o2) (and (checkProp p o1)
                        (checkProp q o2)
                        (checkProp2 c o1 o2)
                   ))
        ))
    )
  )
))

(define-fun countThing ((p Prop) (x Thing)) Int (
  match x (
    (Empty 0)
    ((Full o) (if (checkProp p o) 1 0))
  )
))

(define-fun count ((m Model) (p Prop)) Int (
  + (countThing p (place_1 m))
    (countThing p (place_2 m))
    (countThing p (place_3 m))
    (countThing p (place_4 m))
    (countThing p (place_5 m))
))

(define-fun existsOne ((m Model) (p Prop)) bool (
  or
    (checkThing p (place_1 m))
    (checkThing p (place_2 m))
    (checkThing p (place_3 m))
    (checkThing p (place_4 m))
    (checkThing p (place_5 m))
))

(define-fun existsAdjacent ((m Model) (p Prop) (q Prop) (c Prop2)) bool (
  or
    (checkThing2 p q c (place_1 m) (place_2 m))
    (checkThing2 p q c (place_2 m) (place_1 m))

    (checkThing2 p q c (place_2 m) (place_3 m))
    (checkThing2 p q c (place_3 m) (place_2 m))

    (checkThing2 p q c (place_3 m) (place_4 m))
    (checkThing2 p q c (place_4 m) (place_3 m))

    (checkThing2 p q c (place_4 m) (place_5 m))
    (checkThing2 p q c (place_5 m) (place_4 m))
))

(define-fun existsBefore ((m Model) (p Prop) (q Prop) (c Prop2)) bool (
  or
    (checkThing2 p q c (place_1 m) (place_2 m))
    (checkThing2 p q c (place_1 m) (place_3 m))
    (checkThing2 p q c (place_1 m) (place_4 m))
    (checkThing2 p q c (place_1 m) (place_5 m))

    (checkThing2 p q c (place_2 m) (place_3 m))
    (checkThing2 p q c (place_2 m) (place_4 m))
    (checkThing2 p q c (place_2 m) (place_5 m))

    (checkThing2 p q c (place_3 m) (place_4 m))
    (checkThing2 p q c (place_3 m) (place_5 m))

    (checkThing2 p q c (place_4 m) (place_5 m))
))


(define-fun semOp ((op Op) (x Int) (y Int)) bool (
  match op (
    (Eq  (= x y))
    (Lt  (< x y))
    (Leq (<= x y))
  )
))

(define-fun semTerm ((m Model) (t Term)) Int (
  match t (
    ((Const n) n)
    ((Count p) (count m p))
  )
))

(define-fun checkPosRule ((m Model) (r PosRule)) bool (
  match r (
    ((Exist p)             (existsOne m p))
    ((ExistAdjacent p q r) (existsAdjacent m p q r))
    ((ExistBefore   p q r) (existsBefore   m p q r))
    ((Compare o t1 t2)    (semOp o (semTerm m t1) (semTerm m t2)))
  )
))

(define-fun checkRule ((m Model) (r Rule)) bool (
  semPol (rPol r) (checkPosRule m (rPos r))
))

(declare-const theModel Model)
(declare-const theRule  Rule)

