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

(define-sort Object () (ObjectProps Color Shape))

(declare-datatype Constraint (par (a) (
  (Is (polarity Polarity) (prop a))
  (Unconstrained)
)))

(declare-datatype Constraint2 (
  (Same)
  (Different)
  (Unspecified)
))


(declare-datatype Thing (
  (Empty)
  (Full (object Object))
))

; Syntactic Sugar

(define-sort Prop ()  (ObjectProps (Constraint Color) (Constraint Shape)))
(define-sort Prop2 () (ObjectProps Constraint2 Constraint2))

; Semantics

; The model
(declare-const place_1 Thing)
(declare-const place_2 Thing)
(declare-const place_3 Thing)
(declare-const place_4 Thing)
(declare-const place_5 Thing)



(define-fun checkColor ((expect (Constraint Color)) (x Color)) bool (
  match expect (
    (Unconstrained true)
    ((Is pol prop)
       (match pol (
          (Yes (= prop x))
          (No  (not (= prop x)))
       ))
    )
  )
))

(define-fun checkShape ((expect (Constraint Shape)) (x Shape)) bool (
  match expect (
    (Unconstrained true)
    ((Is pol prop)
       (match pol (
          (Yes (= prop x))
          (No  (not (= prop x)))
       ))
    )
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

(define-fun count ((p Prop)) Int (
  + (countThing p place_1)
    (countThing p place_2)
    (countThing p place_3)
    (countThing p place_4)
    (countThing p place_5)
))

(define-fun existsOne ((p Prop)) bool (
  or
    (checkThing p place_1)
    (checkThing p place_2)
    (checkThing p place_3)
    (checkThing p place_4)
    (checkThing p place_5)
))

(define-fun existsAdjacent ((p Prop) (q Prop) (c Prop2)) bool (
  or
    (checkThing2 p q c place_1 place_2)
    (checkThing2 p q c place_2 place_1)

    (checkThing2 p q c place_2 place_3)
    (checkThing2 p q c place_3 place_2)

    (checkThing2 p q c place_3 place_4)
    (checkThing2 p q c place_4 place_3)

    (checkThing2 p q c place_4 place_5)
    (checkThing2 p q c place_5 place_4)
))

(define-fun existsBefore ((p Prop) (q Prop) (c Prop2)) bool (
  or
    (checkThing2 p q c place_1 place_2)
    (checkThing2 p q c place_1 place_3)
    (checkThing2 p q c place_1 place_4)
    (checkThing2 p q c place_1 place_5)

    (checkThing2 p q c place_2 place_3)
    (checkThing2 p q c place_2 place_4)
    (checkThing2 p q c place_2 place_5)

    (checkThing2 p q c place_3 place_4)
    (checkThing2 p q c place_3 place_5)
))


