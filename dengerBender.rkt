#lang racket



(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require racket/list)
(require racket/math)

(define-values
  (scn_w scn_h spd rot_spd target_rot smooth rot move r roff meteor_count bullet_count draw-bullets pos-bullets bullet_spd score death o)
  (values 750 600 15 7 0 15 0 0 60 0 10 0 empty empty 20 0 0 2))
#|
scn_w ---------------- Ekran Genişlik ölçütü
scn_h ---------------- Ekran yükseklik ölçütü
spd ------------------ Roketin hızı
rot_spd -------------- oyuncunun dünme hızı
target_rot ----------- Hedefin açısı
rot ------------------ Anlık dönüş hızı
move ----------------- Hareket Yönü. Eğer bu değer 1 ise ileri hareket, -1 ise geri hareket eder.  
r -------------------- Meteor yarıçap
roff ----------------- meteor üreticisi için kullanılan yarıçap.
meteor_count --------- Meteor sayısı
bullet_count --------- Var olan bomba
pos-bullets ---------- Atılan bombanın (parçalayıcının) pozisyonu (posn x y)
bullet_spd ----------- parçalayıcının hızı

|#

(define-values (x y) (values (+ (/ scn_w 2) r) (+ r (/ scn_h 2)))) ; ---------- Anlık kordinat bilgisi.
(define target_pos (list (+ (/ scn_w 2) r) (+ r (/ scn_h 2)))) ; -------------- Hedef kordinat pozisyon listesi (x y).




(define (ran x y)
  (cond
    ((<= x 0 y) (- (random (max (round (inexact->exact (+ x (abs x) 1))) 2) (max (round (inexact->exact (+ y (abs x)))) 2)) (round (inexact->exact (abs x)))))
    ((>= x 0 y) (- (random (max (round (inexact->exact (+ x (abs y) 1))) 2) (max (round (inexact->exact (+ y (abs y)))) 2)) (round (inexact->exact (abs y)))))
    ((or (<= y x 0) (<= x y 0)) (* -1 (random (round (inexact->exact x)) (round (inexact->exact y)))))
    (else (random (round (inexact->exact x)) (round (inexact->exact y))))
    )
  )


; Just some shortcuts for fun. :D

(define (P n)
  (list-ref player n)
  )
(define (t-pos n)
  (list-ref target_pos n)
  )

;(define player (list (triangle r "outline" "white") x y 0 ))
(define player (list (bitmap "rocketlast.png") x y 0));-------------------- Oyuncunun  resmi  
(define keys (list 0 0 0 0));--------------------------------------------------- Yön tuş tanım listesi (up right down left)
(define meteors empty);--------------------------------------------------------- Meteor listesi 
(define bullets empty);--------------------------------------------------------- kurşun liste yapısı (x y yön)

(define scn  (bitmap  "backspace1.jpg"));--------------------------- arka plan resmi


(define p (circle 35  0 "white"))


(define (degtorad x)
  (degrees->radians x)
  )



(define (draw-meteor n x y r)
  (let ((meteors empty))
    (let ((rads empty))
      
      (let loop ((i 0))
        (if (< i n) (begin
                      (set! rads (append rads (list (+ r (ran (* r -0.3) (* r 0.5))))))
                      (loop (add1 i))
                      ) void)
        )
      
      (let loop ((i 0))
        (if (< i n) (begin
                    
                    
                      (set! meteors (append meteors (list (list (round (+ x (* (list-ref rads i) (cos (* i (/ (* 2 pi) n)))))) (round (+ y (* (list-ref rads i) (sin (* i (/ (* 2 pi) n)))))) (round (+ x (* (list-ref rads (modulo (add1 i) n)) (cos (* (+ 1 i) (/ (* 2 pi) n)))))) (round (+ y (* (list-ref rads (modulo (add1 i) n)) (sin (* (+ 1 i) (/ (* 2 pi) n)))))) ))))
                      (loop (add1 i))
                      
                      )
            void)
        )
      )
    
    (let loop ((i 0))
      (if (< i n) (add-line (add-line (loop (add1 i))
                                      (list-ref (list-ref meteors i) 0)
                                      (list-ref (list-ref meteors i) 1)
                                     (list-ref (list-ref meteors i) 2)
                                     (list-ref (list-ref meteors i) 3) "red")
                            (list-ref (list-ref meteors i) 2)
                            (list-ref (list-ref meteors i) 3)
                            (list-ref (list-ref meteors (modulo (add1 i) n)) 0)
                            (list-ref (list-ref meteors (modulo (add1 i) n)) 1) "white")
          (rectangle (* r 3) (* r 3) 0 "red"))
      )
    )
  )




(define (outofscreen l n)
  (if (or (< (posn-x (list-ref l n)) 0) (> (posn-x (list-ref l n)) scn_w) (< (posn-y (list-ref l n)) 0) (> (posn-y (list-ref l n)) scn_h)) #t #f)
  )


   
(define (destroy l n)
  (append (take l n) (rest (drop l n)))
  )

(define (dis x1 y1 x2 y2)
  (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2))))
  )

(define (check-hit x1 y1 x2 y2 r)
  (if (< (dis x1 y1 x2 y2) r) #t #f)
  )

(define (split meteors i)
  (let ((rot (random 360))) (append meteors
                                    (list (list (draw-meteor (random 7 13) (* o (/ (list-ref (list-ref meteors i) 5) 2)) (* o (/ (list-ref (list-ref meteors i) 5) 2)) (/ (list-ref (list-ref meteors i) 5) 2)) (list-ref (list-ref meteors i) 1) (list-ref (list-ref meteors i) 2) (inexact->exact (round (* 1.2 (list-ref (list-ref meteors i) 3)))) rot (inexact->exact (round (/ (list-ref (list-ref meteors i) 5) 1.5)))))
                                    (list (list (draw-meteor (random 7 13) (* o (/ (list-ref (list-ref meteors i) 5) 2)) (* o (/ (list-ref (list-ref meteors i) 5) 2)) (/ (list-ref (list-ref meteors i) 5) 2)) (list-ref (list-ref meteors i) 1) (list-ref (list-ref meteors i) 2) (inexact->exact (round (* 1.2 (list-ref (list-ref meteors i) 3)))) (+ 180 rot) (inexact->exact (round (/ (list-ref (list-ref meteors i) 5) 1.5)))))))
  )





(define (draw ws)
  (place-images
   (append
    (list (rotate (P 3) (place-image/align (P 0) 35 27  "center" "center" p)))
    ;skor



    
    (list (bitmap "skor452.png") )
    (list (text (number->string score ) 40 "white"))

    
    (list ( if (and(> score -1 )(< score 6)) (text "level 1" 50 "yellow") (text "" 50 "yellow") ))
    
    (list ( if (and (> score 5) (< score 10) ) (text "level 2" 50 "yellow" ) (text "" 50 "yellow") ))
    
    (list ( if (and (> score 10) (< score 20) )  (text "level 3" 50 "yellow") (text "" 50 "yellow") ))

    (list ( if (and (> score 20) (< score 30) )  (text "level 4" 50 "yellow") (text "" 50 "yellow") ))

    
    
     ;Yıldızlar
    (list ( if  (> score 4)  (bitmap "star3.png") (text "" 50 "white") ))
    (list ( if  (> score 9)   (bitmap "star3.png") (text "" 50 "white") ))
    (list ( if  (> score 15)  (bitmap "star3.png") (text "" 50 "white") ))
    (list ( if  (> score 25)  (bitmap "star3.png") (text "" 50 "white") ))
    (list ( if  (> score 30)  (bitmap "star3.png") (text "" 50 "white") ))
    (list ( if  (= score 29)  (text "Tebrikler Dünyayı Kurtardın!!" 60 "yellow") (text "" 50 "white") ))
    draw-bullets
    )
   
   (append
    (list
      
     (make-posn (P 1) (P 2))
     (make-posn (/ scn_w 2)90)
     (make-posn (/ scn_w 2)100)
     (make-posn (/ scn_w 5) 80)
     (make-posn (/ scn_w 5) 80)
     (make-posn (/ scn_w 5) 80)
     (make-posn (/ scn_w 5) 80)
     (make-posn (/ scn_w 3) 500)
     (make-posn (/ scn_w 2.4) 500)
     (make-posn (/ scn_w 2) 500)
     (make-posn (/ scn_w 1.71) 500)
     (make-posn (/ scn_w 1.5) 500)
     (make-posn (/ scn_w 1.7) 300)
     )
    pos-bullets
    )
   (let loop ((i 0))
     (if (< i (length meteors)) (place-image (list-ref (list-ref meteors i) 0) (- (list-ref (list-ref meteors i) 1) (list-ref (list-ref meteors i) 5)) (- (list-ref (list-ref meteors i) 2) (list-ref (list-ref meteors i) 5)) (loop (add1 i))) scn)
     )
   )
  )

(define (key-handler ws ke)
  (cond
    ((key=? ke "up") (set! keys (list-set keys 0 1)))
    ((key=? ke "right") (set! keys (list-set keys 1 1)))
    ((key=? ke "down") (set! keys (list-set keys 2 1)))
    ((key=? ke "left") (set! keys (list-set keys 3 1)))
    ((key=? ke " ") (set! bullet_count (add1 bullet_count)))
    )
  )

(define (release-handler ws ke)
  (cond
    ((key=? ke "up") (set! keys (list-set keys 0 0)))
    ((key=? ke "right") (set! keys (list-set keys 1 0)))
    ((key=? ke "down") (set! keys (list-set keys 2 0)))
    ((key=? ke "left") (set! keys (list-set keys 3 0)))
    )
  )


(define (mouse-handler ws x y me)
  ws
  )

(define (step ws)
  (begin
    
    (let loop ((i 0))
      (if (and (< i bullet_count) (not (= bullet_count (length bullets)))) (begin
                                                                             (set! bullets (append bullets (list (list (- (modulo (round (+ x r)) (+ scn_w r r)) r) (- (modulo (round (+ y r)) (+ scn_h r r)) r) (modulo (round rot) 360)))))
                                                                             (loop (add1 i))
                                                                             )void)
      )
    
    (if (not (= bullet_count (length draw-bullets)))
        (set! draw-bullets
              (let ((b empty))
                (let loop ((i 0))
                  (if (< i bullet_count) (begin (set! b (append b (list (rotate (list-ref (list-ref bullets i) 2) (bitmap "newbullet.png") )))) (loop (add1 i))) b)
                  )
                )
              )
        void)

    (if (not (= bullet_count (length pos-bullets))) (set! pos-bullets
                                                          (let ((b empty))
                                                            (let loop ((i 0))
                                                              (if (< i bullet_count) (begin (set! b (append b (list (make-posn (- (list-ref (list-ref bullets i) 0) r) (- (list-ref (list-ref bullets i) 1) r))))) (loop (add1 i))) b)
                                                              )
                                                            )
                                                          )
        void)

    (let loop ((i 0))
      (if (and (< i meteor_count) (not (= meteor_count (length meteors)))) (begin
                                                                             (let ((r (random 15 25))) (set! meteors (append meteors (list (list (draw-meteor (random 7 13) (* r o) (* r o) r) (random scn_w) (random scn_h) (random 1 5) (random 360) r)))))
                                                                             (loop (add1 i))
                                                                             )void)
      )



    (let loop ((i 0))
      (if (< i meteor_count) (begin
                               (set! meteors (list-set meteors i (list-set (list-ref meteors i) 1 (modulo (round (+ (list-ref (list-ref meteors i) 1) (* (list-ref (list-ref meteors i) 3) (cos (degtorad (list-ref (list-ref meteors i) 4)))))) (+ scn_w (* 2 (list-ref (list-ref meteors i) 5)))))))
                               (set! meteors (list-set meteors i (list-set (list-ref meteors i) 2 (modulo (round (+ (list-ref (list-ref meteors i) 2) (* (list-ref (list-ref meteors i) 3) (sin (degtorad (list-ref (list-ref meteors i) 4)))))) (+ scn_h (* 2 (list-ref (list-ref meteors i) 5)))))))
                               (loop (add1 i))
                               )
          void)
      )

    (let loop ((i 0))
          
      (if (< i bullet_count) (begin
                               (set! bullets (list-set bullets i (list-set (list-ref bullets i) 0 (round (+ (list-ref (list-ref bullets i) 0) (* bullet_spd (cos (degtorad (+ (list-ref (list-ref bullets i) 2) 90)))))))))
                               (set! bullets (list-set bullets i (list-set (list-ref bullets i) 1 (round (+ (list-ref (list-ref bullets i) 1) (* bullet_spd (sin (degtorad (- (list-ref (list-ref bullets i) 2) 90)))))))))
                               (loop (add1 i))
                               )
          void)
      )

                                                       


    (let loop ((i 0))
      (if (< i bullet_count) (begin
                               (set! pos-bullets (list-set pos-bullets i (make-posn (round (+ (posn-x (list-ref pos-bullets i)) (* bullet_spd (cos (degtorad (+ (list-ref (list-ref bullets i) 2) 90))))))
                                                                                    (round (+ (posn-y (list-ref pos-bullets i)) (* bullet_spd (sin (degtorad (- (list-ref (list-ref bullets i) 2) 90)))))))))
                               (loop (add1 i))
                               )
          void)
      )

    (if (> bullet_count 0) (begin
                             (if (outofscreen pos-bullets 0)
                                 (begin
                                   (set! bullet_count (sub1 bullet_count))
                                   (set! draw-bullets (destroy draw-bullets 0))
                                   (set! pos-bullets (destroy pos-bullets 0))
                                   (set! bullets (destroy bullets 0)))
                                 void)
                             )
        void)

    (let loop_m ((i 0))
      (if (< i meteor_count) (begin
                               (if (check-hit
                                   x
                                   y
                                   (list-ref (list-ref meteors i) 1)
                                   (list-ref (list-ref meteors i) 2)
                                   (+ (+ (list-ref (list-ref meteors i) 5) (* (list-ref (list-ref meteors i) 5) -0.3)) r)
                                   )
                                   (set! death 1)
                                   void
                                   )
                               
                               (let loop_b ((j 0))
                               (if (< j bullet_count) (if (check-hit
                                                           (list-ref (list-ref bullets j) 0)
                                                           (list-ref (list-ref bullets j) 1)
                                                           (list-ref (list-ref meteors i) 1)
                                                           (list-ref (list-ref meteors i) 2)
                                                           (+ (list-ref (list-ref meteors i) 5) (* (list-ref (list-ref meteors i) 5) -0.3))
                                                           )
                                                          (begin
                                                            (set! score (add1 score))
                                                            (set! bullet_count (sub1 bullet_count))
                                                            (set! draw-bullets (destroy draw-bullets j))
                                                            (set! pos-bullets (destroy pos-bullets j))
                                                            (set! bullets (destroy bullets j))
                                                            
                                                            (if (>= (list-ref (list-ref meteors i) 5) 15)
                                                                (begin
                                                                  (set! meteors (split meteors i))
                                                                  (set! meteor_count (+ meteor_count 2)))
                                                                void)
                                                            
                                                            (set! meteor_count (sub1 meteor_count))
                                                            (set! meteors (destroy meteors i))
                                                            )
                                                          (loop_b (add1 j)))
                                   (loop_m (add1 i)))
                               )
                               
                               )void)
      )
    
    
    (if (= (list-ref keys 1) 1) (set! target_rot (- target_rot rot_spd)) (set! target_rot target_rot))
    (if (= (list-ref keys 3) 1) (set! target_rot (+ target_rot rot_spd)) (set! target_rot target_rot))
    (set! move (- (list-ref keys 0) (list-ref keys 2)))
    (set! x (+ x (/ (- (t-pos 0) x) smooth)))
    (set! y (+ y (/ (- (t-pos 1) y) smooth)))
    (set! rot (+ rot (/ (- target_rot rot) (/ smooth 2))))
    
    (set! player (list (P 0) (- (modulo (round x) (+ scn_w r r)) r)  (- (modulo (round y) (+ scn_h r r)) r) (modulo (round rot) 360)))

    (if (= move 0) (set! target_pos target_pos) (if (= move 1) (set! target_pos (list (+ (t-pos 0) (* spd (cos (degtorad (+ rot 90))))) (+ (t-pos 1) (* spd (sin (degtorad (- rot 90))))))) (set! target_pos (list (- (t-pos 0) (* spd (cos (degtorad (+ rot 90))))) (- (t-pos 1) (* spd (sin (degtorad (- rot 90)))))))))


    )
)
(big-bang 0
  (on-tick step)
  (on-mouse mouse-handler)
  (on-draw draw)

  (on-release release-handler)
  (on-key key-handler)
  )


  



