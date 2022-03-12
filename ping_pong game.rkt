#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)



(define-values
  (u1 u2 score1 score2 w_width w_height p1_p p2_p b1_pos b2_pos b_px b_py dir_x dir_y ball_speedx ball_speedy player_speed fg_color bg_color start? pause? restart?)
  (values 0 0 0 0 800 500 250 250 10 490 400 200 1 1 10 10 20 "white" "black" #f #f #f))


(define-values
  (player main_window ball scn bg score1_window score2_window w_midl player_border)
  (values (rectangle 7 80 "solid" fg_color)      ;Player
          (rectangle w_width w_height "solid" bg_color)   ;Main window
          (bitmap "/Users/dinisbarcari/Documents/Documents Permanent/Block 3 project/ball.png") ;(circle 10 "solid" fg_color))   ;Ball 
          (empty-scene 0 0)                      ;Place for PAUSED text
          (bitmap "/Users/dinisbarcari/Documents/Documents Permanent/Block 3 project/tenis-01.png") ;Backgroun
          (text/font (number->string score1) 40 fg_color #f "roman" "normal" "bold" #f) ;Score window/player 1
          (text/font (number->string score2) 40 fg_color #f "roman" "normal" "bold" #f) ;Score window/player 2
          (/ w_width 2)
          (- w_height 45)))
          
          
          

                           
;Main Drawing____________________________________________________________________________________________________________

(define (draw_game ws)
   (place-images
    (list player player ball score1_window score2_window scn bg)
    (list 
     (make-posn 10 p1_p)
     (make-posn 790 p2_p)
     (make-posn  b_px  b_py)
     (make-posn 300 50)
     (make-posn 500 50)
     (make-posn 400 250)
     (make-posn 400 250))  main_window))




;Keys capture_______________On Key pressing event/Key release event________________________________________________________________________________________

(define (on_key ev key)
  (cond
    ((equal? start? #t)
     (cond
       [(equal? pause? #f)
        (cond
          [(string=? key "w")  (set! u1 -1)]
          [(string=? key "s")  (set! u1 1)]
          [(key=? key "up")  (set! u2 -1)]
          [(key=? key "down")  (set! u2 1)]
          [(key=? key "p")  (set! pause? #t)])]
       (else
        (cond [(key=? key "p") (set! pause? #f)]))))
    (else
     (cond
       [(key=? key " ") (set! start? #t)]
       [(key=? key "r") (set! restart? #t)]))))


(define (on_key_release ev key)
  (cond
    [(string=? key "w")  (set! u1 0)]
    [(string=? key "s")  (set! u1 0)]
    [(key=? key "up")  (set! u2 0)]
    [(key=? key "down")  (set! u2 0)]))
       
    



;Update Score____________________________________________________________________________________________________________
(define (updateScore winer)
  (cond
    [(= winer 1)
     (set!-values (score1 b_px start? dir_x ball_speedx ball_speedy) (values (add1 score1) w_midl #f (- dir_x) 10 10))]
    [(= winer 2)
     (set!-values (score2 b_px start? dir_x ball_speedx ball_speedy) (values (add1 score2) w_midl #f (- dir_x) 10 10))])
  
  (set! score1_window (text/font (number->string score1) 40 fg_color #f "roman" "normal" "bold" #f))
  (set! score2_window (text/font (number->string score2) 40 fg_color #f "roman" "normal" "bold" #f)))




;ON STEP 28ticks/s____________main moving function________________________________________________________________________
    
(define (step w)
  ;Stop and Pause events
  (cond
    ((equal? pause? #t) (set! scn (text/font "PAUSED" 40 "white" #f "roman" "normal" "bold" #f)))
    ((equal? start? #f) (set! scn (text/font "Press SPACE to start                Press R to restart     " 30 "white" #f "roman" "normal" "bold" #f)))
    ((equal? restart? #t) (set!-values (score1 score2 b_px restart?) (values 0 0 400 #f)) (updateScore 0))                    
    (else (set! scn (empty-scene 0 0))))
  
  ;Moving the players
  (set!-values (p1_p p2_p) (values (+ p1_p (* player_speed u1)) (+ p2_p (* player_speed u2))))

  ;Moving the ball
  (cond
    ((and(equal? start? #t) (equal? pause? #f))
     (set!-values (b_px  b_py ball_speedx)
                  (values
                   (+ b_px (* ball_speedx dir_x))
                   (+ b_py (* ball_speedy dir_y))
                   (+ ball_speedy 0.005)))
      (cond
       [(> b_px w_width) (updateScore 1)]
       [(< b_px 0) (updateScore 2)])))


  ;Border for ball
  (define-values (platform_size ball_margin_right ball_margin_left ball_vertical_margin)  (values 51 770 35 20))
  
  (cond
     [(>= b_py (- w_height ball_vertical_margin))
      (set! b_py (- w_height ball_vertical_margin)) (set! dir_y (- dir_y))]
     [(<= b_py ball_vertical_margin)
      (set! b_py ball_vertical_margin) (set! dir_y (- dir_y))]
     
     [(and (>= b_px ball_margin_right)(and(< p2_p (+ b_py platform_size)) (> p2_p  (- b_py platform_size))))
      (set! b_px ball_margin_right) (set! dir_x (- dir_x))
      (cond
        ((or (and (> p2_p (+ b_py ball_margin_left)) (< p2_p (+ b_py platform_size)))
            (and (> p2_p (- b_py ball_margin_left)) (< p2_p (- b_py platform_size)))) (set! ball_speedx (+ ball_speedx 10)))
        ((or (and (< p2_p (+ b_py ball_margin_left)) (> p2_p b_py))
            (and (< p2_p (- b_py ball_margin_left)) (> p2_p b_py))) (set! ball_speedx (- ball_speedx 1))))]
     
     [(and (<= b_px 30) (and(< p1_p (+ b_py platform_size)) (> p1_p (- b_py platform_size))))
      (set! b_px 30) (set! dir_x (- dir_x))
      (cond
        ((or (and (> p2_p (+ b_py ball_margin_left)) (< p2_p (+ b_py platform_size)))
            (and (> p2_p (- b_py ball_margin_left)) (< p2_p (- b_py platform_size)))) (set! ball_speedx (+ ball_speedx 10)))
        ((or (and (< p2_p (+ b_py ball_margin_left)) (> p2_p b_py))
            (and (< p2_p (- b_py ball_margin_left)) (> p2_p b_py))) (set! ball_speedx (- ball_speedx 1))))])
  

  ;Borders for players
  (define player_border_bottom 45)
  
  (cond
    [(<= p1_p player_border_bottom) (set! p1_p player_border_bottom)]
    [(>= p1_p player_border) (set! p1_p player_border)])
  (cond
    [(<= p2_p player_border_bottom) (set! p2_p player_border_bottom)]
    [(>= p2_p player_border) (set! p2_p player_border)]))


 

;BIG BANG CALLING____________________________________________________________________________

(big-bang 0
          (on-tick step)
          (on-key on_key)
          (on-release on_key_release)
          (on-draw draw_game))




