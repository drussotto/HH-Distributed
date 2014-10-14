;;WORLD DESIGN

(define HEIGHT 500)
(define WIDTH 500)
(define BACKGROUND (empty-scene HEIGHT WIDTH))
(define HENRY (rectangle 20 20 "solid" "blue"))
(define ENEMY (triangle 20 "solid" "green"))
(define CUPCAKE (circle 9 "solid" "red"))
(define WP (circle 5 "solid" "black"))

(define-struct hh (pos low loc loe gs))
;A WState is a (make-hh Posn [List-of Posn] [List-of Posn] GameState)
;pos: contains the avatar's current position
;low: contains the player's waypoints
;loc: contains the locations of the cupcakes
;loe: contains the locations of the enemy players
;gs: Is the game yet to begin, in progress, or over?




;WState Examples
(define world0 (make-hh (make-posn 0 0)
                        empty 
                        empty
                        empty
                        'waiting))
(define hh1 (make-hh (make-posn 100 100)
                     (list (make-posn 75 75)
                           (make-posn 90 90))
                     (list (make-posn 120 120)
                           (make-posn 40 40))
                     (list
                      (make-posn 30 75)
                      (make-posn 10 90)
                      (make-posn 40 23))
                     'go))
(define eg-winner (make-hh (make-posn 40 40)
                           empty 
                           empty
                           (list
                            (make-posn 30 75)
                            (make-posn 10 90)
                            (make-posn 40 23))
                           'winner))
(define eg-loser (make-hh (make-posn 80 100)
                          empty
                          (list (make-posn 20 20)
                                (make-posn 55 60))
                          (list
                           (make-posn 30 75)
                           (make-posn 10 90)
                           (make-posn 40 23))
                          'loser))

;A GameState is one of:
; -'waiting -> The game hasn't started
; -'go -> The game is in progress
; -'winner -> The player is the winner
; -'loser -> The player lost

; A Server2World is a 
; (list List-Posn [List-of List-Posn] [List-of List-Posn] [List-of List-Posn] GameState)
; interpretation:
; (first Server2World) -> Player's position
; (second Server2World) -> Player's waypoints
; (third Server2World) -> Locations of the cupcakes
; (fourth Server2World) -> Locations of the enemy players
; (fifth Server2World) -> GameState (see interpretation)



(define s2w1 (list (list 100 100)
                   (list (list 75 75)
                         (list 90 90))
                   (list (list 120 120)
                         (list 40 40))
                   (list (list 30 75)
                         (list 10 90)
                         (list 40 23))
                   'go))
(define s2ww (list (list 40 40)
                   empty 
                   empty
                   (list (list 30 75)
                         (list 10 90)
                         (list 40 23))
                   'winner))


;A List-Posn is a (list Number Number)
;interpretation: A List-Posn represents a Posn as an S-Expression for messages


(define (make-world n ip)
  (big-bang world0
            (to-draw render)
            (on-mouse create-waypoint)
            (on-receive new-world)
            (register ip)
            (stop-when no-cupcakes? final-image)
            (name n)))

;render
;WState -> Image
;Creates the playing field with the cupcakes, all the player's
;avatars, and the world's waypoints

(check-expect (render world0)
              (overlay (text "Waiting for players to join..." 20 'black)
                       BACKGROUND))
(check-expect (render hh1)
              (place-image HENRY 100 100 (place-everything hh1)))


(define (render ws)
  (cond [(symbol=? (hh-gs ws) 'waiting)
         (overlay (text "Waiting for players to join..." 20 'black)
                  BACKGROUND)]
        [else
         (place-image HENRY (posn-x (hh-pos ws)) (posn-y (hh-pos ws))
                      (place-everything ws))]))

;place-everything
;WState -> Image
;Combines all of the place functions for one image

(check-expect (place-everything hh1)
              (place-waypoints (hh-low hh1) 
                               (place-enemies (hh-loe hh1) 
                                              (place-cupcakes (hh-loc hh1) BACKGROUND))))

(define (place-everything ws)
  (place-waypoints (hh-low ws) 
                   (place-enemies (hh-loe ws) 
                                  (place-cupcakes (hh-loc ws) 
                                                  BACKGROUND)))) 

;place-abstract
;[List-of Posn] Image Image -> Image
;Abstraction function for placing images

(define (place-abstract lop object img)
  (cond [(empty? lop) img]
        [(cons? lop) (place-image object (posn-x (first lop))
                                  (posn-y (first lop))
                                  (place-abstract (rest lop) object img))]))

;place-cupcakes
;[List-of Posn] Image -> Image
;Places a cupcake at each posn in the list on top of the input image

(check-expect (place-cupcakes (list
                               (make-posn 30 45)
                               (make-posn 12 15)
                               (make-posn 4 7)) BACKGROUND)
              (place-image CUPCAKE 30 45
                           (place-image CUPCAKE 12 15
                                        (place-image CUPCAKE 4 7 BACKGROUND))))

(define (place-cupcakes cakes img)
  (place-abstract cakes CUPCAKE img))

;place-waypoints
;[List-of Posn] Image -> Image
;Places a cupcake at each Posn in the list

(check-expect (place-waypoints (list
                                (make-posn 30 45)
                                (make-posn 12 15)
                                (make-posn 4 7)) BACKGROUND)
              (place-image WP 30 45
                           (place-image WP 12 15
                                        (place-image WP 4 7 BACKGROUND))))

(define (place-waypoints wps img)
  (place-abstract wps WP img))

;place-enemies
;[List-of Posn] Image -> Image
;Represents the locations of the enemy players

(check-expect (place-enemies (list
                              (make-posn 30 75)
                              (make-posn 10 90)
                              (make-posn 40 23)) BACKGROUND)
              (place-image ENEMY 30 75
                           (place-image ENEMY 10 90
                                        (place-image ENEMY 40 23 BACKGROUND))))
(define (place-enemies foes img)
  (place-abstract foes ENEMY img))


;create-waypoint
;WState x y ME -> Package|WS
;If mouse is presssed, reates a waypoint and sends it to the server.
;Otherwise returns the current WState

(check-expect (create-waypoint hh1 45 63 "button-down")
              (make-package hh1 (list 45 63)))
(check-expect (create-waypoint hh1  45 63 "hello world")
              hh1)

(define (create-waypoint ws x y ME)
  (if (and (string? ME) (string=? ME "button-down"))
      (make-package ws (list x y))
      ws))

;new-world 
;WState Server2World -> WState
;receives a message and creates the world from the message

(check-expect (new-world world0 s2w1) hh1)
(check-expect (new-world hh1 s2ww) eg-winner)


(define (new-world ws msg)
  (local ((define (list->posn lp)
            (make-posn (first lp) (second lp))))
    (make-hh (list->posn (first msg))
             (map list->posn (second msg))
             (map list->posn (third msg))
             (map list->posn (fourth msg))
             (fifth msg)))) 


;no-cupcakes?
;WState -> Boolean
;Returns true when the game ends (when all the cupcakes are eaten)
(check-expect (no-cupcakes? hh1) false)
(check-expect (no-cupcakes?
               (make-hh (make-posn 10 10)
                        (list (make-posn 10 10))
                        empty
                        (list
                         (make-posn 30 75)
                         (make-posn 10 90)
                         (make-posn 40 23))
                        'go)) true)

(define (no-cupcakes? ws)
  (and (not (symbol=? (hh-gs ws) 'waiting)) (empty? (hh-loc ws))))

;final-image
;WState -> Image
;Produces the end of game image

(check-expect (final-image eg-winner)
              (overlay (text "Winner Winner Chicken Dinner" 20 'black) BACKGROUND))
(check-expect (final-image eg-loser)
              (overlay (text "You're a loser!" 20 'black) BACKGROUND))


(define (final-image ws)
  (if (symbol=? (hh-gs ws) 'winner)
      (overlay (text "Winner Winner Chicken Dinner" 20 'black) BACKGROUND)
      (overlay (text "You're a loser!" 20 'black) BACKGROUND)))



(define MAX_PLAYERS 4)


; TO-DO
; - Determine how to update scores for players when they eat a cupcake (update-score : need to figure out where to call it) | DONE
; - Determine when and how to change player's GameState as game progresses (win/lose/stop/start)
; - Finish add-waypoints (on-msg handler) | DONE
; - Figure out what to send clients so they can render world (handle-mail)
; - Client program | DAN


(define-struct uni (lop loiw cupcakes))
; UniverseState is (make-uni [List-of Players] [List-of IWorld] [List-of Posns])
; interpetation: each IWorld is a player playing in HungryHenry game,
; posns are representations of cupcakes within the game


(define-struct player (position low num iw GS))
; A Player is a (make-player Posn [List-of Posn] Number IWorld GameState)
; Interpretation: position is the player's current position
; low is the list of waypoints for the player to go
; num is the number of cupcakes the player has eaten
; name is the player's name
; GS is a GameState

;Player example
(define P1 (make-player (make-posn 20 20)
                        (list (make-posn 13 25)
                              (make-posn 9 18))
                        0
                        iworld1 'go))
(define P2 (make-player (make-posn 40 40) empty 0 iworld2 'waiting))


;Universe Example
(define U1 (make-uni (list P1 P2) (list iworld1 iworld2) (list (make-posn 100 100)
                                                               (make-posn 22  200))))


; Nat -> UniverseState
; creates a universe with empty Player and generates Cupcakes to be
; used in game
(define (make-universe cc)
  (universe (make-uni empty empty  (generate-cupcakes cc))
            [on-tick play-game]
            [on-new add-world]
            [on-msg add-waypoint]))

; HELPER FUNCTIONS TO-MAKE 
; - update-players ([List-of Players)] -> [List-of Players]) DONE
; - update-cupcakes ([List-of Players] [List-of Posn] -> [List-of Posn]) DONE
; - handle-mail (UniverseState -> [List-of Mail])
; - add-wayponts (Mail -> UniverseState) DONE

; UniverseState IW -> UniverseBundle
; when a new human player registers to play:
; - creates a new Player structure to represent human player
;   and adds it to UniverseState's player list.
; - adds new IWorld i onto UniverseState's IWorld list.
; - initiates game if new human player registering
;   makes amount of current players equal to max-players value.
; - ignores human player's request if there is already
;   max players.
(define (add-world u i)
  (local ((define new-us (make-uni  (append (uni-lop u) (list (create-player i))) 
                                    (append (uni-loiw u) (list i))
                                    (uni-cupcakes u)))) 
    (make-bundle new-us 
                 (list (make-mail (first (uni-loiw new-us)) (list (list 0 0) empty empty empty 'waiting)))
                 '())))

; UniverseState -> UniverseBundle
; If number of IWorlds connected is equal to MAX_PLAYERS,
; starts game...
; - Moves Players around game if they have waypoints.
; - Determines if a Player is close enough to eat a Cupcake.
; - Removes Cupcakes and updates score if Player eats a Cupcake.
; - Removes Player Waypoints if their position is equal to their current
;   Waypoint.
; Otherwise, changes nothing and returns previous UniverseState in Bundle.
(define (play-game us)
  (cond
    [(= (length (uni-loiw us)) MAX_PLAYERS)
     (make-bundle (make-uni (update-players (uni-lop us) (uni-cupcakes us)) 
                            (uni-loiw us) (update-cupcakes (uni-lop us) (uni-cupcakes us)))
                  (handle-mail us (uni-loiw us) (uni-lop us)) '())]
    [else (make-bundle us '() '())])) 



; [List-of Players] [List-of Posn] -> [List-of Player]
; Updates Player's position and Waypoint list, moving the player
; as long as their Waypoint list is not empty.
; Updates Player's score if they have eaten a cupcake from list loc.
(define (update-players lop loc)
  (cond
    [(empty? lop) empty]
    [else (cons 
           (make-player 
            (if (empty? (player-low (first lop))) ; Moves avatar if player has a waypoint set
                (player-position (first lop))     ; otherwise, position stays the same
                (move-avatar 
                 (player-position (first lop)) 
                 (first (player-low (first lop))))) 
            (filter-posns (player-position (first lop)) (player-low (first lop)))
            (update-score loc (first lop))
            (player-iw (first lop))
            (player-GS (first lop)))
           (update-players (rest lop) loc))]))

(check-within (update-players (list P1 P2) (list
                                            (make-posn 100 100)))
              (list (make-player (make-posn 18 21)
                                 (list (make-posn 13 25)
                                       (make-posn 9 18))
                                 0 iworld1 'go)
                    P2) 5)

(check-within (update-players (list P1 P2)
                              (list (make-posn 15 22)))
              (list (make-player (make-posn 18 21)
                                 (list (make-posn 13 25)
                                       (make-posn 9 18))
                                 1 iworld1 'go)
                    P2) 5)


; Posn Posn -> Posn
; move avatar p closer to the waypoint wp by SPEED pixels
; ASSUME: avatar doesn't have to 'snap' to waypoint because it gets close
(define (move-avatar p wp)
  (local ((define d (distance wp p))
          (define d-SPEED (- d 3))
          (define wp.x (posn-x wp)) ; wp.x is just a funky name
          (define wp.y (posn-y wp)))
    (make-posn (+ wp.x (* (/ (- (posn-x p) wp.x) d) d-SPEED))
               (+ wp.y (* (/ (- (posn-y p) wp.y) d) d-SPEED)))))


; [List-of Player] [List-of Posn] -> [List-of Posn]
; Determines if any player is close enough to a Cupcake,
; and updates score and removes Cupcake if so.
(define (update-cupcakes lop loc)
  (cond
    [(empty? lop) loc]
    [else
     (update-cupcakes (rest lop) (filter-posns (player-position (first lop)) loc))]))

(check-expect (update-cupcakes (list P1 P2) (list (make-posn 22 20)
                                                  (make-posn 100 100)))
              (list (make-posn 100 100)))
(check-expect (update-cupcakes (list P1 P2) (list (make-posn 100 200)
                                                  (make-posn 40 40)))
              (list (make-posn 100 200)))
(check-expect (update-cupcakes (list P1 P2) (list (make-posn 100 222)
                                                  (make-posn 22 20)
                                                  (make-posn 400 200)))
              (list (make-posn 100 222) (make-posn 400 200)))


; Posn [List-of Posn] -> [List-of Posn]
; Given Player position and list of Waypoints (or Cupcakes), removes
; Waypoint/Cupcake if Player position is close enough to current/first Waypoint or any Cupcake.
(define (filter-posns pos low)
  (local (; Posn Posn -> Boolean
          (define (overlap? p-wp)
            (not (<= (distance pos
                               p-wp) 8))))
    (filter overlap? low)))


; [List-of Posn] Player -> Number
; given original list of cupcakes, adds 1 to player score if
; filtered list contains less than original (i.e. a cupcake has been eaten)
(define (update-score loc p)
  (cond
    [(< (length (filter-posns (player-position p) loc)) (length loc))
     (add1 (player-num p))]
    [else (player-num p)]))

(check-expect (update-score (list (make-posn 45 50)
                                  (make-posn 20 20))
                            P1)
              1)
(check-expect (update-score 
               (list (make-posn 100 100)
                     (make-posn 400 200))
               P1)
              0)


; UniverseState [List-of IWorld] [List-of Player] -> [List-of Mail]
; Sends information relevent to each client 
; (positions of all players, cupcake positions,
; each respective player's waypoints, whether
; or not they are the winner/loser/etc.) so that they may
; render the game
(define (handle-mail us loiw lop)
  (cond
    [(empty? loiw) empty]
    [else (cons (make-mail (first loiw) (list (posn->list (player-position (first lop)))
                                              (split-waypoints (player-low (first (filter (lambda (x) (iworld=? (player-iw x) (first loiw))) (uni-lop us)))))
                                              (split-waypoints (uni-cupcakes us))
                                              (get-player-positions (first lop) (uni-lop us))
                                              'go)) 
                
                (handle-mail us (rest loiw) (rest lop)))]))

(check-expect (handle-mail U1 (uni-loiw U1) (uni-lop U1))
              (list (make-mail iworld1 (list (list 20 20)
                                             (list 
                                              (list 13 25)
                                              (list 9 18))
                                             (split-waypoints (uni-cupcakes U1))
                                             (list
                                              (list 40 40))
                                             'go))
                    (make-mail iworld2 (list (list 40 40)
                                             empty
                                             (split-waypoints (uni-cupcakes U1))
                                             (list
                                              (list 20 20))
                                             'go))))

;Posn -> [List-of Number]
;Converts a Posn to an S-exp
(define (posn->list p)
  (list (posn-x p) (posn-y p)))

; Player [List-of Player] -> [List-of [List-of Number]]
; Breaks up Player structures and composes their positions into a list of numbers suitable to send as mail to clients.
; Skips Player passed to function as Client doesn't need to know its own position in this message.
(define (get-player-positions p lop)
  (cond
    [(empty? lop) empty]
    [(iworld=? (player-iw p) (player-iw (first lop)))
     (get-player-positions p (rest lop))]
    [else
     (cons (posn->list (player-position (first lop))) (get-player-positions p (rest lop)))]))

(check-expect (get-player-positions P1 (uni-lop U1)) (list 
                                                      (list 40 40)))


;make-mail iworld (Posn LOW LOC LOE GS)
; LOW -> (list (list Number Number))

; [List-of Posn] -> [List-of [List-of Number]]
; Splits posn structure from Player waypoints into a list of two numbers so that they can be sent as an S-Expression
; in mail.
(define (split-waypoints lop)
  (local (;Posn -> [List-of Number]
          ;Converts a Posn to an S-exp
          (define (posn->list p)
            (list (posn-x p) (posn-y p))))
    (map posn->list lop)))

(check-expect (split-waypoints (list (make-posn 100 100)
                                     (make-posn 22  200)))
              (list (list 100 100)
                    (list 22 200)))


; UniverseState IWorld World2Server -> UniverseState
; Updates a Player's Waypoint list given new mail from a client IWorld.
(define (add-waypoint us iw msg)
  (local (; Player -> Player
          ; determines which player representation receives added waypoint
          ; based on name of IWorld which sent waypoint and adds waypoint and sends back reformed list.
          (define (pick-player p)
            (cond
              [(iworld=? iw (player-iw p))
               (make-player (player-position p) 
                            (append (player-low p) 
                                    (list (make-posn (first msg) (second msg))))
                            (player-num p)
                            (player-iw p)
                            (player-GS p))]
              [else p])))    
    (make-bundle (make-uni (map pick-player (uni-lop us)) (uni-loiw us) (uni-cupcakes us)) empty empty)))


(check-expect (add-waypoint U1 iworld1 (list 50 50))
              (make-bundle (make-uni (list 
                                      (make-player (player-position P1)
                                                   (list (make-posn 13 25)
                                                         (make-posn 9 18)
                                                         (make-posn 50 50)) 0 (player-iw P1) (player-GS P1))
                                      P2) (uni-loiw U1) (uni-cupcakes U1)) empty empty))



; Number -> [List-of Posn]
; Creates a list of locations for a Number "n" cupcakes
(define (generate-cupcakes n)
  (local (; Number -> Posn
          ; creates a random posn that serves as a Cupcake
          (define (create-cupcake num)
            (make-posn (random WIDTH) (random HEIGHT))))
    (build-list n create-cupcake)))

(check-within (generate-cupcakes 3)
              (list
               (make-posn 200 200) 
               (make-posn 200 200) 
               (make-posn 200 200)) 400)


; IWorld -> Player
; Makes a Player structure using connecting IWorld's name to identify it
; to the server
(define (create-player i)
  (make-player (make-posn (random WIDTH) 
                          (random HEIGHT)) empty 0 i 'waiting))

(check-within (create-player iworld1) 
              (make-player (make-posn 200 200) empty 0 iworld1 'waiting) 300)


; Posn Posn -> Number
; Computes absolute distance between two Posns
(define (distance a b)
  (sqrt (+ (sqr (- (posn-x a)
                   (posn-x b)))
           (sqr (- (posn-y a)
                   (posn-y b))))))

(define (henrytime1 cc)
  (launch-many-worlds 
   (make-universe cc)
   (make-world "dan" LOCALHOST)
   (make-world "jack" LOCALHOST)))

(define (henrytime2 cc)
  (launch-many-worlds 
   (make-universe cc)
   (make-world "jack" LOCALHOST)
   (make-world "Ron Burgundy" LOCALHOST)
   (make-world "Chandler Bing" LOCALHOST)
   (make-world "dan" LOCALHOST)))



