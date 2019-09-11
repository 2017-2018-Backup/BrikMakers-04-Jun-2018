;Inserts the Section marks
;A2000 VERSION
(defun RightSection (/ P1 P2 A A1)
   (setvar "CMDECHO" 0)
   (setvar "orthomode" 1)
   (if (= (getvar "attdia") 0)   ;get current setting
       (setvar "attdia" 1))      ;if OFF, turn it on
   (setq P1 (getpoint "\nPick BALLOON Center Point.. "))
   (setq P2 (getpoint P1"\nPick Angle of SECTION Mark.."))
   (setq A (angtos (angle P1 P2) 0 8))
   (if (or (>= (atoi A) 90) (<= (atoi A) 270))(progn
                       (if (or (= (atoi a) 90) (= (atoi a) 270))
                       (setq A1 0) (setq A1 (- (atoi A) 180))
                       )
                                              )
   )
   (if (or (< (atoi A) 90) (> (atoi A) 270)) (setq A1 A))
   (setq SCALE(getvar "dimscale"))
   (command ".INSERT" "SECTY1" P1 SCALE "" (+ (atoi A) 180))
   (command ".INSERT" "SECTZ1" P2 SCALE "" (+ (atoi A) 180))
   (command ".INSERT" "SECTX" P1 SCALE "" "") ;A1
 )
