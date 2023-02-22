(begin 
   (let* ([tmp 2]
          [tmp (+ tmp tmp)]) 
     (check tmp 'two-plus-two))
   (let ([tmp 4]) 
     (expect tmp 'four)))
