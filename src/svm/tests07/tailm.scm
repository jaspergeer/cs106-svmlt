    (define times-plus (n m product) ; returns n * m + product
      (if (= n 0)
          product
          (times-plus (- n 1) m (+ m product))))

    (check-expect (times-plus 1200000 12 99) 14400099)
