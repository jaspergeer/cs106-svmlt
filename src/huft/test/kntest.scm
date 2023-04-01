;; F.LITERAL, F.CHECK_EXPECT, F.CHECK_ASSERT
;;
(check-expect (number? 3) #t)            
(check-expect (number? 'really?) #f)
(check-assert (symbol? 'really?))