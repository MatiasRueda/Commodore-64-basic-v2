(ns commodore-64-basic-v2.core-test
  (:require [clojure.test :refer :all]
            [commodore-64-basic-v2.core :refer :all]))

(deftest palabra-reservada?-test
  (testing "Palabra reservadas correctas devuelven true."
    (is (palabra-reservada? 'ENV))
    (is (palabra-reservada? 'LOAD))
    (is (palabra-reservada? 'SAVE))
    (is (palabra-reservada? 'RUN))
    (is (palabra-reservada? 'EXIT))
    (is (palabra-reservada? 'INPUT))
    (is (palabra-reservada? 'PRINT))
    (is (palabra-reservada? '?))
    (is (palabra-reservada? 'DATA))
    (is (palabra-reservada? 'READ))
    (is (palabra-reservada? 'REM))
    (is (palabra-reservada? 'RESTORE))
    (is (palabra-reservada? 'CLEAR))
    (is (palabra-reservada? 'LET))
    (is (palabra-reservada? 'LIST))
    (is (palabra-reservada? 'NEW))
    (is (palabra-reservada? 'END))
    (is (palabra-reservada? 'FOR))
    (is (palabra-reservada? 'TO))
    (is (palabra-reservada? 'NEXT))
    (is (palabra-reservada? 'STEP))
    (is (palabra-reservada? 'GOSUB))
    (is (palabra-reservada? 'RETURN))
    (is (palabra-reservada? 'GOTO))
    (is (palabra-reservada? 'IF))
    (is (palabra-reservada? 'THEN))
    (is (palabra-reservada? 'ON))
    (is (palabra-reservada? 'GOTO))
    (is (palabra-reservada? 'ATN))
    (is (palabra-reservada? 'INT))
    (is (palabra-reservada? 'SIN))
    (is (palabra-reservada? 'EXP))
    (is (palabra-reservada? 'LOG))
    (is (palabra-reservada? 'LEN))
    (is (palabra-reservada? 'MID$))
    (is (palabra-reservada? 'ASC))
    (is (palabra-reservada? 'CHR$))
    (is (palabra-reservada? 'STR$))
    (is (palabra-reservada? '+))
    (is (palabra-reservada? '-))
    (is (palabra-reservada? '*))
    (is (palabra-reservada? '/))
    (is (palabra-reservada? '=))
    (is (palabra-reservada? '<>))
    (is (palabra-reservada? '<))
    (is (palabra-reservada? '<=))
    (is (palabra-reservada? '>))
    (is (palabra-reservada? '>=))
    (is (palabra-reservada? 'AND))
    (is (palabra-reservada? 'OR)))
  
  (testing "Palabras NO reservadas devuelven false" 
    (is (not (palabra-reservada? 'SPACE)))))


(deftest operador?-test
  (testing "Operadores correctos devuelven true."
    (is (operador? '+))
    (is (operador? '-))
    (is (operador? '/))
    (is (operador? '*))
    (is (operador? '=))
    (is (operador? '<>))
    (is (operador? '<))
    (is (operador? '>))
    (is (operador? '<=))
    (is (operador? '>=))
    (is (operador? 'OR))
    (is (operador? 'AND))))
    

(deftest anular-invalidos-test
  (testing "Devuelve lo pedido" 
    (is (= (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0)) '(IF X nil * Y < 12 THEN LET nil X = 0)))))


(deftest cargar-linea-test
  (testing "Devuelve lo pedido"
    (is (= (cargar-linea '(10 (PRINT X)) [() [:ejecucion-inmediata 0] [] [] [] 0 {}]) 
           '[((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}]))
    (is (= (cargar-linea '(20 (X = 100)) ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}])
           '[((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]))
    (is (= (cargar-linea '(15 (X = X + 1)) ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]) 
           '[((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]))))


(deftest expandir-nexts-test
  (testing "Devuelve lo pedido"
    (is (= (expandir-nexts  (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B))) 
           '((PRINT 1) (NEXT A) (NEXT B))))))


(deftest dar-error-test 
  (testing "Devuelve lo pedido" 
    (is (= (with-out-str (dar-error 16 [:ejecucion-inmediata 4]))  (with-out-str (print "?SYNTAX  ERROR")))) 
    (is (= (with-out-str (dar-error "?ERROR DISK FULL" [:ejecucion-inmediata 4]))  (with-out-str (print "?ERROR DISK FULL")))) 
    (is (= (with-out-str (dar-error 16 [100 3])) (with-out-str (print "?SYNTAX  ERROR IN 100")))) 
    (is (= (with-out-str (dar-error "?ERROR DISK FULL" [100 3])) (with-out-str (print "?ERROR DISK FULL IN 100")))))) 


(deftest variable-float?-test 
  (testing "Devuelve true si es una variable float" 
    (is (variable-float? 'X)))
  (testing "Devuelve false si NO es una variable float"
    (is (not (variable-float? 'X%)))
    (is (not (variable-float? 'X$))))) 


(deftest variable-integer?-test
  (testing "Devuelve true si es una variable integer"
    (is (variable-integer? 'X%)))
  (testing "Devuelve false si NO es una variable integer"
    (is (not (variable-integer? 'X$)))
    (is (not (variable-integer? 'X)))))


(deftest variable-string?-test
  (testing "Devuelve true si es una variable string"
    (is (variable-string? 'X$)))
  (testing "Devuelve false si NO es una variable string"
    (is (not (variable-string? 'X%)))
    (is (not (variable-string? 'X)))))
    

(deftest contar-sentencias-test 
  (testing "Devuelve el numero correcto de sentencias contadas" 
    (is (= (contar-sentencias 10 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}]) 
           2))
    (is (= (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])
           1))
    (is (= (contar-sentencias 20 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])
           2))))


(deftest buscar-lineas-restantes-test 
  (testing "Devuelve lo pedido" 
    (is (= (buscar-lineas-restantes [() [:ejecucion-inmediata 0] [] [] [] 0 {}]) 
           nil))
    (is (= (buscar-lineas-restantes ['((PRINT X) (PRINT Y)) [:ejecucion-inmediata 2] [] [] [] 0 {}])
           nil))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [25 0] [] [] [] 0 {}])
           nil))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 -1] [] [] [] 0 {}])
           '((20))))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 0] [] [] [] 0 {}])
           '((20))))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 1] [] [] [] 0 {}])
           '((20 (NEXT J)))))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 2] [] [] [] 0 {}])
           '((20 (NEXT I) (NEXT J)))))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])
           '((20 (NEXT I) (NEXT J)))))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 0] [] [] [] 0 {}])
           (list (list 15) (list 20 (list 'NEXT 'I (symbol ",") 'J)))))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}])
           (list '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J)))))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 0] [] [] [] 0 {}])
           (list '(10) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J)))))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])
           (list '(10 (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J)))))
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 2] [] [] [] 0 {}])
           (list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))))))) 


(deftest extraer-data-test 
  (testing "retorna una lista con todos los valores embebidos en las sentencias DATA" 
    (is (= (extraer-data '(())) '()))
    (is (= (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))
           '("HOLA" "MUNDO" 10 20))))) 


(deftest ejecutar-asignacion-test
  (testing "retorna el ambiente actualizado al efectuar la asignacion"
    (is (= (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 {}]) 
           '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}]))
    (is (= (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])
           '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}]))
    (is (= (ejecutar-asignacion '(X = X + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])
           '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 3}]))
    (is (= (ejecutar-asignacion '(X$ = X$ + " MUNDO") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])
           '[((10 (PRINT X))) [10 1] [] [] [] 0 {X$ "HOLA MUNDO"}]))))


(deftest preprocesar-expresion-test 
  (testing "recibe una expresion y la retorna con las variables reemplazadas por sus valores y el punto por el cero" 
    (is (= (preprocesar-expresion '(X$ + " MUNDO" + Z$) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}]) 
           '("HOLA" + " MUNDO" + "")))
    (is (= (preprocesar-expresion '(X + . / Y% * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5 Y% 2}])
           '(5 + 0 / 2 * 0)))))


(deftest desambiguar-test
  (testing "recibe un expresion y la retorna sin los + unarios, con los - unarios reemplazados por -u y los MID$ ternarios reemplazados por MID3$"
    (is (= (desambiguar (list '- 2 '* (symbol "(") '- 3 '+ 5 '- (symbol "(") '+ 2 '/ 7 (symbol ")") (symbol ")")))
           (list '-u 2 '* (symbol "(") '-u 3 '+ 5 '- (symbol "(") 2 '/ 7 (symbol ")") (symbol ")"))))
    (is (= (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")))
           (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")))) ;(MID$ ( 1 , 2 ))
    (is (= (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")))
           (list 'MID3$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")"))))
    (is (= (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") '- 2 '+ 'K (symbol ",") 3 (symbol ")")))
           (list 'MID3$ (symbol "(") 1 (symbol ",") '-u 2 '+ 'K (symbol ",") 3 (symbol ")"))))))


(deftest precedencia-test 
  (testing "recibe un token y retorna el valor de su precedencia" 
    (is (= (precedencia 'OR) 1)) 
    (is (= (precedencia 'AND) 2)) 
    (is (= (precedencia '*) 6)) 
    (is (= (precedencia '-u ) 7))
    (is (= (precedencia 'MID$) 8))))


(deftest aridad-test 
  (testing "recibe un token y retorna el valor de su aridad, por ejemplo" 
    (is (= (aridad 'THEN) 0))
    (is (= (aridad 'SIN) 1))
    (is (= (aridad '*) 2))
    (is (= (aridad 'MID$) 2))
    (is (= (aridad 'MID3$) 3)))) 


(deftest eliminar-cero-decimal-test 
  (testing "recibe un numero y lo retorna sin ceros decimales no significativos" 
    (is (= (eliminar-cero-decimal 1.5) 1.5))
    (is (= (eliminar-cero-decimal 1.50) 1.5))
    (is (= (eliminar-cero-decimal 1.0) 1))
    (is (= (eliminar-cero-decimal 'A) 'A)))) 

(deftest eliminar-cero-entero-test 
  (testing "recibe un simbolo y lo retorna convertido en cadena, omitiendo para los numeros del intervalo (-1..1) el cero a la izquierda del punto" 
    (is (= (eliminar-cero-entero nil) nil))
    (is (= (eliminar-cero-entero 'A) "A"))
    (is (= (eliminar-cero-entero 0) " 0"))
    (is (= (eliminar-cero-entero 1.5) " 1.5"))
    (is (= (eliminar-cero-entero 1) " 1"))
    (is (= (eliminar-cero-entero -1) "-1"))
    (is (= (eliminar-cero-entero -1.5) "-1.5"))
    (is (= (eliminar-cero-entero 0.5) " .5"))
    (is (= (eliminar-cero-entero -0.5) "-.5")))) 




