      *Exercice : Créer un programme calculatrice
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Calcul2.
       AUTHOR. ThomasD.

       ENVIRONMENT DIVISION.
      
       DATA DIVISION.
      
      *Création des variables numériques signées avec décimales à saisir par l'utilisateur
       WORKING-STORAGE SECTION.


      *Création de la variable mémoire dans laquelle on va stocker les résultats des différents calculs
       01  WS-MEMORY       PIC S9(10)V999.
      
      *Création de la variable avec laquelle on veut effectuer l'opération choisie
       01  WS-VAR2         PIC S9(10)V999.

      *Création de la variable résultat des différents calculs
       01  WS-RESULT       PIC S9(10)V999.

      *Création de variables d'édition afin d'afficher le résultat sans trop de chiffres inutiles
       01  WS-EDIT-RES     PIC -(10)9(1).9(2).
       01  WS-EDIT-MEM     PIC -(10)9(1).9(2).
       01  WS-EDIT-VAR2    PIC -(10)9(1).9(2).
       

      *Création de la variable de choix de l'opération à effectuer
       01 WS-OPERATION     PIC X(2).
       

      *Création de la variable de choix de poursuite des calculs
       01  WS-CONTINUE              PIC X(2)   VALUE "Y".
           88  WS-CONTINUE-Y                   VALUE "Y" OR "y".
           88  WS-CONTINUE-N                   VALUE "N" OR "n".


      *Création de la variable de choix de suppression des résultats stockés
       01  WS-CLEAR              PIC X(2)   VALUE "N".
           88  WS-CLEAR-Y                   VALUE "Y" OR "y".
           88  WS-CLEAR-N                   VALUE "N" OR "n".



       PROCEDURE DIVISION.

       
      *Affichage des différentes options de saisie

       DISPLAY "Entrez un nombre :".
       PERFORM 0100-AFFI-CALCUL-START
       THRU    0100-AFFI-CALCUL-END.

      *Saisie du premier nombre
       ACCEPT WS-MEMORY.


      *Création de la boucle tant que l'utilisateur souhaite continuer

       PERFORM UNTIL WS-CONTINUE = "N" OR "n"

      *Opérations de calcul 
           PERFORM 0100-CALCUL-START 
           THRU    0100-CALCUL-END

       END-PERFORM.
       STOP RUN.

      ******************************************************************
       
       0100-AFFI-CALCUL-START .
       
           DISPLAY "                           ".
           DISPLAY "   7          8           9".
           DISPLAY "   4          5           6".
           DISPLAY "   1          2           3".
           DISPLAY "  +/-         0           .".
           DISPLAY "                           ".


       0100-AFFI-CALCUL-END .
           EXIT.


      *-----------------------
      *Edition des variables à l'affichage uniquement
       0100-EDITION-START .
           MOVE WS-RESULT TO WS-EDIT-RES.
           MOVE WS-MEMORY TO WS-EDIT-MEM.
           MOVE WS-VAR2 TO WS-EDIT-VAR2.


       0100-EDITION-END .
           EXIT.



      *-----------------------


       0100-CALCUL-START .

      *Affichage des différentes opérations possibles 

           DISPLAY "----------Choisir une opération----------".
           DISPLAY "                                         ".
           DISPLAY "           Addition : +                  ".
           DISPLAY "                                         ".
           DISPLAY "           Soustraction : -              ".
           DISPLAY "                                         ".
           DISPLAY "           Multiplication : *            ".
           DISPLAY "                                         ".
           DISPLAY "           Division : /                  ".
           DISPLAY "                                         ".
           DISPLAY "           Puissance : **                ".
           DISPLAY "                                         ".
           ACCEPT WS-OPERATION.

           
      *Sélection du calcul à effectuer selon l'opération choisie

           EVALUATE WS-OPERATION
               
               WHEN "+" 
                   PERFORM 0110-ADDI-START 
                   THRU    0110-ADDI-END

               WHEN "-" 
                   PERFORM 0110-SOUSTRACT-START 
                   THRU    0110-SOUSTRACT-END

               WHEN "*" 
                   PERFORM 0110-MULTIPLI-START 
                   THRU    0110-MULTIPLI-END

               WHEN "/" 
                   PERFORM 0110-DIVISE-START 
                   THRU    0110-DIVISE-END

               WHEN "**" 
                   PERFORM 0110-PUISSAN-START 
                   THRU    0110-PUISSAN-END


      *Message d'erreur si aucune opération proposée n'est choisie

               WHEN OTHER 
                   DISPLAY "Erreur, choisissez une opération parmi"
                           " celles proposées."
           
           END-EVALUATE.

      *Demande de poursuite du calcul     
           DISPLAY "Continuer ?".
           DISPLAY "Oui : Y/y            "
                   "Non : N/n".

      *Choix de l'utilisateur 

           ACCEPT WS-CONTINUE.

       
      *Différents cas de figure selon la réponse de l'utilisateur 
           EVALUATE TRUE 

               WHEN WS-CONTINUE = "Y " OR "y "
                   SET WS-CONTINUE-Y TO TRUE 

      *Demande de suppression des résultats en mémoire à l'utilisateur si poursuite du calcul

                   DISPLAY "Effacer la valeur en mémoire ? "
                   DISPLAY "Oui : Y/y            "
                   "Non : N/n"

      *Choix de l'utilisateur 
                   ACCEPT WS-CLEAR

                   EVALUATE TRUE 

      *Si non, affichage du résultat stocké en mémoire édité 

                       WHEN WS-CLEAR = "N " OR "n "
                           SET WS-CLEAR-N TO TRUE
                           DISPLAY FUNCTION TRIM (WS-EDIT-MEM)


      *Si oui, la valeur mémoire est remise à 0 

                       WHEN WS-CLEAR = "Y " OR "y "
                           SET WS-CLEAR-Y TO TRUE

                           MOVE 0 TO WS-MEMORY
                           MOVE WS-MEMORY TO WS-EDIT-MEM
                           DISPLAY FUNCTION TRIM (WS-EDIT-MEM)   

      *En cas d'erreur de saisie, message d'erreur et demande réitérée

                       WHEN OTHER
                           PERFORM UNTIL WS-CLEAR = "Y " OR "y " 
                                                         OR "N " 
                                                         OR "n "
                       
                               DISPLAY "Saisie incorrecte"
                               DISPLAY "Effacer la valeur en mémoire ?"
                               DISPLAY "Oui : Y/y            "
                               "Non : N/n"
       
                               ACCEPT WS-CLEAR
                           END-PERFORM 

                   END-EVALUATE

      *Si non, le programme s'arrête 
               WHEN WS-CONTINUE = "N " OR "n "
                   SET WS-CONTINUE-N TO TRUE 


      *En cas d'erreur de saisie, message d'erreur et demande réitérée 

               WHEN OTHER
                   PERFORM UNTIL WS-CONTINUE = "Y " OR "y " 
                                                    OR "N " 
                                                    OR "n "
                       
                       DISPLAY "Saisie incorrecte"
                       DISPLAY "Continuer ?"
                       DISPLAY "Oui : Y/y            "
                       "Non : N/n"
       
                       ACCEPT WS-CONTINUE
                   END-PERFORM
                       
 
                    
           END-EVALUATE. 


           


       0100-CALCUL-END .
           EXIT.

      *----------Différentes opérations possibles----------
      *Addition

       0110-ADDI-START .
           DISPLAY "Entrez un autre nombre :".

           PERFORM 0100-AFFI-CALCUL-START
           THRU    0100-AFFI-CALCUL-END

      *Saisie du deuxième nombre de l'opération par l'utilisateur  
           ACCEPT WS-VAR2.

      *Calcul de l'opération 

           COMPUTE WS-RESULT = WS-MEMORY + WS-VAR2.

                      
           PERFORM 0100-EDITION-START 
           THRU    0100-EDITION-END

      *Affichage du calcul effectué 
           DISPLAY FUNCTION TRIM (WS-EDIT-MEM)
           " + " FUNCTION TRIM (WS-EDIT-VAR2) 
           " = " FUNCTION TRIM (WS-EDIT-RES).

      *Stockage du résultat dans la variable mémoire et affichage après édition
           MOVE WS-RESULT TO WS-MEMORY.
           MOVE WS-EDIT-RES TO WS-EDIT-MEM.
           DISPLAY FUNCTION TRIM (WS-EDIT-MEM).

           
       0110-ADDI-END .
           EXIT.

      *-----------------------
      *Soustraction

       0110-SOUSTRACT-START .
           DISPLAY "Entrez un autre nombre :".

           PERFORM 0100-AFFI-CALCUL-START
           THRU    0100-AFFI-CALCUL-END

           ACCEPT WS-VAR2.

           COMPUTE WS-RESULT = WS-MEMORY - WS-VAR2.

           PERFORM 0100-EDITION-START 
           THRU    0100-EDITION-END

           DISPLAY FUNCTION TRIM (WS-EDIT-MEM)
           " - " FUNCTION TRIM (WS-EDIT-VAR2) 
           " = " FUNCTION TRIM (WS-EDIT-RES).

           MOVE WS-RESULT TO WS-MEMORY.
           MOVE WS-EDIT-RES TO WS-EDIT-MEM.
           DISPLAY FUNCTION TRIM (WS-EDIT-MEM).


       0110-SOUSTRACT-END .
           EXIT.

      *-----------------------
      *Multiplication 

       0110-MULTIPLI-START .
           DISPLAY "Entrez un autre nombre :".

           PERFORM 0100-AFFI-CALCUL-START
           THRU    0100-AFFI-CALCUL-END

           ACCEPT WS-VAR2.

           COMPUTE WS-RESULT = WS-MEMORY * WS-VAR2.

           PERFORM 0100-EDITION-START 
           THRU    0100-EDITION-END

           DISPLAY FUNCTION TRIM (WS-EDIT-MEM)
           " * " FUNCTION TRIM (WS-EDIT-VAR2)
           " = " FUNCTION TRIM (WS-EDIT-RES).

           MOVE WS-RESULT TO WS-MEMORY.
           MOVE WS-EDIT-RES TO WS-EDIT-MEM.
           DISPLAY FUNCTION TRIM (WS-EDIT-MEM).
       

       0110-MULTIPLI-END .
           EXIT.

      *-----------------------
      *Division

       0110-DIVISE-START .
           DISPLAY "Entrez un autre nombre :".

           PERFORM 0100-AFFI-CALCUL-START
           THRU    0100-AFFI-CALCUL-END

           ACCEPT WS-VAR2.

      *Message d'erreur si la deuxième variable saisie pour la division est 0

           IF WS-VAR2 = 0 
               DISPLAY "Impossible de diviser par zéro"
               CONTINUE 

           ELSE 
               COMPUTE WS-RESULT = WS-MEMORY / WS-VAR2

               PERFORM 0100-EDITION-START 
               THRU    0100-EDITION-END

               DISPLAY FUNCTION TRIM (WS-EDIT-MEM)
               " / " FUNCTION TRIM (WS-EDIT-VAR2) 
               " = " FUNCTION TRIM (WS-EDIT-RES)

               MOVE WS-RESULT TO WS-MEMORY
               MOVE WS-EDIT-RES TO WS-EDIT-MEM
               DISPLAY FUNCTION TRIM (WS-EDIT-MEM)
           
           END-IF.


       0110-DIVISE-END .
           EXIT.

       
      *-----------------------
      *Puissance

       0110-PUISSAN-START .
           DISPLAY "Entrez un autre nombre :".

           PERFORM 0100-AFFI-CALCUL-START
           THRU    0100-AFFI-CALCUL-END

           ACCEPT WS-VAR2.

           COMPUTE WS-RESULT = WS-MEMORY ** WS-VAR2.

           PERFORM 0100-EDITION-START 
           THRU    0100-EDITION-END

           DISPLAY FUNCTION TRIM (WS-EDIT-MEM)
           " ** " FUNCTION TRIM (WS-EDIT-VAR2)
           " = " FUNCTION TRIM (WS-EDIT-RES).

           MOVE WS-RESULT TO WS-MEMORY.
           MOVE WS-EDIT-RES TO WS-EDIT-MEM.
           DISPLAY FUNCTION TRIM (WS-EDIT-MEM).

       
       0110-PUISSAN-END .
           EXIT.


       