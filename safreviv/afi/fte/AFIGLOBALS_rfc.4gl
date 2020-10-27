##VALIDA QUE EL NSS SEA NÚMERO Y EL DÍGITO VERIFCICADOR
FUNCTION digito_verif(valor,longitud )
  DEFINE cadena CHAR(20),
                   valor CHAR(10),
                   longitud SMALLINT,
                   suma SMALLINT,
                   sumachar CHAR(2),
                   digito SMALLINT,
                   i,j SMALLINT,
                   temp CHAR(2)
DEFINE ultima	   SMALLINT
DEFINE t SMALLINT 
define x array[10] of char(1)

       LET x[1] =valor[1]
       LET x[2] =valor[2]
       LET x[3] =valor[3]
       LET x[4] =valor[4]
       LET x[5] =valor[5]
       LET x[6] =valor[6]
       LET x[7] =valor[7]
       LET x[8] =valor[8]
       LET x[9] =valor[9]
       LET x[10] =valor[10]

FOR t = 1 TO 10
    IF x[t] <> "0" AND
       x[t] <> "1" AND
       x[t] <> "2" AND
       x[t] <> "3" AND
       x[t] <> "4" AND
       x[t] <> "5" AND
       x[t] <> "6" AND
       x[t] <> "7" AND
       x[t] <> "8" AND
       x[t] <> "9" THEN
	LET digito = 32000
       RETURN  digito
    END IF
END FOR
  LET j = 0
  FOR i = 1 TO longitud
     LET j = j + 1
     IF i MOD 2 = 0 THEN
         LET temp = valor[i] * 2
         LET cadena[j] = temp[1]
         IF LENGTH(temp) > 1 THEN
             LET j = j + 1
             LET cadena[j] = temp[2]
         END IF
    ELSE
         LET cadena[j] = valor[i]
    END IF
  END FOR

  LET suma = 0
  FOR i = 1 TO j
     LET suma = suma + cadena[i]
  END FOR

  LET sumachar = suma
let ultima = LENGTH(sumachar)  

  LET digito = 10 - sumachar[ultima]  

  IF digito = 10 THEN
      LET digito = 0
  END IF

  RETURN digito  

END FUNCTION

FUNCTION valida_fecha_rfc(a)

define a char(6)
define x array[6] of char(1)
define t smallint

       LET x[1] =a[1]
       LET x[2] =a[2]
       LET x[3] =a[3]
       LET x[4] =a[4]
       LET x[5] =a[5]
       LET x[6] =a[6]

FOR t = 1 TO 6
    IF x[t] <> "0" AND
       x[t] <> "1" AND
       x[t] <> "2" AND
       x[t] <> "3" AND
       x[t] <> "4" AND
       x[t] <> "5" AND
       x[t] <> "6" AND
       x[t] <> "7" AND
       x[t] <> "8" AND
       x[t] <> "9" THEN
       RETURN  FALSE
    END IF
END FOR
return true
end function

## VERIFICA QUE EL NOMBRE COMPLETO NO TENGA "XXX" O MAS DE UN ESPACIO

FUNCTION arma_clave(paterno, materno, nombres, fena, estadon, sexo)
#ac----------------------------------------------------------------
   DEFINE paterno, materno, nombres     CHAR(40),
          fena                          DATE    ,
          sexo                          SMALLINT,
          estadon                       SMALLINT,
          sexo1                         CHAR(01),
          fena1                         CHAR(06),
          pa_t1, ma_t1, no_t1           CHAR(40),
          pa_t,  ma_t,  no_t            CHAR(02),
          pater,  pater1, pater2        CHAR(40),
          pater3, pater4, pater5        CHAR(40),
          pa_papa, ma_mama              CHAR(40),
          patmat                        CHAR(03),
          mater , mater1, mater2,
          mater3, mater4, mater5        CHAR(40),
          patmatnom, patmatnom1         CHAR(04),
          nom_b , nom_b1, nom_b2,
          nom_b3, nom_b4, nom_b5        CHAR(40),
          cve_mex                       CHAR(02),
          ent_fed1                      CHAR(02),
          cve_cur                       CHAR(17),
          pa_pa, ma_ma, no_no           CHAR(01),
          ch_ll                         CHAR(02),
          consonante                    CHAR(03),

          bla, ban, i, long, bb, j      SMALLINT,
          enter                         CHAR(1)

   INITIALIZE pa_t1, ma_t1, no_t1 TO NULL
   INITIALIZE pa_t,  ma_t,  no_t  TO NULL
   INITIALIZE pa_pa, ma_ma, no_no, consonante, ch_ll TO NULL
   INITIALIZE pater, pater1, pater2, pater3, pater4, pater5, pa_papa  TO NULL

   LET long = 0  LET i = 0   LET bb = 0   LET j = 0

##paterno

   LET j = 1
   LET pa_t1 = paterno CLIPPED
   LET long = LENGTH(pa_t1 CLIPPED)
   LET long = long + 1

   FOR i = 1 TO long
       IF pa_t1[i,i] = " " THEN
          LET bla = bla + 1
          CASE bla
             WHEN 1
                    LET pater1 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater1
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 1
                       EXIT FOR
                    END IF 
             WHEN 2
                    LET pater2 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater2
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 2
                       EXIT FOR
                    END IF 
             WHEN 3
                    LET pater3 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater3
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 3
                       EXIT FOR
                    END IF 
             WHEN 4
                    LET pater4 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater4
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 4
                       EXIT FOR
                    END IF 
             WHEN 5
                    LET pater5 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater5
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 5
                       EXIT FOR
                    END IF 
          END CASE 
       ELSE
           LET pater[j,j] = pa_t1[i,i]
           LET bb = 6
           LET j = j + 1
       END IF
   END FOR 

   CASE bb
        WHEN 1 LET pa_t1 = pater1
        WHEN 2 LET pa_t1 = pater2
        WHEN 3 LET pa_t1 = pater3
        WHEN 4 LET pa_t1 = pater4
        WHEN 5 LET pa_t1 = pater5
        WHEN 6 LET pa_t1 = pater
   END CASE
   IF pa_t1 IS NULL OR pa_t1 = " " THEN
      LET pa_t1 = pater1 CLIPPED
   END IF

   LET pa_papa = pa_t1 CLIPPED

   LET j = 1
   FOR i = 1 TO long
       IF j = 1 THEN
          LET pa_t[j,j] = pa_t1[i,i]
          IF pa_t[j,j] = "Ñ" OR pa_t[j,j] = "ñ" THEN
             LET pa_t[j,j] = "X"
          END IF
          IF pa_t[j,j] = "C" OR pa_t[j,j] = "L" THEN
             LET ch_ll[j,j] = pa_t[j,j]
          END IF
          LET j = j + 1

       ELSE

          IF pa_t1[i,i] MATCHES "[AEIOU]" THEN
             LET pa_t[j,j] = pa_t1[i,i]

             IF j = 2 THEN
                EXIT FOR
             END IF
{
          ELSE
             LET ch_ll[j,j] = pa_t1[i,i]
             IF ch_ll = "CH" OR ch_ll[j,j] = "LL" THEN
                LET j = 2 
             ELSE
                 LET pa_t[j,j] = pa_t1[i,i]
                 IF j = 2 THEN
                    EXIT FOR
                 END IF
             END IF
}
          END IF
       END IF
   END FOR 

   LET j = 1
   FOR i = 1 TO long
      LET pa_pa[j,j] = pa_papa[i,i]
      IF i > 1 THEN
         IF pa_pa[j,j] NOT MATCHES "[AEIOU]" THEN
            IF pa_pa[j,j] = "Ñ" OR pa_pa[j,j] = "ñ" THEN
               LET pa_pa = "X"
               EXIT FOR
            ELSE
               LET pa_pa = pa_papa[i,i]
               EXIT FOR
            END IF
         END IF
      END IF
   END FOR

## materno
   INITIALIZE mater, mater1, mater2, mater3, mater4, mater5  TO NULL

   LET long = 0  LET i = 0   LET bb = 0  LET bla = 0  LET j = 1

   LET ma_t1 = materno CLIPPED
   LET long = LENGTH(ma_t1 CLIPPED)

   IF long IS NULL OR long = 0 THEN
      LET ma_t = "X"
      LET patmat = pa_t CLIPPED, ma_t CLIPPED
      LET ma_ma = "X"

   ELSE
         FOR i = 1 TO long
             IF ma_t1[i,i] = " " THEN
                LET bla = bla + 1
                CASE bla
                   WHEN 1
                          LET mater1 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater1
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 1
                             EXIT FOR
                          END IF 
                   WHEN 2
                          LET mater2 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater2
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 2
                             EXIT FOR
                          END IF 
                   WHEN 3
                          LET mater3 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater3
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 3
                             EXIT FOR
                          END IF 
                   WHEN 4
                          LET mater4 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                               WHERE palabra = mater4
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 4
                             EXIT FOR
                          END IF 
                   WHEN 5
                          LET mater5 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater5
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 5
                             EXIT FOR
                          END IF 
                END CASE 
             ELSE
                 LET mater[j,j] = ma_t1[i,i]
                 LET bb = 6
                 LET j  = j + 1
             END IF
         END FOR 
      
         CASE bb
              WHEN 1 LET ma_t1 = mater1
              WHEN 2 LET ma_t1 = mater2
              WHEN 3 LET ma_t1 = mater3
              WHEN 4 LET ma_t1 = mater4
              WHEN 5 LET ma_t1 = mater5
              WHEN 6 LET ma_t1 = mater
         END CASE
         IF ma_t1  IS NULL OR ma_t1 = " " THEN
            LET ma_t1 = mater1
         END IF
      
         LET ma_mama = ma_t1 CLIPPED
         FOR i = 1 TO long
             IF i = 1 THEN
                LET ma_t[i,i] = ma_t1[i,i]
                IF ma_t[i,i] = "Ñ" OR ma_t[i,i] = "ñ" THEN
                   LET ma_t[i,i] = "X"
                END IF
                EXIT FOR
             END IF
         END FOR 

         LET j = 1
         FOR i = 1 TO long
            LET ma_ma[j,j] = ma_mama[i,i]
            IF i > 1 THEN
               IF ma_ma[j,j] NOT MATCHES "[AEIOU]" THEN
                  IF ma_ma[j,j] = "Ñ" OR ma_ma[j,j] = "ñ" THEN
                     LET ma_ma = "X"
                     EXIT FOR
                  ELSE
                     LET ma_ma = ma_mama[i,i]
                     EXIT FOR
                  END IF
               END IF
            END IF
         END FOR
   END IF
   
## nombres
   INITIALIZE nom_b, nom_b1, nom_b2, nom_b3, nom_b4, nom_b5  TO NULL

   LET long = 0  LET i = 0   LET bb = 0  LET bla = 0  LET j = 1

   LET no_t1 = nombres CLIPPED
   LET long = LENGTH(no_t1 CLIPPED)
##   LET long = long + 1
   FOR i = 1 TO long
       IF no_t1[i,i] = " " THEN
          LET bla = bla + 1
          CASE bla
             WHEN 1
                    LET nom_b1 = nom_b CLIPPED
                    LET nom_b1 = nom_b1 CLIPPED
                    SELECT "f.X" FROM afi_articulo f
                            WHERE f.palabra MATCHES nom_b1
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 1
                       EXIT FOR
                    END IF 
             WHEN 2
                    LET nom_b2 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b2
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 2
                       EXIT FOR
                    END IF 
             WHEN 3
                    LET nom_b3 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b3
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 3
                       EXIT FOR
                    END IF 
             WHEN 4
                    LET nom_b4 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b4
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 4
                       EXIT FOR
                    END IF 
             WHEN 5
                    LET nom_b5 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b5
                    IF STATUS != NOTFOUND THEN
                       LET j = 1
                       INITIALIZE nom_b TO NULL
                    ELSE
                       LET bb = 5
                       EXIT FOR
                    END IF 
          END CASE 
       ELSE
           LET nom_b[j,j] = no_t1[i,i]
           LET bb = 6
           LET j  = j + 1
       END IF
   END FOR 

   CASE bb
        WHEN 1 LET no_t1 = nom_b1
        WHEN 2 LET no_t1 = nom_b2
        WHEN 3 LET no_t1 = nom_b3
        WHEN 4 LET no_t1 = nom_b4
        WHEN 5 LET no_t1 = nom_b5
        WHEN 6 LET no_t1 = nom_b
   END CASE

   IF no_t1 IS NULL OR no_t1 = " " THEN
      LET nom_b1 = no_t1
   END IF

   FOR i = 1 TO long
     IF i = 1 THEN
        LET no_t[i,i] = no_t1[i,i]
        IF no_t[i,i] = "Ñ" OR no_t[i,i] = "ñ" THEN
           LET no_t[i,i] = "X"
        END IF
        EXIT FOR
     END IF
   END FOR 
   LET patmatnom = pa_t CLIPPED, ma_t CLIPPED, no_t CLIPPED

   SELECT b.palabra_si INTO patmatnom1 FROM afi_no_conviene b
          WHERE palabra_no = patmatnom
   IF STATUS != NOTFOUND THEN
      LET patmatnom = patmatnom1 CLIPPED
   END IF

   LET j = 1
   FOR i = 1 TO long
      LET no_no[j,j] = no_t1[i,i]
      IF i > 1 THEN
         IF no_no[j,j] MATCHES "[AEIOU]" THEN
            DISPLAY ""
         ELSE
            IF no_no[j,j] = "¥" OR no_no[j,j] = "¤" THEN
               LET no_no = "X"
               EXIT FOR
            ELSE
               LET no_no = no_t1[i,i]
               EXIT FOR
            END IF
         END IF
      ELSE
           DISPLAY ""
      END IF
   END FOR


## consonantes
 LET consonante = pa_pa CLIPPED, ma_ma CLIPPED, no_no CLIPPED

## cve_cur
   LET cve_cur = patmatnom CLIPPED, fena1 CLIPPED, sexo1 CLIPPED, 
                 ent_fed1  CLIPPED, consonante CLIPPED

   RETURN cve_cur

END FUNCTION

# Funcion que verif.que el nombre completo del RFC

FUNCTION arma_clave_rfc(paterno, materno, nombres, fena)
#ac----------------------------------------------------------------
   DEFINE paterno, materno, nombres     CHAR(40),
          fena                          DATE    ,
          sexo                          SMALLINT,
          estadon                       SMALLINT,
          sexo1                         CHAR(01),
          fena1                         CHAR(06),
          pa_t1, ma_t1, no_t1           CHAR(40),
          pa_t,  ma_t,  no_t            CHAR(02),
          pater,  pater1, pater2        CHAR(40),
          pater3, pater4, pater5        CHAR(40),
          pa_papa, ma_mama              CHAR(40),
          patmat                        CHAR(03),
          mater , mater1, mater2,
          mater3, mater4, mater5        CHAR(40),
          patmatnom, patmatnom1         CHAR(04),
          nom_b , nom_b1, nom_b2,
          nom_b3, nom_b4, nom_b5        CHAR(40),
          cve_mex                       CHAR(02),
          ent_fed1                      CHAR(02),
          cve_cur                       CHAR(17),
          pa_pa, ma_ma, no_no           CHAR(01),
          ch_ll                         CHAR(02),
          consonante                    CHAR(03),

          bla, ban, i, long, bb, j      SMALLINT

   DEFINE enter char(1)

   INITIALIZE pa_t1, ma_t1, no_t1 TO NULL
   INITIALIZE pa_t,  ma_t,  no_t  TO NULL
   INITIALIZE pa_pa, ma_ma, no_no, consonante, ch_ll TO NULL
   INITIALIZE pater, pater1, pater2, pater3, pater4, pater5, pa_papa  TO NULL

   LET long = 0  LET i = 0   LET bb = 0   LET j = 0

##paterno

   LET j = 1
   LET pa_t1 = paterno CLIPPED
   LET long = LENGTH(pa_t1 CLIPPED)
   LET long = long + 1

   FOR i = 1 TO long
       IF pa_t1[i,i] = " " THEN
          LET bla = bla + 1
          CASE bla
             WHEN 1
                    IF i = 2 THEN
                       LET pater[2,2] = "X"
                       EXIT FOR
                    END IF
                    
                    LET pater1 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater1
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 1
                       EXIT FOR
                    END IF 
             WHEN 2
                    LET pater2 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater2
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 2
                       EXIT FOR
                    END IF 
             WHEN 3
                    LET pater3 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater3
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 3
                       EXIT FOR
                    END IF 
             WHEN 4
                    LET pater4 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater4
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 4
                       EXIT FOR
                    END IF 
             WHEN 5
                    LET pater5 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater5
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 5
                       EXIT FOR
                    END IF 
          END CASE 
       ELSE
           LET pater[j,j] = pa_t1[i,i]
           LET bb = 6
           LET j = j + 1
       END IF
   END FOR 

   CASE bb
        WHEN 1 LET pa_t1 = pater1
        WHEN 2 LET pa_t1 = pater2
        WHEN 3 LET pa_t1 = pater3
        WHEN 4 LET pa_t1 = pater4
        WHEN 5 LET pa_t1 = pater5
        WHEN 6 LET pa_t1 = pater
   END CASE

   IF pa_t1 IS NULL OR pa_t1 = " " THEN
      LET pa_t1 = pater1 CLIPPED
   END IF

   LET j = 1
   FOR i = 1 TO long
       IF j = 1 THEN
          LET pa_t[j,j] = pa_t1[i,i]
          IF pa_t[j,j] = "Ñ" OR pa_t[j,j] = "ñ" THEN
             LET pa_t[j,j] = "X"
          END IF
{
          IF pa_t[j,j] = "C" OR pa_t[j,j] = "L" THEN
             LET ch_ll[j,j] = pa_t[j,j]
          END IF
}
          LET j = j + 1

       ELSE

          IF pa_t1[i,i] MATCHES "[AEIOU]" THEN
             LET pa_t[j,j] = pa_t1[i,i]

             IF j = 2 THEN
                EXIT FOR
             END IF
          END IF
       END IF
   END FOR 

{
   LET j = 1
   FOR i = 1 TO long
      LET pa_pa[j,j] = pa_papa[i,i]
      IF i > 1 THEN
         IF pa_pa[j,j] NOT MATCHES "[AEIOU]" THEN
            IF pa_pa[j,j] = "Ñ" OR pa_pa[j,j] = "ñ" THEN
               LET pa_pa = "X"
               EXIT FOR
            ELSE
               LET pa_pa = pa_papa[i,i]
               EXIT FOR
            END IF
         END IF
      END IF
   END FOR
}

## materno
   INITIALIZE mater, mater1, mater2, mater3, mater4, mater5  TO NULL

   LET long = 0  LET i = 0   LET bb = 0  LET bla = 0  LET j = 1

   LET ma_t1 = materno CLIPPED
   LET long = LENGTH(ma_t1 CLIPPED)

   IF long IS NULL OR long = 0 THEN
      LET ma_t = "X"
      LET patmat = pa_t CLIPPED, ma_t CLIPPED
      LET ma_ma = "X"

   ELSE
         FOR i = 1 TO long
             IF ma_t1[i,i] = " " THEN
                LET bla = bla + 1
                CASE bla
                   WHEN 1
                          LET mater1 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater1
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 1
                             EXIT FOR
                          END IF 
                   WHEN 2
                          LET mater2 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater2
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 2
                             EXIT FOR
                          END IF 
                   WHEN 3
                          LET mater3 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater3
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 3
                             EXIT FOR
                          END IF 
                   WHEN 4
                          LET mater4 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                               WHERE palabra = mater4
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 4
                             EXIT FOR
                          END IF 
                   WHEN 5
                          LET mater5 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater5
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 5
                             EXIT FOR
                          END IF 
                END CASE 
             ELSE
                 LET mater[j,j] = ma_t1[i,i]
                 LET bb = 6
                 LET j  = j + 1
             END IF
         END FOR 
      
         CASE bb
              WHEN 1 LET ma_t1 = mater1
              WHEN 2 LET ma_t1 = mater2
              WHEN 3 LET ma_t1 = mater3
              WHEN 4 LET ma_t1 = mater4
              WHEN 5 LET ma_t1 = mater5
              WHEN 6 LET ma_t1 = mater
         END CASE
         IF ma_t1  IS NULL OR ma_t1 = " " THEN
            LET ma_t1 = mater1
         END IF
      
         LET ma_mama = ma_t1 CLIPPED
         FOR i = 1 TO long
             IF i = 1 THEN
                LET ma_t[i,i] = ma_t1[i,i]
                IF ma_t[i,i] = "Ñ" OR ma_t[i,i] = "ñ" THEN
                   LET ma_t[i,i] = "X"
                END IF
                EXIT FOR
             END IF
         END FOR 

{
         LET j = 1
         FOR i = 1 TO long
            LET ma_ma[j,j] = ma_mama[i,i]
            IF i > 1 THEN
               IF ma_ma[j,j] NOT MATCHES "[AEIOU]" THEN
                  IF ma_ma[j,j] = "Ñ" OR ma_ma[j,j] = "ñ" THEN
                     LET ma_ma = "X"
                     EXIT FOR
                  ELSE
                     LET ma_ma = ma_mama[i,i]
                     EXIT FOR
                  END IF
               END IF
            END IF
         END FOR
}
   END IF
   
## nombres
   INITIALIZE nom_b, nom_b1, nom_b2, nom_b3, nom_b4, nom_b5  TO NULL

   LET long = 0  LET i = 0   LET bb = 0  LET bla = 0  LET j = 1

   LET no_t1 = nombres CLIPPED
   LET long = LENGTH(no_t1 CLIPPED)
##   LET long = long + 1
   FOR i = 1 TO long
       IF no_t1[i,i] = " " THEN
          LET bla = bla + 1
          CASE bla
             WHEN 1
                    LET nom_b1 = nom_b CLIPPED
                    LET nom_b1 = nom_b1 CLIPPED
                    SELECT "f.X" FROM afi_articulo f
                            WHERE f.palabra MATCHES nom_b1
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 1
                       EXIT FOR
                    END IF 
             WHEN 2
                    LET nom_b2 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b2
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 2
                       EXIT FOR
                    END IF 
             WHEN 3
                    LET nom_b3 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b3
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 3
                       EXIT FOR
                    END IF 
             WHEN 4
                    LET nom_b4 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b4
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 4
                       EXIT FOR
                    END IF 
             WHEN 5
                    LET nom_b5 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b5
                    IF STATUS != NOTFOUND THEN
                       LET j = 1
                       INITIALIZE nom_b TO NULL
                    ELSE
                       LET bb = 5
                       EXIT FOR
                    END IF 
          END CASE 
       ELSE
           LET nom_b[j,j] = no_t1[i,i]
           LET bb = 6
           LET j  = j + 1
       END IF
   END FOR 

   CASE bb
        WHEN 1 LET no_t1 = nom_b1
        WHEN 2 LET no_t1 = nom_b2
        WHEN 3 LET no_t1 = nom_b3
        WHEN 4 LET no_t1 = nom_b4
        WHEN 5 LET no_t1 = nom_b5
        WHEN 6 LET no_t1 = nom_b
   END CASE

   IF no_t1 IS NULL OR no_t1 = " " THEN
      LET nom_b1 = no_t1
   END IF


   FOR i = 1 TO long
     IF 1 = 1 THEN
        LET no_t[i,i] = no_t1[i,i]
        IF no_t[i,i] = "Ñ" OR no_t[i,i] = "ñ" THEN
           LET no_t[i,i] = "X"
        END IF
        EXIT FOR
     END IF
   END FOR 
   LET patmatnom = pa_t CLIPPED, ma_t CLIPPED, no_t CLIPPED

   SELECT b.palabra_si INTO patmatnom1 FROM afi_no_conviene b
          WHERE palabra_no = patmatnom
   IF STATUS != NOTFOUND THEN
      LET patmatnom = patmatnom1 CLIPPED
   END IF

{
   LET j = 1
   FOR i = 1 TO long
      LET no_no[j,j] = no_t1[i,i]
      IF i > 1 THEN
         IF no_no[j,j] MATCHES "[AEIOU]" THEN
            DISPLAY ""
         ELSE
            IF no_no[j,j] = "¥" OR no_no[j,j] = "¤" THEN
               LET no_no = "X"
               EXIT FOR
            ELSE
               LET no_no = no_t1[i,i]
               EXIT FOR
            END IF
         END IF
      ELSE
           DISPLAY ""
      END IF
   END FOR
} 
##fecha nacimiento
   LET fena1 = fena USING "YYMMDD"

## cve_cur
   LET cve_cur = patmatnom CLIPPED, fena1 CLIPPED

   RETURN cve_cur
 
END FUNCTION

#VERIFICA QUE EL NOMBRE NO TENGA CARACTERES ESPECIALES

FUNCTION verifica_nombre(no_bre)
#ve---------------------------------
  DEFINE no_bre          CHAR(40),
         bl1, bl2, bl3   SMALLINT,
         i,  long, var   SMALLINT,
         vval            CHAR(80),
         espe            CHAR(40)

  INITIALIZE vval TO NULL
  LET bl1 = 0   LET bl2 = 0  LET bl3 = 0  
  LET i = 0     LET long = 0 LET var = 0

  LET long   = LENGTH(no_bre CLIPPED)
  IF no_bre[1,3] MATCHES "XXX" THEN
     LET bl3 = 1
  END IF

  FOR i = 1 TO long
      IF i < 40 THEN
         IF no_bre[i,i+1] = "  " THEN
            LET bl1 = 1
         END IF
      END IF
  END FOR 

  INITIALIZE espe TO NULL
  LET i = 0
  FOR i = 1 TO long
      IF no_bre[i,i] = "[" OR no_bre[i,i] = '"'  OR
         --no_bre[i,i] = "]" OR no_bre[i,i] = "#"  OR
         no_bre[i,i] = "]" OR
         no_bre[i,i] = "$" OR no_bre[i,i] = "%"  OR
         no_bre[i,i] = "&" OR no_bre[i,i] = "="  OR
         no_bre[i,i] = "/" OR no_bre[i,i] = "?"  OR
         no_bre[i,i] = "-" OR no_bre[i,i] = "'"  OR
         no_bre[i,i] = "(" OR no_bre[i,i] = ")"  OR
         no_bre[i,i] = "^" OR no_bre[i,i] = "!"  OR
         no_bre[i,i] = "~" OR no_bre[i,i] = "_"  OR
         --no_bre[i,i] = "." OR no_bre[i,i] = ":"  OR
         no_bre[i,i] = ":" OR
         no_bre[i,i] = "," OR no_bre[i,i] = ";"  OR
         no_bre[i,i] = "<" OR no_bre[i,i] = ">"  OR
         no_bre[i,i] = "@" OR no_bre[i,i] = "|"  OR
         no_bre[i,i] = "{" OR no_bre[i,i] = "}"  OR
         no_bre[i,i] = "+" OR no_bre[i,i] = "*"  OR
         no_bre[i,i] = "`" OR no_bre[i,i] = "1"  OR
         no_bre[i,i] = "2" OR no_bre[i,i] = "3"  OR
         no_bre[i,i] = "4" OR no_bre[i,i] = "5"  OR
         no_bre[i,i] = "6" OR no_bre[i,i] = "7"  OR
         no_bre[i,i] = "8" OR no_bre[i,i] = "9"  OR
         no_bre[i,i] = "0" OR no_bre[i,i] = "¿"  OR
         no_bre[i,i] = "¡" OR no_bre[i,i] = "Ä"  OR
         no_bre[i,i] = "É" OR no_bre[i,i] = "Í"  OR
         no_bre[i,i] = "Ó" OR no_bre[i,i] = "Ú"  OR
         no_bre[i,i] = "¨" OR no_bre[i,i] = "Ä"  OR
         no_bre[i,i] = "Ë" OR no_bre[i,i] = "Ï"  OR
         no_bre[i,i] = "Ö" OR no_bre[i,i] = "Ö"  OR
         no_bre[i,i] = "Ü" OR no_bre[i,i] = "á"  OR
         no_bre[i,i] = "é" OR no_bre[i,i] = "í"  OR
         no_bre[i,i] = "ó" OR no_bre[i,i] = "ú"  OR
         no_bre[i,i] = "ä" OR no_bre[i,i] = "ë"  OR
         no_bre[i,i] = "ï" OR no_bre[i,i] = "ö"  OR
         no_bre[i,i] = "ü" OR no_bre[i,i] = "´"  OR
         no_bre[i,i] = "Á" THEN

         LET espe[i,i] = no_bre[i,i]
         LET bl2 = 1
         EXIT FOR
      END IF
  END FOR

  IF bl1 = 1 THEN
     LET vval = "tiene mas de 1 espacio "
     LET var = 1
  END IF

  IF bl2 = 1 THEN
     LET long = 0
     LET long = LENGTH(vval CLIPPED)
     IF long > 1 THEN
         LET vval = vval CLIPPED ,", caracteres especiales "
         LET var = 1
     ELSE
         LET vval = vval CLIPPED ," tiene caracteres especiales "
         LET var = 1
     END IF
  END IF

  IF bl3 = 1 THEN
     LET long = 0
     LET long = LENGTH(vval CLIPPED)
     IF long > 1 THEN
        LET vval = vval  CLIPPED,", comienza con XXX "
        LET var = 1
     ELSE
        LET vval = vval  CLIPPED," no debe comenzar con XXX "
        LET var = 1
     END IF
  END IF

  IF bl1 = 0  AND bl2 = 0 AND bl3 = 0 THEN
     LET var = 0
     LET vval = "                 "
  END IF

  RETURN var, vval
END FUNCTION

#VERIFICA QUE EL RFC NO TENGA CARACTERES ESPECIALES

FUNCTION verifica_rfc(r_f_c)
#ve---------------------------------
  DEFINE r_f_c           CHAR(4),
         bl1, bl2        SMALLINT,
         i,  long, var   SMALLINT,
         vval            CHAR(80),
         espe            CHAR(4)

  INITIALIZE vval TO NULL
  LET bl1 = 0   LET bl2 = 0 
  LET i = 0     LET long = 0 LET var = 0

  LET long   = LENGTH(r_f_c CLIPPED)

  FOR i = 1 TO long
      IF i < 4 THEN
         IF r_f_c[i,i+1] = " " THEN
            LET bl1 = 1
         END IF
      END IF
  END FOR 

  INITIALIZE espe TO NULL
  LET i = 0
  FOR i = 1 TO long
      IF r_f_c[i,i] = "[" OR r_f_c[i,i] = '"'  OR
         r_f_c[i,i] = "]" OR r_f_c[i,i] = "#"  OR
         r_f_c[i,i] = "$" OR r_f_c[i,i] = "%"  OR
         r_f_c[i,i] = "&" OR r_f_c[i,i] = "="  OR
         r_f_c[i,i] = "/" OR r_f_c[i,i] = "?"  OR
         r_f_c[i,i] = "-" OR r_f_c[i,i] = "'"  OR
         r_f_c[i,i] = "(" OR r_f_c[i,i] = ")"  OR
         r_f_c[i,i] = "^" OR r_f_c[i,i] = "!"  OR
         r_f_c[i,i] = "~" OR r_f_c[i,i] = "_"  OR
         r_f_c[i,i] = "." OR r_f_c[i,i] = ":"  OR
         r_f_c[i,i] = "," OR r_f_c[i,i] = ";"  OR
         r_f_c[i,i] = "<" OR r_f_c[i,i] = ">"  OR
         r_f_c[i,i] = "@" OR r_f_c[i,i] = "|"  OR
         r_f_c[i,i] = "{" OR r_f_c[i,i] = "}"  OR
         r_f_c[i,i] = "+" OR r_f_c[i,i] = "*"  OR
         r_f_c[i,i] = "`" OR r_f_c[i,i] = "1"  OR
         r_f_c[i,i] = "2" OR r_f_c[i,i] = "3"  OR
         r_f_c[i,i] = "4" OR r_f_c[i,i] = "5"  OR
         r_f_c[i,i] = "6" OR r_f_c[i,i] = "7"  OR
         r_f_c[i,i] = "8" OR r_f_c[i,i] = "9"  OR
         r_f_c[i,i] = "0" OR r_f_c[i,i] = "¿"  OR
         r_f_c[i,i] = "¡" OR r_f_c[i,i] = "Ä"  OR
         r_f_c[i,i] = "É" OR r_f_c[i,i] = "Í"  OR
         r_f_c[i,i] = "Ó" OR r_f_c[i,i] = "Ú"  OR
         r_f_c[i,i] = "¨" OR r_f_c[i,i] = "Ä"  OR
         r_f_c[i,i] = "Ë" OR r_f_c[i,i] = "Ï"  OR
         r_f_c[i,i] = "Ö" OR r_f_c[i,i] = "Ö"  OR
         r_f_c[i,i] = "Ü" OR r_f_c[i,i] = "á"  OR
         r_f_c[i,i] = "é" OR r_f_c[i,i] = "í"  OR
         r_f_c[i,i] = "ó" OR r_f_c[i,i] = "ú"  OR
         r_f_c[i,i] = "ä" OR r_f_c[i,i] = "ë"  OR
         r_f_c[i,i] = "ï" OR r_f_c[i,i] = "ö"  OR
         r_f_c[i,i] = "ü" OR r_f_c[i,i] = "´"  OR
         r_f_c[i,i] = "Á" OR r_f_c[i,i] = " "  THEN

         LET espe[i,i] = r_f_c[i,i]
         LET bl2 = 1
         EXIT FOR
      END IF
  END FOR

  IF bl1 = 1 THEN
     LET vval = "tiene espacio "
     LET var = 1
  END IF

  IF bl2 = 1 THEN
     LET long = 0
     LET long = LENGTH(vval CLIPPED)
     IF long > 1 THEN
         LET vval = vval CLIPPED ,", caracteres especiales  o espacio"
         LET var = 1
     ELSE
         LET vval = vval CLIPPED ," tiene caracteres especiales  o espacio"
         LET var = 1
     END IF
  END IF
 
  IF bl1 = 0  AND bl2 = 0  THEN
     LET var = 0
     LET vval = "                 "
  END IF

  RETURN var, vval
END FUNCTION

#VALIDA ESTRUCUTRA DE RFC

FUNCTION valida_est_rfc(rfc)
#ver------------------------

  DEFINE
     rfc        CHAR(18),
     arr_rfc    ARRAY[18] OF RECORD
                rfc_pos CHAR(1)
     END RECORD,
     i          SMALLINT,
     arr_letr   ARRAY[27] OF RECORD
                car             CHAR(1)
     END RECORD,
     j          SMALLINT,
     arr_nume   ARRAY[10] OF RECORD
                num             CHAR(1)
    END RECORD,
     k          SMALLINT,
     pasa       CHAR(1),
     contador1  SMALLINT,
     contador2  SMALLINT,
     contador3  SMALLINT,
     contador4  SMALLINT,
     contador5  SMALLINT,
     desc_err   CHAR(60),
     desp_err   SMALLINT

   LET pasa = 0

   ### SEPARA RFC POR POSICIONES
   LET arr_rfc [01].rfc_pos = rfc [01]  LET arr_rfc [02].rfc_pos = rfc [02]
   LET arr_rfc [03].rfc_pos = rfc [03]  LET arr_rfc [04].rfc_pos = rfc [04]
   LET arr_rfc [05].rfc_pos = rfc [05]  LET arr_rfc [06].rfc_pos = rfc [06]
   LET arr_rfc [07].rfc_pos = rfc [07]  LET arr_rfc [08].rfc_pos = rfc [08]
   LET arr_rfc [09].rfc_pos = rfc [09]  LET arr_rfc [10].rfc_pos = rfc [10]
   LET arr_rfc [11].rfc_pos = rfc [11]  LET arr_rfc [12].rfc_pos = rfc [12]
   LET arr_rfc [13].rfc_pos = rfc [13]

   ### INICIALIZA ARREGLO CON VALORES ALFABETICOS
   LET arr_letr[01].car = 'A'  LET arr_letr[02].car = 'B'
   LET arr_letr[03].car = 'C'  LET arr_letr[04].car = 'D'
   LET arr_letr[05].car = 'E'  LET arr_letr[06].car = 'F'
   LET arr_letr[07].car = 'G'  LET arr_letr[08].car = 'H'
   LET arr_letr[09].car = 'I'  LET arr_letr[10].car = 'J'
   LET arr_letr[11].car = 'K'  LET arr_letr[12].car = 'L'
   LET arr_letr[13].car = 'M'  LET arr_letr[14].car = 'N'
   LET arr_letr[15].car = 'Ñ'  LET arr_letr[16].car = 'O'
   LET arr_letr[17].car = 'P'  LET arr_letr[18].car = 'Q'
   LET arr_letr[19].car = 'R'  LET arr_letr[20].car = 'S'
   LET arr_letr[21].car = 'T'  LET arr_letr[22].car = 'U'
   LET arr_letr[23].car = 'V'  LET arr_letr[24].car = 'W'
   LET arr_letr[25].car = 'X'  LET arr_letr[26].car = 'Y'
   LET arr_letr[27].car = 'Z'

   ### INICIALIZA ARREGLO CON VALORES NUMERICOS
   LET k = 0
   FOR k = 1 TO 9
     LET arr_nume[k].num = k
   END FOR
   LET arr_nume[10].num = 0

   ### Valida rfc
   LET i         = 0
   LET j         = 0
   LET k         = 0
   LET contador1 = 0
   LET contador2 = 0
   LET desp_err  = 0

   FOR i = 1 TO 13

     ### Valida letras (Pos 1 a 4)
     IF i >= 1 AND i <= 4 THEN
        FOR j = 1 TO 27
           IF arr_rfc [i].rfc_pos = arr_letr[j].car THEN
              LET contador1 = contador1 + 1
           END IF
        END FOR
     END IF

     ### Valida numeros (Pos 5 a 10)
     IF i >= 5 AND i <= 10 THEN
        FOR k = 1 TO 10
           IF arr_rfc [i].rfc_pos = arr_nume[k].num THEN
              LET contador2 = contador2 + 1
           END IF
        END FOR
     END IF

   END FOR

   IF contador1 < 04 THEN
      LET pasa = 1
      LET desc_err = "Error en las primeras 4 posiciones del RFC"
      LET desp_err = 1
   END IF

   IF desp_err = 0 THEN
      IF contador2 < 04 THEN
         LET pasa = 1
         LET desc_err = "Error en las posiciones 5 a 10 del RFC"
         LET desp_err = 1
      END IF
   END IF

   RETURN pasa, desc_err

END FUNCTION

----->erm funcion para extraer apellidos simples de compuestos
FUNCTION ape_nomb_compue(vnombres)
DEFINE  nom_b , nom_b1, nom_b2,
        nom_b3, nom_b4, nom_b5,vnombres            CHAR(40),
        bla, ban, long, bb                         SMALLINT,
        no_t1                                      CHAR(40),
        pa_t, ma_t, no_t                           CHAR(02),
        patmatnom, patmatnom1                      CHAR(04),
        i,j                                        SMALLINT


   ### Obtener apellido compuesto
   INITIALIZE nom_b, nom_b1, nom_b2, nom_b3, nom_b4, nom_b5  TO NULL

   LET long = 0
   LET i    = 0
   LET bb   = 0
   LET bla  = 0
   LET j    = 1

   LET no_t1 = vnombres CLIPPED
   LET long  = LENGTH(no_t1 CLIPPED)

   FOR i = 1 TO long
       IF no_t1[i,i] = " " THEN
          LET bla = bla + 1
          CASE bla
             WHEN 1
                LET nom_b1 = nom_b CLIPPED
                LET nom_b1 = nom_b1 CLIPPED

                SELECT "f.X"
                FROM   afi_articulo f
                WHERE  f.palabra MATCHES nom_b1
                IF STATUS != NOTFOUND THEN
                   INITIALIZE nom_b TO NULL
                   LET j = 1
                ELSE
                   LET bb = 1
                   EXIT FOR
                END IF
             WHEN 2
                LET nom_b2 = nom_b CLIPPED

                SELECT "X"
                FROM   afi_articulo
                WHERE  palabra = nom_b2
                IF STATUS != NOTFOUND THEN
                   INITIALIZE nom_b TO NULL
                   LET j = 1
                ELSE
                   LET bb = 2
                   EXIT FOR
                END IF
             WHEN 3
                LET nom_b3 = nom_b CLIPPED

                SELECT "X"
                FROM   afi_articulo
                WHERE  palabra = nom_b3
                IF STATUS != NOTFOUND THEN
                   INITIALIZE nom_b TO NULL
                   LET j = 1
                ELSE
                   LET bb = 3
                   EXIT FOR
                END IF
             WHEN 4
                LET nom_b4 = nom_b CLIPPED

                SELECT "X"
                FROM   afi_articulo
                WHERE  palabra = nom_b4
                IF STATUS != NOTFOUND THEN
                   INITIALIZE nom_b TO NULL
                   LET j = 1
                ELSE
                   LET bb = 4
                   EXIT FOR
                END IF
             WHEN 5
                LET nom_b5 = nom_b CLIPPED

                SELECT "X"
                FROM   afi_articulo
                WHERE  palabra = nom_b5
                IF STATUS != NOTFOUND THEN
                   LET j = 1
                   INITIALIZE nom_b TO NULL
                ELSE
                   LET bb = 5
                   EXIT FOR
                END IF
          END CASE
       ELSE
          LET nom_b[j,j] = no_t1[i,i]
          LET bb         = 6
          LET j          = j + 1
       END IF
   END FOR

   CASE bb
        WHEN 1 LET no_t1 = nom_b1
        WHEN 2 LET no_t1 = nom_b2
        WHEN 3 LET no_t1 = nom_b3
        WHEN 4 LET no_t1 = nom_b4
        WHEN 5 LET no_t1 = nom_b5
        WHEN 6 LET no_t1 = nom_b
   END CASE

   IF no_t1 IS NULL OR no_t1 = " " THEN
      LET nom_b1 = no_t1
   END IF

   LET vnombres = no_t1

   FOR i = 1 TO long
     IF 1 = 1 THEN
        LET no_t[i,i] = no_t1[i,i]
        IF no_t[i,i] = "Ñ" OR no_t[i,i] = "ñ" THEN
           LET no_t[i,i] = "X"
        END IF
        EXIT FOR
     END IF
   END FOR

   LET patmatnom = pa_t CLIPPED, ma_t CLIPPED, no_t CLIPPED

   SELECT b.palabra_si
   INTO   patmatnom1
   FROM   afi_no_conviene b
   WHERE  palabra_no = patmatnom
   IF STATUS != NOTFOUND THEN
      LET patmatnom = patmatnom1 CLIPPED
   END IF


   RETURN no_t1

END FUNCTION

#valida que el nss sea numérico y los años de afiliación y nacimiento

FUNCTION valida_g_nss(nss)
   DEFINE
      nss CHAR(11),
      arr_nss ARRAY[11] OF RECORD
      nss_pos CHAR(1)
   END RECORD,
      i SMALLINT,
      arr_nume ARRAY[10] OF RECORD
      num CHAR(1)
   END RECORD,
      k SMALLINT,
      pasa CHAR(1),
      contador1 SMALLINT,
      desc_err CHAR(60),
      desp_err SMALLINT

   DEFINE an_nss SMALLINT
   DEFINE VHOY   CHAR(10)
   DEFINE HOY    DATE

   LET pasa = 0
   LET HOY  = TODAY
   LET VHOY = HOY

   ### VALIDA nss QUE NO SEA NULO
   IF nss IS NULL THEN
     LET pasa = 1
     LET desc_err = "Campo NSS NO puede ser nulo"
     LET desp_err = 1

   ELSE
      ### VALIDA nss EN LONGITUD 11 POSICIONES
      IF LENGTH(nss) <> 11 THEN
        LET pasa = 1
        LET desc_err = "Debe ingresar NSS completo"
        LET desp_err = 2

      ELSE
         ### VALIDA nss CADA POSICION QUE SEA NUMERICA
         ### SEPARA nss POR POSICIONES
         LET arr_nss[01].nss_pos = nss[01]  LET arr_nss[02].nss_pos = nss[02]
         LET arr_nss[03].nss_pos = nss[03]  LET arr_nss[04].nss_pos = nss[04]
         LET arr_nss[05].nss_pos = nss[05]  LET arr_nss[06].nss_pos = nss[06]
         LET arr_nss[07].nss_pos = nss[07]  LET arr_nss[08].nss_pos = nss[08]
         LET arr_nss[09].nss_pos = nss[09]  LET arr_nss[10].nss_pos = nss[10]
         LET arr_nss[11].nss_pos = nss[11]

         ### INICIALIZA ARREGLO CON VALORES NUMERICOS
         LET k = 0
         FOR k = 1 TO 9
           LET arr_nume[k].num = k
         END FOR
         LET arr_nume[10].num = 0

         ### Valida nss
         LET i         = 0
         LET k         = 0
         LET contador1 = 0
         LET desp_err  = 0

         FOR i = 1 TO 11

            ### Valida numeros (Pos 1 a 11)
            IF i >= 1 AND i <= 11 THEN
               FOR k = 1 TO 10
                  IF arr_nss[i].nss_pos = arr_nume[k].num THEN
                     LET contador1 = contador1 + 1
                  END IF
               END FOR
            END IF

         END FOR

         IF desp_err = 0 THEN
            IF contador1 < 11 THEN
               LET pasa = 1
               LET desc_err = "Caracteres NO validos en las posiciones del nss"
               LET desp_err = 4
            END IF
         END IF

         ### VALIDA nss EN EL ANIO
         LET an_nss = nss[3,4]
         IF an_nss >= 0 AND
            an_nss <=  VHOY[09,10] THEN
            LET an_nss = 2000 + an_nss
         ELSE
            LET an_nss = 1900 + an_nss
         END IF

         IF an_nss < 1943      OR
            an_nss > YEAR(HOY) THEN
               LET pasa = 1
               LET desc_err = "Añe registro en IMSS erroneo"
               LET desp_err = 3
         END IF
      END IF
   END IF

   RETURN pasa, desc_err, desp_err

END FUNCTION

