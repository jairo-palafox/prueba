--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 09-07-2013
--==============================================================================

################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTG03                                                   #
#Objetivo          => funcion para recuperar los mandatos a preliquidar según  #
#                     configuración de ejecucion                               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 10 Julio 2013                                            #
################################################################################
FUNCTION fn_valida_ejecucion_mandato(p_estado_abonado_pago_mdt)
DEFINE p_estado_abonado_pago_mdt SMALLINT,
       p_mandatos DYNAMIC ARRAY OF RECORD
          v_descripcion_mdt VARCHAR(40),--STRING,
          v_padre           STRING,
          v_identificador   SMALLINT,--STRING,
          v_expandido       BOOLEAN,
          v_municipio       VARCHAR(40),--STRING,
          v_recurrencia     SMALLINT,
         v_complemento_recurrencia SMALLINT,
         v_ejecuta_cod      SMALLINT,
         v_descripcion      VARCHAR(10),--LIKE mdt_cat_mandato_ejecuta_pago.descripcion
         v_ent_federeativa  VARCHAR(40)--STRING
       END RECORD,
       v_indice       SMALLINT,
       v_hora_actual  CHAR(5),
       v_actual_habil SMALLINT,
       v_n_ultimo     CHAR(6),
       v_dia_actual   DATE,
       v_dia_natural  SMALLINT,
       v_dia_semana   SMALLINT,
       v_dia          SMALLINT,
       v_mes          SMALLINT,
       v_continua     BOOLEAN

   LET v_continua = FALSE   
   CALL fn_recupera_mandatos(p_estado_abonado_pago_mdt) RETURNING v_continua, p_mandatos
   IF(v_continua)THEN
      LET v_continua = TRUE
   ELSE
      CALL p_mandatos.clear()
      RETURN v_continua, p_mandatos  
   END IF
   
   FOR v_indice = 1 TO p_mandatos.getLength()
      IF(p_mandatos[v_indice].v_padre <> 0)THEN
         
         CASE p_mandatos[v_indice].v_recurrencia
            

            WHEN 1 # Automática
               # Ejecuta

            WHEN 2 # Diaria
               # Ejecuta

            WHEN 3 # Semanal
               CASE p_mandatos[v_indice].v_ejecuta_cod

                  WHEN 1 # Habil
                     # Obtiene el dia habil actual de la semana
                     CALL v_obtiene_habil_semanal() RETURNING v_actual_habil,v_n_ultimo
                     IF (v_actual_habil  = p_mandatos[v_indice].v_descripcion OR 
                         v_n_ultimo      = p_mandatos[v_indice].v_descripcion )THEN
                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF

                  WHEN 2 # Natural
                     LET v_dia_actual  = TODAY
                     LET v_dia_natural = WEEKDAY(v_dia_actual)
                     IF( v_dia_natural = p_mandatos[v_indice].v_descripcion )THEN

                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF
 
                  WHEN 3 # Dia
                     LET v_dia_semana = WEEKDAY(TODAY) 
                     CALL UPSHIFT(p_mandatos[v_indice].v_descripcion) RETURNING p_mandatos[v_indice].v_descripcion
                     CASE
                        WHEN p_mandatos[v_indice].v_descripcion = "LUNES"     AND v_dia_semana = 1
                           
                        WHEN p_mandatos[v_indice].v_descripcion = "MARTES"    AND v_dia_semana = 2

                        WHEN p_mandatos[v_indice].v_descripcion = "MIERCOLES" AND v_dia_semana = 3

                        WHEN p_mandatos[v_indice].v_descripcion = "JUEVES"    AND v_dia_semana = 4

                        WHEN p_mandatos[v_indice].v_descripcion = "VIERNES"   AND v_dia_semana = 5

                        WHEN p_mandatos[v_indice].v_descripcion = "SABADO"    AND v_dia_semana = 6

                        WHEN p_mandatos[v_indice].v_descripcion = "DOMINGO"   AND v_dia_semana = 0

                        OTHERWISE
                           CALL p_mandatos.deleteElement(v_indice)
                           LET v_indice = v_indice - 1
                     END CASE

                  OTHERWISE
                     CALL p_mandatos.deleteElement(v_indice)
                     LET v_indice = v_indice - 1

               END CASE

            WHEN 4 # Quincenal
               CASE p_mandatos[v_indice].v_ejecuta_cod

                  WHEN 1 # Habil
                     # Obtiene el dia habil actual de la quincena
                     CALL fn_obtiene_habil_quincenal() RETURNING v_actual_habil, v_n_ultimo

                     IF(v_actual_habil = p_mandatos[v_indice].v_descripcion OR
                        v_n_ultimo     = p_mandatos[v_indice].v_descripcion)THEN

                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF

                  WHEN 2 # Natural
                     CALL v_obtiene_natural_quincenal() RETURNING v_dia_natural, v_n_ultimo
                     IF( v_dia_natural = p_mandatos[v_indice].v_descripcion OR
                         v_n_ultimo    = p_mandatos[v_indice].v_descripcion )THEN

                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF

                  OTHERWISE
                     CALL p_mandatos.deleteElement(v_indice)
                     LET v_indice = v_indice - 1

               END CASE

            WHEN 5 # Mensual
               CASE p_mandatos[v_indice].v_ejecuta_cod

                  WHEN 1 # Habil
                     # Obtiene el dia habil actual del mes
                     CALL fn_obtiene_habil_mensual() RETURNING v_actual_habil,v_n_ultimo
                     IF( v_actual_habil = p_mandatos[v_indice].v_descripcion OR
                         v_n_ultimo     = p_mandatos[v_indice].v_descripcion )THEN

                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF
                     

                  WHEN 2 # Natural
                     # Obtiene el dia natural actual del mes
                     CALL fn_obtiene_natural_mensual() RETURNING v_dia_natural,v_n_ultimo
                     IF( v_dia_natural = p_mandatos[v_indice].v_descripcion OR 
                         v_n_ultimo    = p_mandatos[v_indice].v_descripcion)THEN

                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF

                  OTHERWISE
                     CALL p_mandatos.deleteElement(v_indice)
                     LET v_indice = v_indice - 1

               END CASE

            WHEN 6 

            WHEN 7 # Anual
               CASE p_mandatos[v_indice].v_ejecuta_cod

                  WHEN 1 # Habil
                     # Obtiene el dia habil actual de la quincena
                     CALL fn_obtiene_habil_ano() RETURNING v_actual_habil,v_n_ultimo
                     IF( v_actual_habil = p_mandatos[v_indice].v_descripcion OR
                         v_n_ultimo     = p_mandatos[v_indice].v_descripcion)THEN

                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF

                  WHEN 2 # Natural
                     # Obtiene el dia natural actual de la quincena
                     CALL fn_obtiene_natural_ano() RETURNING v_dia_natural,v_n_ultimo
                     IF( v_dia_natural = p_mandatos[v_indice].v_descripcion OR 
                         v_n_ultimo    = p_mandatos[v_indice].v_descripcion)THEN

                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF

                  WHEN 4 # Fecha
                     LET v_dia = DAY(TODAY)USING "&&"
                     LET v_mes = MONTH(TODAY)USING "&&"
                     IF( v_dia = p_mandatos[v_indice].v_descripcion[1,2] AND
                         v_mes = p_mandatos[v_indice].v_descripcion[4,5] )THEN

                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1                        
                     END IF

                  OTHERWISE
                     CALL p_mandatos.deleteElement(v_indice)
                     LET v_indice = v_indice - 1

               END CASE

            WHEN 8 # Serie

            WHEN 9 # Bimestral
               
               CASE 
                  WHEN p_mandatos[v_indice].v_complemento_recurrencia = 1 AND fn_verifica_mes_impar(TODAY) # verifica mes IMPAR
                     
                     CASE p_mandatos[v_indice].v_ejecuta_cod

                        WHEN 1 # Habil
                           # Obtiene el dia habil actual de la quincena
                           CALL fn_obtiene_habil_mensual() RETURNING v_actual_habil,v_n_ultimo
                           IF( v_actual_habil = p_mandatos[v_indice].v_descripcion OR
                               v_n_ultimo     = p_mandatos[v_indice].v_descripcion)THEN

                           ELSE
                              CALL p_mandatos.deleteElement(v_indice)
                              LET v_indice = v_indice - 1
                           END IF

                        WHEN 2 # Natural
                           # Obtiene el dia natural actual de la quincena
                           CALL fn_obtiene_natural_mensual() RETURNING v_dia_natural,v_n_ultimo
                           IF( v_dia_natural = p_mandatos[v_indice].v_descripcion OR 
                               v_n_ultimo    = p_mandatos[v_indice].v_descripcion)THEN

                           ELSE
                               CALL p_mandatos.deleteElement(v_indice)
                               LET v_indice = v_indice - 1
                           END IF

                        OTHERWISE
                           CALL p_mandatos.deleteElement(v_indice)
                           LET v_indice = v_indice - 1

                     END CASE
                  WHEN p_mandatos[v_indice].v_complemento_recurrencia = 2 AND fn_verifica_mes_par(TODAY) # verifica mes PAR
                     CASE p_mandatos[v_indice].v_ejecuta_cod

                        WHEN 1 # Habil
                           # Obtiene el dia habil actual del mes
                           CALL fn_obtiene_habil_mensual() RETURNING v_actual_habil,v_n_ultimo
                           IF( v_actual_habil = p_mandatos[v_indice].v_descripcion OR
                               v_n_ultimo     = p_mandatos[v_indice].v_descripcion)THEN

                           ELSE
                              CALL p_mandatos.deleteElement(v_indice)
                              LET v_indice = v_indice - 1
                           END IF

                        WHEN 2 # Natural
                           # Obtiene el dia natural actual del mes
                           CALL fn_obtiene_natural_mensual() RETURNING v_dia_natural,v_n_ultimo
                           IF( v_dia_natural = p_mandatos[v_indice].v_descripcion OR 
                               v_n_ultimo    = p_mandatos[v_indice].v_descripcion)THEN

                           ELSE
                               CALL p_mandatos.deleteElement(v_indice)
                               LET v_indice = v_indice - 1
                           END IF

                        OTHERWISE
                           CALL p_mandatos.deleteElement(v_indice)
                           LET v_indice = v_indice - 1

                     END CASE

                  OTHERWISE
                     CALL p_mandatos.deleteElement(v_indice)  
                     LET v_indice = v_indice - 1

               END CASE

            WHEN 10 # Semestral
               LET v_mes = fn_obtiene_mes_semestre(TODAY)
               CASE p_mandatos[v_indice].v_ejecuta_cod

                  WHEN 1 # Habil
                     
                     # Obtiene el dia habil actual del mes
                     CALL fn_obtiene_habil_mensual() RETURNING v_actual_habil,v_n_ultimo
                     IF((v_actual_habil = p_mandatos[v_indice].v_descripcion OR
                         v_n_ultimo     = p_mandatos[v_indice].v_descripcion) AND 
                         v_mes = p_mandatos[v_indice].v_complemento_recurrencia)THEN
                     
                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF

                  WHEN 2 # Natural
                     # Obtiene el dia natural actual del mes
                     CALL fn_obtiene_natural_mensual() RETURNING v_dia_natural,v_n_ultimo
                     IF((v_dia_natural = p_mandatos[v_indice].v_descripcion OR 
                         v_n_ultimo    = p_mandatos[v_indice].v_descripcion) AND
                         v_mes = p_mandatos[v_indice].v_complemento_recurrencia)THEN

                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF

                  OTHERWISE
                     CALL p_mandatos.deleteElement(v_indice)
                     LET v_indice = v_indice - 1

               END CASE

            OTHERWISE
               CALL p_mandatos.deleteElement(v_indice)
               LET v_indice = v_indice - 1

         END CASE

      END IF

   END FOR

   # Valida que aún existan mandatos
   LET v_continua = FALSE
   FOR v_indice = 1 TO p_mandatos.getLength()
      IF(p_mandatos[v_indice].v_padre <> 0)THEN # solo los mandatos tienen padres diferente de cero
         LET v_continua = TRUE # Despúes de filtrar por la configuracion se indica que si hay mandatos
         EXIT FOR
      END IF
   END FOR
   IF NOT(v_continua)THEN
      CALL p_mandatos.clear()
   END IF

   RETURN v_continua, p_mandatos
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => Obtiene dia habil actual en la semana                   #
#Parametros Entrada=> Ninguno                                                 #
#Autor             => Jesus David Yanez Moreno                                #
#Modifico          => Alexandro Hollmann, EFP                                 #
#Fecha Modificacion=> 22 Mayo  2012                                           #
###############################################################################
FUNCTION v_obtiene_habil_semanal()
DEFINE resta smallint

DEFINE hoy     ,
       v_fecha date

DEFINE ultimo  ,
       dia     ,
       mes     ,
       ano     ,
       n_habil ,
       i       ,
       inhabil integer

DEFINE enter char(001)
DEFINE n_ultimo CHAR(006)
DEFINE x           ,
       dia_semana  smallint

   LET hoy     = today
   LET dia     = WEEKDAY(hoy)
   LET n_habil = 0
   LET ultimo  = 0
   LET inhabil = 0

   FOR i = 1 TO dia
      LET dia_semana = WEEKDAY(i)
      IF dia_semana <> 6  and
         dia_semana <> 0  THEN

      LET resta = day(hoy - (dia - i))

      LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))


         SELECT "ok"
         FROM   cat_feriado
         WHERE  feriado_fecha = v_fecha
         GROUP BY 1

         IF STATUS <> NOTFOUND  THEN
            LET inhabil = inhabil + 1
         ELSE
            LET n_habil = n_habil + 1
         END IF
      ELSE
          LET inhabil = inhabil + 1
      END IF
   END FOR
   LET ultimo = 0
    FOR i = 1 TO 5

       LET resta   = DAY(hoy - (dia - i))
       LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))

       SELECT "ok"
       FROM   cat_feriado
       WHERE  feriado_fecha = v_fecha
       GROUP BY 1

       IF STATUS = NOTFOUND  THEN
          LET ultimo = ultimo + 1
       END IF
   END FOR
   IF n_habil = ultimo THEN
      LET n_ultimo = "ULTIMO"
   ELSE
      LET n_ultimo = "x"
   END IF
   RETURN n_habil,n_ultimo

END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => Obtiene dia habil actual en la quincena                 #
#Parametros Entrada=> Ninguno                                                 #
#Autor             => Jesus David Yanez Moreno                                #
#Modifico          => Alexandro Hollmann, EFP                                 #
#Fecha Modificacion=> 22 Mayo  2012                                           #
###############################################################################
FUNCTION fn_obtiene_habil_quincenal()

DEFINE resta       SMALLINT
DEFINE f_ultima ,
	     hoy      ,
       v_fecha     DATE

DEFINE dia     , 
       mes     ,
       ano     ,
	     ultimo  ,
       n_habil ,
       i       , 
       inhabil     INTEGER 

DEFINE enter       CHAR(1)
DEFINE n_ultimo    CHAR(6)
DEFINE x           ,
       dia_semana  SMALLINT

   LET hoy     = TODAY
   LET dia     = DAY(hoy)
   LET n_habil = 0
   LET inhabil = 0

   IF dia < 16 THEN
      FOR i = 1 TO dia
      
         LET dia_semana = WEEKDAY(i)
         IF dia_semana <> 6  AND
            dia_semana <> 0  THEN
      
            LET resta = day(hoy - (dia - i))

            LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))
      
            SELECT "ok"
              FROM cat_feriado
             WHERE feriado_fecha = v_fecha
             GROUP BY 1
      
            IF STATUS <> NOTFOUND  THEN 
               LET inhabil = inhabil + 1
            ELSE 
               LET n_habil = n_habil + 1
            END IF
         ELSE 
             LET inhabil = inhabil + 1 
         END IF
      END FOR
	 ELSE
      FOR i = 16 TO dia
      
         LET dia_semana = WEEKDAY(i)
         IF dia_semana <> 6  AND
            dia_semana <> 0  THEN
      
            LET resta = day(hoy - (dia - i))
            LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))
      
            SELECT "ok"
              FROM cat_feriado
             WHERE feriado_fecha = v_fecha
             GROUP BY 1
      
             IF STATUS <> NOTFOUND  THEN 
                LET inhabil = inhabil + 1
             ELSE 
                LET n_habil = n_habil + 1
             END IF
         ELSE 
            LET inhabil = inhabil + 1 
         END IF
      END FOR
	 END IF 

   IF dia < 16 THEN
      FOR i = 1 TO 15

         LET resta = day(hoy - (dia - i))
         LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))
         
         LET dia_semana = WEEKDAY(v_fecha)

         IF dia_semana <> 6  AND
            dia_semana <> 0  THEN
         
            SELECT "ok"
              FROM cat_feriado
             WHERE feriado_fecha = v_fecha
             GROUP BY 1
         
            IF STATUS = NOTFOUND  THEN 
               LET ultimo = ultimo + 1
            END IF
         END IF
      END FOR 
   ELSE
      -- Obtiene el ultimo dia del mes proporcionado
      CALL fn_verifica_mes(hoy) RETURNING f_ultima
	    LET dia = DAY(f_ultima)
      FOR i = 16 TO dia 

         LET resta = day(hoy - (dia - i))
         LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))
         
         LET dia_semana = WEEKDAY(v_fecha)

         IF dia_semana <> 6  AND
            dia_semana <> 0  THEN
      
            SELECT "ok"
              FROM cat_feriado
             WHERE feriado_fecha = v_fecha
             GROUP BY 1
      
            IF STATUS = NOTFOUND  THEN 
               LET ultimo = ultimo + 1
            END IF
         END IF
      END FOR 
   END IF

	 IF n_habil = ultimo THEN
		  LET n_ultimo = "ULTIMO"
   ELSE 
		  LET n_ultimo = "x"
   END IF
   
   RETURN n_habil,n_ultimo 

END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => Obtiene dia natural actual en la quincena               #
#Parametros Entrada=> Ninguno                                                 #
#Autor             => Jesus David Yanez Moreno                                #
#Modifico          => Alexandro Hollmann, EFP                                 #
#Fecha Modificacion=> 22 Mayo  2012                                           #
###############################################################################
FUNCTION v_obtiene_natural_quincenal()

DEFINE f_ultima ,
	     hoy     ,
       v_fecha     DATE

DEFINE dia     , 
       mes     ,
       ano     ,
       n_natural ,
	     ultimo  ,
       i       , 
       inhabil     INTEGER

DEFINE enter       CHAR(1)
DEFINE n_ultimo    CHAR(6)

DEFINE x           ,
       dia_semana  SMALLINT

   LET hoy       = TODAY
   LET dia       = DAY(hoy)
   LET n_natural = 0

   IF dia < 15  THEN
      FOR i = 1 TO dia
         LET n_natural = n_natural + 1
      END FOR
	 ELSE 
      FOR i = 15 TO dia
         LET n_natural = n_natural + 1
      END FOR
	 END IF

   IF dia < 15 THEN
      LET ultimo = 15
   ELSE 
      -- Obtiene el ultimo dia del mes proporcionado
      CALL fn_verifica_mes(hoy) RETURNING f_ultima
	    LET ultimo = DAY(f_ultima)
   END IF

   IF ultimo = n_natural THEN
		  LET n_ultimo = "ULTIMO"
   ELSE
		  LET n_ultimo = "x" 
   END IF
   
   RETURN n_natural,n_ultimo
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => Obtiene dia habil actual en el mes                      #
#Parametros Entrada=> Ninguno                                                 #
#Autor             => Jesus David Yanez Moreno                                #
#Modifico          => Alexandro Hollmann, EFP                                 #
#Fecha Modificacion=> 22 Mayo  2012                                           #
###############################################################################
FUNCTION fn_obtiene_habil_mensual()

DEFINE resta       SMALLINT
DEFINE f_ultima ,
	     hoy     ,
       v_fecha     DATE

DEFINE dia     , 
       mes     ,
       ano     ,
       n_habil ,
	     ultimo  ,
       i       , 
       inhabil     INTEGER

DEFINE enter       CHAR(1)
DEFINE n_ultimo    CHAR(6)


DEFINE x           ,
       dia_semana  SMALLINT

   LET hoy     = TODAY
   LET dia     = DAY(hoy)
   LET n_habil = 0
   LET inhabil = 0

   FOR i = 1 TO dia

      LET resta = day(hoy - (dia - i))
      LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))
      LET dia_semana = WEEKDAY(v_fecha)

      IF dia_semana <> 6  AND
         dia_semana <> 0  THEN

         SELECT "ok"
           FROM cat_feriado
          WHERE feriado_fecha = v_fecha
          GROUP BY 1

         IF STATUS <> NOTFOUND  THEN 
            LET inhabil = inhabil + 1
         ELSE 
            LET n_habil = n_habil + 1
         END IF
      ELSE 
          LET inhabil = inhabil + 1 
      END IF
   END FOR


   -- Obtiene el ultimo dia del mes proporcionado
   CALL fn_verifica_mes(hoy) RETURNING f_ultima
   
   LET dia = DAY(f_ultima)

   FOR i = 1 TO dia

      LET resta   = DAY(hoy - (dia - i))
      LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))

         LET dia_semana = WEEKDAY(v_fecha)

         IF dia_semana <> 6  AND
            dia_semana <> 0  THEN

            SELECT "ok"
            FROM   cat_feriado
            WHERE  feriado_fecha = v_fecha
            GROUP BY 1


            IF STATUS = NOTFOUND  THEN

               LET ultimo = ultimo + 1
            END IF
         END IF
   END FOR

   IF ultimo = n_habil THEN
      LET n_ultimo = "ULTIMO"
   ELSE 
   	  LET n_ultimo = "x"
   END IF

   RETURN n_habil,n_ultimo
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => Obtiene dia natural actual en el mes                    #
#Parametros Entrada=> Ninguno                                                 #
#Autor             => Jesus David Yanez Moreno                                #
#Modifico          => Alexandro Hollmann, EFP                                 #
#Fecha Modificacion=> 22 Mayo  2012                                           #
###############################################################################
FUNCTION fn_obtiene_natural_mensual()

DEFINE f_ultima,
	     hoy     ,
       v_fecha     DATE

DEFINE dia     , 
       mes     ,
       ano     ,
       n_natural ,
	     ultimo  ,
       i       , 
       inhabil     INTEGER

DEFINE enter       CHAR(1)
DEFINE n_ultimo    CHAR(6)

DEFINE x           ,
       dia_semana  SMALLINT

   LET hoy       = TODAY
   LET dia       = DAY(hoy)
   LET n_natural = 0
   
   -- Obtiene el ultimo dia del mes proporcionado
   CALL fn_verifica_mes(hoy) RETURNING f_ultima
	 
	 LET ultimo = DAY(f_ultima)
	 IF ultimo = dia THEN
	    LET n_ultimo = "ULTIMO"
   ELSE 
	    LET n_ultimo = "x"
   END IF

   RETURN dia,n_ultimo
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => Obtiene dia habil actual del ano                        #
#Parametros Entrada=> Ninguno                                                 #
#Autor             => Alexandro Hollmann, EFP                                 #
#Fecha             => 29 Mayo  2012                                           #
###############################################################################
FUNCTION fn_obtiene_habil_ano()
DEFINE v_habil  INTEGER
DEFINE v_habil_ano  INTEGER
DEFINE v_ultimo CHAR(6)
DEFINE v_mes    SMALLINT
DEFINE v_pos    SMALLINT
DEFINE v_fecha  CHAR(10)
DEFINE d_fecha  DATE
DEFINE f_ultima DATE
DEFINE n_ultimo CHAR(6)

  LET v_habil_ano = 0
  LET v_mes = MONTH(TODAY)
  
  FOR v_pos = 1 TO v_mes-1
  	CASE v_pos
  		 WHEN 1
  		 	  LET v_fecha = '01/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 2
  		 	  LET v_fecha = '02/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 3
  		 	  LET v_fecha = '03/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 4
  		 	  LET v_fecha = '04/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 5
  		 	  LET v_fecha = '05/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 6
  		 	  LET v_fecha = '06/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 7
  		 	  LET v_fecha = '07/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 8
  		 	  LET v_fecha = '08/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 9
  		 	  LET v_fecha = '09/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 10
  		 	  LET v_fecha = '10/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 11
  		 	  LET v_fecha = '11/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
    END CASE
  END FOR
  
  CALL fn_obtiene_habil_mensual() RETURNING v_habil, v_ultimo
  LET v_habil_ano = v_habil_ano + v_habil
  
   IF 265 = v_habil_ano THEN
      LET n_ultimo = "ULTIMO"
   ELSE 
   	  LET n_ultimo = "x"
   END IF
  
  RETURN v_habil_ano, n_ultimo
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => Obtiene el ultimo dia del mes proporcionado             #
#Parametros Entrada=> hoy4 - fecha a verificar                                #
#Autor             => Jesus David Yanez Moreno                                #
#Modifico          => Alexandro Hollmann, EFP                                 #
#Fecha Modificacion=> 22 Mayo  2012                                           #
###############################################################################
FUNCTION fn_verifica_mes(hoy4)

DEFINE mes_int CHAR(10)
DEFINE hoy4    DATE
DEFINE hoy3    DATE

   CASE MONTH(hoy4)
      WHEN  1 LET hoy3 = MDY(MONTH(hoy4),31,YEAR(hoy4))
      WHEN  2 IF YEAR(hoy4) MOD 4 = 0 THEN     
                  LET hoy3 = MDY(MONTH(hoy4),29,YEAR(hoy4)) 
              ELSE
                  LET hoy3 = MDY(MONTH(hoy4),28,YEAR(hoy4)) 
              END IF
      WHEN  3 LET hoy3 = MDY(MONTH(hoy4),31,YEAR(hoy4))
      WHEN  4 LET hoy3 = MDY(MONTH(hoy4),30,YEAR(hoy4))
      WHEN  5 LET hoy3 = MDY(MONTH(hoy4),31,YEAR(hoy4))
      WHEN  6 LET hoy3 = MDY(MONTH(hoy4),30,YEAR(hoy4))
      WHEN  7 LET hoy3 = MDY(MONTH(hoy4),31,YEAR(hoy4))
      WHEN  8 LET hoy3 = MDY(MONTH(hoy4),31,YEAR(hoy4))
      WHEN  9 LET hoy3 = MDY(MONTH(hoy4),30,YEAR(hoy4))
      WHEN 10 LET hoy3 = MDY(MONTH(hoy4),31,YEAR(hoy4))
      WHEN 11 LET hoy3 = MDY(MONTH(hoy4),30,YEAR(hoy4))
      WHEN 12 LET hoy3 = MDY(MONTH(hoy4),31,YEAR(hoy4))
   END CASE

   RETURN hoy3

END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => Obtiene dia habil para una fecha especifica del mes     #
#Parametros Entrada=> Ninguno                                                 #
#Autor             => Alexandro Hollmann, EFP                                 #
#Fecha             => 29 Mayo  2012                                           #
###############################################################################
FUNCTION fn_obtiene_habil_mensual_xfecha(p_fecha)
DEFINE p_fecha     DATE

DEFINE resta       SMALLINT
DEFINE f_ultima ,
	     hoy     ,
       v_fecha     DATE

DEFINE dia     , 
       mes     ,
       ano     ,
       n_habil ,
	     ultimo  ,
       i       , 
       inhabil     INTEGER

DEFINE enter       CHAR(1)
DEFINE n_ultimo    CHAR(6)


DEFINE x           ,
       dia_semana  SMALLINT

   LET hoy     = p_fecha
   LET dia     = DAY(hoy)
   LET n_habil = 0
   LET inhabil = 0

   FOR i = 1 TO dia

      LET resta = day(hoy - (dia - i))
      LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))
      LET dia_semana = WEEKDAY(v_fecha)

      IF dia_semana <> 6  AND
         dia_semana <> 0  THEN

         SELECT "ok"
           FROM cat_feriado
          WHERE feriado_fecha = v_fecha
          GROUP BY 1

         IF STATUS <> NOTFOUND  THEN 
            LET inhabil = inhabil + 1
         ELSE 
            LET n_habil = n_habil + 1
         END IF
      ELSE 
          LET inhabil = inhabil + 1 
      END IF
   END FOR


   -- Obtiene el ultimo dia del mes proporcionado
   CALL fn_verifica_mes(hoy) RETURNING f_ultima
   
   LET dia = DAY(f_ultima)

   FOR i = 1 TO dia

      LET resta   = DAY(hoy - (dia - i))
      LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))

         LET dia_semana = WEEKDAY(v_fecha)

         IF dia_semana <> 6  AND
            dia_semana <> 0  THEN

            SELECT "ok"
            FROM   cat_feriado
            WHERE  feriado_fecha = v_fecha
            GROUP BY 1


            IF STATUS = NOTFOUND  THEN

               LET ultimo = ultimo + 1
            END IF
         END IF
   END FOR

   IF ultimo = n_habil THEN
      LET n_ultimo = "ULTIMO"
   ELSE 
   	  LET n_ultimo = "x"
   END IF

   RETURN n_habil,n_ultimo
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => Obtiene dia natural actual en el ano                    #
#Parametros Entrada=> Ninguno                                                 #
#Autor             => Alexandro Hollmann, EFP                                 #
#Fecha             => 29 Mayo  2012                                           #
###############################################################################
FUNCTION fn_obtiene_natural_ano()

DEFINE f_ultima,
	     inicio  ,
	     hoy     ,
       v_fecha     DATE
DEFINE v_inicio  CHAR(10)

DEFINE dia     , 
       mes     ,
       ano     ,
       n_natural ,
	     ultimo  ,
       i       , 
       inhabil     INTEGER

DEFINE enter       CHAR(1)
DEFINE n_ultimo    CHAR(6)

DEFINE x           ,
       dia_semana  SMALLINT

   LET v_inicio  = '12/31/',YEAR(TODAY)-1
   LET inicio    = v_inicio
   LET hoy       = TODAY
   LET dia       = hoy - inicio
   
	 LET ultimo = 365
	 IF ultimo = dia THEN
	    LET n_ultimo = "ULTIMO"
   ELSE 
	    LET n_ultimo = "x"
   END IF

   RETURN dia,n_ultimo
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => verifica si el mes par para la fecha de paramatro       #
#Parametros Entrada=> p_fecha                                                 #
#Autor             =>                                  #
#Fecha             => 08 Julio 2013                                           #
###############################################################################
FUNCTION fn_verifica_mes_par(p_fecha)
DEFINE p_fecha DATE,
       v_mes   SMALLINT,
       v_par   BOOLEAN

   LET v_par = FALSE
   LET v_mes = MONTH(p_fecha)
   

   CASE v_mes

      WHEN 2    LET v_par = TRUE
      WHEN 4    LET v_par = TRUE
      WHEN 6    LET v_par = TRUE
      WHEN 8    LET v_par = TRUE
      WHEN 10   LET v_par = TRUE
      WHEN 12   LET v_par = TRUE
      OTHERWISE LET v_par = FALSE

   END CASE

   RETURN v_par
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => verifica si el mes impar para la fecha de paramatro     #
#Parametros Entrada=> p_fecha                                                 #
#Autor             =>                                  #
#Fecha             => 08 Julio 2013                                           #
###############################################################################
FUNCTION fn_verifica_mes_impar(p_fecha)
DEFINE p_fecha DATE,
       v_mes   SMALLINT,
       v_par   BOOLEAN

   LET v_par = FALSE
   LET v_mes = MONTH(p_fecha)
   

   CASE v_mes

      WHEN 1    LET v_par = TRUE
      WHEN 3    LET v_par = TRUE
      WHEN 5    LET v_par = TRUE
      WHEN 7    LET v_par = TRUE
      WHEN 9    LET v_par = TRUE
      WHEN 11   LET v_par = TRUE
      OTHERWISE LET v_par = FALSE

   END CASE

   RETURN v_par
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => obtiene el mes de un semestre de un rango de 1 -6       #
#Parametros Entrada=> p_fecha                                                 #
#Autor             =>                                  #
#Fecha             => 08 Julio 2013                                           #
###############################################################################
FUNCTION fn_obtiene_mes_semestre(p_fecha)
DEFINE p_fecha        DATE,
       v_mes          SMALLINT,
       v_mes_semestre SMALLINT

   LET v_mes = MONTH(p_fecha)

   CASE v_mes
      WHEN 1 LET v_mes_semestre = 1
      WHEN 2 LET v_mes_semestre = 2
      WHEN 3 LET v_mes_semestre = 3
      WHEN 4 LET v_mes_semestre = 4
      WHEN 5 LET v_mes_semestre = 5
      WHEN 6 LET v_mes_semestre = 6
      WHEN 7 LET v_mes_semestre = 1
      WHEN 8 LET v_mes_semestre = 2
      WHEN 8 LET v_mes_semestre = 3
      WHEN 10 LET v_mes_semestre = 4
      WHEN 11 LET v_mes_semestre = 5
      WHEN 12 LET v_mes_semestre = 6
   END CASE

   RETURN v_mes_semestre
END FUNCTION

{===============================================================================
Nombre: fn_recupera_mandatos
Fecha creacion: 05 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para 
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_recupera_mandatos(p_estado_abonado_pago_mdt)
DEFINE p_estado_abonado_pago_mdt SMALLINT,
       v_consulta STRING,
       v_mandato RECORD
         v_padre           VARCHAR(10),
         v_identificador   SMALLINT,--LIKE mdt_cat_mandato.id_cat_mandato,
         v_descripcion_mdt VARCHAR(40),--LIKE mdt_cat_mandato.desc_mandato,
         v_tpo_mandato     SMALLINT,--LIKE mdt_cat_mandato.tpo_mandato,
         v_recurrencia     SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.recurrencia,
         v_complemento_recurrencia SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.complemento_recurrencia,
         v_ejecuta_cod     SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.ejecuta_cod,
         v_descripcion     CHAR(10)--LIKE mdt_cat_mandato_ejecuta_pago.descripcion
       END RECORD,
       v_mandatos DYNAMIC ARRAY OF RECORD
          v_descripcion_mdt VARCHAR(40),--STRING,
          v_padre           STRING,
          v_identificador   SMALLINT,--STRING,
          v_expandido       BOOLEAN,
          v_municipio       VARCHAR(40),--STRING,
          v_recurrencia     SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.recurrencia,
          v_complemento_recurrencia SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.complemento_recurrencia,
          v_ejecuta_cod      SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.ejecuta_cod,
          v_descripcion      VARCHAR(10),--LIKE mdt_cat_mandato_ejecuta_pago.descripcion
          v_ent_federeativa  VARCHAR(40)--STRING
       END RECORD,
       v_municipio RECORD
         v_etiqueta       CHAR(40),--LIKE mdt_cat_gpo_etiqueta.etiqueta,
         v_valor_etiqueta CHAR(120)--LIKE mdt_cat_instancia_mandato.valor_etiqueta
       END RECORD,
       v_ent_fed RECORD
         v_etiqueta       CHAR(40),--LIKE mdt_cat_gpo_etiqueta.etiqueta,
         v_valor_etiqueta CHAR(120)--LIKE mdt_cat_instancia_mandato.valor_etiqueta
       END RECORD,
       v_indice   SMALLINT,
       v_continua BOOLEAN

   LET v_indice   = 2
   LET v_continua = FALSE
   CALL v_mandatos.clear()

   LET v_mandatos[1].v_descripcion   = "TIPOS DE MANDATO"   
   LET v_mandatos[1].v_padre         = "00"
   LET v_mandatos[1].v_identificador = "0"   
   LET v_mandatos[1].v_expandido     = TRUE
   LET v_mandatos[1].v_municipio     = ""

   # consulta para recupeerar el municipio del mandato
   LET v_consulta = "\n SELECT etq.etiqueta,ins.valor_etiqueta",
                    "\n   FROM mdt_cat_mandato mdt JOIN mdt_cat_atributo_nivel nvl",
                    "\n     ON nvl.id_cat_mandato = mdt.id_cat_mandato",
                    "\n        JOIN mdt_cat_instancia_mandato ins",
                    "\n     ON ins.id_atr_nivel = nvl.id_atr_nivel",
                    "\n        JOIN mdt_cat_gpo_etiqueta etq",
                    "\n     ON etq.id_gpo_etiqueta = nvl.id_gpo_etiqueta",
                    "\n  WHERE etq.etiqueta = 'MUNICIPIO'",
                    "\n    AND mdt.id_cat_mandato = ?"
   PREPARE prp_rec_municipio_etiqueta FROM v_consulta

   # consulta para recupeerar la entidad federativa del mandato
   LET v_consulta = "\n SELECT etq.etiqueta,ins.valor_etiqueta",
                    "\n   FROM mdt_cat_mandato mdt JOIN mdt_cat_atributo_nivel nvl",
                    "\n     ON nvl.id_cat_mandato = mdt.id_cat_mandato",
                    "\n        JOIN mdt_cat_instancia_mandato ins",
                    "\n     ON ins.id_atr_nivel = nvl.id_atr_nivel",
                    "\n        JOIN mdt_cat_gpo_etiqueta etq",
                    "\n     ON etq.id_gpo_etiqueta = nvl.id_gpo_etiqueta",
                    "\n  WHERE etq.etiqueta = 'ENTIDAD FEDERATIVA'",
                    "\n    AND mdt.id_cat_mandato = ?"
   PREPARE prp_rec_ent_fed_etiqueta FROM v_consulta  
   
   LET v_consulta = "\n(SELECT 0,",
                    "\n        tpo_mandato,",
                    "\n        desc_tpo_mandato,",
                    "\n        tpo_mandato,",
                    "\n        0,",
                    "\n        0,",
                    "\n        0,",
                    "\n        ''",
                    "\n   FROM mdt_tpo_mandato",
                    "\n  WHERE 1 = 1",
                    "\n UNION ALL ",
                    "\n SELECT DISTINCT cat.tpo_mandato,",
                    "\n        cat.id_cat_mandato,",
                    "\n        cat.desc_mandato,",
                    "\n        cat.tpo_mandato,",
                    "\n        eje.recurrencia,",
                    "\n        eje.complemento_recurrencia,",
                    "\n        eje.ejecuta_cod,",
                    "\n        eje.descripcion",
                    "\n   FROM mdt_cat_mandato_ejecuta_pago eje JOIN mdt_cat_mandato cat",
                    "\n     ON cat.id_cat_mandato = eje.id_cat_mandato",
                    "\n        JOIN mdt_det_aplica_monto mto", # solo se busca que exista el mandato en la tabla
                    "\n     ON mto.id_cat_mandato = eje.id_cat_mandato",
                    "\n  WHERE cat.estado = ?)",
                    "\n  ORDER BY 4,1"
   
   PREPARE prp_recupera_mandatos_ejecuta FROM v_consulta
   DECLARE cur_recupera_mandatos_ejecuta CURSOR FOR prp_recupera_mandatos_ejecuta   
   FOREACH cur_recupera_mandatos_ejecuta USING p_estado_abonado_pago_mdt
                                          INTO v_mandato.*

      INITIALIZE v_municipio.* TO NULL
      INITIALIZE v_ent_fed.* TO NULL
      IF(v_mandato.v_padre <> 0)THEN         
         # recupera municipio
         EXECUTE prp_rec_municipio_etiqueta USING v_mandato.v_identificador
                                             INTO v_municipio.*
         # recupera entidad
         EXECUTE prp_rec_ent_fed_etiqueta USING v_mandato.v_identificador
                                           INTO v_ent_fed.*
      END IF
                                          
      LET v_mandatos[v_indice].v_padre           = v_mandato.v_padre
      LET v_mandatos[v_indice].v_identificador   = v_mandato.v_identificador
      LET v_mandatos[v_indice].v_descripcion_mdt = v_mandato.v_descripcion_mdt
      LET v_mandatos[v_indice].v_expandido       = FALSE
      LET v_mandatos[v_indice].v_municipio       = v_municipio.v_valor_etiqueta
      LET v_mandatos[v_indice].v_recurrencia     = v_mandato.v_recurrencia
      LET v_mandatos[v_indice].v_complemento_recurrencia = v_mandato.v_complemento_recurrencia
      LET v_mandatos[v_indice].v_ejecuta_cod     = v_mandato.v_ejecuta_cod
      LET v_mandatos[v_indice].v_descripcion     = v_mandato.v_descripcion
      LET v_mandatos[v_indice].v_ent_federeativa = v_ent_fed.v_valor_etiqueta
      LET v_indice = v_indice + 1
      
   END FOREACH
   FREE cur_recupera_mandatos_ejecuta
   
   
   IF(v_mandatos.getLength() > 1)THEN
      LET v_continua = TRUE
   ELSE
      LET v_continua = FALSE
      CALL v_mandatos.clear()
   END IF

   RETURN v_continua, v_mandatos
END FUNCTION