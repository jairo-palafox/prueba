#########################################################################
#Modulo            => AGR                                               #
#Programa          => AGRL72                                            #
#Objetivo          => Programa que genera reporte de cifras control     #
#                     y registros afectados en la actualización de      #
#                     de marca conciliación a la entrade de recurrente. #
#Autor             => Emilio Abarca, EFP                                #
#Fecha inicio      => 31/JULIO/2018                                     #
#########################################################################

DATABASE safre_viv

GLOBALS 
   DEFINE g_usuario           CHAR(20)
   DEFINE g_titulo            STRING
   DEFINE g_tipo_ejecucion    SMALLINT
   DEFINE v_ruta_binaria      CHAR(40)
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE g_proceso_cod       SMALLINT  
   DEFINE g_opera_cod         SMALLINT
   DEFINE g_pid               DECIMAL(9,0)

   --variables del proceso
   DEFINE g_pid           DECIMAL(9,0)
   DEFINE v_bnd           SMALLINT
   DEFINE v_comando       STRING
   DEFINE v_mensaje       STRING

END GLOBALS

MAIN 

   LET g_usuario          = ARG_VAL  (1)
   LET g_tipo_ejecucion   = ARG_VAL  (2)
   LET g_titulo           = ARG_VAL  (3)
   LET g_proceso_cod      = 349   -- GENERACIÓN ARCHIVO MARCAS CONCILIACIÓN
   LET g_opera_cod        = 1

   IF (g_titulo IS NOT NULL) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

   CALL STARTLOG(g_usuario CLIPPED|| ".AGRL72.log")

   CLOSE WINDOW SCREEN 

   SELECT ruta_bin
     INTO v_ruta_binaria
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'
   
   CALL reporte_act_marca()
   
END MAIN

FUNCTION reporte_act_marca()

   DEFINE v_confirma  SMALLINT
   DEFINE v_f_inicial DATE
   DEFINE v_f_final   DATE
   DEFINE v_total     INTEGER

   OPEN WINDOW vtn1 WITH FORM "AGRL721"

      LET v_f_inicial = NULL
      LET v_f_final   = NULL
      LET v_total     = 0
      
      INPUT BY NAME v_f_inicial,v_f_final ATTRIBUTES (UNBUFFERED,WITHOUT DEFAULTS)

         ON ACTION ACCEPT

            -- Valida fechas
            IF(v_f_inicial IS NULL) AND (v_f_final IS NULL) THEN
               CALL fn_mensaje("","Debe ingresar el rango de fechas","")
               NEXT FIELD v_f_inicial
            END IF

            IF(v_f_inicial IS NULL) AND (v_f_final IS NOT NULL) THEN
               CALL fn_mensaje("","Ingresa la fecha inicial","")
               NEXT FIELD v_f_inicial
            END IF

            IF(v_f_inicial IS NOT NULL) AND (v_f_final IS NULL) THEN
               CALL fn_mensaje("","Ingresa la fecha final","")
               NEXT FIELD v_f_final
            END IF

            IF(v_f_inicial > v_f_final) THEN
               CALL fn_mensaje("","La fecha final no puede ser menor a la fecha inicial ","")
               NEXT FIELD v_f_final
            END IF

            SELECT COUNT(*)
              INTO v_total
              FROM cre_marca_conciliacion
             WHERE f_proceso BETWEEN v_f_inicial AND v_f_final;

            IF(v_total = 0) THEN
               CALL fn_mensaje("","No existe información para el rango de fechas solicitada","")
               CONTINUE INPUT
            ELSE
               CALL fn_ventana_confirma("Alerta","¿Está seguro de generar el reporte?","stop") RETURNING v_confirma

               IF(v_confirma = 1) THEN

                  #Inicializa el proceso
                  -- Genera PID
                  LET g_pid = fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario)

                  -- Inicializa proceso
                  CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,"","AGRL72","",g_usuario) RETURNING v_bnd

                  IF(v_bnd <> 0) THEN
                     -- Si no se puede ejecutar el proceso se indica la razon
                     CALL fn_muestra_inc_operacion(v_bnd)
                     EXIT INPUT
                  END IF

                  -- Actualiza operación como iniciada
                  CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"AGRL72","",g_usuario) RETURNING v_bnd

                  IF(v_bnd <> 0) THEN
                     -- Si no se peude ejecutar el proceso se indica la razon
                     CALL fn_muestra_inc_operacion(v_bnd)
                     EXIT INPUT
                  END IF

                  -- Ejecuta lanzado
                  LET v_comando = "nohup fglrun ",v_ruta_binaria CLIPPED,"/AGRP50 ",
                                                  g_usuario," ",
                                                  g_pid," ",
                                                  g_proceso_cod," ",
                                                  g_opera_cod," ",
                                                  v_f_inicial," ",
                                                  v_f_final,
                                                  " ' ' 1>",
                                                  v_ruta_listados CLIPPED,
                                                  "/nohup:",g_pid USING "&&&&&",":",
                                                  g_proceso_cod   USING "&&&&&",":",
                                                  g_opera_cod     USING "&&&&&" ," 2>&1 &"

                  RUN v_comando

                  LET v_mensaje = "Se ejecutó la generación de reporte de conciliación de marcas"," \n",
                                  "Verificar en el monitor de procesos el PID: ",g_pid USING "<<<<<<<<<"

                  CALL fn_mensaje("",v_mensaje,"")
                  EXIT INPUT
               ELSE
                  CALL fn_mensaje("","Se ha cancelado la operación","")
                  EXIT INPUT
               END IF
            END IF 

         ON ACTION CANCEL
            EXIT INPUT

      END INPUT

   CLOSE WINDOW vtn1

END FUNCTION
