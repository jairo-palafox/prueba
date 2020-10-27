######################################################################
#Proyecto          => SAFRE VIVIENDA                                 #
#Propietario       => EFP                                            #
#Programa CTAX01   => VALIDA ARCHIVO ESPECIAL MOVIMIENTOS INICIALES  #
#Fecha             => OCTUBRE DE 2014                                #
######################################################################
IMPORT os
DATABASE safre_viv

DEFINE g_proceso_cod    SMALLINT
DEFINE g_opera_cod      SMALLINT
DEFINE g_archivo        STRING
DEFINE g_usuario        CHAR(20)

DEFINE g_rutas          RECORD
   ruta_rescate            CHAR(40),
   ruta_listados           CHAR(40),
   ruta_listados_bat       CHAR(40)
END RECORD

MAIN
   LET g_archivo     = ARG_VAL(1)
   LET g_proceso_cod = 798
   LET g_opera_cod   = 1 
   LET g_usuario     = "OPSISSACI"
   IF fn_val_archivo() THEN
      CALL fn_lanza_carga()
   END IF
END MAIN

FUNCTION fn_val_archivo()

   DEFINE v_ruta_archivo   STRING
   DEFINE v_extension      CHAR(10)

   DEFINE v_bandera        SMALLINT
   DEFINE v_archivos       SMALLINT

   LET v_bandera = 1

   SELECT ruta_rescate,
          ruta_listados
     INTO g_rutas.ruta_rescate,
          g_rutas.ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = "cta"

   SELECT ruta_listados
     INTO g_rutas.ruta_listados_bat
     FROM seg_modulo
    WHERE modulo_cod = "bat"

   IF g_archivo IS NULL THEN
      DISPLAY "*******EXCEPCION SACI*******"
      DISPLAY "DEBE DE EJECUTAR EL PROGRAMA DE LA SIGUIENTE FORMA: "
      DISPLAY "fglrun CTAX01 nombre_archivo_saldos"
      LET v_bandera = 0
   ELSE
      SELECT count(*)
        INTO v_archivos
        FROM glo_ctr_archivo
       WHERE proceso_cod = g_proceso_cod
         AND estado = 2
       
      IF v_archivos > 0 THEN
         DISPLAY "*******EXCEPCION SACI*******"
         DISPLAY "EL PROCESO YA SE HA EJECUTADO"
         LET v_bandera = 0
      ELSE
         LET v_ruta_archivo = g_rutas.ruta_rescate CLIPPED ,"/",g_archivo CLIPPED
         IF NOT os.Path.exists(v_ruta_archivo) THEN
            DISPLAY "*******EXCEPCION SACI*******"
            DISPLAY "EL ARCHIVO NO EXISTE EN LA RUTA:", g_rutas.ruta_rescate
            LET v_bandera = 0
         ELSE
            IF NOT os.Path.readable(v_ruta_archivo) THEN
               DISPLAY "*******EXCEPCION SACI*******"
               DISPLAY "EL ARCHIVO NO EXISTE EN LA RUTA:", g_rutas.ruta_rescate
               LET v_bandera = 0
            ELSE
               SELECT extension
                 INTO v_extension
                 FROM cat_operacion 
                WHERE proceso_cod = g_proceso_cod
                  AND opera_cod = g_opera_cod 

               IF os.Path.extension(v_ruta_archivo) != v_extension CLIPPED THEN
                  DISPLAY "*******EXCEPCION SACI*******"
                  DISPLAY "EL ARCHIVO NO CUENTA CON LA EXTENSIÓN CORRESPONDIENTE:", g_rutas.ruta_rescate
                  LET v_bandera = 0
               END IF
            END IF
         END IF
      END IF
   END IF

   RETURN v_bandera
END FUNCTION

FUNCTION fn_lanza_carga()
   DEFINE v_pid               INTEGER
   DEFINE r_bandera           INTEGER
   DEFINE v_comando           STRING
   DEFINE v_ruta_ejecutable   CHAR(40)
   
   LET v_pid     = 0
   LET r_bandera = 0

   CALL fn_valida_operacion(v_pid,g_proceso_cod,g_opera_cod) RETURNING r_bandera
   
   IF r_bandera <> 0 THEN
      CALL fn_display_excepcion(r_bandera)
   ELSE
      CALL fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario)
               RETURNING v_pid
      CALL fn_inicializa_proceso(v_pid,g_proceso_cod,g_opera_cod,0,
                                 "CTAX01",g_archivo,g_usuario)
                                    RETURNING r_bandera
      IF r_bandera <> 0 THEN
         CALL fn_display_excepcion(r_bandera)
      ELSE
         CALL fn_actualiza_opera_ini(v_pid,g_proceso_cod,g_opera_cod,"","AFIX01",
                                     g_archivo,g_usuario) RETURNING r_bandera
         IF r_bandera THEN
            CALL fn_display_excepcion(r_bandera)
         ELSE
            SELECT ruta_bin
              INTO v_ruta_ejecutable
              FROM seg_modulo
             WHERE modulo_cod = "glo"

            LET v_comando = "nohup fglrun ", v_ruta_ejecutable CLIPPED,"/GLOE02.42r ",
                                             g_usuario," ",v_pid," ",g_proceso_cod," ",
                                             g_opera_cod," '",g_archivo,"' '", 
                                   "' 1>>", g_rutas.ruta_listados_bat CLIPPED ,
                                   "/nohup:",v_pid USING "&&&&&",":",
                                             g_proceso_cod USING "&&&&&",":",
                                             g_opera_cod   USING "&&&&&" ,
                                   " 2>&1 &"

            RUN v_comando
            IF STATUS THEN
               DISPLAY "*******EXCEPCION SACI*******"
               DISPLAY "Ocurrió un error al iniciar el proceso batch"
            ELSE
               DISPLAY "SE HA INICIADO EL PROCESO BATCH."
               DISPLAY "REVISAR EL DETALLE EN EL MONITOREO DE PROCESOS PARA EL PID: " ||v_pid
            END IF
         END IF
      END IF
   END IF
END FUNCTION

FUNCTION fn_display_excepcion(p_cod_salida)

DEFINE p_cod_salida  INTEGER,
       v_descripcion CHAR(150)

   # Se recupera la descripcion del codigo devuelto
   SELECT descripcion
     INTO v_descripcion 
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_cod_salida
   # Se muestra el mensaje
   DISPLAY "*******EXCEPCION SACI*******"
   DISPLAY v_descripcion
   
END FUNCTION
