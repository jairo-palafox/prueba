############################################################################
#Proyecto          => SAFRE VIVIENDA                                       #
#Propietario       => EFP                                                  #
#Programa CTAX01   => INTEGRA Y PRELIQUIDA ARCHIVO ESPECIAL MOV INICIALES  #
#Fecha             => OCTUBRE DE 2014                                      #
############################################################################
DATABASE safre_viv

DEFINE g_proceso_cod    SMALLINT
DEFINE g_opera_cod      SMALLINT
DEFINE g_archivo        CHAR(40)
DEFINE g_usuario        CHAR(20)


MAIN
   LET g_archivo     = ARG_VAL(1)
   LET g_proceso_cod = 798
   LET g_opera_cod   = 2 
   LET g_usuario     = "OPSISSACI"
   IF fn_val_archivo() THEN
      CALL fn_lanza_integra()
   END IF
END MAIN


FUNCTION fn_val_archivo()
   DEFINE v_bandera        SMALLINT
   DEFINE v_registros      INTEGER

   LET v_bandera = 1

   SELECT count(*)
     INTO v_registros
     FROM glo_ctr_archivo
    WHERE nombre_archivo = g_archivo
      AND estado = 1

   IF v_registros < 1 THEN
      DISPLAY "*******EXCEPCION SACI*******"
      DISPLAY "EL ARCHIVO INDICADO NO HA SIDO VALIDADO EN EL SISTEMA"
      LET v_bandera = 0
   ELSE
      SELECT COUNT(*)
        INTO v_registros
        FROM glo_valor_fondo
       WHERE fondo = 11
         AND f_valuacion = TODAY 
      IF v_registros < 1 THEN
         DISPLAY "*******EXCEPCION SACI*******"
         DISPLAY "NO EXISTE PRECIO DE ACCIÓN"
         LET v_bandera = 0
      ELSE
         SELECT count(*)
           INTO v_registros
           FROM safre_tmp:tmp_sdo_si_pendiente

         IF v_registros < 1 THEN
            DISPLAY "*******EXCEPCION SACI*******"
            DISPLAY "NO EXISTEN REGISTROS A INTEGRAR EN EL SISTEMA"
            LET v_bandera = 0
         ELSE
            DISPLAY "**************************"
            DISPLAY "TOTAL DE REGISTROS A INTEGRAR: ", v_registros USING "&&&,&&&,&&&"
         END IF
      END IF
   END IF

   RETURN v_bandera
END FUNCTION


FUNCTION fn_lanza_integra()
   DEFINE v_pid               INTEGER
   DEFINE v_folio             DECIMAL(9,0)
   DEFINE r_bandera           INTEGER
   DEFINE v_comando           STRING
   DEFINE v_ruta_ejecutable   CHAR(40)
   DEFINE v_ruta_listados_bat CHAR(40)

   LET r_bandera = 0

   SELECT MAX(pid)
     INTO v_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod
      AND estado_cod = 2

   CALL fn_valida_operacion(v_pid,g_proceso_cod,g_opera_cod) RETURNING r_bandera
   
   IF r_bandera <> 0 THEN
      CALL fn_display_excepcion(r_bandera)
   ELSE
      CALL fn_genera_folio(g_proceso_cod,g_opera_cod,g_usuario)
               RETURNING v_folio

      CALL fn_actualiza_opera_ini(v_pid,g_proceso_cod,g_opera_cod, v_folio,"AFIX02",
                                  g_archivo,g_usuario) RETURNING r_bandera
      IF r_bandera THEN
         CALL fn_display_excepcion(r_bandera)
      ELSE
         SELECT ruta_bin
           INTO v_ruta_ejecutable
           FROM seg_modulo
          WHERE modulo_cod = "cta"

          SELECT ruta_listados
           INTO v_ruta_listados_bat
           FROM seg_modulo
          WHERE modulo_cod = "bat"

         LET v_comando = "nohup fglrun ", v_ruta_ejecutable CLIPPED,"/CTAX021.42r ",
                                          g_usuario," ",v_pid," ",g_proceso_cod," ",
                                          g_opera_cod," '",g_archivo CLIPPED,"' ",v_folio, 
                                " 1>>", v_ruta_listados_bat CLIPPED ,
                                "/nohup:",v_pid USING "&&&&&",":",
                                          g_proceso_cod USING "&&&&&",":",
                                          g_opera_cod   USING "&&&&&" ,
                                " 2>&1 &"

         RUN v_comando
         IF STATUS THEN
            DISPLAY "*******EXCEPCION SACI*******"
            DISPLAY "OCURRIÓ UN ERROR AL INICIAR EL PROCESO BATCH"
         ELSE
            DISPLAY "SE HA INICIADO EL PROCESO BATCH."
            DISPLAY "REVISAR EL DETALLE EN EL MONITOREO DE PROCESOS PARA EL PID: " ||v_pid
         END IF
      END IF
   END IF
END FUNCTION


FUNCTION fn_display_excepcion(p_cod_salida)

   DEFINE p_cod_salida  INTEGER
   DEFINE v_descripcion CHAR(150)

   # Se recupera la descripcion del codigo devuelto
   SELECT descripcion
     INTO v_descripcion 
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_cod_salida
   # Se muestra el mensaje
   DISPLAY "*******EXCEPCION SACI*******"
   DISPLAY v_descripcion
   
END FUNCTION
