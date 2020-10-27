############################################################################
#Proyecto          => SAFRE VIVIENDA                                       #
#Propietario       => EFP                                                  #
#Programa CTAX01   => LIQUIDA ARCHIVO ESPECIAL MOV INICIALES               #
#Fecha             => OCTUBRE DE 2014                                      #
############################################################################
DATABASE safre_viv

DEFINE g_usuario           CHAR(20)
DEFINE g_proceso_cod       SMALLINT
DEFINE g_opera_cod         SMALLINT
DEFINE g_archivo           CHAR(40)

MAIN 
   DEFINE v_tabla_preliq      CHAR(30)

   LET g_archivo     = ARG_VAL(1)
   LET g_proceso_cod = 798
   LET g_opera_cod   = 3
   LET g_usuario     = "OPSISSACI"

   SELECT nombre_tabla
     INTO v_tabla_preliq
     FROM cat_preliquida
    WHERE proceso_cod = g_proceso_cod
      AND opera_cod   = g_opera_cod
            
   -- si la operacion que se envio es de consulta de liquidacion
   CALL fn_liquidar_si(v_tabla_preliq)
END MAIN


FUNCTION fn_liquidar_si(v_tabla)
   DEFINE v_bandera           SMALLINT
   DEFINE v_tabla             STRING
   DEFINE v_folio             DECIMAL(9,0)
   DEFINE v_registros         SMALLINT

   LET v_bandera = 1

   SELECT count(*)
     INTO v_registros
     FROM glo_ctr_archivo
    WHERE nombre_archivo = g_archivo
      AND estado = 2

   IF v_registros < 1 THEN
      DISPLAY "*******EXCEPCION SACI*******"
      DISPLAY "EL ARCHIVO INDICADO NO HA SIDO INTEGRADO EN EL SISTEMA"
      LET v_bandera = 0
   ELSE
      SELECT a.folio
        INTO v_folio
        FROM glo_folio a
       WHERE a.proceso_cod = g_proceso_cod
         AND a.opera_cod   = 2
         AND a.status = 0

      IF v_folio IS NULL OR
         v_folio < 1 THEN
         DISPLAY "*******EXCEPCION SACI*******"
         DISPLAY "NO EXISTE FOLIO A LIQUIDAR"
         LET v_bandera = 0
      ELSE
         DISPLAY "********************************"
         DISPLAY " FOLIO A LIQUIDAR :    ",v_folio USING "###,###"
         CALL fn_ejecuta_liq_si(v_folio, 
                                     v_tabla)
      END IF
   END IF
END FUNCTION

FUNCTION fn_ejecuta_liq_si(p_folio, v_tabla)
   DEFINE p_folio             DECIMAL(9,0)
   DEFINE v_pid               DECIMAL(9,0)
   DEFINE v_tabla             STRING
   DEFINE v_comando           STRING
   DEFINE l_bat_ruta_listado  CHAR(40)
   DEFINE v_ruta_glo          CHAR(40)
   DEFINE v_desc_salida       VARCHAR(100)
   DEFINE v_bandera           SMALLINT -- para verificar resultado de iniciar la operacion
            
   SELECT pid
     INTO v_pid
     FROM bat_ctr_operacion
    WHERE proceso_cod = g_proceso_cod
      AND opera_cod   = g_opera_cod - 1
      AND folio = p_folio
            
   IF v_pid IS NULL OR v_pid = 0 THEN
      SELECT MAX(pid)
        INTO v_pid
        FROM bat_ctr_proceso
       WHERE proceso_cod = g_proceso_cod
         AND estado_cod  = 2
   END IF   
            
   SELECT ruta_listados
     INTO l_bat_ruta_listado
     FROM seg_modulo
    WHERE modulo_cod = 'bat'
            
   SELECT ruta_bin
     INTO v_ruta_glo
     FROM seg_modulo
    WHERE modulo_cod = 'glo'
            
   -- se inicia la operacion                             
   CALL fn_actualiza_opera_ini(v_pid,
                               g_proceso_cod,
                               g_opera_cod,
                               p_folio,
                               "CTAX03",
                               "",
                               g_usuario)
           RETURNING v_bandera

   IF ( v_bandera = 0 ) THEN
                              
      LET v_comando = "nohup fglrun ",v_ruta_glo CLIPPED,"/GLOG03 ",
                       g_usuario CLIPPED, " ",
                       v_pid            , " ",
                       g_proceso_cod    , " ",
                       g_opera_cod      , " ",
                       p_folio          , " ",
                       " 1>", l_bat_ruta_listado CLIPPED ,
                       "/nohup:",v_pid  USING "&&&&&",":",
                       g_proceso_cod    USING "&&&&&",":",
                       g_opera_cod      USING "&&&&&",
                       " 2>&1 &"
      RUN v_comando

      DISPLAY ""
      DISPLAY ""
      DISPLAY "SE HA ENVIADO LA LIQUIDACIÛN."
      DISPLAY "REVISAR EL AVANCE EN EL MONITOR DE EJECUCIÛN DE PROCESOS, PID:",v_pid
   ELSE
      SELECT descripcion
      INTO   v_desc_salida
      FROM   cat_bat_parametro_salida
      WHERE  cod_salida = v_bandera
      
      DISPLAY "*******EXCEPCION SACI*******"
      DISPLAY v_comando
   END IF
            
END FUNCTION
