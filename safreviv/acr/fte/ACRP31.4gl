################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 24/06/2012                                      #
################################################################################

################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => ACR                                                      #
#Programa          => ACRP31                                                   #
#Objetivo          => Programa para la liquidacion de saldo de fondo de ahorro #
#Fecha inicio      => 24/06/2012                                               #
################################################################################
DATABASE safre_viv
GLOBALS "ACRG10.4gl"
GLOBALS
DEFINE g_usuario_cod  LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_folio        LIKE deo_preliquida.folio_liquida
END GLOBALS
DEFINE arr_arbol_pre         DYNAMIC ARRAY OF RECORD
          subcuenta_desc     CHAR(30)     ,
          siefore            SMALLINT     ,
          monto_pesos        DECIMAL(28,6),
          monto_acciones     DECIMAL(28,6),
          subcuenta          SMALLINT     ,
          padre_id           STRING       ,
          id                 STRING       ,
          nivel              SMALLINT
       END RECORD
MAIN 
DEFINE p_pid                  LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod            LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_nombre_archivo       LIKE glo_ctr_archivo.nombre_archivo, 
       v_s_sql                STRING, -- cadena con una instruccion SQL
       v_i_resultado          INTEGER -- resultado del proceso
       ,r_bnd_fin_oper        SMALLINT
       ,v_si_correcto_integra SMALLINT
       ,p_titulo              STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje             STRING -- cuerpo del mensaje enviado
       ,v_error_isam          INTEGER
       ,v_mensaje             VARCHAR(250)
       ,r_valida              SMALLINT -- indica si el stored tuvo error
   
   ##Ejecuta prevalidación de saldos
   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET g_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   -- se crea el archivo log
   CALL STARTLOG(g_usuario_cod CLIPPED|| ".ACRP31.log")
   
   LET v_s_sql = "EXECUTE PROCEDURE sp_liquida_fondo72(?,?,?,?,?,?)"
   PREPARE prp_actualiza_edo1 FROM  v_s_sql
   EXECUTE prp_actualiza_edo1 USING g_folio      ,      
                                    g_usuario_cod,    
                                    p_pid        ,   
                                    p_proceso_cod,
                                    p_opera_cod  ,
                                    "dse_restitucion_fondo72"
                               INTO r_valida, v_error_isam, v_mensaje

   IF ( r_valida = 0 ) THEN 
      -- se finaliza la operacion
      LET r_valida = fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
      DISPLAY "Proceso de liquidación finaliza con código: ", r_valida
      DISPLAY v_mensaje
      
      -- se genera el reporte
      CALL fn_genera_reporte_fondo72(p_pid, p_proceso_cod, p_opera_cod)

   ELSE 
      DISPLAY "Ocurrió un error al realizar la liquidación de fondo 72: ", r_valida
      DISPLAY v_mensaje
      CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)
           RETURNING r_valida
   END IF
END MAIN

#Función para llamar al reporte de la liquidación con fondo de ahorro 72
FUNCTION fn_genera_reporte_fondo72(p_pid, p_proceso_cod, p_opera_cod)
DEFINE   p_pid             LIKE bat_ctr_proceso.pid,
         p_proceso_cod     LIKE cat_operacion.proceso_cod,
         p_opera_cod       LIKE cat_operacion.opera_cod,
         v_c_programa_cod  LIKE cat_operacion.programa_cod, -- nombrel del programa origen
         v_ruta_reporte    STRING, -- ruta del archivo del reporte
         v_ruta_listados   STRING, -- ruta de los listados
         v_ruta_ejecutable STRING, -- ruta del ejecutable
         v_origen_datos    STRING,
         v_indice          SMALLINT,
         manejador_rpt     om.SaxDocumentHandler  -- Contenedor documentos reporte
         
    LET v_origen_datos = g_usuario_cod
         
    -- se obtiene el nombrel del programa correspondiente
    LET v_c_programa_cod = fn_obten_nom_programa(p_proceso_cod , p_opera_cod)

    CALL fn_rutas("acr") RETURNING v_ruta_ejecutable, v_ruta_listados
    LET v_ruta_reporte = v_ruta_listados.trim(),
                         "/",
                         v_origen_datos.trim(),"-",
                         v_c_programa_cod CLIPPED,"-",
                         p_pid         USING "&&&&&","-",
                         p_proceso_cod USING "&&&&&","-",
                         p_opera_cod   USING "&&&&&",".pdf"

   DISPLAY "Ruta del reporte --- ",v_ruta_reporte

   --Se asigna la plantilla para generar el reporte
   IF ( fgl_report_loadCurrentSettings("ACRP311.4rp") ) THEN
       CALL fgl_report_selectDevice ("PDF")

       -- sin indica que no es necesario el preview
       CALL fgl_report_selectPreview(0)

       -- se indica que se escriba en archivo
       CALL fgl_report_setOutputFileName(v_ruta_reporte)
    
       -- se asigna la configuración en el menejador del reporte
       LET manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
       DISPLAY "No se pudo generar el reporte"
       EXIT PROGRAM
   END IF

   START REPORT rp_liquida_fondo72 TO XML HANDLER manejador_rpt

         FOR v_indice = 1 TO arr_arbol_pre.getLength()

              OUTPUT TO REPORT rp_liquida_fondo72 (arr_arbol_pre[v_indice].*)

    
         END FOR  
   DISPLAY "Termina reporte"
   FINISH REPORT rp_liquida_fondo72

END FUNCTION

#Reporte para la liquidación de con fondo de ahorro 72
REPORT rp_liquida_fondo72 (arr_datos)
DEFINE arr_datos          RECORD
         subcuenta_desc     CHAR(30)     ,
         siefore            SMALLINT     ,
         monto_pesos        DECIMAL(28,6),
         monto_acciones     DECIMAL(28,6),
         subcuenta          SMALLINT     ,
         padre_id           STRING       ,
         id                 STRING       ,
         nivel              SMALLINT
   END RECORD,
   fecha_reporte          DATE
   
   FORMAT 
      FIRST PAGE HEADER
         LET fecha_reporte = TODAY 
         PRINTX   g_usuario_cod
         PRINTX   fecha_reporte USING "dd-mm-yyyy"
         PRINTX   g_folio 

      ON EVERY ROW 
         PRINTX arr_datos.*
END REPORT 
