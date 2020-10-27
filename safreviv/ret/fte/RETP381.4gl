################################################################################
#Modulo        => RET                                                          #
#Programa      => RETP381                                                      #
#Ojetivo       => Programa lanzado para integrar archivo con cuentas clabe     #
#                 para transferencia.                                          #
#Fecha inicio  => 11 de Agosto, 2015.                                          #
################################################################################
IMPORT os
DATABASE safre_viv
GLOBALS "RETG01.4gl"

--Parametros de entrada del programa lanzado
--PID, proceso, operacion, folio, archivo y usuario
DEFINE g_pid            DECIMAL(9,0)
DEFINE g_proceso_cod    SMALLINT
DEFINE g_opera_cod      SMALLINT
DEFINE g_folio          DECIMAL(9,0)
DEFINE g_archivo        CHAR(50)
DEFINE g_usuario_cod    CHAR(20)
PRIVATE DEFINE v_ruta_listados            CHAR(40)
PRIVATE DEFINE v_ruta_listado             STRING

MAIN

   DEFINE
      v_estado                         SMALLINT,
      v_cuenta_archivo                 SMALLINT,
      v_sql_procedure                  STRING,
      r_bnd_oera_error                 SMALLINT,                              -- Bandera actualiza operacion en error
      r_cod_error                      SMALLINT,                              --Variables obtenidas desde el SPL
      r_error_isam                     INTEGER,                               --Variables obtenidas desde el SPL
      r_mensaje_error                  VARCHAR(255),                          --Variables obtenidas desde el SPL
      r_int,r_nss_dup,r_clabe_dup      INTEGER,                               --Variables obtenidas desde el SPL
      v_r_ret_ctr_archivo_clabe        RECORD
         folio                         DECIMAL(9,0),
         nombre_archivo                CHAR(40),
         num_registros                 INTEGER,
         f_actualiza                   DATE,
         h_actualiza                   DATETIME HOUR TO SECOND
      END RECORD,                                                             -- registro de control de archivo
      v_ruta_rescate                   LIKE seg_modulo.ruta_rescate,          -- ruta donde se colocara el archivo
      v_ruta_bin                       LIKE seg_modulo.ruta_bin               -- ruta donde estan los ejecutables

   LET g_usuario_cod = ARG_VAL(1)
   LET g_pid = ARG_VAL(2)
   LET g_proceso_cod = ARG_VAL(3)
   LET g_opera_cod = ARG_VAL(4)
   LET g_folio = ARG_VAL(5)
   LET g_archivo = ARG_VAL(6)

   --Texto a la bitacora
   CALL fn_display_proceso(0,"INICIA INTEGRACION")

   -- se obtiene la ruta de rescate y ejecutable
   SELECT ruta_rescate, ruta_bin
   INTO   v_ruta_rescate, v_ruta_bin
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"
   
   
   --Se cuenta el numero de registros del archivo
   SELECT COUNT(*)
   INTO   v_r_ret_ctr_archivo_clabe.num_registros
   FROM   safre_tmp:tmp_ret_det_ccc

   -- se inserta el registro
   LET v_r_ret_ctr_archivo_clabe.nombre_archivo = g_archivo
   LET v_r_ret_ctr_archivo_clabe.folio          = g_folio
   LET v_r_ret_ctr_archivo_clabe.f_actualiza    = TODAY
   LET v_r_ret_ctr_archivo_clabe.h_actualiza    = CURRENT HOUR TO SECOND

   SELECT COUNT(*) INTO v_cuenta_archivo
   FROM ret_ctr_archivo_clabe
   WHERE nombre_archivo = g_archivo
   IF v_cuenta_archivo > 0 THEN
   
      DISPLAY "El archivo ",g_archivo CLIPPED," ya fue cargado previamente."

      --Se marca la operacion en error
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_bnd_oera_error
      
   ELSE
      --
      DISPLAY "EJECUTANDO STORED DE INTEGRACION: ",TODAY
      -- se prepara y ejecuta el procedimiento de integracion de respuesta de fico pago por dap
      LET v_sql_procedure = "EXECUTE FUNCTION fn_ret_integra_ccc(?)"
      PREPARE prp_procedure FROM v_sql_procedure
      EXECUTE prp_procedure USING g_folio
      INTO r_cod_error, r_error_isam, r_mensaje_error
      
      -- si no se integro correctamente
      IF r_cod_error < 0 THEN
         DISPLAY "Error al ejecutar el SP de integración de respuesta de FICO pago por DAP"     
         DISPLAY "Código de ERROR SQL ",SQLCA.sqlcode
         DISPLAY "Error (SQL)    : ", r_cod_error
         DISPLAY "Error (ISAM)   : ", r_error_isam
         DISPLAY "Error (mensaje): ", r_mensaje_error

         --Se marca la operacion en error
         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_bnd_oera_error
      ELSE
         
         INSERT INTO ret_ctr_archivo_clabe VALUES ( v_r_ret_ctr_archivo_clabe.* )
         
         DISPLAY "Detalle de integracion del archivo"
         DISPLAY "Registros integrados correctamente: ",r_int USING "<,<<<,<<<"
         --Actualizo estado y folio
         UPDATE glo_ctr_archivo
             SET folio = g_folio,
                estado = 2
           WHERE nombre_archivo = g_archivo
         --
         CALL fn_actualiza_opera_fin(g_pid,
                             g_proceso_cod,
                             g_opera_cod)
                             RETURNING v_estado
         DISPLAY "Integración realizada con éxito."
      END IF
   END IF
   
   CALL fn_display_proceso(1,"INTEGRACION")

   DELETE 
   FROM glo_ctr_archivo
   WHERE proceso_cod = g_proceso_carga_cuentas_clabe
    
END MAIN

FUNCTION fn_configuracion(v_reporte, v_formato, v_preview)
---------------------------------------------------------------------------

  DEFINE 
    v_reporte                STRING,
    v_formato                STRING,
    v_preview                INTEGER

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'ret'

   LET v_ruta_listado = v_ruta_listados CLIPPED , "/" ,
                        g_usuario_cod CLIPPED , "-",        -- usuario
                        "RETL381" CLIPPED, "-",             -- programa
                        g_pid USING "&&&&&","-",            -- PID
                        g_proceso_cod USING "&&&&&", "-",   -- codigo del proceso
                        g_opera_cod   USING "&&&&&",".pdf"  -- codigo de la operación

   DISPLAY "Ruta del listado:"
   DISPLAY v_ruta_listado

   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- if  the file loaded OK
      CALL fgl_report_setOutputFileName(v_ruta_listado)
      CALL fgl_report_selectDevice(v_formato)
      CALL fgl_report_selectPreview(v_preview)
   ELSE
       DISPLAY "Error: No se pudo encontrar el archivo ", v_reporte
       EXIT PROGRAM
   END IF
  
   RETURN fgl_report_commitCurrentSettings()

END FUNCTION
