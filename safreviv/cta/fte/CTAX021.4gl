############################################################################
#Proyecto          => SAFRE VIVIENDA                                       #
#Propietario       => EFP                                                  #
#Programa CTAX01   => INTEGRA Y PRELIQUIDA ARCHIVO ESPECIAL MOV INICIALES  #
#Fecha             => OCTUBRE DE 2014                                      #
############################################################################
DATABASE safre_viv

DEFINE g_usuario        CHAR(20)
DEFINE g_pid            DECIMAL(9,0)
DEFINE g_proceso_cod    SMALLINT
DEFINE g_opera_cod      SMALLINT
DEFINE g_archivo        CHAR(40)
DEFINE g_folio          DECIMAL(9,0)
DEFINE g_hoy            DATE

MAIN
   DEFINE v_bandera     INTEGER

   LET g_usuario      = ARG_VAL(1)
   LET g_pid          = ARG_VAL(2)
   LET g_proceso_cod  = ARG_VAL(3)
   LET g_opera_cod    = ARG_VAL(4)
   LET g_archivo      = ARG_VAL(5)
   LET g_folio        = ARG_VAL(6)

   LET g_hoy         = TODAY

   CALL fn_display_proceso(0,"INTEGRA MOVIMIENTOS ESPECALES SI")

   CALL fn_prepara_cuentas() RETURNING v_bandera

   IF v_bandera = 0 THEN
      DISPLAY ""
      DISPLAY "GENERANDO REPORTE DE CIFRAS CONTROL"
      DISPLAY ""

      CALL fn_genera_reporte()

      DISPLAY ""
      DISPLAY "REPORTE DE CIFRAS CONTROL GENERADO CORRECTAMENTE..."
      DISPLAY ""

      CALL fn_actualiza_opera_fin(g_pid,
                                 g_proceso_cod,
                                 g_opera_cod)
                                 RETURNING v_bandera

      UPDATE glo_ctr_archivo
         SET estado = 2
       WHERE nombre_archivo = g_archivo
         AND estado = 1
   ELSE
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod)  RETURNING v_bandera
   END IF
   --Se ejecutan los displays
   CALL fn_display_proceso(1,"INTEGRA MOVIMIENTOS ESPECALES SI")


END MAIN

FUNCTION fn_prepara_cuentas ()

   DEFINE v_sql_procedure  STRING
   DEFINE v_estatus        SMALLINT
   DEFINE r_cod_error      SMALLINT
   DEFINE r_error_isam     INTEGER
   DEFINE r_mensaje_error  VARCHAR(255)

   LET v_estatus = 0 --El cero indica que se jecuto con exito
   LET v_sql_procedure = "EXECUTE PROCEDURE sp_integra_mov_si_especial(?,?)"

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE prp_prepara FROM v_sql_procedure
   LET r_cod_error = 0
   EXECUTE prp_prepara USING g_folio, g_hoy
      INTO r_cod_error, r_error_isam, r_mensaje_error

   IF (r_cod_error = 0) THEN
      DISPLAY r_mensaje_error
      DISPLAY r_cod_error
   ELSE
      LET v_estatus = 1  --El uno indca que ocurrio un error al ejecutarse
      DISPLAY "\n [SACI EXCEPCION ] "
      DISPLAY "Error de ejecución en 'sp_integra_mov_si_especial' (SQL): ",r_cod_error
      DISPLAY "Error en 'sp_integra_mov_si_especial' (ISAM):",r_error_isam,"\n"
      DISPLAY "Error en 'sp_integra_mov_si_especial' (Mensaje):",r_mensaje_error,"\n"
   END IF

   RETURN r_cod_error
END FUNCTION


FUNCTION fn_genera_reporte()
   DEFINE v_reporte           STRING
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE v_ruta_reporte      STRING
   DEFINE v_excepcion         SMALLINT

   DEFINE report_handler      om.SaxDocumentHandler

   DEFINE r_reporte           RECORD
      subcuenta                  SMALLINT,
      fondo                      SMALLINT,
      movimiento                 SMALLINT,
      acciones                   DECIMAL(22,6),
      pesos                      DECIMAL(18,2),
      total                      INTEGER
   END RECORD

   LET v_reporte = "CTAX021.4rp"

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'cta'

   LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" ,
                        g_usuario CLIPPED , "-", -- usuario
                        "CTAX02", "-", -- programa
                        g_pid USING "&&&&&","-", -- PID
                        g_proceso_cod USING "&&&&&", "-", -- codigo del proceso
                        g_opera_cod   USING "&&&&&",".pdf" -- codigo de la operación

   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- if  the file loaded OK
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
   ELSE
      DISPLAY "[ SACI EXCEPCIÓN ] NO SE ENCUENTRA LA PLANTILLA PARA GENERAR EL REPORTE: ", v_reporte
      LET v_excepcion = 1
   END IF
   IF NOT v_excepcion THEN
      DECLARE cur_saldos CURSOR FOR SELECT a.subcuenta,
                                           a.fondo_inversion,
                                           a.movimiento,
                                           SUM(monto_acciones),
                                           SUM(monto_pesos),
                                           COUNT(*)
                                      FROM cta_si_preliquida a
                                     WHERE folio_liquida = g_folio
                                     GROUP BY 1,2,3
                                     ORDER BY 1,2,3 

      START REPORT rep_saldos TO XML HANDLER report_handler
      FOREACH cur_saldos INTO r_reporte.*
         OUTPUT TO REPORT rep_saldos(r_reporte.*)
      END FOREACH
      FINISH REPORT rep_saldos
   END IF
END FUNCTION

REPORT rep_saldos(p_reporte)
   DEFINE p_reporte           RECORD
      subcuenta                  SMALLINT,
      fondo                      SMALLINT,
      movimiento                 SMALLINT,
      acciones                   DECIMAL(22,6),
      pesos                      DECIMAL(18,2),
      total                      INTEGER
   END RECORD
   DEFINE p_r_encabezado    RECORD
        p_usuario_cod         STRING,
        p_fecha               DATE,
        f_corte               DATE,
        folio                 DECIMAL(9,0),
        archivo               STRING,
        cuentas_previas       INTEGER,
        acciones_previas      DECIMAL(22,6)
   END RECORD

   DEFINE desc_movimiento     CHAR(30)
   DEFINE desc_subcuenta      CHAR(30)
   DEFINE desc_fondo          CHAR(30)
   DEFINE v_total_cuentas     INTEGER
   DEFINE v_total_acciones    DECIMAL(22,6)
   DEFINE v_total_pesos       DECIMAL(18,2)
   FORMAT

      FIRST PAGE HEADER
         LET v_total_cuentas    = 0

         LET p_r_encabezado.p_fecha = TODAY
         LET p_r_encabezado.p_usuario_cod = g_usuario
         LET p_r_encabezado.f_corte   = g_hoy
         LET p_r_encabezado.folio     = g_folio
         LET p_r_encabezado.archivo   = g_archivo

         SELECT sum(acciones/1000000), count(*)
           INTO p_r_encabezado.acciones_previas, p_r_encabezado.cuentas_previas
           FROM safre_tmp:tmp_sdo_si_pendiente

         PRINTX p_r_encabezado.*

      ON EVERY ROW
         SELECT movimiento_desc
           INTO desc_movimiento
           FROM cat_movimiento
          WHERE movimiento = p_reporte.movimiento

         SELECT subcuenta_desc
           INTO desc_subcuenta
           FROM cat_subcuenta
          WHERE subcuenta = p_reporte.subcuenta

         SELECT razon_social
           INTO desc_fondo
           FROM cat_fondo_local
          WHERE fondo = p_reporte.fondo
         
         PRINTX p_reporte.*, desc_fondo, desc_movimiento, desc_subcuenta

         LET v_total_cuentas  = v_total_cuentas  + p_reporte.total
         --LET v_total_acciones = v_total_acciones + p_reporte.acciones
         --LET v_total_pesos    = v_total_pesos    + p_reporte.pesos  

      ON LAST ROW
         LET v_total_acciones = SUM(p_reporte.acciones)
         LET v_total_pesos    = SUM(p_reporte.pesos)
         PRINTX v_total_cuentas, v_total_acciones, v_total_pesos
END REPORT
