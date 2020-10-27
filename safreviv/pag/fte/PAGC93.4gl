-----------------------------------------------------------------------------------------
-- Modulo        => PAG
-- Programa      => PAGC93
-- Objetivo      => Consulta de registros cambiaVit por fecha de pago
-- Autor         => GERARDO ALFONSO VEGA PAREDES
-- Fecha inicio  => 5 de Febrero de 2019
-- Requerimiento =>
-----------------------------------------------------------------------------------------
-- Modificación =>
-- Fehca        =>
-- Autor        =>
-- Clave cambio =>
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod

MAIN
   CALL STARTLOG("PAGC93.log")
   CLOSE WINDOW SCREEN

   CALL fn_principal()

END MAIN

FUNCTION fn_principal()
   DEFINE v_fecha DATE
   DEFINE v_contador INTEGER

   LET v_contador = 0
   LET v_fecha = TODAY
   OPEN WINDOW w1 WITH FORM "PAGC931"
   INPUT BY NAME v_fecha ATTRIBUTES (UNBUFFERED) 
      ON ACTION ACCEPT
         IF v_fecha IS NULL THEN
            CALL fn_mensaje("Atencion","Debe capturar una fecha","stop")
            CONTINUE INPUT
         ELSE
            DECLARE cur_fecha CURSOR FROM "SELECT count(*) FROM pag_det_cvt WHERE f_pago = ? "
            FOREACH cur_fecha USING v_fecha INTO v_contador

            END FOREACH
            DISPLAY v_fecha
            DISPLAY v_contador
            IF v_contador = 0 THEN
               CALL fn_mensaje("Atención","Para la fecha capturada no existen liquidaciones","stop")
            ELSE
               CALL fn_despliega_datos(v_fecha)
            END IF
         END IF 

   END INPUT

   CLOSE WINDOW w1
END FUNCTION

FUNCTION fn_despliega_datos(v_fecha)
   DEFINE v_fecha DATE
   DEFINE i SMALLINT
   DEFINE reg_det DYNAMIC ARRAY OF RECORD
      nss         CHAR(11),
      curp        CHAR(18),
      credito     DECIMAL(10,0),
      cod_ident   CHAR(10),
      marca_cam   CHAR(04),
      num_caso    DECIMAL(10,0),
      f_pago      DATE,
      monto_dep   DECIMAL(12,2),
      ind_reg     SMALLINT,
      result_oper CHAR(02)
   END RECORD

   DEFINE reg_actual        INTEGER
   DEFINE v_ruta_ejecutable STRING
   DEFINE v_ruta_listados   STRING
   DEFINE v_ruta_reporte    STRING
   DEFINE manejador_rpt     om.SaxDocumentHandler

   OPEN WINDOW w2 WITH FORM "PAGC932"

   DECLARE cur_det CURSOR FROM "SELECT afi.nss,
                                       det.curp,
                                       det.credito_infonavit,
                                       det.cod_identificacion,
                                       det.marca_cambio_casa,
                                       det.num_caso,
                                       det.f_pago,
                                       det.monto_deposito,
                                       det.ind_registro,
                                       det.result_operacion
                                FROM   pag_det_cvt det,
                                       afi_derechohabiente afi
                                WHERE  f_pago = ? 
                                AND    afi.id_derechohabiente = det.id_derechohabiente "
   LET i = 1
   FOREACH cur_det USING v_fecha INTO reg_det[i].*
      LET i = i + 1
   END FOREACH

   CALL reg_det.deleteElement(i)
   DISPLAY ARRAY reg_det TO record1.*
      ON ACTION REPORTE
         CALL arr_curr() RETURNING reg_actual
         CALL fn_rutas("pag") RETURNING v_ruta_ejecutable, v_ruta_listados

--         CALL fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/PAGC93.4rp") RETURNING v_existe
--         IF v_existe THEN

         IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/PAGC93.4rp") THEN
            CALL fgl_report_selectDevice("PDF")
            LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","PAGC93.Consulta_detalle_GP"

            CALL fgl_report_setOutputFileName(v_ruta_reporte)
            CALL fgl_report_selectPreview(1)  -- 1=envia reporte a pantalla. 2=enviar reporte a ruta
            LET manejador_rpt =  fgl_report_commitCurrentSettings()

         ELSE
            DISPLAY "No fue posible generar el reporte. No se encuentra la plantilla PAGC93.4rp"
            EXIT PROGRAM
         END IF

         END DISPLAY
   CLOSE WINDOW w2
END FUNCTION 