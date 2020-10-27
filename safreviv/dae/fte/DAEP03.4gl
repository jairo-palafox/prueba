################################################################################
#Modulo            => DAE                                                      #
#Programa          => DAEP03                                                   #
#Objetivo          => Programa que genera el estado de cuenta exclusivo de     #
#                     Amortizaciones Excedentes en formato PDF                 #
#Fecha inicio      => Julio 2013                                               #
################################################################################

GLOBALS "DAEP03.inc"

DATABASE safre_viv

PRIVATE DEFINE v_parametros datos_edo_cuenta

PUBLIC FUNCTION fn_genera_estado_cuenta(p_ruta, p_parametros)
   DEFINE p_ruta        STRING 
   DEFINE p_parametros  datos_edo_cuenta

   DEFINE reporte          om.SaxDocumentHandler
   DEFINE archivo          STRING
   DEFINE i                SMALLINT

   LET v_parametros.* = p_parametros.*
   LET archivo = ""

   IF fgl_report_loadCurrentSettings("DAEP030.4rp") THEN
      CALL fgl_report_selectDevice("PDF")# PDF, XLS, HTML
      IF p_ruta IS NULL OR p_ruta.getLength() = 0 THEN
         CALL fgl_report_selectPreview(TRUE)
      ELSE 
         LET archivo = p_ruta, v_parametros.nss, " ", TODAY USING "DD-MM-YYYY" 
         CALL fgl_report_selectPreview(FALSE)
         CALL fgl_report_setOutputFileName(archivo)
      END IF
      LET reporte = fgl_report_commitCurrentSettings()
      IF reporte IS NOT NULL THEN
         START REPORT estado_cuenta TO XML HANDLER reporte
            FOR i = 1 TO v_parametros.movimientos.getLength()
               IF v_parametros.movimientos[i].subcuenta IS NOT NULL THEN
                  LET v_parametros.hoja_detalle = TRUE
                  OUTPUT TO REPORT estado_cuenta(v_parametros.movimientos[i].*)
               END IF
            END FOR 
            IF NOT v_parametros.hoja_detalle THEN
               OUTPUT TO REPORT estado_cuenta(v_parametros.movimientos[1].*)
            END IF
         FINISH REPORT estado_cuenta
      END IF
   END IF
   RETURN archivo
END FUNCTION

REPORT estado_cuenta(p_detalle)
   DEFINE p_detalle RECORD
      subcuenta               LIKE cta_movimiento.subcuenta,
      fecha                   LIKE cta_movimiento.f_liquida,
      nrp                     varchar(80),
      tipo_movimiento         LIKE cat_movimiento.movimiento_desc,
      monto_pesos             LIKE cta_movimiento.monto_pesos
   END RECORD
   DEFINE imagen_grupo        STRING

   ORDER BY p_detalle.subcuenta, p_detalle.fecha DESC, p_detalle.tipo_movimiento
   
   FORMAT

   FIRST PAGE HEADER

   PRINTX   v_parametros.nombre_completo,
            v_parametros.calle,
            v_parametros.numero,
            v_parametros.colonia,
            v_parametros.municipio,
            v_parametros.estado,
            v_parametros.codigo_postal,
            v_parametros.fecha_corte USING "DD-MM-YYYY",
            v_parametros.periodo_inicio USING "DD-MM-YYYY",
            v_parametros.periodo_fin USING "DD-MM-YYYY",
            v_parametros.nss,
            v_parametros.curp,
            v_parametros.tipo_derechohabiente,
            v_parametros.origen_afiliacion,
            v_parametros.indicador_credito,
            v_parametros.desc_voluntaria,
            v_parametros.anterior_voluntaria,
            v_parametros.cargo_voluntaria,
            v_parametros.abono_voluntaria,
            v_parametros.final_voluntaria,
            v_parametros.saldo_total,
            v_parametros.hoja_detalle

   BEFORE GROUP OF p_detalle.subcuenta
      CASE p_detalle.subcuenta
            WHEN 46
                LET imagen_grupo = '/safreviv/img/excedentes.png'       #CAMBIAR IMAGEN
        END CASE
        PRINTX imagen_grupo
   ON EVERY ROW

   PRINTX   p_detalle.fecha USING "DD-MM-YYYY",
            p_detalle.nrp,
            p_detalle.monto_pesos,
            p_detalle.subcuenta,
            p_detalle.tipo_movimiento
END REPORT