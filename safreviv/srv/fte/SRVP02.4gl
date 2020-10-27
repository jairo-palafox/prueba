####################################################################
#Modulo            =>SRV                                           #
#Programa          =>SRVP02                                        #
#Objetivo          =>Programa que genera un pdf con el estado de   #
#                    cuenta del afiliado                           #
#Fecha inicio      =>16 Marzo 2012                                 #
####################################################################

GLOBALS "SRVP02.inc"

DATABASE safre_viv

PRIVATE DEFINE v_parametros datos_edo_cuenta

PUBLIC FUNCTION fn_genera_estado_cuenta(p_pantalla, p_parametros)
   DEFINE p_pantalla    BOOLEAN 
   DEFINE p_parametros  datos_edo_cuenta

   DEFINE reporte          om.SaxDocumentHandler
   DEFINE archivo          STRING
   DEFINE i                SMALLINT

   LET v_parametros.* = p_parametros.*

   IF fgl_report_loadCurrentSettings("SRVP02.4rp") THEN
      CALL fgl_report_selectDevice("PDF")# PDF, XLS, HTML
      IF p_pantalla THEN
         CALL fgl_report_selectPreview(TRUE)
      ELSE 
         LET archivo = "/safreviv/srv/edo_cuenta/", v_parametros.nss, " ", TODAY USING "DD-MM-YYYY" 
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
            v_parametros.desc_72,
            v_parametros.anterior_72,
            v_parametros.cargo_72,
            v_parametros.abono_72,
            v_parametros.final_72,
            v_parametros.desc_92,
            v_parametros.anterior_92,
            v_parametros.cargo_92,
            v_parametros.abono_92,
            v_parametros.final_92,
            v_parametros.desc_97,
            v_parametros.anterior_97,
            v_parametros.cargo_97,
            v_parametros.abono_97,
            v_parametros.final_97,
            v_parametros.desc_amortizacion,
            v_parametros.anterior_amortizacion,
            v_parametros.cargo_amortizacion,
            v_parametros.abono_amortizacion,
            v_parametros.final_amortizacion,
            v_parametros.desc_92_infonavit,
            v_parametros.anterior_92_infonavit,
            v_parametros.cargo_92_infonavit,
            v_parametros.abono_92_infonavit,
            v_parametros.final_92_infonavit,
            v_parametros.desc_97_infonavit,
            v_parametros.anterior_97_infonavit,
            v_parametros.cargo_97_infonavit,
            v_parametros.abono_97_infonavit,
            v_parametros.final_97_infonavit,
            v_parametros.desc_amortizacion_infonavit,
            v_parametros.anterior_amortizacion_infonavit,
            v_parametros.cargo_amortizacion_infonavit,
            v_parametros.abono_amortizacion_infonavit,
            v_parametros.final_amortizacion_infonavit,
            v_parametros.saldo_total,
            v_parametros.hoja_detalle

   BEFORE GROUP OF p_detalle.subcuenta
      CASE p_detalle.subcuenta
            WHEN 41
                LET imagen_grupo = '/safreviv/srv/img/amortizacion.png'
            WHEN 43
                LET imagen_grupo = '/safreviv/srv/img/amortizacionI.png'
            WHEN 40
                LET imagen_grupo = '/safreviv/srv/img/fondos.png'
            WHEN 8
                LET imagen_grupo = '/safreviv/srv/img/viv2.png'
            WHEN 42
                LET imagen_grupo = '/safreviv/srv/img/viv2_2.png'
            WHEN 4
                LET imagen_grupo = '/safreviv/srv/img/viv97.png'
            WHEN 44
                LET imagen_grupo = '/safreviv/srv/img/viv97_1.png'
        END CASE
        PRINTX imagen_grupo
   ON EVERY ROW

   PRINTX   p_detalle.fecha USING "DD-MM-YYYY",
            p_detalle.nrp,
            p_detalle.monto_pesos,
            p_detalle.subcuenta,
            p_detalle.tipo_movimiento
END REPORT