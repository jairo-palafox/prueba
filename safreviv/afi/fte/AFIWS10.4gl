####################################################################
#Modulo            =>AFI                                           #
#Programa          =>AFIWS10.4gl                                   #
#Objetivo          =>                                              #
#Fecha inicio      =>11 NOVIEMBRE 2015                             #
#AUTOR             => José Eduardo Ventura                         #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT XML

DATABASE safre_viv

   DEFINE nss_in  RECORD
          nss     CHAR(11)
   END RECORD

   DEFINE res_out   DYNAMIC ARRAY OF RECORD
          nss             CHAR (11),
          clave           CHAR(3),
          descripcion     CHAR(50),
          folioLQ         DECIMAL(9,0),
          nrp             CHAR(11),
          bimestrePago    CHAR(6),
          folioSua        DECIMAL(9,0),
          fechaPago       DATE,
          indLiquidacion  VARCHAR(50),
          tipoAclaracion  VARCHAR(150),
          aportacion      DECIMAL(12,2),
          amortizacion    DECIMAL(12,2)
   END RECORD

   DEFINE a SMALLINT
   DEFINE bnd_dato SMALLINT
   DEFINE v_dato CHAR(1)
   
MAIN
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER

   CALL CreateMovimientosAclaratorios() RETURNING servicio

   CALL com.WebServiceEngine.Start()

   DISPLAY("The server is listening.")

   WHILE TRUE
      # Procesa cada request, regresa un entero que representa el estatus del request
      # el parametro -1 representa un valor infinito de espera
      LET respuesta = com.WebServiceEngine.ProcessServices(-1)
      CASE respuesta
         WHEN 0
            --DISPLAY "Request processed."
         WHEN -1
            DISPLAY "Timeout reached."
         WHEN -2
            DISPLAY "Disconnected from application server."
            EXIT PROGRAM
         WHEN -3
            DISPLAY "Client Connection lost."
         WHEN -4
            DISPLAY "Server interrupted with Ctrl-C."
         WHEN -10
            DISPLAY "Internal server error."
     END CASE

     IF int_flag<>0 THEN
        LET int_flag=0
        EXIT WHILE
     END IF
   END WHILE
END MAIN

#-------------------------------------------------------------------------------
# Service: MovimientosAclaratorios
# Port:    MovimientosAclaratorios
#-------------------------------------------------------------------------------
#
# FUNCTION MomivimientosAclaratorios
#   RETURNING soapstatus
#
FUNCTION CreateMovimientosAclaratorios()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("MovimientosAclaratorios","http://services.safre.efp.com")

    # Publish Operation : Marcas Operativas Activas
    LET operation = com.WebOperation.CreateDOCStyle("movimientosaclaratorios","movimientosaclaratorios",nss_in,res_out)
    CALL service.publishOperation(operation,"")


    #
    # Register Service
    #
    CALL com.WebServiceEngine.RegisterService(service)
    RETURN 0

  CATCH
    RETURN STATUS
  END TRY

END FUNCTION

FUNCTION movimientosaclaratorios()
   DEFINE v_nss               CHAR(11)
   DEFINE v_cta               SMALLINT
   DEFINE v_qry               STRING
   DEFINE b                   SMALLINT
   DEFINE v_cta_reg           INTEGER
   DEFINE v_id_derechohabiente DECIMAL(9,0)

   LET v_nss = nss_in.nss
      IF LENGTH(v_nss) <> 11 THEN
         LET res_out[1].nss         = v_nss
         LET res_out[1].clave       = (10)
         LET res_out[1].descripcion ="NSS incorrecto"
         LET res_out[1].folioLQ        = ""
         LET res_out[1].nrp            = ""
         LET res_out[1].bimestrePago   = ""
         LET res_out[1].folioSua       = ""
         LET res_out[1].fechaPago      = ""
         LET res_out[1].indLiquidacion = ""
         LET res_out[1].tipoAclaracion = ""
         LET res_out[1].aportacion     = ""
         LET res_out[1].amortizacion   = ""
      ELSE
         LET a = 1
         FOR a= 1 TO LENGTH(v_nss)
            LET v_dato = v_nss[a,a]
            CALL fn_valida_numero(v_dato)
            IF bnd_dato = 0 THEN
               LET res_out[1].nss         = v_nss
               LET res_out[1].clave       = (10)
               LET res_out[1].descripcion ="NSS incorrecto"
               LET res_out[1].folioLQ        = ""
               LET res_out[1].nrp            = ""
               LET res_out[1].bimestrePago   = ""
               LET res_out[1].folioSua       = ""
               LET res_out[1].fechaPago      = ""
               LET res_out[1].indLiquidacion = ""
               LET res_out[1].tipoAclaracion = ""
               LET res_out[1].aportacion     = ""
               LET res_out[1].amortizacion   = ""
               EXIT FOR
            END IF
         END FOR

         IF bnd_dato = 1 THEN

            LET v_cta = 0
            SELECT count(*)
              INTO v_cta
              FROM afi_derechohabiente
             WHERE nss = v_nss

            IF v_cta > 0 THEN
               SELECT id_derechohabiente
                 INTO v_id_derechohabiente
                 FROM afi_derechohabiente
                WHERE nss = v_nss

               SELECT COUNT(*)
                 INTO v_cta_reg
                 FROM cta_his_pagos cta,pag_ind_liquidacion ind,pag_tpo_aclaracion tpo
                WHERE cta.id_derechohabiente = v_id_derechohabiente
                  AND cta.ind_liquidacion = ind.ind_liquidacion
                  AND cta.tpo_aclaracion = tpo.aclaracion_cod
                  AND cta.origen_archivo = 1
                  AND cta.ind_liquidacion = 1

               IF v_cta_reg > 0 THEN
                  LET v_qry = 
                      "SELECT FIRST 30 ",
                              "'",v_nss,"'",
                              ",'20', 
                              ' NSS correcto con movimientos', 
                              cta.folio,
                              cta.nrp,
                         CASE
                            WHEN cta.periodo_pago[5,6] IN (01,02) THEN cta.periodo_pago[1,4]||'01'
                            WHEN cta.periodo_pago[5,6] IN (03,04) THEN cta.periodo_pago[1,4]||'02'
                            WHEN cta.periodo_pago[5,6] IN (05,06) THEN cta.periodo_pago[1,4]||'03'
                            WHEN cta.periodo_pago[5,6] IN (07,08) THEN cta.periodo_pago[1,4]||'04'
                            WHEN cta.periodo_pago[5,6] IN (09,10) THEN cta.periodo_pago[1,4]||'05'
                            WHEN cta.periodo_pago[5,6] IN (11,12) THEN cta.periodo_pago[1,4]||'06'
                         END, cta.folio_sua, 
                              cta.f_pago, 
                              ind.desc_ind_liquidacion, 
                              cta.tpo_aclaracion || ' - ' || tpo.aclaracion_descripcion,
                              cta.imp_ap_pat, 
                              cta.imp_am_cre 
                         FROM cta_his_pagos cta,pag_ind_liquidacion ind,pag_tpo_aclaracion tpo
                        WHERE cta.id_derechohabiente = ",v_id_derechohabiente,"
                          AND cta.ind_liquidacion = ind.ind_liquidacion
                          AND cta.tpo_aclaracion = tpo.aclaracion_cod
                          AND cta.origen_archivo IN (1,4)
                          AND cta.ind_liquidacion = 1
                     ORDER BY cta.folio DESC, cta.f_pago DESC"

                  PREPARE prp_movimientos FROM v_qry
                  DECLARE cur_movimientos CURSOR FOR prp_movimientos

                  LET b=1
                  FOREACH cur_movimientos INTO res_out[b].*
                     LET b = b+1
                  END FOREACH
               ELSE
                  LET res_out[1].nss         = v_nss
                  LET res_out[1].clave       = (20)
                  LET res_out[1].descripcion ="NSS correcto Sin Movimientos Aclaratorios"
                  LET res_out[1].folioLQ        = ""
                  LET res_out[1].nrp            = ""
                  LET res_out[1].bimestrePago   = ""
                  LET res_out[1].folioSua       = ""
                  LET res_out[1].fechaPago      = ""
                  LET res_out[1].indLiquidacion = ""
                  LET res_out[1].tipoAclaracion = ""
                  LET res_out[1].aportacion     = ""
                  LET res_out[1].amortizacion   = ""
               END IF
            ELSE
               LET res_out[1].nss         = v_nss
               LET res_out[1].clave       = (11)
               LET res_out[1].descripcion ="NSS no existe en Base de Datos"
               LET res_out[1].folioLQ        = ""
               LET res_out[1].nrp            = ""
               LET res_out[1].bimestrePago   = ""
               LET res_out[1].folioSua       = ""
               LET res_out[1].fechaPago      = ""
               LET res_out[1].indLiquidacion = ""
               LET res_out[1].tipoAclaracion = ""
               LET res_out[1].aportacion     = ""
               LET res_out[1].amortizacion   = ""
            END IF
         END IF
      END IF 
END FUNCTION

FUNCTION fn_valida_numero(v_dato)

   DEFINE v_dato CHAR(1)

   LET bnd_dato = 0

   IF (v_dato MATCHES '[0-9]*') THEN
      LET bnd_dato = 1
   ELSE
      LET bnd_dato = 0
   END IF
END FUNCTION

FUNCTION fn_tmp()
   WHENEVER ERROR CONTINUE

      DROP TABLE tmp_marcas_operativas

   WHENEVER ERROR STOP
END FUNCTION