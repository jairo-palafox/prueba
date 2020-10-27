####################################################################
#Modulo            =>RET                                           #
#Programa          =>RETP02.inc                                    #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>18 JULIO 2013                                 #
####################################################################

GLOBALS

   CONSTANT RET_ESTADO_PAGADA                SMALLINT = 71
   CONSTANT SUBCTA_FONDO72                   SMALLINT = 40
   CONSTANT MOVTO_CARGO_FONDO72              SMALLINT = 182
   CONSTANT MOVTO_TANTO_ADICIONAL            SMALLINT = 422

   TYPE encabezado RECORD
      tpo_registro                           CHAR(2),
      num_secuencua                          CHAR(7),
      cod_operacion                          CHAR(2),
      banco                                  CHAR(3),
      sentido                                CHAR(1),
      servicio                               CHAR(1),
      num_bloque                             CHAR(7),
      f_presentacion                         CHAR(8),
      cod_divisa                             CHAR(2),
      causa_rechazo                          CHAR(2),
      modalidad                              CHAR(1),
      uso_futuro_cce                         CHAR(41),
      uso_futuro_banco                       CHAR(345)
   END RECORD

   TYPE detalle RECORD
      tpo_registro                           CHAR(2),
      num_secuencua                          CHAR(7),
      cod_operacion                          CHAR(2),
      divisa                                 CHAR(2),
      f_transferencia                        CHAR(8),
      banco_presentador                      CHAR(3),
      banco_receptor                         CHAR(3),
      importe_operacion                      CHAR(15),
      uso_futuro_cce                         CHAR(16),
      tpo_operacion                          CHAR(2),
      f_aplicacion                           CHAR(8),
      tpo_cuenta_ordenante                   CHAR(2),
      num_cuenta_ordenante                   CHAR(20),
      nombre_ordenante                       CHAR(40),
      rfc_ordenante                          CHAR(18),
      tpo_cuenta_receptor                    CHAR(2),
      num_cuenta_receptor                    CHAR(20),
      nombre_receptor                        CHAR(40),
      rfc_receptor                           CHAR(18),
      referencia_servicio                    CHAR(40),
      nombre_titular_servicio                CHAR(40),
      importe_iva                            CHAR(15),
      ref_numerica_ordenante                 CHAR(7),
      ref_leyenda_ordenante                  CHAR(40),
      clave_rastreo                          CHAR(30),
      movto_devolucion                       CHAR(2),
      f_presentacion_ini                     CHAR(8),
      solicitud_confirmacion                 CHAR(1),
      uso_futuro_banco                       CHAR(11)
   END RECORD

   TYPE sumario RECORD
      tpo_registro                           CHAR(2),
      num_secuencua                          CHAR(7),
      cod_operacion                          CHAR(2),
      num_bloque                             CHAR(7),
      num_operaciones                        CHAR(7),
      importe_total                          CHAR(18),
      uso_futuro_cce                         CHAR(40),
      uso_futuro_banco                       CHAR(339)
   END RECORD

END GLOBALS