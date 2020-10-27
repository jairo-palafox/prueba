####################################################################
#Modulo            =>SRV                                           #
#Programa          =>SRVP02.inc                                    #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>15 MARZO 2012                                 #
####################################################################
DATABASE safre_viv

GLOBALS
   TYPE datos_edo_cuenta RECORD
      #Datos del cliente
      nombre_completo            VARCHAR(80),
      nss                        LIKE afi_derechohabiente.nss,
      curp                       LIKE afi_derechohabiente.curp,
      tipo_derechohabiente       VARCHAR(15),
      origen_afiliacion          VARCHAR(15),
      indicador_credito          VARCHAR(40),
      
      #Direccion
      calle                      VARCHAR(50),
      numero                     VARCHAR(50),
      colonia                    LIKE cat_colonia.colonia_desc,
      municipio                  LIKE cat_municipio.municipio_desc,
      estado                     LIKE cat_entidad_federativa.entidad_desc_larga,
      codigo_postal              LIKE cat_colonia.cp,

      #manejo de las fechas
      fecha_corte                DATE,
      periodo_inicio             DATE,
      periodo_fin                DATE,

      #Sumario de saldos para fondo de ahorro 72-92
      desc_72                    STRING,
      anterior_72                LIKE cta_movimiento.monto_pesos,
      cargo_72                   LIKE cta_movimiento.monto_pesos,
      abono_72                   LIKE cta_movimiento.monto_pesos,
      final_72                   LIKE cta_movimiento.monto_pesos,

      #Sumario de saldos para Vivienda 92-97
      desc_92                    STRING,
      anterior_92                LIKE cta_movimiento.monto_pesos,
      cargo_92                   LIKE cta_movimiento.monto_pesos,
      abono_92                   LIKE cta_movimiento.monto_pesos,
      final_92                   LIKE cta_movimiento.monto_pesos,

      #Sumario de saldos para Vivienda 97
      desc_97                    STRING,
      anterior_97                LIKE cta_movimiento.monto_pesos,
      cargo_97                   LIKE cta_movimiento.monto_pesos,
      abono_97                   LIKE cta_movimiento.monto_pesos,
      final_97                   LIKE cta_movimiento.monto_pesos,

      #Sumario de saldos para Amortizacion
      desc_amortizacion          STRING,
      anterior_amortizacion      LIKE cta_movimiento.monto_pesos,
      cargo_amortizacion         LIKE cta_movimiento.monto_pesos,
      abono_amortizacion         LIKE cta_movimiento.monto_pesos,
      final_amortizacion         LIKE cta_movimiento.monto_pesos,

      #Sumario de saldos para Vivienda 92-97 solo infonavit
      desc_92_infonavit          STRING,
      anterior_92_infonavit      LIKE cta_movimiento.monto_pesos,
      cargo_92_infonavit         LIKE cta_movimiento.monto_pesos,
      abono_92_infonavit         LIKE cta_movimiento.monto_pesos,
      final_92_infonavit         LIKE cta_movimiento.monto_pesos,

      #Sumario de saldos para Vivienda 97 solo infonavit
      desc_97_infonavit          STRING,
      anterior_97_infonavit      LIKE cta_movimiento.monto_pesos,
      cargo_97_infonavit         LIKE cta_movimiento.monto_pesos,
      abono_97_infonavit         LIKE cta_movimiento.monto_pesos,
      final_97_infonavit         LIKE cta_movimiento.monto_pesos,

      #Sumario de saldos para Amortizacion solo infonavit
      desc_amortizacion_infonavit          STRING,
      anterior_amortizacion_infonavit      LIKE cta_movimiento.monto_pesos,
      cargo_amortizacion_infonavit         LIKE cta_movimiento.monto_pesos,
      abono_amortizacion_infonavit         LIKE cta_movimiento.monto_pesos,
      final_amortizacion_infonavit         LIKE cta_movimiento.monto_pesos,

      saldo_total                LIKE cta_movimiento.monto_pesos,

      #Datos generales de la hoja de detalle
      hoja_detalle               BOOLEAN,

      #lista de movimientos
      movimientos DYNAMIC ARRAY OF RECORD
         subcuenta               LIKE cta_movimiento.subcuenta,
         fecha                   LIKE cta_movimiento.f_liquida,
         nrp                     varchar(80),
         tipo_movimiento         LIKE cat_movimiento.movimiento_desc,
         monto_pesos             LIKE cta_movimiento.monto_pesos
      END RECORD
   END RECORD 
END GLOBALS