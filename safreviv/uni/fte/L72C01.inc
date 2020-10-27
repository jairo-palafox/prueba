###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CUENTAS                                                 #
#Programa          => CTAC04                                                  #
#Objetivo          => CONSULTA DE FONDO 72                                    #
#Fecha Inicio      => 03-JULIO-2012                                           #
###############################################################################

GLOBALS
   CONSTANT MAX_REGISTROS     SMALLINT = 50
   
   TYPE datos_generales RECORD
      id_afi_fondo72           DECIMAL(9,0),
      nss                      CHAR(11),
      rfc                      CHAR(13),
      nombre_completo          VARCHAR(60)
   END RECORD

   TYPE movimientos_fondo RECORD
      id_afi_fondo72             DECIMAL(9,0),
      f_liquida                  DATE,
      folio_liquida              DECIMAL(9,0),
      movimiento                 VARCHAR(100),
      origen                     VARCHAR(30),
      monto_pesos                DECIMAL(22,2)
   END RECORD

DEFINE arr_datos_unificar DYNAMIC ARRAY OF RECORD
       v_uni_id_afi_fondo72  DECIMAL(9,0),
       v_uni_nss             CHAR(11),
       v_uni_rfc             CHAR(13),
       v_uni_nombre          CHAR(30),
       v_uni_fecha_liquida   DATE,
       v_uni_folio_liquida   DECIMAL(9,0),
       v_uni_movimiento      VARCHAR(100), 
       v_uni_origen          VARCHAR(30), 
       v_uni_monto_pesos     DECIMAL(22,2),
       v_uni_tipo_nss        SMALLINT
END RECORD

DEFINE arr_hist_detalles DYNAMIC ARRAY OF RECORD 
          v_nss          CHAR(11),
          v_rfc          CHAR(13),
          v_nombre       CHAR(40),
          v_folio        DECIMAL(9,0),
          v_ano          SMALLINT,
          v_clave_mov    CHAR(2),
          v_empresa      CHAR(40),
          v_bimestres    SMALLINT,
          v_importe      DECIMAL(10,2),
          v_saldo        DECIMAL(10,2),
          v_ind_verifica SMALLINT
END RECORD

DEFINE v_ind_datos   INTEGER,
       v_i_dm        INTEGER,
       v_i_dethist   INTEGER  

END GLOBALS