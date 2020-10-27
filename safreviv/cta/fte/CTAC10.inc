###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CUENTAS                                                 #
#Programa          => CTAC03                                                  #
#Objetivo          => CONSULTA DE DECRETO                                     #
#Fecha Inicio      => 02-JULIO-2012                                           #
###############################################################################

GLOBALS
   CONSTANT MAX_REGISTROS     SMALLINT = 50
   
   TYPE datos_generales RECORD
      id_decreto               DECIMAL(9,0),
      consec_cuenta            DECIMAL(11,0),
      nss                      CHAR(11),
      rfc                      CHAR(13),
      curp                     CHAR(18),
      nombre_trabajador          CHAR(120),
      cve_icefa                CHAR(3),
      nombre_patron            CHAR(120),
      folio             DECIMAL(9,0),
      f_movimiento                 DATE
      
   END RECORD

   TYPE datos_generales_cursor RECORD      
      folio                    DECIMAL(9,0),
      consec_cuenta            DECIMAL(11,0),
      origen_traspaso          CHAR(2),
      curp                     CHAR(18) ,
      nombre_trabajador          CHAR(120),
      nss_icefa                CHAR(11),
      rfc_icefa                CHAR(13),
      nss_afo_recep            CHAR(11),
      rfc_afo_recep            CHAR(13),
      nci_icefa                CHAR(30),
      sdo_viv92                DECIMAL(14,2),
      int_viv92                DECIMAL(14,2),
      result_operacion         CHAR(2),
      aivs_viv92               DECIMAL(16,6),
      f_movimiento             DATE ,
      id_decreto               DECIMAL(9,0),
      cve_icefa                CHAR(3),
      nombre_patron            CHAR(120)
        
   END RECORD

   TYPE movimientos_decreto RECORD
      id_decreto                 DECIMAL(9,0),
      f_movimiento                  DATE,
      folio                      DECIMAL(9,0),
    --  movimiento                 VARCHAR(100),
      origen_traspaso            VARCHAR(30),
      aivs_viv92                 DECIMAL(22,2),
      sdo_viv92                  DECIMAL(22,2)
     -- fondo_inversion            SMALLINT
   END RECORD
   
END GLOBALS