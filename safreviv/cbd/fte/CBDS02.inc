####################################################################
#Modulo            =>CBD                                           #
#Programa          =>CBDS02.inc                                    #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>20 JUNIO 2012                                 #
####################################################################

GLOBALS

   CONSTANT VIVIENDA_97                      SMALLINT = 4
   CONSTANT VIVIENDA_92                      SMALLINT = 8
   CONSTANT FONDO_72                         SMALLINT = 40
   CONSTANT SOLO_INFONAVIT_97                SMALLINT = 44
   CONSTANT SOLO_INFONAVIT_92                SMALLINT = 42
   CONSTANT VIVIENDA_EFIRISS                 SMALLINT = 55

   CONSTANT ARCHIVO_IMSS                     SMALLINT = 1
   CONSTANT ARCHIVO_INF                      SMALLINT = 2

   TYPE detalle RECORD
      nss                                    CHAR(11),
      rfc                                    CHAR(13),
      curp                                   CHAR(18),
      ape_paterno                            CHAR(40),
      ape_materno                            CHAR(40),
      nombre                                 CHAR(40),
      nombre_imss                            CHAR(50),
      tipo_trabajador                        CHAR(1),
      cve_afore                              CHAR(3),
      --aport_viv97                            CHAR(11),
      ------------------------------------------------------------------------
      aport_viv97                            CHAR(13),
      ------------------------------------------------------------------------
      num_aportaciones97                     CHAR(5),
      f_ssv97                                CHAR(6),
     -- aport_viv92                            CHAR(9),
      ------------------------------------------------------------------------
      aport_viv92                            CHAR(11),
      ------------------------------------------------------------------------
      num_aportaciones92                     CHAR(5),
      f_ssv92                                CHAR(6),
      fondo_viv72                            CHAR(7),
      f_fondo_anterior                       CHAR(6),
      --aivs_viv97                             CHAR(12),
      --aivs_viv92                             CHAR(13),
      --------------------------------------------------------------------
      aivs_viv97                             CHAR(15),
      aivs_viv92                             CHAR(15),
      --------------------------------------------------------------------
      valor_paivs                            CHAR(12),
      f_paivs                                CHAR(6),
      marca_modif_nombre                     CHAR(1),
      estatus_unificacion                    CHAR(1),
      marca_unificacion                      CHAR(1),
      estatus_retiro                         CHAR(1),
      marca_retiro                           CHAR(1),
      estatus_traspaso_afore                 CHAR(1),
      estatus_devolucion_pagos               CHAR(1),
      estatus_trans_acreditados              CHAR(1),
      marca_trans_43bis                      CHAR(1),
      estatus_trabajador_credito             CHAR(1),
      solicitud_credito                      CHAR(1),
      f_credito                              CHAR(6),
      f_aivs_retiro                          CHAR(6),
      f_ultimo_movimiento                    CHAR(6),
      diferencia_nombre                      CHAR(1),
      estatus_traspaso_sar92                 CHAR(1),
      estatus_acreditados_43pr               CHAR(1),
      estatus_acreditados_43gt               CHAR(1),
      estatus_acreditados_43su               CHAR(1),
      indica_negativo_92                     CHAR(1),
      indica_negativo_97                     CHAR(1)
   END RECORD

END GLOBALS