####################################################################
#Proyecto          => SACI VIVIENDA                                #
#Propietario       => E.F.P.                                       #
#------------------------------------------------------------------#
#Modulo            => AFI                                          #
#Programa          => AFIM001                                      #
#Objetivo          => Archivo de definicion de datos               #
#Fecha Inicio      => 28 - Agosto - 2017                           #
####################################################################

GLOBALS

   TYPE datos_saci RECORD
      id_derechohabiente               DECIMAL(9,0),
      nss                              CHAR(11),
      rfc                              CHAR(13),
      curp                             CHAR(18),
      paterno                          VARCHAR(40),
      materno                          VARCHAR(40),
      nombre                           VARCHAR(40),
      nombre_imss                      VARCHAR(50),
      f_nacimiento                     DATE,
      genero                           VARCHAR(40),
      ind_fallecido                    CHAR(1)
   END RECORD

   TYPE datos_renapo RECORD
      curp                             CHAR(18),
      paterno                          VARCHAR(40),
      materno                          VARCHAR(40),
      nombre                           VARCHAR(40),
      sexo                             CHAR(1),
      f_nacimiento                     DATE,
      cve_entidad                      CHAR(2),
      nacionalidad                     CHAR(3),
      tpo_doc_probatorio               CHAR(1),
      estatus_curp                     CHAR(3)
   END RECORD
   
END GLOBALS