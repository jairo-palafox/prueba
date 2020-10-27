####################################################################
#Proyecto          => SACI VIVIENDA                                #
#Propietario       => E.F.P.                                       #
#------------------------------------------------------------------#
#Modulo            => AFI                                          #
#Programa          => AFIW08                                       #
#Objetivo          => Archivo de definicion de datos               #
#Fecha Inicio      => 07 - Septiembre - 2017                       #
####################################################################

GLOBALS

   CONSTANT ENTIDAD_EMISORA            CHAR(5) = '04001'
   CONSTANT IP_CONSULTA                CHAR(7) = '0.0.0.0'
   CONSTANT TIPO_TRANSACCION           INTEGER = 1
   CONSTANT USUARIO                    CHAR(1) = ' '
   CONSTANT PASSWORD                   CHAR(1) = ' '
   CONSTANT URL                        STRING  = 'alias://consultaCurpRenapo'
   CONSTANT INTENTOS                   INTEGER = 5

   TYPE respuesta_renapo RECORD
      status_operacion           STRING,
      desc_status                STRING,
      curp                       CHAR(18),
      apellido_paterno           VARCHAR(50),
      apellido_materno           VARCHAR(50),
      nombre                     VARCHAR(50),
      sexo                       CHAR(1),
      fecha_nacimiento           CHAR(10),
      nacionalidad               CHAR(3),
      tipo_doc_probatorio        CHAR(1),
      anio_registro              CHAR(4),
      foja                       CHAR(5),
      tomo                       CHAR(3),
      libro                      CHAR(4),
      num_acta                   CHAR(5),
      crip                       CHAR(15),
      cve_entidad_reg            CHAR(2),
      cve_municipio_reg          CHAR(3),
      num_reg_extrangeros        CHAR(7),
      filio_carta                CHAR(5),
      cve_ent_nacimiento         CHAR(2),
      cve_ent_emisora            STRING,
      status_curp                CHAR(3)
   END RECORD

END GLOBALS