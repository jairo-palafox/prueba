####################################################################
#Modulo            =>AFI                                           #
#Programa          =>AFIWS01.4gl                                   #
#Objetivo          =>                                              #
#Fecha inicio      =>09 NOVIEMBRE 2015                             #
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
          clave       CHAR(3),
          descripcion CHAR(40),
          nss         CHAR (11),
          marca_cod   CHAR (3),
          marca_desc  CHAR (100)
   END RECORD

   DEFINE a SMALLINT
   DEFINE bnd_dato SMALLINT
   DEFINE v_dato CHAR(1)
   
MAIN
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER

   CALL CreateMarcasOperativasActivas() RETURNING servicio

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
# Service: MarcasOperativasActivasServices
# Port:    MarcasOperativasActivasServices
#-------------------------------------------------------------------------------
#
# FUNCTION MarcasOperativasActivas
#   RETURNING soapstatus
#
FUNCTION CreateMarcasOperativasActivas()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("MarcasOperativasActivas","http://services.safre.efp.com")

    # Publish Operation : Marcas Operativas Activas
    LET operation = com.WebOperation.CreateDOCStyle("marcasoperativasactivas","marcasoperativasactivas",nss_in,res_out)
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

FUNCTION marcasoperativasactivas()
   DEFINE v_nss               CHAR(11)
   DEFINE v_cta               SMALLINT
   DEFINE v_qry               STRING
   DEFINE b                   SMALLINT
   DEFINE v_cta_reg           INTEGER

   DEFINE arr_marcas DYNAMIC ARRAY OF RECORD
          nss CHAR(11),
          marca CHAR(3),
          descripcion CHAR (100)
   END RECORD

   LET v_nss = nss_in.nss
      IF LENGTH(v_nss) <> 11 THEN
         LET res_out[1].nss         = v_nss
         LET res_out[1].clave       = (10)
         LET res_out[1].descripcion ="NSS incorrecto"
         LET res_out[1].marca_cod   = ""
         LET res_out[1].marca_desc  =""
      ELSE
         LET a = 1
         FOR a= 1 TO LENGTH(v_nss)
            LET v_dato = v_nss[a,a]
            CALL fn_valida_numero(v_dato)
            IF bnd_dato = 0 THEN
               LET res_out[1].nss         = v_nss
               LET res_out[1].clave       = (10)
               LET res_out[1].descripcion ="NSS incorrecto"
               LET res_out[1].marca_cod   = ""
               LET res_out[1].marca_desc  =""
               EXIT FOR
            END IF
         END FOR
         IF bnd_dato = 1 THEN
            SELECT COUNT(*)
               INTO v_cta
               FROM afi_derechohabiente
               WHERE nss = v_nss

            IF v_cta >= 1 THEN
               CALL fn_tmp()
               LET v_qry = "EXECUTE PROCEDURE sp_marcas_operativas(?)"

               PREPARE prp_marcas FROM v_qry
               EXECUTE prp_marcas USING v_nss

               SELECT COUNT(*)
                 INTO v_cta_reg
                 FROM tmp_marcas_operativas

               IF v_cta_reg >= 1 THEN
               
                  LET v_qry = "SELECT *
                                 FROM tmp_marcas_operativas"

                  PREPARE prp_marca_activa FROM v_qry
                  DECLARE cur_marca_activa CURSOR FOR prp_marca_activa

                  LET b=1
                  FOREACH cur_marca_activa INTO res_out[b].*
                     LET b = b+1
                  END FOREACH
               ELSE
                  LET res_out[1].nss         = v_nss
                  LET res_out[1].clave       = (20)
                  LET res_out[1].descripcion ="NSS correcto"
                  LET res_out[1].marca_cod   = "000"
                  LET res_out[1].marca_desc  ="Cuenta sin Marcas"
               END IF

            ELSE
               LET res_out[1].nss         = v_nss
               LET res_out[1].clave       = (11)
               LET res_out[1].descripcion ="NSS no existe en Base de Datos"
               LET res_out[1].marca_cod   = ""
               LET res_out[1].marca_desc  =""
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