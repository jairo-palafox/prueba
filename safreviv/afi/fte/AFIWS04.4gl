####################################################################
#Modulo            =>AFI                                           #
#Programa          =>AFIWS01.4gl                                   #
#Objetivo          =>                                              #
#Fecha inicio      =>15 AGOSTO 2015                                #
#                                                                  #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT XML

DATABASE safre_viv

   DEFINE nss_in  RECORD
          nss     CHAR(11),
          tpo_act SMALLINT,
          rfc     CHAR(13),
          curp    CHAR(18),
          paterno char(40),
          materno CHAR(40),
          nombre  CHAR(40)
   END RECORD

   DEFINE res_out   RECORD
          nss         CHAR (11),
          clave       CHAR(3),
          descripcion CHAR(40)
   END RECORD

   DEFINE a SMALLINT
   DEFINE bnd_dato SMALLINT
   DEFINE v_dato CHAR(1)
   -- Definicion de variables para registro bitacora
   -- Folio008-2020 30-10-2020
   DEFINE g_identificador_servicio  SMALLINT
   DEFINE g_sesionID                CHAR(100)
MAIN
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER

   CALL CreateActDatosMaestros() RETURNING servicio

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
# Service: ActDatMaestrosServices
# Port:    ActDatMaestrosServices
#-------------------------------------------------------------------------------
#
# FUNCTION ActDatosMaestros
#   RETURNING soapstatus
#
FUNCTION CreateActDatosMaestros()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("ActDatosMaestros","http://services.safre.efp.com")


    #
    # Operation: Actualiza Datos Maestros
    #

    # Publish Operation : Actualiza Datos Maestros
    LET operation = com.WebOperation.CreateDOCStyle("actdatmaestros","actdatmaestros",nss_in,res_out)
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

FUNCTION actdatmaestros()
   DEFINE v_nss               CHAR(11)
   DEFINE v_cta               SMALLINT
   DEFINE bnd_nss             SMALLINT
   DEFINE bnd_infonavit       SMALLINT
   DEFINE v_valida            CHAR(2)
   DEFINE v_ruta_ejecutable   LIKE seg_modulo.ruta_bin
   DEFINE v_ruta_log          STRING
   DEFINE v_cadena            STRING
   -- se obtiene la ruta ejecutable
   -- Jairo Palafox 
   -- Folio008-2020 30-10-2020
   SELECT ruta_bin
   INTO   v_ruta_ejecutable
   FROM   seg_modulo
   WHERE  modulo_cod = "afi"

   -- se define la ruta del log
   LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/AFIWS04."
   LET v_cadena   = TODAY USING "yyyymmdd"
   LET v_ruta_log = v_ruta_log || v_cadena
   LET v_cadena   = CURRENT HOUR TO HOUR
   LET v_ruta_log = v_ruta_log || v_cadena
   LET v_cadena   = CURRENT MINUTE TO MINUTE
   LET v_ruta_log = v_ruta_log || v_cadena
   LET v_cadena   = CURRENT SECOND TO SECOND
   LET v_ruta_log = v_ruta_log || v_cadena || ".log"
   LET g_sesionID                 = v_ruta_log
   DISPLAY "Ruta del log creada del servidor: ", v_ruta_log

   -- se inicia el log del programa
   CALL STARTLOG(v_ruta_log)

   
   LET v_nss = nss_in.nss
      IF LENGTH(v_nss) <> 11 THEN
         LET res_out.nss         = v_nss
         LET res_out.clave       = (8)
         LET res_out.descripcion ="NSS erróneo o vacío"
      ELSE
         LET a = 1
         FOR a= 1 TO LENGTH(v_nss)
            LET v_dato = v_nss[a,a]
            CALL fn_valida_numero(v_dato)
            IF bnd_dato = 0 THEN
               EXIT FOR
               LET res_out.nss         = v_nss
               LET res_out.clave       = (8)
               LET res_out.descripcion ="NSS erróneo o vacío"
            END IF
         END FOR
         IF bnd_dato = 1 THEN
            SELECT COUNT(*)
               INTO v_cta
               FROM afi_derechohabiente
               WHERE nss = v_nss

            IF v_cta >= 1 THEN
               LET bnd_nss = 1
            END IF
         END IF

         IF bnd_nss = 1 THEN
            IF nss_in.tpo_act IS NULL THEN
               LET res_out.nss         =v_nss
               LET res_out.clave       ="27"
               LET res_out.descripcion ="Tipo de actualización no válido"
            ELSE

               LET bnd_infonavit = 0
               LET v_valida = v_nss[1,2]
               IF v_valida = 77 THEN
                  LET bnd_infonavit = 1
               ELSE
                  LET bnd_infonavit = 2
               END IF

               IF bnd_infonavit = 1 THEN
                  
                  CASE
                     WHEN (nss_in.tpo_act = 1)
                        AND (nss_in.rfc IS NOT NULL)
                        CALL fn_rfc(nss_in.rfc,nss_in.nss)
                        
                     WHEN (nss_in.tpo_act = 2)
                        AND (nss_in.curp IS NOT NULL)
                        CALL fn_curp(nss_in.curp,nss_in.nss)

                     WHEN (nss_in.tpo_act = 3)
                        AND (nss_in.nombre IS NOT NULL) AND (nss_in.paterno IS NOT NULL) --THEN
                        CALL fn_nombre(nss_in.paterno,nss_in.materno,nss_in.nombre,nss_in.nss)
                      
                     OTHERWISE
                        LET res_out.nss         =v_nss
                        LET res_out.clave       ="27"
                        LET res_out.descripcion ="Tipo de actualización no válido"
                  END CASE
                 
               END IF

               IF bnd_infonavit = 2 THEN
                  CASE
                     WHEN (nss_in.tpo_act = 1)
                        AND (nss_in.rfc IS NOT NULL)
                        CALL fn_rfc(nss_in.rfc,nss_in.nss)

                     WHEN (nss_in.tpo_act = 2)
                        AND (nss_in.curp IS NOT NULL)
                        CALL fn_curp(nss_in.curp,nss_in.nss)

                     --WHEN (nss_in.tpo_act = 3)
                      --  AND (nss_in.nombre IS NOT NULL) AND (nss_in.paterno IS NOT NULL) --THEN
                      -- CALL fn_nombre(nss_in.paterno,nss_in.materno,nss_in.nombre,nss_in.nss)

                     OTHERWISE
                        LET res_out.nss         =v_nss
                        LET res_out.clave       ="27"
                        LET res_out.descripcion ="Tipo de actualización no válido"
                  END CASE
               END IF


            END IF
         ELSE
            LET res_out.nss         =v_nss
            LET res_out.clave       ="01"
            LET res_out.descripcion ="NSS no existe"
         END IF
      END IF
      -- se invoca guardado a bitacora
      -- Cambio Jairo Palafox
      -- Folio008-2020 30-10-2020
      -- registro bitacora 
      CALL fn_registra_bitacora()

END FUNCTION

FUNCTION fn_rfc(v_rfc,v_nss)

   DEFINE v_rfc               CHAR(13)
   DEFINE v_nss               CHAR(11)
   DEFINE bnd_rfc1            SMALLINT
   DEFINE bnd_rfc2            SMALLINT
   DEFINE bnd_rfc3            SMALLINT

   LET v_rfc = upshift(v_rfc)
   IF length(v_rfc) = 13 THEN
      LET a = 1
      FOR a= 1 TO 4
         LET v_dato = v_rfc[a,a]
         IF (v_dato = " ") THEN
            LET bnd_dato = 0
            EXIT FOR
         END IF
         CALL fn_valida_letra(v_dato)
         IF bnd_dato = 0 THEN
            EXIT FOR
         END IF
      END FOR
      LET bnd_rfc1 = bnd_dato

      FOR a= 5 TO 10
         LET v_dato = v_rfc[a,a]
         CALL fn_valida_numero(v_dato)
         IF bnd_dato = 0 THEN
            EXIT FOR
         END IF
       END FOR
       LET bnd_rfc2 = bnd_dato

       --LET a = 11
      FOR a= 11 TO 13
         LET v_dato = v_rfc[a,a]
         IF (v_dato = " ") THEN
            LET bnd_dato = 0
            EXIT FOR
         END IF
         CALL fn_valida_letra(v_dato)
         IF bnd_dato = 0 THEN
            CALL fn_valida_numero(v_dato)
         END IF
         IF bnd_dato= 0 THEN
            EXIT FOR
         END IF
      END FOR
      LET bnd_rfc3 = bnd_dato

      IF (bnd_rfc1=1) AND (bnd_rfc2=1) AND (bnd_rfc3=1) THEN
         INSERT INTO afi_his_derechohabiente
              SELECT id_derechohabiente,
                     TODAY,
                     folio_lote,
                     2,
                     curp,
                     rfc,
                     ind_nrp,
                     f_nacimiento,
                     nombre_imss,
                     nombre_af,
                     ap_paterno_af,
                     ap_materno_af
                FROM afi_derechohabiente
               WHERE nss =v_nss

         UPDATE afi_derechohabiente
            SET rfc = v_rfc
          WHERE nss = v_nss

          INSERT INTO afi_his_modifica_ws
              SELECT id_derechohabiente,
                     CURRENT YEAR TO SECOND,
                     2,
                     curp,
                     rfc,
                     nombre_af,
                     ap_paterno_af,
                     ap_materno_af
                FROM afi_derechohabiente
               WHERE nss =v_nss
         
           LET res_out.nss         =v_nss
           LET res_out.clave       ="00"
           LET res_out.descripcion ="Actualización correcta"
      ELSE
         LET res_out.nss         =v_nss
         LET res_out.clave       ="23"
         LET res_out.descripcion ="RFC erróneo o vacío"
      END IF
   ELSE
      LET res_out.nss         =v_nss
      LET res_out.clave       ="23"
      LET res_out.descripcion ="RFC erróneo o vacío"
   END IF

END FUNCTION

FUNCTION fn_curp(v_curp,v_nss)
   DEFINE v_curp              CHAR(18)
   DEFINE v_nss               CHAR(11)
   DEFINE bnd_curp1           SMALLINT
   DEFINE bnd_curp2           SMALLINT
   DEFINE bnd_curp3           SMALLINT
   DEFINE bnd_curp4           SMALLINT
   DEFINE bnd_curp5           SMALLINT

   LET v_curp = upshift(v_curp)
   IF LENGTH(v_curp) = 18 THEN
      LET a = 1
      FOR a= 1 TO 4
         LET v_dato = v_curp[a,a]
         IF (v_dato = " ") THEN
            LET bnd_dato = 0
            EXIT FOR
         END IF
         CALL fn_valida_letra(v_dato)
         IF bnd_dato = 0 THEN
            EXIT FOR
         END IF
      END FOR
      LET bnd_curp1 = bnd_dato

      FOR a= 5 TO 10
         LET v_dato = v_curp[a,a]
         IF (v_dato = " ") THEN
            LET bnd_dato = 0
            EXIT FOR
         END IF
         CALL fn_valida_numero(v_dato)
         IF bnd_dato = 0 THEN
            EXIT FOR
         END IF
      END FOR
      LET bnd_curp2 = bnd_dato
 
      FOR a= 11 TO 16
         LET v_dato = v_curp[a,a]
         IF (v_dato = " ") THEN
            LET bnd_dato = 0
            EXIT FOR
         END IF
         CALL fn_valida_letra(v_dato)
         IF bnd_dato= 0 THEN
            EXIT FOR
         END IF
      END FOR
      LET bnd_curp3 = bnd_dato

      FOR a= 17 TO 17
         LET v_dato = v_curp[a,a]
         IF (v_dato = " ") THEN
            LET bnd_dato = 0
            EXIT FOR
         END IF
         CALL fn_valida_letra(v_dato)
         IF bnd_dato= 0 THEN
            CALL fn_valida_numero(v_dato)
         END IF
         IF bnd_dato = 0 THEN
            EXIT FOR
         END IF
      END FOR
      LET bnd_curp4 = bnd_dato

      FOR a= 18 TO 18
         LET v_dato = v_curp[a,a]
         CALL fn_valida_numero(v_dato)
         IF bnd_dato = 0 THEN
            EXIT FOR
         END IF
      END FOR
      LET bnd_curp5 = bnd_dato

      IF (bnd_curp1=1) AND
         (bnd_curp2=1) AND
         (bnd_curp3=1) AND
         (bnd_curp4=1) AND
         (bnd_curp5=1) THEN
         INSERT INTO afi_his_derechohabiente
              SELECT id_derechohabiente,
                     TODAY,
                     folio_lote,
                     1,
                     curp,
                     rfc,
                     ind_nrp,
                     f_nacimiento,
                     nombre_imss,
                     nombre_af,
                     ap_paterno_af,
                     ap_materno_af
                FROM afi_derechohabiente
               WHERE nss =v_nss

         UPDATE afi_derechohabiente
            SET curp = v_curp
          WHERE nss = v_nss

          INSERT INTO afi_his_modifica_ws
              SELECT id_derechohabiente,
                     CURRENT YEAR TO SECOND,
                     1,
                     curp,
                     rfc,
                     nombre_af,
                     ap_paterno_af,
                     ap_materno_af
                FROM afi_derechohabiente
               WHERE nss =v_nss

         LET res_out.nss         =v_nss
         LET res_out.clave       ="00"
         LET res_out.descripcion ="Actualización correcta"
      ELSE
         LET res_out.nss         =v_nss
         LET res_out.clave       ="24"
         LET res_out.descripcion ="CURP erróneo o vacío"
      END IF
   ELSE
      LET res_out.nss         =v_nss
      LET res_out.clave       ="24"
      LET res_out.descripcion ="CURP erróneo o vacío"
   END IF

END FUNCTION

FUNCTION fn_nombre(v_paterno,v_materno,v_nombre_af,v_nss)
   DEFINE v_nombre1           CHAR(20)
   DEFINE v_nombre2           CHAR(20)
   DEFINE v_nombre_af         CHAR(40)
   DEFINE v_nom_af            STRING
   DEFINE v_paterno           CHAR(40)
   DEFINE v_materno           CHAR(40)
   DEFINE a                   SMALLINT
   DEFINE bnd_nombre1         SMALLINT
   DEFINE bnd_nombre2         SMALLINT
   DEFINE bnd_paterno         SMALLINT
   DEFINE bnd_materno         SMALLINT
   DEFINE buf                 base.StringBuffer
   DEFINE v_esp               SMALLINT
   DEFINE v_nss               CHAR(11)
   DEFINE v_nombre_imss       STRING
   DEFINE v_nom_imss          CHAR(120)

   LET v_nombre_af = upshift(v_nombre_af)
   LET v_paterno   = upshift(v_paterno)
   LET v_materno   = upshift(v_materno)

   IF (v_nombre_af) IS NOT NULL AND
      (v_paterno) IS NOT NULL THEN
{
      LET v_nom_af = v_nombre_af
      LET buf = base.StringBuffer.create()
      CALL buf.append(v_nom_af)
      LET v_esp = buf.getIndexOf(" ",1)
      LET v_nombre1 = buf.subString(1,(v_esp-1))
      LET v_nombre2 = buf.subString((v_esp+1),(length(v_nom_af)))
      
      LET a = 1
      FOR a= 1 TO LENGTH(v_nombre1)
         LET v_dato = v_nombre1[a,a]
         CALL fn_valida_letra(v_dato)
         IF bnd_dato = 0 THEN
            EXIT FOR
         END IF
      END FOR
      LET bnd_nombre1 = bnd_dato

      LET a=1
      FOR a= 1 TO LENGTH(v_nombre2)
         LET v_dato = v_nombre2[a,a]
         CALL fn_valida_letra(v_dato)
         IF bnd_dato = 0 THEN
            EXIT FOR
         END IF
      END FOR
      LET bnd_nombre2 = bnd_dato
}
      LET a = 1
      FOR a= 1 TO LENGTH(v_nombre_af)
         LET v_dato = v_nombre_af[a,a]
         IF (a = 1) AND (v_dato = " ") THEN
            LET bnd_dato = 0
            EXIT FOR
         END IF
         CALL fn_valida_letra(v_dato)
         IF bnd_dato = 0 THEN
            EXIT FOR
         END IF
      END FOR
      LET bnd_nombre1 = bnd_dato


      LET a=1
      FOR a=1 TO LENGTH(v_paterno)
         LET v_dato = v_paterno[a,a]
         IF (a = 1) AND (v_dato = " ") THEN
            LET bnd_dato = 0
            EXIT FOR
         END IF
         CALL fn_valida_letra(v_dato)
         IF bnd_dato= 0 THEN
            EXIT FOR
         END IF
      END FOR
      LET bnd_paterno = bnd_dato

      IF v_materno IS NOT NULL THEN
         LET a=1
         FOR a= 1 TO LENGTH(v_materno)
            LET v_dato = v_materno[a,a]
            IF (a = 1) AND (v_dato = " ") THEN
               LET bnd_dato = 0
               EXIT FOR
            END IF
            CALL fn_valida_letra(v_dato)
           IF bnd_dato = 0 THEN
              EXIT FOR
           END IF
         END FOR
         LET bnd_materno = bnd_dato
      ELSE
         LET bnd_materno = 1
      END IF

      --IF (bnd_nombre1=1) AND
       IF(bnd_nombre1=1) AND
         (bnd_paterno=1) AND
         (bnd_materno=1) THEN

         --LET v_nom_af =v_nombre1 CLIPPED,' ',v_nombre2 CLIPPED
         --LET v_nombre_af = v_nom_af

         INSERT INTO afi_his_derechohabiente
              SELECT id_derechohabiente,
                     TODAY,
                     folio_lote,
                     6,
                     curp,
                     rfc,
                     ind_nrp,
                     f_nacimiento,
                     nombre_imss,
                     nombre_af,
                     ap_paterno_af,
                     ap_materno_af
                FROM afi_derechohabiente
               WHERE nss =v_nss

               LET v_nombre_af = v_nombre_af CLIPPED
               LET v_materno = v_materno CLIPPED
               LET v_paterno = v_paterno CLIPPED

         IF length (v_materno) >= 1 THEN 
            LET v_nombre_imss = v_paterno CLIPPED||'$'||v_materno CLIPPED||'$'||v_nombre_af CLIPPED
            LET v_nom_imss = v_nombre_imss
         ELSE
            LET v_nombre_imss = v_paterno CLIPPED||'$'||'$'||v_nombre_af CLIPPED
            LET v_nom_imss = v_nombre_imss
         END IF

         UPDATE afi_derechohabiente
            SET nombre_af     = v_nombre_af,
                ap_paterno_af = v_paterno,
                ap_materno_af = v_materno,
                nombre_imss   = v_nom_imss --v_paterno||'$'||v_materno||'$'||v_nombre_af
          WHERE nss = v_nss

          
          INSERT INTO afi_his_modifica_ws
              SELECT id_derechohabiente,
                     CURRENT YEAR TO SECOND,
                     6,
                     curp,
                     rfc,
                     nombre_af,
                     ap_paterno_af,
                     ap_materno_af
                FROM afi_derechohabiente
               WHERE nss =v_nss
     
         LET res_out.nss         =v_nss
         LET res_out.clave       ="00"
         LET res_out.descripcion ="Actualización correcta"
      ELSE
         LET res_out.nss         =v_nss
         LET res_out.clave       ="07"
         LET res_out.descripcion ="NOMBRE erróneo o vacío"
      END IF
   ELSE
      LET res_out.nss         =v_nss
      LET res_out.clave       ="07"
      LET res_out.descripcion ="NOMBRE erróneo o vacío"
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

FUNCTION fn_valida_letra(v_dato)

   DEFINE v_dato CHAR(1)

   LET bnd_dato = 0

   IF (v_dato = "Ñ") OR
      --(v_dato = "#") OR
      (v_dato = " ") THEN
      LET bnd_dato = 1
   ELSE 
      IF (v_dato MATCHES '[A-Z]*') THEN
         LET bnd_dato = 1
      ELSE
         LET bnd_dato = 0
      END IF
   END IF
END FUNCTION

################################################################################
#- funcion para registrar los eventos en la bitacora                           #
# Autor - Jairo Giovanny Palafox Sanchez                                       #
# Empresa -Omnisys                                                             #
# Fecha Creacion : 30-10-2020                                                  #
################################################################################
            
FUNCTION fn_registra_bitacora()
 DEFINE v_resultado         SMALLINT
 DEFINE v_eventoID          CHAR(51)
 
  -- inicio de variables
  SELECT id_ws_ctr_maestra
   INTO g_identificador_servicio 
   FROM ws_ctr_maestra
   WHERE id_ws_ctr_maestra  =  5

  DISPLAY "RESULTADO RESPUESTA: ", res_out.clave 
  -- verificar si se realiza actualizacion de lo contrario se indica el motivo
  IF  res_out.clave = "00" THEN
     -- se realiza registro
     CASE (nss_in.tpo_act)
      WHEN 1
         -- titulo
         LET v_eventoID =  nss_in.rfc CLIPPED, 
                           nss_in.nss CLIPPED
      WHEN 2
         -- titulo
         LET v_eventoID = nss_in.curp CLIPPED, 
                          nss_in.nss  CLIPPED
     
      WHEN 3
         -- titulo
         LET v_eventoID = nss_in.paterno CLIPPED,
                          nss_in.materno CLIPPED,
                          nss_in.nombre  CLIPPED,
                          nss_in.nss     CLIPPED
      OTHERWISE
     END CASE
  ELSE
     -- titulo
     LET v_eventoID = " NO ACTUALIZA DATOS "
  END IF
 
  -- se ejecuta funcion global para el registro de la bitacora por evento
  CALL fn_registra_bitacora_ws(g_identificador_servicio,g_sesionID,v_eventoID) RETURNING v_resultado
  DISPLAY "Resultado registro Bitacora: >>", v_resultado  
END FUNCTION