####################################################################
#Modulo            =>AFI                                           #
#Programa          =>AFIWS11                                       #
#Objetivo          =>WS de corrección de datos Maestros            #
#Fecha inicio      =>03 AGOSTO 2016                                #
#                                                                  #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT XML

DATABASE safre_viv

   DEFINE datos_in RECORD 
          nombre     CHAR(40),
          paterno    char(40),
          materno    CHAR(40),
          rfc        CHAR(13),
          curp       CHAR(18),
          nss        CHAR(11),
          arr_nss    DYNAMIC ARRAY OF RECORD
             nss_unificado CHAR(11)
          END RECORD
   END RECORD

   DEFINE res_out   RECORD
          clave       CHAR(3),
 {         arr_res DYNAMIC ARRAY OF RECORD
             bnd_rfc1    CHAR(1),
             bnd_rfc2    CHAR(1),  
             bnd_rfc3    CHAR(1),
             bnd_curp1   CHAR(1),
             bnd_curp2   CHAR(1),
             bnd_curp3   CHAR(1),
             bnd_curp4   CHAR(1),
             bnd_curp5   CHAR(1),
             bnd_nombre1 CHAR(1),
             bnd_paterno CHAR(1),
             bnd_materno CHAR(1)
          END RECORD
}
          arr_res     DYNAMIC ARRAY OF RECORD
             nss         CHAR (11),
             descripcion CHAR(40)
          END RECORD
   END RECORD

   DEFINE arr_general DYNAMIC ARRAY OF RECORD
      nss          CHAR(11)
   END RECORD

   DEFINE arr_ver_clave DYNAMIC ARRAY OF RECORD
      clave CHAR(3)
   END RECORD

   DEFINE a                   SMALLINT
   DEFINE z                   SMALLINT
   DEFINE bnd_dato            SMALLINT
   DEFINE v_dato              CHAR(1)
   DEFINE y                   SMALLINT
   DEFINE bnd_rfc1            SMALLINT
   DEFINE bnd_rfc2            SMALLINT
   DEFINE bnd_rfc3            SMALLINT
   DEFINE bnd_curp1           SMALLINT
   DEFINE bnd_curp2           SMALLINT
   DEFINE bnd_curp3           SMALLINT
   DEFINE bnd_curp4           SMALLINT
   DEFINE bnd_curp5           SMALLINT
   DEFINE bnd_nombre1         SMALLINT
   DEFINE bnd_paterno         SMALLINT
   DEFINE bnd_materno         SMALLINT
   DEFINE v_bnd_nom           SMALLINT
   
MAIN
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER

   CALL CreateCorreccionDatosMaestros() RETURNING servicio

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
# Service:  CorreccionDatosMaestros
# Port:     CorreccionDatosMaestros
#-------------------------------------------------------------------------------
#
# FUNCTION  CorreccionDatosMaestros
#   RETURNING soapstatus
#
FUNCTION CreateCorreccionDatosMaestros()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("CorreccionDatosMaestros","http://services.safre.efp.com")
    #
    # Operation: CorreccionDatosMaestros
    #
    # Publish Operation : CorreccionDatosMaestros
    LET operation = com.WebOperation.CreateDOCStyle("CorreccionDatosMaestros","CorreccionDatosMaestros",datos_in,res_out)
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

FUNCTION CorreccionDatosMaestros()
   DEFINE v_nss               CHAR(11)
   DEFINE v_cta               SMALLINT
   DEFINE bnd_nss             SMALLINT
   DEFINE bnd_infonavit       SMALLINT
   DEFINE v_cuenta            SMALLINT

   CALL arr_general.clear()

   LET arr_general[1].nss = datos_in.nss

   IF datos_in.arr_nss[1].nss_unificado IS NOT NULL THEN
      IF datos_in.arr_nss.getLength() >= 1 THEN
         FOR  z=1 TO datos_in.arr_nss.getLength()
            LET  arr_general[z+1].nss = datos_in.arr_nss[z].nss_unificado
         END FOR
      END IF
   END IF

   IF arr_general.getLength() >= 2 THEN
      CALL res_out.arr_res.clear()
      CALL arr_ver_clave.clear()

      FOR  z = 1 TO arr_general.getLength()
 
         LET v_nss = arr_general[z].nss
         IF LENGTH(v_nss) <> 11 THEN
            LET res_out.arr_res[z].nss         = v_nss
            LET res_out.arr_res[z].descripcion ="NSS erróneo o vacío"
            LET arr_ver_clave[z].clave         = 1
            CONTINUE FOR
         ELSE
            LET a = 1
            FOR a= 1 TO LENGTH(v_nss)
               LET v_dato = v_nss[a,a]
               CALL fn_valida_numero(v_dato)
               IF bnd_dato = 0 THEN
                  LET res_out.arr_res[z].nss         = v_nss
                  LET res_out.arr_res[z].descripcion ="NSS erróneo o vacío"
                  LET arr_ver_clave[z].clave         = 1
                  EXIT FOR
               END IF
            END FOR

            IF bnd_dato = 1 THEN
               SELECT COUNT(*)
                 INTO v_cta
                 FROM afi_derechohabiente
                WHERE nss = v_nss

               IF v_cta >= 1 THEN
                  IF v_nss[1,2] <> '77' THEN
                     LET res_out.arr_res[z].nss         = v_nss
                     LET res_out.arr_res[z].descripcion ="NSS IMSS"
                     LET arr_ver_clave[z].clave         = 1
                     CONTINUE FOR
                  ELSE
                     LET bnd_nss = 1
                  END IF
               ELSE
                  LET res_out.arr_res[z].nss         = v_nss
                  LET res_out.arr_res[z].descripcion ="NSS no existe"
                  LET arr_ver_clave[z].clave         = 1
                  CONTINUE FOR
               END IF
            END IF

            IF bnd_nss = 1 THEN
               LET datos_in.paterno = upshift(datos_in.paterno)
               LET datos_in.materno = upshift(datos_in.materno)
               LET datos_in.nombre  = upshift(datos_in.nombre)
               CALL fn_rfc(datos_in.rfc,v_nss)
               IF (bnd_rfc1 = 1)AND
                  (bnd_rfc2 = 1)AND
                  (bnd_rfc3 = 1) THEN
                  CALL fn_curp(datos_in.curp,v_nss)
                  IF (bnd_curp1 = 1)AND
                     (bnd_curp2 = 1)AND
                     (bnd_curp3 = 1)AND
                     (bnd_curp4 = 1)AND
                     (bnd_curp5 = 1)THEN
                     CALL fn_nombre(datos_in.paterno,datos_in.materno,datos_in.nombre,v_nss)
                     IF (bnd_materno = 1)AND
                        (bnd_paterno = 1)AND
                        (bnd_nombre1 = 1)THEN
                        CALL fn_actualiza(datos_in.paterno,datos_in.materno,datos_in.nombre,datos_in.rfc,datos_in.curp,v_nss)
                     ELSE
                        LET res_out.arr_res[z].nss            = v_nss
                        IF v_bnd_nom = 1 THEN 
                           LET res_out.arr_res[z].descripcion ="Nombre erroneo"
                        END IF
                        IF v_bnd_nom = 2 THEN 
                           LET res_out.arr_res[z].descripcion ="Apellido paterno erroneo"
                        END IF
                        IF v_bnd_nom = 3 THEN 
                           LET res_out.arr_res[z].descripcion ="Apellido materno erroneo"
                        END IF
                        IF v_bnd_nom = 4 THEN 
                           LET res_out.arr_res[z].descripcion ="Nombre erroneo y Apellido paterno erroneo"
                        END IF
                        LET arr_ver_clave[z].clave            = 1
                        CONTINUE FOR
                     END IF
                  ELSE
                     LET res_out.arr_res[z].nss         = v_nss
                     LET res_out.arr_res[z].descripcion ="CURP erronea"
                     LET arr_ver_clave[z].clave         = 1
                     CONTINUE FOR
                  END IF
               ELSE
                  LET res_out.arr_res[z].nss         = v_nss
                  LET res_out.arr_res[z].descripcion ="RFC erroneo"
                  LET arr_ver_clave[z].clave         = 1
                 CONTINUE FOR
               END IF
            END IF
         END IF
      END FOR
   ELSE
      LET res_out.arr_res[z].nss         = v_nss
      LET res_out.arr_res[z].descripcion ="Familia incompleta"
      LET arr_ver_clave[z].clave         = 1
   END IF

      LET v_cuenta = 0

      FOR z = 1 TO arr_ver_clave.getLength()
         IF arr_ver_clave[z].clave = 0 THEN
            LET v_cuenta = v_cuenta +1
         ELSE
            LET v_cuenta = v_cuenta
         END IF
      END FOR

      IF v_cuenta = 0 THEN
         LET res_out.clave = 02
      END IF

      IF v_cuenta = arr_ver_clave.getLength() THEN
         LET res_out.clave = 01
      END IF

      IF (v_cuenta > 0) AND
         (v_cuenta < arr_ver_clave.getLength()) THEN
         LET res_out.clave = 03
      END IF

      CALL datos_in.arr_nss.clear()
      CALL arr_general.clear()

END FUNCTION

FUNCTION fn_rfc(v_rfc,v_nss)

   DEFINE v_rfc               CHAR(13)
   DEFINE v_nss               CHAR(11)

   LET bnd_rfc1 = 0
   LET bnd_rfc2 = 0
   LET bnd_rfc3 = 0

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
   END IF

END FUNCTION

FUNCTION fn_curp(v_curp,v_nss)

   DEFINE v_curp              CHAR(18)
   DEFINE v_nss               CHAR(11)

   LET bnd_curp1 = 0
   LET bnd_curp2 = 0
   LET bnd_curp3 = 0
   LET bnd_curp4 = 0
   LET bnd_curp5 = 0

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
   END IF

END FUNCTION

FUNCTION fn_nombre(v_paterno,v_materno,v_nombre_af,v_nss)
   DEFINE a                   SMALLINT
   DEFINE v_nombre_af         CHAR(40)
   DEFINE v_paterno           CHAR(40)
   DEFINE v_materno           CHAR(40)
   DEFINE v_nss               CHAR(11)

   LET bnd_materno = 0
   LET bnd_paterno = 0
   LET bnd_nombre1 = 0
    
   LET v_nombre_af = upshift(v_nombre_af)
   LET v_paterno   = upshift(v_paterno)
   LET v_materno   = upshift(v_materno)

   LET v_bnd_nom = 0

   IF (v_nombre_af) IS NOT NULL AND
      (v_paterno) IS NOT NULL THEN

      LET a = 1
      FOR a= 1 TO LENGTH(v_nombre_af)
         LET v_dato = v_nombre_af[a,a]
         IF (a = 1) AND (v_dato = " ") THEN
            LET bnd_dato = 0
            LET v_bnd_nom = 1
            EXIT FOR
         END IF
         CALL fn_valida_letra(v_dato)
         IF bnd_dato = 0 THEN
            LET v_bnd_nom = 1
            EXIT FOR
         END IF
      END FOR
      LET bnd_nombre1 = bnd_dato

      LET a=1
      FOR a=1 TO LENGTH(v_paterno)
         LET v_dato = v_paterno[a,a]
         IF (a = 1) AND (v_dato = " ") THEN
            LET bnd_dato = 0
            LET v_bnd_nom = 2
            EXIT FOR
         END IF
         CALL fn_valida_letra(v_dato)
         IF bnd_dato= 0 THEN
            LET v_bnd_nom = 2
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
               LET v_bnd_nom = 3
               EXIT FOR
            END IF
            CALL fn_valida_letra(v_dato)
           IF bnd_dato = 0 THEN
              LET v_bnd_nom = 3
              EXIT FOR
           END IF
         END FOR
         LET bnd_materno = bnd_dato
      ELSE
         LET bnd_materno = 1
      END IF
   ELSE
     LET v_bnd_nom = 4
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

FUNCTION fn_actualiza(v_paterno,v_materno,v_nombre_af,v_rfc,v_curp,v_nss)

   DEFINE v_nombre_imss       STRING
   DEFINE v_nom_imss          CHAR(120)
   DEFINE v_nombre_af         CHAR(40)
   DEFINE v_paterno           CHAR(40)
   DEFINE v_materno           CHAR(40)
   DEFINE v_curp              CHAR(18)
   DEFINE v_rfc               CHAR(13)
   DEFINE v_nss               CHAR(11)

      IF (bnd_rfc1=1)    AND
         (bnd_rfc2=1)    AND
         (bnd_rfc3=1)    AND
         (bnd_curp1=1)   AND
         (bnd_curp2=1)   AND
         (bnd_curp3=1)   AND
         (bnd_curp4=1)   AND
         (bnd_curp5=1)   AND
         (bnd_nombre1=1) AND
         (bnd_paterno=1) AND
         (bnd_materno=1) THEN

         INSERT INTO afi_his_derechohabiente
              SELECT id_derechohabiente,
                     TODAY,
                     folio_lote,
                     "",--verificar que identificador de actualización corresponde
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
                nombre_imss   = v_nom_imss,
                curp = v_curp,
                rfc = v_rfc
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
         --LET res_out.clave = 15
{
         LET res_out.arr_res[z].bnd_curp1 = bnd_curp1
         LET res_out.arr_res[z].bnd_curp2 = bnd_curp2
         LET res_out.arr_res[z].bnd_curp3 = bnd_curp2
         LET res_out.arr_res[z].bnd_curp4 = bnd_curp4
         LET res_out.arr_res[z].bnd_curp5 = bnd_curp5
         LET res_out.arr_res[z].bnd_materno = bnd_materno
         LET res_out.arr_res[z].bnd_nombre1 = bnd_nombre1
         LET res_out.arr_res[z].bnd_paterno = bnd_paterno
         LET res_out.arr_res[z].bnd_rfc1 = bnd_rfc1
         LET res_out.arr_res[z].bnd_rfc1 = bnd_rfc1
         LET res_out.arr_res[z].bnd_rfc2 = bnd_rfc2
         LET res_out.arr_res[z].bnd_rfc3 = bnd_rfc3
}
         LET res_out.arr_res[z].nss         = v_nss
         LET res_out.arr_res[z].descripcion ="NSS ACTUALIZADO"
         LET arr_ver_clave[z].clave = 0
         CALL datos_in.arr_nss.clear()
      ELSE
         CALL datos_in.arr_nss.clear()
         --LET res_out.clave = 16
{
         LET res_out.arr_res[z].bnd_curp1 = bnd_curp1
         LET res_out.arr_res[z].bnd_curp2 = bnd_curp2
         LET res_out.arr_res[z].bnd_curp3 = bnd_curp2
         LET res_out.arr_res[z].bnd_curp4 = bnd_curp4
         LET res_out.arr_res[z].bnd_curp5 = bnd_curp5
         LET res_out.arr_res[z].bnd_materno = bnd_materno
         LET res_out.arr_res[z].bnd_nombre1 = bnd_nombre1
         LET res_out.arr_res[z].bnd_paterno = bnd_paterno
         LET res_out.arr_res[z].bnd_rfc1 = bnd_rfc1
         LET res_out.arr_res[z].bnd_rfc1 = bnd_rfc1
         LET res_out.arr_res[z].bnd_rfc2 = bnd_rfc2
         LET res_out.arr_res[z].bnd_rfc3 = bnd_rfc3
}

           {    LET bnd_infonavit = 0
               LET v_valida = v_nss[1,2]
               IF v_valida = 77 THEN
                  LET bnd_infonavit = 1
               ELSE
                  LET bnd_infonavit = 2
               END IF

               IF bnd_infonavit = 1 THEN
               END IF

               IF bnd_infonavit = 2 THEN
               END IF
               }
      END IF
END FUNCTION