###############################################################################################
# Modulo       => AFI                                                                         #
# Programa     => AFIE06                                                                      #
# Objetivo     => Programa que realiza la lectura del archivo para la actualizacion           #
#                   de datos de RFC masiva                                                    #
# Autor        => Héctor Jiménez                                                              #
# Fecha        => 04/Junio/2015                                                               #
# actualización 21/03/2016  se agrega WS para homologar información con TRM                   #
# Autor mod.   => Emilio Abarca                                                               #
# Fecha mod.   => 26/Abril/2018                                                               #
# Objetivo     => Se genera folio para historia del registro.                                 #
###############################################################################################
DATABASE safre_viv

   DEFINE g_usuario              CHAR (20)
   DEFINE g_pid                  DECIMAL (9,0)  --ID del proceso
   DEFINE g_proceso_cod          SMALLINT       --código del proceso
   DEFINE g_opera_cod            SMALLINT       --código de operacion
   DEFINE v_nom_archivo          STRING 
   DEFINE v_ruta_rescate         CHAR(40)
   DEFINE v_cadena               STRING

   DEFINE bnd_ws                 SMALLINT
   DEFINE bnd_rfc                SMALLINT
   DEFINE bnd_act                CHAR(1)
   DEFINE v_bnd                  SMALLINT
   DEFINE v_s_comando            STRING
   DEFINE v_partner              CHAR(1)
   DEFINE g_folio                DECIMAL(10,0)

   DEFINE g_envio_ws             SMALLINT

MAIN
   DEFINE v_comando              STRING
   DEFINE v_estado_sql           SMALLINT
   DEFINE v_ruta_envio           CHAR(40)
   DEFINE v_ruta_listados        VARCHAR (40)
   DEFINE v_ruta_reporte         STRING
   DEFINE v_report_handler       om.SaxDocumentHandler -- handler para el reporte en PDF
   DEFINE nom_archivo            CHAR (40)
   DEFINE v_ruta_nom             STRING
   DEFINE v_nom_arch_sin_ext     base.StringBuffer
   DEFINE v_status               SMALLINT
   DEFINE v_cnt_aceptados        INTEGER
   DEFINE v_cnt_rechazados       INTEGER 
   DEFINE v_rfc_validar          CHAR (13)
   DEFINE v_nss_validar          CHAR (11)
   DEFINE v_bandera              SMALLINT
   DEFINE v_cnt_totales          INTEGER
   -- Record para la inserción en la historica
   DEFINE v_rec_inserta      RECORD
     id_dh                       DECIMAL(9,0),
     f_modifica                  DATE,
     folio_lote                  DECIMAL(9,0),
     ind_modifica                CHAR(18),
     curp                        CHAR(18),
     rfc                         CHAR(13),
     ind_nrp                     CHAR(1),
     f_nacimiento                DATE ,
     nombre_imss                 CHAR(50),
     nombre_af                   CHAR(40),
     ap_paterno_af               CHAR(40),
     ap_materno_af               CHAR(40)
   END RECORD

   LET g_usuario     = ARG_VAL (1)
   LET g_pid         = ARG_VAL (2)
   LET g_proceso_cod = ARG_VAL (3)
   LET g_opera_cod   = ARG_VAL (4)
   LET v_nom_archivo = ARG_VAL (5)
   LET g_envio_ws    = ARG_VAL(6)

   -- Se obtiene el nombre del archivo sin extensión 
   LET v_nom_arch_sin_ext = base.StringBuffer.create()

   CALL v_nom_arch_sin_ext.append(v_nom_archivo)
   CALL v_nom_arch_sin_ext.replace(".rfcact","",0)

   CALL STARTLOG(g_usuario CLIPPED||".AFIE06.log")

   -- se obtiene el folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod, g_usuario)
        RETURNING g_folio

   DISPLAY "FOLIO: ",g_folio
   
   LET v_comando = "SELECT ruta_rescate, ruta_envio
                      FROM seg_modulo
                     WHERE modulo_cod = 'afi' "

   PREPARE prp_modulo FROM v_comando
   EXECUTE prp_modulo INTO v_ruta_rescate,
                           v_ruta_envio

   IF (v_nom_archivo IS NULL) THEN
      DISPLAY "El archivo no existe"
   ELSE
      LET nom_archivo = v_nom_archivo CLIPPED

       #se ejecuta el proceso que crea la tabla temporal
      DATABASE safre_tmp  -- se cambia de DB por que se borran las tablas

      LET v_comando = " EXECUTE PROCEDURE sp_crea_modif_rfc()"
      PREPARE prp_afore_tmp FROM v_comando
      EXECUTE prp_afore_tmp

      -- Se regresa a la BD safre_vov
      DATABASE safre_viv

      LET v_ruta_nom = v_ruta_rescate CLIPPED ||"/"||v_nom_archivo

      DISPLAY "RUTA ARCHIVO : ",v_ruta_nom

      -- Se realiza el load a la tabla temporal
      LOAD FROM v_ruta_nom
      INSERT INTO safre_tmp:tmp_modificacion_rfc

      -- Se obtiene el total de registros en el archivo
      LET v_comando = "SELECT COUNT(*)
                         FROM safre_tmp:tmp_modificacion_rfc"

      PREPARE prp_totales FROM v_comando
      EXECUTE prp_totales INTO v_cnt_totales
      
      -- Se ejecuta la funcion que valida que exista el nss y que la cuenta este activa
      LET v_comando = "EXECUTE PROCEDURE sp_valida_modif_rfc()"
      PREPARE prp_valida_rfc FROM v_comando
      EXECUTE prp_valida_rfc INTO v_cnt_aceptados, v_cnt_rechazados


      DECLARE cur_tmp CURSOR FOR SELECT nss,rfc FROM safre_tmp:tmp_modificacion_rfc
      FOREACH cur_tmp INTO v_nss_validar, v_rfc_validar

         CALL fn_valida_rfc_alt(v_rfc_validar,v_nss_validar) RETURNING v_bandera

         -- si la bandera es cero la estructura es incorrecta
         IF v_bandera = 0 THEN
            -- rechazado continua el ciclo
            CONTINUE FOREACH
         ELSE
            -- Se obtienen los datos para la historica
            LET v_cadena = "SELECT id_derechohabiente,
                                   curp,
                                   rfc,
                                   nombre_af,
                                   ap_paterno_af,
                                   ap_materno_af,
                                   nombre_imss,
                                   f_nacimiento,
                                   folio_lote,
                                   ind_nrp
                              FROM afi_derechohabiente
                             WHERE nss = ? "

            PREPARE prp_cons_afi FROM v_cadena
            EXECUTE prp_cons_afi USING v_nss_validar INTO v_rec_inserta.id_dh,
                                                          v_rec_inserta.curp,
                                                          v_rec_inserta.rfc,
                                                          v_rec_inserta.nombre_af,
                                                          v_rec_inserta.ap_paterno_af,
                                                          v_rec_inserta.ap_materno_af,
                                                          v_rec_inserta.nombre_imss,
                                                          v_rec_inserta.f_nacimiento,
                                                          v_rec_inserta.folio_lote,
                                                          v_rec_inserta.ind_nrp

            LET v_rec_inserta.ind_modifica = 2
            LET v_rec_inserta.f_modifica   = TODAY

            -- Se realiza la inserciòn en la tabla historica
            LET v_cadena = "INSERT INTO afi_his_derechohabiente
                                 VALUES(?,?,?,?,?,?,?,?,?,?,?,?)"

            PREPARE prp_ins_afi_his FROM v_cadena
            EXECUTE prp_ins_afi_his USING  v_rec_inserta.id_dh         ,
                                           v_rec_inserta.f_modifica    ,
                                           g_folio                     ,
                                           v_rec_inserta.ind_modifica  ,
                                           v_rec_inserta.curp          ,
                                           v_rec_inserta.rfc           ,
                                           v_rec_inserta.ind_nrp       ,
                                           v_rec_inserta.f_nacimiento  ,
                                           v_rec_inserta.nombre_imss   ,
                                           v_rec_inserta.nombre_af     ,
                                           v_rec_inserta.ap_paterno_af ,
                                           v_rec_inserta.ap_materno_af

         
            -- Se realiza la actualización del RFC
            LET v_cadena = "UPDATE afi_derechohabiente
                               SET rfc = ?
                             WHERE nss = ?"
            PREPARE prp_upd_rfc FROM v_cadena
            EXECUTE prp_upd_rfc USING v_rfc_validar, v_nss_validar
         END IF

--******************************************************************************

      LET bnd_ws  = 0
      LET bnd_rfc = 0

      -- DISPLAY "v_nss_validar : ",v_nss_validar

      IF v_nss_validar IS NOT NULL THEN
         LET bnd_ws  = 1
         LET bnd_rfc = 1

         IF v_rfc_validar IS NULL THEN
            LET v_rfc_validar = " "
         END IF

         -- DISPLAY "se consume WS"
      END IF

      IF (bnd_ws = 1) AND (g_envio_ws = 1) THEN

         LET bnd_rfc   = 1
         LET bnd_act   = "R"
         LET v_partner = " "

         LET v_bnd = 1
         LET v_s_comando = " nohup time fglrun ",
                           "AFIW06"," ",
                           "'",v_bnd         CLIPPED,"'"," ",
                           "'",bnd_act       CLIPPED,"'"," ",
                           "'",v_partner     ,"'"," ",
                           "'",v_rfc_validar ,"'"," ",
                           "'"," "           ,"'"," ",
                           "'"," "           ,"'"," ",
                           "'"," "           ,"'"," ",
                           "'"," "           ,"'"," ",
                           "'",v_nss_validar ,"'"," "--, " ",
                           --" 1>",seg_modulo_bat.ruta_listados clipped,
                           --"/nohup:",g_pid USING "&&&&&",":",
                           --g_proceso_cod   USING "&&&&&",":",
                           --g_opera_cod     USING "&&&&&" ,
                           --" 2>&1 &"
         --DISPLAY v_s_comando
         RUN v_s_comando

         --CALL fn_mensaje("Atención","","information")
      END IF
--******************************************************************************
      END FOREACH

      DISPLAY "Registros en archivo  : ",v_cnt_totales
      DISPLAY "Archivo cargado  : ",v_nom_archivo

      -----------------------------------------------------
      -- Se genera el archivo de salida con los rechazos --
      -----------------------------------------------------
      UNLOAD TO v_ruta_envio CLIPPED ||"/"|| v_nom_arch_sin_ext.toString() CLIPPED ||".rerfcact"
      SELECT *
        FROM safre_tmp:tmp_modificacion_rechazo_rfc

      DISPLAY "Archivo Rechazos : " || v_ruta_envio CLIPPED ||"/"|| v_nom_arch_sin_ext.toString() CLIPPED ||".rerfcact"

      -------------------------------
      -- INICIA REPORTE            --
      -------------------------------
      LET v_cadena = " SELECT ruta_listados
                         FROM seg_modulo
                        WHERE modulo_cod = 'afi' "

      PREPARE prp_ruta_lst FROM v_cadena
      EXECUTE prp_ruta_lst INTO v_ruta_listados

      LET v_ruta_reporte = v_ruta_listados CLIPPED, "/",
                           g_usuario       CLIPPED, "-",        -- usuario
                           "AFIE06-"                   ,        -- programa
                           g_pid           USING "&&&&&","-",   -- PID
                           g_proceso_cod   USING "&&&&&", "-",  -- codigo del proceso
                           g_opera_cod     USING "&&&&&",".pdf" -- codigo de la operación

      DISPLAY "Ruta del reporte : ", v_ruta_reporte

      -- se indica que el reporte usara la plantilla creada
      IF ( fgl_report_loadCurrentSettings("../../afi/bin/AFIE06.4rp") ) THEN  -- if  the file loaded OK
         -- sin preview
         CALL fgl_report_selectPreview(0)
         -- se indica que se escriba en archivo
         CALL fgl_report_setOutputFileName(v_ruta_reporte)       

         LET v_report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings

         -- Se obtienen las cifras control
         SELECT COUNT(*)
           INTO v_cnt_aceptados
           FROM safre_tmp:tmp_modificacion_rfc

         SELECT COUNT(*)
         INTO v_cnt_rechazados
         FROM safre_tmp:tmp_modificacion_rechazo_rfc

         DISPLAY "\nTotal      : ",v_cnt_totales
         DISPLAY "Aceptados  : ",v_cnt_aceptados
         DISPLAY "Rechazados : ",v_cnt_rechazados

         -- se inicia el reporte
         START REPORT rpt_cifras_control TO XML HANDLER v_report_handler

         -- se envian los datos al reporte
         OUTPUT TO REPORT rpt_cifras_control(v_nom_archivo        ,
                                             g_usuario            ,
                                             v_cnt_aceptados      ,
                                             v_cnt_totales        ,
                                             v_cnt_rechazados
                                             )

         FINISH REPORT rpt_cifras_control
      ELSE
         DISPLAY "No se puede leer la plantilla del reporte AFIE06.4rp"
      END IF

      IF v_estado_sql < 0 THEN 
         DISPLAY "Ocurrió un error "
         EXIT PROGRAM 
      END IF 
   END IF

   LET nom_archivo = v_nom_archivo CLIPPED 

   -- Se inserta el archivo en glo_ctr_archivo
   INSERT INTO glo_ctr_archivo
        VALUES(g_proceso_cod, 1, nom_archivo, g_folio, 2, TODAY, g_usuario)

   -- Se actualiza la operación a finalizado
   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_status

END MAIN 

REPORT rpt_cifras_control(v_nom_archivo     ,
                          p_usuario_cod     ,
                          p_regs_aceptados  ,
                          p_regs_totales    ,
                          p_regs_rechazados )

   DEFINE p_usuario_cod          CHAR(20) -- Clave de usuario
   DEFINE v_nom_archivo          STRING 
   DEFINE v_fecha_texto          VARCHAR(10)
   DEFINE p_regs_aceptados       INTEGER   -- numero de movimientos aceptados
   DEFINE p_regs_totales         INTEGER   -- numero de movimientos rechazados
   DEFINE p_regs_rechazados      INTEGER 

   FORMAT

      FIRST PAGE HEADER

         LET v_fecha_texto = TODAY USING "dd-mm-yyyy"

         -- se despliegan los datos del encabezado
         PRINTX v_nom_archivo
         PRINTX p_usuario_cod
         PRINTX v_fecha_texto

         --Se imprimen las cifras control
         PRINTX p_regs_aceptados
         PRINTX p_regs_totales
         PRINTX p_regs_rechazados

END REPORT

-- Función que valida la estructura del RFC
FUNCTION fn_valida_rfc_alt(p_rfc,p_nss)
   DEFINE p_rfc             STRING
   DEFINE p_nss             CHAR(11)
   DEFINE v_cnt             SMALLINT
   DEFINE v_rfc_sbs         STRING
   DEFINE v_bandera         SMALLINT
   DEFINE v_rfc_char        CHAR(13)
   DEFINE v_cod_rechazo     CHAR(30)

   LET v_rfc_sbs  = p_rfc.trim()
   LET v_rfc_char = p_rfc
   LET v_cod_rechazo = "ESTRUCTURA INCORRECTA"

   -- El RFC puede ser nulo
   IF p_rfc IS NULL OR p_rfc = "" THEN
      LET v_bandera = 1
      RETURN v_bandera
   END IF

   -- Se valida que el RFC sea de 10 o 13 posiciones
   IF v_rfc_sbs.getLength() <> 10 AND v_rfc_sbs.getLength() <> 13 THEN
      -- se inserta en la tabla de rechazos
      INSERT INTO safre_tmp:tmp_modificacion_rechazo_rfc
           VALUES(p_nss,v_rfc_char,v_cod_rechazo)

      -- Se elimina de la tabla temporal para ya no procesar registros rechazados
      DELETE
        FROM safre_tmp:tmp_modificacion_rfc
       WHERE rfc = v_rfc_char
          OR nss = p_nss

      LET v_bandera = 0

      RETURN v_bandera
   ELSE
      -- Se enciende la bandera ne uno indicando que el RFC cumple con la condición de ser de 10 o 13 posiciones
      LET v_bandera = 1
   END IF

   IF v_bandera = 1 THEN
      -- se valida si son 10 o 13 posiciones
      IF v_rfc_sbs.getLength() = 10 THEN
         FOR v_cnt = 1 TO 4
            IF p_rfc.getCharAt(v_cnt) NOT MATCHES "[A-Z]" THEN
               INSERT INTO safre_tmp:tmp_modificacion_rechazo_rfc
                   VALUES(p_nss,v_rfc_char,v_cod_rechazo)

               DELETE
                 FROM safre_tmp:tmp_modificacion_rfc
                WHERE rfc = v_rfc_char
                   OR nss = p_nss

               -- bandera cero que hay error y no pasa a la segunda validacion
               LET v_bandera = 0

               EXIT FOR

               RETURN v_bandera
            ELSE
               LET v_bandera = 1
            END IF
         END FOR

         IF v_bandera = 1 THEN
            FOR v_cnt = 5 TO 10
               IF p_rfc.getCharAt(v_cnt) NOT MATCHES "[0-9]"  THEN
                  -- bandera cero que hay error y no pasa a la segunda validacion
                  INSERT INTO safre_tmp:tmp_modificacion_rechazo_rfc
                       VALUES(p_nss,v_rfc_char,v_cod_rechazo)

                  DELETE
                    FROM safre_tmp:tmp_modificacion_rfc
                   WHERE rfc = v_rfc_char
                      OR nss = p_nss

                  LET v_bandera = 0

                  EXIT FOR

                  RETURN v_bandera
               ELSE
                  LET v_bandera = 1
               END IF
            END FOR
         END IF
      ELSE
         FOR v_cnt = 1 TO 4
            IF p_rfc.getCharAt(v_cnt) NOT MATCHES "[A-Z]" THEN
               -- bandera cero que hay error y no pasa a la segunda validacion
               INSERT INTO safre_tmp:tmp_modificacion_rechazo_rfc
                    VALUES(p_nss,v_rfc_char,v_cod_rechazo)

               DELETE
                 FROM safre_tmp:tmp_modificacion_rfc
                WHERE rfc = v_rfc_char
                   OR nss = p_nss
               
               LET v_bandera = 0

               EXIT FOR

               RETURN v_bandera
            ELSE
               LET v_bandera = 1
            END IF
         END FOR

         IF v_bandera = 1 THEN
            FOR v_cnt = 5 TO 10
               IF p_rfc.getCharAt(v_cnt) NOT MATCHES "[0-9]"  THEN
                  -- bandera cero que hay error y no pasa a la segunda validacion
                  INSERT INTO safre_tmp:tmp_modificacion_rechazo_rfc
                       VALUES(p_nss,v_rfc_char,v_cod_rechazo)

                  DELETE
                    FROM safre_tmp:tmp_modificacion_rfc
                   WHERE rfc = v_rfc_char
                      OR nss = p_nss

                  LET v_bandera = 0

                  EXIT FOR

                  RETURN v_bandera
               ELSE
                  LET v_bandera = 1
               END IF
            END FOR
         END IF

         IF v_bandera = 1 THEN
            FOR v_cnt = 11 TO 13
                IF p_rfc.getCharAt(v_cnt) NOT MATCHES "[0-9]" AND
                   p_rfc.getCharAt(v_cnt) NOT MATCHES "[A-Z]"  THEN
                   -- bandera cero que hay error y no pasa a la segunda validacion
                   INSERT INTO safre_tmp:tmp_modificacion_rechazo_rfc
                        VALUES(p_nss,v_rfc_char,v_cod_rechazo)

                   DELETE
                     FROM safre_tmp:tmp_modificacion_rfc
                    WHERE rfc = v_rfc_char
                       OR nss = p_nss

                   LET v_bandera = 0

                   EXIT FOR

                   RETURN v_bandera
                ELSE
                   LET v_bandera = 1
                END IF
            END FOR
         END IF
      END IF
   END IF
   RETURN v_bandera
END FUNCTION

