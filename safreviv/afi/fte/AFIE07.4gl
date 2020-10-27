###############################################################################################
# Modulo       => AFI                                                                         #
# Programa     => AFIE07                                                                      #
# Objetivo     => Programa que realiza la lectura del archivo para la actualizacion           #
#                   de datos de CURP masiva                                                   #
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
   DEFINE bnd_curp               SMALLINT
   DEFINE bnd_act                CHAR(1)
   DEFINE v_bnd                  SMALLINT
   DEFINE v_s_comando            STRING
   DEFINE v_partner              CHAR(1)
   DEFINE g_folio                DECIMAL(10,0)

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
   DEFINE v_curp_validar         CHAR (18)
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
     rfc                         CHAR(11),
     ind_nrp                     CHAR(1),
     f_nacimiento                DATE ,
     nombre_imss                 CHAR(50),
     nombre_af                   CHAR(40),
     ap_paterno_af               CHAR(40),
     ap_materno_af               CHAR(40)
   END RECORD

   LET g_usuario        = ARG_VAL (1)
   LET g_pid            = ARG_VAL (2)
   LET g_proceso_cod    = ARG_VAL (3)
   LET g_opera_cod      = ARG_VAL (4)
   LET v_nom_archivo    = ARG_VAL (5)

   LET v_curp_validar = ""

   -- Se obtiene el nombre del archivo sin extensión 
   LET v_nom_arch_sin_ext = base.StringBuffer.create()
   CALL v_nom_arch_sin_ext.append(v_nom_archivo)--bu.append(v_nom_archivo)
   CALL v_nom_arch_sin_ext.replace(".curpact","",0)

   CALL STARTLOG(g_usuario CLIPPED||".AFIE07.log")

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

      LET v_comando = " EXECUTE PROCEDURE sp_crea_modif_curp()"
      PREPARE prp_afore_tmp FROM v_comando
      EXECUTE prp_afore_tmp

      -- Se regresa a la BD safre_vov
      DATABASE safre_viv

      LET v_ruta_nom = v_ruta_rescate CLIPPED ||"/"||v_nom_archivo

      DISPLAY "RUTA ARCHIVO : ",v_ruta_nom

      -- Se realiza el load a la tabla temporal
      LOAD FROM v_ruta_nom
      INSERT INTO safre_tmp:tmp_modificacion_curp

      -- Se obtiene el total de registros en el archivo
      LET v_comando = "SELECT COUNT(*)
                         FROM safre_tmp:tmp_modificacion_curp"

      PREPARE prp_totales FROM v_comando
      EXECUTE prp_totales INTO v_cnt_totales
      
      -- Se ejecuta la funcion que valida que exista el nss y que la cuenta este activa
      LET v_comando = "EXECUTE PROCEDURE sp_valida_modif_curp()"
      PREPARE prp_valida_curp FROM v_comando
      EXECUTE prp_valida_curp INTO v_cnt_aceptados, v_cnt_rechazados


      DECLARE cur_tmp CURSOR FOR SELECT nss,curp FROM safre_tmp:tmp_modificacion_curp
      FOREACH cur_tmp INTO v_nss_validar, v_curp_validar

         CALL fn_valida_curp_alt(v_curp_validar,v_nss_validar) RETURNING v_bandera

         -- si la bandera es cero la estructura es incorrecta
         IF v_bandera = 0 THEN
            -- rechazado, continua el ciclo
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

            LET v_rec_inserta.ind_modifica = 1
            LET v_rec_inserta.f_modifica   = TODAY

            -- Se realiza la inserciòn en la tabla historica
            LET v_cadena = "INSERT INTO afi_his_derechohabiente
                                 VALUES(?,?,?,?,?,?,?,?,?,?,?,?)"

            PREPARE prp_ins_afi_his FROM v_cadena
            EXECUTE prp_ins_afi_his USING  v_rec_inserta.id_dh        ,
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

            -- Se realiza la actualización del CURP
            LET v_cadena = "UPDATE afi_derechohabiente
                               SET curp = ?
                             WHERE nss  = ?"
            PREPARE prp_upd_curp FROM v_cadena
            EXECUTE prp_upd_curp USING v_curp_validar, v_nss_validar
         END IF
--******************************************************************************

      LET bnd_ws  = 0
      LET bnd_curp = 0

    --  DISPLAY "v_nss_validar : ",v_nss_validar

      IF v_nss_validar IS NOT NULL THEN
         LET bnd_ws  = 1
         LET bnd_curp = 1
      END IF

      IF bnd_ws = 1 THEN

         LET bnd_curp = 1
         LET bnd_act = "C"
         LET v_partner = " "

         LET v_bnd = 1
         LET v_s_comando = " nohup time fglrun ",
                           "AFIW06"," ",
                           "'",v_bnd          CLIPPED,"'"," ",
                           "'",bnd_act        CLIPPED,"'"," ",
                           "'",v_partner      ,"'"," ",
                           "'"," "            ,"'"," ",
                           "'",v_curp_validar CLIPPED,"'"," ",
                           "'"," "            ,"'"," ",
                           "'"," "            ,"'"," ",
                           "'"," "            ,"'"," ",
                           "'",v_nss_validar  CLIPPED,"'"," "--, " ",
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
      DISPLAY "Archivo cargado       : ",v_nom_archivo

      -----------------------------------------------------
      -- Se genera el archivo de salida con los rechazos --
      -----------------------------------------------------
      UNLOAD TO v_ruta_envio CLIPPED ||"/"|| v_nom_arch_sin_ext.toString() CLIPPED ||".rcurpact"
      SELECT *
        FROM safre_tmp:tmp_modificacion_rechazo_curp

      DISPLAY "Archivo Rechazos : " || v_ruta_envio CLIPPED ||"/"|| v_nom_arch_sin_ext.toString() CLIPPED ||".rcurpact"

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
                           "AFIE07-"                   ,        -- programa
                           g_pid           USING "&&&&&","-",   -- PID
                           g_proceso_cod   USING "&&&&&", "-",  -- codigo del proceso
                           g_opera_cod     USING "&&&&&",".pdf" -- codigo de la operación

      DISPLAY "Ruta del reporte: ", v_ruta_reporte

      -- se indica que el reporte usara la plantilla creada
      IF ( fgl_report_loadCurrentSettings("../../afi/bin/AFIE07.4rp") ) THEN  -- if  the file loaded OK
         -- sin preview
         CALL fgl_report_selectPreview(0)
         -- se indica que se escriba en archivo
         CALL fgl_report_setOutputFileName(v_ruta_reporte)       

         LET v_report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings

         -- Se obtienen las cifras control
         SELECT COUNT(*)
           INTO v_cnt_aceptados
           FROM safre_tmp:tmp_modificacion_curp

         SELECT COUNT(*)
         INTO v_cnt_rechazados
         FROM safre_tmp:tmp_modificacion_rechazo_curp

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
         DISPLAY "No se puede leer la plantilla del reporte AFIE07.4rp"
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
   DEFINE p_regs_aceptados       INTEGER  -- numero de movimientos aceptados
   DEFINE p_regs_totales         INTEGER  -- numero de movimientos rechazados
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

-- Función que valida el CURP
FUNCTION fn_valida_curp_alt(p_curp,p_nss)
   DEFINE p_curp               STRING
   DEFINE p_nss                CHAR(18)
   DEFINE v_cnt                SMALLINT
   DEFINE v_curp_sbs           STRING
   DEFINE v_bandera            SMALLINT
   DEFINE v_err_msj            STRING
   DEFINE v_curp_char          CHAR(18)
   DEFINE v_cod_rechazo        CHAR(40)
   DEFINE v_ax_curp            CHAR(18)

   LET v_curp_sbs    = p_curp.trim()
   LET v_curp_char   = p_curp
   LET v_cod_rechazo = "ESTRUCTURA INCORRECTA"

   IF v_curp_sbs.getLength() <> 18 THEN
      IF p_curp IS NULL OR p_curp = "" THEN

         -- Se comenta debido a que se solicito que ya no se validen que sean 18 ceros
         {LET v_cadena = " SELECT curp
                            FROM afi_derechohabiente
                           WHERE nss = ? "
                           --WHERE nss = ",p_nss

         PREPARE prp_curp_nul FROM v_cadena
         EXECUTE prp_curp_nul USING p_nss INTO v_ax_curp

         IF v_ax_curp = "000000000000000000" THEN}
            LET v_bandera = 1
         {ELSE
            LET v_cod_rechazo = "CURP ACTUAL NO CONTIENE 18 CEROS"
            INSERT INTO safre_tmp:tmp_modificacion_rechazo_curp
                      VALUES(p_nss,v_curp_char,v_cod_rechazo)

            DELETE
              FROM safre_tmp:tmp_modificacion_curp
             WHERE curp = v_curp_char
                OR nss  = p_nss

            LET v_bandera = 0
         END IF}
      ELSE
         INSERT INTO safre_tmp:tmp_modificacion_rechazo_curp
                      VALUES(p_nss,v_curp_char,v_cod_rechazo)

         DELETE
           FROM safre_tmp:tmp_modificacion_curp
          WHERE curp = v_curp_char
             OR nss  = p_nss

         LET v_bandera = 0
      END IF
   ELSE
      -- inicia primera validacion de la posición 1 a 4
      FOR v_cnt = 1 TO 4
         IF p_curp.getCharAt(v_cnt) NOT MATCHES "[A-Z]" THEN
            INSERT INTO safre_tmp:tmp_modificacion_rechazo_curp
                   VALUES(p_nss,v_curp_char,v_cod_rechazo)

               DELETE
                 FROM safre_tmp:tmp_modificacion_curp
                WHERE curp = v_curp_char
                   OR nss  = p_nss

            -- bandera cero que hay error y no pasa a la segunda validación
            LET v_bandera = 0
            EXIT FOR
         ELSE
            LET v_bandera = 1
         END IF
      END FOR

      -- si no hay error con la primera validación inicia la segunda de la posicion 5 al 10
      IF v_bandera = 1 THEN
         FOR v_cnt = 5 TO 10
            IF p_curp.getCharAt(v_cnt) NOT MATCHES "[0-9]"  THEN
               INSERT INTO safre_tmp:tmp_modificacion_rechazo_curp
                   VALUES(p_nss,v_curp_char,v_cod_rechazo)

               DELETE
                 FROM safre_tmp:tmp_modificacion_curp
                WHERE curp = v_curp_char
                   OR nss  = p_nss


               -- bandera cero que hay error y no pasa a la tercera validacion
               LET v_bandera = 0
               EXIT FOR
            ELSE
               LET v_bandera = 1
            END IF
         END FOR
      END IF

      -- si no hay error en la sgeunda validaciín incia validación de la posición 11 a 16
      IF v_bandera = 1 THEN
         FOR v_cnt = 11 TO 16
            IF p_curp.getCharAt(v_cnt) NOT MATCHES "[A-Z]" THEN
               INSERT INTO safre_tmp:tmp_modificacion_rechazo_curp
                   VALUES(p_nss,v_curp_char,v_cod_rechazo)

               DELETE
                 FROM safre_tmp:tmp_modificacion_curp
                WHERE curp = v_curp_char
                   OR nss  = p_nss

               -- bandera cero que hay error y no pasa a la segunda validación
               LET v_bandera = 0
               EXIT FOR
            ELSE
               LET v_bandera = 1
            END IF
         END FOR
      END IF

      -- si no hay error en la tercera validación incia validación de la posición 17
      IF v_bandera = 1 THEN
         IF p_curp.getCharAt(17) NOT MATCHES "[A-Z]" AND p_curp.getCharAt(17) NOT MATCHES "[0-9]"  THEN
            INSERT INTO safre_tmp:tmp_modificacion_rechazo_curp
                VALUES(p_nss,v_curp_char,v_cod_rechazo)

            DELETE
              FROM safre_tmp:tmp_modificacion_curp
             WHERE curp = v_curp_char
                OR nss  = p_nss
            

            -- bandera cero que hay error y no pasa a la segunda validación
            LET v_bandera = 0
         ELSE
            LET v_bandera = 1
         END IF
      END IF

      -- si no hay error en la cuarta validación incia validación de la posición 18
      IF v_bandera = 1 THEN
         IF p_curp.getCharAt(18) NOT MATCHES "[0-9]"  THEN
            INSERT INTO safre_tmp:tmp_modificacion_rechazo_curp
                 VALUES(p_nss,v_curp_char,v_cod_rechazo)

            DELETE
              FROM safre_tmp:tmp_modificacion_curp
             WHERE curp = v_curp_char
                OR nss  = p_nss
           
            -- bandera cero que hay error y no pasa a la segunda validación
            LET v_bandera = 0
         ELSE
            LET v_bandera = 1
         END IF
      END IF

   END IF
   RETURN v_bandera
END FUNCTION
