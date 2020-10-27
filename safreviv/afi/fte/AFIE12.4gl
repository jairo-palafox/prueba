################################################################################
# Proyecto     => SAFRE VIVIENDA                                               #
# Propietario  => E.F.P.                                                       #
# Modulo       => AFI                                                          #
# Programa     => AFIE12                                                       #
# Objetivo     => Lanzado para carga de archivo de modificación Fecha de Nac   #
# Autor        => Antonio Gómez                                                #
# Fecha        => Agosto 21, 2017                                              #
################################################################################
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
   DEFINE v_f_nacimiento         CHAR (8)
   DEFINE v_bandera              SMALLINT
   DEFINE v_cnt_totales          INTEGER
   DEFINE v_c_fecha_salida       CHAR(10)
   DEFINE v_QryTxt               STRING
   DEFINE v_formato_fecha        CHAR(8)   

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

   -- Se obtiene el nombre del archivo sin extensión 
   LET v_nom_arch_sin_ext = base.StringBuffer.create()

   CALL v_nom_arch_sin_ext.append(v_nom_archivo)
   CALL v_nom_arch_sin_ext.replace(".fnac","",0)

   CALL STARTLOG(g_usuario CLIPPED||".AFIE12.log")

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

      LET v_comando = " EXECUTE PROCEDURE sp_crea_modif_f_nacimiento()"

      PREPARE prp_afore_tmp FROM v_comando
      EXECUTE prp_afore_tmp

      -- Se regresa a la BD safre_vov
      DATABASE safre_viv

      LET v_ruta_nom = v_ruta_rescate CLIPPED ||"/"||v_nom_archivo

      --DISPLAY "RUTA ARCHIVO : ",v_ruta_nom

      -- Se realiza el load a la tabla temporal
      LOAD FROM v_ruta_nom
      INSERT INTO safre_tmp:tmp_modificacion_f_nacimiento

      -- Se obtiene el total de registros en el archivo
      LET v_comando = "SELECT COUNT(*)
                         FROM safre_tmp:tmp_modificacion_f_nacimiento"

      PREPARE prp_totales FROM v_comando
      EXECUTE prp_totales INTO v_cnt_totales
      
      -- Se ejecuta la funcion que valida que exista el nss y que la cuenta este activa
      LET v_comando = "EXECUTE PROCEDURE sp_valida_mod_f_nacimiento()"
      PREPARE prp_valida_rfc FROM v_comando
      EXECUTE prp_valida_rfc INTO v_bandera, v_cnt_aceptados, v_cnt_rechazados

      -- si la bandera es cero la estructura es incorrecta
         IF v_bandera = 0 THEN
         ELSE
            DECLARE cur_update CURSOR FOR SELECT nss, f_nacimiento
                                          FROM safre_tmp:tmp_modificacion_f_nacimiento
            DISPLAY "   ---- DATOS ACTUALIZADOS ---- "

            FOREACH cur_update INTO v_nss_validar, v_f_nacimiento
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

               -- se obtienen las cifras
               LET v_formato_fecha = "ddmmyyyy"
               LET v_QryTxt = "EXECUTE FUNCTION fn_verifica_fecha(?,?)"
               PREPARE prp_verifica_fecha FROM v_QryTxt
               EXECUTE prp_verifica_fecha USING v_f_nacimiento, v_formato_fecha
                                          INTO  v_bandera, v_c_fecha_salida

               DISPLAY v_nss_validar, " - ", v_bandera ," - ", v_c_fecha_salida

               -- Se realiza la actualización del RFC
               LET v_cadena = "UPDATE afi_derechohabiente
                                  SET f_nacimiento = ?
                                WHERE nss = ?"

               PREPARE prp_upd_rfc FROM v_cadena
               EXECUTE prp_upd_rfc USING v_c_fecha_salida, v_nss_validar

            END FOREACH
         END IF

--******************************************************************************

      LET bnd_ws  = 0
      LET bnd_rfc = 0

      IF v_nss_validar IS NOT NULL THEN
         LET bnd_ws  = 1
         LET bnd_rfc = 1

         IF v_rfc_validar IS NULL THEN
            LET v_rfc_validar = " "
         END IF

         --DISPLAY "se consume WS"
      END IF

      IF bnd_ws = 1 THEN

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
         --RUN v_s_comando

         --CALL fn_mensaje("Atención","","information")
      END IF
      
--******************************************************************************

      DISPLAY "\n    ---- INFORMACIÓN DE ARCHIVOS ----" 
      DISPLAY "Registros en archivo  : ",v_cnt_totales
      DISPLAY "Archivo cargado  : ",v_nom_archivo

      -- Se obtienen las cifras control
      SELECT COUNT(*)
      INTO   v_cnt_aceptados
      FROM   safre_tmp:tmp_modificacion_f_nacimiento

      SELECT COUNT(*)
      INTO   v_cnt_rechazados
      FROM   safre_tmp:tmp_mod_rch_f_nacimiento
      
      -----------------------------------------------------
      -- Se genera el archivo de salida con los rechazos --
      -----------------------------------------------------
      IF v_cnt_rechazados > 0 THEN 
         UNLOAD TO v_ruta_envio CLIPPED ||"/"|| v_nom_arch_sin_ext.toString() CLIPPED ||".rchfnac"
         SELECT *
         FROM safre_tmp:tmp_mod_rch_f_nacimiento

         DISPLAY "Archivo Rechazos : " || v_ruta_envio CLIPPED ||"/"|| v_nom_arch_sin_ext.toString() CLIPPED ||".rchfnac"
      END IF

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
                           "AFIE12-"                   ,        -- programa
                           g_pid           USING "&&&&&","-",   -- PID
                           g_proceso_cod   USING "&&&&&", "-",  -- codigo del proceso
                           g_opera_cod     USING "&&&&&",".pdf" -- codigo de la operación

      DISPLAY "Ruta del reporte : ", v_ruta_reporte

      -- se indica que el reporte usara la plantilla creada
      IF ( fgl_report_loadCurrentSettings("../../afi/bin/AFIE12.4rp") ) THEN  -- if  the file loaded OK
         -- sin preview
         CALL fgl_report_selectPreview(0)
         -- se indica que se escriba en archivo
         CALL fgl_report_setOutputFileName(v_ruta_reporte)       

         LET v_report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings

         DISPLAY "\n    ---- CIFRAS CONTROL ---- "
         DISPLAY "Total      : ",v_cnt_totales
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
         DISPLAY "No se puede leer la plantilla del reporte AFIE12.4rp"
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
