###############################################################################################
# Modulo       => AFI                                                                         #
# Programa     => AFIE08                                                                      #
# Objetivo     => Programa que realiza la lectura del archivo para la actualizacion           #
#                   de datos paterno,materno y nombre                                         #
# Autor        => Héctor Jiménez                                                              #
# Fecha        => 04/Junio/2015                                                               #
# actualización 21/03/2016  se agrega WS para homologar información con TRM                   #
# Autor mod.   => Emilio Abarca                                                               #
# Fecha mod.   => 26/Abril/2018                                                               #
# Objetivo     => Se genera folio para historia del registro.                                 #
###############################################################################################
DATABASE safre_viv

   DEFINE g_usuario                CHAR (20)
   DEFINE g_pid                    DECIMAL (9,0)  --ID del proceso
   DEFINE g_proceso_cod            SMALLINT       --código del proceso
   DEFINE g_opera_cod              SMALLINT       --código de operacion
   DEFINE v_nom_archivo            STRING
   DEFINE v_ruta_rescate           CHAR(40)
   DEFINE v_cadena                 STRING

   DEFINE bnd_ws                 SMALLINT
   DEFINE bnd_nombre             SMALLINT
   DEFINE bnd_act                CHAR(1)
   DEFINE v_bnd                  SMALLINT
   DEFINE v_s_comando            STRING
   DEFINE v_partner              CHAR(1)
   DEFINE g_folio                DECIMAL(10,0)

MAIN
   DEFINE v_comando                STRING
   DEFINE v_estado_sql             SMALLINT
   DEFINE v_ruta_envio             CHAR(40)
   DEFINE v_ruta_listados          VARCHAR (40)
   DEFINE v_ruta_reporte           STRING
   DEFINE v_report_handler         om.SaxDocumentHandler -- handler para el reporte en PDF
   DEFINE nom_archivo              CHAR (40)
   DEFINE v_ruta_nom               STRING
   DEFINE v_nom_arch_sin_ext       base.StringBuffer
   DEFINE v_status                 SMALLINT
   DEFINE v_cnt_aceptados          INTEGER
   DEFINE v_cnt_rechazados         INTEGER 
   DEFINE v_nss_validar            CHAR (11)
   DEFINE v_ap_paterno_validar     CHAR (40)
   DEFINE v_ap_materno_validar     CHAR (40)
   DEFINE v_nombre_validar         CHAR (40)
   DEFINE v_bandera                SMALLINT
   DEFINE v_cnt_totales            INTEGER


   LET g_usuario      = ARG_VAL (1)
   LET g_pid          = ARG_VAL (2)
   LET g_proceso_cod  = ARG_VAL (3)
   LET g_opera_cod    = ARG_VAL (4)
   LET v_nom_archivo  = ARG_VAL (5)

   -- Se obtiene el nombre del archivo sin extensión 
   LET v_nom_arch_sin_ext = base.StringBuffer.create()
   CALL v_nom_arch_sin_ext.append(v_nom_archivo)--bu.append(v_nom_archivo)
   CALL v_nom_arch_sin_ext.replace(".nomact","",0)

   CALL STARTLOG(g_usuario CLIPPED||".AFIE08.log")

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

      LET v_comando = " EXECUTE PROCEDURE sp_crea_modif_nombre()"
      PREPARE prp_afore_tmp FROM v_comando
      EXECUTE prp_afore_tmp

      -- Se regresa a la BD safre_vov
      DATABASE safre_viv

      LET v_ruta_nom = v_ruta_rescate CLIPPED ||"/"||v_nom_archivo

      DISPLAY "RUTA ARCHIVO : ",v_ruta_nom

      -- Se realiza el load a la tabla temporal
      LOAD FROM v_ruta_nom
      INSERT INTO safre_tmp:tmp_modificacion_nombre

      -- Se obtiene el total de registros en el archivo
      LET v_comando = "SELECT COUNT(*)
                         FROM safre_tmp:tmp_modificacion_nombre"

      PREPARE prp_totales FROM v_comando
      EXECUTE prp_totales INTO v_cnt_totales
      
      -- Se ejecuta la funcion que valida que exista el nss y que la cuenta este activa
      LET v_comando = "EXECUTE PROCEDURE sp_valida_modif_nombre()"
      PREPARE prp_valida_rfc FROM v_comando
      EXECUTE prp_valida_rfc INTO v_cnt_aceptados, v_cnt_rechazados


      DECLARE cur_tmp CURSOR FOR SELECT nss,ap_paterno_af,ap_materno_af,nombre_af FROM safre_tmp:tmp_modificacion_nombre
      FOREACH cur_tmp INTO v_nss_validar, v_ap_paterno_validar,v_ap_materno_validar,v_nombre_validar

         CALL fn_valida_nombre_alt(v_nss_validar,v_ap_paterno_validar,v_ap_materno_validar,v_nombre_validar)
         RETURNING v_bandera
         --IF v_bandera = 0 THEN
           -- CONTINUE FOREACH
         --END IF

      END FOREACH

      DISPLAY "Registros en archivo  : ",v_cnt_totales
      DISPLAY "Archivo cargado       : ",v_nom_archivo

      -----------------------------------------------------
      -- Se genera el archivo de salida con los rechazos --
      -----------------------------------------------------
      UNLOAD TO v_ruta_envio CLIPPED ||"/"|| v_nom_arch_sin_ext.toString() CLIPPED ||".rnomact"
      SELECT *
        FROM safre_tmp:tmp_modificacion_rechazo_nombre

      DISPLAY "Archivo Rechazos : " || v_ruta_envio CLIPPED ||"/"|| v_nom_arch_sin_ext.toString() CLIPPED ||".rnomact"

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
                           "AFIE08-"                   ,        -- programa
                           g_pid           USING "&&&&&","-",   -- PID
                           g_proceso_cod   USING "&&&&&", "-",  -- codigo del proceso
                           g_opera_cod     USING "&&&&&",".pdf" -- codigo de la operación

      DISPLAY "Ruta del reporte: ", v_ruta_reporte

      -- se indica que el reporte usara la plantilla creada
      IF ( fgl_report_loadCurrentSettings("../../afi/bin/AFIE08.4rp") ) THEN  -- if  the file loaded OK
         -- sin preview
         CALL fgl_report_selectPreview(0)
         -- se indica que se escriba en archivo
         CALL fgl_report_setOutputFileName(v_ruta_reporte)       

         LET v_report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings

         -- Se obtienen las cifras control
         SELECT COUNT(*)
           INTO v_cnt_aceptados
           FROM safre_tmp:tmp_modificacion_nombre

         SELECT COUNT(*)
         INTO v_cnt_rechazados
         FROM safre_tmp:tmp_modificacion_rechazo_nombre

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
         DISPLAY "No se puede leer la plantilla del reporte AFIE08.4rp"
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

FUNCTION fn_valida_nombre_alt(p_nss,p_paterno,p_materno,p_nombre) --(p_nombre,p_param)
   DEFINE p_nss                    CHAR(11)
   DEFINE p_nombre                 CHAR(40)
   DEFINE p_paterno                CHAR(40)
   DEFINE p_materno                CHAR(40)
   DEFINE v_nombre                 CHAR(40)
   DEFINE v_paterno                CHAR(40)
   DEFINE v_materno                CHAR(40)
   DEFINE v_s_nombre               STRING
   DEFINE v_s_paterno              STRING
   DEFINE v_s_materno              STRING
   DEFINE v_cnt                    SMALLINT
   DEFINE v_bandera                SMALLINT
   -- Record para la inserción en la historica
   DEFINE v_rec_inserta        RECORD
     id_dh                         DECIMAL(9,0),
     f_modifica                    DATE,
     folio_lote                    DECIMAL(9,0),
     ind_modifica                  CHAR(18),
     curp                          CHAR(18),
     rfc                           CHAR(11),
     ind_nrp                       CHAR(1),
     f_nacimiento                  DATE ,
     nombre_imss                   CHAR(50),
     nombre_af                     CHAR(40),
     ap_paterno_af                 CHAR(40),
     ap_materno_af                 CHAR(40)
   END RECORD

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
   EXECUTE prp_cons_afi USING p_nss INTO v_rec_inserta.id_dh,
                                         v_rec_inserta.curp,
                                         v_rec_inserta.rfc,
                                         v_rec_inserta.nombre_af,
                                         v_rec_inserta.ap_paterno_af,
                                         v_rec_inserta.ap_materno_af,
                                         v_rec_inserta.nombre_imss,
                                         v_rec_inserta.f_nacimiento,
                                         v_rec_inserta.folio_lote,
                                         v_rec_inserta.ind_nrp

   LET v_rec_inserta.ind_modifica = 9
   LET v_rec_inserta.f_modifica   = TODAY

   -- Se valida el apellido paterno
   LET v_cadena = " SELECT ap_paterno_af
                      FROM safre_tmp:tmp_modificacion_nombre
                     WHERE nss = ? "

   PREPARE prp_cons_paterno FROM v_cadena
   EXECUTE prp_cons_paterno USING p_nss INTO v_paterno

   IF v_paterno IS NULL OR v_paterno[1] = " " THEN
      -- Se inserta el rechazo
      LET v_cadena = " INSERT INTO safre_tmp:tmp_modificacion_rechazo_nombre
                            VALUES(?,?,?,?) "

      PREPARE prp_ins_rch_pat FROM v_cadena
      EXECUTE prp_ins_rch_pat USING p_nss,
                                    p_paterno,
                                    p_materno,
                                    p_nombre

      -- Se elimina de la temporal
      LET v_cadena = " DELETE
                         FROM safre_tmp:tmp_modificacion_nombre
                        WHERE ap_paterno_af = ?
                           OR nss           = ? "

      PREPARE prp_del_tmp_paterno FROM v_cadena
      EXECUTE prp_del_tmp_paterno USING v_paterno,
                                        p_nss

      --RETURN v_bandera = 0
   ELSE
      LET v_s_paterno = v_paterno CLIPPED
      
      FOR v_cnt = 1 TO v_s_paterno.getLength()
         IF v_s_paterno.getCharAt(v_cnt) NOT MATCHES "[A-Z]" AND
            v_s_paterno.getCharAt(v_cnt) NOT MATCHES "[Ñ]"   AND
            v_s_paterno.getCharAt(v_cnt) NOT MATCHES "[#]"   AND 
            v_s_paterno.getCharAt(v_cnt) NOT MATCHES " "  THEN

            -- inserta en la tabla de rechazos
            LET v_cadena = " INSERT INTO safre_tmp:tmp_modificacion_rechazo_nombre
                            VALUES(?,?,?,?) "

            PREPARE prp_ins_rch_pate FROM v_cadena
            EXECUTE prp_ins_rch_pate USING p_nss,
                                           p_paterno,
                                           p_materno,
                                           p_nombre

      
            DELETE
              FROM safre_tmp:tmp_modificacion_nombre
             WHERE ap_paterno_af = p_paterno
                OR nss  =  p_nss

            -- Sale del ciclo for
            EXIT FOR
         ELSE
            -- Se inserta el registro en la tabla historica
            LET v_cadena = "INSERT INTO afi_his_derechohabiente
                             VALUES(?,?,?,?,?,?,?,?,?,?,?,?)"
      
            PREPARE prp_ins_his FROM v_cadena
            EXECUTE prp_ins_his USING v_rec_inserta.id_dh         ,
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

            -- Se actualiza afi_derechohabiente
            LET v_cadena = " UPDATE afi_derechohabiente
                                SET ap_paterno_af = ?
                              WHERE nss = ? "

            PREPARE prp_upd_paterno FROM v_cadena
            EXECUTE prp_upd_paterno USING v_paterno,
                                          p_nss

            -- Se invoca la función que realiza la actualización de nombre_imss
            CALL fn_actualiza_nombre_imss(p_nss)

            EXIT FOR 
         END IF
      END FOR
   END IF

   -- Se valida el apellido materno
   LET v_cadena = " SELECT ap_materno_af
                      FROM safre_tmp:tmp_modificacion_nombre
                     WHERE nss = ?"

   PREPARE prp_cons_materno FROM v_cadena
   EXECUTE prp_cons_materno USING p_nss INTO v_materno

   IF v_materno IS NULL OR v_materno[1] = " " THEN
      INSERT INTO safre_tmp:tmp_modificacion_rechazo_nombre
           VALUES(p_nss,p_paterno,p_materno,p_nombre)

      DELETE
        FROM safre_tmp:tmp_modificacion_nombre
       WHERE ap_materno_af = v_materno
          OR nss  = p_nss

      --RETURN v_bandera = 0
   ELSE
      LET v_s_materno = v_materno CLIPPED

      FOR v_cnt = 1 TO v_s_materno.getLength()
         IF v_s_materno.getCharAt(v_cnt) NOT MATCHES "[A-Z]" AND 
            v_s_materno.getCharAt(v_cnt) NOT MATCHES "[Ñ]"   AND
            v_s_materno.getCharAt(v_cnt) NOT MATCHES "[#]"   AND
            v_s_materno.getCharAt(v_cnt) NOT MATCHES " "  THEN

            -- inserta en la tabla de rechazos
            INSERT INTO safre_tmp:tmp_modificacion_rechazo_nombre
                 VALUES(p_nss,p_paterno,p_materno,p_nombre)
         
            DELETE
              FROM safre_tmp:tmp_modificacion_nombre
             WHERE ap_materno_af = v_materno
                OR nss  = p_nss

            --RETURN v_bandera = 0

            EXIT FOR
         ELSE
            -- Se inserta el registro en la tabla historica
            LET v_cadena = "INSERT INTO afi_his_derechohabiente
                             VALUES(?,?,?,?,?,?,?,?,?,?,?,?)"
      
            PREPARE prp_ins_historica FROM v_cadena
            EXECUTE prp_ins_historica USING v_rec_inserta.id_dh         ,
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

            -- Se actualiza el materno
            LET v_cadena = " UPDATE afi_derechohabiente
                                SET ap_materno_af = ?
                              WHERE nss = ? "

            PREPARE prp_upd_materno FROM v_cadena
            EXECUTE prp_upd_materno USING v_materno,
                                          p_nss

            -- Se invoca la función que realiza la actualización de nombre_imss
            CALL fn_actualiza_nombre_imss(p_nss)         

            -- Sale del ciclo
            EXIT FOR
             --RETURN v_bandera = 1
         END IF
      END FOR
   END IF

   -- Se valida el nombre
   LET v_cadena = " SELECT nombre_af
                      FROM safre_tmp:tmp_modificacion_nombre
                     WHERE nss = ? "

   PREPARE prp_cons_nombre FROM v_cadena
   EXECUTE prp_cons_nombre USING p_nss INTO v_nombre

   IF v_nombre IS NULL OR v_nombre[1] = " " THEN
      INSERT INTO safre_tmp:tmp_modificacion_rechazo_nombre
           VALUES(p_nss,p_paterno,p_materno,p_nombre)

      DELETE
        FROM safre_tmp:tmp_modificacion_nombre
       WHERE nombre_af = v_nombre
          OR nss  = p_nss

      --RETURN v_bandera = 0
   ELSE
      LET v_s_nombre = v_nombre

      FOR v_cnt = 1 TO v_s_nombre.getLength()
         IF v_s_nombre.getCharAt(v_cnt) NOT MATCHES "[A-Z]" AND
            v_s_nombre.getCharAt(v_cnt) NOT MATCHES "[Ñ]"   AND
            v_s_nombre.getCharAt(v_cnt) NOT MATCHES "[#]"   AND
            v_s_nombre.getCharAt(v_cnt) NOT MATCHES " "  THEN
      
            -- se valida si es un espacio en blanco ya que seria aceptado
            IF v_s_nombre.getCharAt(v_cnt) = " " THEN
               CONTINUE FOR
            END IF

            -- inserta en la tabla de rechazos
            INSERT INTO safre_tmp:tmp_modificacion_rechazo_nombre
                 VALUES(p_nss,p_paterno,p_materno,p_nombre)
         
            DELETE
              FROM safre_tmp:tmp_modificacion_nombre
             WHERE nombre_af = v_nombre
                OR nss  = p_nss

            --RETURN v_bandera = 0
            EXIT FOR
         ELSE
            -- Se inserta el registro en la tabla historica
            LET v_cadena = "INSERT INTO afi_his_derechohabiente
                             VALUES(?,?,?,?,?,?,?,?,?,?,?,?)"
      
            PREPARE prp_ins_histo FROM v_cadena
            EXECUTE prp_ins_histo USING v_rec_inserta.id_dh         ,
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

            LET v_cadena = " UPDATE afi_derechohabiente
                                SET nombre_af = ?
                              WHERE nss = ? "

            PREPARE prp_upd_nombre FROM v_cadena
            EXECUTE prp_upd_nombre USING v_nombre,
                                         p_nss

--******************************************************************************

      LET bnd_ws     = 0
      LET bnd_nombre = 0

      IF (v_nombre IS NOT NULL) OR
         (v_paterno IS NOT NULL) OR
         (v_materno IS NOT NULL) THEN
         
         IF v_nombre IS NULL THEN
            LET v_nombre = " "
         END IF

         IF v_paterno IS NULL THEN
            LET v_paterno = " "
         END IF

         IF v_materno IS NULL THEN
            LET v_materno = " "
         END IF

         LET bnd_ws     = 1
         LET bnd_nombre = 1
      END IF

      IF bnd_ws = 1 THEN

         LET bnd_nombre = 1

         LET bnd_act = "N"

         LET v_partner = " "

         LET v_bnd = 1
         LET v_s_comando = " nohup time fglrun ",
                           "AFIW06"," ",
                           "'",v_bnd          CLIPPED,"'"," ",
                           "'",bnd_act        CLIPPED,"'"," ",
                           "'",v_partner      ,"'"," ",
                           "'"," "            ,"'"," ",
                           "'"," "            ,"'"," ",
                           "'",v_nombre       ,"'"," ",
                           "'",v_paterno      ,"'"," ",
                           "'",v_materno      ,"'"," ",
                           "'",p_nss          CLIPPED,"'"," "--, " ",
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
            -- Se invoca la función que realiza la actualización de nombre_imss
            CALL fn_actualiza_nombre_imss(p_nss)

            -- Sale del ciclo
            EXIT FOR
         END IF
      END FOR
   END IF 

   RETURN v_bandera
END FUNCTION

#  Función para actualizar el nombre imss en base
#  a las modificaciones de nombre y/o paterno y/o materno
FUNCTION fn_actualiza_nombre_imss(p_nss) --,p_nombre,p_param)
   DEFINE p_nss                         CHAR(11)
   DEFINE v_nom_imss                    CHAR(40)
   DEFINE v_pat_imss                    CHAR(40)
   DEFINE v_mat_imss                    CHAR(40)
   DEFINE v_ax_nom_imss                 CHAR(50)
   DEFINE v_qry_exe                     STRING

   -- Se inicializan las variables
   LET v_nom_imss = " "
   LET v_pat_imss = " "
   LET v_mat_imss = " "

   -- Consulta para armar el nombre_imss
   LET v_qry_exe = "SELECT nombre_af,
                           ap_paterno_af,
                           ap_materno_af
                      FROM afi_derechohabiente
                     WHERE nss = ?"

   PREPARE prp_imss FROM v_qry_exe
   EXECUTE prp_imss USING p_nss INTO v_nom_imss,
                                     v_pat_imss,
                                     v_mat_imss

   CASE
      WHEN v_nom_imss IS NULL
         LET v_nom_imss = " "
      WHEN v_pat_imss IS NULL
         LET v_pat_imss = " "
      WHEN v_mat_imss IS NULL
         LET v_mat_imss = " "
   END CASE

   -- Se arma el nombre imss
   LET v_ax_nom_imss = v_pat_imss CLIPPED || "$" || v_mat_imss CLIPPED || "$" || v_nom_imss

   -- Se actualiza el nombre_imss
   LET v_qry_exe = "UPDATE afi_derechohabiente
                       SET nombre_imss = ?
                     WHERE nss         = ? "

   PREPARE prp_exe_imss FROM v_qry_exe
   EXECUTE prp_exe_imss USING v_ax_nom_imss,
                              p_nss

   --RETURN v_ax_nom_imss
END FUNCTION
