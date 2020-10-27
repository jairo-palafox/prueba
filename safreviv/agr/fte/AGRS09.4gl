########################################################################
#Modulo            => AGR                                              #
#Programa          => AGRS09                                           #
#Objetivo          => Programa que ejecuta el proceso de generación    #
#                     de extractor rechazos del WS de marca y desmarca #
#Autor             => Héctor Jiménez                                   #
#Fecha inicio      => 09 Julio 2015                                    #
########################################################################
DATABASE safre_viv

GLOBALS "AGRG01.4gl"
   DEFINE m_v_usuario               LIKE seg_usuario.usuario     -- nombre del usuario
   DEFINE m_d_pid                   LIKE bat_ctr_proceso.pid     -- pid
   DEFINE m_i_proceso_cod           LIKE cat_proceso.proceso_cod -- código del proceso
   DEFINE m_i_opera_cod             LIKE cat_operacion.opera_cod -- código de la operación
   DEFINE v_s_qry                   STRING
   DEFINE v_r_envio                 LIKE seg_modulo.ruta_envio
   DEFINE v_r_listados              LIKE seg_modulo.ruta_listados
   DEFINE v_estado                  SMALLINT

MAIN
   DEFINE p_fec_inicio              DATE
   DEFINE p_fec_fin                 DATE

   -- se recuperan los parametros que envia el programa lanzador
   LET m_v_usuario      = ARG_VAL(1)
   LET m_d_pid          = ARG_VAL(2)
   LET m_i_proceso_cod  = ARG_VAL(3)
   LET m_i_opera_cod    = ARG_VAL(4)
   LET p_fec_inicio     = ARG_VAL(5)
   LET p_fec_fin        = ARG_VAL(6)

   -- Se invoca la función que realiza el proceso principal
   CALL fn_genera_extractor(p_fec_inicio,p_fec_fin)

END MAIN

FUNCTION fn_genera_extractor(v_fec_inicio,v_fec_fin)
   DEFINE v_fec_inicio              DATE
   DEFINE v_fec_fin                 DATE
   DEFINE v_ruta_arch               STRING
   DEFINE v_nom_arch                STRING
   DEFINE v_canal                   base.Channel
   DEFINE v_cnt_rech                INTEGER
   DEFINE v_cnt_rch_defini          INTEGER
   DEFINE v_c_programa_cod          LIKE seg_programa.programa_cod
   DEFINE v_nom_reporte             STRING
   DEFINE v_manejador_rpt           om.SaxDocumentHandler
   DEFINE v_rec_cta             RECORD
      nss                           LIKE afi_derechohabiente.nss,
      num_credito                   LIKE cta_marca_ws.num_credito,
      desc_rechazo                  VARCHAR(80),
      f_solicita                    LIKE cta_marca_ws.f_solicita,
      intento                       LIKE cta_marca_ws.intento,
      dias_transc                   INTEGER
   END RECORD

   DEFINE arr_cadena DYNAMIC ARRAY OF RECORD
      diagnostico    CHAR(3),
      cuenta         SMALLINT
   END RECORD

   DEFINE arr_agrupa DYNAMIC ARRAY OF RECORD
      nss            CHAR(11),
      num_credito    DECIMAL(10,0),
      f_min          DATE,
      f_max          DATE
   END RECORD

   DEFINE a          SMALLINT
   DEFINE b          SMALLINT
   DEFINE v_cadena   STRING
   DEFINE v_qry      STRING
   DEFINE v_cta_reg  INTEGER
   DEFINE v_reg      STRING

   -- Se inicializan las variables
   LET v_cnt_rech       = 0
   LET v_cnt_rch_defini = 0

   -- Se actualiza la operación
   CALL fn_actualiza_opera_ini( m_d_pid         ,
                                m_i_proceso_cod ,
                                m_i_opera_cod   ,
                                0               ,
                                "AGRS09"        ,
                                ""              ,
                                m_v_usuario     ) RETURNING v_estado

   DISPLAY "=INICIA AGRS09="
   DISPLAY " USUARIO       : ",m_v_usuario
   DISPLAY " PID           : ",m_d_pid

   IF v_fec_fin IS NULL OR v_fec_fin = "" THEN
      LET v_fec_fin = TODAY 
   END IF
   
   LET v_s_qry = "SELECT ruta_envio,
                         ruta_listados
                    FROM seg_modulo
                   WHERE modulo_cod = 'agr' "

   PREPARE prp_r_lst FROM v_s_qry
   EXECUTE prp_r_lst INTO v_r_envio,
                          v_r_listados

   LET v_nom_arch     = "extracto_rechazos_WS_Marca_Desmarca.rchws" CLIPPED
   LET v_ruta_arch    = v_r_envio CLIPPED || "/" || v_nom_arch CLIPPED

   DISPLAY "\n Fecha inicio     : ",v_fec_inicio
   DISPLAY " Fecha fin        : ",v_fec_fin

   CALL fn_crea_tmp()

   -- Se abre el canal
   LET v_canal = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_canal.openFile(v_ruta_arch, "w" )

   -- Se establece que el delimitador sean pipes
   CALL v_canal.setDelimiter("")

   -- Se obtienen los registros de cta_his_marca_ws rechazoz definitivos
   LET v_s_qry = "SELECT a.nss,
                         cm.num_credito,
                         cm.diagnostico || ' - ' || rc.desc_rechazo,
                         cm.f_solicita,
                         cm.intento, '"
                         || v_fec_fin || "' - cm.f_solicita
                    FROM afi_derechohabiente a,
                         cta_his_marca_ws cm,
                         cat_rechazo rc
                   WHERE a.id_derechohabiente  = cm.id_derechohabiente
                     AND cm.diagnostico        = rc.cod_rechazo
                     AND cm.f_solicita BETWEEN '" || v_fec_inicio || "' AND '" ||v_fec_fin ||"'" ||
                   " AND cm.diagnostico       <> '001'
                     AND situacion            IN (0,2)
                     AND intento              <> 1 "

   PREPARE prp_his FROM v_s_qry
   DECLARE cur_his CURSOR FOR prp_his

   FOREACH cur_his INTO v_rec_cta.*
      -- Se escribe en el archivo
      --CALL v_canal.write(v_rec_cta)
      INSERT INTO safre_tmp:tmp_agrupa_rch_ws VALUES (v_rec_cta.nss,
                                                   v_rec_cta.num_credito,
                                                   v_rec_cta.desc_rechazo,
                                                   v_rec_cta.f_solicita,
                                                   v_rec_cta.intento,
                                                   v_rec_cta.dias_transc)

      -- Se incrementa el contador
      LET v_cnt_rch_defini = v_cnt_rch_defini + 1
   END FOREACH

   -- Se obtienen los rechazoz de cta_marca_ws
   LET v_s_qry = "SELECT a.nss,
                         cm.num_credito,
                         cm.diagnostico || ' - ' || rc.desc_rechazo,
                         cm.f_solicita,
                         cm.intento, '"
                         ||v_fec_fin || "' - cm.f_solicita
                    FROM afi_derechohabiente a,
                         cta_marca_ws cm,
                         cat_rechazo rc
                   WHERE a.id_derechohabiente  = cm.id_derechohabiente
                     AND cm.diagnostico        = rc.cod_rechazo
                     AND cm.f_solicita BETWEEN '" || v_fec_inicio || "' AND '" ||v_fec_fin ||"'" ||
                   " AND cm.diagnostico       <> '001'
                     AND situacion            IN (0,2)
                     AND intento              <> 1"

   PREPARE prp_cta_marca FROM v_s_qry
   DECLARE cur_cta_marca CURSOR FOR prp_cta_marca

   FOREACH cur_cta_marca INTO v_rec_cta.*
      -- Se escribe en el archivo
      --CALL v_canal.write(v_rec_cta)
      INSERT INTO safre_tmp:tmp_agrupa_rch_ws VALUES (v_rec_cta.nss,
                                                   v_rec_cta.num_credito,
                                                   v_rec_cta.desc_rechazo,
                                                   v_rec_cta.f_solicita,
                                                   v_rec_cta.intento,
                                                   v_rec_cta.dias_transc)

      -- Se incrementa el contador
      LET v_cnt_rech = v_cnt_rech + 1
   END FOREACH

   LET v_s_qry = "SELECT UNIQUE nss
                    FROM safre_tmp:tmp_agrupa_rch_ws"

   PREPARE prp_agrupa FROM v_s_qry
   DECLARE cur_agrupa CURSOR FOR prp_agrupa

   LET a = 1
   FOREACH cur_agrupa INTO arr_agrupa[a].nss

      SELECT MIN(f_solicita),
             MAX(f_solicita)
        INTO arr_agrupa[a].f_min,
             arr_agrupa[a].f_max
        FROM safre_tmp:tmp_agrupa_rch_ws
       WHERE nss = arr_agrupa[a].nss

      LET v_qry = "SELECT FIRST 1(num_credito)
                     FROM safre_tmp:tmp_agrupa_rch_ws
                    WHERE nss = ",arr_agrupa[a].nss

       PREPARE prp_num_cred FROM v_qry
      DECLARE cur_num_cred CURSOR FOR prp_num_cred

      LET b = 1
      FOREACH cur_num_cred INTO arr_agrupa[a].num_credito
         LET b = b +1
         IF arr_agrupa[a].num_credito IS NOT NULL THEN
            EXIT FOREACH
         END IF
      END FOREACH

       

       SELECT COUNT (*)
         INTO v_cta_reg
         FROM safre_tmp:tmp_agrupa_rch_ws
        WHERE nss = arr_agrupa[a].nss

      LET v_qry = "
      SELECT diagnostico,
             COUNT(*) cuenta
        FROM safre_tmp:tmp_agrupa_rch_ws
       WHERE nss = ",arr_agrupa[a].nss," group by 1"

      LET b = 1 

      CALL arr_cadena.clear()

      PREPARE prp_cadena FROM v_qry
      DECLARE cur_cadena CURSOR FOR prp_cadena
      
      FOREACH cur_cadena INTO arr_cadena[b].*
         LET b = b +1
      END FOREACH

      CALL arr_cadena.deleteElement(b)

      LET v_cadena = " "
      FOR b = 1 TO arr_cadena.getLength()
         LET v_cadena = v_cadena CLIPPED,arr_cadena[b].diagnostico[1,3],arr_cadena[b].cuenta USING "&&&"
         --DISPLAY v_cadena
      END FOR

      LET v_reg      = arr_agrupa[a].nss CLIPPED,
                       arr_agrupa[a].num_credito CLIPPED USING "&&&&&&&&&&",
                       arr_agrupa[a].f_min USING "YYYYMMDD" CLIPPED,
                       arr_agrupa[a].f_max USING "YYYYMMDD" CLIPPED,
                       v_cta_reg USING "&&&",
                       arr_agrupa[a].f_max - arr_agrupa[a].f_min USING "&&&",
                       v_cadena CLIPPED

   CALL v_canal.write(v_reg)
   END FOREACH

   -- se cierra el manejador de lectura
   CALL v_canal.close()

   DISPLAY "\n Nombre Archivo             : ",v_ruta_arch
   DISPLAY " Total rechazos             : ",v_cnt_rech
   DISPLAY " Total rechazos definitivos : ",v_cnt_rch_defini


   -- se carga la configuración del reporte
   IF fgl_report_loadCurrentSettings("AGRS091.4rp") THEN
      -- se obtiene el nombrel del programa correspondiente
      LET v_c_programa_cod = "AGRS09"

      -- se crea el nombre del reporte
      LET v_nom_reporte = m_v_usuario CLIPPED,"-",
                            v_c_programa_cod CLIPPED,"-",
                            m_d_pid USING "&&&&&","-",
                            m_i_proceso_cod USING "&&&&&","-",
                            m_i_opera_cod USING "&&&&&",".pdf"

      -- se indica en donde se guardará el reporte
      CALL fgl_report_setOutputFileName(v_r_listados CLIPPED||"/"||v_nom_reporte)

      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración en el menejador del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY "ERROR: No fue posible abrir la plantilla del reporte"

      EXIT PROGRAM
   END IF

   -- se inicia el reporte
   START REPORT rpt_rechazos_ws_marca_desmarca TO XML HANDLER v_manejador_rpt

   -- salida del reporte
   OUTPUT TO REPORT rpt_rechazos_ws_marca_desmarca(m_v_usuario,v_cnt_rech,v_cnt_rch_defini)

   -- Termina el reporte
   FINISH REPORT rpt_rechazos_ws_marca_desmarca

   -- Se actualiza la operación a Finalizada
   CALL fn_actualiza_opera_fin(m_d_pid,
                               m_i_proceso_cod,
                               m_i_opera_cod)
                     RETURNING v_estado

   DISPLAY "= FIN ="
END FUNCTION

FUNCTION fn_crea_tmp()
   DATABASE safre_tmp

   DROP TABLE IF EXISTS tmp_agrupa_rch_ws

   CREATE TABLE tmp_agrupa_rch_ws ( nss         CHAR(11),
                                    num_credito DECIMAL(10,0),
                                    diagnostico CHAR(50),
                                    f_solicita  DATE,
                                    intento     SMALLINT,
                                    dias_tras   SMALLINT
                                    )
   DATABASE safre_viv   
                                    
END FUNCTION

#Objetivo: Genera el reporte de Rechazos
REPORT rpt_rechazos_ws_marca_desmarca(p_v_usuario, p_cnt_rech, p_cnt_rch_defini)
   DEFINE p_v_usuario               LIKE seg_usuario.usuario_cod
   DEFINE p_cnt_rech                INTEGER
   DEFINE p_cnt_rch_defini          INTEGER
   DEFINE v_fecha                   DATE

   FORMAT
      FIRST PAGE HEADER
         LET v_fecha = TODAY

         PRINTX m_v_usuario
         PRINTX v_fecha USING "dd-mm-yyyy"
         PRINTX p_cnt_rech
         PRINTX p_cnt_rch_defini

END REPORT