--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

#####################################################################
#Modulo            => AGR                                           #
#Programa          => AGRE15                                        #
#Objetivo          => Programa que permite la integración del       #
#                     archivo de homologación TRM SAFRE             #
#Autor             => Mauro Muñiz Caballero, EFP                    #
#Fecha inicio      => 19 de mayo de 2014                            #
#####################################################################

DATABASE safre_viv

GLOBALS "AGRG01.4gl"

   DEFINE g_v_usuario              LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE g_d_pid                  LIKE bat_ctr_proceso.pid -- pid
   DEFINE g_i_proceso_cod          LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE g_i_opera_cod            LIKE cat_operacion.opera_cod -- codigo de la operacion
   DEFINE g_d_folio                LIKE glo_ctr_archivo.folio -- numero de folio
   DEFINE g_v_arch_proceso         VARCHAR(100) -- nombre del archivo a integrar
   DEFINE g_id_cre_ctr_archivo     LIKE cre_acreditado.id_cre_ctr_archivo -- id del archivo
   DEFINE g_c_ruta_bin             LIKE seg_modulo.ruta_bin -- ruta del bin del módulo
   DEFINE g_c_ruta_listados        LIKE seg_modulo.ruta_listados -- ruta de listados del módulo
   DEFINE g_edo_homologa           SMALLINT
   DEFINE g_extension              CHAR(4)
   DEFINE g_tpo                    SMALLINT
   DEFINE g_edo                    SMALLINT

   DEFINE v_c_ruta_list_bat        LIKE seg_modulo.ruta_listados -- ruta listados de bat
   DEFINE v_dt_f_lote              LIKE cre_ctr_archivo.f_lote -- fecha del lote
   DEFINE v_i_operacion            LIKE cre_ctr_archivo.operacion -- operacion del proceso
   DEFINE v_c_programa_cod         LIKE cat_operacion.programa_cod -- programa de la operación
   DEFINE v_err_nss                CHAR(11)

   DEFINE v_r_cre_ctr_archivo RECORD
      tot_registros                LIKE cre_ctr_archivo.tot_registros, -- total de registros
      tot_aceptados                LIKE cre_ctr_archivo.tot_aceptados, -- total aceptados 
      tot_procesados               LIKE cre_ctr_archivo.tot_aceptados, -- total procesados
      tot_rechazados               LIKE cre_ctr_archivo.tot_rechazados, -- total rechazados
      tot_sin_origen               LIKE cre_ctr_archivo.tot_sin_origen -- total sin origen
   END RECORD

   DEFINE v_ax_estado              INTEGER;   -- total diferente estado
   DEFINE v_ax_tipo                INTEGER;   -- total diferente tipo
   DEFINE v_ax_numero              INTEGER;   -- total diferente número
   DEFINE v_ax_tp_edo              INTEGER;   -- total diferente tipo y estado

   DEFINE v_si_lote                INTEGER -- lote
   DEFINE v_s_qryTxt               STRING  -- guarda una sentencia SQL a ejecutar
   DEFINE v_s_mens_correo          STRING  -- contiene el cuerpo del correo
   DEFINE v_s_titulo_correo        STRING  -- contiene el titulo del correo
   DEFINE v_s_archivo_correo       STRING  -- ruta y nombre del archivo adjunto en el correo 
   DEFINE v_v_nom_reporte          VARCHAR(80) -- nombre del reporte
   DEFINE r_existe_error           SMALLINT -- indica si ocurrió un error durante algun proceso
   DEFINE r_b_valida               SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE r_edo_credito            INTEGER  -- estado del registro a homologar
   DEFINE v_existe_info            DECIMAL(8,0)  ---variable para verificar si existe información en la tabla temporal
   DEFINE v_ruta_archivo_rechazo   STRING
   DEFINE r_total                  INTEGER

#Objetivo: Función que realiza la integracion del archivo recurrente
MAIN

   -- se recuperan los parametros que envia el programa lanzador
   LET g_v_usuario          = ARG_VAL(1)
   LET g_d_pid              = ARG_VAL(2)
   LET g_i_proceso_cod      = ARG_VAL(3)
   LET g_i_opera_cod        = ARG_VAL(4)
   LET g_d_folio            = ARG_VAL(5)
   LET g_v_arch_proceso     = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(g_v_usuario CLIPPED|| ".AGRE15.log")

   CLOSE WINDOW SCREEN

   DISPLAY "=INICIA AGRE15="
   DISPLAY " USUARIO       : ",g_v_usuario
   DISPLAY " PID           : ",g_d_pid
   DISPLAY " ARCHIVO       : ",g_v_arch_proceso

   CALL fn_proceso_principal()

   DISPLAY "=FIN="

END MAIN

FUNCTION fn_proceso_principal()
#fpp---------------------------

   -- se inicializan variables
   LET v_i_operacion = 31
   LET v_existe_info = 0
   LET g_tpo         = 0
   LET g_edo         = 0

   -- se genera el folio
   LET g_d_folio = fn_genera_folio(g_i_proceso_cod, g_i_opera_cod, g_v_usuario)
   DISPLAY " FOLIO         : ",g_d_folio USING "#########&"

   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("agr") RETURNING g_c_ruta_bin, g_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat INTO v_c_ruta_list_bat

   -- se busca el identificador de la tabla de control de archivo correspondiente al proceso
   LET v_s_qryTxt = " SELECT FIRST 1 id_cre_ctr_archivo, lote, f_proceso\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_proceso = ",g_proc_cod_cre_trm,"\n",
                    "    AND operacion = ",v_i_operacion,"\n",
                    "    AND estado = 10\n",
                    "  ORDER BY id_cre_ctr_archivo DESC"

   PREPARE prp_id_creCtrArchivo FROM v_s_qryTxt
   EXECUTE prp_id_creCtrArchivo INTO g_id_cre_ctr_archivo, v_si_lote, v_dt_f_lote

   -- se verifica si fue posible obtener el identificador del archivo
   IF g_id_cre_ctr_archivo IS NULL THEN
      DISPLAY " ERROR: No fue posible obtener el identificador del archivo"

      EXIT PROGRAM
   END IF

   DISPLAY " IDENTIFICADOR DEL ARCHIVO: ",g_id_cre_ctr_archivo
   DISPLAY " INTEGRA ARCHIVO DE HOMOLOGACIÓN TRM - Saci SAFRE"

   DATABASE safre_tmp

   --Crea la tabla temporal de trabajo
   LET v_s_qryTxt = "EXECUTE PROCEDURE sp_crea_tabla_homologacion()"

   PREPARE prp_crea_tbl FROM v_s_qryTxt
   EXECUTE prp_crea_tbl

   DATABASE safre_viv

   -- se crea la sentencia que ejecuta el procedure que integra el archivo de homologación
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_cre_integra_homologacion(?,?,?,?)"

   PREPARE prp_integra_recurrente FROM v_s_qryTxt
   EXECUTE prp_integra_recurrente USING g_id_cre_ctr_archivo, g_d_folio, g_v_arch_proceso, g_v_usuario
                                   INTO r_b_valida, v_err_nss, r_edo_credito, v_ax_estado, v_ax_tipo, v_ax_numero, v_ax_tp_edo

   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      DISPLAY "ERROR EN EL PROCESO DE INTEGRACIÓN HOMOLOGACIÓN: ",r_b_valida
      DISPLAY "NSS: ",v_err_nss

      -- se invoca la función que deja la operación en estado ERRONEA
      LET r_b_valida = fn_error_opera(g_d_pid, g_i_proceso_cod, g_i_opera_cod)

      -- se verifica si fue posible marcar la operacion como Erronea
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se realiza el display de las cifras de control para información del archivo
   LET v_s_qryTxt = " SELECT tot_registros, tot_aceptados, tot_rechazados, tot_sin_origen\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",g_id_cre_ctr_archivo

   PREPARE prp_cifras_control FROM v_s_qryTxt
   EXECUTE prp_cifras_control INTO v_r_cre_ctr_archivo.tot_registros,
                                   v_r_cre_ctr_archivo.tot_procesados,
                                   v_r_cre_ctr_archivo.tot_rechazados,
                                   v_r_cre_ctr_archivo.tot_sin_origen

   DISPLAY ""
   DISPLAY " TOTALES ARCHIVO DE HOMOLOGACIÓN"
   DISPLAY "  Total registros                      : ", v_r_cre_ctr_archivo.tot_registros
   DISPLAY "  Total homologados                    : ", v_r_cre_ctr_archivo.tot_procesados
   DISPLAY "  Total rechazados                     : ", v_r_cre_ctr_archivo.tot_rechazados
   DISPLAY "  Sin originación o crédito diferente  : ", v_r_cre_ctr_archivo.tot_sin_origen
   DISPLAY ""
   DISPLAY " TOTALES POR TIPO DE HOMOLOGACIÓN"
   DISPLAY "  Cambio de estado                     : ", v_ax_estado
   DISPLAY "  Cambio de tipo                       : ", v_ax_tipo
   DISPLAY "  Cambio de número                     : ", v_ax_numero
   DISPLAY "  Cambio de tipo y estado              : ", v_ax_tp_edo
   DISPLAY ""
   DISPLAY " GENERA ARCHIVOS"
   DISPLAY ""

   -- el reporte se generará en la operación de Integración
   LET g_i_opera_cod = 2

   -- se actualiza el folio en la tabla de control de operaciones
   LET v_s_qryTxt = " UPDATE bat_ctr_operacion\n",
                    "    SET folio = ",g_d_folio,"\n",
                    "  WHERE pid = ",g_d_pid,"\n",
                    "    AND proceso_cod = ",g_i_proceso_cod,"\n",
                    "    AND opera_cod = ",g_i_opera_cod

   PREPARE prp_actualiza_folio FROM v_s_qryTxt
   EXECUTE prp_actualiza_folio

   --*****************
   --verifica si existe información para generar el archivo de nss no catalogados

   LET g_edo_homologa = 11
   LET g_extension    = "nsnc"

   LET v_s_qryTxt = "SELECT COUNT(*) \n",
                    "  FROM safre_tmp:tmp_reg_homologa \n",
                    " WHERE edo_homologa = ",g_edo_homologa

   PREPARE stm_nss_no_cat FROM v_s_qryTxt
   EXECUTE stm_nss_no_cat INTO v_existe_info

   -- genera archivo de salida
   IF v_existe_info > 0 THEN
      --se invoca la generación del archivo
      CALL fn_genera_archivo(v_dt_f_lote, g_edo_homologa, g_extension)
      RETURNING v_ruta_archivo_rechazo

      -- envia mensaje de generación de archivo
      DISPLAY "Se generó el archivo de NSS no catalogados en la ruta: ",v_ruta_archivo_rechazo
      DISPLAY ""
   ELSE
      -- se envía mensaje indicando que no existen registros para generar el archivo
      DISPLAY "No existen NSS no catalogados, no se generó archivo"
      DISPLAY ""
   END IF
   --*****************

   --*****************
   --verifica si existe información para generar el archivo de nss sin originación

   LET g_edo_homologa = 26
   LET g_extension    = "nsso"

   LET v_s_qryTxt = "SELECT COUNT(*) \n",
                    "  FROM safre_tmp:tmp_reg_homologa \n",
                    " WHERE edo_homologa = ",g_edo_homologa

   PREPARE stm_sin_orig FROM v_s_qryTxt
   EXECUTE stm_sin_orig INTO v_existe_info

   -- genera archivo de salida
   IF v_existe_info > 0 THEN
      --se invoca la generación del archivo
      CALL fn_genera_archivo(v_dt_f_lote, g_edo_homologa, g_extension)
      RETURNING v_ruta_archivo_rechazo

      -- envia mensaje de generación de archivo
      DISPLAY "Se generó el archivo de NSS sin originación en la ruta: ",v_ruta_archivo_rechazo
      DISPLAY ""
   ELSE
      -- se envía mensaje indicando que no existen registros para generar el archivo
      DISPLAY "No existen NSS sin originación, no se generó archivo"
      DISPLAY ""
   END IF
   --*****************

   --*****************
   --verifica si existe información para generar el archivo de nss sin crédito vigente ni liquidado

   LET g_edo_homologa = 14
   LET g_extension    = "nssc"

   LET v_s_qryTxt = "SELECT COUNT(*) \n",
                    "  FROM safre_tmp:tmp_reg_homologa \n",
                    " WHERE edo_homologa = ",g_edo_homologa

   PREPARE stm_sin_cred FROM v_s_qryTxt
   EXECUTE stm_sin_cred INTO v_existe_info

   -- genera archivo de salida
   IF v_existe_info > 0 THEN
      --se invoca la generación del archivo
      CALL fn_genera_archivo(v_dt_f_lote, g_edo_homologa, g_extension)
      RETURNING v_ruta_archivo_rechazo

      -- envia mensaje de generación de archivo
      DISPLAY "Se generó el archivo de NSS sin crédito vigente ó liquidado en la ruta: ",v_ruta_archivo_rechazo
      DISPLAY ""
   ELSE
      -- se envía mensaje indicando que no existen registros para generar el archivo
      DISPLAY "No existen NSS sin crédito vigente ó liquidado, no se genéró arhcivo"
      DISPLAY ""
   END IF
   --*****************

   --*****************
   --verifica si existe información para generar el archivo de nss con número de crédito diferente

   LET g_edo_homologa = 22
   LET g_extension    = "nnci"

   LET v_s_qryTxt = "SELECT COUNT(*) \n",
                    "  FROM safre_tmp:tmp_reg_homologa \n",
                    " WHERE edo_homologa = ",g_edo_homologa

   PREPARE stm_num_cred FROM v_s_qryTxt
   EXECUTE stm_num_cred INTO v_existe_info

   -- genera archivo de salida
   IF v_existe_info > 0 THEN
      --se invoca la generación del archivo
      CALL fn_genera_archivo(v_dt_f_lote, g_edo_homologa, g_extension)
      RETURNING v_ruta_archivo_rechazo

      -- envia mensaje de generación de archivo
      DISPLAY "Se generó el archivo de NSS con número de crédito diferente en la ruta: ",v_ruta_archivo_rechazo
      DISPLAY ""
   ELSE
      -- se envía mensaje indicando que no existen registros para generar el archivo
      DISPLAY "No existen NSS con número de crédito diferente, no se generó archivo"
      DISPLAY ""
   END IF
   --*****************

   --*****************
   --verifica si existe información para generar el archivo de nss con tipo de crédito diferente

   LET g_edo_homologa = 16
   LET g_extension    = "nstc"

   LET v_s_qryTxt = "SELECT COUNT(*) \n",
                    "  FROM safre_tmp:tmp_reg_homologa \n",
                    " WHERE edo_homologa = ",g_edo_homologa

   PREPARE stm_tip_cred FROM v_s_qryTxt
   EXECUTE stm_tip_cred INTO v_existe_info

   -- genera archivo de salida
   IF v_existe_info > 0 THEN
      --se invoca la generación del archivo
      CALL fn_genera_archivo(v_dt_f_lote, g_edo_homologa, g_extension)
      RETURNING v_ruta_archivo_rechazo

      -- envia mensaje de generación de archivo
      DISPLAY "Se generó el archivo de NSS con tipo de crédito deferente en la ruta: ",v_ruta_archivo_rechazo
      DISPLAY ""
      LET g_tpo = g_edo_homologa
   ELSE
      -- se envía mensaje indicando que no existen registros para generar el archivo
      DISPLAY "No existen NSS con tipo de crédito deferente, no se generó archivo"
      DISPLAY ""
   END IF
   --*****************

   --*****************
   --verifica si existe información para generar el archivo de nss con estado de crédito diferente

   LET g_edo_homologa = 15
   LET g_extension    = "nsec"

   LET v_s_qryTxt = "SELECT COUNT(*) \n",
                    "  FROM safre_tmp:tmp_reg_homologa \n",
                    " WHERE edo_homologa = ",g_edo_homologa

   PREPARE stm_edo_cred FROM v_s_qryTxt
   EXECUTE stm_edo_cred INTO v_existe_info

   -- genera archivo de salida
   IF v_existe_info > 0 THEN
      --se invoca la generación del archivo
      CALL fn_genera_archivo(v_dt_f_lote, g_edo_homologa, g_extension)
      RETURNING v_ruta_archivo_rechazo

      -- envia mensaje de generación de archivo
      DISPLAY "Se generó el archivo de NSS con estado de crédito deferente en la ruta: ",v_ruta_archivo_rechazo
      DISPLAY ""
      LET g_edo = g_edo_homologa
   ELSE
      -- se envía mensaje indicando que no existen registros para generar el archivo
      DISPLAY "No existen NSS con estado de crédito diferente, no se generó archivo"
      DISPLAY ""
   END IF
   --*****************

   --*****************
   --verifica si existe información para generar el archivo de nss con estado de crédito diferente y tipo de crédito diferente

   LET g_edo_homologa = 29
   LET g_extension    = "tpst"

   LET v_s_qryTxt = "SELECT COUNT(*) \n",
                    "  FROM safre_tmp:tmp_reg_homologa \n",
                    " WHERE edo_homologa = ",g_edo_homologa

   PREPARE stm_edo_tpo FROM v_s_qryTxt
   EXECUTE stm_edo_tpo INTO v_existe_info

   -- genera archivo de salida
   IF v_existe_info > 0 THEN
      --se invoca la generación del archivo
      CALL fn_genera_archivo(v_dt_f_lote, g_edo_homologa, g_extension)
      RETURNING v_ruta_archivo_rechazo

      -- envia mensaje de generación de archivo
      DISPLAY "Se generó el archivo de NSS con estado y tipo de crédito deferente en la ruta: ",v_ruta_archivo_rechazo
      DISPLAY ""
      LET g_edo = g_edo_homologa
   ELSE
      -- se envía mensaje indicando que no existen registros para generar el archivo
      DISPLAY "No existen NSS con estado y tipo de crédito diferente, no se generó archivo"
      DISPLAY ""
   END IF
   --*****************

   DISPLAY ""
   DISPLAY "INICIA HOMOLOGACIÓN DE TIPOS DE CRÉDITO"

   -- se crea la sentencia que ejecuta el procedure que homologa los tipos de crédito para dif tpo y mismo edo
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_cre_homologa_tpo_cred(?,?,?,?,?)"

   PREPARE prp_homologa_tpo FROM v_s_qryTxt
   EXECUTE prp_homologa_tpo USING g_tpo, g_v_usuario, g_i_proceso_cod, g_d_folio, g_id_cre_ctr_archivo
                             INTO r_b_valida, v_err_nss, r_total

   LET g_tpo = 29

   -- se crea la sentencia que ejecuta el procedure que homologa los tipos de crédito para dif tpo y dif edo
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_cre_homologa_tpo_cred(?,?,?,?,?)"

   PREPARE prp_homologa_tp2 FROM v_s_qryTxt
   EXECUTE prp_homologa_tp2 USING g_tpo, g_v_usuario, g_i_proceso_cod, g_d_folio, g_id_cre_ctr_archivo
                             INTO r_b_valida, v_err_nss, r_total

   DISPLAY "TERMINA HOMOLOGACIÓN DE TIPOS DE CRÉDITO"
   DISPLAY ""
   DISPLAY "INICIA HOMOLOGACIÓN DE ESTADOS DE CRÉDITO"

   -- se crea la sentencia que ejecuta el procedure que homologa los estados de crédito para dif edo y mismo tpo
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_cre_homologa_edo_cred(?,?,?,?,?)"

   PREPARE prp_homologa_edo FROM v_s_qryTxt
   EXECUTE prp_homologa_edo USING g_edo, g_v_usuario, g_i_proceso_cod, g_d_folio, g_id_cre_ctr_archivo
                             INTO r_b_valida, v_err_nss, r_total

   LET g_edo = 15

   -- se crea la sentencia que ejecuta el procedure que homologa los tipos de crédito para dif edo y dif tpo
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_cre_homologa_edo_cred(?,?,?,?,?)"

   PREPARE prp_homologa_ed2 FROM v_s_qryTxt
   EXECUTE prp_homologa_ed2 USING g_edo, g_v_usuario, g_i_proceso_cod, g_d_folio, g_id_cre_ctr_archivo
                             INTO r_b_valida, v_err_nss, r_total

   DISPLAY "TERMINA HOMOLOGACIÓN DE ESTADOS DE CRÉDITO"
   DISPLAY ""

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(g_d_pid, g_i_proceso_cod, g_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   DISPLAY " GENERA REPORTE"
   DISPLAY ""

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(g_i_proceso_cod , g_i_opera_cod)

   -- se asigna el nombre del reporte
   LET v_v_nom_reporte = g_v_usuario CLIPPED, "-", v_c_programa_cod CLIPPED,"-", g_d_pid USING "&&&&&", "-", g_i_proceso_cod USING "&&&&&", "-", g_i_opera_cod USING "&&&&&"

   -- se invoca la funcion que genera el reporte del proceso de Integración
   CALL f_genera_rpt_Homologa(v_v_nom_reporte, v_r_cre_ctr_archivo.*, v_ax_estado, v_ax_tipo, v_ax_numero, v_ax_tp_edo)
        RETURNING r_existe_error
   -- recupera la ruta bin y de listados para el módulo en proceso

   IF NOT r_existe_error THEN
      DISPLAY " ENVIA CORREO DEL REPORTE"
      -- se asigna el titulo del correo
      LET v_s_titulo_correo = "Proceso: RECEPCIÓN HOMOLOGACIÓN TRM - Saci SAFRE"

      -- se asigna el archivo a adjuntar
      LET v_s_archivo_correo = g_c_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

      -- se asigna el cuerpo del correo
      LET v_s_mens_correo =  " ID Proceso   : ",g_d_pid,"\n",
                             " Proceso      : RECEPCIÓN HOMOLOGACIÓN TRM - Saci SAFRE\n",
                             " Operacion    : INTEGRA ARCHIVO HOMOLOGACIÓN TRM - Saci SAFRE\n",
                             " Fecha Inicio : ",TODAY,"\n",
                             " Fecha Fin    : ",TODAY

      -- se invoca la función que envía por correo el elemento generado
      CALL fn_correo_proceso(g_d_pid,
                             g_i_proceso_cod,
                             g_i_opera_cod,
                             v_s_archivo_correo,
                             v_s_titulo_correo,
                             v_s_mens_correo)
   END IF

END FUNCTION

#OBJETIVO: Genera el archivo respectivo
FUNCTION  fn_genera_archivo(p_dt_f_lote, p_edo_homologa, p_extension)

   DEFINE v_s_sql                   STRING
   DEFINE v_nom_archivo             STRING
   DEFINE v_ruta_archivo            STRING
   DEFINE v_ch_archivo              base.Channel
   DEFINE v_s_linea                 STRING
   DEFINE v_c_ruta_envio            VARCHAR(40)
   DEFINE p_dt_f_lote               DATE
   DEFINE p_edo_homologa            SMALLINT
   DEFINE p_extension               CHAR(4)

   DEFINE r_arh_qry RECORD
      nss                           CHAR(11), 
      num_credito                   DECIMAL(10,0),
      tpo_credito                   SMALLINT,
      edo_credito                   SMALLINT,
      num_credito1                  DECIMAL(10,0),
      tpo_credito1                  SMALLINT,
      edo_credito1                  SMALLINT
   END RECORD

   DEFINE r_archivo RECORD
      nss                           CHAR(11),
      num_credito                   CHAR(10),
      tpo_credito                   CHAR(3),
      edo_credito                   CHAR(3),
      num_credito1                  CHAR(10),
      tpo_credito1                  CHAR(3),
      edo_credito1                  CHAR(3)
   END RECORD

   -- arma el nombre y ruta del archivo
   LET v_nom_archivo = "htrs",TODAY USING "ddmmyyyy.",p_extension
   LET v_nom_archivo = v_nom_archivo.trimRight()
   LET v_nom_archivo = v_nom_archivo.trimLeft()

   --Obtiene el valor de la ruta de envio
   LET v_s_sql="SELECT ruta_envio FROM seg_modulo \n ",
               "WHERE modulo_cod='agr'"
               
   PREPARE stm_ruta_envio FROM v_s_sql
   EXECUTE stm_ruta_envio INTO v_c_ruta_envio

   -- concatena la ruta y nombre del archivo
   LET v_c_ruta_envio = v_c_ruta_envio CLIPPED
   LET v_ruta_archivo = v_c_ruta_envio,"/",v_nom_archivo

   -- creación del canal del archivo
   LET v_ch_archivo = base.channel.Create()
   CALL v_ch_archivo.openFile(v_ruta_archivo,"w")
   CALL v_ch_archivo.setDelimiter(NULL)

   -- comienza con el armado del query
   LET v_s_sql = "SELECT t.nss, \n",
                       " t.num_credito , \n",
                       " t.tpo_credito , \n",
                       " t.edo_credito , \n",
                       " t.num_credito1, \n",
                       " t.tpo_credito1, \n",
                       " t.edo_credito1 \n",
                 "  FROM safre_tmp:tmp_reg_homologa t \n",
                 " WHERE t.edo_homologa = ", p_edo_homologa 

   -- preparación del statement
   PREPARE stm_marca_rechazo FROM v_s_sql 
   DECLARE cur_marca_rechazo CURSOR FOR stm_marca_rechazo

   -- recorrido del arreglo
   FOREACH cur_marca_rechazo INTO r_arh_qry.*

   --formatea la salida del archivo
      LET r_archivo.nss          = r_arh_qry.nss
      LET r_archivo.num_credito  = r_arh_qry.num_credito  USING "&&&&&&&&&&"
      LET r_archivo.tpo_credito  = r_arh_qry.tpo_credito  USING "&&&"
      LET r_archivo.edo_credito  = r_arh_qry.edo_credito  USING "&&&"
      LET r_archivo.num_credito1 = r_arh_qry.num_credito1 USING "&&&&&&&&&&"
      LET r_archivo.tpo_credito1 = r_arh_qry.tpo_credito1 USING "&&&"
      LET r_archivo.edo_credito1 = r_arh_qry.edo_credito1 USING "&&&"

     --Concatena línea
     LET v_s_linea = r_archivo.nss        ,
                     r_archivo.num_credito,
                     r_archivo.tpo_credito,
                     r_archivo.edo_credito,
                     r_archivo.num_credito1,
                     r_archivo.tpo_credito1,
                     r_archivo.edo_credito1

      --Escribe en el archivo
      CALL v_ch_archivo.write([v_s_linea])

   END FOREACH

   -- cierra el archivo
   CALL v_ch_archivo.close()

   DISPLAY "Se genera archivo: ", v_nom_archivo CLIPPED
   
   -- regresa la ruta completa del archivo
   RETURN v_ruta_archivo 

END FUNCTION

#Objetivo: Función que genera el reporte de Integración de homologación
FUNCTION f_genera_rpt_Homologa(p_v_nom_reporte, p_r_cre_ctr_archivo, p_ax_estado, p_ax_tipo, p_ax_numero, p_ax_tp_edo)

   DEFINE p_v_nom_reporte   VARCHAR(80) -- nombre del reporte

   DEFINE v_r_rpt_res RECORD -- registro de resumen
      nom_archivo    LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
      fecha_hr_ini   LIKE bat_ctr_operacion.fecha_ini, -- fecha inicial del proceso
      fecha_hr_fin   LIKE bat_ctr_operacion.fecha_fin, -- fecha final de la operacion
      id_operacion   LIKE cre_ctr_archivo.operacion, -- operacion
      desc_operacion LIKE cat_operacion_prc.desc_operacion, -- descripción de la operación
      usuario        LIKE bat_ctr_operacion.usuario, -- nombre del usuario
      tot_registros  INTEGER, -- número total de registros
      tot_aceptados  INTEGER, -- número total de regs mismo crédito
      tot_procesados INTEGER, -- número total de regs procesados
      tot_rechazados INTEGER, -- número total de regs rechazados
      tot_sin_origen INTEGER  -- número total de regs sin origen
   END RECORD

   DEFINE v_r_reporte RECORD -- registro de detalle
      estado         SMALLINT,
      desc_edo       CHAR(20),
      edo_homologa   SMALLINT,
      desc_edo_hom   CHAR(30)
   END RECORD

   DEFINE p_r_cre_ctr_archivo RECORD
      tot_registros         LIKE cre_ctr_archivo.tot_registros, -- total de registros
      tot_aceptados         LIKE cre_ctr_archivo.tot_aceptados, -- total aceptados 
      tot_procesados        LIKE cre_ctr_archivo.tot_aceptados, -- total procesados
      tot_rechazados        LIKE cre_ctr_archivo.tot_rechazados, -- total rechazados
      tot_sin_origen        LIKE cre_ctr_archivo.tot_sin_origen -- total sin origen
   END RECORD

   DEFINE p_ax_estado              INTEGER;   -- total diferente estado
   DEFINE p_ax_tipo                INTEGER;   -- total diferente tipo
   DEFINE p_ax_numero              INTEGER;   -- total diferente número
   DEFINE p_ax_tp_edo              INTEGER;   -- total diferente tipo y estado

   DEFINE v_r_bat_ctr_opera     RECORD LIKE bat_ctr_operacion.* -- registro de bat ctr operación
   DEFINE v_i_folio_format      INTEGER -- número de folio con formato
   DEFINE v_manejador_rpt       OM.SaxDocumentHandler # Contenedor de Documentos para el reporte
   DEFINE v_s_qryTxt            STRING -- contiene una sentencia sql a ejecutar
   DEFINE v_si_existe_err       SMALLINT -- booleana que indica si existe un error durante la creación del reporte

   -- se inicializan variables
   LET v_si_existe_err = FALSE -- se asume que no ocurrirá ningun error

   -- se indica que el reporte usará la plantilla creada
   IF fgl_report_loadCurrentSettings("AGRE151.4rp") THEN
      -- se indica la salida del reporte
      CALL fgl_report_setOutputFileName(g_c_ruta_listados CLIPPED||"/"||p_v_nom_reporte)

      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración en el menejador del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY " ERROR: No fue posible abrir plantilla del reporte"
      -- se indica que ha ocurrido un error
      LET v_si_existe_err = true

      RETURN v_si_existe_err
   END IF

   -- se crea la sentencia sql que busca la información de la operación
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE proceso_cod = ",g_i_proceso_cod,"\n",
                    "    AND opera_cod = ",g_i_opera_cod,"\n",
                    "    AND folio = ",g_d_folio

   PREPARE prp_bat_ctr_opera FROM v_s_qryTxt
   EXECUTE prp_bat_ctr_opera INTO v_r_bat_ctr_opera.*

   -- se asignan los valores del registro del reporte
   LET v_r_rpt_res.nom_archivo    = v_r_bat_ctr_opera.nom_archivo
   LET v_r_rpt_res.fecha_hr_ini   = v_r_bat_ctr_opera.fecha_ini
   LET v_r_rpt_res.fecha_hr_fin   = v_r_bat_ctr_opera.fecha_fin
   LET v_r_rpt_res.id_operacion   = 31
   LET v_r_rpt_res.desc_operacion = fn_obt_desc_operacion(31)
   LET v_r_rpt_res.usuario        = v_r_bat_ctr_opera.usuario
   LET v_r_rpt_res.tot_registros  = p_r_cre_ctr_archivo.tot_registros
   LET v_r_rpt_res.tot_aceptados  = p_r_cre_ctr_archivo.tot_aceptados
   LET v_r_rpt_res.tot_procesados = p_r_cre_ctr_archivo.tot_procesados
   LET v_r_rpt_res.tot_rechazados = p_r_cre_ctr_archivo.tot_rechazados
   LET v_r_rpt_res.tot_sin_origen = p_r_cre_ctr_archivo.tot_sin_origen

   -- se le da formato al folio
   LET v_i_folio_format = g_d_folio --USING "&&&&&&&&&&"

   -- inicia el reporte de registros con rechazo
   START REPORT reporte_integ_homologa TO XML HANDLER v_manejador_rpt

   -- Se busca el detalle de las marcas
   LET v_s_qryTxt = " SELECT c.edo_credito, \n",
                          " DECODE (c.edo_credito,1,'CRÉDITO VIGENTE',2,'CRÉDITO LIQUIDADO','CREDITO CANCELADO'),\n",
                          " c.edo_homologa,\n",
                          " count(*)\n",
                    "   FROM cre_homologa_trm c\n",
                    "  WHERE c.id_cre_ctr_archivo = ",g_id_cre_ctr_archivo,"\n",
                    " GROUP BY c.edo_credito, c.edo_homologa\n",
                    " ORDER BY c.edo_credito"

---DISPLAY v_s_qryTxt

   PREPARE prp_regs FROM v_s_qryTxt
   DECLARE cur_regs CURSOR FOR prp_regs

   FOREACH cur_regs INTO v_r_reporte.estado, v_r_reporte.desc_edo, v_r_reporte.edo_homologa
      CASE v_r_reporte.edo_homologa
         WHEN 11
            LET v_r_reporte.desc_edo_hom = "NSS NO CATALOGADOS"
         WHEN 14
            LET v_r_reporte.desc_edo_hom = "NSS SIN ORIGINACIÓN O CREDITO DIFERENTE"
         WHEN 15
            LET v_r_reporte.desc_edo_hom = "CRÉDITO CON ESTADO DIFERENTE"
         WHEN 16
            LET v_r_reporte.desc_edo_hom = "CRÉDITO CON TIPO DIFERENTE"
         WHEN 22
            LET v_r_reporte.desc_edo_hom = "CRÉDITO CON NÚMERO DIFERENTE"
         WHEN 26
            LET v_r_reporte.desc_edo_hom = "NSS SIN ORIGINACIÓN"
         WHEN 29
            LET v_r_reporte.desc_edo_hom = "CRÉDITO MISMO NCI CON ESTADO Y TIPO DIFERENTE"
         OTHERWISE
            LET v_r_reporte.desc_edo_hom = "NSS SIN ORIGINACIÓN O CREDITO DIFERENTE"
      END CASE

      -- salida del reporte
      OUTPUT TO REPORT reporte_integ_homologa(v_r_rpt_res.*, v_r_reporte.*, v_i_folio_format, p_ax_estado, p_ax_tipo, p_ax_numero, p_ax_tp_edo)
   END FOREACH

   --OUTPUT TO REPORT reporte_integ_homologa(v_r_rpt_res.*, v_r_reporte.*, v_i_folio_format, p_ax_estado, p_ax_tipo, p_ax_numero, p_ax_tp_edo)

   -- finaliza el reporte
   FINISH REPORT reporte_integ_homologa

   RETURN v_si_existe_err

END FUNCTION

#OBJETIVO: Genera el reporte de Integración de Recurrente
REPORT reporte_integ_homologa(p_r_res, p_r_reporte, p_i_folio, rp_ax_estado, rp_ax_tipo, rp_ax_numero, rp_ax_tp_edo)

   DEFINE p_i_folio                 VARCHAR(10) -- numero de folio con formato

   DEFINE p_r_res RECORD
      nom_archivo                   LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
      fecha_hr_ini                  LIKE bat_ctr_operacion.fecha_ini, -- fecha inicial del proceso
      fecha_hr_fin                  LIKE bat_ctr_operacion.fecha_fin, -- fecha final de la operacion
      id_operacion                  LIKE cre_ctr_archivo.operacion, -- operacion
      desc_operacion                LIKE cat_operacion_prc.desc_operacion, -- descripción de la operación
      usuario                       LIKE bat_ctr_operacion.usuario, -- nombre del usuario
      tot_registros                 INTEGER, -- número total de registros
      tot_aceptados                 INTEGER, -- número total de regs mismo crédito
      tot_procesados                INTEGER, -- número total de regs procesados
      tot_rechazados                INTEGER, -- número total de regs rechazados
      tot_sin_origen                INTEGER  -- número total de regs sin origen
   END RECORD

   DEFINE p_r_reporte RECORD
      estado                        SMALLINT,
      desc_edo                      CHAR(20),
      edo_homologa                  SMALLINT,
      desc_edo_hom                  CHAR(30)
   END RECORD

   DEFINE v_fecha_reporte           DATE

   DEFINE rp_ax_estado              INTEGER;   -- total diferente estado
   DEFINE rp_ax_tipo                INTEGER;   -- total diferente tipo
   DEFINE rp_ax_numero              INTEGER;   -- total diferente número
   DEFINE rp_ax_tp_edo              INTEGER;   -- total diferente tipo y estado

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY

      PRINTX v_fecha_reporte USING "DD-MM-YYYY"
      PRINTX g_v_usuario
      PRINTX p_i_folio
      PRINTX p_r_res.nom_archivo
      PRINTX p_r_res.fecha_hr_ini
      PRINTX p_r_res.fecha_hr_fin
      PRINTX p_r_res.id_operacion
      PRINTX p_r_res.desc_operacion
      PRINTX p_r_res.usuario

   ON EVERY ROW
      PRINTX p_r_res.tot_registros  USING "#########&"
      PRINTX p_r_res.tot_procesados USING "#########&"
      PRINTX p_r_res.tot_rechazados USING "#########&"
      PRINTX p_r_res.tot_sin_origen USING "#########&"

      PRINTX rp_ax_estado           USING "#########&"
      PRINTX rp_ax_tipo             USING "#########&"
      PRINTX rp_ax_numero           USING "#########&"
      PRINTX rp_ax_tp_edo           USING "#########&"

END REPORT

#Objetivo: Busca la descripción de la operación
FUNCTION fn_obt_desc_operacion(p_c_operacion)

   DEFINE p_c_operacion             LIKE cat_operacion_prc.operacion -- operación
   DEFINE v_c_desc_opera            LIKE cat_operacion_prc.desc_operacion -- descripción de la operación
   DEFINE v_s_qryTxt                STRING -- se asigna consulta sql a ejecutar

   -- se consulta la descripción del estado
   LET v_s_qryTxt = " SELECT desc_operacion\n",
                    "   FROM cat_operacion_prc\n",
                    "  WHERE operacion = '",p_c_operacion,"'"

   PREPARE prp_desc_operacion FROM v_s_qryTxt
   EXECUTE prp_desc_operacion INTO v_c_desc_opera

   -- se verifica si se encontró descripción
   IF v_c_desc_opera IS NULL THEN
      LET v_c_desc_opera = "DESCRIPCIÓN NO ENCONTRADA"
   END IF

   RETURN v_c_desc_opera

END FUNCTION
