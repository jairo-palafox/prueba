--===============================================================
-- Version: 1.0.0
-- Fecha última modificación:
--===============================================================

#####################################################################
#Módulo            => AGR                                           #
#Programa          => AGRC10                                        #
#Objetivo          => Programa que realiza la consulta de marcas    #
#                     de WS con Procesar                            #
#Autor             => Mauro Muñiz Caballero                         #
#Fecha creación    => 9 septiembre 2012                             #
#Modificó          => Mauro Muñiz Caballero                         #
#Fecha modifica    => 24 de junio de 2016                           #
#                     Nuevo diagnóstico de registros aceptados      #
#####################################################################

IMPORT os

DATABASE safre_viv

GLOBALS

   DEFINE g_ban_salir               SMALLINT
   DEFINE g_ciclo                   SMALLINT
   DEFINE p_tipo_ejecucion          SMALLINT -- forma como ejecutara el programa
   DEFINE p_s_titulo                STRING -- titulo de la ventana
   DEFINE g_usuario                 CHAR(10)
   DEFINE g_fecha                   DATE

   DEFINE arr_marca DYNAMIC ARRAY OF RECORD
      situacion                     SMALLINT,
      desc_situacion                CHAR(10),
      fecha                         DATE,
      tot_marcas                    SMALLINT
   END RECORD

   DEFINE r_detsol DYNAMIC ARRAY OF RECORD
      solicitud                     CHAR(25),
      total_sol                     INTEGER
   END RECORD

   DEFINE f_w                       ui.form
   DEFINE w                         ui.window

END GLOBALS

MAIN

   -- se recuperan las claves desde parámetro
   LET g_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(g_usuario CLIPPED|| ".AGRC10.log")

   CLOSE WINDOW SCREEN

   -- si se obtuvo el título, se pone como título de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CALL fn_proceso_principal()

END MAIN

FUNCTION fn_proceso_principal()
#fpp---------------------------

   --se inicializan variables
   LET g_ban_salir = FALSE

   OPEN WINDOW w_consulta_marca WITH FORM "AGRC101"
      --LET w = ui.Window.forName("AGRC102")
      LET w = ui.Window.getCurrent()
      LET f_w = w.getForm()

      LET g_ciclo = TRUE
      LET g_fecha = NULL

      WHILE g_ciclo
         CALL fn_cons_fecha() RETURNING g_fecha

         IF g_fecha > '12/31/1899' AND g_fecha IS NOT NULL THEN
            CALL fn_llena_arbol_marcas(g_fecha)
         END IF

         LET g_fecha = '12/31/1899'
      END WHILE

   CLOSE WINDOW w_consulta_marca

END FUNCTION

FUNCTION fn_cons_fecha()
#fcf--------------------

   DEFINE v_fecha                   DATE

   CALL f_w.setElementHidden("group2",1)
   CALL f_w.setElementHidden("group3",1)

   INPUT v_fecha FROM fecha_consulta ATTRIBUTE (UNBUFFERED)
      BEFORE INPUT
         LET v_fecha = TODAY
         DISPLAY v_fecha TO fecha_consulta

         NEXT FIELD fecha_consulta

         ON ACTION ACCEPT
            IF v_fecha IS NULL THEN
               CALL fn_mensaje("Aviso","Se requiere ingresar la fecha de consulta","stop")
               CONTINUE INPUT
            ELSE
               --Valida la existencia de información
               IF fn_verifica_registros(v_fecha) THEN
                  EXIT INPUT
               ELSE
                  CALL fn_mensaje("Aviso","No existe información con la fecha indicada","stop")
                  CONTINUE INPUT
               END IF
            END IF

         ON ACTION CANCEL
            --se canceló y se cierra la ventana de despliegue de registros
            LET g_ciclo     = FALSE
            LET v_fecha     = '12/31/1899'

            EXIT INPUT

      END INPUT

   RETURN v_fecha

END FUNCTION

## Función que verifica si existen registros con base en la fecha indicada ##
FUNCTION fn_verifica_registros(p_fec_marca)

   DEFINE p_fec_marca               DATE
   DEFINE qry_string                STRING
   DEFINE v_s_existe_info           SMALLINT
   DEFINE v_f_marca                 DATE
   DEFINE v_situacion               SMALLINT
   DEFINE v_tot_marca               INTEGER

   --Inicializa la variable de existencia de información
   LET v_s_existe_info = FALSE

   SELECT UNIQUE id_derechohabiente, situacion, f_actualiza
     FROM cta_his_marca_ws
    WHERE f_actualiza = p_fec_marca
   INTO TEMP reg_marcas

   --Armado del query que obtiene el detalle de la consulta para verificar si existen registros
   LET qry_string = " SELECT UNIQUE s.situacion, s.f_actualiza, COUNT(*)\n ",
                    "   FROM reg_marcas s \n",
                    "  WHERE s.f_actualiza = '",p_fec_marca,"'\n",
                    " GROUP BY 1,2 \n",
                    " ORDER BY 1 desc, 2 DESC "

      --Preparación del statement
      PREPARE prp_cnt_marcas FROM qry_string
      DECLARE cur_cnt_marcas CURSOR FOR prp_cnt_marcas
      
      FOREACH cur_cnt_marcas INTO v_situacion,v_f_marca,v_tot_marca
         --Si existe al menos un registro se envía señal que si existen registros
         LET v_s_existe_info = TRUE 
      END FOREACH

  --Regresa valor recién obtenido
  RETURN v_s_existe_info

END FUNCTION

## Función que llena el árbol de marcas a desplegar ##
FUNCTION fn_llena_arbol_marcas(p_fec_marca)
#flam--------------------------------------

   DEFINE v_tot_marca               INTEGER
   DEFINE v_sum_marca               INTEGER

   DEFINE i                         SMALLINT
   DEFINE v_situacion               SMALLINT
   DEFINE v_pos                     SMALLINT
   DEFINE resp_visualiza            SMALLINT

   DEFINE p_fec_marca               DATE
   DEFINE v_f_marca                 DATE
   DEFINE v_f_situacion             DATE

   DEFINE qry_string                STRING

   --DISPLAY "consulta crédito", qry_string

   LET i           = 1

    --se limpia el arreglo que se va a desplegar
      LET qry_string = " SELECT UNIQUE s.situacion, s.f_actualiza, COUNT(*)\n ",
                       "   FROM reg_marcas s \n",
                       "  WHERE s.f_actualiza = '",p_fec_marca,"'\n",
                       " GROUP BY 1,2 \n",
                       " ORDER BY 1 desc, 2 DESC "

      --DISPLAY "MARCAS ", qry_string

      PREPARE prp_marcas FROM qry_string
      DECLARE cur_marcas CURSOR FOR prp_marcas

      FOREACH cur_marcas INTO v_situacion, v_f_marca, v_tot_marca
         LET arr_marca[i].situacion      = v_situacion
         LET arr_marca[i].desc_situacion = fn_desc_situacion(v_situacion)
         LET arr_marca[i].fecha          = v_f_marca
         LET arr_marca[i].tot_marcas     = v_tot_marca

         LET v_sum_marca = v_sum_marca + v_tot_marca
         LET i           = i + 1
      END FOREACH

   CLOSE cur_marcas
   FREE cur_marcas

   LET i = i - 1

   CALL f_w.setElementHidden("group2",0)
   CALL f_w.setElementHidden("group3",0)

   DIALOG ATTRIBUTES(UNBUFFERED)

   DISPLAY ARRAY arr_marca TO scr1.*

      BEFORE DISPLAY
         CALL DIALOG.setactionhidden("close",1)

      BEFORE ROW
         LET v_pos = ARR_CURR()

            IF v_pos > 0 THEN
               IF arr_marca[v_pos].situacion = 2 THEN
                  LET v_situacion = 2
               ELSE
                  LET v_situacion = 0
               END IF

               LET v_f_situacion = arr_marca[v_pos].fecha

               CALL fn_despliega_detalle_sol(v_situacion, v_f_situacion)
                    RETURNING resp_visualiza

               IF NOT resp_visualiza THEN
                  CALL r_detsol.clear()
               ELSE
                  CALL r_detsol.deleteelement(r_detsol.getlength())
               END IF
            ELSE
               CALL r_detsol.clear()
            END IF

      ON ACTION ACCEPT
         EXIT DIALOG

      ON ACTION CANCEL
         EXIT DIALOG

      --Invoca la generación del archivo   
      ON ACTION ARCHIVO
        CALL fn_genera_archivo_marcas(p_fec_marca)
        CONTINUE DIALOG

      --Invoca la generación de reporte
      ON ACTION REPORTE
         CALL fn_genera_reporte_marcas()
         CONTINUE DIALOG

   END DISPLAY

   DISPLAY ARRAY r_detsol TO scr2.*

   END DISPLAY

   END DIALOG

END FUNCTION

# Función que obtiene la descripción de la situación
FUNCTION fn_desc_situacion(p_situacion)

   DEFINE p_situacion               SMALLINT
   DEFINE v_desc_situacion          CHAR(8)

   IF p_situacion = 2 THEN
      LET v_desc_situacion = 'MARCA'
   ELSE
      LET v_desc_situacion = 'DESMARCA'
   END IF

   RETURN  v_desc_situacion

END FUNCTION

FUNCTION fn_despliega_detalle_sol(p_situacion, p_fecha)
#fdds--------------------------------------------------

   DEFINE p_situacion               SMALLINT
   DEFINE i                         SMALLINT
   DEFINE v_tot                     DECIMAL(6,0)
   DEFINE v_sol                     DECIMAL(6,0)

   DEFINE p_fecha                   DATE

   CALL r_detsol.clear()

   LET i = 1
   LET v_sol = 0
   LET v_tot = 0

      ---Solicitudes aceptadas
   SELECT COUNT(UNIQUE id_derechohabiente)
     INTO v_sol
     FROM cta_his_marca_ws ha
    WHERE ha.situacion = p_situacion
      AND ha.f_actualiza = p_fecha
      AND ha.diagnostico IN('1','01','001','')

   LET r_detsol[i].solicitud = "ACEPTADAS"
   LET r_detsol[i].total_sol = v_sol
   LET v_tot = v_tot + v_sol

   LET i = i + 1
   LET v_sol = 0

   ---Rechazadas actual
   SELECT COUNT(UNIQUE id_derechohabiente)
     INTO v_sol
     FROM cta_his_marca_ws ra
    WHERE ra.situacion = p_situacion
      AND ra.f_actualiza = p_fecha
      AND diagnostico NOT IN('1','01','001','')

   LET r_detsol[i].solicitud = "RECHAZADAS ACTUAL"
   LET r_detsol[i].total_sol = v_sol
   LET v_tot = v_tot + v_sol

   LET i = i + 1
   LET v_sol = 0

   ---Enviadas

   LET r_detsol[i].solicitud = "ENVIADAS"
   LET r_detsol[i].total_sol = v_tot

   LET i = i + 1
   LET v_sol = 0

   ---Rechazadas anteriores
   SELECT COUNT(*)
     INTO v_sol
     FROM cta_marca_ws rh
    WHERE rh.situacion = p_situacion
      AND rh.f_solicita < p_fecha
      AND rh.diagnostico IS NOT NULL

   LET r_detsol[i].solicitud = "RECHAZADAS ANTERIORES"
   LET r_detsol[i].total_sol = v_sol
   LET v_tot = v_tot + v_sol

   LET i = i + 1
   LET v_sol = 0

   ---Por enviar
   SELECT COUNT(*)
     INTO v_sol
     FROM cta_marca_ws ev
    WHERE ev.f_solicita = p_fecha
      AND ev.situacion = p_situacion
      AND ev.diagnostico IS NULL

   LET r_detsol[i].solicitud = "POR ENVIAR"
   LET r_detsol[i].total_sol = v_sol
   LET v_tot = v_tot + v_sol

   IF v_tot = 0 THEN
      RETURN 0
   ELSE
      RETURN 1
   END IF

END FUNCTION

#Función que genera el archivo de marcas
FUNCTION fn_genera_archivo_marcas(p_fecha)

   DEFINE p_fecha                   DATE  ---Fecha de solicitud
   DEFINE v_s_qry                   STRING --String para armar el query
   DEFINE v_c_ruta_envio            LIKE seg_modulo.ruta_envio --Ruta envío del archivo
   DEFINE v_c_nom_archivo           CHAR(30)
   DEFINE v_c_ruta_escritura        STRING --Ruta donde escribirá el archivo
   DEFINE v_c_fec_archivo           CHAR(8)  --Fecha del nombre de archivo
   DEFINE v_s_detalle               STRING --Detalle del archivo
   DEFINE v_ch_archivo              base.channel --Canal para escritura del archivo
   DEFINE v_s_existe_info           SMALLINT  --Bandera para indicar si existe información

   DEFINE v_c_id_dh                 DECIMAL(9,0) -- id_derechohabiente
   DEFINE v_c_nss                   CHAR (11) --Número de seguridad social
   DEFINE v_c_num_credito           CHAR (10) --Número de crédito para la consulta de cta_marca_ws
   DEFINE v_c_tpo_credito           CHAR (2) --Tipo de crédito de la tabla cta_marca_ws
   DEFINE v_c_rechazo               CHAR (3) --Motivo de rechazo de acuerdo a código de PROCESAR
   DEFINE v_c_situacion             CHAR (1) --Situación del registro 
   DEFINE v_c_accion                CHAR (1) --Acción de acuerdo con situación

   --Inicializa bandera
   LET v_s_existe_info = FALSE

   --Se obtiene la ruta de envío del archivo
   SELECT ruta_envio 
     INTO v_c_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'agr'
   
   --Se asgna la fecha de solicitud
   LET  v_c_fec_archivo = p_fecha USING "ddmmyyyy"
   
   --Se asigna el nombre del archivo
   LET v_c_nom_archivo = "/Respmarcajeprc",v_c_fec_archivo,".rws"
   
   --Concatenación de la ruta donde escribirá el archivo
   LET v_c_ruta_escritura= v_c_ruta_envio CLIPPED || v_c_nom_archivo
     
   --Se abre canal de escritura
   LET v_ch_archivo= base.Channel.create()
   CALL v_ch_archivo.openFile(v_c_ruta_escritura,"w")

   DECLARE cur_rech CURSOR FOR
   SELECT id_derechohabiente
     FROM reg_marcas
   ORDER BY situacion DESC

   FOREACH cur_rech INTO v_c_id_dh
      --Se arma el query de rechazados
      LET v_s_qry=  " SELECT FIRST 1 a.nss, \n",
                    "        c.num_credito, \n",
                    "        c.tpo_credito, \n",
                    "        c.diagnostico, \n",
                    "        c.situacion    \n",
                    "   FROM cta_his_marca_ws c, \n",
                    "        afi_derechohabiente a  \n",
                    "  WHERE a.id_derechohabiente = ",v_c_id_dh," \n",
                    "    AND c.id_derechohabiente = a.id_derechohabiente \n ",
                    "    AND c.f_actualiza = '",p_fecha,"' \n",
                    "    AND c.situacion IN(0,2) \n",
                    "    AND c.diagnostico NOT IN('1','01','001','') \n",
                    " ORDER BY c.situacion DESC, c.intento DESC, a.nss "

      --Preparado del statement
      PREPARE stm_marca FROM v_s_qry
      DECLARE cur_marca CURSOR FOR stm_marca

      --Iteración de los resultados
      FOREACH cur_marca INTO v_c_nss,
                             v_c_num_credito,
                             v_c_tpo_credito,
                             v_c_rechazo,    
                             v_c_situacion  

         --Se asigna la acción correspondiente
         IF v_c_situacion = 2 THEN
            --Marca
            LET v_c_accion='M'
         END IF

         IF v_c_situacion = 0 THEN
            --Desmarca   
            LET v_c_accion = 'D'
         END IF

         --Se formatea el tipo y el número
         LET v_c_tpo_credito=v_c_tpo_credito USING "&&"
         LET v_c_num_credito=v_c_num_credito USING "&&&&&&&&&&"

         --Se concatena el detalle
         LET v_s_detalle = v_c_nss,
                           v_c_num_credito,
                           v_c_tpo_credito,
                           v_c_rechazo,
                           v_c_accion

         --Escribe la cadena formada en el archivo
         CALL v_ch_archivo.writeLine(v_s_detalle)

         --Actualiza bandera de existencia
         LET v_s_existe_info = TRUE
      END FOREACH
   END FOREACH

  --Se arma el query de aceptados
  LET v_s_qry= " SELECT UNIQUE a.nss,  \n",
               "       c.num_credito,  \n",
               "       c.tpo_credito,  \n",
               "       c.diagnostico,  \n",
               "       c.situacion     \n",
               " FROM cta_his_marca_ws c,     \n",
               "       afi_derechohabiente a  \n",
               " WHERE c.f_actualiza = '",p_fecha,"' \n",
               "   AND c.situacion IN(0,2)    \n",
               "   AND c.diagnostico IN('1','01','001','') \n",
               "   AND c.id_derechohabiente = a.id_derechohabiente \n",
               " ORDER BY c.situacion DESC, a.nss"

   #debug#
   --DISPLAY "Query aceptados ",v_s_qry
   #END DEBUG#

   --Se prepara el statement
   PREPARE stm_aceptados FROM v_s_qry
   DECLARE cur_aceptados CURSOR FOR stm_aceptados

      --Recorrido del cursor
      FOREACH cur_aceptados INTO v_c_nss,
                                 v_c_num_credito,
                                 v_c_tpo_credito,
                                 v_c_rechazo,    
                                 v_c_situacion  

         --Se asigna la acción correspondiente
         IF v_c_situacion = 2 THEN
            --Marca
            LET v_c_accion='M'
         END IF

         IF v_c_situacion = 0 THEN
            --Desmarca   
            LET v_c_accion = 'D'
         END IF

         --Se formatea el tipo y el número
         LET v_c_tpo_credito = v_c_tpo_credito USING "&&"
         LET v_c_num_credito = v_c_num_credito USING "&&&&&&&&&&"

         --Se concatena el detalle
         LET v_s_detalle = v_c_nss,
                           v_c_num_credito,
                           v_c_tpo_credito,
                           v_c_rechazo,
                           v_c_accion

         --Escribe la cadena formada en el archivo
         CALL v_ch_archivo.writeLine(v_s_detalle)

         --Actualiza bandera de existencia
         LET v_s_existe_info = TRUE
   END FOREACH

   --Concluye la escritura del archivo
   CALL v_ch_archivo.close()

   IF v_s_existe_info THEN
       --Envía mensaje indicando que se acaba de crear un archivo 
       CALL fn_mensaje("Información","Se ha generado el archivo de Consulta de Solicitud de Marcaje a Procesar vía WS\n en la ruta"||v_c_ruta_escritura,"information")                     
   ELSE
      CALL fn_mensaje("Aviso","No existe información para generar el archivo","stop")
   END IF

END FUNCTION

## Funcion que genera el reporte de marcas##
FUNCTION fn_genera_reporte_marcas()

   DEFINE manejador_rpt             om.SaxDocumentHandler --Contenedor documentos reporte 
   DEFINE v_i_indice_situacion      SMALLINT
   DEFINE v_i_indice_solicitud      SMALLINT
   DEFINE v_i_resp_detalle          SMALLINT

   DEFINE r_det_envio RECORD 
      v_s_situacion                 SMALLINT,
      v_c_desc_situacion            CHAR(10),
      v_d_fecha                     DATE,
      v_c_tot_marcas                SMALLINT,
      v_c_solicitud                 CHAR(25),
      v_i_total_sol                 INTEGER
   END RECORD 

   --Se asigna el manejador del reporte
   IF fgl_report_loadCurrentSettings("AGRC101.4rp") THEN 
       CALL fgl_report_selectDevice ("PDF")
       LET manejador_rpt = fgl_report_commitCurrentSettings()
    END IF

   --Inicializa el reporte
   START REPORT rpt_situacion TO XML HANDLER manejador_rpt

   --Recorrido del arreglo de marcas
   FOR v_i_indice_situacion = 1 TO arr_marca.getLength()

      --Asigna valores
      --Valida que no sea nula la situación
      #DEBUG#
      --DISPLAY "Situación", arr_marca[v_i_indice_situacion].situacion 
      #END DEBUG#

      IF arr_marca[v_i_indice_situacion].situacion  IS NULL THEN 
         LET r_det_envio.v_s_situacion = 0
      ELSE
         LET r_det_envio.v_s_situacion     = arr_marca[v_i_indice_situacion].situacion
      END IF

      LET r_det_envio.v_c_desc_situacion= arr_marca[v_i_indice_situacion].desc_situacion
      LET r_det_envio.v_d_fecha         = arr_marca[v_i_indice_situacion].fecha
      LET r_det_envio.v_c_tot_marcas    = arr_marca[v_i_indice_situacion].tot_marcas

      --Se invoca la función que obtiene la situacion
      CALL  fn_despliega_detalle_sol(r_det_envio.v_s_situacion,r_det_envio.v_d_fecha)
      RETURNING v_i_resp_detalle

      --Se recorre el resultado obtenido
      FOR v_i_indice_solicitud = 1 TO r_detsol.getLength()
         --Se asigna el valor de la solicitud
         LET r_det_envio.v_c_solicitud = r_detsol[v_i_indice_solicitud].solicitud

         --Valida 0
         IF r_detsol[v_i_indice_solicitud].total_sol IS NULL THEN
            LET r_det_envio.v_i_total_sol = 0
         ELSE
            LET r_det_envio.v_i_total_sol = r_detsol[v_i_indice_solicitud].total_sol
         END IF 

         --Se envía el registro al reporte
         OUTPUT TO REPORT rpt_situacion (r_det_envio.*)
      END FOR
   END FOR

   --Finaliza la elaboración del reporte
   FINISH REPORT rpt_situacion

END FUNCTION 

## Reporte de marcas##
REPORT rpt_situacion (r_det_reporte)

   DEFINE v_d_fecha                 CHAR(10) 

   DEFINE r_det_reporte RECORD 
      v_s_situacion                 CHAR(1),
      v_c_desc_situacion            CHAR(10),
      v_d_fecha                     DATE,
      v_c_tot_marcas                SMALLINT,
      v_c_solicitud                 CHAR(25),
      v_i_total_sol                 CHAR(20)
   END RECORD

   ORDER BY r_det_reporte.v_s_situacion

   FORMAT

   FIRST PAGE HEADER
      --Asigna fecha
      LET v_d_fecha = TODAY USING "DD-MM-YYYY"

      PRINTX g_usuario
      PRINTX v_d_fecha

      BEFORE GROUP OF r_det_reporte.v_s_situacion

      PRINTX r_det_reporte.v_s_situacion
      PRINTX r_det_reporte.v_c_desc_situacion
      PRINTX r_det_reporte.v_d_fecha
      PRINTX r_det_reporte.v_c_tot_marcas

  ON EVERY ROW

    PRINTX r_det_reporte.v_c_solicitud
    PRINTX r_det_reporte.v_i_total_sol

END REPORT