##########################################################################
#Modulo            => AGR                                                #
#Programa          => AGRL75                                             #
#Objetivo          => Transacción para carga de archivo y actualización  #
#                     de remanentes para solicitud de saldo a Procesar   #
#Autor             => Emilio Abarca, EFP                                 #
#Fecha inicio      => 09/Octubre/2018                                    #
##########################################################################

DATABASE safre_viv

GLOBALS 
   DEFINE g_usuario           CHAR(20)
   DEFINE g_titulo            STRING
   DEFINE g_tipo_ejecucion    SMALLINT
   DEFINE v_ruta_rescate      CHAR(40)
   DEFINE v_ruta_binaria      CHAR(40)
   DEFINE v_ruta_envio        CHAR(40)
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE g_proceso_cod       SMALLINT  
   DEFINE g_opera_cod         SMALLINT
   DEFINE g_pid               DECIMAL(9,0)

   -- variables para el reporte
   DEFINE v_f_ini_opera   LIKE bat_ctr_operacion.fecha_ini
   DEFINE v_f_fin_opera   LIKE bat_ctr_operacion.fecha_fin

   TYPE rec_total        RECORD
      total          INTEGER,
      aivs92         DECIMAL(18,6),
      aivs97         DECIMAL(18,6),
      porcentaje     CHAR(12)
   END RECORD

   DEFINE r_t_global     rec_total
   -- record marca 1
   DEFINE r_t_marca1     rec_total
   DEFINE r_t_marca1_r   rec_total
   DEFINE r_t_marca1_a   rec_total
   DEFINE r_t_marca1_d   rec_total
   DEFINE r_t_marca1_n   rec_total
   -- Record marca 4
   DEFINE r_t_marca4     rec_total
   DEFINE r_t_marca4_r   rec_total
   DEFINE r_t_marca4_a   rec_total
   DEFINE r_t_marca4_d   rec_total
   DEFINE r_t_marca4_n   rec_total
   -- Detalle de rechazos
   DEFINE arr_det_rch      DYNAMIC ARRAY OF RECORD
      tpo_originacion   SMALLINT,
      marca_desc        CHAR(25),
      t_registros       INTEGER,
      diagnostico       CHAR(3),
      diag_desc         CHAR(100)
   END RECORD

   -- Detalle devoluciones
   DEFINE arr_det_dev      DYNAMIC ARRAY OF RECORD
      tpo_originacion   SMALLINT,
      marca_desc        CHAR(25),
      t_registros       INTEGER,
      diagnostico       CHAR(3),
      diag_desc         CHAR(100)
   END RECORD

   --Variables archivo de salida
   DEFINE v_ruta_salida   STRING

END GLOBALS 

MAIN 

   LET g_usuario          = ARG_VAL  (1)
   LET g_tipo_ejecucion   = ARG_VAL  (2)
   LET g_titulo           = ARG_VAL  (3)
   LET g_proceso_cod      = 351   -- Carga solicitud remanente
   LET g_opera_cod        = 1     -- Carga archivo remanente

   IF (g_titulo IS NOT NULL) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

   CALL STARTLOG(g_usuario CLIPPED|| ".AGRL75.log")

   CLOSE WINDOW SCREEN 

   SELECT ruta_bin,
          ruta_rescate,
          ruta_envio
     INTO v_ruta_binaria,
          v_ruta_rescate,
          v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'agr'


   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

   CALL carga_archivo()
   
END MAIN 

FUNCTION carga_archivo()

   DEFINE v_archivo      STRING
   DEFINE v_aux_archivo  CHAR(40)
   DEFINE buf            base.StringBuffer
   DEFINE v_long_arh     INTEGER 
   DEFINE v_pos_ext      SMALLINT 
   DEFINE v_extension    STRING 
   DEFINE v_pos_arh      INTEGER
   DEFINE v_ruta_archivo STRING 
   DEFINE bnd_transfer   SMALLINT 
   DEFINE v_comando      STRING
   DEFINE v_bnd          SMALLINT
   DEFINE v_mensaje      STRING
   DEFINE v_id_archivo   DECIMAL(9,0)
   
   OPEN WINDOW vtn1 WITH FORM "AGRL751"
      INPUT BY NAME v_archivo ATTRIBUTE(UNBUFFERED)

         ON ACTION ACCEPT 
            LET bnd_transfer = 0  -- El archivo no ha transferido al server
            IF(v_archivo IS NULL) THEN 
                CALL fn_mensaje("","Selecciona el archivo a cargar","about")
               NEXT FIELD v_archivo
            END IF 

            -- Verifica si hay un espacio en el nombre del archivo
            IF (v_archivo.getIndexOf(" ",1)) THEN
               CALL fn_mensaje("","El archivo no debe contener espacios","")
               LET v_archivo = NULL 
               NEXT FIELD v_archivo 
            END IF

            IF(v_archivo IS NOT NULL) THEN
               -- valida extensión del archivo
               LET buf = base.StringBuffer.create()   # Se crea objeto StringBuffer 
               CALL buf.append(v_archivo)

               -- Longitud del archivo
               LET v_long_arh = LENGTH(v_archivo)

               -- Posición donde se encuentra el '.' del archivo
               LET v_pos_ext = buf.getIndexOf(".",1)
               LET v_pos_ext   = v_pos_ext + 1                       # Incrementamos 1 para obtener solo el nombre de la extensión
               LET v_extension = buf.subString(v_pos_ext,v_long_arh) # Obtiene nombre de la extensión
               LET v_pos_arh   = buf.getIndexOf("C:",1)
               
               -- Obtiene sólo el nombre del archivo, sin la ruta
               IF(v_pos_arh >= 1) THEN
                  LET v_archivo = buf.subString(13,v_long_arh) 
               END IF 

               IF(v_extension <> "srac") THEN
                  CALL fn_mensaje ("","La extensión del archivo no es correcta.\nEl archivo debe tener extensión '.srac'","")
                  LET v_archivo = NULL 
                  NEXT FIELD v_archivo
               ELSE
                  LET v_ruta_archivo = v_ruta_rescate CLIPPED,"/",v_archivo

                  LET v_aux_archivo = v_archivo CLIPPED

                  -- Verifica si se ha cargado el archivo
                  SELECT id_cre_ctr_archivo
                    INTO v_id_archivo
                    FROM cre_ctr_archivo
                   WHERE id_proceso  = 351
                     AND operacion   = 1
                     AND nom_archivo = v_aux_archivo;

                  IF(v_id_archivo IS NOT NULL) THEN
                     CALL fn_mensaje("","No se puede continuar,el archivo ya ha sido cargado","")
                     LET v_archivo = NULL
                     NEXT FIELD v_archivo
                  END IF

                  --*************************************************************************
                  --Se recupera el archivo cargado y se deja en la ruta rescate del servidor
                  --para trabajarlo desde ahí.
                  --*************************************************************************
                  TRY
                     CALL FGL_GETFILE(v_archivo,v_ruta_archivo)
                     LET bnd_transfer = 1 --Archivo transferido correctamente
                  CATCH
                     CALL fn_mensaje("","No se pudo realizar la carga del archivo","")
                     CONTINUE INPUT
                  END TRY 

                  IF(bnd_transfer = 1) THEN
                     #Inicializa el proceso
                     -- Genera PID
                     LET g_pid = fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario)

                     -- Inicializa proceso
                     CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,"","AGRP53",v_archivo,g_usuario) RETURNING v_bnd

                     -- Actualiza operación como iniciada
                     CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"AGRP53",v_archivo,g_usuario) RETURNING v_bnd

                     -- Ejecuta lanzado
                     LET v_comando = "nohup fglrun ",v_ruta_binaria CLIPPED,"/AGRP53 ",
                                      g_usuario," ",
                                      g_pid," ",
                                      g_proceso_cod," ",
                                      g_opera_cod," ",
                                      v_archivo," ",
                                      v_ruta_archivo," ",
                                      " ' ' 1>",
                                      v_ruta_listados CLIPPED,
                                      "/nohup:",g_pid USING "&&&&&",":",
                                      g_proceso_cod   USING "&&&&&",":",
                                      g_opera_cod     USING "&&&&&" ," 2>&1 &"

                     RUN v_comando

                     LET v_mensaje = "Se ejecutó el proceso de carga solicitud remanente"," \n",
                                     "Verificar en el monitor de procesos el PID: ",g_pid USING "<<<<<<<<<"

                     CALL fn_mensaje("",v_mensaje,"")
                     EXIT INPUT
                  ELSE 
                     CALL fn_mensaje("","El archivo no pudo ser transferido","") 
                  END IF 
               END IF 
            END IF

         ON ACTION Reporte
            CALL fn_rpt_resp_procesar()
            
         ON ACTION CANCEL 
            EXIT INPUT 
            
      END INPUT 
   CLOSE WINDOW vtn1
END FUNCTION

FUNCTION fn_rpt_resp_procesar()

   DEFINE arr_archivos     DYNAMIC ARRAY OF RECORD
      id_cre_ctr_archivo DECIMAL(9,0),
      nom_archivo        CHAR(40),
      tot_aceptados      DECIMAL(10,0),
      f_proceso          DATE,
      activa             SMALLINT
   END RECORD
   DEFINE v_k              INTEGER
   DEFINE v_t_activos      INTEGER
   DEFINE v_mensaje        STRING
   DEFINE r_aux_archivo    RECORD
      id_cre_ctr_archivo DECIMAL(9,0),
      nom_archivo        CHAR(40),
      tot_aceptados      DECIMAL(10,0),
      f_proceso          DATE
   END RECORD

   OPEN WINDOW vtn2 WITH FORM "AGRL752"
      INPUT ARRAY arr_archivos FROM record1.* ATTRIBUTE(WITHOUT DEFAULTS,APPEND ROW = FALSE,
                                                      DELETE ROW = FALSE,INSERT ROW = FALSE,UNBUFFERED)

         BEFORE INPUT
            -- Llena arreglo
            CALL arr_archivos.clear()

            DECLARE crs_archivos_rema CURSOR FOR
            SELECT id_cre_ctr_archivo,
                   nom_archivo       ,
                   tot_aceptados     ,
                   f_proceso
              FROM cre_ctr_archivo
             WHERE id_proceso = 351
               AND estado = 20
             ORDER BY f_proceso DESC;

            LET v_k =  1

            FOREACH crs_archivos_rema INTO arr_archivos[v_k].id_cre_ctr_archivo,
                                           arr_archivos[v_k].nom_archivo,
                                           arr_archivos[v_k].tot_aceptados,
                                           arr_archivos[v_k].f_proceso

               LET arr_archivos[v_k].activa = 0

               LET v_k = v_k + 1

            END FOREACH

            -- Elimina fila en blanco
            IF (arr_archivos[arr_archivos.getLength()].id_cre_ctr_archivo IS NULL) THEN
               CALL arr_archivos.deleteElement(arr_archivos.getLength())
            END IF

            IF(v_k =  1) THEN
               CALL fn_mensaje("","No existen archivos cargados","")
               EXIT INPUT
            END IF


         ON ACTION ACCEPT

            INITIALIZE r_aux_archivo.* TO NULL
            LET v_t_activos      = 0 -- Total de checkboxs activos

            -- Verifica que se haya seleccionado el archivo
            FOR v_k = 1 TO arr_archivos.getLength()
               IF(arr_archivos[v_k].activa =  1) THEN
                  LET v_t_activos = v_t_activos + 1
               END IF
            END FOR

            IF(v_t_activos > 1) THEN
               CALL fn_mensaje("","Debe seleccionar sólo un archivo para generar el PDF","")
               CONTINUE INPUT
            ELSE
               IF(v_t_activos = 0) THEN
                  CALL fn_mensaje("","Debe seleccionar el archivo","")
                  CONTINUE INPUT
               END IF
            END IF

             FOR v_k = 1 TO arr_archivos.getLength()
               IF(arr_archivos[v_k].activa =  1) THEN
                  LET r_aux_archivo.id_cre_ctr_archivo = arr_archivos[v_k].id_cre_ctr_archivo
                  LET r_aux_archivo.nom_archivo        = arr_archivos[v_k].nom_archivo
                  LET r_aux_archivo.tot_aceptados      = arr_archivos[v_k].tot_aceptados
                  LET r_aux_archivo.f_proceso          = arr_archivos[v_k].f_proceso
                  EXIT FOR
               END IF
            END FOR

            -- Genera PDF
            CALL fn_genera_pdf(r_aux_archivo.*)
            CALL fn_genera_archivo_salida(r_aux_archivo.*)
            LET v_mensaje = "El reporte PDF se ha generado correctamente \n",
                            "El archivo detalle se ha generado en: ",v_ruta_salida
            CALL fn_mensaje("",v_mensaje,"")
            EXIT INPUT 

         ON ACTION CANCEL
            EXIT INPUT

      END INPUT
   CLOSE WINDOW vtn2

END FUNCTION

FUNCTION fn_genera_pdf(p_archivo)

   DEFINE p_archivo        RECORD
      id_cre_ctr_archivo DECIMAL(9,0),
      nom_archivo        CHAR(40),
      tot_aceptados      DECIMAL(10,0),
      f_proceso          DATE
   END RECORD
   DEFINE r_sol_remanente  RECORD
      tpo_originacion SMALLINT,
      aivs92          DECIMAL(18,6),
      aivs97          DECIMAL(18,6),
      edo_procesar    SMALLINT,
      total           INTEGER
   END RECORD
   DEFINE v_aux_porcentaje DECIMAL(6,2)
   DEFINE v_a         INTEGER
   DEFINE v_reporte_bin     STRING
   DEFINE v_ruta_rpt        STRING
   DEFINE object_rpt        om.SaxDocumentHandler

   -- Obtiene inf. de la carga del archivo
   SELECT MAX (fecha_ini),
          MAX (fecha_fin)
     INTO v_f_ini_opera,
          v_f_fin_opera
     FROM bat_ctr_operacion
    WHERE proceso_cod = g_proceso_cod
      AND opera_cod   = g_opera_cod
      AND nom_archivo = p_archivo.nom_archivo
      AND factualiza  = p_archivo.f_proceso;

   -- Total global del archivo cargado
   SELECT SUM(aivs92),
          SUM(aivs97),
          COUNT(*)
     INTO r_t_global.aivs92,
          r_t_global.aivs97,
          r_t_global.total
     FROM cre_his_remanente
    WHERE id_cre_ctr_archivo = p_archivo.id_cre_ctr_archivo;

   IF(r_t_global.aivs92 IS NULL) THEN
      LET r_t_global.aivs92 = 0
   END IF

   IF(r_t_global.aivs97 IS NULL) THEN
      LET r_t_global.aivs97 = 0
   END IF

   -- Inicializa valores
   -- Marca 1
   LET r_t_marca1.total    = 0
   LET r_t_marca1.aivs92   = 0
   LET r_t_marca1.aivs97   = 0
   LET r_t_marca1_r.total  = 0
   LET r_t_marca1_r.aivs92 = 0
   LET r_t_marca1_r.aivs97 = 0
   LET r_t_marca1_a.total  = 0
   LET r_t_marca1_a.aivs92 = 0
   LET r_t_marca1_a.aivs97 = 0
   LET r_t_marca1_d.total  = 0
   LET r_t_marca1_d.aivs92 = 0
   LET r_t_marca1_d.aivs97 = 0
   LET r_t_marca1_n.total  = 0
   LET r_t_marca1_n.aivs92 = 0
   LET r_t_marca1_n.aivs97 = 0
   
   -- Marca 4
   LET r_t_marca4.total    = 0
   LET r_t_marca4.aivs92   = 0
   LET r_t_marca4.aivs97   = 0
   LET r_t_marca4_r.total  = 0
   LET r_t_marca4_r.aivs92 = 0
   LET r_t_marca4_r.aivs97 = 0
   LET r_t_marca4_a.total  = 0
   LET r_t_marca4_a.aivs92 = 0
   LET r_t_marca4_a.aivs97 = 0
   LET r_t_marca4_d.total  = 0
   LET r_t_marca4_d.aivs92 = 0
   LET r_t_marca4_d.aivs97 = 0
   LET r_t_marca4_n.total  = 0
   LET r_t_marca4_n.aivs92 = 0
   LET r_t_marca4_n.aivs97 = 0
   LET v_aux_porcentaje    = 0

   # Respuesta de Procesar: 
   DECLARE crs_respuesta_procesar CURSOR FOR
   SELECT r.tpo_originacion,
          SUM(r.aivs92),
          SUM(r.aivs97),
          h.edo_procesar,
          COUNT(*)
     FROM cre_his_remanente r,
          cre_his_acreditado h
    WHERE r.id_referencia      = h.id_cre_acreditado
      AND r.id_cre_ctr_archivo = p_archivo.id_cre_ctr_archivo
      AND h.edo_procesar IN (90,100,110,120)
      AND h.f_proceso > p_archivo.f_proceso
      GROUP BY 1,4

   INITIALIZE r_sol_remanente.* TO NULL

   FOREACH crs_respuesta_procesar INTO r_sol_remanente.tpo_originacion,
                                       r_sol_remanente.aivs92,
                                       r_sol_remanente.aivs97,
                                       r_sol_remanente.edo_procesar,
                                       r_sol_remanente.total

      CASE
         WHEN r_sol_remanente.tpo_originacion =  1
            -- Incrementa total global marca 1
            LET r_t_marca1.total  = r_t_marca1.total  + r_sol_remanente.total
            LET r_t_marca1.aivs92 = r_t_marca1.aivs92 + r_sol_remanente.aivs92
            LET r_t_marca1.aivs97 = r_t_marca1.aivs97 + r_sol_remanente.aivs97

            -- Rechazos
            IF(r_sol_remanente.edo_procesar = 90) THEN
               -- Incrementa total rechazos
               LET r_t_marca1_r.total  = r_t_marca1_r.total  + r_sol_remanente.total
               LET r_t_marca1_r.aivs92 = r_t_marca1_r.aivs92 + r_sol_remanente.aivs92
               LET r_t_marca1_r.aivs97 = r_t_marca1_r.aivs97 + r_sol_remanente.aivs97
            END IF

            IF(r_sol_remanente.edo_procesar = 100) THEN
               -- Incrementa total devoluciones
               LET r_t_marca1_d.total  = r_t_marca1_d.total  + r_sol_remanente.total
               LET r_t_marca1_d.aivs92 = r_t_marca1_d.aivs92 + r_sol_remanente.aivs92
               LET r_t_marca1_d.aivs97 = r_t_marca1_d.aivs97 + r_sol_remanente.aivs97
            END IF

            IF(r_sol_remanente.edo_procesar = 110) THEN
               -- Incrementa total devoluciones
               LET r_t_marca1_n.total  = r_t_marca1_n.total  + r_sol_remanente.total
               LET r_t_marca1_n.aivs92 = r_t_marca1_n.aivs92 + r_sol_remanente.aivs92
               LET r_t_marca1_n.aivs97 = r_t_marca1_n.aivs97 + r_sol_remanente.aivs97
            END IF

            -- Aceptados (Saldos transferidos)
            IF(r_sol_remanente.edo_procesar = 120) THEN
               -- Incrementa total aceptados
               LET r_t_marca1_a.total  = r_t_marca1_a.total  + r_sol_remanente.total
               LET r_t_marca1_a.aivs92 = r_t_marca1_a.aivs92 + r_sol_remanente.aivs92
               LET r_t_marca1_a.aivs97 = r_t_marca1_a.aivs97 + r_sol_remanente.aivs97
            END IF

         WHEN r_sol_remanente.tpo_originacion =  4
            -- Incrementa total global marca 4
            LET r_t_marca4.total  = r_t_marca4.total  + r_sol_remanente.total
            LET r_t_marca4.aivs92 = r_t_marca4.aivs92 + r_sol_remanente.aivs92
            LET r_t_marca4.aivs97 = r_t_marca4.aivs97 + r_sol_remanente.aivs97

            IF(r_sol_remanente.edo_procesar = 90) THEN
               -- Incrementa total rechazos
               LET r_t_marca4_r.total  = r_t_marca4_r.total  + r_sol_remanente.total
               LET r_t_marca4_r.aivs92 = r_t_marca4_r.aivs92 + r_sol_remanente.aivs92
               LET r_t_marca4_r.aivs97 = r_t_marca4_r.aivs97 + r_sol_remanente.aivs97
            END IF

            IF(r_sol_remanente.edo_procesar = 100) THEN
               -- Incrementa total devoluciones
               LET r_t_marca4_d.total  = r_t_marca4_d.total  + r_sol_remanente.total
               LET r_t_marca4_d.aivs92 = r_t_marca4_d.aivs92 + r_sol_remanente.aivs92
               LET r_t_marca4_d.aivs97 = r_t_marca4_d.aivs97 + r_sol_remanente.aivs97
            END IF

            IF(r_sol_remanente.edo_procesar = 110) THEN
               -- Incrementa total devoluciones
               LET r_t_marca4_n.total  = r_t_marca4_n.total  + r_sol_remanente.total
               LET r_t_marca4_n.aivs92 = r_t_marca4_n.aivs92 + r_sol_remanente.aivs92
               LET r_t_marca4_n.aivs97 = r_t_marca4_n.aivs97 + r_sol_remanente.aivs97
            END IF

            -- Aceptados (Saldos transferidos)
            IF(r_sol_remanente.edo_procesar = 120) THEN
               -- Incrementa total aceptados
               LET r_t_marca4_a.total  = r_t_marca4_a.total  + r_sol_remanente.total
               LET r_t_marca4_a.aivs92 = r_t_marca4_a.aivs92 + r_sol_remanente.aivs92
               LET r_t_marca4_a.aivs97 = r_t_marca4_a.aivs97 + r_sol_remanente.aivs97
            END IF

      END CASE

   END FOREACH

   -- Calcula porcentajes
   LET v_aux_porcentaje = (r_t_global.total / r_t_global.total) * 100
   LET r_t_global.porcentaje = v_aux_porcentaje CLIPPED,"%"

   -- Marca 1
   LET v_aux_porcentaje = (r_t_marca1.total / r_t_global.total) * 100
   LET r_t_marca1.porcentaje = v_aux_porcentaje CLIPPED,"%"

   LET v_aux_porcentaje = (r_t_marca1_r.total / r_t_global.total ) * 100
   LET r_t_marca1_r.porcentaje = v_aux_porcentaje CLIPPED,"%"

   LET v_aux_porcentaje = (r_t_marca1_a.total / r_t_global.total ) * 100
   LET r_t_marca1_a.porcentaje = v_aux_porcentaje CLIPPED,"%"

   LET v_aux_porcentaje = (r_t_marca1_d.total / r_t_global.total ) * 100
   LET r_t_marca1_d.porcentaje = v_aux_porcentaje CLIPPED,"%"

   LET v_aux_porcentaje = (r_t_marca1_n.total / r_t_global.total ) * 100
   LET r_t_marca1_n.porcentaje = v_aux_porcentaje CLIPPED,"%"
   
   -- Marca 4
   LET v_aux_porcentaje = (r_t_marca4.total / r_t_global.total) * 100
   LET r_t_marca4.porcentaje = v_aux_porcentaje CLIPPED,"%"

   LET v_aux_porcentaje = (r_t_marca4_r.total / r_t_global.total ) * 100
   LET r_t_marca4_r.porcentaje = v_aux_porcentaje CLIPPED,"%"

   LET v_aux_porcentaje = (r_t_marca4_a.total / r_t_global.total ) * 100
   LET r_t_marca4_a.porcentaje = v_aux_porcentaje CLIPPED,"%"

   LET v_aux_porcentaje = (r_t_marca4_d.total / r_t_global.total ) * 100
   LET r_t_marca4_d.porcentaje = v_aux_porcentaje CLIPPED,"%"

   LET v_aux_porcentaje = (r_t_marca4_n.total / r_t_global.total ) * 100
   LET r_t_marca4_n.porcentaje = v_aux_porcentaje CLIPPED,"%"
   
   -- Detalle rechazos Procesar

   LET arr_det_rch[1].tpo_originacion = 0
   LET arr_det_rch[1].marca_desc      = "No existen rechazos"
   LET arr_det_rch[1].t_registros     = 0
   LET arr_det_rch[1].diagnostico     = " "
   LET arr_det_rch[1].diag_desc       = " "

   DECLARE crs_det_rch CURSOR FOR
   SELECT r.tpo_originacion,
          c.causal,
          c.desc_causal,
          COUNT(*)
     FROM cre_his_remanente r,
          cre_his_acreditado h,
          cat_rechazo_causal c
    WHERE r.id_referencia      = h.id_cre_acreditado
      AND r.id_cre_ctr_archivo = p_archivo.id_cre_ctr_archivo
      AND h.edo_procesar = 90
      AND h.diagnostico  = c.causal 
      AND c.entidad      = 'PRC'
      AND h.f_proceso > p_archivo.f_proceso
      GROUP BY 1,2,3
      ORDER BY r.tpo_originacion;

   LET v_a = 1

   FOREACH crs_det_rch INTO arr_det_rch[v_a].tpo_originacion,
                            arr_det_rch[v_a].diagnostico,
                            arr_det_rch[v_a].diag_desc,
                            arr_det_rch[v_a].t_registros

      IF(arr_det_rch[v_a].tpo_originacion = 1) THEN
         LET arr_det_rch[v_a].marca_desc = "Marca 1"
      ELSE
         IF(arr_det_rch[v_a].tpo_originacion = 4) THEN
            LET arr_det_rch[v_a].marca_desc = "Marca 4"
         END IF
      END IF

      LET v_a = v_a + 1

   END FOREACH

   -- Elimina fila en blanco
   IF (arr_det_rch[arr_det_rch.getLength()].tpo_originacion IS NULL) THEN
      CALL arr_det_rch.deleteElement(arr_det_rch.getLength())
   END IF

   -- Detalle devoluciones Afore
   LET arr_det_dev[1].tpo_originacion = 0
   LET arr_det_dev[1].marca_desc      = "No existen devoluciones"
   LET arr_det_dev[1].t_registros     = 0
   LET arr_det_dev[1].diagnostico     = " "
   LET arr_det_dev[1].diag_desc       = " "

   DECLARE crs_det_dev CURSOR FOR
   SELECT r.tpo_originacion,
          c.causal,
          c.desc_causal,
          COUNT(*)
     FROM cre_his_remanente r,
          cre_his_acreditado h,
          cat_rechazo_causal c
    WHERE r.id_referencia      = h.id_cre_acreditado
      AND r.id_cre_ctr_archivo = p_archivo.id_cre_ctr_archivo
      AND h.edo_procesar = 100
      AND to_char(h.diagnostico,"&&&")  = c.causal
      AND c.entidad      = 'AFO'
      AND h.f_proceso > p_archivo.f_proceso
      GROUP BY 1,2,3
      ORDER BY r.tpo_originacion;

    LET v_a = 1

   FOREACH crs_det_dev INTO arr_det_dev[v_a].tpo_originacion,
                            arr_det_dev[v_a].diagnostico,
                            arr_det_dev[v_a].diag_desc,
                            arr_det_dev[v_a].t_registros

      IF(arr_det_dev[v_a].tpo_originacion = 1) THEN
         LET arr_det_dev[v_a].marca_desc = "Marca 1"
      ELSE
         IF(arr_det_dev[v_a].tpo_originacion = 4) THEN
            LET arr_det_dev[v_a].marca_desc = "Marca 4"
         END IF
      END IF

      LET v_a = v_a + 1

   END FOREACH

   -- Elimina fila en blanco
   IF (arr_det_dev[arr_det_dev.getLength()].tpo_originacion IS NULL) THEN
      CALL arr_det_dev.deleteElement(arr_det_rch.getLength())
   END IF

   # ~~~~~~~~ Configuración del reporte PDF ~~~~~~~~~~ #

   LET v_reporte_bin = v_ruta_binaria CLIPPED,"/AGRL751.4rp"
   LET v_ruta_rpt    = "Reporte_resp_procesar_",g_usuario CLIPPED,".pdf"
 
   IF (fgl_report_loadCurrentSettings(v_reporte_bin)) THEN 
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(1) -- Se muestra en automático
      CALL fgl_report_setOutputFileName(v_ruta_rpt)
      LET object_rpt = fgl_report_commitCurrentSettings()

      IF (object_rpt IS NOT NULL) THEN
         
         START REPORT imprime_pdf TO XML HANDLER object_rpt

            OUTPUT TO REPORT imprime_pdf(p_archivo.nom_archivo)

         FINISH REPORT imprime_pdf

      END IF
   ELSE
      CALL fn_mensaje("","No fué posible abrir la platilla del reporte","")
      EXIT PROGRAM
   END IF

END FUNCTION

REPORT imprime_pdf(p_nombre_archivo)

   DEFINE p_nombre_archivo   CHAR(40)
   DEFINE v_fecha            DATE
   DEFINE v_f                INTEGER

   FORMAT
      FIRST PAGE HEADER
         LET v_fecha = TODAY
         
         #Encabezado
         PRINTX g_usuario
         PRINTX v_fecha USING "dd/mm/yyyy"
         PRINTX p_nombre_archivo
         PRINTX v_f_ini_opera
         PRINTX v_f_fin_opera
         #Imprime totale global
         PRINTX r_t_global.total
         PRINTX r_t_global.aivs92
         PRINTX r_t_global.aivs97
         PRINTX r_t_global.porcentaje
         --Total marca 1
         PRINTX r_t_marca1.total
         PRINTX r_t_marca1.aivs92
         PRINTX r_t_marca1.aivs97
         PRINTX r_t_marca1.porcentaje
         PRINTX r_t_marca1_r.total
         PRINTX r_t_marca1_r.aivs92
         PRINTX r_t_marca1_r.aivs97
         PRINTX r_t_marca1_r.porcentaje
         PRINTX r_t_marca1_a.total
         PRINTX r_t_marca1_a.aivs92
         PRINTX r_t_marca1_a.aivs97
         PRINTX r_t_marca1_a.porcentaje
         PRINTX r_t_marca1_d.total
         PRINTX r_t_marca1_d.aivs92
         PRINTX r_t_marca1_d.aivs97
         PRINTX r_t_marca1_d.porcentaje
         PRINTX r_t_marca1_n.total
         PRINTX r_t_marca1_n.aivs92
         PRINTX r_t_marca1_n.aivs97
         PRINTX r_t_marca1_n.porcentaje
         --Total marca 4
         PRINTX r_t_marca4.total
         PRINTX r_t_marca4.aivs92
         PRINTX r_t_marca4.aivs97
         PRINTX r_t_marca4.porcentaje
         PRINTX r_t_marca4_r.total
         PRINTX r_t_marca4_r.aivs92
         PRINTX r_t_marca4_r.aivs97
         PRINTX r_t_marca4_r.porcentaje
         PRINTX r_t_marca4_a.total
         PRINTX r_t_marca4_a.aivs92
         PRINTX r_t_marca4_a.aivs97
         PRINTX r_t_marca4_a.porcentaje
         PRINTX r_t_marca4_d.total
         PRINTX r_t_marca4_d.aivs92
         PRINTX r_t_marca4_d.aivs97
         PRINTX r_t_marca4_d.porcentaje
         PRINTX r_t_marca4_n.total
         PRINTX r_t_marca4_n.aivs92
         PRINTX r_t_marca4_n.aivs97
         PRINTX r_t_marca4_n.porcentaje

      ON EVERY ROW 
         -- Detalle resta Procesar
         FOR v_f = 1 TO arr_det_rch.getLength()
            PRINTX arr_det_rch[v_f].marca_desc
            PRINTX arr_det_rch[v_f].t_registros
            PRINTX arr_det_rch[v_f].diagnostico
            PRINTX arr_det_rch[v_f].diag_desc
         END FOR

         FOR v_f = 1 TO arr_det_dev.getLength()
            PRINTX arr_det_dev[v_f].marca_desc
            PRINTX arr_det_dev[v_f].t_registros
            PRINTX arr_det_dev[v_f].diagnostico
            PRINTX arr_det_dev[v_f].diag_desc
         END FOR

END REPORT

FUNCTION fn_genera_archivo_salida(p_archivo_salida)

   DEFINE p_archivo_salida  RECORD
      id_cre_ctr_archivo DECIMAL(9,0),
      nom_archivo        CHAR(40),
      tot_aceptados      DECIMAL(10,0),
      f_proceso          DATE
   END RECORD

   DEFINE r_detalle         RECORD
      id_referencia      DECIMAL(9,0),
      id_derechohabiente DECIMAL(9,0),
      nss                CHAR(11),
      aivs92             CHAR(15),
      aivs97             CHAR(15),
      tpo_credito        CHAR(3),
      num_credito        CHAR(10),
      f_originacion      DATE,
      marca_activa       CHAR(3),
      marca_prc          CHAR(1),
      tpo_resp_prc       CHAR(2),
      diagnostico        CHAR(3),
      causal_rch         CHAR(60),
      causal_operativa   CHAR(3)
   END RECORD

   DEFINE archivo           base.channel
   DEFINE v_detalle         STRING
   DEFINE v_qry             STRING

   -- Ruta del archivo
   LET v_ruta_salida = v_ruta_envio CLIPPED,"/Detalle_SR_",TODAY USING "yyyymmdd",".txt"

   LET archivo = base.Channel.create()
   CALL archivo.openFile(v_ruta_salida,"w")

   LET v_qry = "SELECT rem.id_referencia,
                    \n rem.id_derechohabiente,
                    \n afi.nss,
                    \n rem.aivs92,
                    \n rem.aivs97,
                    \n acr.tpo_credito,
                    \n acr.num_credito,
                    \n acr.f_otorga,
                    \n rem.tpo_originacion,
                    \n DECODE(his.edo_procesar,90,'RE',100,'DE',110,'NA',120,'AC'),
                    \n his.diagnostico,
                    \n '000'
               \n FROM cre_his_remanente rem,
                    \n afi_derechohabiente afi,
                    \n cre_acreditado acr,
                    \n cre_his_acreditado his
              \n WHERE rem.id_derechohabiente = afi.id_derechohabiente
                \n AND rem.id_referencia      = acr.id_cre_acreditado
                \n AND acr.id_cre_acreditado  = his.id_cre_acreditado
                \n AND rem.id_cre_ctr_archivo = ",p_archivo_salida.id_cre_ctr_archivo,"
                \n AND his.edo_procesar IN (90,100,110,120)
                \n AND his.f_proceso > ","'",p_archivo_salida.f_proceso,"'"

   INITIALIZE r_detalle.* TO NULL
   LET v_detalle = NULL

   PREPARE prp_det_respuesta FROM v_qry
   DECLARE crs_det_respuesta CURSOR FOR prp_det_respuesta

   FOREACH crs_det_respuesta INTO r_detalle.id_referencia,
                                  r_detalle.id_derechohabiente,
                                  r_detalle.nss,
                                  r_detalle.aivs92,
                                  r_detalle.aivs97,
                                  r_detalle.tpo_credito,
                                  r_detalle.num_credito,
                                  r_detalle.f_originacion,
                                  r_detalle.marca_prc,
                                  r_detalle.tpo_resp_prc,
                                  r_detalle.diagnostico,
                                  r_detalle.causal_operativa

      -- Recupera marca activa
      SELECT MAX (marca) 
        INTO r_detalle.marca_activa
        FROM sfr_marca_activa
       WHERE id_derechohabiente = r_detalle.id_derechohabiente 
         AND n_referencia = r_detalle.id_referencia
      
      -- obtiene descripción de causal
      LET r_detalle.causal_rch = NULL
      
      IF(r_detalle.tpo_resp_prc = "RE") THEN
         SELECT trim(causal)||"-"||trim(desc_causal)
           INTO r_detalle.causal_rch
           FROM cat_rechazo_causal
          WHERE causal  = r_detalle.diagnostico
            AND entidad = 'PRC';
      ELSE
         IF(r_detalle.tpo_resp_prc = "DE") THEN
            LET r_detalle.diagnostico = r_detalle.diagnostico USING "&&&"
            
            SELECT trim(causal)||"-"||trim(desc_causal)
              INTO r_detalle.causal_rch
              FROM cat_rechazo_causal
             WHERE causal  = r_detalle.diagnostico
               AND entidad = 'AFO';
         END IF
      END IF

      LET v_detalle = r_detalle.nss,
                      r_detalle.aivs92        USING "&&&&&&&&&&&&.&&",
                      r_detalle.aivs97        USING "&&&&&&&&&&&&.&&",
                      r_detalle.tpo_credito   USING "&&&",
                      r_detalle.num_credito   USING "&&&&&&&&&&",
                      r_detalle.f_originacion USING "yyyymmdd",
                      r_detalle.marca_activa  USING "&&&",
                      r_detalle.marca_prc     USING "&",
                      r_detalle.tpo_resp_prc,
                      r_detalle.causal_rch,
                      r_detalle.causal_operativa

      CALL archivo.writeLine(v_detalle)

   END FOREACH

   CALL archivo.close()

END FUNCTION