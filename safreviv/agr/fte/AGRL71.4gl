######################################################################
#Modulo            => AGR                                            #
#Programa          => AGRL71                                         #
#Objetivo          => Transacción para el envío de archivos de       #
#                     Restitución a PROCESAR.                        #
#Autor             => Emilio Abarca, EFP                             #
#Fecha inicio      => 10/JULIO/2018                                  #
######################################################################

IMPORT os

DATABASE safre_viv

GLOBALS 
   DEFINE g_usuario           CHAR(20)
   DEFINE g_titulo            STRING
   DEFINE g_tipo_ejecucion    SMALLINT
   DEFINE v_ruta_binaria      CHAR(40)
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE g_proceso_cod       SMALLINT  
   DEFINE g_opera_cod         SMALLINT
   DEFINE g_pid               DECIMAL(9,0)

   #Arreglo
   DEFINE arr_archivos DYNAMIC ARRAY OF RECORD
      id_proceso        SMALLINT,
      archivo           CHAR(40),
      ruta_envio        CHAR(40),
      f_proceso         CHAR(10),
      f_proceso_aux     CHAR(10),
      h_proceso         CHAR(10),
      t_registros       INTEGER,
      check_box         SMALLINT
   END RECORD

   --variables del proceso
   DEFINE g_pid           DECIMAL(9,0)
   DEFINE v_bnd           SMALLINT
   DEFINE v_comando       STRING
   DEFINE v_mensaje       STRING

END GLOBALS

MAIN 

   LET g_usuario          = ARG_VAL  (1)
   LET g_tipo_ejecucion   = ARG_VAL  (2)
   LET g_titulo           = ARG_VAL  (3)
   LET g_proceso_cod      = 348   --ENVIO DE ARCHIVOS A PROCESAR
   LET g_opera_cod        = 1

   IF (g_titulo IS NOT NULL) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

   CALL STARTLOG(g_usuario CLIPPED|| ".AGRL71.log")

   CLOSE WINDOW SCREEN 

   SELECT ruta_bin
     INTO v_ruta_binaria
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'
   
   CALL envia_archivos_restitucion()
   
END MAIN

FUNCTION envia_archivos_restitucion()

   DEFINE v_ind_reg   INTEGER
   DEFINE v_f         INTEGER
   DEFINE v_ind_check INTEGER
   DEFINE v_confirma  SMALLINT
   DEFINE v_pos       INTEGER

   OPEN WINDOW vtn1 WITH FORM "AGRL711"

      LET v_ind_reg = 1  --No existen archivos a enviar

      INPUT ARRAY arr_archivos FROM tabla1.* ATTRIBUTE(WITHOUT DEFAULTS,APPEND ROW = FALSE,
                                                       DELETE ROW = FALSE,INSERT ROW = FALSE,UNBUFFERED)

         BEFORE INPUT
            LET v_ind_reg = recupera_archivos()

            IF(v_ind_reg = 1) THEN
               CALL fn_mensaje("","No existen archivos por enviar a Procesar","")
               EXIT INPUT
            END IF

         BEFORE ROW
            LET v_pos = ARR_CURR()

         ON CHANGE check_box
            IF (arr_archivos[v_pos].check_box = 1) AND
               (arr_archivos[v_pos].t_registros = 0) THEN
               CALL fn_mensaje("","El archivo seleccionado no se puede enviar, no contiene registros","")
               LET arr_archivos[v_pos].check_box = 0
            END IF

         ON ACTION ACCEPT
            LET v_ind_check = 0
            --verifica que por lo menos se haya seleccionado 1 archivo a enviar
            FOR v_f = 1 TO arr_archivos.getLength()
               IF(arr_archivos[v_f].check_box = 1) THEN
                  LET v_ind_check = 1
                  EXIT FOR
               END IF
            END FOR

            IF(v_ind_check = 0) THEN
               CALL fn_mensaje("","Selecciona por lo menos un archivo a enviar","")
               CONTINUE INPUT
            END IF

            CALL fn_ventana_confirma("Alerta","¿Está seguro de continuar con el envío?","stop") RETURNING v_confirma

            IF(v_confirma = 1) THEN

               CALL crea_temporal()
            
               --Recorre arreglo para guardar los archivos a enviar
               FOR v_f = 1 TO arr_archivos.getLength()
                  IF(arr_archivos[v_f].check_box = 1) THEN
                     INSERT INTO tmp_envio_arh_dev_exc (
                                          id_proceso,
                                          archivo,
                                          ruta_envio,
                                          f_proceso,
                                          h_proceso,
                                          t_registros
                                         )
                                  VALUES (arr_archivos[v_f].id_proceso,
                                          arr_archivos[v_f].archivo,
                                          arr_archivos[v_f].ruta_envio,
                                          arr_archivos[v_f].f_proceso_aux,
                                          arr_archivos[v_f].h_proceso,
                                          arr_archivos[v_f].t_registros
                                          );
                  END IF
               END FOR

               #Inicializa el proceso
               -- Genera PID
               LET g_pid = fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario)

               -- Inicializa proceso
               CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,"","AGRL71","",g_usuario) RETURNING v_bnd

               IF(v_bnd <> 0) THEN
                  -- Si no se peude ejecutar el proceso se indica la razon
                  CALL fn_muestra_inc_operacion(v_bnd)
                  EXIT INPUT
               END IF

               -- Actualiza operación como iniciada
               CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"AGRL71","",g_usuario) RETURNING v_bnd

               IF(v_bnd <> 0) THEN
                  -- Si no se peude ejecutar el proceso se indica la razon
                  CALL fn_muestra_inc_operacion(v_bnd)
                  EXIT INPUT
               END IF

               -- Ejecuta lanzado
               --En el parámetro 5 se envía "2" haciendo referencia que son archivos de RESTITUCIÓN a enviar
               LET v_comando = "nohup fglrun ",v_ruta_binaria CLIPPED,"/AGRP49 ",
                                               g_usuario," ",
                                               g_pid," ",
                                               g_proceso_cod," ",
                                               g_opera_cod," ",
                                               2,
                                               " ' ' 1>",
                                               v_ruta_listados CLIPPED,
                                               "/nohup:",g_pid USING "&&&&&",":",
                                               g_proceso_cod   USING "&&&&&",":",
                                               g_opera_cod     USING "&&&&&" ," 2>&1 &"

               RUN v_comando

               LET v_mensaje = "Se ejecutó el proceso de envío de archivos a Procesar"," \n",
                               "Verificar en el monitor de procesos el PID: ",g_pid USING "<<<<<<<<<"

               CALL fn_mensaje("",v_mensaje,"")
               EXIT INPUT
            ELSE
               CALL fn_mensaje("","Se ha cancelado el envío","")
               EXIT INPUT
            END IF

         ON ACTION CANCEL
            EXIT INPUT

      END INPUT

   CLOSE WINDOW vtn1

END FUNCTION

FUNCTION recupera_archivos()

   DEFINE v_c             INTEGER
   DEFINE v_archivo_envia CHAR(40)
   DEFINE v_id_proceso    SMALLINT
   DEFINE v_ruta_archivo  STRING
   DEFINE v_mtime         CHAR(25)
   DEFINE v_comando_t     STRING
   DEFINE ch              base.channel
   DEFINE v_arh_total     STRING
   DEFINE v_linea         INTEGER

   DEFINE r_seg_modulo    RECORD
      modulo_cod   CHAR(3),
      ruta_envio   CHAR(40)
   END RECORD

   CALL arr_archivos.clear()          --Limpia arreglo
   INITIALIZE r_seg_modulo.* TO NULL  --Limpia record

   DECLARE crs_sol_sdo CURSOR FOR
   SELECT modulo_cod,
          ruta_envio
     FROM seg_modulo
    WHERE modulo_cod IN ('acr','grt');

   LET v_c = 1

   FOREACH crs_sol_sdo INTO r_seg_modulo.modulo_cod,
                            r_seg_modulo.ruta_envio

      LET v_mtime = NULL

      --Archivo a enviar de acuerdo al módulo
      CASE
      
         --TA
         WHEN r_seg_modulo.modulo_cod = "acr"
            LET v_archivo_envia = "sol_dev_acr.dse"
            LET v_id_proceso    = 217   --Generación solicitud devolución sdo exc ACR

         --43BIS
         WHEN r_seg_modulo.modulo_cod = "grt"
            LET v_archivo_envia = "sol_dev_ug.cdse"
            LET v_id_proceso    = 1222  --Generación solicitud devolución sdo exc GRT

      END CASE

      --concatena la ruta completa del archivo
      LET v_ruta_archivo = r_seg_modulo.ruta_envio CLIPPED,"/",v_archivo_envia CLIPPED

      --Verifica que exista el archivo en el servidor
      IF (os.Path.exists(v_ruta_archivo)) THEN

         LET arr_archivos[v_c].id_proceso = v_id_proceso
         LET arr_archivos[v_c].archivo    = v_archivo_envia
         LET arr_archivos[v_c].ruta_envio = r_seg_modulo.ruta_envio
         LET arr_archivos[v_c].check_box  = 0 --Inactivo

         --Recupera la fecha del archivo en el servidor
         LET v_mtime = os.Path.mtime(v_ruta_archivo)
         LET arr_archivos[v_c].f_proceso     = v_mtime[9,10],"/",v_mtime[6,7],"/",v_mtime[1,4]
         LET arr_archivos[v_c].f_proceso_aux = v_mtime[6,7],"/",v_mtime[9,10],"/",v_mtime[1,4] --Se crea fecha con formato correcto para insert en la temporal
         LET arr_archivos[v_c].h_proceso     = v_mtime[12,20]

         --Total de registros en el archivo
         LET v_comando_t = "wc -l ",v_ruta_archivo," > ",r_seg_modulo.ruta_envio CLIPPED,"/wc_arh_dev.unl"
         RUN v_comando_t

         --Obtiene sólo el total de registros del archivo
         LET v_comando_t = "cut -d ' ' -f 1 ",r_seg_modulo.ruta_envio CLIPPED,"/wc_arh_dev.unl"," > ",r_seg_modulo.ruta_envio CLIPPED,"/total_arh_dev.unl"
         RUN v_comando_t

         --Se lee el archivo total_sol_ta.unl
         LET v_arh_total = r_seg_modulo.ruta_envio CLIPPED,"/total_arh_dev.unl"
         LET ch = base.Channel.create() # Creamos un objeto de la clase channel
         CALL ch.openFile(v_arh_total,"r")

         LET v_linea = NULL

         WHILE TRUE

            --Mientras el bucle se cumpla obtiene el valor de la fila
            LET v_linea = ch.readLine()

            IF(v_linea IS NOT NULL) THEN
               EXIT WHILE  --Sale del bucle, ya que nos interesa la primera línea del archivo
            ELSE
               --Verifica si ya llegó al final del archivo
               IF(ch.isEof()) THEN
                  EXIT WHILE
               END IF
            END IF

         END WHILE

         CALL ch.close()

         --Asigna total de registros al arreglo
         LET arr_archivos[v_c].t_registros = v_linea - 2 --(Resta el encabezado y sumario)

         --Por último elimna los archivos de salida
         LET v_comando_t = "rm ",r_seg_modulo.ruta_envio CLIPPED,"/wc_arh_dev.unl"
         RUN v_comando_t

         LET v_comando_t = "rm ",r_seg_modulo.ruta_envio CLIPPED,"/total_arh_dev.unl"
         RUN v_comando_t

         LET v_c = v_c + 1  --Incrementa contador al existir el archivo

      END IF

   END FOREACH


   --Retorna contador
   RETURN v_c

END FUNCTION

FUNCTION crea_temporal()

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_envio_arh_dev_exc

   WHENEVER ERROR STOP
      CREATE TABLE tmp_envio_arh_dev_exc (
                        id_proceso   SMALLINT,
                        archivo      CHAR(40),
                        ruta_envio   CHAR(40),
                        f_proceso    DATE,
                        h_proceso    CHAR(10),
                        t_registros  INTEGER
                        );
END FUNCTION