####################################################################################
#Módulo            => GRT                                                          #
#Programa          => GRTL50                                                       #
#Objetivo          => Programa lanzador para el proceso Generación del archivo     #
#                     de solicitudes UG histórico, el cual contendrá el detalle de #
#                     los registros que Cartera (SAS) solicitó para pagos UG 43BIS #
#Autor             => Emilio Abarca, EFP / Mauro Muñiz Caballero                   #
#Fecha inicio      => 3/julio/2018                                                 #
####################################################################################

DATABASE safre_viv

GLOBALS

   DEFINE g_usuario                 CHAR(20)
   DEFINE g_titulo                  STRING
   DEFINE g_tipo_ejecucion          SMALLINT
   DEFINE cbox                      DECIMAL(9,0)
   DEFINE g_proceso_cod             SMALLINT
   DEFINE g_opera_cod               SMALLINT
   DEFINE g_pid                     DECIMAL(9,0)
   DEFINE v_f_proceso               DATE
   DEFINE v_nom_archivo             CHAR(40)
   DEFINE v_bnd                     SMALLINT 
   DEFINE v_mensaje                 STRING 
   DEFINE v_s_comando               STRING
   DEFINE v_ruta_ejecutable         CHAR(40)
   DEFINE v_ruta_listados           CHAR(40)

END GLOBALS 

MAIN

   LET g_usuario        = ARG_VAL  (1)
   LET g_tipo_ejecucion = ARG_VAL  (2)
   LET g_titulo         = ARG_VAL  (3)
   LET g_proceso_cod    = 1234 -- GENERACIÓN HISTÓRICO ARCHIVO SOLICITUDES SAS
   LET g_opera_cod      = 1

    -- Se asigna el título de la ventana
   IF ( g_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'grt'

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

   -- se crea el archivo log
   CALL STARTLOG(g_usuario CLIPPED|| ".GRTL50.log")

   CLOSE WINDOW SCREEN

   CALL fn_principal()

END MAIN 

FUNCTION fn_principal()  

   OPEN WINDOW vtn WITH FORM "GRTL501"

      INPUT BY NAME cbox ATTRIBUTE(UNBUFFERED)

         BEFORE INPUT 
            CALL fn_carga_cbox()

         ON ACTION ACCEPT 
            LET v_nom_archivo = NULL 
            LET v_F_proceso   = NULL 

            IF(cbox IS NULL) THEN
               CALL fn_mensaje("","Selecciona el archivo","") 
               NEXT FIELD cbox
            ELSE 
               -- obtiene la f_proceso del archivo seleccionado.
               SELECT nom_archivo,
                       f_proceso
                  INTO v_nom_archivo,
                       v_f_proceso
                  FROM cre_ctr_archivo
                 WHERE id_cre_ctr_archivo = cbox;

               -- Genera PID
               LET g_pid = fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario)

               -- Inicializa proceso
               CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,"","GRTS07","",g_usuario) RETURNING v_bnd

                -- Actualiza operación como iniciada
               CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"GRTS07",v_nom_archivo,g_usuario) RETURNING v_bnd

                -- Ejecuta lanzado
               LET v_s_comando = "nohup fglrun ",
                                  v_ruta_ejecutable CLIPPED,
                                  "/GRTS07"," ",
                                  g_usuario," ",
                                  g_pid," ",
                                  g_proceso_cod," ",
                                  g_opera_cod," ",
                                  cbox," ",
                                  v_nom_archivo," ",
                                  v_f_proceso," ",
                                  1," ",
                                  " ' ' 1>",v_ruta_listados CLIPPED,
                                  "/nohup:",g_pid USING "&&&&&",":",
                                  g_proceso_cod   USING "&&&&&",":",
                                  g_opera_cod     USING "&&&&&" ," 2>&1 &"

               RUN v_s_comando

               LET v_mensaje = "Se ejecutó la generación del histórico de solicitudes UG (SAS)"," \n",
                               "Consultar ejecución en el monitor de procesos, PID: ", g_pid

               CALL fn_mensaje("",v_mensaje,"")
               EXIT INPUT

            END IF

         ON ACTION CANCEL
            EXIT INPUT

      END INPUT

   CLOSE WINDOW vtn

END FUNCTION 

FUNCTION fn_carga_cbox()

   DEFINE cbx                       ui.ComboBox

   DEFINE r_archivo RECORD
      id_cre_ctr_archivo            DECIMAL(9,0),
      nombre_archivo                CHAR(40)
   END RECORD 

   LET cbx = ui.ComboBox.forName("cbox")

   # 1202 --> Recepción uso garantía 43Bis (SAS)
   # Recupera archivos que envía CARTERA desde el cambio de plataforma SAS-43BIS (Enero-2017)
   DECLARE crs_arh_grt CURSOR FOR 
      SELECT UNIQUE r.id_cre_ctr_archivo,
              r.nom_archivo
        FROM cre_ctr_archivo r, cre_uso_garantia c
       WHERE r.id_proceso = 1202
         AND r.nom_archivo MATCHES "*rcu"
         AND r.estado = 20
         AND r.f_proceso > '04/28/2017'
         AND r.id_cre_ctr_archivo = c.id_cre_ctr_archivo
         AND c.tpo_transferencia = "48"

   INITIALIZE r_archivo.* TO NULL

   FOREACH crs_arh_grt INTO r_archivo.id_cre_ctr_archivo,
                             r_archivo.nombre_archivo

      CALL cbx.addItem(r_archivo.id_cre_ctr_archivo,r_archivo.nombre_archivo)

   END FOREACH 

END FUNCTION
