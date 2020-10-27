#####################################################################
#Modulo            => AGR                                           #
#Programa          => AGRL52                                        #
#Objetivo          => Consulta Movimientos de Cuenta Individual     #
#Autor             => Jose Eduardo Ventura                          #
#Fecha inicio      => 15 de Abril de 2015                           #
#####################################################################


DATABASE safre_viv

GLOBALS

   DEFINE g_proceso_cod            INTEGER
   DEFINE g_opera_cod              INTEGER
   DEFINE g_usuario                CHAR (20)
   DEFINE r_bandera                SMALLINT
   DEFINE p_tpo_ejecucion          SMALLINT
   DEFINE p_s_titulo               STRING        -- Título de la ventana
   DEFINE r_b_valida               SMALLINT
   DEFINE v_pid                    DECIMAL(9,0)
   DEFINE v_s_comando              STRING
   DEFINE v_ruta_ejecutable        LIKE seg_modulo.ruta_bin
   DEFINE v_ruta_listados          LIKE seg_modulo.ruta_listados
   DEFINE v_ruta_envio             LIKE seg_modulo.ruta_envio
   DEFINE v_nss                    CHAR(11)
   DEFINE v_archivo                STRING
   DEFINE v_entrada                STRING
   DEFINE w ui.Window
   DEFINE f ui.Form

END GLOBALS

MAIN

   LET g_usuario       = ARG_VAL (1)
   LET p_tpo_ejecucion = ARG_val (2)
   LET p_s_titulo      = ARG_VAL (3)
   LET g_proceso_cod   = 335  -- numero de proceso correspondiente
   LET g_opera_cod     = 1    -- numero de operacion correspondiente

   --CLOSE WINDOW SCREEN

  -- se abre la ventana de consulta
  -- OPEN WINDOW AGR521 WITH FORM "AGR521"
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   MENU

      ON ACTION nss
         LET r_bandera = 1
         CALL fn_valida()
         EXIT MENU

      ON ACTION archivo
         LET r_bandera = 2
         CALL fn_valida()
         EXIT MENU

      ON ACTION CANCEL
         EXIT MENU
         END MENU
END MAIN

FUNCTION fn_valida()

   DEFINE v_archivo_d              STRING   -- Variable para saber ruta y nombre donde se dejará el archivo
   DEFINE v_archivo                STRING   -- Nombre de archivo seleccionado
   DEFINE v_pos                    INTEGER  -- Posición donde inicia la extensión ".txt"
   DEFINE cant                     INTEGER  -- Cantidad de caracteres que tiene el nombre del archivo
   DEFINE buf                      base.StringBuffer
   DEFINE v_extension              STRING
   DEFINE v_ruta_rescate           LIKE seg_modulo.ruta_rescate

   --OPEN WINDOW AGRL521 WITH FORM "AGRL521"

   IF r_bandera = 1 THEN

      OPEN WINDOW AGRL521 WITH FORM "AGRL521"

      LET w = ui.Window.getCurrent()
      LET f = w.getForm()

      CALL f.setElementHidden("tb_archivo",1)

      INPUT BY NAME v_nss ATTRIBUTES (UNBUFFERED)
         AFTER INPUT
         ON ACTION ACCEPT
         
         CASE
            WHEN v_nss IS NULL
               CALL fn_mensaje ("Archivo","Debe ingresar un NSS","information")
               NEXT FIELD v_nss

            WHEN length(v_nss) <> 11
               CALL fn_mensaje ("Archivo","NSS debe contener 11 caracteres","information")
               NEXT FIELD v_nss

            WHEN (v_nss IS NOT NULL) AND ( length(v_nss) = 11 )
               LET v_entrada = v_nss
               CALL fn_lanzado()
               EXIT INPUT

         END CASE

        ON ACTION CANCEL
           EXIT INPUT
           END INPUT
    CLOSE WINDOW AGRL521

   END IF

   IF r_bandera = 2 THEN

       OPEN WINDOW AGRL521 WITH FORM "AGRL521"

       LET w = ui.Window.getCurrent()
       LET f = w.getForm()

       CALL f.setElementHidden("lb_nss",1)
       CALL f.setFieldHidden("v_nss",1)

       INPUT BY NAME v_archivo ATTRIBUTES (UNBUFFERED)
       AFTER INPUT
      ON ACTION ACCEPT

--Se valida que el archivo tenga nombre y extensión correctos

         IF v_archivo IS NULL THEN
            CALL fn_mensaje ("Archivo","Debe de seleccionar un archivo","information")
            NEXT FIELD v_archivo
         END IF

         IF v_archivo.getIndexOf(" ", 1) THEN
         CALL fn_mensaje ("Transferencia Archivo","El nombre del archivo no debe contener espacios en blanco","information")
            LET v_archivo = ""
            DISPLAY BY NAME v_archivo
            NEXT FIELD v_archivo
         END IF

         IF v_archivo IS NOT NULL THEN
         LET v_entrada = v_archivo
         --CALL fn_mensaje ("Transferencia" ,v_archivo,"information")
            LET buf = base.StringBuffer.create()
            CALL buf.append(v_archivo)

            LET cant         = LENGTH(v_archivo)
            LET v_pos        = buf.getIndexof(".",1)
            LET v_extension  = buf.subString(v_pos,cant)

            IF (v_extension <> ".cmci") THEN 
            CALL fn_mensaje ("Transferencia Archivo","La extensión del archivo no es correcta,  \n
                               el archivo debe tener extensión .cmci","information")
                           LET v_archivo = ""
               DISPLAY BY NAME v_archivo
               NEXT FIELD v_archivo

            END IF 
            --CALL fn_mensaje ("Transferencia" ,v_nom_archivo,"information")

            IF  v_extension   = ".cmci" THEN

            LET v_archivo_d = v_archivo
            --DISPLAY "archivo :",v_archivo_d
--**********************************************************************************************************

--***************************************************
--Se  recupera el archivo y se deja en ruta rescate *
--***************************************************

-- Se recuperan las rutas de rescate y envio para el archivo
            SELECT ruta_rescate,
                   ruta_envio
              INTO v_ruta_rescate,
                   v_ruta_envio
              FROM seg_modulo
             WHERE modulo_cod ="agr"

            LET v_archivo_d = v_ruta_rescate CLIPPED,"/",v_archivo--||v_extension

            TRY

               CALL FGL_GETFILE(v_archivo,v_archivo_d)
               --MESSAGE "ARCHIVO TRANSFERIDO CORRRECTAMENTE"
               --CALL fn_mensaje ("Transferencia" ,"ARCHIVO TRANSFERIDO CORRRECTAMENTE","information")
               LET v_entrada = v_archivo_d
               CALL fn_lanzado()
               EXIT INPUT
         CONTINUE INPUT

            CATCH
               ERROR "NO SE PUDO TRANSFERIR"
               CONTINUE INPUT
            END TRY
         EXIT INPUT
      CLOSE WINDOW AGRL521

      END IF
    END IF

      EXIT INPUT

      CLOSE WINDOW AGRL521

      END INPUT
   END IF
END FUNCTION
   --MENU
   
   --ON ACTION ACCEPT
FUNCTION fn_lanzado()
   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion( 0,g_proceso_cod,g_opera_cod)   RETURNING r_b_valida
   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN  
   -- en caso de error se muestra un mensaje a usuario y no continua
     CALL fn_muestra_inc_operacion(r_b_valida)
     DISPLAY "ERROR en fn_valida_operacion"
   ELSE 
   
        --se obtienen rutas necesarias
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = "agr"

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

   CALL fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario) RETURNING v_pid
   CALL fn_inicializa_proceso(v_pid,g_proceso_cod,g_opera_cod,
                              "","AGRP33","",g_usuario)  RETURNING r_b_valida

   CALL fn_actualiza_opera_ini(v_pid,g_proceso_cod,
                               g_opera_cod,"",
                               "AGRP33","",
                               g_usuario)  RETURNING r_b_valida

   LET v_s_comando = "nohup fglrun ",
                      v_ruta_ejecutable CLIPPED,
                     "/AGRP33"    ," ",
                     g_usuario    ," ",
                     v_pid        ," ",
                     g_proceso_cod," ",
                     g_opera_cod  ," '",
                     r_bandera    ,"'" ," '" ,
                     v_entrada    ,"'" ,
                     " ' '  1>",
                     v_ruta_listados CLIPPED ,
                     "/nohup:",
                     v_pid         USING "&&&&&",":",
                     g_proceso_cod USING "&&&&&",":",
                     g_opera_cod   USING "&&&&&" ," 2>&1 &"
   RUN v_s_comando

  DISPLAY "v_s_comando", v_s_comando

  LET v_s_comando = "Se ejecutó la generación de archivos"," ",
                    "Verificar en el monitor de proceso la ejecución el PID ", v_pid USING "<<<<<<<<<"
  CALL fn_mensaje("Cuentas",v_s_comando,"information")
   --CALL fn_mensaje("Cuentas",v_entrada,"information") 
      
   END IF
 END FUNCTION  
