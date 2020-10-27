--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 29/10/2013
--==============================================================================

################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DAE                                                      #
#Programa          => DAEL15                                                   #
#Objetivo          => Programa que ejecuta el stored procedure que realiza la  #
#                   preliquidación para la devolución amortizaciones excedentes#
#Fecha inicio      => 29/10/2013                                               #
################################################################################

--LANZADO: DAES02

DATABASE safre_viv 
GLOBALS "DAEG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo   RECORD
        ruta_binarios    CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD

END GLOBALS

MAIN
DEFINE p_usuario        LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING, -- titulo de la ventana
       v_folio_dictamen LIKE deo_preliquida.folio_liquida,
       v_s_cadena       STRING, -- cadena de texto
       v_cbx_folios     ui.ComboBox, -- combo de afores
       v_i_conArch      INTEGER,
       v_folio_lote     LIKE deo_preliquida.folio_liquida
DEFINE v_r_glo_ctr_archivo      RECORD
          folio          CHAR(10),
          nombre_archivo LIKE glo_ctr_archivo.nombre_archivo
END RECORD

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = 2402 --2400
   LET g_opera_cod   = 4    -- preliquidacion

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'dae'

    SELECT b.ruta_listados
      INTO seg_modulo_bat.ruta_listados
      FROM seg_modulo b
     WHERE b.modulo_cod = 'bat'
  
   -- se abre la ventana que envia el proceso de preliquidacion
   CLOSE WINDOW SCREEN
   OPEN WINDOW w_folio_preliquida WITH FORM "DAEL150"

   -- se le asigna el apuntado der combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
  
   -- se inicia el combobox en blanco
   CALL v_cbx_folios.clear()
   
   CALL v_cbx_folios.addItem(-1," ")
   
   -- se captura el folio
   INPUT v_folio_dictamen
   WITHOUT DEFAULTS
   FROM  cmb_folio
   ATTRIBUTES (UNBUFFERED)

      BEFORE INPUT
         -- se asignan los valores por omision
         LET v_folio_dictamen  = -1       

         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR

            SELECT a.folio, " DICTAMEN "
            FROM   glo_folio a,
                   dae_det_solicitud c 
            WHERE  a.proceso_cod = 2402
            AND    a.status = 2
            AND    a.opera_cod = 1
            AND    a.folio = c.folio_dictamen 
            AND    c.estado = 4 
            GROUP BY 1
            ORDER BY 1
            
         LET v_i_conArch = 0
         FOREACH cur_folios INTO v_r_glo_ctr_archivo.*

            LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " -", 
                v_r_glo_ctr_archivo.nombre_archivo 
            CALL v_cbx_folios.addItem(v_r_glo_ctr_archivo.folio, v_s_cadena)
            -- Contador de archivos eoncontrados
            LET v_i_conArch = v_i_conArch + 1
         END FOREACH

         IF(v_i_conArch<1)THEN
            CALL fn_mensaje("Atención","No existen registros liquidados recientemente","info")
            EXIT INPUT
         END IF

      ON ACTION ACCEPT
         IF ( v_folio_dictamen IS NULL OR v_folio_dictamen = -1 ) THEN
            CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
            CONTINUE INPUT
         END IF
        
         --Se solicita la generación del archivo
         CALL fn_dae_generacion_archivo(p_usuario, v_folio_dictamen)--Se cambia a después de la liquidación
         EXIT INPUT
        
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_folio_preliquida
END MAIN

#OBJETIVO: Ejecutar la preliquidacion de la devolución de amortizaciones excedentes
FUNCTION fn_dae_ejecuta_preliquidacion(p_folio, p_usuario_cod, p_folio_dictamen)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       v_si_codigo       SMALLINT,
       v_mensaje         STRING,
       r_bnd_fin_oper    SMALLINT,
       p_folio_dictamen  DECIMAL(9,0)

   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "ARCHIVOAQUI"

   -- se verifica si se puede continuar con la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_si_codigo
   
   IF ( v_si_codigo = 0 ) THEN
   -- Inicio operacion.
      CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,p_folio,"DAEL13",
                                 "",p_usuario_cod)
       RETURNING r_bnd_fin_oper
      IF (r_bnd_fin_oper = 0) THEN
         LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_binarios CLIPPED,"/DAEP05 ",
                             p_usuario_cod, " ",
                             g_pid  , " " ,
                             g_proceso_cod , " " ,
                             g_opera_cod ," ",
                             p_folio ," ",--
                             v_nombre_archivo ," ",
                             p_folio_dictamen ," ",
                             " 1>",seg_modulo_bat.ruta_listados clipped ,
                             "/nohup:",g_pid        USING "&&&&&",":",
                             g_proceso_cod USING "&&&&&",":",
                             g_opera_cod   USING "&&&&&" ,
                             " 2>&1 &"
                            
            DISPLAY v_s_comando
                            
         -- RUN v_s_comando
          CALL fn_mensaje("Atención","Se ha enviado la preliquidación.\n"||
               "Puede revisar el avance del proceso en el monitor de "||
               "ejecución de procesos","information")
      ELSE
         CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) RETURNING v_mensaje

         CALL fn_mensaje("Atención", v_mensaje, "stop")
      END IF         
   ELSE
      CALL fn_recupera_inconsis_opera(v_si_codigo) RETURNING v_s_comando
      CALL fn_mensaje("Atención", v_s_comando.trim(),"information") 
   END IF
 
END FUNCTION


#OBJETIVO: Ejecutar la generación del archivo de salida de detalles.
FUNCTION fn_dae_generacion_archivo(p_usuario_cod, p_folio_dictamen)
DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       v_s_comando        STRING, -- cadena con una instruccion de consola
       v_nombre_archivo   LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       p_folio_dictamen   DECIMAL(9,0),
       r_bnd_opera_ini    SMALLINT, 
       v_mensaje          STRING, 
       r_bnd_valida_opera SMALLINT
   LET v_nombre_archivo = "NA"

   -- se verifica si se puede continuar con la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) 
   RETURNING r_bnd_valida_opera

   IF ( r_bnd_valida_opera = 0 ) THEN
      -- Inicio operacion.
      CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,p_folio_dictamen,"DAEL15 ","",p_usuario_cod)
      RETURNING r_bnd_opera_ini
      
      IF (r_bnd_opera_ini = 0) THEN
         LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_binarios CLIPPED,"/DAES02 ",
                          p_usuario_cod, " ",
                          g_pid  , " " ,
                          g_proceso_cod , " " ,
                          g_opera_cod ," ",
                          p_folio_dictamen ," ",
                          v_nombre_archivo ," ",
                          " 1>", seg_modulo_bat.ruta_listados clipped ,
                          "/nohup:",g_pid        USING "&&&&&",":",
                          g_proceso_cod USING "&&&&&",":",
                          g_opera_cod   USING "&&&&&" ,
                          " 2>&1 &"

               DISPLAY v_s_comando
               RUN v_s_comando
               
               CALL fn_mensaje("Atención","Se ha enviado la generación del archivo.\n"||
                                          "Puede revisar el avance del proceso en el monitor de "||
                                          "ejecución de procesos","information")
      ELSE
         CALL fn_recupera_inconsis_opera(r_bnd_opera_ini) 
         RETURNING v_mensaje

         CALL fn_mensaje("Atención", v_mensaje, "stop")
      END IF         
   ELSE
      CALL fn_recupera_inconsis_opera(r_bnd_valida_opera) 
      RETURNING v_mensaje
      CALL fn_mensaje("Atención", v_mensaje,"information") 
   END IF
END FUNCTION