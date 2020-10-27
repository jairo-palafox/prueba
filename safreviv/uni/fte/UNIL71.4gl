--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 26/10/2015
--==============================================================================
################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIL71                                                        #
#Objetivo     => Lanzador reverso generar archivo salida                       #
#Fecha inicio => 26/10/2015                                                    #
################################################################################

GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo   RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD

END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo       STRING -- titulo de la ventana
       ,v_folio          DECIMAL(9,0)
       ,v_s_cadena       STRING -- cadena de texto
       ,v_cbx_folios     ui.ComboBox -- combo de afores
       ,v_i_conArch      INTEGER
       ,v_r_glo_ctr_archivo RECORD
          folio          DECIMAL(9,0),
          nombre_archivo LIKE glo_ctr_archivo.nombre_archivo
       END RECORD

       ,v_proceso_desc    LIKE cat_proceso.proceso_desc
       ,v_opera_desc      LIKE cat_operacion.opera_desc
       ,v_status          SMALLINT
       ,v_QryTxt          STRING

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = 2318 -- UNI
   LET g_opera_cod   = 6    -- Archivo Salida

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
   INTO   g_reg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'uni'

   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'

   CALL fn_recuper_datos_proceso(g_proceso_cod, g_opera_cod)
        RETURNING v_proceso_desc, v_opera_desc
     
   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_preliquida WITH FORM "UNIL710"
   -- Muestra detalle de operaci�n
   DISPLAY BY NAME v_proceso_desc, v_opera_desc

   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
  
   -- se inicia el combobox en blanco
   CALL v_cbx_folios.clear()
   
   CALL v_cbx_folios.addItem(-1," ")
   
   -- se captura el folio
   INPUT v_folio
   WITHOUT DEFAULTS
   FROM cmb_folio
   ATTRIBUTES (UNBUFFERED)

      BEFORE INPUT

      -- se asignan los valores por omision
      LET v_folio = -1
      
      LET v_QryTxt = " \n SELECT status    ",
                     " \n FROM   glo_folio ",
                     " \n WHERE  folio IN (SELECT MAX(folio) ",
                     " \n                  FROM   glo_folio  ",
                     " \n                  WHERE  proceso_cod = ", g_proceso_cod ,")"
     PREPARE prp_status FROM v_QryTxt
     EXECUTE prp_status INTO v_status

     IF v_status = 5 THEN 
        -- se llena el arreglo de folios
        DECLARE cur_folios_rev CURSOR FOR SELECT a.folio, b.nombre_archivo 
                                          FROM   glo_folio a, 
                                                 glo_ctr_archivo b
                                          WHERE  a.proceso_cod = g_proceso_cod
                                          AND    a.proceso_cod = b.proceso_cod
                                          AND    a.folio_referencia = b.folio
                                          AND    a.opera_cod   = 2
                                          AND    a.status      = 5 
                                          GROUP BY 1,2
                                          ORDER BY 1

         LET v_i_conArch = 0
         FOREACH cur_folios_rev INTO v_r_glo_ctr_archivo.*
            LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " -", 
                v_r_glo_ctr_archivo.nombre_archivo 
            CALL v_cbx_folios.addItem(v_r_glo_ctr_archivo.folio, v_s_cadena)
            -- Contador de archivos eoncontrados
            LET v_i_conArch = v_i_conArch + 1
         END FOREACH
     ELSE 
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR SELECT a.folio, b.nombre_archivo 
                                       FROM   glo_folio a, 
                                              glo_ctr_archivo b, 
                                              uni_det_unificador c 
                                       WHERE  a.proceso_cod = g_proceso_cod
                                       AND    a.proceso_cod = b.proceso_cod
                                       AND    a.folio_referencia = b.folio
                                       AND    c.folio_liquidacion = a.folio
                                       AND    a.opera_cod   = 3
                                       AND    c.diagnostico = 6
                                       AND    a.status      = 2 
                                       GROUP BY 1,2
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
            CALL fn_mensaje("Atenci�n",
                 "No existen archivos recientemente liquidados","info")
            EXIT INPUT
         END IF
      END IF 
         
      ON ACTION ACCEPT
         IF ( v_folio IS NULL OR v_folio = -1) THEN
            CALL fn_mensaje("Atenci�n","Es necesario capturar un folio","stop")
            CONTINUE INPUT
         END IF
      
         -- se invoca la ejecucion del stored procedure
         CALL fn_uni_reverso_archivo_salida(v_folio, v_status, p_usuario_cod)
         EXIT INPUT
        
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT

   
   CLOSE WINDOW w_folio_preliquida
END MAIN

#OBJETIVO: Generar el reverso del archivo de salida.
FUNCTION fn_uni_reverso_archivo_salida(p_folio, p_status, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       bnd_resultado     SMALLINT,
       p_status          SMALLINT

   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "NA"

   -- se verifica si se puede continuar con la operacion
   
   CALL fn_valida_reverso(g_pid,g_proceso_cod,g_opera_cod) 
        RETURNING bnd_resultado     
   LET bnd_resultado = 0
   IF bnd_resultado = 0 THEN
      LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/UNIR42 ",
                          p_usuario_cod CLIPPED, " ",
                          g_pid  , " " ,
                          g_proceso_cod , " " ,
                          g_opera_cod ," ",
                          p_folio ," ",
                          v_nombre_archivo CLIPPED," ",
                          p_status ," ",
                          " 1>",seg_modulo_bat.ruta_listados clipped ,
                          "/nohup:",g_pid        USING "&&&&&",":",
                          g_proceso_cod USING "&&&&&",":",
                          g_opera_cod   USING "&&&&&" ,
                          " 2>&1 &"

       DISPLAY v_s_comando
       RUN v_s_comando

       CALL fn_mensaje("Atenci�n","Se ha enviado el reverso de la generacion de archivo.\n"||
                       "Puede revisar el avance del proceso en el monitor de ejecuci�n de procesos",
                       "information")

   ELSE
      CALL fn_recupera_inconsis_opera(bnd_resultado) RETURNING v_s_comando
      CALL fn_mensaje("Atenci�n", v_s_comando.trim(),"information")
   END IF
 
END FUNCTION

#OBJETIVO: Recupera descripciones de proceso y operaci�n para datos de pantalla.
FUNCTION fn_recuper_datos_proceso(p_proceso, p_operacion)
  DEFINE 
   p_proceso         LIKE cat_proceso.proceso_cod
   ,p_operacion      LIKE cat_operacion.opera_cod
   --
   ,v_proceso_desc    LIKE cat_proceso.proceso_desc
   ,v_opera_desc      LIKE cat_operacion.opera_desc
   
   -- <Recupera descripci�n del proceso> --
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso

    -- <Recupera descripci�n de operaci�n> --
   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso
      AND opera_cod   = p_operacion
      
 --DISPLAY BY NAME v_proceso_desc, v_opera_desc
 RETURN v_proceso_desc, v_opera_desc
END FUNCTION -- fn_recuper_datos_proceso