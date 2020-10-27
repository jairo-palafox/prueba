-----------------------------------------------------------------------------------------
-- Modulo        => ACL                                                                    
-- Componente    => ACLL09
-- Objetivo      => Lanzador que ejecuta programa ACLC20 que genera reporte y archivo de rechazos
-- Autor         => GERARDO ALFONSO VEGA PAREDES                                           
-- Fecha inicio  => 24 de agostro de 2018
-- Requerimiento => saci2018-68
-----------------------------------------------------------------------------------------
-- Modificación => 
-- Fehca        => 
-- Autor        => 
-- Clave cambio => 
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS "ACLG02.4gl" --archivo de variables globales proceso_cod

GLOBALS
   DEFINE g_pid            LIKE bat_ctr_proceso.pid,      --  ID del proceso
          g_proceso_cod    LIKE cat_proceso.proceso_cod,  -- codigo del proceso
          g_opera_cod      LIKE cat_operacion.opera_cod,  -- codigo de operacion
          g_usuario_cod    LIKE seg_usuario.usuario_cod,  -- clave usuario firmado
          g_tipo_ejecucion SMALLINT,                      -- forma como ejecutara el programa
          g_s_titulo       STRING                         -- titulo de la ventana
          
   DEFINE v_nombre_archivo CHAR(40)

   DEFINE  
      v_ventana ui.Window,
      v_forma   ui.form,
      p_ventana STRING, 
      g_usuario_cod LIKE seg_usuario.usuario_cod,
      v_i_cont_registros INTEGER
          
END GLOBALS
     
MAIN 

   DEFINE v_radio       CHAR(15)
   DEFINE v_folio       DECIMAL(9,0)
   DEFINE reg_glo_folio RECORD LIKE glo_folio.*
   DEFINE v_s_cadena    STRING                      -- cadena de texto
   DEFINE v_sql         STRING
   DEFINE v_cb_folio ui.ComboBox                 -- Combo para folios          
   -- se recuperan parametros de entrada
   LET g_usuario_cod    = ARG_VAL(1) -- Recibe la variable de usuario
   LET g_tipo_ejecucion = ARG_VAL(2) -- Recibe el tipo de proceso
   LET g_s_titulo       = ARG_VAL(3) -- Recibe el nombre del programa

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF g_s_titulo IS NOT NULL THEN
      CALL ui.Interface.setText(g_s_titulo)
   END IF

   -- se asignan los valores de proceso y operacion
   LET g_proceso_cod = g_proceso_cod_acl_rechazos
   LET g_opera_cod   = 1        -- generar reporte

--====== SECCIÓN NUEVA ========
   CLOSE WINDOW SCREEN

   OPEN WINDOW ventana01 WITH FORM "ACLL191"

   -- Se asigna el titulo de la ventana
   LET v_ventana = ui.Window.getCurrent()
   LET v_forma = v_ventana.getForm() 
   
   IF p_ventana IS NOT NULL THEN
      CALL ui.Interface.setText(p_ventana)         
      CALL v_ventana.setText(p_ventana)
   END IF

   -- Inicar combos  
   LET v_cb_folio = ui.ComboBox.forName("v_cb_folio")
   -- Limiar combo años
   --CALL v_cb_folio.clear()   --g

   --CALL v_cb_folio.addItem(-1," ")  --g

--   INPUT v_radio , v_folio WITHOUT DEFAULTS  --solo por archivo
   INPUT v_radio WITHOUT DEFAULTS   
--      FROM radiogroup1, v_cb_folio ATTRIBUTES (UNBUFFERED ,ACCEPT=FALSE)
      FROM radiogroup1 ATTRIBUTES (UNBUFFERED ,ACCEPT=FALSE)
      
      -- Se pre-seleccionan los combos con Enero y el año en curso
      -- Se evita seleccion en nulo de los criterio con la forma
      BEFORE INPUT
        LET v_radio = NULL
        LET v_folio = -1

      ON CHANGE radiogroup1
         IF v_radio = 1 THEN
         	LET g_proceso_cod = 102
            
            {LET v_sql = " SELECT a.* ",
                        " FROM   glo_folio a, ",
                        "        acl_sum_sc_nss b ",
                        " WHERE  a.proceso_cod = ",g_proceso_cod, 
                        " AND    a.folio = b.folio ",
                        " ORDER  BY a.folio DESC "}
         END IF
         IF v_radio = 2 THEN
         	LET g_proceso_cod = 103
            
            {LET v_sql = " SELECT a.* ",
                        " FROM   glo_folio a, ",
                        "        acl_sum_cc_nss b ",
                        " WHERE  a.proceso_cod = ",g_proceso_cod, 
                        " AND    a.folio = b.folio ",
                        " ORDER  BY a.folio DESC "}
         END IF
         IF v_radio = 3 THEN
         	LET g_proceso_cod = 107
            
            {LET v_sql = " SELECT a.* ",
                        " FROM   glo_folio a, ",
                        "        acl_sum_cc_nss b ",
                        " WHERE  a.proceso_cod = ",g_proceso_cod, 
                        " AND    a.folio = b.folio ",
                        " ORDER  BY a.folio DESC "}            
         END IF

{         PREPARE cla_sql FROM v_sql
         DECLARE cur_folios CURSOR FOR cla_sql

         FOREACH cur_folios INTO reg_glo_folio.*
            LET v_s_cadena = reg_glo_folio.folio
            CALL v_cb_folio.addItem(reg_glo_folio.folio, v_s_cadena)
         END FOREACH
         FREE cur_folios
}

      ON ACTION ACCEPT
{         IF v_folio IS NULL OR v_folio = -1 THEN
            CALL fn_mensaje("Consulta","Este folio no existe","about")
            LET v_radio = NULL
            LET v_folio = -1
            NEXT FIELD radiogroup1
        END IF
}

        LET v_folio = 0

        CALL Ejecuta_reporte(v_radio,v_folio)
    
      ON ACTION CANCEL
        EXIT INPUT

   END INPUT

   CLOSE WINDOW ventana01

END MAIN

FUNCTION Ejecuta_reporte(v_radio,v_folio)
   DEFINE v_radio CHAR(15)
   DEFINE v_folio DECIMAL(9,0)

   DEFINE v_bandera          SMALLINT -- para verificar resultado de iniciar la operacion
   DEFINE v_comando          STRING
   DEFINE l_bat_ruta_listado CHAR(40)
   DEFINE v_ruta_origen      CHAR(40)
   DEFINE v_desc_salida      VARCHAR(100)
   DEFINE v_mensaje          STRING

   SELECT ruta_listados
   INTO   l_bat_ruta_listado
   FROM   seg_modulo
   WHERE  modulo_cod = 'bat'
            
   SELECT ruta_bin,
          USER
   INTO   v_ruta_origen,
          g_usuario_cod
   FROM   seg_modulo
   WHERE  modulo_cod = 'acl'

   LET g_proceso_cod = 112
   
    -- se valida si se puede continuar con la operacion
   LET v_bandera = fn_valida_operacion(0,g_proceso_cod,g_opera_cod)
   
   IF v_bandera <> 0 THEN
      -- no se puede ejecutar la operacion
      CALL fn_recupera_inconsis_opera(v_bandera) RETURNING v_mensaje
      CALL fn_mensaje("Atención", v_mensaje, "stop")
      RETURN
   END IF 

   LET g_opera_cod = 1
   -- se genera el pid
   CALL fn_genera_pid(g_proceso_cod, g_opera_cod, g_usuario_cod) RETURNING g_pid

   LET v_nombre_archivo = "NA"
   
   --validación para iniciar el proceso
   CALL fn_inicializa_proceso(g_pid             ,
                              g_proceso_cod     ,
                              g_opera_cod       ,
                              v_folio           ,
                              "ACLL19"          ,
                              v_nombre_archivo  ,
                              g_usuario_cod)  RETURNING v_bandera
   IF v_bandera <> 0 THEN
      -- se obtiene la descripcion del parametro de salida
      SELECT descripcion
      INTO   v_desc_salida
      FROM   cat_bat_parametro_salida
      WHERE  cod_salida = v_bandera
      
      -- se construye el mensaje de error
      LET v_comando = "No se puede iniciar la operación.\nError: ", v_desc_salida CLIPPED
      CALL fn_mensaje("Atención",v_comando,"stop")
      RETURN
   END IF

   -- se inicia la operacion
   CALL fn_actualiza_opera_ini(g_pid,
                            g_proceso_cod,
                            g_opera_cod,
                            v_folio,
                            "ACLC20",
                            v_nombre_archivo,
                            g_usuario_cod)
        RETURNING v_bandera
               
   -- se ejcuta el comando que genera el archivo de salida			   
   LET v_comando = "nohup fglrun ",v_ruta_origen CLIPPED,"/ACLC20.42r ",
                    g_usuario_cod CLIPPED, " ",
                    g_pid                , " ",
                    g_proceso_cod        , " ",
                    g_opera_cod          , " ",
                    v_nombre_archivo     , " ",
                    v_radio              , " ",
                    v_folio              , " ",
                    " 1>", l_bat_ruta_listado CLIPPED ,
                    "/nohup:",g_pid  USING "&&&&&",":",
                    g_proceso_cod    USING "&&&&&",":",
                    g_opera_cod      USING "&&&&&",
                    " 2>&1 &"
   DISPLAY v_comando
   RUN v_comando
            
   CALL fn_mensaje("Atención","Se ha enviado la generacion del archivo.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","")

END FUNCTION