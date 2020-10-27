################################################################################
#Modulo       => LAV                                                           #
#Programa     => LAVC02                                                        #
#Objetivo     => Consultar el precio del dólar por fecha o por folio.          #
#Fecha inicio => 30/12/2014                                                    #
################################################################################
--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--==============================================================================
DATABASE safre_viv

GLOBALS
DEFINE g_usuario_cod    LIKE seg_usuario.usuario_cod,        -- Usuario firmado 
       g_pid            LIKE bat_ctr_proceso.pid,            -- ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod,        -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod,        -- codigo de operacion,
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo  -- Nombre Archivo Procesado
DEFINE g_reg_modulo   RECORD
        ruta_bin         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
END RECORD
DEFINE seg_modulo_bat RECORD
        ruta_listados    CHAR(40)   
END RECORD

DEFINE arr_detalles DYNAMIC ARRAY OF RECORD 
          v_id_tipo_cambio DECIMAL(9,0),
          v_f_valor         DATE,
          v_valor           DECIMAL(10,4), 
          v_f_registro      DATE
END RECORD

END GLOBALS

MAIN
DEFINE f_ventana     ui.window
DEFINE f_forma       ui.form

DEFINE v_folio           DECIMAL(9,0), 
       v_fecha_valor     DATE,
       v_fecha_registro  DATE,
       v_tot             INTEGER,
       p_tipo_ejecucion  SMALLINT,
       p_titulo          STRING

   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF
   
   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, 
          s.ruta_rescate, 
          s.ruta_listados
   INTO   g_reg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'lav'

   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'

   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_genera WITH FORM "LAVC021"
   DIALOG ATTRIBUTES(UNBUFFERED) 

   --Se obtienen los parámetros de folio, estado, NSS para consulta
   INPUT v_fecha_valor, v_fecha_registro
   FROM  de_f_valor, de_f_registro

   BEFORE INPUT
      -- Recupera punteros a ventana para control de grupos
      LET f_ventana = ui.Window.getCurrent()
      LET f_forma = f_ventana.getForm()

      LET v_fecha_valor    = TODAY
      LET v_fecha_registro = TODAY

      CALL f_forma.setelementhidden("gr_detalles", 1)
   END INPUT

   ON ACTION ACCEPT
      CALL fn_consulta_detalles(v_fecha_valor, v_fecha_registro)
           RETURNING v_tot

      IF v_tot >= 1 THEN
         CALL f_forma.setelementhidden("gr_detalles", 0)

         DISPLAY ARRAY arr_detalles TO scr_detalles.*
         ATTRIBUTES(UNBUFFERED, CANCEL = FALSE) 
            ON ACTION cancelar
               CALL f_forma.setelementhidden("gr_detalles", 1)
               EXIT DISPLAY
         END DISPLAY

      ELSE 
         CALL fn_mensaje ("Atención", 
                          "No existen registros con los parámetros seleccionados",
                          "stop")
      END IF

   ON ACTION cancelar 
      EXIT DIALOG

   END DIALOG    
   CLOSE WINDOW w_folio_genera

END MAIN

FUNCTION fn_consulta_detalles(p_fecha_valor, p_fecha_registro)
DEFINE p_fecha_valor     DATE, 
       p_fecha_registro  DATE,
       v_qry_txt         STRING,
       v_tot        INTEGER

   LET v_qry_txt = " \n SELECT id_tipo_cambio, ",
                   " \n        f_valor, ",
                   " \n        valor, ",                   
                   " \n        f_registro ",
                   " \n FROM   lav_tipo_cambio ",
                   " \n WHERE  1 = 1 "

       IF p_fecha_valor IS NOT NULL THEN
          LET v_qry_txt = v_qry_txt || " \n AND    f_valor = '", p_fecha_valor, "'"
       END IF   

       IF p_fecha_registro IS NOT NULL THEN
          LET v_qry_txt = v_qry_txt || " \n AND    f_registro = '", p_fecha_registro, "'"
       END IF

       LET v_qry_txt = v_qry_txt || " \n ORDER BY 2 "    

   LET v_tot = 1

   PREPARE prp_detalles FROM v_qry_txt
   DECLARE cur_detalles CURSOR FOR prp_detalles

   FOREACH cur_detalles INTO arr_detalles[v_tot].v_id_tipo_cambio,
                             arr_detalles[v_tot].v_f_valor,
                             arr_detalles[v_tot].v_valor,
                             arr_detalles[v_tot].v_f_registro

      LET v_tot = v_tot + 1 

   END FOREACH
   
   CALL arr_detalles.deleteElement(v_tot)
   LET v_tot = v_tot - 1 

   RETURN v_tot

END FUNCTION 

