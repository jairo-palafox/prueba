################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIL47                                                        #
#Objetivo     => Programa lanzado que ejecuta el proceso de índicadores de     #
#                crédito y desmarca de cuentas en unificación recurrente       #
#Fecha inicio => 06/06/2012                                                    #
################################################################################
--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 30/06/2014
-- Fecha ultima modificacion: 20/10/2014   HMH
--===============================================================

GLOBALS "UNIG01.4gl"
GLOBALS
   DEFINE g_pid               LIKE bat_ctr_proceso.pid,     --  ID del proceso
          g_proceso_cod       LIKE cat_proceso.proceso_cod, -- codigo del proceso
          g_opera_cod         LIKE cat_operacion.opera_cod  -- codigo de operacion
          
   DEFINE g_reg_modulo RECORD
          ruta_exp            CHAR(40),
          ruta_rescate        CHAR(40),
          ruta_listados       CHAR(40)
   END RECORD
   
   DEFINE seg_modulo_bat RECORD
           ruta_listados      CHAR(40)
   END RECORD
   
   DEFINE w                   ui.Window,
          f                   ui.Form,
          v_folio             LIKE deo_preliquida.folio_liquida,
          v_cmb_folio         DECIMAL(9,0)                  --LIKE deo_preliquida.folio_liquida
          
   DEFINE r_orig_credito RECORD
          v_tipo_originacion  SMALLINT,
          v_tipo_origin_des   VARCHAR(25),
          v_tipo_credito      SMALLINT,
          v_tipo_credito_des  VARCHAR(25),
          v_totales           INTEGER
   END RECORD 
   
   DEFINE arr_orig_credito DYNAMIC ARRAY OF RECORD
          v_tipo_originacion  SMALLINT,
          v_tipo_origin_des   VARCHAR(25),
          v_tipo_credito      SMALLINT,
          v_tipo_credito_des  VARCHAR(25),
          v_totales           INTEGER
   END RECORD
   
END GLOBALS
################################################################################
MAIN 
   DEFINE p_usuario_cod               LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          p_tipo_ejecucion            SMALLINT,                     -- forma como ejecutara el programa
          p_s_titulo                  STRING,                       -- titulo de la ventana
          v_cbx_folios                ui.ComboBox,                  -- combo de afores
          v_i_conArch                 INTEGER,
          v_total_desmarca_unificador INTEGER,
          v_total_desmarca_unificado  INTEGER,
          v_folio_liquida             DECIMAL(9,0),
          p_folio_unificacion         DECIMAL(9,0)

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = 2314
   LET g_opera_cod   = 5 

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   SELECT s.ruta_bin,
          s.ruta_rescate,
          s.ruta_listados
   INTO   g_reg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'uni'

   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'

   -- se abre la ventana que envia el proceso de indicadores de credito
   CLOSE WINDOW SCREEN 
   
   OPEN WINDOW w_folio_indicadores WITH FORM "UNIL300"
   
   LET v_cbx_folios = ui.ComboBox.forName("v_cmb_folio")

   --Se selecciona el folio 
   INPUT v_folio_liquida WITHOUT DEFAULTS FROM v_cmb_folio ATTRIBUTES(UNBUFFERED)

      BEFORE INPUT 
         LET w = ui.Window.getCurrent()
         LET f = w.getForm()

         --Oculta las secciones de detalle de unificadores y unificados 
         CALL f.setElementHidden("grup_previa",1)
         CALL f.setElementHidden("gr_desmarca_unificados",1)
         CALL f.setElementHidden("grid_indicadores_unificadores",1)
         
         LET INT_FLAG = FALSE
         
         CALL fn_llena_combo_indicadores() RETURNING v_i_conArch
                     
         IF(v_i_conArch<1)THEN
            CALL fn_mensaje("Atención","No existen archivos recientemente liquidados","info")
            EXIT PROGRAM 
         END IF
      
      --Botón ACEPTAR recupera el folio y valida que tenga valor 
      ON ACTION accept     
         IF ( v_folio_liquida IS NULL OR v_folio_liquida = -1 ) THEN
            CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
            CONTINUE INPUT
         ELSE
            --Muestra la sección de detalle de unificados
            CALL f.setElementHidden("grup_previa",0)
            CALL f.setElementHidden("gr_desmarca_unificados",0)

            --Recuperar recuperar detalles de unificados y unificadores, totales a desmarcar e inhabilitar 
            CALL fn_muestra_resumen_indicador(v_folio_liquida) 
                 RETURNING v_total_desmarca_unificador,
                           v_total_desmarca_unificado,
                           p_folio_unificacion                  
         END IF

         EXIT INPUT 
            
      --Botón cancelar sale del INPUT 
      ON ACTION cancel
         LET INT_FLAG = TRUE
         EXIT PROGRAM
   END INPUT

   --Muestra detalles de unificados 
   DISPLAY ARRAY arr_orig_credito TO scr_unificados.* ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)   
      AFTER DISPLAY
   
      --Sale del programa al dar cancelar           
      ON ACTION cancelar
         EXIT PROGRAM
      
      --Muestra totales de unificados inhabilitar 
      ON ACTION Actualizacion
         --Muestra la sección de detalle de unificadores
         CALL f.setElementHidden("grid_indicadores_unificadores",0)
         
         DISPLAY v_total_desmarca_unificado TO txt_tot_des_do1
         --Muestra detalles de unificadores 
         DISPLAY ARRAY arr_orig_credito TO scr_unificadores.* ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)        
            ON ACTION cancelar
               EXIT DISPLAY       
            --Ejecuta la función de marca y desmarca de índicadores 
            ON ACTION Aplicar
               CALL fn_uni_ejecuta_indicadores_IMSS(p_folio_unificacion,
                                                    v_folio_liquida,
                                                    p_usuario_cod)               
               EXIT PROGRAM
         END DISPLAY
      EXIT DISPLAY
   END DISPLAY
CLOSE WINDOW w_folio_indicadores
END MAIN
{======================================================================
Clave: 
Nombre: fn_muestra_resumen_indicador()
Fecha creacion: 22/05/2012
Narrativa del proceso que realiza:
Muestra un resúmen del indicador solo IMSS
======================================================================}
################################################################################
FUNCTION fn_muestra_resumen_indicador(p_folio)
   DEFINE p_folio                         DECIMAL(9,0),
          v_folio_unificacion             DECIMAL(9,0),
          v_id_derechohabiente_unificador DECIMAL(9,0),
          v_id_derechohabiente_unificado  DECIMAL(9,0),
          v_id_unificador                 DECIMAL(9,0),
          v_nss_unificador                CHAR(11),
          v_s_sql                         STRING,
          v_cred_total                    SMALLINT,
          v_cred_total_1                  SMALLINT,
          v_cred_total_2                  SMALLINT,
          v_cred_total_3                  SMALLINT,
          v_desmarca_unificador           SMALLINT,
          v_total_desmarca_unificador     INTEGER,
          v_desmarca_unificado            SMALLINT,
          v_total_desmarca_unificado      INTEGER,
          v_indice                        INTEGER,
          v_indice_2                      INTEGER,
          v_i_res                         INTEGER,
          v_tot_regs                      INTEGER,
          v_f_txt                         STRING,
          r_tot_registros                 INTEGER  

   LET v_cred_total                = 0
   LET v_cred_total_1              = 0
   LET v_cred_total_2              = 0
   LET v_cred_total_3              = 0
   LET v_desmarca_unificador       = 0
   LET v_desmarca_unificado        = 0
   LET v_total_desmarca_unificador = 0
   LET v_total_desmarca_unificado  = 0
   
   SELECT folio_unificacion
   INTO   v_folio_unificacion
   FROM   uni_det_unificador
   WHERE  folio_liquidacion = p_folio
   GROUP BY 1

   DISPLAY "FOLIO LIQUIDACION  ",     p_folio
   DISPLAY "FOLIO UNIFICACION  ",     v_folio_unificacion   

   DATABASE safre_tmp

   CREATE TABLE safre_tmp:tmp_dae_indicadores_recurrente 
   (tipo_originacion SMALLINT,
    tipo_origin_des  VARCHAR(25),
    tipo_credito     SMALLINT,
    tipo_credito_des VARCHAR(25),
    totales          INTEGER
   );

   DATABASE safre_viv

   LET v_f_txt =  " EXECUTE FUNCTION fn_resumen_indicadores_recurrente(?, ?) "
   
   PREPARE prp_res_indicadores FROM  v_f_txt 
   EXECUTE prp_res_indicadores USING v_folio_unificacion, 
                                     p_usuario_cod
           INTO  r_tot_registros,      
                 v_total_desmarca_unificador, 
                 v_total_desmarca_unificado;

   DECLARE cur_totales_resumen CURSOR FOR 
   SELECT tipo_originacion, 
          tipo_origin_des ,
          tipo_credito    , 
          tipo_credito_des, 
          SUM(totales)          
   FROM   safre_tmp:tmp_dae_indicadores_recurrente
   GROUP BY 1,3,2,4
   ORDER BY 1,3,2,4

   LET v_i_res = 1

   FOREACH cur_totales_resumen INTO  arr_orig_credito[v_i_res].*
      LET v_i_res = v_i_res + 1; 
   END FOREACH 

   DISPLAY v_total_desmarca_unificador,
           v_total_desmarca_unificado
        TO txt_tot_des_dor,
           txt_tot_des_do
   
   RETURN v_total_desmarca_unificador,
          v_total_desmarca_unificado,
          v_folio_unificacion
END FUNCTION
{======================================================================
Clave: 
Nombre: fn_uni_ejecuta_preliquidacion_IMSS
Fecha creacion: 22/05/2012
Narrativa del proceso que realiza:
Ejecuta la preliquidacion de unificacion de cuentas solo IMSS
======================================================================}
################################################################################
FUNCTION fn_uni_ejecuta_indicadores_IMSS(p_folio_unificacion,
                                         p_folio_liquida,
                                         p_usuario_cod)
   DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod,        -- usuario que ejecuta el programa
          p_folio_unificacion LIKE glo_folio.folio,                -- folio para preliquidar
          p_folio_liquida     LIKE glo_folio.folio,                -- folio para preliquidar
          v_s_comando         STRING,                              -- cadena con una instruccion de consola
          v_nombre_archivo    LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
          r_bnd_fin_oper      SMALLINT,
          v_mensaje           STRING

   LET v_nombre_archivo = "CREDITOUNIFICACION"

   -- Inicio operacion.
   CALL fn_actualiza_opera_ini(g_pid,
                               g_proceso_cod,
                               g_opera_cod,
                               p_folio_liquida,
                               "UNIL47",
                               "",
                               p_usuario_cod)
        RETURNING r_bnd_fin_oper
      
   IF (r_bnd_fin_oper = 0) THEN
   
      LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/UNIP18 ",
                          p_usuario_cod, " ",
                          g_pid  , " " ,
                          g_proceso_cod , " " ,
                          g_opera_cod ," ",
                          p_folio_liquida ," ",
                          v_nombre_archivo ," ",
                          " 1>",seg_modulo_bat.ruta_listados clipped ,
                          "/nohup:",g_pid        USING "&&&&&",":",
                          g_proceso_cod USING "&&&&&",":",
                          g_opera_cod   USING "&&&&&" ,
                          " 2>&1 &"

      DISPLAY v_s_comando      
      RUN v_s_comando

      CALL fn_mensaje("Atención",
                      "Se ha enviado la actualización de indicadores.\n"||"Puede revisar el avance del proceso en el monitor de "||"ejecución de procesos",
                      "information")
   ELSE
      CALL fn_recupera_inconsis_opera(r_bnd_fin_oper)
           RETURNING v_mensaje

      CALL fn_mensaje("Atención",
                      v_mensaje,
                      "stop")
   END IF         
END FUNCTION
################################################################################
#Objetivo: Genera el ComboBox
FUNCTION fn_llena_combo_indicadores()                                                                
   DEFINE v_s_cadena        STRING,      -- cadena de texto
          v_cbx_folios      ui.ComboBox, -- combo de afores
          v_i_conArch       INTEGER,
          v_QryTxt          STRING, 
          v_droptable       STRING,
          v_resultado_drop  CHAR(10) 
          
   DEFINE v_r_glo_ctr_archivo RECORD
          folio          DECIMAL(9,0),
          nombre_archivo LIKE glo_ctr_archivo.nombre_archivo
   END RECORD       

   LET v_cbx_folios = ui.ComboBox.forName("v_cmb_folio")

   IF v_cbx_folios IS NULL THEN                                                                    
      ERROR "Form field not found in current form"                                       
      EXIT PROGRAM                                                                       
   END IF

   --Borra la tabla temporal del resúmen si existe. 
   DATABASE safre_tmp
   LET v_droptable = " DROP TABLE IF EXISTS tmp_dae_indicadores_recurrente " 
   
   PREPARE prp_droptable FROM  v_droptable  
   EXECUTE prp_droptable

   
   DATABASE safre_viv   
   
   LET v_QryTxt = "\n SELECT a.folio, b.nombre_archivo ",
                  "\n FROM   glo_folio a, ",
                  "\n        glo_ctr_archivo b, ",
                  "\n        uni_det_unificador c ",
                  "\n WHERE  a.proceso_cod = ", g_proceso_cod,
                  "\n AND    a.proceso_cod = b.proceso_cod ",
                  "\n AND    a.folio_referencia = b.folio ",
                  "\n AND    c.folio_unificacion = b.folio ",
                  "\n AND    c.diagnostico = 4 ",
                  "\n GROUP BY 1,2 ",
                  "\n ORDER BY 1,2 "    
   
   CALL v_cbx_folios.clear()

   PREPARE prp_cons_folio FROM v_QryTxt 
   DECLARE cur_fn_folios CURSOR FOR prp_cons_folio 

   LET v_i_conArch = 1
   
   --Llena el registro con los reultados de la consulta
   FOREACH cur_fn_folios INTO v_r_glo_ctr_archivo.*
      
      LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########",
                       " -",
                       v_r_glo_ctr_archivo.nombre_archivo 
                       
      CALL v_cbx_folios.addItem(v_r_glo_ctr_archivo.folio,
                                v_s_cadena)
      
      -- Contador de archivos eoncontrados
      LET v_i_conArch = v_i_conArch + 1
   END FOREACH
   
   RETURN v_i_conArch

END FUNCTION 