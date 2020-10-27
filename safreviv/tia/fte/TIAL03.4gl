--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#########################################################################################
#Modulo       => TIA                                                                    #
#Programa     => TIAL03                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la preliquidacion #
#                de Traspasos I-A                                                       #
#Fecha inicio => Marzo 26, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "TIAG01.4gl" --archivo de variables globales proceso_cod
GLOBALS
DEFINE g_pid           LIKE bat_ctr_proceso.pid, --  ID del proceso
       --g_proceso_cod   LIKE cat_proceso.proceso_cod, -- codigo del proceso
       --g_opera_cod     LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_ruta_bin      LIKE seg_modulo.ruta_bin,
       g_ruta_listados LIKE seg_modulo.ruta_listados
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING, -- titulo de la ventana
       v_folio          LIKE deo_preliquida.folio_liquida,
       v_proceso_desc   LIKE cat_proceso.proceso_desc, -- descripcion del proceso
       v_opera_desc     LIKE cat_operacion.opera_desc,  -- descripcion de la operacion
       v_ruta_vacia     LIKE seg_modulo.ruta_bin,
       v_r_glo_folio    RECORD LIKE glo_folio.*,
       v_s_cadena       STRING, -- cadena de texto
       v_cbx_folios     ui.ComboBox, -- combo de afores
       v_query          STRING

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   CALL STARTLOG(p_usuario_cod CLIPPED||".TIAL03.log")
   
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- se asigna proceso y operacion
   --LET g_proceso_cod = 1701 -- Registro de traspasos
   --LET g_opera_cod   = 3 -- preliquidacion

   -- se obtiene el PID del proceso
   CALL fn_max_pid(g_proceso_cod_tia, g_opera_cod_tia_preliquidacion)
                   RETURNING g_pid

   -- se obtienen las rutas de control del modulo
   CALL fn_rutas('tia') RETURNING g_ruta_bin,   v_ruta_vacia
   CALL fn_rutas('bat') RETURNING v_ruta_vacia, g_ruta_listados
  
   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_preliquida WITH FORM "TIAL031"

   -- se le asigna el apuntado der combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
  
   -- se inicia el combobox en blanco
   CALL v_cbx_folios.clear()
   
   -- el primer elemento en el combo es la cadena que indica que se debe elegir
   -- una afore
   CALL v_cbx_folios.addItem(-1," ")
   
   -- se captura el folio
   INPUT
    v_folio
   WITHOUT DEFAULTS
   FROM
    cmb_folio
   ATTRIBUTES (UNBUFFERED)

      BEFORE INPUT
         -- se obtiene la desripcion del proceso y de la operacion
         SELECT proceso_desc
         INTO   v_proceso_desc
         FROM   cat_proceso
         WHERE  proceso_cod = g_proceso_cod_tia

         SELECT opera_desc
         INTO   v_opera_desc
         FROM   cat_operacion
         WHERE  proceso_cod = g_proceso_cod_tia
         AND    opera_cod = g_opera_cod_tia_preliquidacion

         -- se despliegan las descripciones
         DISPLAY v_proceso_desc, v_opera_desc
         TO txt_proceso_desc, txt_opera_desc

         LET v_query = "SELECT * \n ",
                       "FROM   glo_folio \n ",
                       "WHERE  proceso_cod = ",g_proceso_cod_tia," \n ",
                       "AND    status = 0 \n ",
                       "ORDER BY 1 DESC"
                       
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FROM v_query
          FOREACH cur_folios INTO v_r_glo_folio.*
            LET v_s_cadena = v_r_glo_folio.folio
            CALL v_cbx_folios.addItem(v_r_glo_folio.folio, v_s_cadena)
         END FOREACH

         FREE cur_folios
   
      ON ACTION ACCEPT
         IF ( v_folio IS NULL ) THEN
            CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
            CONTINUE INPUT
         END IF
           
         -- se invoca la ejecucion del stored procedure
         CALL fn_tia_ejecuta_preliquidacion(v_folio, p_usuario_cod)
         EXIT INPUT
        
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_folio_preliquida
END MAIN

{
======================================================================
Clave: 
Nombre: fn_tia_ejecuta_preliquidacion
Fecha creacion: Marzo 26, 2012
Autor: Ilhuitemoc Ricardo Ortiz
Narrativa del proceso que realiza:
Ejecuta la preliquidacion de traspasos I-A
para un folio dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_tia_ejecuta_preliquidacion(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       v_si_codigo       SMALLINT, -- codigo de error
       v_sum_aivs_viv92  DECIMAL(24,6) -- suma de AIVs viv 92
       

   -- se obtiene el nombre del archivo
   --CALL fn_recupera_arch_cargado(g_proceso_cod, g_opera_cod_tia_preliquidacion)
   CALL fn_recupera_arch_cargado(g_proceso_cod_tia, 1)  # 1 --> obtiene el nombre de archivo
                                RETURNING v_nombre_archivo

   IF ( v_nombre_archivo IS NULL ) THEN
      LET v_nombre_archivo = "NA"
   END IF

   -- se verifica si la suma de los datos que se van a preliquidar es mayor a cero
   SELECT SUM(aivs_viv92)
   INTO v_sum_aivs_viv92
   FROM tia_det_traspaso
   WHERE folio = p_folio

   IF ( v_sum_aivs_viv92 <= 0 ) THEN
      CALL fn_mensaje("Atención", "No existen montos para ser preliquidados.\nActualice los estados para poder cargar otro archivo posteriormente.","exclamation")
      RETURN
   END IF
   
   CALL fn_valida_operacion(g_pid,g_proceso_cod_tia,g_opera_cod_tia_preliquidacion) RETURNING v_si_codigo
   
   -- se verifica si se puede continuar con la operacion
   IF ( v_si_codigo = 0 ) THEN
     
      # Se registra el inicio de la operacion
      CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod_tia,g_opera_cod_tia_preliquidacion,p_folio,
                                  "TIAP02",v_nombre_archivo,p_usuario_cod)
         RETURNING v_si_codigo

      -- si no se pudo iniciar la operacion
      IF ( v_si_codigo = 0 ) THEN
     
         -- se invoca la ejecucion del programa lanzado. los parametros se envian 
         -- en orden segun lo acordado el dia 12/Enero/2012 con equipo EFP
         -- usuario, pid, proceso_cod, opera_cod, folio y archivo
         LET v_s_comando = " nohup time fglrun ",g_ruta_bin CLIPPED,"/TIAP02.42r ",
                             p_usuario_cod                  , " ",
                             g_pid                          , " ",
                             g_proceso_cod_tia              , " ",
                             g_opera_cod_tia_preliquidacion , " ",
                             p_folio                        , " '",
                             v_nombre_archivo CLIPPED       ,"' ",
                             " 1>", g_ruta_listados CLIPPED ,
                             "/nohup:",g_pid                USING "&&&&&",":",
                             g_proceso_cod_tia              USING "&&&&&",":",
                             g_opera_cod_tia_preliquidacion USING "&&&&&" ,
                             " 2>&1 &"
                            
                            DISPLAY v_s_comando
                            
         RUN v_s_comando
         CALL fn_mensaje("Atención","Se ha enviado la preliquidación.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","information")
      ELSE
         -- se indica en pantalla el motivo por el que no se pudo iniciar la operacion
         CALL fn_recupera_inconsis_opera(v_si_codigo) RETURNING v_s_comando
         CALL fn_mensaje("Atención", v_s_comando.trim(),"information")
      END IF
   ELSE
      CALL fn_recupera_inconsis_opera(v_si_codigo) RETURNING v_s_comando
      CALL fn_mensaje("Atención", v_s_comando.trim(),"information")
   END IF
 
END FUNCTION