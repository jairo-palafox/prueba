--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================
#################################################################################################
#Modulo       => RET                                                                            #
#Programa     => RETL484                                                                        #
#Objetivo     => Lanzador del programa que ejecuta el stored procedure que realiza la reverso   #
#                del proceso de preliquidacion de restitucion de rechazo de retiros ley73 FICO  #
#Fecha inicio => Noviembre 12, 2020                                                             #
#################################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid,     -- ID del proceso
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
       ,p_s_titulo       STRING   -- titulo de la ventana
       ,v_folio          LIKE ret_preliquida.folio_liquida  
       ,v_s_cadena       STRING -- cadena de texto
       ,v_cbx_folios     ui.ComboBox -- combo de afores
       ,v_i_conLiq      INTEGER
       ,v_b_rev_pre     SMALLINT 
       ,v_r_glo_folio      RECORD LIKE safre_viv:glo_folio.*

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
   LET g_proceso_cod = g_proceso_cod_restitucion_ret_generico_ley73fico     # 1543 Restitucion retiro  ley 73 FICO
   LET g_opera_cod   = g_opera_cod_restitucion_ret_generico_preliquidacion # 1    Preliquidacion

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'ret'

    SELECT b.ruta_listados
      INTO seg_modulo_bat.ruta_listados
      FROM seg_modulo b
     WHERE b.modulo_cod = 'bat'
  
   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_preliquida WITH FORM "RETL4801"

   -- se le asigna el apuntado der combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
  
   -- se inicia el combobox en blanco
   CALL v_cbx_folios.clear()   
   CALL v_cbx_folios.addItem(-1," ")
   
   -- se captura el folio
   INPUT
    v_folio
   WITHOUT DEFAULTS
   FROM
    cmb_folio
   ATTRIBUTES (UNBUFFERED)

      BEFORE INPUT
         -- se asignan los valores por omision
         LET v_folio                  = -1
         
         --DISPLAY g_proceso_cod,g_opera_cod
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
          SELECT  *
            FROM  glo_folio a
           WHERE  a.proceso_cod = g_proceso_cod
             AND ( a.status = g_opera_cod
              OR  a.status = 1)
          ORDER BY folio desc
       

         LET v_i_conLiq = 0
         --busca folios de preliquidacion 
         FOREACH cur_folios INTO v_r_glo_folio.*
            LET v_s_cadena = v_r_glo_folio.folio USING "##########"
       --         v_r_glo_folio.nombre_archivo

            --agrega folios encontrados al combobox "v_cbx_folios"
            CALL v_cbx_folios.addItem(
                 v_r_glo_folio.folio USING "##########", v_s_cadena)
            -- Contador fr preliquidaciones 
            LET v_i_conLiq = v_i_conLiq + 1
         END FOREACH
         IF(v_i_conLiq<1 OR v_r_glo_folio.folio IS NULL  )THEN
            CALL fn_mensaje("Atención",
                 "No existen preliquidaciones recientes","info")
            EXIT INPUT
         END IF
   
      ON ACTION ACCEPT        
         CALL fn_ret_reversa_preliquidacion(v_folio,p_usuario_cod)
         EXIT INPUT 
     
        
      ON ACTION CANCEL
         EXIT INPUT  
   
   END INPUT
   
   CLOSE WINDOW w_folio_preliquida
END MAIN

{
======================================================================
Clave: 
Nombre: fn_ret_reversa_preliquidacion
Fecha creacion: Noviembre 12, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Ejecuta el programa lanzador del reverso de la preliquidacion de restitucion
de rechazo de retiros ley73 por FICO

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_reversa_preliquidacion(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_bandera         SMALLINT, -- para revisar que los procesos ejecutaron bien
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
DEFINE v_program         varchar(50) 
   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "NA"  
   LET v_program        = "RETL484" 
   LET v_bandera        = 0

   SELECT pid
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod
      AND (estado_cod = 2
       OR estado_cod = 1) 
    
   
      LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETR306 ",
                          p_usuario_cod CLIPPED, " ",
                          g_pid  , " " ,
                          g_proceso_cod , " " ,
                          g_opera_cod ," ",
                          p_folio ," ",
                          v_nombre_archivo CLIPPED," ",
                          " 1>",seg_modulo_bat.ruta_listados CLIPPED  ,
                          "/nohup:",g_pid        USING "&&&&&",":",
                          g_proceso_cod          USING "&&&&&",":",
                          g_opera_cod            USING "&&&&&" ,
                          " 2>&1 &"
                    DISPLAY v_s_comando
                         
       RUN v_s_comando
       DISPLAY v_s_comando
       CALL fn_mensaje("Atención","Se ha enviado el reverso de preliquidación.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","information")

 
END FUNCTION