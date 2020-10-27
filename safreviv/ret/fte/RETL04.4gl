--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETL04                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la reverso        #
#                liquidacion para retiro solo infonavit                              #               #
#Fecha inicio => Febrero 22, 2012                                                       #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid,     -- ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_opera_cod_liquidacion LIKE cat_operacion.opera_cod, -- codigo de operacion       
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
DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion  SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo        STRING   -- titulo de la ventana
       ,v_folio           LIKE ret_preliquida.folio_liquida  
       ,v_tmp_folio       LIKE ret_preliquida.folio_liquida
       ,v_bnd_continuar   SMALLINT
DEFINE v_query                 STRING,
       r_ruta_bin              STRING,
       r_ruta_listados         STRING
       ,v_cbx_folios     ui.ComboBox -- combo de afores
       ,v_r_glo_folio      RECORD LIKE safre_viv:glo_folio.*
       ,v_i_conLiq SMALLINT
       ,v_s_cadena             STRING -- cadena de texto

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
   LET g_proceso_cod = g_proceso_cod_ret_solo_infonavit     --50 -- RETIRO SOLO INFONAVIT
   LET g_opera_cod   = g_opera_cod_ret_soloInfo_liquidacion --2
   LET g_opera_cod_liquidacion   = g_opera_cod  -- liquidacion

     -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'ret'

    SELECT b.ruta_listados
      INTO seg_modulo_bat.ruta_listados
      FROM seg_modulo b
     WHERE b.modulo_cod = 'bat'
   
  LET v_query = "\n SELECT MAX(pid)                        "
                ,"\n FROM   bat_ctr_operacion               "
                ,"\n WHERE  folio = ?                       "
                ,"\n AND    proceso_cod = ?                 "
                
   PREPARE prp_obtiene_pid_liq FROM v_query
   --Se obtiene las rutas
   CALL fn_rutas("ret") RETURNING r_ruta_bin,r_ruta_listados
   
   OPEN WINDOW w_reversaIntegracion WITH FORM r_ruta_bin.trim()||"/RETL080"
   LET v_cbx_folios = ui.ComboBox.forName("formonly.v_folio")

   CALL v_cbx_folios.clear()   
   CALL v_cbx_folios.addItem(-1," ")

      INPUT BY NAME v_folio ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
      
         BEFORE INPUT
            CALL ui.Interface.setText(p_s_titulo)
            --Se despliaga la descripción del proceso
            CALL fn_despliega_desc(g_proceso_cod, g_opera_cod_liquidacion)
             -- se asignan los valores por omision
             -- se asignan los valores por omision
            LET v_folio                  = -1       
            --DISPLAY g_proceso_cod,g_opera_cod
            -- se llena el arreglo de folios

     LET v_query = "\n  SELECT FIRST 1 gl.*                         ",
                   "\n          FROM   glo_folio gl                 ",
                   "\n                ,ret_solo_infonavit fh          ",
                   "\n         WHERE gl.proceso_cod =  ",g_proceso_cod, 
                   "\n           AND gl.folio   =  fh.folio          ",
                   "\n           AND gl.status  = ",g_opera_cod      ,  
                   "\n           AND fh.estado_solicitud = 60       "

            DISPLAY v_query
            PREPARE pre_folio FROM  v_query
            DECLARE cur_folios CURSOR FOR pre_folio

            LET v_i_conLiq = 0
            --busca folios de preliquidacion 
            FOREACH cur_folios INTO v_r_glo_folio.*
                LET v_s_cadena = v_r_glo_folio.folio USING "##########"
            --         v_r_glo_folio.nombre_archivo

                --agrega folios encontrados al combobox "v_cbx_folios"
                CALL v_cbx_folios.addItem(
                     v_r_glo_folio.folio USING "##########", v_s_cadena)
                -- Contador folio preliquidaciones 
                LET v_i_conLiq = v_i_conLiq + 1
            END FOREACH
            IF(v_i_conLiq< 1 OR v_r_glo_folio.folio IS NULL )THEN
               CALL fn_mensaje("Atención",
                    "No existen liquidaciones recientes","info")
               EXIT INPUT
            END IF
 
         ON ACTION reverso
            LET v_folio = GET_FLDBUF(v_folio) CLIPPED
            IF(v_folio IS NULL)THEN
               CALL fn_mensaje("Atención","Capture folio","information")
               --ERROR "Capture folio" ATTRIBUTE(REVERSE)
               NEXT FIELD v_folio 
            END IF
            IF NOT(v_folio > 0)THEN
               CALL fn_mensaje("Atención","Capture folio válido","information")
               --ERROR "Capture folio válido" ATTRIBUTE(REVERSE)
               NEXT FIELD v_folio 
            END IF
            --Query para verificar si el folio existe
              LET v_query = "\n SELECT folio"
                           ,"\n FROM   glo_folio"
                           ,"\n WHERE  folio = ", v_folio
                           ,"\n AND    proceso_cod = ",g_proceso_cod
                           ,"\n AND    status  =  2"

            PREPARE prp_obtiene_pid_fol FROM v_query

            -- DISPLAY v_query
            EXECUTE prp_obtiene_pid_fol INTO v_tmp_folio 
            --Verificamos el codigo sql para determinar si encontro el folio
            IF( SQLCA.SQLCODE <> 0)THEN
               CALL fn_mensaje("Atención","No existe folio","information")
               --ERROR "No existe folio" ATTRIBUTE(REVERSE)
               NEXT FIELD v_folio 
            END IF

            --Se obtiene el pid del folio
            EXECUTE prp_obtiene_pid_liq USING v_folio, g_proceso_cod
                                           INTO g_pid
            DISPLAY "PID obtenido: ",g_pid
            CALL fn_ventana_confirma("Reverso de la Liquidación"
                                    ,"Se realizara el reverso para el folio "||v_folio
                                    ||"\n¿Desea continuar?" ,"")
                                    RETURNING v_bnd_continuar
            # v_bnd_continuar = 1 --> Aceptar
            # v_bnd_continuar = 2 --> Cancelar
            IF(v_bnd_continuar = 1)THEN
              CALL fn_ret_ejecuta_liquidacion(v_folio, p_usuario_cod)
               EXIT INPUT                
            END IF
            
         ON ACTION salir
            EXIT INPUT

      END INPUT

   CLOSE WINDOW w_reversaIntegracion
END MAIN

{
======================================================================
Clave: 
Nombre: fn_ret_ejecuta_liquidacion
Fecha creacion: Febrero 22, 2012
Autor: Erick Rodriguez, EFP
Narrativa del proceso que realiza:
Ejecuta la liquidacion de Retiro solo infonavit
para un folio dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_ejecuta_liquidacion(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_bandera         SMALLINT, -- para revisar que los procesos ejecutaron bien
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
DEFINE v_program         varchar(50) 
   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "NA"  
   LET v_program        = "RETL04" 
   LET v_bandera = 0
   


      LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETR02 ",
                          p_usuario_cod CLIPPED, " ",
                          g_pid , " " ,    
                          g_proceso_cod , " " ,
                          g_opera_cod ," ",
                          p_folio ," ",
                          v_nombre_archivo CLIPPED," ",
                          " 1>",seg_modulo_bat.ruta_listados CLIPPED  ,
                          "/nohup:",g_pid        USING "&&&&&",":",
                          g_proceso_cod          USING "&&&&&",":",
                          g_opera_cod            USING "&&&&&" ,
                          " 2>&1 &"
                    
                         
       RUN v_s_comando
       CALL fn_mensaje("Atención","Se ha enviado el reverso de liquidación.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","information")

 
END FUNCTION