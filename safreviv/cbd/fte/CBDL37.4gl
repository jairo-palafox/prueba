#########################################################################################
#Modulo       => CBD                                                                    #
#Programa     => CBDL37                                                                 #
#Objetivo     => Programa lanzador que preliquida archivo ajuste saldo proceso 2116     #
#Fecha inicio => 11 septiembre 2015                                                     #
#########################################################################################

DATABASE safre_viv

DEFINE v_folio          decimal(9,0)
DEFINE g_pid               LIKE bat_ctr_proceso.pid --  ID del proceso
DEFINE g_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
DEFINE g_opera_cod         LIKE cat_operacion.opera_cod -- codigo de operacion
DEFINE g_opera_cod_ant     LIKE cat_operacion.opera_cod -- codigo de operacion anterior
DEFINE g_reg_modulo        RECORD
        ruta_bin              CHAR(40),
        ruta_rescate          CHAR(40),
        ruta_listados         CHAR(40)--ruta donde se alojan los reportes --/qa/safreviv_lst/glo
END RECORD

DEFINE seg_modulo_bat      RECORD
         ruta_listados        CHAR(40)--//log de bitacora /qa/safreviv_lst/bat
END RECORD

DEFINE g_usuario_cod       CHAR(20)
DEFINE g_tipo_ejecucion    SMALLINT -- forma como ejecutara el programa
DEFINE g_titulo            STRING -- titulo de la ventana

MAIN
   LET g_usuario_cod    = ARG_VAL(1)
   LET g_tipo_ejecucion = ARG_VAL(2)
   LET g_titulo         = ARG_VAL(3)

   --Se obtiene el folio con estado 1 (integrado) para continuar con la preliquidaion
    SELECT folio 
    INTO v_folio
    FROM cbd_ctr_ajuste_saldo
    WHERE estado=1
    
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( g_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF
   
   -- se inicia el log del programa
   CALL STARTLOG (g_usuario_cod CLIPPED||".CBDL37.log")
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = 2116
   LET g_opera_cod   = 3
   LET g_opera_cod_ant = 2

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'cbd'

   SELECT b.ruta_listados--//se recupera la ruta para la bitacora, bat siempre es para las bitacoras
     INTO seg_modulo_bat.ruta_listados
     FROM seg_modulo b
    WHERE b.modulo_cod = 'bat'

    CALL fn_lanza_preliquida_ajuste()  
   
END MAIN


FUNCTION fn_cbd_integracion(p_archivo)--realiza el fgl_run
   DEFINE v_s_comando        STRING -- cadena con una instruccion de consola
   DEFINE v_mensaje          STRING
   DEFINE v_i_resultado      INTEGER -- resultado del proceso
   
   DEFINE v_archivo          STRING
   DEFINE r_resultado_opera  SMALLINT  --CODIGO  DE ERROR fn_actualiza_opera_ini
   DEFINE p_archivo string
   
   LET v_archivo = "NA"
   
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_i_resultado
   
   IF ( v_i_resultado = 0 ) THEN
   
    
     CALL fn_actualiza_opera_ini(g_pid,
                               g_proceso_cod,
                               g_opera_cod,
                               v_folio,
                               "CBDP37",
                               p_archivo,
                               g_usuario_cod)
             RETURNING r_resultado_opera
             
     IF ( r_resultado_opera = 0 ) THEN
        LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_bin CLIPPED,"/CBDP37 ",
                         g_usuario_cod CLIPPED, " ",
                         g_pid                , " ",
                         g_proceso_cod        , " ",
                         g_opera_cod          , " ",
                         v_folio              , " '",
                         p_archivo CLIPPED    , "' ",
                         " 1>",seg_modulo_bat.ruta_listados clipped,
                         "/nohup:",g_pid USING "&&&&&",":",
                         g_proceso_cod   USING "&&&&&",":",
                         g_opera_cod     USING "&&&&&" ,
                         " 2>&1 &"
       RUN v_s_comando
       CALL fn_mensaje("Atención",
                        "Se ha enviado la integración.\nPodrá revisar el resultado en el monitor de ejecución de procesos",
                        "information")
      ELSE                   
      	CALL fn_recupera_inconsis_opera(v_i_resultado) RETURNING v_mensaje
      END IF                   
   
   ELSE
      CALL fn_recupera_inconsis_opera(v_i_resultado) RETURNING v_mensaje--Mensaje de por que no se ejecuta la funcion
      CALL fn_mensaje("Atención", v_mensaje, "stop")
   END IF 
END FUNCTION

--función que mustra la pantalla de cifras generales y ejecuta la función que lanza el programa lanzado
FUNCTION fn_lanza_preliquida_ajuste()
    
    DEFINE v_nom_archivo    VARCHAR(100)
   
    DEFINE v_cont           INTEGER 
    DEFINE v_respuesta      smallint
    
--Arreglo en el que se guardará toda la información general para la forma de preliquidación 
    DEFINE v_arr_info       DYNAMIC ARRAY OF RECORD 
        subcuenta        VARCHAR (100), 
        fondo_inversion  VARCHAR (100), 
        total_acciones   decimal(26,6),
        total_pesos      decimal(22,2)
    END RECORD

 


    
    
    
        --Se obtiene el nombre del archivo que fue integrado 
            SELECT nombre_archivo 
            INTO v_nom_archivo
            FROM glo_ctr_archivo
            WHERE folio=v_folio 
        --Se obtienen datos generales de la tabla cbd_cifras_ajsute_archivo
        --para cargar la forma de preliquidación
           PREPARE prp_info from "SELECT cbd.subcuenta||' - '|| cat.subcuenta_desc sub,
                                         cbd.fondo_inversion ||' - '|| fondo.razon_social fond,
                                         cbd.total_acciones,
                                         cbd.total_pesos
                                  FROM cbd_cifras_ajuste_saldo cbd, 
                                       cat_subcuenta cat, 
                                       cat_fondo_local fondo
                                  WHERE cbd.folio=? and 
                                        cat.subcuenta=cbd.subcuenta and 
                                        cbd.fondo_inversion=fondo.fondo
                                        order by sub,fond"

            DECLARE cur_info CURSOR FOR prp_info

            LET v_cont=1
            
            FOREACH cur_info USING v_folio INTO v_arr_info[v_cont].*
                LET v_cont=v_cont+1
            END FOREACH 
            CLOSE WINDOW SCREEN 
            OPEN WINDOW Preliquida WITH FORM "CBDL371" 
            IF v_folio IS NULL THEN --Se revisa que exista un folio con estado 1 (integrado) para poder preliquidar
                CALL fgl_winmessage("Aviso","No se ncontro ningún archivo para preliquidar","Next")
            ELSE 

                    DISPLAY BY NAME v_folio
                    DISPLAY BY NAME v_nom_archivo
                    
                    INPUT   ARRAY  v_arr_info FROM  record1.* ATTRIBUTES (UNBUFFERED,  WITHOUT DEFAULTS,
                                                                                                    INSERT ROW =false,
                                                                                                    DELETE row=false,
                                                                                                    accept=false,
                                                                                                    append row=FALSE,
                                                                                                    CANCEL = false)
                            BEFORE INPUT 
                                EXIT INPUT
                        
                    END  INPUT 

                    MENU 
                        ON ACTION Aceptar
                            --Solicita confirmar(1) o cancelar(0) la operación de Registro
                             CALL fn_ventana_confirma("Atención","¿Desea ejecutar el proceso de Preliquidación?","quest") RETURNING v_respuesta
                            --1 aceptar, 0 cancelar ... glog01.42x fn_ventana_confirma
                            IF ( v_respuesta = 1 ) THEN
                               CALL fn_cbd_integracion(v_nom_archivo)
                               EXIT MENU
                            ELSE 
                                CALL fgl_winmessage("Aviso","No se ejecuto la preliquidación","Next")
                                EXIT MENU 
                            END IF 
                        on ACTION Cancelar 
                            EXIT MENU 

                    END MENU 
               END IF 
     
    CLOSE WINDOW Preliquida 
    
    

END FUNCTION 