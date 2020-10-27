################################################################################
#Nombre del Programa => RETT80                                                #
#Programa            => LANZADOR DEL PROGRAMA QUE GENERA UN EXTRACTOR DE LO QUE#
#                       SE TIENE EN SACI DE RETIRO GENÉRICO.                   #
#Fecha creacion      => 20 DE AGOSTO DEL 2014                                  #
#Desarrado por       => FRANCO ULLOA VIDELA                                    #
################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
    DEFINE g_reg_modulo RECORD
        ruta_exp             CHAR(40) ,
        ruta_rescate         CHAR(40) ,
        ruta_listados        CHAR(40)
    END RECORD

    DEFINE seg_modulo_bat RECORD
        ruta_listados        CHAR(40)
    END RECORD
    
    DEFINE g_pid             LIKE bat_ctr_proceso.pid     ,-- ID del proceso
       g_proceso_cod         LIKE cat_proceso.proceso_cod ,-- codigo del proceso
       g_opera_cod           LIKE cat_operacion.opera_cod  -- codigo de operacion
       
       
    DEFINE 
       f_solicitud_inicial   DATE ,
       f_solicitud_final     DATE
    
    DEFINE reg_3 RECORD
       ruta_rescate           CHAR(40) ,
       ruta_envio             CHAR(40)
    END RECORD
    
    DEFINE
        v_archivo_salida      CHAR(100)
        
    DEFINE #glo #date
        HOY                   DATE
        
    DEFINE #glo #char
        enter                 CHAR(1)
END GLOBALS

MAIN
    DEFINE 
        p_usuario_cod         LIKE seg_usuario.usuario_cod ,--clave del usuario firmado
        p_tipo_ejecucion      SMALLINT                     ,--forma como ejecutara el programa
        p_s_titulo            STRING                       ,--titulo de la ventana
        r_bnd_fin_oper        SMALLINT                     ,
        v_rest_valida         SMALLINT

    DEFINE
        v_s_comando          STRING
    DEFINE v_cadena          STRING
        
    --CALL init() #i
    LET p_usuario_cod    = ARG_VAL(1)
    LET p_tipo_ejecucion = ARG_VAL(2)
    LET p_s_titulo       = ARG_VAL(3)

    LET r_bnd_fin_oper = 0
    --Si se obtuvo el titulo, se pone como título de programa--
    IF ( p_s_titulo IS NOT NULL ) THEN
       CALL ui.Interface.setText(p_s_titulo)
    END IF
   
    --Se asigna proceso y operación--
    LET g_proceso_cod = g_proceso_extractor_amortizaciones --Variable global definida en el programa RETG01.4gl, valor constante igual a 2602
    LET g_opera_cod   = g_opera_extractor_amortizaciones   --Variable global definida en el programa RETG01.4gl, valor constante igual a 1
   
   
    --Valida operacion para verificar si se puede continuar--

    OPEN WINDOW retl2781 WITH FORM "RETL2781"

    INPUT BY NAME f_solicitud_inicial,
                  f_solicitud_final
                          
        BEFORE INPUT
            CALL init() #i
            DISPLAY f_solicitud_inicial TO f_solicitud_inicial
            DISPLAY f_solicitud_final TO f_solicitud_final 
 
        AFTER FIELD f_solicitud_inicial
            NEXT FIELD f_solicitud_final
                   
        AFTER FIELD f_solicitud_final
            NEXT FIELD f_solicitud_inicial
                   
        ON ACTION ACCEPT
            CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
                 RETURNING v_rest_valida
            
            IF ( v_rest_valida = 0 ) THEN
                CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING g_pid
        
                CALL fn_inicializa_proceso(g_pid         ,
                                           g_proceso_cod ,
                                           g_opera_cod   ,
                                           0             ,
                                           "RETS278"     ,
                                           ""            ,
                                           p_usuario_cod
                                           ) RETURNING v_rest_valida
                                  
               IF v_rest_valida = 0 THEN 
                  CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"RETS278","NA",p_usuario_cod)
                      RETURNING v_rest_valida

                   
                       --Se invoca la ejecucion del programa lanzado--
                  LET v_s_comando = "nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETS278 ",
                                                         p_usuario_cod CLIPPED, " ",
                                                         g_pid                , " " ,
                                                         g_proceso_cod        , " " ,
                                                         g_opera_cod          , " '",
                                                         f_solicitud_inicial  , "' '",
                                                         f_solicitud_final    , "' ",
                                                         " 1>",seg_modulo_bat.ruta_listados CLIPPED ,
                                                         "/nohup:",g_pid  USING "&&&&&",":",
                                                         g_proceso_cod    USING "&&&&&",":",
                                                         g_opera_cod      USING "&&&&&" ,
                                                         " 2>&1 &"
                  RUN v_s_comando
                       
                  CALL fn_mensaje("Atención","Se ha enviado la generación del archivo.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","information")
               ELSE
                  CALL fn_recupera_inconsis_opera(v_rest_valida) RETURNING v_cadena
                  
                  CALL fn_mensaje("Atención",v_cadena,"information")
               END IF
            ELSE
               CALL fn_recupera_inconsis_opera(v_rest_valida) RETURNING v_cadena
               CALL fn_mensaje("Atención",v_cadena,"information")
            END IF
        EXIT INPUT
    END INPUT
    CLOSE WINDOW retl2781
END MAIN

FUNCTION init()
#i-------------
    LET HOY               = TODAY
    LET f_solicitud_final = HOY
    
    SELECT MIN(f_solicitud)
    INTO   f_solicitud_inicial
    FROM   ret_solicitud_generico
    WHERE  modalidad_retiro = 9 --RETIRO AMORTIZACIONES

    SELECT b.ruta_listados
    INTO   seg_modulo_bat.ruta_listados
    FROM   seg_modulo b
    WHERE  b.modulo_cod = 'bat'

    --Se obtienen las rutas de control del modulo
    SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
    INTO   g_reg_modulo.*
    FROM   seg_modulo s
    WHERE  s.modulo_cod = 'ret'
END FUNCTION