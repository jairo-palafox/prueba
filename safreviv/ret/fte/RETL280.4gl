################################################################################
#Nombre del Programa => RETL280                                                #
#Programa            => LANZADOR DEL PROGRAMA RETS280 QUE GENERA UN EXTRACTOR  #
#                       DE LOS REGISTROS PAGADOS DE FONDO DE AHORRO, POR EL    #
#                       PROCESO DE FONDO DE AHORRO MASIVO                      #
#Fecha creacion      => 24 DE NOVIEMBRE DEL 2014                               #
#Requerimiento       => PRODINF-552                                            #
#Desarrado por       => FRANCO ESTEBAN ULLOA VIDELA                            #
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
    
    DEFINE
       g_pid                 LIKE bat_ctr_proceso.pid     ,-- ID del proceso
       g_proceso_cod         LIKE cat_proceso.proceso_cod ,-- codigo del proceso
       g_opera_cod           LIKE cat_operacion.opera_cod  -- codigo de operacion
       
       
    DEFINE 
       num_registros         DECIMAL(10,0)
    
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
        
    DEFINE
        v_cadena             STRING
        
    LET p_usuario_cod    = ARG_VAL(1)
    LET p_tipo_ejecucion = ARG_VAL(2)
    LET p_s_titulo       = ARG_VAL(3)

    LET r_bnd_fin_oper = 0
    --Si se obtuvo el titulo, se pone como título de programa--
    IF ( p_s_titulo IS NOT NULL ) THEN
       CALL ui.Interface.setText(p_s_titulo)
    END IF
   
    --Se asigna proceso y operación--
    LET g_proceso_cod = g_proceso_pago_fondo_ahorro_masivo --Variable global definida en el programa RETG01.4gl, valor constante igual a 2604
    LET g_opera_cod   = g_opera_pago_fondo_ahorro_masivo   --Variable global definida en el programa RETG01.4gl, valor constante igual a 1
   
   
    --Valida operacion para verificar si se puede continuar--

    OPEN WINDOW retl2801 WITH FORM "RETL2801"

    MENU --BY NAME num_registros
        BEFORE MENU 
            CALL init() #i
            
            DISPLAY num_registros TO num_registros
            DISPLAY "Num registros", num_registros
            --CALL dialog.setfieldactive("num_registros",0)
 
                   
        ON ACTION ACCEPT
            CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
                 RETURNING v_rest_valida
            
            IF ( v_rest_valida = 0 ) THEN
                CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING g_pid
        
                CALL fn_inicializa_proceso(g_pid         ,
                                           g_proceso_cod ,
                                           g_opera_cod   ,
                                           0             ,
                                           "RETS280"     ,
                                           ""            ,
                                           p_usuario_cod
                                           ) RETURNING v_rest_valida
                                  
               IF v_rest_valida = 0 THEN 
                    CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"RETS280","NA",p_usuario_cod)
                      RETURNING v_rest_valida

                   
                    --Se invoca la ejecucion del programa lanzado--
                    LET v_s_comando = "nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETS280 ",
                                                           p_usuario_cod CLIPPED, " ",
                                                           g_pid                , " " ,
                                                           g_proceso_cod        , " " ,
                                                           g_opera_cod          , " '",
                                                           num_registros        , "' ",
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
            EXIT MENU
            
        ON ACTION CANCEL
            EXIT MENU  
    END MENU 
    CLOSE WINDOW retl2801
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY

    SELECT COUNT(*)
    INTO   num_registros 
    FROM   cta_fondo72 A, ret_fondo_ahorro_masivo C
    WHERE  A.folio_liquida   IN(25314,25334,25969)
    AND    A.movimiento      IN(182) --422 queda fuera de lo contrario se estaría duplicando
    AND    A.id_afi_fondo72   = C.id_afi_fondo72
    AND    C.estado_solicitud = 71

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