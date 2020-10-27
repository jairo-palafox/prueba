###############################################################################
#Nombre del Programa => RETT801                                                #
#Programa            => GENERA UN EXTRACTOR DE LO QUE SE TIENE EN SACI DE      #
#                       RETIROS GENÉRICO. SOLICITA UN RANGO DE FECHA           #
#Fecha creacion      => 20 DE AGOSTO DEL 2014                                  #
#Desarrado por       => FRANCO ULLOA VIDELA                                    #
################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
    DEFINE g_pid             LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod         LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod           LIKE cat_operacion.opera_cod -- codigo de operacion
       
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
        p_usuario_cod         LIKE seg_usuario.usuario_cod , -- clave del usuario firmado
        p_tipo_ejecucion      SMALLINT                     , -- forma como ejecutara el programa
        p_s_titulo            STRING                       ,-- titulo de la ventana
        r_bnd_fin_oper        SMALLINT                     ,
        v_rest_valida         SMALLINT
       
    --CALL init() #i
    LET p_usuario_cod    = ARG_VAL(1)
    LET p_tipo_ejecucion = ARG_VAL(2)
    LET p_s_titulo       = ARG_VAL(3)

    LET r_bnd_fin_oper = 0
    -- si se obtuvo el titulo, se pone como titulo de programa
    IF ( p_s_titulo IS NOT NULL ) THEN
       CALL ui.Interface.setText(p_s_titulo)
    END IF
   
    -- se asigna proceso y operacion
    LET g_proceso_cod = g_proceso_extractor_amortizaciones
    LET g_opera_cod   = g_opera_extractor_amortizaciones
   
   
    -- Valida operacion para verificar si se puede continuar.
    CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
                             RETURNING v_rest_valida

    IF ( v_rest_valida = 0 ) THEN
        CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING g_pid
        
        CALL fn_inicializa_proceso(g_pid             ,
                                 g_proceso_cod     ,
                                 g_opera_cod       ,
                                 0           ,
                                 "RETS276"          ,
                                 ""  ,
                                 p_usuario_cod)  RETURNING v_rest_valida
        IF v_rest_valida <> 0 THEN 
            DISPLAY "el resultado del llamado termino con error", v_rest_valida
        END IF 
        CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"RETS276","NA",p_usuario_cod)
        RETURNING v_rest_valida

        OPEN WINDOW rets2761 WITH FORM "RETS2761"
            INPUT BY NAME f_solicitud_inicial,
                          f_solicitud_final
                          
                BEFORE INPUT
                    CALL init() #i
                    DISPLAY "HOY ",HOY
                    DISPLAY f_solicitud_inicial TO f_solicitud_inicial
                    DISPLAY f_solicitud_final TO f_solicitud_final 
                    
                ON ACTION ACCEPT 
                    CALL primer_paso()
                    CALL fn_mensaje("Atención","Se ha enviado la generación del archivo.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","information")
                    EXIT INPUT
            END INPUT
        CLOSE WINDOW rets2761
        CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
        RETURNING v_rest_valida
    END IF
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY
    LET f_solicitud_final = HOY
    
    SELECT MIN(f_solicitud)
    INTO   f_solicitud_inicial
    FROM   ret_solicitud_generico
    WHERE  modalidad_retiro = 9 --RETIRO AMORTIZACIONES
    
    --LET f_solicitud_final = HOY
END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    SELECT ruta_rescate ,
           ruta_envio
    INTO   reg_3.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"
     
    DATABASE safre_tmp
    WHENEVER ERROR CONTINUE
        DROP TABLE ret_prodinf275;
    WHENEVER ERROR STOP 
     
    CREATE TABLE ret_prodinf275
    (
     id_solicitud        DECIMAL(9,0)  ,
     modalidad_retiro    SMALLINT      ,
     des_modalidad       CHAR(20)      ,
     nss                 CHAR(11)      ,
     rfc                 CHAR(13)      ,
     caso_adai           CHAR(10)      ,
     folio_liquida       DECIMAL(9,0)  ,
     f_solicitud         DATE          ,
     monto_acciones      DECIMAL(16,6) ,
     monto_pesos         DECIMAL(12,2) ,
     estado_solicitud    SMALLINT      ,
     cod_rechazo         SMALLINT      ,
     f_extraccion        DATE
    )
    ;
    DISPLAY "hola ", f_solicitud_inicial," ",f_solicitud_final
    DATABASE safre_viv
    INSERT INTO safre_tmp:ret_prodinf275
    SELECT A.id_solicitud     ,
           A.modalidad_retiro ,
           B.des_corta        ,--Descripción de la modalidad
           A.nss              ,
           A.rfc              ,
           A.caso_adai        ,
           C.folio_liquida    ,
           A.f_solicitud      ,
           C.monto_acciones   ,
           C.monto_pesos      ,
           A.estado_solicitud ,
           A.cod_rechazo      ,
           TODAY
    FROM   ret_solicitud_generico A, ret_modalidad_retiro B, OUTER cta_movimiento C
    WHERE  A.modalidad_retiro   = 9 --RETIRO AMORTIZACIONES
    AND    A.modalidad_retiro   = B.modalidad_retiro
    AND    A.id_derechohabiente = C.id_derechohabiente
    AND    A.f_solicitud  BETWEEN f_solicitud_inicial AND f_solicitud_final
    AND    C.subcuenta          = 46   --AMORTIZACIÓN EXCEDENTE
    AND    C.movimiento         = 1402 --CARGO RETIRO AMORTIZACIÓN EXCEDENTE 
    
    
    
    LET v_archivo_salida = reg_3.ruta_envio CLIPPED,"/","SACIRGAE_09.dae"
    
    UNLOAD TO v_archivo_salida
    SELECT *
    FROM   safre_tmp:ret_prodinf275
    
END FUNCTION