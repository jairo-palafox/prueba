################################################################################
#Nombre del Programa => RETL279 (RET348) PRODINF-348                           #
#Programa            => GENERA UN EXTRACTOR DE LOS TRABAJADORES PENDIENTES DE  #
#                       PAGO                                                   #
#Fecha creacion      => 25 DE SEPTIEMBRE DEL 2014                              #
#Desarrado por       => FRANCO ESTEBAN ULLOA VIDELA                            #
################################################################################
--IMPORT FGL WSHelper
--IMPORT com
DATABASE safre_viv

GLOBALS "RETG01.4gl"
GLOBALS
    DEFINE g_reg_modulo RECORD
        ruta_exp             CHAR(40),
        ruta_rescate         CHAR(40),
        ruta_listados        CHAR(40)
    END RECORD

    DEFINE seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
    END RECORD
    
    DEFINE 
       g_pid                 LIKE bat_ctr_proceso.pid     , --Id del proceso
       g_proceso_cod         LIKE cat_proceso.proceso_cod , --codigo del proceso
       g_opera_cod           LIKE cat_operacion.opera_cod   --codigo de operacion
       
    DEFINE
       f_solicitud_inicial   DATE ,
       f_solicitud_final     DATE
  
    DEFINE reg_2 RECORD
       nss                   CHAR(11) ,
       id_derechohabiente    DECIMAL(9,0) ,
       monto_acciones        DECIMAL(16,6)
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
        p_usuario_cod        LIKE seg_usuario.usuario_cod ,-- clave del usuario firmado
        p_tipo_ejecucion     SMALLINT                     ,-- forma como ejecutara el programa
        p_s_titulo           STRING                       ,-- titulo de la ventana
        r_bnd_fin_oper       SMALLINT                     ,
        v_rest_valida        SMALLINT

    DEFINE
        v_s_comando          STRING
    
    LET p_usuario_cod    = ARG_VAL(1)
    LET p_tipo_ejecucion = ARG_VAL(2)
    LET p_s_titulo       = ARG_VAL(3)
    
    LET r_bnd_fin_oper = 0

    --Si se obtuvo el título, se pone como título de programa--
    IF ( p_s_titulo IS NOT NULL ) THEN
       CALL ui.Interface.setText(p_s_titulo)
    END IF
    
    --Se asigna proceso y operacion--
        OPEN WINDOW retl2771 WITH FORM "RETL2771"
            INPUT BY NAME f_solicitud_inicial,
                          f_solicitud_final
                          
                BEFORE INPUT
                    CALL init() #i
                    DISPLAY f_solicitud_inicial TO f_solicitud_inicial
                    DISPLAY f_solicitud_final   TO f_solicitud_final

                    
                AFTER FIELD f_solicitud_inicial
                   NEXT FIELD f_solicitud_final
                   
                AFTER FIELD f_solicitud_final
                   NEXT FIELD f_solicitud_inicial
                    
                ON ACTION ACCEPT
                   LET g_proceso_cod = g_proceso_extractor_amortizaciones_pendientes  --Variable global definida en el programa RETG01.4gl, valor constante igual a 2603
                   LET g_opera_cod   = g_opera_extractor_amortizaciones_pendientes    --Variable global definida en el programa RETG01.4gl, valor constante igual a 1

               
                   --Valida operacion para verificar si se puede continuar-- Es absolutamente necesario ejecutar esta función--ff
                   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) --g_pid no tiene asignación de valor--ff
                                            RETURNING v_rest_valida
                   
                   IF ( v_rest_valida = 0 ) THEN
                       CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING g_pid --ff Cómo se donde está esta función?
                       
                       CALL fn_inicializa_proceso(g_pid             ,--ff Mismo caso dónde está esta función?
                                                  g_proceso_cod     ,
                                                  g_opera_cod       ,
                                                  0                 ,
                                                  "RETS279"         ,
                                                  ""                ,
                                                  p_usuario_cod
                                                 ) RETURNING v_rest_valida
                                                 
                       IF v_rest_valida <> 0 THEN 
                           CALL fn_muestra_inc_operacion(v_rest_valida)
                       ELSE
                          CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"RETS277","NA",p_usuario_cod) --Dónde está esta función
                          RETURNING v_rest_valida
                          
                          IF v_rest_valida <> 0 THEN 
                            CALL fn_muestra_inc_operacion(v_rest_valida)
                          ELSE 
                                   --Se invoca la ejecucion del programa lanzado--
                             LET v_s_comando = "nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETS277 ",
                                                              p_usuario_cod CLIPPED, " ",
                                                              g_pid                , " " ,
                                                              g_proceso_cod        , " " ,
                                                              g_opera_cod          , " '",
                                                              f_solicitud_inicial  USING "DDMMYYYY", "' '",
                                                              f_solicitud_final    USING "DDMMYYYY", "' ",
                                                              " 1>",seg_modulo_bat.ruta_listados CLIPPED ,
                                                              "/nohup:",g_pid  USING "&&&&&",":",
                                                              g_proceso_cod    USING "&&&&&",":",
                                                              g_opera_cod      USING "&&&&&" ,
                                                              " 2>&1 &"
                                         
                             DISPLAY v_s_comando
                             RUN v_s_comando
                            CALL fn_mensaje("Atención","Se ha enviado la generación del archivo.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","information")
                          END IF
                       END IF
                   END IF 
                   EXIT INPUT

                ON ACTION CANCEL
                    EXIT INPUT
            END INPUT
        CLOSE WINDOW retl2771
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY
        
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
    
    LET f_solicitud_final   = HOY
    
    DATABASE safre_tmp
    WHENEVER ERROR CONTINUE
        DROP TABLE ret_prodinf423;
    WHENEVER ERROR STOP 
     
    CREATE TABLE ret_prodinf423
    (
     id_dae_referencia   DECIMAL(9,0) ,
     nss                 CHAR(11)     ,
     id_derechohabiente  DECIMAL(9,0) ,
     num_credito         CHAR(10)     ,
     fecha_pago          DATE         ,
     periodo_pago        CHAR(4)      ,
     registro_pago       CHAR(8)      ,
     origen              CHAR(1)      ,
     delegacion          CHAR(2)      ,
     importe_amort       DECIMAL(16,6),
     total_importe       DECIMAL(16,6),
     tipo_pago           CHAR(3)      ,
     entidad_receptora   CHAR(3)      ,
     folio_liquida       DECIMAL(9,0) ,
     fecha_liquida       DATE         ,--Fecha ingreso cuenta
     monto_acciones      DECIMAL(16,6)
    );
    
    DATABASE safre_viv
END FUNCTION

{
REQUISITOS DE INSTALACIÓN
-------------------------
INSERT INTO cat_proceso VALUES (2603,"ret","EXTRACTOR DE LOS TRABAJADORES PENDIENTES DE PAGO","",0,"","","",TODAY,"safreviv");
INSERT INTO cat_operacion VALUES (2603,1,"GENERA EXTRACTOR AMORTIZACIONES PENDIENTES DE PAGO","RETL277",0,1,"","","",1,0,"",0,0);
}