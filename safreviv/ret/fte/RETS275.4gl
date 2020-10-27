###############################################################################
#Nombre del Programa => RETT801                                                #
#Programa            => GENERA UN EXTRACTOR DE LO QUE SE TIENE EN SACI DE      #
#                       RETIROS GENÉRICO. SOLICITA UN RANGO DE FECHA           #
#Fecha creacion      => 20 DE AGOSTO DEL 2014                                  #
#Desarrado por       => FRANCO ULLOA VIDELA                                    #
################################################################################
DATABASE safre_viv
GLOBALS
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
    CALL init() #i
    OPEN WINDOW rets2751 WITH FORM "RETS2751"
        INPUT BY NAME f_solicitud_inicial,
                      f_solicitud_final
                      
            BEFORE INPUT
                --LET f_solicitud_inicial = f_solicitud_inicial
                --LET f_solicitud_final   = f_solicitud_finall
                
            ON ACTION ACCEPT 
                CALL primer_paso()
        END INPUT
    CLOSE WINDOW rets2751
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY
    
    SELECT MIN(f_solicitud)
    INTO   f_solicitud_inicial
    FROM   ret_solicitud_generico
    WHERE  modalidad_retiro = 9 --RETIRO AMORTIZACIONES
    
    LET f_solicitud_final = HOY
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