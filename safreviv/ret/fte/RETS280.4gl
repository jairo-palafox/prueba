################################################################################
#Nombre del Programa => LANZADO RETS280;  LANZADOR RETL280                     #
#Programa            => PROGRAMA QUE GENERA UN EXTRACTOR DE LOS REGISTROS      #
#                       PAGADOS DE FONDO DE AHORRO, POR EL PROCESO DE FONDO DE #
#                       AHORRO MASIVO                                          #
#Fecha creacion      => 20 DE AGOSTO DEL 2014                                  #
#Desarrado por       => FRANCO ULLOA VIDELA                                    #
#Rquerimiento        => PRODINF-552                                            #
################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
    DEFINE reg_3 RECORD
       ruta_rescate          CHAR(40) ,
       ruta_envio            CHAR(40)
    END RECORD
    
    DEFINE
        v_archivo_salida     CHAR(100)
        
    DEFINE #glo #date
        HOY                  DATE
        
    DEFINE #glo #char
        enter                CHAR(1)
END GLOBALS

MAIN
    DEFINE
       g_pid                 LIKE bat_ctr_proceso.pid     ,-- ID del proceso
       g_proceso_cod         LIKE cat_proceso.proceso_cod ,-- codigo del proceso
       g_opera_cod           LIKE cat_operacion.opera_cod ,-- codigo de operacion
       p_usuario_cod         LIKE seg_usuario.usuario_cod ,-- clave del usuario firmado
       v_rest_valida         SMALLINT
       

    LET p_usuario_cod        = ARG_VAL(1)
    LET g_pid                = ARG_VAL(2)
    LET g_proceso_cod        = ARG_VAL(3)
    LET g_opera_cod          = ARG_VAL(4)

    CALL init()         --Se crea tabla de trabajo en safre_tmp
    CALL primer_paso()  --Se insertan registros en tabla de trabajo
    CALL segundo_paso() --Calcula el saldo de los trabajadores que tuvieron un retiro
    CALL tercer_paso()  --Genera archivo plano

    CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
    RETURNING v_rest_valida
END MAIN

FUNCTION init()
---------------
    SELECT ruta_rescate ,
           ruta_envio
    INTO   reg_3.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"
     
    DATABASE safre_tmp
    WHENEVER ERROR CONTINUE
        DROP TABLE ret_prodinf552;
    WHENEVER ERROR STOP 
     
    CREATE TABLE ret_prodinf552
    (
     id_afi_fondo72      DECIMAL(9,0)  ,
     nss                 CHAR(11)      ,
     f_liquida           DATE          ,
     importe             DECIMAL(12,2) ,
     saldo               DECIMAL(12,2) ,
     movimiento          SMALLINT
    )
    DATABASE safre_viv
END FUNCTION

FUNCTION primer_paso()
---------------------
    INSERT INTO safre_tmp:ret_prodinf552
    SELECT B.id_afi_fondo72 ,
           B.nss            ,
           A.f_liquida      ,
           A.importe        ,
           0                ,
           A.movimiento
    FROM   cta_fondo72 A, afi_fondo72 B, ret_fondo_ahorro_masivo C
    WHERE  A.folio_liquida IN(25314,25334,25969)
    AND    A.movimiento    IN(182,422)
    AND    A.id_afi_fondo72   = B.id_afi_fondo72
    AND    B.id_afi_fondo72   = C.id_afi_fondo72
    AND    C.estado_solicitud = 71
END FUNCTION

FUNCTION segundo_paso()
-----------------------
    DEFINE reg_1 RECORD
        id_afi_fondo72      DECIMAL(9,0)  ,
        nss                 CHAR(11)      ,
        f_liquida           DATE          ,
        importe             DECIMAL(12,2) ,
        saldo               DECIMAL(12,2) ,
        movimiento          SMALLINT
    END RECORD
    
    DECLARE cur_1 CURSOR FOR
    SELECT *
    FROM   safre_tmp:ret_prodinf552
    WHERE  movimiento <> 422 --Para que no calcule el saldo del trabajador dos veces
    
    FOREACH cur_1 INTO reg_1.*
        LET reg_1.saldo = 0
        
        SELECT SUM(importe)
        INTO   reg_1.saldo
        FROM   cta_fondo72
        WHERE  id_afi_fondo72 = reg_1.id_afi_fondo72
        AND    movimiento    <> 422 --Para que no considere el tanto adicional en el saldo del trabajador
        
        UPDATE safre_tmp:ret_prodinf552
        SET    saldo = reg_1.saldo
        WHERE  id_afi_fondo72 = reg_1.id_afi_fondo72
    END FOREACH
END FUNCTION

FUNCTION tercer_paso()
---------------------
    LET v_archivo_salida = reg_3.ruta_envio CLIPPED,"/","SALIDA552.unl"
    
    UNLOAD TO v_archivo_salida
    SELECT nss        ,
           f_liquida  ,
           importe    ,
           saldo      ,
           movimiento 
    FROM   safre_tmp:ret_prodinf552
    ORDER BY 2,1,5

    DISPLAY "El archivo se generó de manera exitosa en la ruta:"
    DISPLAY reg_3.ruta_envio
    DISPLAY "\n"
    DISPLAY "Con el nombre:","SALIDA552.unl"
    DISPLAY "\n"
    DISPLAY "\n"
END FUNCTION
