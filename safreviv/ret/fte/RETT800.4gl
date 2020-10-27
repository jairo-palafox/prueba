################################################################################
#Nombre del Programa => RETT800                                                #
#Programa            => PERMITE DESMARCAR TRABAJADORES QUE TIENEN UNA MARCA    #
#                       810-RETIRO POR AMORTIZACIONES EXCEDENTES               #
#Fecha creacion      => 19 DE AGOSTO DEL 2014                                  #
#Desarrado por       => FRANCO ULLOA VIDELA                                    #
################################################################################
DATABASE safre_viv
GLOBALS
    DEFINE reg_1 RECORD
        nss                  CHAR(11)
    END RECORD
    
    DEFINE reg_2 RECORD
        id_derechohabiente   DECIMAL(9,0) ,
        n_referencia         DECIMAL(9,0) ,
        marca_causa          SMALLINT     ,
        proceso_marca        SMALLINT
    END RECORD;
           
    DEFINE #glo #char
        enter                CHAR(1) ,
        v_sql                CHAR(200)
        
    DEFINE
        v_resultado           SMALLINT
        
    DEFINE #glo #date
        HOY                  DATE
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL init() #i
    OPEN WINDOW rett8001 AT 4,4 WITH FORM "RETT8001" ATTRIBUTE(BORDER)
    DISPLAY " <Ctrl-C> Salir                                        <Esc> Para Ejecutar     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETT800       DESMARCA MARCA DE AMORTIZACIONES EXCEDENTES                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME reg_1.nss WITHOUT DEFAULTS

        AFTER FIELD nss
            SELECT id_derechohabiente
            INTO   reg_2.id_derechohabiente
            FROM   afi_derechohabiente    
            WHERE  nss = reg_1.nss
            GROUP BY 1
            
            IF STATUS = NOTFOUND THEN
                ERROR "    NSS NO EXISTE EN BASE DE DATOS DE AFILIADOS" ATTRIBUTE(NORMAL)
                NEXT FIELD nss
            END IF
            
            SELECT "OK"
            FROM   sfr_marca_activa A
            WHERE  A.id_derechohabiente = reg_2.id_derechohabiente
            AND    A.marca              = 810
            GROUP BY 1
            
            IF STATUS = NOTFOUND THEN
                ERROR "    NSS NO TIENE MARCA DE RETIRO POR AMORTIZACIONES EXCEDENTES" ATTRIBUTE(NORMAL)
                NEXT FIELD nss
            END IF
            
            
            SELECT A.n_referencia
            INTO   reg_2.n_referencia
            FROM   sfr_marca_activa A, ret_solicitud_generico B
            where  A.id_derechohabiente = reg_2.id_derechohabiente
            AND    A.marca              = 810
            AND    A.n_referencia       = B.id_solicitud
            
            IF STATUS = NOTFOUND THEN
                ERROR "    EL NSS MARCADO NO TIENE UNA SOLICITUD DE RETIRO DE AMORTIZACIONES ASOCIADA" ATTRIBUTE(NORMAL)
                NEXT FIELD nss
            END IF

        ON KEY (ESC)
            SELECT "OK"  
            FROM   afi_derechohabiente    
            WHERE  nss = reg_1.nss
            GROUP BY 1
            
            IF STATUS = NOTFOUND THEN
                ERROR "    NSS NO EXISTE EN BASE DE DATOS DE AFILIADOS" ATTRIBUTE(NORMAL)
                NEXT FIELD nss
            END IF

            EXIT INPUT

        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

        ON KEY (CONTROL-C)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

    END INPUT

    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

    CALL primer_paso() #pp
    
    PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
    EXIT PROGRAM

    CLOSE WINDOW rett8001
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY
END FUNCTION

FUNCTION primer_paso()
#pp-------------------
 --DESMARCAMOS LA CUENTA-----------------
    LET reg_2.marca_causa        = 0 
    LET reg_2.proceso_marca      = 1516
    
    LET v_sql = "\nEXECUTE FUNCTION fn_desmarca_cuenta(","\n",reg_2.id_derechohabiente, ",",
                                                         "\n 810,"                         ,--RETIRO POR AMORTIZACIONES EXCEDENTES
                                                         "\n",reg_2.n_referencia,","       ,
                                                         "\n 0,"                           ,
                                                         "\n",reg_2.marca_causa,","        ,
                                                         "\n",'"SAFREVIV2"',","            ,
                                                         "\n",reg_2.proceso_marca,")"
                                                           
    PREPARE v_desmarca FROM v_sql
    EXECUTE v_desmarca INTO v_resultado
END FUNCTION
