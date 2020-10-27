DATABASE safre_viv

MAIN 
    DEFINE v_display STRING 
    DEFINE arr_prueba DYNAMIC ARRAY OF RECORD 
        nss char(11),
        id_derechohabiente DECIMAL(9,0)
    end RECORD
    DEFINE v_query STRING 
    DEFINE v_cont SMALLINT 
    LET v_cont=1
    LET v_query="Select first 10 nss, id_derechohabiente from afi_derechohabiente"
    
   PREPARE  prp_afi FROM  v_query
    DECLARE cur_afi CURSOR FOR prp_afi

    FOREACH cur_afi INTO arr_prueba[v_cont].*
        LET v_cont=v_cont +1
    END FOREACH 

    

    close WINDOW SCREEN 
    OPEN WINDOW Prueba WITH FORM "CBD-Prueba1"
        MENU 
            ON ACTION display
                LET v_display="Name of your tears"
                DISPLAY v_display TO edit1
            ON ACTION CANCEL 
                EXIT MENU 
            ON ACTION afi
                DISPLAY ARRAY arr_prueba to record2.* ATTRIBUTES( UNBUFFERED=1)
        
                END DISPLAY  
        END MENU 
    CLOSE WINDOW Prueba 

END MAIN 