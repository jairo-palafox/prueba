################################################################################
#Modulo             => AFI                                                     #
#Programa           => Transferencia de archivos al servidor GWC               #
#                                                                              #
#Objetivo           => Validar archivos antes de subirlos al servidor          #
#                                                                              #
#Autor              => Eduardo Aaron Gaytan Nu�ez                              #
#Fecha inicio       => 07 de Junio2017                                         #
################################################################################

DATABASE safre_viv
    
MAIN

    DEFINE        g_proceso_cod   INTEGER    
    DEFINE        g_opera_cod     INTEGER 
    DEFINE        p_titulo_ventana   STRING  
    DEFINE        p_tipo_ejecucion SMALLINT 
    DEFINE        g_usuario      CHAR(20)
    
   # Se recupera la clave de usuario desde par�metro 
     LET g_usuario        = ARG_VAL(1)        # par�metro recibido por men�
     LET p_tipo_ejecucion = ARG_VAL(2) # par�metro recibido por men�
     LET p_titulo_ventana = ARG_VAL(3) # par�metro recibido por men�
     LET g_proceso_cod    = 1812    -- numero de proceso correspondiente   
     LET g_opera_cod      = 1       -- numero de operacion correspondiente 

-- se crea el archivo log
    CALL STARTLOG(g_usuario CLIPPED|| ".AFIL29.log")
    CLOSE WINDOW SCREEN
    CALL ui.Interface.setText("Carga Archivos") 
    CALL fn_transfiere_archivo (g_proceso_cod, g_opera_cod, g_usuario)
    
END MAIN