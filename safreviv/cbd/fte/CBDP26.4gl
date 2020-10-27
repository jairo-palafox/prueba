#################################################################################
#Modulo       => CBD                                                            #
#Programa     => CBDP26                                                         #
#Objetivo     => BLOQUEAR NSS QUE INICIAN CON SIGNO NEGATIVO                    #        
#Fecha_inicio => 01/09/2015                                                     # 
#################################################################################

DATABASE safre_viv 

DEFINE g_arr_nss_invalido DYNAMIC ARRAY OF  RECORD #Arreglo con el que capuramos la información 
     g_id_derechohabiente    decimal(9,0),
     g_nss                   char(11),               #de los nss invalidos
     g_rfc                   char(13),   
     g_a_paterno             char(40),
     g_a_materno             char(40),
     g_nombre                char(40),
     g_s_92_AIVS             DECIMAL(22,6),
     g_s_97_AIVS             DECIMAL(22,6),
     g_nss_marcados          SMALLINT 
END RECORD 



MAIN 
    DEFINE v_ruta                      STRING 
     DEFINE r_resultado_opera          INTEGER
     DEFINE v_nss_marcados             INTEGER 
    #Parametros generales del proceso
    DEFINE p_pid                      DECIMAL(9,0)      -- PID del proceso
    DEFINE p_proceso_cod              SMALLINT          -- codigo del proceso
    DEFINE p_opera_cod                SMALLINT          -- codigo de la operacion
    DEFINE p_usuario_cod              CHAR(20)          -- clave del usuario firmado
    DEFINE v_folio                    DECIMAL(9,0)

    DEFINE v_proceso_desc             CHAR(40)
    DEFINE v_extension                CHAR(10)
    DEFINE v_opera_desc               CHAR(40)
    DEFINE v_layout                   SMALLINT
    DEFINE v_usuario_proceso          CHAR(20)
    DEFINE v_ruta_rescate             STRING
    DEFINE v_ruta_listados            CHAR(40)

    


   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)

    WHENEVER ERROR CONTINUE
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario_proceso 

    #se solicita el numero de folio asociado a la operacion
      CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
      RETURNING v_folio

      #Se actualiza el folio del proceso
      UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid

      UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid
    #Encabezado para el archivo de monitoreo
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

   DISPLAY "Buscando nss negativos..."
   WHENEVER ERROR CONTINUE
   
    CALL fn_marca_nss() 
    WHENEVER ERROR STOP
     DISPLAY ""
     DISPLAY "Termina la ejecucion de la funcion que busca nss negativos"
     DISPLAY ""
     DISPLAY "Lanzando la funcion que genera el archivo con los nss encontrados"

    
    CALL fn_crea_archivo() RETURNING v_ruta
    
     DISPLAY "Termina la ejecucion de la funcion que genera el archivo"
     DISPLAY "El archivo se creo en la ruta: ", v_ruta
     

     # Finaliza la operacion
    CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)

    RETURNING r_resultado_opera
    IF(r_resultado_opera <> 0)THEN         
                     # Actualiza a estado erróneo
                     DISPLAY "Ocurrio un ERROR al intentar actualizar el estado de la operacion..."
                     CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
                     RETURNING r_resultado_opera
                  END IF


    
     DISPLAY "*******************************************************************"
                  DISPLAY ""
                  DISPLAY "Termino la generacion del archivo con NSS bloqueados: "
                  DISPLAY ""
                  DISPLAY " PROCESO            : ",v_proceso_desc
                  DISPLAY " OPERACIÓN          : ",v_opera_desc
                  DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
                  DISPLAY " HORA               : ",TIME(CURRENT)
                  DISPLAY ""
     DISPLAY "*******************************************************************"
     
END MAIN 


FUNCTION fn_marca_nss()--Funcion que carga el arreglo con la información de los nss utilizada para 
                       --crear archivo posteriormente
    

    DEFINE v_query  STRING
    DEFINE v_cont INTEGER 

   


    LET v_query="execute function fn_cbd_marca_nss_negativos()"
                    
    PREPARE prp_function FROM  v_query 
    DECLARE cur_function CURSOR FOR prp_function


    LET v_cont=1

    FOREACH cur_function INTO g_arr_nss_invalido[v_cont].*--Se retorna toda la información 
        LET v_cont = v_cont+1                             --de los nss y se guarda en el arreglo
    END FOREACH                                           --global 

    RETURN 

END FUNCTION 

FUNCTION fn_crea_archivo()--Función que crea el archivo en el que se encontrara la información 
                          --de los nss a marcar
    DEFINE v_nss_informacion     STRING --Se utiliza para concatenar toda la información que se mandara al archivo
    DEFINE channel_1             base.Channel
    DEFINE v_ruta_ch1            STRING--Ruta del archivo a crear 
    DEFINE v_ruta_envio          LIKE seg_modulo.ruta_envio--Ruta del archivo en tipo de variable para consultas
    DEFINE v_cont                INTEGER 
    DEFINE v_aivs                STRING --saldo
    DEFINE v_query               STRING
   LET channel_1=base.Channel.create() 
--Se asigna nombre y ruta del archivo a crear 
   SELECT ruta_envio #Obtenemos la ruta de envio 
    INTO v_ruta_envio
    FROM seg_modulo 
    WHERE modulo_cod = 'cbd'

   # LET v_ruta_ch1=v_ruta_envio CLIPPED ||"/NSS_BLOQUEADOS_"||day(today)||month(today)||year(today)||".bloc"
    LET v_ruta_ch1=v_ruta_envio CLIPPED, "/NSS_BLOQUEADOS_", TODAY USING 'ddmmyyyy'||".bloc"
    

   CALL channel_1.openFile(v_ruta_ch1, "w")
--Se concatena toda la informacón del arreglo en un un string para ser escrito en el archivo 
    LET v_cont=1
    FOR  v_cont=1 TO g_arr_nss_invalido.getLength()-1
        LET v_nss_informacion="|",g_arr_nss_invalido[v_cont].g_nss,"|"
                              ,g_arr_nss_invalido[v_cont].g_rfc CLIPPED ,"|"
                              ,g_arr_nss_invalido[v_cont].g_a_paterno CLIPPED ,"|"
                              ,g_arr_nss_invalido[v_cont].g_a_materno CLIPPED ,"|"
                              ,g_arr_nss_invalido[v_cont].g_nombre CLIPPED 
        LET v_aivs=g_arr_nss_invalido[v_cont].g_s_92_AIVS
        LET  v_nss_informacion=v_nss_informacion,"|",v_aivs CLIPPED   
        LET v_aivs=g_arr_nss_invalido[v_cont].g_s_97_AIVS 
        LET  v_nss_informacion=v_nss_informacion,"|",v_aivs CLIPPED ,"|"
        CALL channel_1.writeLine(v_nss_informacion)
    END FOR 

    CALL channel_1.close()

    LET v_query ="drop table if EXISTS tmp_nss_negativos"--Se borra tabla temporal creada en el script 
                                                         --utilizado anteriomente
    PREPARE prp_drop FROM  v_query 
    EXECUTE prp_drop 
    
    RETURN v_ruta_ch1

END FUNCTION 