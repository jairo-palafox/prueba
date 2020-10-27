#########################################################################################################
#Modulo       =>NOT                                                                                     #
#Programa     => NOTL06                                                                                 #
#Objetivo     => Pantalla que permite enviar archivos de notificaciones que no han sido enviados a CRM  #        
#Fecha_inicio => 17/08/2015  
#Ultima modificación =>19/01/2016                                                                           # 
#########################################################################################################

IMPORT os
DATABASE safre_viv

DEFINE g_arr_informacion_archivo DYNAMIC ARRAY OF  RECORD #Arreglo con el que capturamos los datos de los archivos
     g_folio                 DECIMAL (9,0),
     g_folio_operativo       DECIMAL(9,0),
     g_proceso_notificado    VARCHAR(100),
     g_f_proceso             DATE,
     g_enviar                BOOLEAN #Variable que identifica los archivos que fueron seleccionados por el usuario  
    
END RECORD 

   DEFINE glo_pid             LIKE bat_ctr_proceso.pid  -- ID del proceso
   DEFINE glo_folio           DECIMAL(9,0)  -- parametro del folio que se genera con la la funcion glo_folio
   DEFINE glo_proceso_cod     INTEGER     
   DEFINE glo_operacion_cod   INTEGER

   DEFINE v_usuario        VARCHAR(30)
   DEFINE v_tipo_proceso   SMALLINT -- Forma como ejecutara el programa 
   DEFINE v_nom_prog       VARCHAR(30) -- Almacena opción del menú 
   
      
   

 
MAIN
    DEFINE v_resp           BOOLEAN #bandera que identifica si ocurrio algun error en la función fn_carga_nombre_archivos()  
    DEFINE v_cont           BOOLEAN  #identifica que se hayan seleccionado almenos un archivo
    CLOSE WINDOW SCREEN 
    OPEN WINDOW vtn_notl061 WITH FORM "NOTL061"

    -- se asignan los parametros que vienen del fglrun
   LET v_usuario       = ARG_VAL(1)
   LET v_tipo_proceso = ARG_VAL(2)
   LET v_nom_prog     = ARG_VAL(3)
   
   -- se asigna el titulo del programa
   IF ( v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_nom_prog)
   END IF

            input ARRAY g_arr_informacion_archivo  FROM  Record1.* ATTRIBUTES (UNBUFFERED,  WITHOUT DEFAULTS,
                                                                                        INSERT ROW =false,
                                                                                        DELETE row=false,
                                                                                        accept=false,
                                                                                        append row=FALSE,
                                                                                        CANCEL = false)
            BEFORE INPUT 

                
                CALL fn_carga_array() RETURNING v_resp  #Función que carga los datos del archivo al arreglo
                IF v_resp!=0 THEN #Evaluación del buen funcionamiento de la función fn_carga_nombre_archivos 
                  EXIT INPUT   
                END IF
            BEFORE FIELD Edit1, Edit2,Edit3,Edit4
                NEXT  FIELD Edit5             
            ON ACTION Salir 
                EXIT INPUT
            ON ACTION Enviar 
                CALL fn_update_bd() RETURNING v_cont 
                IF v_cont  THEN
                    CALL fn_lanzado_envio()
                    CALL fgl_winmessage("Aviso","Se a enviado a ejecutar el proceso de envío de archivos","Next")
                    EXIT INPUT
                ELSE 
                    CALL fgl_winmessage("Aviso","No se ha seleccionando ningún campo","Next")
                END IF 
                
            END input 
    CLOSE WINDOW vtn_notl061 
END MAIN 


FUNCTION fn_carga_array()--Función que carga el arreglo con la información que será mostrada en la pantalla
    DEFINE v_ruta_envio             string ----Variable para capturar la ruta de los archivos
    DEFINE v_sql_qry                STRING --Query que busca el nombre del archivo en la tabla 
    DEFINE v_cont                   SMALLINT 
    LET v_cont=1
    LET v_sql_qry="SELECT
                          ctr.folio_notifica,
                          ctr.folio_operativo,
                          cat.proceso_desc,
                          ctr.f_proceso,
                          0
                   FROM    cat_proceso cat,
                           not_ctr_archivo ctr,
                           glo_folio glo
                   WHERE  ctr.estado = 1 
                          AND glo.folio = ctr.folio_notifica
                          AND glo.proceso_cod = cat.proceso_cod "
    DECLARE cur_arch CURSOR  From v_sql_qry
    FOREACH cur_arch INTO g_arr_informacion_archivo[v_cont].g_folio,
                          g_arr_informacion_archivo[v_cont].g_folio_operativo,
                          g_arr_informacion_archivo[v_cont].g_proceso_notificado,
                          g_arr_informacion_archivo[v_cont].g_f_proceso,
                          g_arr_informacion_archivo[v_cont].g_enviar
        LET v_cont=v_cont+1
    END FOREACH 
    CALL g_arr_informacion_archivo.deleteElement(g_arr_informacion_archivo.getLength())
   RETURN 0 #Se ejecuto correctamente la función 
END FUNCTION 

FUNCTION fn_update_bd()
    DEFINE v_respuesta          BOOLEAN  
    DEFINE v_cont               INTEGER
    LET v_respuesta=0 
    FOR v_cont=1 TO g_arr_informacion_archivo.getLength()
        IF g_arr_informacion_archivo[v_cont].g_enviar THEN 
            UPDATE not_ctr_archivo
            SET estado=2
            WHERE folio_notifica=g_arr_informacion_archivo[v_cont].g_folio AND
                  folio_operativo=g_arr_informacion_archivo[v_cont].g_folio_operativo
            LET v_respuesta=1
        END IF 
    END FOR 
    
    RETURN v_respuesta
END FUNCTION 



FUNCTION fn_lanzado_envio()
   DEFINE v_ruta_bin   LIKE seg_modulo.ruta_bin               -- Ruta del ejecutable
    DEFINE v_bandera        SMALLINT
    DEFINE v_resultado      SMALLINT  

    DEFINE v_pid               LIKE bat_ctr_proceso.pid -- ID del proceso
    DEFINE v_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
    DEFINE v_opera_cod         LIKE cat_operacion.opera_cod -- codigo de operacion
    DEFINE v_folio             LIKE glo_ctr_archivo.folio
    DEFINE v_usuario             VARCHAR(30) -- Almacena al usuario

    DEFINE v_ruta_ejecutable   LIKE seg_modulo.ruta_bin -- Ruta del ejecutable
    DEFINE v_ruta_listados   LIKE seg_modulo.ruta_listados -- Rute del log
   
    DEFINE r_resultado_opera   INTEGER
    DEFINE v_nom_archivo       CHAR(40)
    DEFINE v_comando            STRING 

     --Obtiene las rutas ejecutable
   SELECT ruta_bin
     INTO v_ruta_ejecutable
   FROM seg_modulo 
   WHERE modulo_cod = 'not'

   --Obtiene ruta listados
   SELECT ruta_listados
     INTO v_ruta_listados
   FROM seg_modulo 
   WHERE modulo_cod = 'bat'

   LET v_proceso_cod = 2918
   LET v_opera_cod = 1
   LET v_usuario = 'safreviv'
   LET v_folio = 1
   LET v_nom_archivo = 'prueba'

   # se valida si se puede generar el proceso
   CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) RETURNING r_resultado_opera
   IF ( r_resultado_opera <> 0 ) THEN
      DISPLAY "No paso la operacion"
      CALL fn_muestra_inc_operacion(r_resultado_opera)
   ELSE
      # se genera el pid para el proceso
      CALL fn_genera_pid(v_proceso_cod,v_opera_cod,v_usuario)
             RETURNING v_pid

      CALL fn_inicializa_proceso(v_pid,v_proceso_cod,v_opera_cod,0,
                                             "NOTP06",v_nom_archivo,v_usuario)
                                    RETURNING r_resultado_opera
      IF ( r_resultado_opera <> 0 ) THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      ELSE
         # Inicia operación
         CALL fn_actualiza_opera_ini(v_pid,v_proceso_cod,v_opera_cod,null,"NOTP06",
                               v_nom_archivo,v_usuario) RETURNING r_resultado_opera
         # En el caso de que exista una inconsistencia al iniciar el proceso, se
         # Muestra un mensaje con la descripcion
         IF(r_resultado_opera)THEN
            CALL fn_muestra_inc_operacion(r_resultado_opera)
         ELSE
            #se solicita el numero de folio asociado a la operacion
            CALL fn_genera_folio(v_proceso_cod,v_opera_cod,v_usuario)
            RETURNING v_folio

            #Se actualiza el folio del proceso
            UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = v_pid

            UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = v_pid
            
            LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/NOTP06.42r ",
                                                v_usuario," ",
                                                v_pid," ",
                                                v_proceso_cod," ",
                                                v_opera_cod," ",
                                                v_folio," '",
                                                v_nom_archivo,
                                                "' 1>", v_ruta_listados CLIPPED ,
                                                "/nohup:",v_pid USING "&&&&&",":",
                                                         v_proceso_cod USING "&&&&&",":",
                                                         v_opera_cod USING "&&&&&" ," 2>&1 &"

            DISPLAY v_comando                        
            RUN v_comando
            IF(STATUS)THEN
               DISPLAY "Envío de archivos de notificación", 
                               "Ocurrió un error al iniciar el proceso batch"
            ELSE
               # Se indica que se realizo el proceso de carga
               DISPLAY "Envío de archivos de notificación", 
                               "Se ha iniciado el proceso batch. Podrá revisar el detalle\nen el monitoreo de procesos para el pid " ||v_pid
            END IF

         END IF 
      END IF 
    END IF 
   
   LET glo_folio = 1
   LET v_comando = "nohup fglrun ",v_ruta_bin CLIPPED,"/NOTP06.42r"
   DISPLAY v_comando                        
   RUN v_comando
                
END FUNCTION 
