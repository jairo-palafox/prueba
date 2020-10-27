################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 24/05/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CNT                                                      #
#Programa          => fn_confirma_poliza_cnt                                   #
#Objetivo          => Carga archivo para confirmación de póliza contable       #
#Fecha inicio      => 18/05/2012                                               #
################################################################################
IMPORT os


GLOBALS "CNTG01.4gl"

GLOBALS 
   DEFINE 
      v_pid    LIKE bat_ctr_proceso.pid
END GLOBALS



MAIN 

   CALL fn_confirma_poliza_cnt()
   
END MAIN

FUNCTION fn_confirma_poliza_cnt ()


   --Define las variables de la función
   DEFINE 
      p_proceso_cod        LIKE cat_proceso.proceso_cod,
      p_opera_cod          LIKE cat_operacion.opera_cod,
      v_folio_proc_cnt     DECIMAL(9,0),
      v_nom_archivo        STRING,
      v_tipo_ejecucion     SMALLINT,
      v_valida             SMALLINT,
      v_comando1           STRING,
      v_comando2           STRING,
      p_ruta_ejecutable    LIKE seg_modulo.ruta_bin,
      p_aux                LIKE seg_modulo.ruta_bin,
      p_ruta_listados      LIKE seg_modulo.ruta_listados,
      p_nom_archivo        STRING,
      p_programa           VARCHAR(30)  


   ##### NOTA: La asignación de parámetros generales depende de los parámetros indicados 
   ##### en módulo proceso batch para la aoperación 1
      
   --Asignación de parametros generales 
   LET g_usuario = ARG_VAL(1)
   LET v_pid = ARG_VAL(2) 
   LET p_proceso_cod = ARG_VAL(3) 
   LET p_opera_cod = ARG_VAL(4)
   LET v_nom_archivo = ARG_VAL(5)
   LET p_programa = ARG_VAL(6) 
   


   LET v_tipo_ejecucion = 2 -- 2 es automático 


   CALL STARTLOG("SAFREVIV.CNTS02.log")

   --Obtiene el último pid generado
   --CALL fn_max_pid(g_proceso_cod2, 1) RETURNING v_pid
   --DISPLAY "pid ",v_pid

   ---## Genera PID ##---
   --CALL fn_genera_pid (g_proceso_cod2, g_opera_cod1,g_usuario)
   --   RETURNING v_pid

   
   --Valida la ejecución de la operación

   
   --CALL fn_valida_operacion(0,g_proceso_cod2,g_opera_cod1) RETURNING v_valida
   LET v_valida = 0
   --CALL fn_muestra_inc_operacion(v_valida)
   CALL fn_desplega_inc_operacion(v_valida)
   IF (v_valida = 0 ) THEN
      DISPLAY "La operación se validó exitosamente"

            --Inicia el proceso
            {CALL fn_inicializa_proceso (v_pid, g_proceso_cod2, g_opera_cod1, 0, 
                                   "CNTS02", "NA", g_usuario)
            RETURNING v_valida
            CALL fn_desplega_inc_operacion(v_valida)}

      
             -- Inicia la operación asignando el estatus de PROCESANDO
            {CALL fn_actualiza_opera_ini(v_pid, --Pod
                                       g_proceso_cod2, --Proceso 
                                       g_opera_cod1, --Operación
                                       0, --Folio
                                       "CNTS02",  --Programa
                                       "NA", --Archivo
                                       g_usuario)
            RETURNING v_valida
            CALL fn_desplega_inc_operacion(v_valida)}

      
            ---## Recupera rutas ##---
            
            CALL fn_rutas("bat") RETURNING p_aux,p_ruta_listados
      

            --Función para la carga del archivo (p_pid, p_proceso, p_operacion, p_tipo_carga, p_programa, p_prog_a_lanzar, p_usuario,p_inicia_proceso)
            DISPLAY "Carga del archivo --> " --, fn_carga_archivo(g_pid,g_proceso_cod2,g_opera_cod1,2,"CNTS02","","",TRUE)
            DISPLAY "v_pid -- ",v_pid
            DISPLAY "g_proceso_cod2 -- ",g_proceso_cod2
            DISPLAY "g_opera_cod1 -- ",g_opera_cod1
            DISPLAY "g_usuario -- ",g_usuario

            
            CALL fn_rutas("cnt") RETURNING p_ruta_ejecutable,p_aux
            
            LET v_comando2 = " fglrun ",p_ruta_ejecutable CLIPPED,"/CNTE02 ", 
                              g_usuario," ",  --Usuario 
                              v_pid, " ",  --Pid
                              g_proceso_cod2, " ", --Proceso
                              g_opera_cod1, " ", --Operacion
                              "NA", " ", --Archivo
                              "NA", " " --Programa a lanzar 
                                
            DISPLAY " Lanzado: ",v_comando2

            CALL fn_rutas("glo") RETURNING p_ruta_ejecutable,p_aux
               LET v_comando1 = " nohup fglrun ",p_ruta_ejecutable CLIPPED,"/GLOE02 ", 
                     g_usuario," ",  --Usuario 
                     v_pid, " ",  --Pid
                     g_proceso_cod2, " ", --Proceso
                     g_opera_cod1, " '", --Operacion
                     v_nom_archivo, "' '", --Archivo
                     v_comando2, "' ", --Programa a lanzar 
                     " 1>", p_ruta_listados CLIPPED ,
                     "/nohup:",v_pid USING "&&&&&",":",
                     g_proceso_cod2 USING "&&&&&",":",
                     g_opera_cod1   USING "&&&&&" ,
                     " 2>&1 &"
            

            DISPLAY "v_comando1 -- :", v_comando1
            
            RUN v_comando1
           { CALL  fn_carga_archivo( v_pid,
                                    g_proceso_cod2,
                                    g_opera_cod1,
                                    v_tipo_ejecucion,
                                    "CNTS02",
                                    "NA", --Comando
                                    g_usuario, -- Usuario
                                    FALSE) -- TRUE se inicializa el proceso
                  RETURNING v_carga_archivo}

         {
                          
						   
			CALL fn_actualiza_opera_fin(v_pid, g_proceso_cod2,g_opera_cod1) RETURNING v_actualiza
			--DISPLAY "Actualiza opera fin -->",v_actualiza
			CALL fn_desplega_inc_operacion(v_actualiza)
			IF v_actualiza = 0 THEN 
				DISPLAY "Se confirmó la póliza a SAP-FICO" 
			ELSE 
				
				DISPLAY "La operación no se validó correctamente"
				CALL fn_error_opera(v_pid,g_proceso_cod2,g_opera_cod1)
                      RETURNING v_actualiza
				CALL fn_desplega_inc_operacion(v_actualiza)
			END IF
						   
			DISPLAY "Fin de la operación"}
   ELSE 
      DISPLAY "Error al validar la operación"
      
         
   END IF 
           
END FUNCTION 

