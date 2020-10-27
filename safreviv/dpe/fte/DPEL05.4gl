--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/04/2012
--===============================================================

#########################################################################################
#Modulo       => DPE                                                                    #
#Programa     => DPEL05                                                                 #
#Objetivo     => Programa que hace genera resumen de cifras control de archivo de salida#
#                para PROCESAR con datos históricos de folios dentro de un periodo      #
#Fecha inicio => Marzo 15, 2012                                                         #
#Autor        => Jose Soto                                                              #
#########################################################################################
GLOBALS "DPEG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod,-- codigo de operacion
       g_reg_modulo   RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados CHAR(40)
       END RECORD,
       w          ui.window,
       f          ui.form,
       r_bnd_fin_oper SMALLINT,
       v_rest_valida  SMALLINT 
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- se asigna proceso y operacion
   --LET g_proceso_cod = 1001 -- devolucion por errores de operacion
   LET g_proceso_cod = g_proceso_cod_dpe_procesar_gen
   LET g_opera_cod   = 1 -- genera archivo procesar
   
   CALL fn_genera_resumen_procesar(p_usuario_cod)
END MAIN

{
======================================================================
Clave: 
Nombre: fn_genera_resumen_procesar
Fecha creacion: Febrero 28, 2012
Autor: Jose Soto
Narrativa del proceso que realiza:
Genera resumen del archivo a generar para procesar
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_genera_resumen_procesar(p_usuario_cod)
   DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          v_d_inicial, -- Fecha inicial del periodo para el archivo de salida 
          v_d_final     DATE, -- Fecha final del periodo para el archivo de salida 
          v_s_sql_folio VARCHAR(1000),
          v_i_indice    SMALLINT
          

   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".DPEL05.log")
  
   -- Ventana principal para ingresar parametros para generar el archivo de salida
   OPEN WINDOW vtn_periodo_salida WITH FORM "DPEL050"
      CLOSE WINDOW SCREEN
      LET w = ui.Window.getCurrent()
      LET f = w.getForm()   
      LET INT_FLAG = FALSE
      -- Oculta grupos folio a generar y resumen
      CALL f.setElementHidden("grupo_folios",1)
      CALL f.setElementHidden("grupo_resumen",1)

      INPUT v_d_inicial, v_d_final --Rango de fechas a procesar para generar el archivo
         FROM f_inicial, f_final ATTRIBUTES (UNBUFFERED)
      
      BEFORE INPUT

         AFTER FIELD f_inicial
            IF v_d_inicial IS NULL THEN
               CALL fn_mensaje("Atención","Ingrese una fecha inicial para generar","info")
               NEXT FIELD f_inicial
            END IF
         
         AFTER FIELD f_final
            IF v_d_final IS NULL THEN
               CALL fn_mensaje("Atención",
                    "Ingrese una fecha final para generar","info")
               NEXT FIELD f_final
            ELSE 
               IF v_d_final > TODAY THEN 
                  CALL fn_mensaje("Atención",
                    "La fecha final no puede ser posterior al día de hoy","info")
               NEXT FIELD f_final
               END IF     
            END IF   
         
         ON ACTION ACCEPT -- Genera el resumen del archivo a generar
            IF v_d_inicial IS NULL THEN
               CALL fn_mensaje("Atención",
                    "Ingrese una fecha inicial para generar","info")
               NEXT FIELD f_inicial
            END IF
            
            IF v_d_final IS NULL THEN
               CALL fn_mensaje("Atención",
                    "Ingrese una fecha final para generar","info")
               NEXT FIELD f_final
            ELSE 
               IF v_d_final > TODAY THEN 
                  CALL fn_mensaje("Atención",
                    "La fecha final no puede ser posterior al día de hoy","info")
               NEXT FIELD f_final
               END IF  
            END IF
            
            CALL f.setElementHidden("grupo_folios",0)
            CALL f.setElementHidden("grupo_resumen",0)
            CALL fn_folios_disponibles(p_usuario_cod, v_d_inicial, v_d_final) RETURNING v_i_indice
            LET INT_FLAG = FALSE
            IF v_i_indice = 0 THEN
               LET INT_FLAG = TRUE
               CALL f.setElementHidden("grupo_folios",1)
               CALL f.setElementHidden("grupo_resumen",1)
               CONTINUE INPUT
            END IF	
            
         ON ACTION CANCEL
            LET INT_FLAG = TRUE
            CALL f.setElementHidden("grupo_folios",1)
            CALL f.setElementHidden("grupo_resumen",1)
            EXIT INPUT
            	
      END INPUT
   CLOSE WINDOW vtn_periodo_salida -- Cierra pantalla principal
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_folios_disponibles
Fecha creacion: Marzo 21, 2012
Autor: Jose Soto
Narrativa del proceso que realiza:
Verifica que existan folios disponibles para envio a PROCESAR
dentro de la fecha de consulta
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}

FUNCTION fn_folios_disponibles(p_usuario_cod, p_d_inicial, p_d_final)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_d_inicial, -- Fecha inicial del periodo para el archivo de salida 
       p_d_final   DATE, -- Fecha final del periodo para el archivo de salida
       v_folio_seleccionado STRING,  
       v_r_folios_disponibles RECORD
          v_s_folio          LIKE glo_folio.folio,
          v_s_folio_liquida  LIKE dpe_sol_trab_parcial.folio_liquida,
          v_c_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo,
          v_s_status         LIKE glo_folio.status,
          v_f_actualiza      LIKE glo_folio.f_actualiza
      END RECORD,
      v_ar_folios_disponibles DYNAMIC ARRAY OF RECORD -- arreglo para desplegar consulta
          v_s_folio          LIKE glo_folio.folio,
          v_s_folio_liquida  LIKE dpe_sol_trab_parcial.folio_liquida,
          v_c_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo,
          v_s_status         LIKE glo_folio.status,
          v_f_actualiza      LIKE glo_folio.f_actualiza,
          v_s_disponible     SMALLINT
      END RECORD,	
       v_s_sql       STRING, -- cadena con una instruccion SQL
       v_si_indice   SMALLINT, -- indice de arreglo
       cont_marcados SMALLINT, -- indice, conteo de folios marcados
       v_s_sql_folio VARCHAR(1000),
       v_i_sol_patronales   SMALLINT, -- Contador de numero de solicitudes por periodo
       v_i_reg_trabajadores SMALLINT, -- Contador de numero de registro de trabajadores
       v_i_tot_registros    SMALLINT, -- Contador de numero de registro de registros
       v_d_tot_vivienda     DECIMAL(16,2), -- Contador de numero de vivienda
       v_d_tot_avis         DECIMAL(16,2), -- Contador de numero de avis
       v_d_inicial, -- Fecha inicial del periodo para el archivo de salida 
       v_d_final     DATE, -- Fecha final del periodo para el archivo de salida 
       v_valida_diagnostico SMALLINT
DEFINE buf base.StringBuffer
       
   LET v_s_sql =  "SELECT DISTINCT t.folio, t.folio_liquida,",
                  "\n             b.nombre_archivo, g.status, g.f_actualiza",
                  "\n        FROM dpe_sol_trab_parcial t, dpe_preliquida p,",
                  "\n             glo_ctr_archivo b, glo_folio g",
                  "\n       WHERE t.folio_liquida = p.folio_liquida",
                  "\n         AND b.folio = t.folio",
                  "\n         AND g.folio = t.folio",
                  "\n         AND g.proceso_cod = ",g_proceso_cod_dpe_disposicion,
                  "\n         AND g.f_actualiza BETWEEN ","'",p_d_inicial,"'",
                  "\n         AND ","'", p_d_final,"'"
   
   --DISPLAY "Folios Disponibles: ", v_s_sql CLIPPED
   PREPARE Prpr_ObtFolios_Disponibles FROM v_s_sql CLIPPED
   DECLARE Curr_ObtFolios_Disponibles CURSOR FOR Prpr_ObtFolios_Disponibles
   
   LET v_i_tot_registros = 0
   LET v_i_sol_patronales = 0
   LET v_i_reg_trabajadores = 0
   LET v_d_tot_vivienda = 0
   LET v_d_tot_avis = 0
   
   LET v_si_indice = 0

   LET buf = base.StringBuffer.create()
   
   -- Llena el arreglo que se mostrara en pantalla
   FOREACH Curr_ObtFolios_Disponibles INTO v_r_folios_disponibles.*
      -- se incrementa el indice
      LET v_si_indice = v_si_indice + 1
      
      LET v_ar_folios_disponibles[v_si_indice].v_s_folio = 
                                               v_r_folios_disponibles.v_s_folio
      
      LET v_ar_folios_disponibles[v_si_indice].v_s_folio_liquida = 
                                               v_r_folios_disponibles.v_s_folio_liquida
      
      LET v_ar_folios_disponibles[v_si_indice].v_c_nombre_archivo = 
                                               v_r_folios_disponibles.v_c_nombre_archivo
      
      LET v_ar_folios_disponibles[v_si_indice].v_s_status = 
                                               v_r_folios_disponibles.v_s_status
      
      LET v_ar_folios_disponibles[v_si_indice].v_f_actualiza = 
                                               v_r_folios_disponibles.v_f_actualiza
      
   END FOREACH
   FREE Curr_ObtFolios_Disponibles
   
   -- Valida si es que existe información
   IF v_si_indice = 0 THEN
      CALL v_ar_folios_disponibles.clear()
      CALL fn_mensaje("Atención",
           "No se encontraron datos con los criterios indicados",
           "information")
         RETURN v_si_indice
   END IF

   -- Despliega los datos obtenidos en la consulta

      DIALOG ATTRIBUTES(UNBUFFERED)
         INPUT ARRAY v_ar_folios_disponibles
         	FROM tb_folios_disponibles.* ATTRIBUTES(INSERT ROW = FALSE, APPEND ROW = FALSE, 
         	                                        DELETE ROW = FALSE)
         BEFORE INPUT
         	  CALL DIALOG.setActionHidden("close",1)
            
         --ON ACTION Seleccionar
         --   FOR cont_marcados = 1 TO v_si_indice
         --    LET v_ar_folios_disponibles[cont_marcados].v_s_disponible =  1
         --  END FOR
         --  CONTINUE DIALOG
         --      
         --ON ACTION Deseleccionar
         --   FOR cont_marcados = 1 TO v_si_indice
         --    LET v_ar_folios_disponibles[cont_marcados].v_s_disponible =  0
         --  END FOR
         --  CONTINUE DIALOG
         
         ON CHANGE v_s_disponible
            LET v_s_sql_folio = ""
            LET v_folio_seleccionado = ""
         	  FOR cont_marcados = 1 TO v_si_indice
         	     IF v_ar_folios_disponibles[cont_marcados].v_s_disponible =  1 THEN
         	        --Asigna valor de arreglo a variable 
                    LET v_folio_seleccionado = v_ar_folios_disponibles[cont_marcados].v_s_folio CLIPPED
                    
                    CALL buf.append(v_folio_seleccionado)
                    CALL buf.trimleft()
                    
                    --Concatena valores 
                    LET v_s_sql_folio = v_s_sql_folio CLIPPED,v_folio_seleccionado CLIPPED,","
                        
                    CALL buf.append(v_s_sql_folio)
                    CALL buf.trimleft()
                    --DISPLAY "FOLIOS SELECCIONADOS:", v_s_sql_folio                 
         	  	 END IF
         	  END FOR
              
         	  IF LENGTH(v_s_sql_folio CLIPPED) <> 0 THEN
         	     LET v_s_sql_folio = v_s_sql_folio[1,LENGTH(v_s_sql_folio)-1]
         	  ELSE
         	  	 LET  v_s_sql_folio = 0  
         	  END IF

            -- Obtiene los totales de importes y trabajadores
            -- Para el resumen de pantalla.
            LET v_s_sql = "SELECT SUM(imp_viv_dev), SUM(avis_viv_dev), COUNT(*)",
                          "\n FROM dpe_sol_trabajador d, glo_folio g",
                          "\n WHERE g.f_actualiza BETWEEN ","'",p_d_inicial,"'",
                          "\n AND ","'", p_d_final,"'",
                          "\n AND d.folio = g.folio",
                          "\n AND g.folio IN (",v_s_sql_folio,")"
               
               
               --DISPLAY "Sumas: ", v_s_sql CLIPPED
               PREPARE Prpr_Obt_sumas FROM v_s_sql CLIPPED
               EXECUTE Prpr_Obt_sumas INTO v_d_tot_vivienda, 
                                           v_d_tot_avis,
                                           v_i_reg_trabajadores
               
               LET v_s_sql = "SELECT COUNT(*)",
                              "\n FROM dpe_patron a",
                              "\n WHERE a.folio IN (",v_s_sql_folio,")"
                  
               --DISPLAY "Sumas_2: ", v_s_sql CLIPPED
               PREPARE Prpr_Obt_patrones FROM v_s_sql CLIPPED
               EXECUTE Prpr_Obt_patrones INTO v_i_sol_patronales
               
               LET v_i_tot_registros =  v_i_sol_patronales + v_i_reg_trabajadores
               
               DISPLAY BY NAME v_i_sol_patronales, v_i_reg_trabajadores, 
                            v_i_tot_registros, v_d_tot_vivienda, v_d_tot_avis
         
         ON ACTION ACCEPT
         	  --LET v_s_sql_folio = ""
         	  --FOR cont_marcados = 1 TO v_si_indice
         	     --IF v_ar_folios_disponibles[cont_marcados].v_s_disponible =  1 THEN
         	  	    --LET v_s_sql_folio = v_s_sql_folio CLIPPED ||
         	  	  	    --v_ar_folios_disponibles[cont_marcados].v_s_folio CLIPPED,","
         	  	 --END IF
         	  --END FOR
            LET v_s_sql_folio = ""
            LET v_folio_seleccionado = ""
         	  FOR cont_marcados = 1 TO v_si_indice
         	     IF v_ar_folios_disponibles[cont_marcados].v_s_disponible =  1 THEN
         	        --Asigna valor de arreglo a variable 
                    LET v_folio_seleccionado = v_ar_folios_disponibles[cont_marcados].v_s_folio CLIPPED
                    
                    CALL buf.append(v_folio_seleccionado)
                    CALL buf.trimleft()
                    
                    --Concatena valores 
                    LET v_s_sql_folio = v_s_sql_folio CLIPPED,v_folio_seleccionado CLIPPED,","
                        
                    CALL buf.append(v_s_sql_folio)
                    CALL buf.trimleft()
                    --DISPLAY "FOLIOS:", v_s_sql_folio                 
         	  	 END IF
         	  END FOR


              
         	  
         	  LET v_s_sql_folio = v_s_sql_folio[1,LENGTH(v_s_sql_folio)-1]
         	  LET v_s_sql = "SELECT COUNT(*)",
         	  	            "\n FROM dpe_sol_trab_parcial",
         	  	            "\n WHERE folio IN (",v_s_sql_folio,")"--,
        	  	 
         	   --DISPLAY "v_s_sql: ", v_s_sql CLIPPED
         	   PREPARE Prpr_Obt_diag FROM v_s_sql CLIPPED
               EXECUTE Prpr_Obt_diag INTO v_valida_diagnostico

               DISPLAY "FOLIOS SELECCIONADOS: ", v_s_sql_folio 
               
         	  IF LENGTH(v_s_sql_folio CLIPPED) <> 0 THEN
         	     IF v_valida_diagnostico > 0 THEN
         	        --LET v_s_sql_folio = v_s_sql_folio[1,LENGTH(v_s_sql_folio)-1]
         	        CALL fn_dpe_ejecuta_generacion_archivo_proc(p_usuario_cod, 
                                                                p_d_inicial, 
                                                                p_d_final, 
                                                                v_s_sql_folio)
         	        EXIT DIALOG
         	     ELSE
         	     CALL fn_mensaje("Atención",
                               "Los folios ya se usarón para generar un archivo",
                               "information")	
         	        EXIT DIALOG
         	     END IF
         	        	
         	  ELSE
         	     IF v_valida_diagnostico > 0 THEN
         	  	    CALL fn_dpe_ejecuta_generacion_archivo_proc(p_usuario_cod, 
                                                                p_d_inicial, 
                                                                p_d_final, 
                                                                v_s_sql_folio)
         	  	    EXIT DIALOG
         	  	 ELSE
         	  	    CALL fn_mensaje("Atención",
                                  "Los folios ya se usarón para generar un archivo",
                                  "information")	
         	        EXIT DIALOG
         	  	 END IF   	
         	  END IF
         	  CALL f.setElementHidden("grupo_folios",1)
            CALL f.setElementHidden("grupo_resumen",1)
         	  --LET INT_FLAG = TRUE
         	  CALL v_ar_folios_disponibles.CLEAR()
            EXIT DIALOG

         ON ACTION CANCEL
            CALL f.setElementHidden("grupo_folios",1)
            CALL f.setElementHidden("grupo_resumen",1)
            LET INT_FLAG = TRUE
            EXIT DIALOG
            END INPUT

      END DIALOG
    
    RETURN v_si_indice 

END FUNCTION --fn_folios_disponibles

{
======================================================================
Clave: 
Nombre: fn_dpe_ejecuta_generacion_archivo_proc
Fecha creacion: Marzo 16, 2012
Autor: Jose Soto
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_dpe_ejecuta_generacion_archivo_proc(p_usuario_cod, 
                                                p_d_inicial, 
                                                p_d_final, 
                                                p_cad_folios)
   DEFINE 
    p_usuario_cod LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
    p_d_inicial, -- Fecha inicial del periodo para el archivo de salida 
    p_d_final    DATE, -- Fecha final del periodo para el archivo de salida
    p_cad_folios VARCHAR(1000) -- Cadena que contiene los folios a consultar
    --
    ,v_s_comando       STRING -- cadena con una instruccion de consola
    ,v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
    ,v_i_resultado    INTEGER -- resultado del proceso
    ,r_bandera        SMALLINT

    -- Valida operacion para verificar si se puede continuar.
      -- Inicio operacion.       
      ## > -- se obtiene el ID del proceso
      CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod)
           RETURNING g_pid
         
      IF (fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) = 0 ) THEN
         ## > Inicializa proceso porque es la primera operacion
         CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,0,"DPEL05","",
                                    p_usuario_cod) RETURNING v_rest_valida
      
         IF ( v_rest_valida = 0)THEN
             -- se obtienen las rutas de control del modulo
            SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
              INTO g_reg_modulo.*
              FROM seg_modulo s
             WHERE s.modulo_cod = 'dpe'
            
             SELECT b.ruta_listados
               INTO seg_modulo_bat.ruta_listados
               FROM seg_modulo b
              WHERE b.modulo_cod = 'bat'
              
            -- se obtiene el nombre del archivo
            LET v_nombre_archivo = "ARCHIVOAQUI"
               	
            -- Inicia la operación asignando el estatus de PROCESANDO
            CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"DPEL05",
                                        "",p_usuario_cod)
            RETURNING v_rest_valida

            --LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/DPES01 ",
            LET v_s_comando = " fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/DPES01 ",
                                      p_usuario_cod, " ",
                                      g_pid  , " " ,
                                      g_proceso_cod , " " ,
                                      g_opera_cod ," ",
                                      "0 ",
                                      v_nombre_archivo ," ",
                                      p_d_inicial ," ",
                                      p_d_final ," ",
                                      p_cad_folios," ",
                                      " 1>",seg_modulo_bat.ruta_listados CLIPPED ,
                                      "/nohup:",g_pid        USING "&&&&&",":",
                                      g_proceso_cod USING "&&&&&",":",
                                      g_opera_cod   USING "&&&&&" ,
                                      " 2>&1 &"
                                     
                   DISPLAY v_s_comando
                   RUN v_s_comando
                      CALL fn_mensaje("Atención",
                           "Se ha enviado la generación de archivo para procesar.\n"||
                           "Puede revisar el avance del proceso en el monitor de "||
                           "ejecución de procesos","information")

                       EXIT PROGRAM          
         ELSE
            CALL fn_mues_desc_valida(v_rest_valida)
         END IF
      ELSE
         CALL fn_muestra_inc_operacion(fn_valida_operacion(g_pid,g_proceso_cod, g_opera_cod))
      END IF  --De valida operacion   
 
END FUNCTION -- fn_dpe_ejecuta_generacion_archivo_proc

{

 Obtiene la descripcion del error de la validacion y
  la muestra en mensaje para suario.
}
FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT
        ,v_descripcion LIKE cat_bat_parametro_salida.descripcion
  

   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
     INTO v_descripcion
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_resultado_opera

   -- Muestra el mensaje encontrado
   CALL fn_mensaje("Atención",v_descripcion CLIPPED,"information")

END FUNCTION -- fn_mues_desc_valida