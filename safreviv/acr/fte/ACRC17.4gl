--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>ACR                                           #
#Programa          =>ACRC17                                        #
#Objetivo          =>Programa que realiza la consulta de marcas    #
#Autor             =>Mauricio Sanchez, EFP                         #
#Fecha inicio      =>2 ABRIL 2012                                  #
####################################################################
IMPORT os
DATABASE safre_viv
GLOBALS
  DEFINE arr_arbol_marca   DYNAMIC ARRAY OF RECORD 
            desc_credito   LIKE sfr_marca.descripcion_marca,
            fecha          LIKE sfr_marca_activa.f_inicio,
            tot_marcas     SMALLINT,
            padre_id       STRING,
            id             STRING,
            nivel          SMALLINT  
         END RECORD,
         g_usuario_cod     LIKE seg_usuario.usuario_cod, -- clave del usuario firmado 
         g_ban_salir       SMALLINT,
         g_tpo_originacion LIKE cre_acreditado.tpo_originacion --tipo de originacion          
END GLOBALS

MAIN
   DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
          p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   LET g_tpo_originacion = 1

   --se inicializa la bandera que contola la ventana de despliegue en caso de que se cancele
   LET g_ban_salir = FALSE

   -- se crea el archivo log
   CALL STARTLOG(g_usuario_cod CLIPPED|| ".ACRC17.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

  --se llama a la funcion donde se selecciona el tipo de consulta 
   CALL fn_consulta_marcas() 
       
END MAIN

## ventana que muestra las opciones para consultas de marcas ##
FUNCTION fn_consulta_marcas()
   DEFINE v_op_consulta CHAR(4)

   OPEN WINDOW w_op_consulta WITH FORM "ACRC172"

   INPUT v_op_consulta FROM opciones ATTRIBUTE (UNBUFFERED)

      ON ACTION ACCEPT
         IF v_op_consulta = "opt1" THEN --consulta global           
           CALL fn_muestra_marcas(1)
           CALL arr_arbol_marca.clear()
         END IF
         IF v_op_consulta = "opt2" THEN --consulta por fecha        
           CALL fn_muestra_marcas(2)
           CALL arr_arbol_marca.clear()
         END IF         

      ON ACTION CANCEL 
         EXIT INPUT    

   END INPUT 
   CLOSE WINDOW w_op_consulta
END FUNCTION 

## ventana que muestra las marcas consultadas ##
FUNCTION fn_muestra_marcas(p_opt_cons)
   DEFINE p_opt_cons       SMALLINT, --parametro que indica el tipo de consulta
          v_indice         SMALLINT,
          v_manejador_rpt  OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
          r_ruta_bin       LIKE seg_modulo.ruta_bin,
          v_nom_reporte    VARCHAR(80), -- nombre del reporte
          r_ruta_listados  LIKE seg_modulo.ruta_listados,
          f_w              ui.form,
          w                ui.window,
          v_existe_archivo INTEGER,
          v_indice_rep     SMALLINT          
                    
   OPEN WINDOW w_consulta_marca WITH FORM "ACRC171"
     LET  w = ui.Window.getCurrent()
     LET  f_w = w.getForm()
     
     DISPLAY ARRAY arr_arbol_marca TO scr1.*

       BEFORE DISPLAY 
           LET g_ban_salir = FALSE       
           IF p_opt_cons = 1 THEN --si la consulta es global 
             --se llama a la funcion que llena el arbol a mostrar  
             CALL fn_llena_arbol_marcas(p_opt_cons,TODAY,NULL)
           ELSE   --la consulta es por fecha
             --llama a la funcion donde se captura la fecha y el tipo de credito
             CALL fn_cons_fecha(p_opt_cons)             
           END IF   

           --se valida que si se cancelo la consulta cierre la ventana
           IF g_ban_salir = TRUE THEN 
              EXIT DISPLAY 
           END IF       

       IF arr_arbol_marca[1].desc_credito IS NULL AND arr_arbol_marca[1].fecha IS NULL THEN 
          CALL fn_mensaje("Aviso","No existen registros de marcas activas","info") 
          EXIT DISPLAY    
       END IF 
       
       ON ACTION ACCEPT
          EXIT DISPLAY 
            
       ON ACTION CANCEL 
          EXIT DISPLAY 
            
       ON ACTION reporte
          -- CALL fn_reporte_marcas()
          LET v_indice = arr_arbol_marca.getLength()
          
          # Recupera la ruta de listados en el que se enviara el archivo
          CALL fn_rutas("acr") RETURNING r_ruta_bin, r_ruta_listados
   
          # Se indica que el reporte usara la plantilla creada
          IF fgl_report_loadCurrentSettings("ACRC171.4rp") THEN 
             CALL fgl_report_selectDevice("PDF") 
             LET v_nom_reporte = g_usuario_cod CLIPPED || "-ACRC17-","00000","-","00000","-","00000" 
             CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)

             -- sin preview
             CALL fgl_report_selectPreview(0)
      
             LET v_manejador_rpt = fgl_report_commitCurrentSettings()

             # Inicia el reporte de consulta historica por derechohabiente
             START REPORT reporte_marca TO XML HANDLER v_manejador_rpt
             FOR v_indice_rep = 1 TO v_indice
                 OUTPUT TO REPORT reporte_marca(arr_arbol_marca[v_indice_rep].*)
             END FOR
                 
             #Finaliza el reporte
             FINISH REPORT reporte_marca
             LET v_existe_archivo = 1

             IF(LENGTH(r_ruta_listados) > 0)THEN
                # se revisa si existe el archivo en la ruta de listados
                CALL os.Path.exists(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED||".pdf") RETURNING v_existe_archivo
             END IF

             # si no existe el archivo, se oculta la imagen link que visualiza el pdf
             IF NOT(v_existe_archivo)THEN
                CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",1)
             ELSE
                CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",0)
             END IF

                              
             # muestra una imagen(PDF) link que visualiza el reporte de la operacion en cuetion
             DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte CLIPPED||".pdf"||"','acr')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte
             CALL ui.Interface.refresh()        
  
             CALL fn_mensaje("Aviso","Se ha generado el reporte de marcas","info") 
          ELSE
             DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
          END IF                

     END DISPLAY
  CLOSE WINDOW w_consulta_marca      
END FUNCTION 

## Funcion que llena el arbol de marcas a desplegar ##
FUNCTION fn_llena_arbol_marcas(p_opt_cons,p_fec_marca,p_tpo_credito)    
    DEFINE qry_string       STRING
    DEFINE i                INTEGER
    DEFINE j                INTEGER    
    DEFINE cont_arbol       INTEGER
    DEFINE v_tpo_credito    LIKE cat_tipo_credito.tpo_credito   
    DEFINE v_desc_credito   LIKE cat_tipo_credito.desc_credito    
    DEFINE v_marca          LIKE sfr_marca_activa.marca
    DEFINE p_fec_marca      LIKE sfr_marca_activa.f_inicio
    DEFINE p_tpo_credito    LIKE cat_tipo_credito.tpo_credito
    DEFINE p_opt_cons       SMALLINT --parametro que indica el tipo de consulta
    DEFINE v_fecha_ini      LIKE sfr_marca_activa.f_inicio 
    DEFINE v_tot_marca      INTEGER
    DEFINE v_sum_marca      INTEGER   
    DEFINE v_marca_tmp      LIKE sfr_marca_activa.marca
    DEFINE v_fec_marca_tmp  LIKE sfr_marca_activa.f_inicio
            
    IF p_opt_cons = 1 THEN --consulta global
        LET qry_string = " SELECT UNIQUE c.tpo_credito, c.desc_credito ",
                         "   FROM sfr_marca_activa b, cre_acreditado a, cat_tipo_credito c ",
                         "  WHERE b.id_derechohabiente = a.id_Derechohabiente ",
                         "    AND b.marca = c.marca_prc ",
                         "    AND a.tpo_originacion = ",g_tpo_originacion,
                         "    AND a.tpo_credito = c.tpo_credito"
                        
    ELSE --consulta por fecha
       IF p_tpo_credito IS NULL THEN --solo se selecciono la fecha       
          LET qry_string = " SELECT UNIQUE c.tpo_credito, c.desc_credito ",
                           "   FROM sfr_marca_activa b, cre_acreditado a, cat_tipo_credito c ",
                           "  WHERE b.id_derechohabiente = a.id_Derechohabiente ",
                           "    AND b.marca = c.marca_prc ",
                           "    AND a.tpo_originacion = ",g_tpo_originacion,
                           "    AND a.tpo_credito = c.tpo_credito",   
                           "    AND b.f_inicio = '",p_fec_marca,"' "
       ELSE --se selecciono tipo de credito y se incluye la consulta 
          LET qry_string = " SELECT UNIQUE c.tpo_credito, c.desc_credito ",
                           " FROM sfr_marca_activa b, cre_acreditado a, cat_tipo_credito c ",
                           " WHERE b.id_derechohabiente = a.id_Derechohabiente ",
                           " AND b.marca = c.marca_prc ",
                           " AND a.tpo_originacion = ",g_tpo_originacion,
                           " AND a.tpo_credito = c.tpo_credito",   
                           " AND b.f_inicio = '",p_fec_marca,"' ", 
                           " AND c.tpo_credito = ", p_tpo_credito        
       END IF                           
    END IF     

    DISPLAY "consulta credito", qry_string
    PREPARE prp_nivel1 FROM qry_string
    DECLARE cur_nivel1 CURSOR FOR prp_nivel1   
            
    LET i           = 1
    LET j           = 1    
    LET cont_arbol  = 1
    LET v_sum_marca = 0
    LET v_tot_marca = 0
    
     --se limpia el arreglo que se va a desplegar     
    CALL arr_arbol_marca.clear()
            
    FOREACH cur_nivel1 INTO v_tpo_credito,v_desc_credito
        LET arr_arbol_marca[cont_arbol].desc_credito = v_desc_credito
        LET arr_arbol_marca[cont_arbol].fecha = ""

        IF p_opt_cons = 1 THEN --consulta global 
           LET qry_string = " SELECT UNIQUE (b.marca), b.f_inicio, COUNT(*) ",
                            " FROM sfr_marca_activa b, cre_acreditado a, cat_tipo_credito c ",
                            " WHERE b.id_derechohabiente = a.id_derechohabiente ",
                            " AND b.marca = c.marca_prc ",
                            " AND a.tpo_originacion = ",g_tpo_originacion,
                            " AND a.tpo_credito = c.tpo_credito ",
                            " AND c.tpo_credito = ", v_tpo_credito,
                            " GROUP BY 1,2 ",
                            " UNION ", 
                            " SELECT UNIQUE (b.marca), b.f_inicio, COUNT(*) ",
                            " FROM sfr_marca_activa b, cre_acreditado a, cat_tipo_credito c ",
                            " WHERE  b.id_derechohabiente = a.id_derechohabiente ",
                            " AND b.marca = c.marca_inf ",
                            " AND a.tpo_originacion = ",g_tpo_originacion,
                            " AND a.tpo_credito = c.tpo_credito ",
                            " AND c.tpo_credito = ", v_tpo_credito,
                            " GROUP BY 1,2 ",
                            " ORDER BY 1,2" 
        ELSE
           LET qry_string = " SELECT UNIQUE (b.marca), b.f_inicio, COUNT(*) ",
                            " FROM sfr_marca_activa b, cre_acreditado a, cat_tipo_credito c ",
                            " WHERE  b.id_derechohabiente = a.id_derechohabiente ",
                            " AND b.marca = c.marca_prc ",
                            " AND a.tpo_originacion = ",g_tpo_originacion,
                            " AND a.tpo_credito = c.tpo_credito ",
                            " AND c.tpo_credito = ", v_tpo_credito,
                            " AND b.f_inicio = '",p_fec_marca,"' ",
                            " GROUP BY 1,2 ",
                            " UNION ", 
                            " SELECT UNIQUE (b.marca), b.f_inicio, COUNT(*) ",
                            " FROM sfr_marca_activa b, cre_acreditado a, cat_tipo_credito c ",
                            " WHERE  b.id_derechohabiente = a.id_derechohabiente ",
                            " AND b.marca = c.marca_inf ",
                            " AND a.tpo_originacion = ",g_tpo_originacion,
                            " AND a.tpo_credito = c.tpo_credito ",
                            " AND c.tpo_credito = ", v_tpo_credito,
                            " AND b.f_inicio = '",p_fec_marca,"' ",
                            " GROUP BY 1,2 ",
                            " ORDER BY 1,2"    
        END IF         

        LET v_sum_marca = 0
        DISPLAY "MARCAS ", qry_string
        PREPARE prp_cta_marcas FROM qry_string
        DECLARE cur_cta_marcas CURSOR FOR prp_cta_marcas    
        FOREACH cur_cta_marcas INTO v_marca_tmp, v_fec_marca_tmp, v_tot_marca       
           LET v_sum_marca = v_sum_marca + v_tot_marca
        END FOREACH    
        
        LET arr_arbol_marca[cont_arbol].tot_marcas = v_sum_marca        
        LET arr_arbol_marca[cont_arbol].id = v_tpo_credito
        LET arr_arbol_marca[cont_arbol].nivel = 1
        LET arr_arbol_marca[cont_arbol].padre_id = ""        
        LET cont_arbol = cont_arbol + 1
            
        IF p_opt_cons = 1 THEN --consulta global 
           LET qry_string = " SELECT UNIQUE (b.marca), b.f_inicio, COUNT(*) ",
                            " FROM sfr_marca_activa b, cre_acreditado a, cat_tipo_credito c ",
                            " WHERE  b.id_derechohabiente = a.id_derechohabiente ",
                            " AND b.marca = c.marca_prc ",
                            " AND a.tpo_originacion = ",g_tpo_originacion,
                            " AND a.tpo_credito = c.tpo_credito ",
                            " AND c.tpo_credito = ", v_tpo_credito,
                            " GROUP BY 1,2 ",
                            " UNION ", 
                            " SELECT UNIQUE (b.marca), b.f_inicio, COUNT(*) ",
                            " FROM sfr_marca_activa b, cre_acreditado a, cat_tipo_credito c ",
                            " WHERE  b.id_derechohabiente = a.id_derechohabiente ",
                            " AND b.marca = c.marca_inf ",
                            " AND a.tpo_originacion = ",g_tpo_originacion,
                            " AND a.tpo_credito = c.tpo_credito ",
                            " AND c.tpo_credito = ", v_tpo_credito,
                            " GROUP BY 1,2 ",
                            " ORDER BY 1,2" 
        ELSE 
           LET qry_string = " SELECT UNIQUE (b.marca), b.f_inicio, COUNT(*) ",
                            " FROM sfr_marca_activa b, cre_acreditado a, cat_tipo_credito c ",
                            " WHERE  b.id_derechohabiente = a.id_derechohabiente ",
                            " AND b.marca = c.marca_prc ",
                            " AND a.tpo_originacion = ",g_tpo_originacion,
                            " AND a.tpo_credito = c.tpo_credito ",
                            " AND c.tpo_credito = ", v_tpo_credito,
                            " AND b.f_inicio = '",p_fec_marca,"' ",
                            " GROUP BY 1,2 ",
                            " UNION ", 
                            " SELECT UNIQUE (b.marca), b.f_inicio, COUNT(*) ",
                            " FROM sfr_marca_activa b, cre_acreditado a, cat_tipo_credito c ",
                            " WHERE  b.id_derechohabiente = a.id_derechohabiente ",
                            " AND b.marca = c.marca_inf ",
                            " AND a.tpo_originacion = ",g_tpo_originacion,
                            " AND a.tpo_credito = c.tpo_credito ",
                            " AND c.tpo_credito = ", v_tpo_credito,
                            " AND b.f_inicio = '",p_fec_marca,"' ",
                            " GROUP BY 1,2 ",
                            " ORDER BY 1,2"   
        END IF 

        DISPLAY "CONSULTA MARCAS N2", qry_string     
        PREPARE prp_nivel2 FROM qry_string
        DECLARE cur_nivel2 CURSOR FOR prp_nivel2    
        FOREACH cur_nivel2 INTO v_marca, v_fecha_ini, v_tot_marca

           LET arr_arbol_marca[cont_arbol].desc_credito = fn_desc_marca(v_marca)
           LET arr_arbol_marca[cont_arbol].fecha      = v_fecha_ini
           LET arr_arbol_marca[cont_arbol].tot_marcas = v_tot_marca          
           LET arr_arbol_marca[cont_arbol].id         = v_tpo_credito USING"<<",".",
                                                        v_marca USING"<<"
           LET arr_arbol_marca[cont_arbol].nivel      = 2
           LET arr_arbol_marca[cont_arbol].padre_id   = v_tpo_credito USING"<<"            
           LET cont_arbol = cont_arbol + 1
                                
           LET j = j + 1
        END FOREACH
        CLOSE cur_nivel2
        LET i = i + 1
    END FOREACH
    CLOSE cur_nivel1
END FUNCTION

## Funcion que obtiene la descripcion de una marca ##
FUNCTION fn_desc_marca(p_marca)
   DEFINE p_marca       LIKE sfr_marca_activa.marca,
          v_desc_marca  LIKE sfr_marca.descripcion_marca

   SELECT descripcion_marca
   INTO v_desc_marca    
   FROM sfr_marca
   WHERE marca = p_marca
   
   RETURN  v_desc_marca      
END FUNCTION

## funcion que permite capturar la fecha y el tipo de credito a consultar ##
FUNCTION fn_cons_fecha(p_opt_cons)
  DEFINE v_fecha LIKE sfr_marca_activa.f_inicio,
         v_tpo_credito  LIKE cat_tipo_credito.tpo_credito,
         cb             ui.ComboBox,
         v_credito      LIKE cat_tipo_credito.tpo_credito,   
         v_desc_credito LIKE cat_tipo_credito.desc_credito,
         v_sqlqry       STRING,
         p_opt_cons     SMALLINT --parametro que indica el tipo de consulta
                 
   OPEN WINDOW w_op_fecha WITH FORM "ACRC173"

     INPUT v_fecha, v_tpo_credito FROM fecha, tipo_credito ATTRIBUTE (UNBUFFERED)

        BEFORE INPUT 
          LET cb = ui.ComboBox.forName("tipo_credito")   
          CALL cb.clear()
          LET v_sqlqry = " SELECT tpo_credito, desc_credito ",          
                         " FROM cat_tipo_credito ",
                         " WHERE tpo_originacion = ",g_tpo_originacion

          PREPARE prp_cons_cred FROM v_sqlqry        
          DECLARE cur_credito CURSOR FOR prp_cons_cred
          FOREACH cur_credito INTO v_credito, v_desc_credito          
             CALL cb.addItem(v_credito,v_credito ||"-"||v_desc_credito)
          END FOREACH     

        AFTER FIELD tipo_credito
           NEXT FIELD fecha

        ON ACTION ACCEPT 
          IF v_fecha IS NULL THEN 
             CALL fn_mensaje("Aviso","Se requiere capturar la fecha de consulta","stop")
             CONTINUE INPUT
          ELSE 
             --se llama a la funcion que llena el arbol a mostrar
             CALL fn_llena_arbol_marcas(p_opt_cons,v_fecha,v_tpo_credito)            
             EXIT INPUT            
          END IF 
 
        ON ACTION CANCEL
           --se cancelo y se cierra la ventana de despliegue de registros
           LET g_ban_salir = TRUE  
           EXIT INPUT 

     END INPUT            
   CLOSE WINDOW w_op_fecha   
END FUNCTION

#Objetivo: Genera el reporte de consulta de deudor
REPORT reporte_marca(v_rec_marca)
  DEFINE v_rec_marca     RECORD 
            desc_credito LIKE sfr_marca.descripcion_marca,
            fecha        LIKE sfr_marca_activa.f_inicio,
            tot_marcas   SMALLINT,
            padre_id     STRING,
            id           STRING,
            nivel        SMALLINT  
       END RECORD,
       v_fecha_reporte   DATE

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY      
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"             
      PRINTX g_usuario_cod
                    
   ON EVERY ROW
      PRINTX v_rec_marca.desc_credito
      PRINTX v_rec_marca.fecha USING "dd-mm-yyyy"             
      PRINTX v_rec_marca.tot_marcas
      PRINTX v_rec_marca.nivel
END REPORT