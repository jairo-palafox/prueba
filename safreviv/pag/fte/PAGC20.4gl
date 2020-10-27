--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/04/2012
--===============================================================

#########################################################################################
#Modulo       => pag                                                                    #
#Programa     => PAGC20                                                                 #
#Objetivo    => Consulta Estado del Pago                                                #
#Fecha inicio => ABRIL 11, 2012                                                         # 
#Autor        => Ilhuitemoc Ricardo Ortiz                                               #
#########################################################################################

DATABASE safre_viv


MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- consulta de informacion recibida LQINFO
   CALL fn_consulta_registros(p_usuario_cod)

END MAIN

{ ======================================================================
Clave: PAGC20
Nombre: fn_consulta_registros
Fecha creacion: ABRIL 11, 2012
Autor: ILHUITEMOC RICARDO ORTIZ
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_registros(p_usuario_cod)
DEFINE p_usuario_cod            LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_fecha                  DATE,
       v_bandera                SMALLINT,
       v_registros              INTEGER,
       arr_registros            DYNAMIC ARRAY OF RECORD
            folio               DECIMAL(9,0),
            id_derechohab_aux   DECIMAL(9,0),
            nss                 VARCHAR(200),
            estado_pago         VARCHAR(150),
            tpo_aclaracion      VARCHAR(150),
            f_actualiza         DATE
       END RECORD,
      { arr_registros_historicos  DYNAMIC ARRAY OF RECORD
            nss                 VARCHAR(150),
            tpo_archivo         VARCHAR(150),
            origen_pago         VARCHAR(150),
            tpo_afiliacion      VARCHAR(150),
            tpo_aclaracion      VARCHAR(150),
            f_actualiza         DATE
       END RECORD,}
       v_nombre_af              VARCHAR(150),
       v_ap_paterno_af          VARCHAR(150),
       v_ap_materno_af          VARCHAR(150),
       v_archivo_descripcion    VARCHAR(150),
       v_pago_descripcion       VARCHAR(150),
       v_afiliacion_descripcion VARCHAR(150),
       v_aclaracion_descripcion VARCHAR(150),
       cve_origen_archivo       LIKE cta_his_pagos.origen_archivo,
       cve_tipo_aclaracion      LIKE pag_tpo_aclaracion.aclaracion_cod,
       v_query                STRING,
       v_condicion            STRING,
       v_folio                LIKE cta_his_pagos.folio,
       v_id_derechohabiente   LIKE cta_his_pagos.id_derechohabiente,
       v_indice               INTEGER,
       v_f_actualiza          VARCHAR(15),
       v_arr_curr             INTEGER,
       v_indice_aux           INTEGER,
       v_nss                  LIKE afi_derechohabiente.nss,
        r_registros_aux           RECORD
            folio               DECIMAL(9,0),
            id_derechohab_aux   DECIMAL(9,0),
            nss                 VARCHAR(200),
            estado_pago         VARCHAR(150),
            tpo_aclaracion      VARCHAR(150),
            f_actualiza         DATE
       END RECORD
       DEFINE manejador_rpt         om.SaxDocumentHandler
       DEFINE v_ruta_reporte       STRING -- ruta del archivo del reporte
       DEFINE v_ruta_listados      STRING -- ruta de los listados
       DEFINE v_ruta_ejecutable    STRING -- ruta del ejecutable
       DEFINE v_nombre    STRING -- ruta del ejecutable
       DEFINE v_tipo_archivo    STRING -- ruta del ejecutable
   
   
   OPEN WINDOW w_consulta_registros WITH FORM "PAGC201"
   
   INPUT v_nss
   FROM  ed_nss
   ATTRIBUTES(UNBUFFERED, WITHOUT DEFAULTS)
   
      ON ACTION ACCEPT
         --valida que se seleccione por lomenos un criterio de bsuqueda
         IF v_nss IS NULL THEN
            CALL fn_mensaje("Consulta",
                            "Debe de ingresar un NSS",
                            "about")
         ELSE
            LET v_query = "SELECT id_derechohabiente \n",
                           "FROM   afi_derechohabiente \n",
                           "WHERE  nss = '",v_nss,"'"
            --DISPLAY v_query 
            PREPARE prp_id_derechohab FROM v_query
            EXECUTE prp_id_derechohab INTO v_id_derechohabiente
            IF v_id_derechohabiente IS NULL OR 
               v_id_derechohabiente = 0 THEN
               LET INT_FLAG = TRUE
               CALL fn_mensaje("Consulta",
                            "No existen registros para el NSS ingresado",
                            "about")
                CONTINUE INPUT
            ELSE
               LET INT_FLAG = FALSE
               EXIT INPUT
            END IF
         END IF 
         
       ON ACTION cancel
         LET INT_FLAG = TRUE                           
         EXIT INPUT
   END INPUT

    IF NOT INT_FLAG THEN     
      --hace el conteo de registros
      --hace el conteo de registros
      
      LET v_indice = 1
       LET v_indice_aux = 1   
      LET v_query = "SELECT COUNT(*) \n",
                    "FROM   pag_ctr_pago a \n",
                    "WHERE  id_derechohabiente = ",v_id_derechohabiente
      --DISPLAY v_query
      PREPARE prp_count_registros FROM v_query
      EXECUTE prp_count_registros INTO v_registros

      IF v_registros IS NULL THEN
        LET v_registros = 0
      END IF

      --valida que se econtrarón registros
      IF v_registros > 0 THEN
        --realizala busqueda para llenar el arreglo
        LET v_query = "SELECT a.folio, a.id_derechohabiente, b.nss ||' '|| trim (b.nombre_af) ||' '|| \n", 
               "       trim ( b.ap_paterno_af)||' '|| trim (b.ap_materno_af),                  \n",
               "       a.estado_pago ||' '|| trim (d.estado_pago_desc),                        \n",
               "       a.tpo_aclaracion ||' '||trim (f.aclaracion_descripcion), a.f_actualiza  \n",
               "FROM   pag_ctr_pago a, afi_derechohabiente b,                                  \n",
               "       pag_cat_edo_pago d, pag_tpo_aclaracion f          \n",
               "WHERE  a.id_derechohabiente = b.id_derechohabiente                             \n",
               "  AND  a.tpo_aclaracion = f.aclaracion_cod                                     \n",
               "  AND  a.estado_pago = d.estado_pago                                           \n",
               "  AND  a.id_derechohabiente = ",v_id_derechohabiente
        --DISPLAY v_query
        PREPARE prp_registros FROM v_query
        DECLARE cur_registros CURSOR FOR prp_registros
        --llen ael arreglo
        FOREACH cur_registros INTO arr_registros[v_indice ].*
                 
                 LET r_registros_aux.folio               = arr_registros[v_indice ].folio 
                 LET r_registros_aux.id_derechohab_aux   = arr_registros[v_indice ].id_derechohab_aux 
                 LET r_registros_aux.nss                 = arr_registros[v_indice ].nss               
                 LET r_registros_aux.estado_pago         = arr_registros[v_indice ].estado_pago       
                 LET r_registros_aux.tpo_aclaracion      = arr_registros[v_indice ].tpo_aclaracion    
                 LET r_registros_aux.f_actualiza         = arr_registros[v_indice ].f_actualiza       

        	
        	
        	
            LET v_indice = v_indice + 1 
        END FOREACH
        
        --SE ASUME QUE LA TABLA pag_his_ctr_pago  se relaciona 1-1 con la tbala pag_ctr_pago
        {
        --hace el conteo de registros
        LET v_query = "SELECT COUNT(*) \n",
                      "FROM   pag_his_ctr_pago \n",
                      "WHERE  id_derechohabiente = '",arr_registros[v_indice].id_derechohab_aux,"'"
        DISPLAY v_query
        PREPARE prp_count_registros_historicos FROM v_query
        EXECUTE prp_count_registros_historicos INTO v_registros

        IF v_registros IS NULL THEN
          LET v_registros = 0
        END IF
        }
        {--valida que se econtrarón registros
        IF v_registros > 0 THEN}
          --realizala busqueda para llenar el arreglo
      {    LET v_query = "SELECT b.nss, b.nombre_af, b.ap_paterno_af, b.ap_materno_af, \n",
                        "       a.tpo_archivo, c.archivo_descripcion, a.origen_pago, d.pago_descripcion, \n",
                        "       a.tpo_afiliacion, e.afiliacion_descripcion, a.tpo_aclaracion, \n",
                        "       f.aclaracion_descripcion, a.f_actualiza \n ",
                        " FROM  pag_his_ctr_pago a, afi_derechohabiente b, pag_tpo_archivo c, \n",
                        "       pag_origen_pago d, pag_tpo_afiliacion e, pag_tpo_aclaracion f \n ",
                        "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
                        "  AND  a.tpo_archivo = c.archivo_cod \n",
                        "  AND  a.origen_pago = d.pago_cod \n",
                        "  AND  a.tpo_afiliacion = e.afiliacion_cod \n",
                        "  AND  a.tpo_aclaracion = f.aclaracion_cod \n",
                        "  AND  a.id_derechohabiente =",v_id_derechohabiente                               

          --DISPLAY v_query
          PREPARE prp_registros_historicos FROM v_query
          DECLARE cur_registros_historicos CURSOR FOR prp_registros_historicos

          --llen ael arreglo
        
          FOREACH cur_registros_historicos INTO  arr_registros_historicos[v_indice_aux].nss,
                                      v_nombre_af,
                                      v_ap_paterno_af,
                                      v_ap_materno_af,
                                      arr_registros_historicos[v_indice_aux].tpo_archivo,
                                      v_archivo_descripcion,
                                      arr_registros_historicos[v_indice_aux].origen_pago,
                                      v_pago_descripcion,
                                      arr_registros_historicos[v_indice_aux].tpo_afiliacion,
                                      v_afiliacion_descripcion,
                                      arr_registros_historicos[v_indice_aux].tpo_aclaracion,
                                      v_aclaracion_descripcion,
                                      arr_registros_historicos[v_indice_aux].f_actualiza
        
              LET arr_registros_historicos[v_indice_aux].nss = arr_registros_historicos[v_indice_aux].nss CLIPPED,
                                                               " - ",v_nombre_af CLIPPED," ",v_ap_paterno_af CLIPPED," ",v_ap_materno_af CLIPPED
              LET arr_registros_historicos[v_indice_aux].tpo_archivo = arr_registros_historicos[v_indice_aux].tpo_archivo CLIPPED, " - " , v_archivo_descripcion CLIPPED
              LET arr_registros_historicos[v_indice_aux].origen_pago = arr_registros_historicos[v_indice_aux].origen_pago CLIPPED, " - " , v_pago_descripcion CLIPPED
              LET arr_registros_historicos[v_indice_aux].tpo_afiliacion = arr_registros_historicos[v_indice_aux].tpo_afiliacion CLIPPED, " - " , v_afiliacion_descripcion CLIPPED
              LET arr_registros_historicos[v_indice_aux].tpo_aclaracion = arr_registros_historicos[v_indice_aux].tpo_aclaracion CLIPPED, " - " , v_aclaracion_descripcion CLIPPED
              
              LET v_indice_aux = v_indice_aux  + 1    
          END FOREACH       }
          
          --elimina ultimo renglon en blanco        
          CALL arr_registros.deleteElement(arr_registros.getLength())

          --elinina ultimo renglon en blanco                    
         -- CALL arr_registros_historicos.deleteElement(arr_registros_historicos.getLength())
          
         {
        ELSE
          CALL fn_mensaje("Consulta",
              "No existen registros con los criterios dados.",
              "about")   
        END IF
        }

        DIALOG ATTRIBUTE(UNBUFFERED ) 
        
        
        DISPLAY ARRAY arr_registros TO tbl_registros.*
        
        END DISPLAY

      {  DISPLAY ARRAY arr_registros_historicos TO tbl_registros_historico.*
                    --ATTRIBUTE ()
        END DISPLAY}

        ON ACTION REPORTE      
             CALL ARR_CURR( ) RETURNING v_arr_curr  
            # Recupera la ruta de listados en el que se enviara el archivo
            CALL fn_rutas("pag") RETURNING v_ruta_ejecutable, v_ruta_listados
           
            --Se asigna la plantilla para generar el reporte
            IF fgl_report_loadCurrentSettings("PAGC20.4rp") THEN 
                CALL fgl_report_selectDevice ("PDF")
                            
                LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","estado_de_pago"
                CALL fgl_report_setOutputFileName(v_ruta_reporte)
                CALL fgl_report_selectPreview(1)
                LET manejador_rpt = fgl_report_commitCurrentSettings() 
                
            ELSE         
                DISPLAY "no fue posible generar el reporte"
                EXIT PROGRAM 
            END IF   
            --Inicia el reporte de registros con rechazo
            START REPORT rpt_detalle_pagos TO XML HANDLER manejador_rpt
            -- Asigna el titulo del reporte
             FOR v_indice = 1 TO arr_registros.getLength()
                OUTPUT TO REPORT rpt_detalle_pagos( r_registros_aux.*) {,                 
                                                    arr_registros_historicos[v_arr_curr].nss            ,
                                                    arr_registros_historicos[v_arr_curr].tpo_archivo    ,
                                                    arr_registros_historicos[v_arr_curr].origen_pago    ,
                                                    arr_registros_historicos[v_arr_curr].tpo_afiliacion ,
                                                    arr_registros_historicos[v_arr_curr].tpo_aclaracion ,
                                                    arr_registros_historicos[v_arr_curr].f_actualiza   
                                                   )}
            END FOR                
            FINISH REPORT rpt_detalle_pagos 
        
        
        
        
        ON ACTION cancelar
            EXIT DIALOG


        END DIALOG
      ELSE
        CALL fn_mensaje("Consulta",
                        "No existen registros con los criterios dados.",
                        "about")   
      END IF
      
   --RETURN v_registros
   END IF 
    
   CLOSE WINDOW w_consulta_registros

END FUNCTION



-- OBJETIVO: Emitir el reporte de liquidacion/preliquidacion
REPORT rpt_detalle_pagos(r_registros_aux)--, arr_registros_historicos)
 DEFINE r_registros_aux  RECORD   
            folio               DECIMAL(9,0),
            id_derechohab_aux   DECIMAL(9,0),
            nss                 VARCHAR(200),
            estado_pago         VARCHAR(150),
            tpo_aclaracion      VARCHAR(150),
            f_actualiza         DATE
       END RECORD
 {DEFINE arr_registros_historicos     RECORD
            nss                 VARCHAR(150),
            tpo_archivo         VARCHAR(150),
            origen_pago         VARCHAR(150),
            tpo_afiliacion      VARCHAR(150),
            tpo_aclaracion      VARCHAR(150),
            f_actualiza         DATE
       END RECORD},
        v_fecha_reporte           DATE
                                                                                                                                                                                          
FORMAT                                                                                        
                        
   
   FIRST PAGE HEADER
   
      LET v_fecha_reporte = TODAY 
         
      PRINTX r_registros_aux.folio
      PRINTX r_registros_aux.id_derechohab_aux
      PRINTX r_registros_aux.nss             
      PRINTX r_registros_aux.estado_pago     
      PRINTX r_registros_aux.tpo_aclaracion  
      PRINTX r_registros_aux.f_actualiza  USING "DD-MM-YYYY"    
                                                  
                                                                                              
      PRINTX v_fecha_reporte USING "DD-MM-YYYY"
   {ON EVERY ROW     
      PRINTX arr_registros_historicos.nss             
      PRINTX arr_registros_historicos.tpo_archivo     
      PRINTX arr_registros_historicos.origen_pago     
      PRINTX arr_registros_historicos.tpo_afiliacion  
      PRINTX arr_registros_historicos.tpo_aclaracion  
      PRINTX arr_registros_historicos.f_actualiza  USING "DD-MM-YYYY"}
      
                                                                                           
END REPORT  