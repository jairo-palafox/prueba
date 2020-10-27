--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
################################################################################
#MODULO       => SEP                                                           #
#PROGRAMA     => SEPC29                                                        #
#OBJETIVO     =>                                                               #
#                                                                              #
#FECHA INICIO => JUNIO 26, 2012                                                #
#MODIFICACION =>                                                               #
################################################################################
DATABASE 
     safre_viv 
MAIN
   DEFINE v_estado     SMALLINT
   DEFINE v_flujo   SMALLINT 
   DEFINE v_ind_contac SMALLINT
   DEFINE v_query      STRING
   DEFINE v_c          SMALLINT 
   DEFINE v_reporte_pdf STRING 
   
   DEFINE  v_expediente DYNAMIC ARRAY OF RECORD
            v_id_num          INTEGER,
            v_expediente      LIKE sep_expediente.id_expediente,
            v_invadido        LIKE sep_nss_expediente.nss,
            v_asociado        LIKE sep_nss_expediente.nss,
            v_tipo_flujo      LIKE sep_cat_tipo_flujo.flujo_desc,
            v_canal           LIKE sep_cat_canal_recepcion_exp.canal_desc,
            v_fecha_recepcion LIKE sep_expediente.f_recepcion_infonavit,
            v_fecha_captura   LIKE sep_expediente.f_captura,
            v_estado          LIKE sep_estado_expediente.descripcion
   END RECORD

   DEFINE v_exp_recuperados RECORD 
           v_ind           SMALLINT     ,   -- v_ind          
  v_diag          CHAR(3)      ,   -- vi_diag        
  v_sql_error     INTEGER      ,   -- v_sql_error    
  v_id_expediente DECIMAL(9,0) ,   -- id_expediente  
  v_invadido      CHAR(011)    ,   -- invadido       
  v_asociado      CHAR(011)    ,   -- asociado       
  v_flujo         CHAR(40)     ,   -- flujo          
  v_canal         CHAR(40)     ,   -- canal          
  v_f_recepcion   DATE         ,   -- fecha recepcion
  v_f_captura     DATE         ,   -- fecha captura  
  v_estado        CHAR(20)                           
END RECORD

   DEFINE cb_estado    ui.ComboBox,
          cb_flujo_cod ui.ComboBox
   
   DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
         ,p_tipo_ejecucion SMALLINT                     -- forma como ejecutara el programa
         ,p_s_titulo       STRING                       -- titulo de la ventana

 DEFINE v_estado_expediente RECORD 
          v_estado_cod     LIKE sep_estado_expediente.estado,
          v_estado_desc    LIKE sep_estado_expediente.descripcion
        END RECORD

 DEFINE v_flujo_expediente RECORD 
          v_flujo_cod  LIKE sep_cat_tipo_flujo.flujo_cod,
          v_flujo_desc LIKE sep_cat_tipo_flujo.flujo_desc
        END RECORD
 DEFINE w ui.Window
 DEFINE f ui.Form
 DEFINE v_ruta_ejecutable LIKE seg_modulo.modulo_cod 

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   
   

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "sep"
   
   OPEN WINDOW w_consulta WITH FORM "SEPC231"

      LET w = ui.Window.getCurrent()
      LET f = w.getForm()
       -- si se obtuvo el titulo, se pone como titulo de programa
      IF(p_s_titulo IS NOT NULL)THEN
         CALL ui.Interface.setText(p_s_titulo)
      END IF

      LET cb_estado    = ui.ComboBox.forName("formonly.cb_estado_cod") 
      LET cb_flujo_cod = ui.ComboBox.forName("formonly.cb_flujo_cod")      
 
      CALL cb_estado.clear()
      CALL cb_flujo_cod.clear() 

   
   
   INPUT v_estado,v_flujo FROM  cb_estado_cod,cb_flujo_cod          
       ATTRIBUTES (ACCEPT = FALSE, UNBUFFERED,CANCEL = FALSE )

    BEFORE INPUT
        CALL f.setElementHidden("gpo_exp_encontrados",1)    
        CALL cb_estado.clear()
        # Llena combo de estado de expedientes
        DECLARE  c_cb_tpo_retiro CURSOR FOR  SELECT estado, descripcion
                                               FROM sep_estado_expediente                                      
                                              ORDER BY estado
        
        FOREACH c_cb_tpo_retiro INTO v_estado_expediente.v_estado_cod,v_estado_expediente.v_estado_desc
           CALL cb_estado.addItem(v_estado_expediente.v_estado_cod,v_estado_expediente.v_estado_desc)
        END FOREACH
        FREE c_cb_tpo_retiro

        # Llena combo de flujo de expedientes
        CALL cb_flujo_cod.clear()
        DECLARE  cur_rec_indicador CURSOR FOR SELECT flujo_cod, flujo_desc
                                                 FROM sep_cat_tipo_flujo
                                                ORDER BY flujo_cod

        FOREACH cur_rec_indicador INTO v_flujo_expediente.v_flujo_cod,v_flujo_expediente.v_flujo_desc
           CALL cb_flujo_cod.addItem(v_flujo_expediente.v_flujo_cod,v_flujo_expediente.v_flujo_desc)
        END FOREACH               
        FREE cur_rec_indicador

        
    ON ACTION aceptar 
        IF(v_flujo IS NULL)THEN
           LET v_flujo = 0
        END IF
        IF(v_estado IS NULL)THEN
           LET v_estado = 0
        END IF
        # SP que recuepra los datos del expediente
        LET v_query = "EXECUTE PROCEDURE   sp_sep_consulta_edo_expediente(?,?)"
        PREPARE prp_rec_espedientes FROM v_query
        DECLARE cur_rec_espedientes CURSOR FOR prp_rec_espedientes
        
        LET v_c = 0
        FOREACH  cur_rec_espedientes USING v_estado,
                                           v_flujo                                     
                                      INTO v_exp_recuperados.*
           LET v_c = v_c + 1          
           LET v_expediente[v_c].v_id_num          = v_c
           LET v_expediente[v_c].v_expediente      = v_exp_recuperados.v_id_expediente
           LET v_expediente[v_c].v_invadido        = v_exp_recuperados.v_invadido
           LET v_expediente[v_c].v_asociado        = v_exp_recuperados.v_asociado
           LET v_expediente[v_c].v_tipo_flujo      = v_exp_recuperados.v_flujo
           LET v_expediente[v_c].v_canal           = v_exp_recuperados.v_canal
           LET v_expediente[v_c].v_fecha_recepcion = v_exp_recuperados.v_f_recepcion
           LET v_expediente[v_c].v_fecha_captura   = v_exp_recuperados.v_f_captura
           LET v_expediente[v_c].v_estado          = v_exp_recuperados.v_estado
           LET v_expediente[v_c].v_estado          = v_exp_recuperados.v_estado
                      
        END FOREACH
        FREE cur_rec_espedientes

        IF v_c > 0 THEN 
           CALL f.setElementHidden("gpo_exp_encontrados",0)
           DISPLAY ARRAY v_expediente TO sr_expediente.*
               ATTRIBUTES (ACCEPT =FALSE,CANCEL = FALSE )

              BEFORE  DISPLAY 
                 DISPLAY v_c  TO ff_total_reg
                 DISPLAY ""   TO lbl_ruta_reporte

              ON ACTION reporte
                CALL fn_genera_reporte_expedientes(v_estado,v_flujo,p_usuario_cod,0,0,0)
                LET v_reporte_pdf = p_usuario_cod CLIPPED,"-",
                                    "SEPC23","-",
                                    "0" USING "&&&&&","-"
                                    ,"0" USING "&&&&&","-"
                                    ,"0" USING "&&&&&",".pdf"
                DISPLAY v_reporte_pdf
                # despliega link para visuaizar reporte
                DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_reporte_pdf||"','"||"sep"||"')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte
                --DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_reporte_pdf||"','"||"sep"||"')\" target='_blank'>Reporte</a>" TO lbl_ruta_reporte
                --DISPLAY v_reporte_pdf TO lbl_ruta_reporte
                
              ON ACTION cancelar
                 CALL f.setElementHidden("gpo_exp_encontrados",1)
                 CALL v_expediente.clear()
                EXIT DISPLAY           
              
           END DISPLAY
        ELSE 
          CALL fn_mensaje("Aviso","Sin expedientes encontrados","exclamation")
        END IF 
      
      ON ACTION cancelar
       EXIT INPUT                
           
    END INPUT
END MAIN 