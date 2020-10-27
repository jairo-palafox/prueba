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
   DEFINE v_ind_arch   SMALLINT 
   DEFINE v_ind_contac SMALLINT
   DEFINE v_query      STRING
   DEFINE v_c          SMALLINT 
   DEFINE v_reporte_pdf STRING 
   

   DEFINE  arr_sep_cuentas DYNAMIC ARRAY OF RECORD
            v_id_num          INTEGER,
            v_caso_adai       LIKE sep_expediente.caso_adai,
            v_estado          LIKE sep_estado_expediente.descripcion,
            v_nss             LIKE sep_nss_expediente.nss,
            v_tipo_trabajador CHAR(40),
            v_nombre          LIKE sep_nss_expediente.nombre,
            v_tel1            LIKE sep_nss_expediente.tel_contacto1,
            v_tel2            LIKE sep_nss_expediente.tel_contacto1,
            v_cel             LIKE sep_nss_expediente.tel_celular,
            v_correo_e        LIKE sep_nss_expediente.correo_e              
   END RECORD

   DEFINE r_sep_cuentas RECORD
            v_ind             SMALLINT,
            v_diag            CHAR(3),
            v_sql_error       INTEGER,
            v_id_expediente   LIKE sep_expediente.id_expediente,
            v_caso_adai       LIKE sep_expediente.caso_adai,
            v_estado          LIKE sep_estado_expediente.descripcion,
            v_nss             LIKE sep_nss_expediente.nss,
            v_tipo_trabajador CHAR(40),
            v_nombre          LIKE sep_nss_expediente.nombre,
            v_tel1            LIKE sep_nss_expediente.tel_contacto1,
            v_tel2            LIKE sep_nss_expediente.tel_contacto1,
            v_cel             LIKE sep_nss_expediente.tel_celular,
            v_correo_e        LIKE sep_nss_expediente.correo_e,
            v_contactado      CHAR(40)  
   END RECORD

   DEFINE cb_estado ui.ComboBox
   DEFINE cb_ind_arch ui.ComboBox
   DEFINE cb_ind_cont ui.ComboBox
   

   DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
         ,p_tipo_ejecucion SMALLINT                     -- forma como ejecutara el programa
         ,p_s_titulo       STRING                       -- titulo de la ventana

 DEFINE ar_sep_estado_expediente
                    RECORD LIKE sep_estado_expediente.* 

 DEFINE ar_sep_cat_indicador_exp
                    RECORD LIKE sep_cat_indicador_exp.* 
 
   DEFINE w ui.Window
   DEFINE f ui.Form   

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   
    -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN 
   OPEN WINDOW w_consulta WITH FORM "SEPC291"

   LET w = ui.Window.getCurrent()
   LET f = w.getForm()

   LET cb_estado      = ui.ComboBox.forName("formonly.cb_estado") 
   LET cb_ind_arch    = ui.ComboBox.forName("formonly.cb_ind_arch")
   LET cb_ind_cont    = ui.ComboBox.forName("formonly.cb_ind_cont")
 
   CALL cb_estado.clear()
   CALL cb_ind_arch.clear() 

   
   
   INPUT v_estado
        ,v_ind_arch
        ,v_ind_contac
     FROM  cb_estado
          ,cb_ind_arch
          ,cb_ind_cont          
    ATTRIBUTES (ACCEPT = FALSE, UNBUFFERED,CANCEL = FALSE )

    BEFORE INPUT
        CALL f.setElementHidden("g_separador",1)    
        CALL cb_estado.clear()
        DECLARE  c_cb_tpo_retiro CURSOR FOR  
                                    SELECT estado, descripcion
                                      FROM sep_estado_expediente
                                      
                                         ORDER BY estado
                                         
         
        FOREACH c_cb_tpo_retiro INTO ar_sep_estado_expediente.estado,ar_sep_estado_expediente.descripcion
        CALL cb_estado.addItem(ar_sep_estado_expediente.estado,ar_sep_estado_expediente.descripcion)
        END FOREACH

        CALL cb_ind_arch.clear()
        DECLARE  c_cb_cat_indicador CURSOR FOR  
                                    SELECT indicador_cod, indicador_desc
                                      FROM sep_cat_indicador_exp
                                      ORDER BY indicador_cod

                
        FOREACH c_cb_cat_indicador INTO ar_sep_cat_indicador_exp.indicador_cod,ar_sep_cat_indicador_exp.indicador_desc
        CALL cb_ind_arch.addItem(ar_sep_cat_indicador_exp.indicador_cod,ar_sep_cat_indicador_exp.indicador_desc)
        END FOREACH               

    AFTER FIELD cb_ind_cont
         NEXT FIELD cb_estado
        
    ON ACTION aceptar      
        LET v_query = "EXECUTE PROCEDURE   sp_sep_consulta_datos_contacto(?,?,?)"
        PREPARE pr_separacion FROM v_query
        DECLARE cur_separacion CURSOR FOR pr_separacion
        
        LET v_c = 0
        FOREACH  cur_separacion USING v_estado
                                     ,v_ind_arch
                                     ,v_ind_contac 
                                INTO r_sep_cuentas.*
          LET v_c = v_c + 1
          
           LET arr_sep_cuentas[v_c].v_id_num           = v_c
           LET arr_sep_cuentas[v_c].v_caso_adai        = r_sep_cuentas.v_caso_adai
           LET arr_sep_cuentas[v_c].v_nss              = r_sep_cuentas.v_nss
           LET arr_sep_cuentas[v_c].v_estado           = r_sep_cuentas.v_estado
           LET arr_sep_cuentas[v_c].v_tipo_trabajador  = r_sep_cuentas.v_tipo_trabajador
           LET arr_sep_cuentas[v_c].v_nombre           = r_sep_cuentas.v_nombre
           LET arr_sep_cuentas[v_c].v_tel1             = r_sep_cuentas.v_tel1
           LET arr_sep_cuentas[v_c].v_tel2             = r_sep_cuentas.v_tel2
           LET arr_sep_cuentas[v_c].v_cel              = r_sep_cuentas.v_cel
           LET arr_sep_cuentas[v_c].v_correo_e         = r_sep_cuentas.v_correo_e
                      
        END FOREACH

        IF v_c > 0 THEN 
        CALL f.setElementHidden("g_separador",0)
         DISPLAY ARRAY arr_sep_cuentas TO t_sep_cuentas.*
             ATTRIBUTES (ACCEPT =FALSE,CANCEL = FALSE )


          ON ACTION reporte            
            CALL fn_genera_reporte_contactos(v_estado,v_ind_arch,v_ind_contac ,p_usuario_cod,0,0,0)

            LET v_reporte_pdf = p_usuario_cod CLIPPED 
                                ,"-",
                                "SEPS29",
                                "-",
                                "0" USING "&&&&&"
                                ,"-"
                                ,"0" USING "&&&&&"
                                ,"-"
                                ,"0" USING "&&&&&"
                                ,".pdf"
                                DISPLAY v_reporte_pdf
                                
            DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_reporte_pdf||"','"||"sep"||"')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte
             
          BEFORE  DISPLAY 
              DISPLAY v_c  TO ff_total_reg
              DISPLAY ""   TO lbl_ruta_reporte

         ON ACTION cancelar
           CALL f.setElementHidden("g_separador",1)
           CALL arr_sep_cuentas.clear()
           EXIT DISPLAY           
              
         END DISPLAY
        ELSE 
          CALL fn_mensaje("Aviso","Sin expedientes encontrados","exclamation")
        END IF 
      
      ON ACTION cancelar
       EXIT INPUT                
           
    END INPUT 

END MAIN 