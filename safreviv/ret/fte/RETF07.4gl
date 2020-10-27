    --===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
################################################################################
#MODULO       => RET                                                           #
#PROGRAMA     => RETF07                                                        #
#OBJETIVO     => PROGRAMA DE CONSULTA SALDO TRABAJADORES DE FONDO72            #
#                                                                              #
#Fecha inicio => Septiembre 20, 2012                                           #
#Modificacion =>                                                               #
################################################################################
DATABASE safre_viv
GLOBALS 
    DEFINE v_nss            CHAR(20)
    DEFINE v_saldo          DECIMAL(14,2)
    
    DEFINE r_arr_afi_fondo72
       DYNAMIC ARRAY OF RECORD 
        id_afi_fondo72       DECIMAL(9,0)
       ,rfc                  CHAR(13)
       ,nss                  CHAR(11)
       ,id_derechohabiente   DECIMAL(9,0)
       ,nombre               CHAR(60)
       ,f_apertura           DATE
       ,total_afiliado       DECIMAL(14,2)
       ,monto_solic          DECIMAL(14,2)
       --,total_afiliado_tanto DECIMAL(14,2)
       ,tanto_solic          DECIMAL(14,2)
     END RECORD         

     DEFINE  afi_nss RECORD
        nss                  CHAR(11)
      END RECORD   

      DEFINE  afi_nss RECORD
        nss                  CHAR(11)
      END RECORD   
   
  DEFINE r_arr_cta_fondo72
    DYNAMIC ARRAY OF RECORD LIKE cta_fondo72.*

 DEFINE w      ui.Window
 DEFINE f      ui.Form
 DEFINE cb_nss   ui.ComboBox
-- variables del display 
 DEFINE v_total_cta_fondo         LIKE cta_fondo72.importe
 DEFINE v_total_capturado_imp     DECIMAL(14,2)
 DEFINE v_total_capturado_tanto   DECIMAL(14,2)

 DEFINE t_total_pendiente_imp     DECIMAL(19,2)
 DEFINE t_total_pendiente_tanto   DECIMAL(19,2)  
 DEFINE r_total_pesos72           DECIMAL(14,2)
 DEFINE r_total_tanto_adicional   DECIMAL(14,2)
 DEFINE v_estado          SMALLINT 
 
END GLOBALS 

MAIN 

 
 DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion SMALLINT                     -- forma como ejecutara el programa
       ,p_s_titulo       STRING                       -- titulo de la ventana 
 
 --banderas de control
 
 DEFINE v_c               INTEGER 
 DEFINE v_bnd_guardado    SMALLINT  
  
 CREATE TEMP TABLE tmp_nss_status_18_fa72(
                  t_nss        CHAR(11),
                  t_viv72      DECIMAL(14,2),
                  t_tanto_adic DECIMAL(14,2))

 LET p_usuario_cod    = ARG_VAL(1)
 LET p_tipo_ejecucion = ARG_VAL(2)
 LET p_s_titulo       = ARG_VAL(3)
 LET v_estado         = 18  -- estado de la solicitd con inconsistencia en la solicitd

 DISPLAY "Creo la tabla temporal"
   -- si se obtuvo el titulo, se pone como titulo de programa
 IF ( p_s_titulo IS NOT NULL ) THEN
    CALL ui.Interface.setText(p_s_titulo)
 END IF

 CLOSE WINDOW SCREEN 
 OPEN WINDOW w_consulta WITH FORM "RETF071"

 LET cb_nss    = ui.ComboBox.forName("formonly.cb_nss")
 LET w         = ui.Window.getCurrent()
 LET f         = w.getForm()
 
 INPUT 
     v_nss
  FROM  cb_nss
  ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE  )       
    BEFORE INPUT 
     CALL  r_arr_afi_fondo72.clear()
     CALL  r_arr_cta_fondo72.clear()
     CALL  f.setElementHidden("g_afi",1)
     CALL  f.setElementHidden("g_cta",1)

     LET v_c = 1
     LET v_bnd_guardado = 0 
     
        TRUNCATE tmp_nss_status_18_fa72
        CALL fn_llenado_combo_nss() RETURNING v_bnd_guardado
        IF v_bnd_guardado THEN
           EXIT INPUT   
        END IF
    ON CHANGE cb_nss
       IF v_nss IS NULL  THEN
          CALL fn_mensaje("Error","Se debe seleccionar un nss  ","info")
          NEXT FIELD cb_nss
       END IF 
       SELECT t_viv72 , t_tanto_adic
         INTO t_total_pendiente_imp, t_total_pendiente_tanto
         FROM tmp_nss_status_18_fa72
         WHERE t_nss = v_nss
      
       DISPLAY t_total_pendiente_imp   TO e_saldo_soli
       DISPLAY t_total_pendiente_tanto TO e_tanto_soli
    #al seleccionar nss se en listan los datos del afiliado 
    ON ACTION aceptar
       CALL r_arr_afi_fondo72.clear()
       CALL  f.setElementHidden("g_afi",0)
       DECLARE  c_cb_nss_id CURSOR FOR  SELECT afi.*
                                      FROM ret_fondo_ahorro ret,
                                             ret_det_fondo72 det,
                                             afi_fondo72 afi
                                       WHERE afi.id_afi_fondo72    = det.id_afi_fondo72
                                         AND ret.id_solicitud      = det.id_solicitud
                                         AND afi.nss               = v_nss                                       
                                       AND ret.estado_solicitud    = v_estado
                                     ORDER BY nss
       LET v_c = 1
        #se calcula el importe capturado en cta_fondo72
       FOREACH c_cb_nss_id INTO r_arr_afi_fondo72[v_c].*
            SELECT SUM(cta.importe)
              INTO v_total_cta_fondo
              FROM cta_fondo72 cta
             WHERE cta.id_afi_fondo72 = r_arr_afi_fondo72[v_c].id_afi_fondo72
               AND subcuenta = 40 

            LET r_arr_afi_fondo72[v_c].total_afiliado = v_total_cta_fondo
            LET v_c = v_c + 1
       END FOREACH
       CALL r_arr_afi_fondo72.deleteElement(r_arr_afi_fondo72.getLength())

       LET v_c = v_c - 1       
       IF v_c  <= 0  THEN
        CALL fn_mensaje("Error","No existe nss disponible para mostrar","info")
          CONTINUE INPUT  
        END IF
        
       --CALL r_arr_afi_fondo72.deleteElement(r_arr_afi_fondo72.getLength())
       # se habilitan display de consulta de los datos del afiliado
       DISPLAY ARRAY r_arr_afi_fondo72 TO r_afi_fondo72.*
           ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE  )

         # se habilitan display de consulta de saldos del afiliado seleccionado
         ON ACTION doubleclick
           CALL fn_muestra_detalle() RETURNING v_bnd_guardado
           IF v_bnd_guardado = 1 THEN
              EXIT DISPLAY
           END IF 

         ON ACTION Editar
           CALL fn_muestra_detalle() RETURNING v_bnd_guardado
           IF v_bnd_guardado = 1 THEN
              EXIT DISPLAY
           END IF 

         ON ACTION cancelar
            CALL  f.setElementHidden("g_afi",1)
            CALL  f.setElementHidden("g_cta",1)
            EXIT DISPLAY 
            EXIT INPUT 

        END DISPLAY

    ON ACTION CLOSE  
       EXIT INPUT

  END INPUT

  DROP TABLE tmp_nss_status_18_fa72 
  CLOSE WINDOW w_consulta
END MAIN

#funcion que realiza el alta del ajuste en la tabla de preliquidacion 
FUNCTION fn_ajuste_manual_retiro_fondo_ahorro_cont_solicitud(p_id_solicitud,p_monto_solic,p_tanto_solic)
   DEFINE p_id_afi_fondo72  DECIMAL(9,0)
   DEFINE p_id_solicitud    DECIMAL(9,0)
   DEFINE p_monto_solic     DECIMAL(14,2)
   DEFINE p_tanto_solic     DECIMAL(14,2)  
   DEFINE v_s_sql           STRING 
   DEFINE v_i_resultado     SMALLINT;
   DEFINE isam_err          INTEGER;
   DEFINE err_txt           VARCHAR(250);
   
   LET p_id_afi_fondo72 = 0 

LET v_s_sql = "EXECUTE FUNCTION fn_ajuste_manual_retiro_fondo_ahorro_cont(?,?,?,?,?)"
       -- se prepara la ejecucion del stored procedure para la preliquidacion
       PREPARE sid_ret_sol_fondo_ahorro FROM  v_s_sql
       EXECUTE sid_ret_sol_fondo_ahorro USING p_id_afi_fondo72
                                             ,p_id_solicitud
                                             ,p_monto_solic
                                             ,p_tanto_solic
                                             ,'S'
                          INTO v_i_resultado , isam_err ,err_txt
                          
RETURN v_i_resultado
END FUNCTION 

#funcion que realiza el alta del ajuste en la tabla de preliquidacion 
FUNCTION fn_ajuste_manual_retiro_fondo_ahorro_cont_detalle(p_id_afi_fondo72,p_id_solicitud,p_monto_solic,p_tanto_solic)
   DEFINE p_id_afi_fondo72  DECIMAL(9,0)
   DEFINE p_id_solicitud    DECIMAL(9,0)
   DEFINE p_monto_solic     DECIMAL(14,2)
   DEFINE p_tanto_solic     DECIMAL(14,2)
   DEFINE v_s_sql           STRING 
   DEFINE v_i_resultado     SMALLINT;
   DEFINE isam_err          INTEGER;
   DEFINE err_txt           VARCHAR(250);

   --DISPLAY "********************** store" 
   --DISPLAY p_id_afi_fondo72
                                             --,p_id_solicitud
                                             --,p_monto_solic
                                             --,p_tanto_solic
                                             --,'D'
   --DISPLAY "********************** store"

LET v_s_sql = "EXECUTE FUNCTION fn_ajuste_manual_retiro_fondo_ahorro_cont(?,?,?,?,?)"
       -- se prepara la ejecucion del stored procedure para la preliquidacion
       PREPARE sid_ret_det_fondo_ahorro FROM  v_s_sql
       EXECUTE sid_ret_det_fondo_ahorro USING p_id_afi_fondo72
                                             ,p_id_solicitud
                                             ,p_monto_solic
                                             ,p_tanto_solic
                                             ,'D'
                          INTO v_i_resultado , isam_err ,err_txt
RETURN v_i_resultado
END FUNCTION   

FUNCTION fn_muestra_detalle ()
 DEFINE v_cont          INTEGER 
 DEFINE v_c             INTEGER  
 DEFINE v_bandera       INTEGER  
 DEFINE v_bnd_guardado  INTEGER 
 DEFINE v_s_result      SMALLINT   
 DEFINE v_id_solicitud  LIKE ret_det_fondo72.id_solicitud

            CALL  f.setElementHidden("g_cta",0)
            LET v_cont = arr_curr()
            DIALOG   ATTRIBUTES(UNBUFFERED)
               # se habilitan display de consulta de saldos del afiliado seleccionado
               DISPLAY ARRAY r_arr_cta_fondo72 TO r_cta_fondo72.*
                   BEFORE DISPLAY 
                      DECLARE  c_cta_fondo72 CURSOR FOR SELECT cta.*
                                                          FROM cta_fondo72 cta
                                                         WHERE cta.id_afi_fondo72 = r_arr_afi_fondo72[v_cont].id_afi_fondo72

                      LET v_c = 1
                      FOREACH  c_cta_fondo72 INTO r_arr_cta_fondo72[v_c].*
                         LET v_c = v_c + 1 
                      END FOREACH
               END DISPLAY
               
                # se habilitan campos de captura de los montos
               INPUT ARRAY r_arr_afi_fondo72
                      FROM  r_afi_fondo72.*
                      ATTRIBUTES ( APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE)

                  --ON CHANGE r_afi_fondo72
                       

                  # se valida la nformacion y montos capturados para realizar la carga del registro a la preliquidacion
                  ON ACTION guardar
                     LET v_total_capturado_imp   = 0
                     LET v_total_capturado_tanto = 0 
                          FOR v_c = 1 TO r_arr_cta_fondo72.getLength()
                              IF r_arr_afi_fondo72[v_c].monto_solic >=  0 THEN 
                                 LET v_total_capturado_imp = v_total_capturado_imp + r_arr_afi_fondo72[v_c].monto_solic
                              END IF
                             IF r_arr_afi_fondo72[v_c].tanto_solic >=  0 THEN 
                                 LET v_total_capturado_tanto = v_total_capturado_tanto + r_arr_afi_fondo72[v_c].tanto_solic
                              END IF
                              DISPLAY r_arr_afi_fondo72[v_c].monto_solic
                          END FOR
                      IF t_total_pendiente_imp = v_total_capturado_imp 
                         AND (t_total_pendiente_tanto = v_total_capturado_tanto OR t_total_pendiente_tanto = 0) 
                         AND (t_total_pendiente_tanto = t_total_pendiente_imp OR t_total_pendiente_tanto = 0 )THEN
                         CALL fn_ventana_confirma("Guardar",
                                                  "Desea Guardar los cambios realizados?",
                                                  "") RETURNING v_bandera

                          IF v_bandera = 1 THEN
                              LET v_bnd_guardado = 0
                              FOR v_c = 1 TO r_arr_cta_fondo72.getLength()
                                  IF r_arr_afi_fondo72[v_c].monto_solic >=  0 THEN
                                     LET v_bnd_guardado = 1
                                     
                                     SELECT id_solicitud
                                       INTO v_id_solicitud
                                       FROM ret_det_fondo72
                                      WHERE id_afi_fondo72 = r_arr_afi_fondo72[v_c].id_afi_fondo72
                                     
                                       CALL fn_ajuste_manual_retiro_fondo_ahorro_cont_detalle(r_arr_afi_fondo72[v_c].id_afi_fondo72
                                                                                           ,v_id_solicitud
                                                                                           ,r_arr_afi_fondo72[v_c].monto_solic
                                                                                           ,r_arr_afi_fondo72[v_c].tanto_solic) 
                                       RETURNING v_s_result
                                       --DISPLAY "*******************detalle"
                                       --DISPLAY r_arr_afi_fondo72[v_c].id_afi_fondo72,v_id_solicitud,r_arr_afi_fondo72[v_c].monto_solic,r_arr_afi_fondo72[v_c].tanto_solic
                                       --DISPLAY v_s_result
                                       --DISPLAY "*******************detalle" 
                                  END IF
                                  
                                  IF v_s_result <> 0 THEN 
                                     LET v_bnd_guardado = 0
                                  END IF 
                               END FOR

                               IF v_bnd_guardado = 1 THEN                                    
                                 CALL fn_ajuste_manual_retiro_fondo_ahorro_cont_solicitud(v_id_solicitud
                                                                                         ,t_total_pendiente_imp
                                                                                         ,t_total_pendiente_tanto)
                                  RETURNING v_s_result
                                  --DISPLAY "*******************solicitud"
                                  --DISPLAY v_id_solicitud,t_total_pendiente_imp,t_total_pendiente_tanto
                                  --DISPLAY v_s_result
                                  --DISPLAY "*******************solicitud"

                                  IF v_s_result <> 0 THEN 
                                     LET v_bnd_guardado = 0
                                  END IF 
                               END IF
                               
                               IF v_bnd_guardado THEN 
                                  CALL fn_mensaje("Atención","Se realizo el guardado correctamente","information") 
                                  CALL  f.setElementHidden("g_afi",1)
                                  CALL  f.setElementHidden("g_cta",1)
                                  DISPLAY v_nss
                                  CALL cb_nss.removeItem(v_nss)
                                  EXIT DIALOG
                               ELSE
                                 CALL fn_mensaje("Atención","No se guardaron los cambios "|| "- Error: "||v_s_result,"information") 
                                 CONTINUE DIALOG 
                               END IF 
                          ELSE
                              CALL fn_mensaje("Atención","No se guardaron los cambios ","information") 
                              CONTINUE DIALOG 
                          END IF
                      ELSE  
                          CALL fn_mensaje("Atención","Los montos capturados no conciden con el solicitado","information")
                          DISPLAY t_total_pendiente_imp ," - ", v_total_capturado_imp
                          DISPLAY t_total_pendiente_tanto ," - ", v_total_capturado_tanto
                          CONTINUE DIALOG
                      END IF
               END INPUT

           ON ACTION cancelar
              CALL  f.setElementHidden("g_cta",1)
              EXIT DIALOG

           END DIALOG 
RETURN v_bnd_guardado 
END FUNCTION 

FUNCTION fn_llenado_combo_nss()
DEFINE v_c          INTEGER 
DEFINE v_bnd_salida SMALLINT 

LET v_bnd_salida = 0 

--ERV # esta parte esta por consultar no se a tomado en cuenta los qe no tengan d_derechohabiente en la
           #tabla de afiliados
      {DECLARE  c_cb_nss CURSOR FOR  SELECT afi.*
                                      --afi.nss,ret.saldo_viv72, ret.tanto_adicional
                                      FROM ret_fondo_ahorro ret,
                                           ret_det_fondo72 det,  
                                           afi_fondo72 afi
                                     WHERE afi.id_afi_fondo72    = det.id_afi_fondo72
                                       AND ret.id_solicitud      = det.id_solicitud
                                       AND ret.estado_solicitud  = v_estado
                                     ORDER BY nss
        
        FOREACH c_cb_nss INTO r_arr_afi_fondo72[v_c].*
           CALL cb_nss.addItem(r_arr_afi_fondo72[v_c].nss ,r_arr_afi_fondo72[v_c].nss)
           INSERT INTO tmp_nss_status_18_fa72 VALUES (afi_nss.nss,r_total_pesos72,r_total_tanto_adicional)
            LET v_c = v_c + 1
        END FOREACH}
        CALL cb_nss.clear()
        DECLARE  c_cb_nss CURSOR FOR SELECT UNIQUE  afi.nss,ret.saldo_viv72, ret.tanto_adicional
                                        FROM ret_fondo_ahorro ret,
                                             ret_det_fondo72 det,
                                             afi_fondo72 afi
                                       WHERE ret.estado_solicitud  = v_estado
                                         AND ret.id_solicitud      = det.id_solicitud
                                         AND afi.id_afi_fondo72    = det.id_afi_fondo72
                                       ORDER BY nss
        
        #se llena el combo de nss con las solcitudes en estado 18
        FOREACH c_cb_nss INTO afi_nss.nss,r_total_pesos72,r_total_tanto_adicional
           CALL cb_nss.addItem(afi_nss.nss ,afi_nss.nss)
           --DISPLAY "si entro" 
           INSERT INTO tmp_nss_status_18_fa72 VALUES (afi_nss.nss,r_total_pesos72,r_total_tanto_adicional)
           
            LET v_c = v_c + 1
        END FOREACH
        --DISPLAY v_c 
        IF v_c  <= 0  THEN
        CALL fn_mensaje("Error","No existe nss disponible para mostrar 1","info")
          LET v_bnd_salida = 1 
        END IF
RETURN v_bnd_salida
END FUNCTION 