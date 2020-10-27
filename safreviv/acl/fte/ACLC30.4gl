--------------------------------------------------------------------------------------
-- Version: 1.0.0
-- Fecha ultima modificacion: 09-01-2014
--------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------
-- Modulo        => ACL
-- Programa      => ACLC30
-- Objetivo      => Consulta Rechazos en ACL
-- Fecha inicio  => 22-ABR-2014
-- Autor         => GERARDO ALFONSO VEGA PAREDES
-- Actualizacion =>
--------------------------------------------------------------------------------------

DATABASE safre_viv

--g- GLOBALS "PAGG01.4gl"

GLOBALS

   DEFINE g_folio_param   LIKE glo_folio.folio,
          g_id_referencia LIKE cta_his_pagos.id_referencia

END GLOBALS

MAIN

   DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod

   LET p_usuario_cod    = ARG_VAL(1)
   
   CALL fn_consulta_registros(p_usuario_cod)

END MAIN

{ ======================================================================
Clave:  ACLC30
Nombre: fn_consulta_registros
Fecha creacion: 22 Abril 2014
Autor: Gerardo Vega, EFP
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}

FUNCTION fn_consulta_registros(p_usuario_cod)

   DEFINE p_usuario_cod            LIKE seg_usuario.usuario_cod, -- clave del usuario
          v_registros              INTEGER,      
          cve_origen_archivo       LIKE cta_his_pagos.origen_archivo,
          v_query                  STRING,
          v_nss                    LIKE afi_derechohabiente.nss,
          v_nrp                    LIKE cta_his_pagos.nrp,
          v_periodo_pago           LIKE cta_his_pagos.periodo_pago,
          v_indice                 INTEGER,
          v_indice_aux             INTEGER,
          v_ventana                ui.WINDOW

   DEFINE arr_registros DYNAMIC ARRAY OF RECORD
      nss                   LIKE afi_derechohabiente.nss,
      folio                 LIKE cta_his_pagos.folio,
      nrp                   LIKE cta_his_pagos.nrp,
      periodo_pago          LIKE cta_his_pagos.periodo_pago,
      folio_sua             LIKE cta_his_pagos.folio_sua,
      f_pago                LIKE cta_his_pagos.f_pago,
      id_derechohabiente    LIKE cta_his_pagos.id_derechohabiente,
      cve_entidad_receptora LIKE cta_his_pagos.cve_ent_receptora,
      imp_ap_pat            LIKE cta_his_pagos.imp_ap_pat,
      imp_am_cre            LIKE cta_his_pagos.imp_am_cre,
      aiv_ap_pat            LIKE cta_his_pagos.aiv_ap_pat, 
      valor_aiv             LIKE cta_his_pagos.valor_aiv,
      int_gen_pgo_ext       LIKE cta_his_pagos.int_gen_pgo_ext,
      aiv_gen_pgo_ext       LIKE cta_his_pagos.aiv_gen_pgo_ext,
      archivo               CHAR(40),
      result_operacion      LIKE cta_his_pagos.result_operacion,
      rechazo               CHAR(60)
   END RECORD

   DEFINE v_periodo_par CHAR(06),
          v_periodo_non CHAR(06)
   
   OPEN WINDOW w_consulta_registros WITH FORM "../../acl/bin/ACLC301"
   LET  v_ventana = UI.WINDOW.GETCURRENT()
   CALL v_ventana.SETTEXT("Consulta rechazo aclaratorio")

   INPUT v_nss,v_nrp,v_periodo_pago FROM ed_nss,ed_nrp,ed_periodo_pago
         ATTRIBUTES(UNBUFFERED, WITHOUT DEFAULTS)
   
      ON ACTION ACCEPT

         IF v_nss IS NULL THEN 
      	   CALL fn_mensaje("Consulta","Debe de ingresar el NSS","about")
           NEXT FIELD ed_nss         	 
         END IF
         
         IF v_nrp IS NULL THEN
      	   CALL fn_mensaje("Consulta","Debe de ingresar el NRP","about")
           NEXT FIELD ed_nrp         
         END IF
         
         IF v_periodo_pago IS NULL THEN
      	   CALL fn_mensaje("Consulta","Debe de ingresar el periodo de pago","about")
           NEXT FIELD ed_periodo_pago
         END IF         

         IF v_periodo_pago[5,6] > 6 THEN   
            CALL fn_mensaje("Atención","El Bimestre no puede ser mayor a 6","stop") 
            NEXT FIELD ed_periodo_pago                                                
         END IF

         CALL fn_convierte_periodo(v_periodo_pago) RETURNING v_periodo_par,v_periodo_non
         
         LET INT_FLAG = FALSE
         EXIT INPUT
         
      ON ACTION CANCEL
         LET INT_FLAG = TRUE                           
         EXIT INPUT

   END INPUT

   IF NOT INT_FLAG THEN

      --hace el conteo de registros
      LET v_query = " SELECT COUNT(*)",
                    " FROM   cta_his_pagos cta, afi_derechohabiente afi, glo_ctr_archivo glo ",
                    " WHERE  afi.id_derechohabiente = cta.id_derechohabiente ", 
                    " AND    glo.proceso_cod in (1401,102,103,107) ",
                    " AND    glo.opera_cod = 1 ",
                    " AND    afi.nss = ? ",
                    " AND    cta.nrp = ? ",
                    " AND    (cta.periodo_pago = ? OR periodo_pago = ? )"
                    
      DISPLAY " v_query  = ",v_query," ",v_periodo_par," ",v_periodo_non
      PREPARE prp_count_registros FROM v_query
      EXECUTE prp_count_registros USING v_nss, v_nrp, v_periodo_par, v_periodo_non 
      INTO v_registros
      
      IF v_registros IS NULL THEN
        LET v_registros = 0
      END IF
      
      --valida que se econtrarón registros
      IF v_registros > 0 THEN
         --realizala busqueda para llenar el arreglo
         LET v_query ="\n SELECT afi.nss                 ,",
                      "\n        a.folio                 ,", 
                      "\n        a.nrp                   ,",
                      "\n        fn_bimestre_pago(a.periodo_pago),",
                      "\n        a.folio_sua             ,",
                      "\n        a.f_pago                ,", 
                      "\n        a.id_derechohabiente    ,",
                      "\n        a.cve_ent_receptora     ,",
                      "\n        a.imp_ap_pat            ,",
                      "\n        a.imp_am_cre            ,",
                      "\n        a.aiv_ap_pat            ,",
                      "\n        a.valor_aiv             ,",
                      "\n        a.int_gen_pgo_ext       ,",
                      "\n        a.aiv_gen_pgo_ext       ,",
                      "\n        g.nombre_archivo        ,",
                      "\n        a.result_operacion      ,",
                      "\n        h.estado_pago_desc       ",
                      "\n  FROM  cta_his_pagos    a,      ",
                      "\n        glo_ctr_archivo  g,      ", 
                      "\n        pag_ctr_pago     f,      ",
                      "\n        pag_cat_edo_pago h,      ",
                      "\n        afi_derechohabiente afi, ",
                      "\n        cta_pag_complemento d    ",
                      "\n WHERE  d.id_derechohabiente = a.id_derechohabiente       ",
                      "\n AND    a.folio              = d.folio                    " ,
                      "\n AND    a.folio              = g.folio                    ",
                      "\n AND    a.folio              = f.folio                    ",
                      "\n AND    a.id_referencia      = d.id_referencia            " ,
                      "\n AND    a.id_referencia      = f.id_referencia            ",
                      "\n AND    a.id_derechohabiente = afi.id_derechohabiente     ",
                      "\n AND    g.proceso_cod in (1401,102,103,107)               ",
                      "\n AND    g.opera_cod = 1                                   ",
                      "\n AND    f.estado_pago = h.estado_pago                     ",
                      "\n AND    afi.nss        = ","'",v_nss,"'",
                      "\n AND    a.nrp          = ","'",v_nrp,"'",
                      "\n AND    a.periodo_pago in (","'",v_periodo_par,"',","'",v_periodo_non,"'",")"
      
         DISPLAY "@QUERY: ",v_query
         PREPARE prp_registros FROM v_query
         DECLARE cur_registros CURSOR FOR prp_registros
      
         LET v_indice = 1
         --llen ael arreglo
         LET v_indice_aux = 1
         FOREACH cur_registros INTO  arr_registros[v_indice].nss,
                                     arr_registros[v_indice].folio,
                                     arr_registros[v_indice].nrp,     
                                     arr_registros[v_indice].periodo_pago, 
                                     arr_registros[v_indice].folio_sua,          
                                     arr_registros[v_indice].f_pago,              
                                     arr_registros[v_indice].id_derechohabiente, 
                                     arr_registros[v_indice].cve_entidad_receptora,
                                     arr_registros[v_indice].imp_ap_pat,           
                                     arr_registros[v_indice].imp_am_cre,           
                                     arr_registros[v_indice].aiv_ap_pat,           
                                     arr_registros[v_indice].valor_aiv,            
                                     arr_registros[v_indice].int_gen_pgo_ext,      
                                     arr_registros[v_indice].aiv_gen_pgo_ext,
                                     arr_registros[v_indice].archivo,
                                     arr_registros[v_indice].result_operacion,
                                     arr_registros[v_indice].rechazo

            -- Si el origen archivo no es de Solo Infonavit se hace la conversion en bimestre
            -- si el origen archivo es de solo infonavit no se hace la conversion
            IF cve_origen_archivo <> 3 THEN 
                LET arr_registros[v_indice].periodo_pago = v_periodo_pago
            END IF            

            LET v_indice = v_indice+ 1
         END FOREACH
        
         --elinina ultimo renglon en blanco
         LET v_indice = v_indice - 1
         IF arr_registros[arr_registros.getLength()].folio IS NULL THEN
             CALL arr_registros.deleteElement(arr_registros.getLength())
         END IF   
      
         IF (arr_registros[1].folio IS NULL OR arr_registros[1].folio = 0) THEN
           CALL fn_mensaje("Consulta","No se obtuvieron resultados con el NSS ingresado.","about") 
         ELSE 
            DIALOG  ATTRIBUTE(UNBUFFERED)
         
            DISPLAY ARRAY arr_registros TO tbl_registros.*
               BEFORE DISPLAY
                  DISPLAY v_nss TO ed_nss
            END DISPLAY
         
            ON ACTION cancelar
               EXIT DIALOG
              END DIALOG
         END IF 
      ELSE
         CALL fn_mensaje("Consulta","No existen registros con los criterios dados.","about")   
      END IF

   END IF 
    
   CLOSE WINDOW w_consulta_registros

END FUNCTION

FUNCTION fn_convierte_periodo(v_periodo_pago)

   DEFINE v_periodo_pago LIKE cta_his_pagos.periodo_pago
   
   DEFINE v_pp_par       CHAR(06),
          v_pp_non       CHAR(06)
       
   CASE v_periodo_pago[5,6]
      WHEN "06" 
         LET v_pp_par = v_periodo_pago[1,4],"12"
         LET v_pp_non = v_periodo_pago[1,4],"11"
      WHEN "05" 
         LET v_pp_par = v_periodo_pago[1,4],"10"
         LET v_pp_non = v_periodo_pago[1,4],"09"
      WHEN "04" 
         LET v_pp_par = v_periodo_pago[1,4],"08"
         LET v_pp_non = v_periodo_pago[1,4],"07"
      WHEN "03" 
         LET v_pp_par = v_periodo_pago[1,4],"06"
         LET v_pp_non = v_periodo_pago[1,4],"05" 
      WHEN "02" 
         LET v_pp_par = v_periodo_pago[1,4],"04"
         LET v_pp_non = v_periodo_pago[1,4],"03"     
      WHEN "01" 
         LET v_pp_par = v_periodo_pago[1,4],"02"
         LET v_pp_non = v_periodo_pago[1,4],"01"
   END CASE

   RETURN v_pp_par, v_pp_non

END FUNCTION
