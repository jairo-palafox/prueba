--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#########################################################################################
#Modulo       => ACL                                                                    #
#Programa     => ACLC01                                                                 #
#Objetivo     => Consulta Totales de archivo de aclaraciones enaclara                   #
#Fecha inicio => Febrero 07, 2012                                                       # 
#Autor        => Francisco López                                                        #
#Modificacion => se agrega archivo globales de aclaratorio y se sustituyen las          #
#                variables correspondientes; hilda rivas                                #
#########################################################################################

DATABASE safre_viv

GLOBALS "ACLG02.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
	DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
END GLOBALS

MAIN
DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
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
   CALL fn_consulta_folio(p_usuario_cod)

END MAIN

{ ======================================================================
Clave: ACL01
Nombre: fn_consulta_folio
Fecha creacion: Febrero 07, 2012
Autor: Francisco López
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_folio(p_usuario_cod)
DEFINE p_usuario_cod            LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_folio                  DECIMAL(9,0), -- folio
       v_cbx_folios             ui.ComboBox, -- combo de afores
       v_s_cadena               STRING, -- cadena de texto
       v_r_glo_ctr_archivo      RECORD LIKE glo_ctr_archivo.*,
       v_r_glo_folio            RECORD LIKE glo_folio.*

   OPEN WINDOW w_consulta_folio WITH FORM "ACLC071"
   
   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
  
   -- se inicia el combobox en blanco
   CALL v_cbx_folios.clear()
   
   -- el primer elemento en el combo es la cadena que indica que se debe elegir
   -- una afore
   CALL v_cbx_folios.addItem(-1," ")
   
   INPUT v_folio WITHOUT DEFAULTS
      FROM cmb_folio    ATTRIBUTES (UNBUFFERED)
  
      BEFORE INPUT
         -- se asignan los valores por omision
         LET v_folio = -1
         
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
         SELECT a.*
         FROM   glo_folio a,
                acl_sum_aclaracion b -- se cruza con el sumario para que luego del reverso no este el folio
         WHERE  a.proceso_cod = g_proceso_cod_acl_registro_pagos --Proceso Aclaraciones enaclara
         AND    a.folio = b.folio
         --AND
         -- estado = 2 -- integrado

         FOREACH cur_folios INTO v_r_glo_folio.*
            LET v_s_cadena = v_r_glo_folio.folio
            CALL v_cbx_folios.addItem(v_r_glo_folio.folio, v_s_cadena)
         END FOREACH

         FREE cur_folios
        
      ON ACTION ACCEPT
         -- se envian los parametros de consulta
         IF v_folio IS NOT NULL AND v_folio >= 0  THEN
            CALL fn_consulta_archivo(v_folio) 
         ELSE
            CONTINUE INPUT
         END IF
         
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_folio

END FUNCTION

{
======================================================================
Clave: ACLC01
Nombre: fn_consulta_archivo
Fecha creacion: Febrero 07, 2012
Autor: Francisco López
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_archivo(p_folio)

   DEFINE
      p_folio DECIMAL(9,0),
      vindice SMALLINT,

      reg_cifras RECORD
         --tpo_registro            LIKE acl_sum_aclaracion.tpo_registro
         tot_ap                  INTEGER --LIKE acl_sum_aclaracion.tot_ap
        ,suma_ap_pat             DECIMAL(18,2) --LIKE acl_sum_aclaracion.suma_ap_pat
        ,suma_am                 DECIMAL(18,2) --LIKE acl_sum_aclaracion.suma_am
        ,suma_aivs               DECIMAL(18,6) --LIKE acl_sum_aclaracion.suma_aivs
      END RECORD

       
   -- se realiza la consulta para obtener las cifras
   DECLARE cur_pag CURSOR FOR
   SELECT tot_ap
         ,suma_ap_pat
         ,suma_am
         ,suma_aivs
   FROM   acl_sum_aclaracion
   WHERE  folio = p_folio

   LET vindice = 1
   FOREACH cur_pag INTO reg_cifras.*
      LET vindice = vindice + 1
   END FOREACH

   OPEN WINDOW w_consulta WITH FORM "ACLC072"
   DISPLAY BY NAME reg_cifras.*

    MENU ""
       COMMAND "Cancelar"
          EXIT MENU

       COMMAND "Reporte"
           CALL fn_reporte_carga_archivo(p_folio, reg_cifras.*, TRUE)
    END MENU
   
   CLOSE WINDOW w_consulta
   
   CLOSE WINDOW w_consulta

END FUNCTION

-- OBJETIVO: Obtener los datos necesarios para emitir el reporte de Integración LQINFO
FUNCTION fn_reporte_carga_archivo(p_folio, reg_cifras, p_b_despliegue_pantalla)
    DEFINE p_folio                 INTEGER
    DEFINE p_b_despliegue_pantalla SMALLINT -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
    DEFINE reg_cifras RECORD
         --tpo_registro            LIKE acl_sum_aclaracion.tpo_registro
         tot_ap                  INTEGER --LIKE acl_sum_aclaracion.tot_ap
        ,suma_ap_pat             DECIMAL(18,2) --LIKE acl_sum_aclaracion.suma_ap_pat
        ,suma_am                 DECIMAL(18,2) --LIKE acl_sum_aclaracion.suma_am
        ,suma_aivs               DECIMAL(18,6) --LIKE acl_sum_aclaracion.suma_aivs
      END RECORD,

    manejador_rpt         om.SaxDocumentHandler,
    
    v_ruta_reporte       STRING, -- ruta del archivo del reporte
    v_ruta_listados      STRING, -- ruta de los listados
    v_ruta_ejecutable    STRING, -- ruta del ejecutable
    v_nombre_usuario     LIKE seg_usuario.usuario_desc,
    v_nombre_archivo     LIKE glo_ctr_archivo.nombre_archivo

     # Recupera la ruta de listados en el que se enviara el archivo
    CALL fn_rutas("acl") RETURNING v_ruta_ejecutable, v_ruta_listados 
                                                --r_ruta_bin, r_ruta_listados

    --Se consulta el nombre del usuario para despliegue en reporte 
    SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario 
     WHERE usuario_cod = p_usuario_cod                                      
     
   -- se consulta el nombre del archivo que se carga para despliegue en reporte
    SELECT nombre_archivo 
      INTO v_nombre_archivo
      FROM glo_ctr_archivo
     WHERE folio = p_folio
       AND proceso_cod = 101 
                                                
    --Se asigna la plantilla para generar el reporte
    IF fgl_report_loadCurrentSettings("rpt_carga_enaclara.4rp") THEN 
        CALL fgl_report_selectDevice ("PDF")
                    
        LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","carga_enaclara"
        CALL fgl_report_setOutputFileName(v_ruta_reporte)
        CALL fgl_report_selectPreview(1)
        LET manejador_rpt = fgl_report_commitCurrentSettings()
    ELSE         
        DISPLAY "no fue posible generar el reporte"
        EXIT PROGRAM 
    END IF   
    --Inicia el reporte de registros con rechazo
    START REPORT rpt_carga_enaclara TO XML HANDLER manejador_rpt
    -- Asigna el titulo del reporte
                
    OUTPUT TO REPORT rpt_carga_enaclara(reg_cifras.*, p_folio, v_nombre_usuario, v_nombre_archivo)                                                                
    FINISH REPORT rpt_carga_enaclara 
END FUNCTION

-- OBJETIVO: Emitir el reporte de liquidacion/preliquidacion
REPORT rpt_carga_enaclara(reg_cifras, p_folio, p_nombre_usuario, p_nombre_archivo)
  DEFINE  p_folio DECIMAL(9,0)
  DEFINE  reg_cifras RECORD
         --tpo_registro            LIKE acl_sum_aclaracion.tpo_registro
         tot_ap                  INTEGER --LIKE acl_sum_aclaracion.tot_ap
        ,suma_ap_pat             DECIMAL(18,2) --LIKE acl_sum_aclaracion.suma_ap_pat
        ,suma_am                 DECIMAL(18,2) --LIKE acl_sum_aclaracion.suma_am
        ,suma_aivs               DECIMAL(18,6) --LIKE acl_sum_aclaracion.suma_aivs
      END RECORD,

        v_fecha_reporte          DATE,
        p_nombre_usuario         LIKE seg_usuario.usuario_desc,
        p_nombre_archivo         LIKE glo_ctr_archivo.nombre_archivo

                                                                                                                                                                                          
FORMAT                                                                                        
                                                                                              
   FIRST PAGE HEADER                                                                          
                                                                                              
      LET v_fecha_reporte = TODAY CLIPPED                                                     
                                                                                              
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"                                                  
      PRINTX p_folio               
      PRINTX p_nombre_usuario
      PRINTX p_nombre_archivo
      PRINTX p_usuario_cod
                                                                                              
   ON EVERY ROW                                                                               
      PRINTX reg_cifras.*
                                                                                           
END REPORT  