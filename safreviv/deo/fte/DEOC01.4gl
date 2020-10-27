--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================
#########################################################################################
#Modulo       => DEO                                                                    #
#Programa     => DEOC01                                                                 #
#Objetivo     => Consultar los datos cargados de informacion recibida para devolucion   #
#                por errores de operacion                                               #
#Fecha inicio => Diciembre 28, 2011                                                     #
#########################################################################################

DATABASE safre_viv
GLOBALS "DEOG01.4gl"

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
   CALL ui.Interface.setContainer("name_1")
   CALL ui.Interface.setType("child")
      
   -- consulta de informacion recibida de OP98
   CALL fn_consulta_info_recibida_op98(p_usuario_cod)

END MAIN

{ ======================================================================
Clave: DEOC01
Nombre: fn_consulta_info_recibida_op98
Fecha creacion: Diciembre 28, 2011
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_info_recibida_op98(p_usuario_cod)
DEFINE p_usuario_cod            LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_folio                  STRING, -- folio
       v_afore_cod              LIKE cat_afore.afore_cod, -- afore elegida 
       v_dt_fecha_carga_inicial DATE, -- fecha de carga inicial
       v_dt_fecha_carga_final   DATE, -- fecha de carga final
       v_cbx_folios             ui.ComboBox, -- combo de afores
       v_cbx_afores             ui.ComboBox, -- combo de afores
       v_r_cat_afore            RECORD LIKE cat_afore.*, -- registro de afore
       v_s_cadena               STRING, -- cadena de texto
       v_r_glo_ctr_archivo      RECORD LIKE glo_ctr_archivo.*
       ,v_i_conArch            INTEGER

   OPEN WINDOW w_consulta_info_op98 WITH FORM "DEOC010"
   
   -- se le asigna el apuntado der combo a la variable
   LET v_cbx_afores = ui.ComboBox.forName("formonly.cmb_afore")
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
  
   -- se inicia el combobox en blanco
   CALL v_cbx_afores.clear()
   CALL v_cbx_folios.clear()
   
   -- el primer elemento en el combo es la cadena que indica que se debe elegir
   -- una afore
   CALL v_cbx_afores.addItem(-1," ")
   CALL v_cbx_folios.addItem(-1," ")
   
   INPUT
    v_folio,
    v_afore_cod,
    v_dt_fecha_carga_inicial,
    v_dt_fecha_carga_final
   WITHOUT DEFAULTS
   FROM
    cmb_folio,
    cmb_afore,
    dte_fecha_carga_inicial,
    dte_fecha_carga_final
   ATTRIBUTES (UNBUFFERED)
  
      BEFORE INPUT
         -- se asignan los valores por omision
         LET v_folio                  = -1
         LET v_afore_cod              = -1
         LET v_dt_fecha_carga_inicial = NULL
         LET v_dt_fecha_carga_final   = NULL

         -- se cargan las afores         
         DECLARE cur_afores CURSOR FOR 
         SELECT *
         FROM cat_afore
         ORDER BY
          afore_desc
         
         -- se llena el combo con los datos del catalogo de afores
         FOREACH cur_afores INTO v_r_cat_afore.*
            -- se agrega la afore al combo
            CALL v_cbx_afores.addItem(v_r_cat_afore.afore_cod,v_r_cat_afore.afore_desc)
         END FOREACH
         
         -- se libera el cursor
         FREE cur_afores
         
         
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
         SELECT DISTINCT g.*
         FROM glo_ctr_archivo g, deo_det_op98 d
         WHERE g.proceso_cod = g_proceso_cod_deo -- devolucion de operaciones
           AND g.estado = 2 -- integrado
           AND g.folio = d.folio

         -- <Se cruzan los folios con deo_det_op98 para obtener folios procesados>
         -- <  completamente y en forma exitosa para toda la afore.              >
         
         LET v_i_conArch = 0
         FOREACH cur_folios INTO v_r_glo_ctr_archivo.*
            LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " -", 
                v_r_glo_ctr_archivo.nombre_archivo 
            CALL v_cbx_folios.addItem(v_r_glo_ctr_archivo.folio 
                 USING "##########",v_s_cadena)
            -- Contador de archivos eoncontrados
            LET v_i_conArch = v_i_conArch + 1
         END FOREACH
         IF(v_i_conArch<1)THEN
            CALL fn_mensaje("Atención",
                 "No existen archivos recientemente integrados para consulta",
                 "info")
            EXIT INPUT
         END IF

         FREE cur_folios
         LET v_dt_fecha_carga_inicial = TODAY
         LET v_dt_fecha_carga_final = TODAY
         DISPLAY v_dt_fecha_carga_inicial, v_dt_fecha_carga_final 
              TO dte_fecha_carga_inicial, dte_fecha_carga_final

        
      ON ACTION ACCEPT
         IF(v_folio = '-1')THEN
            CALL fn_mensaje("Atención","Indique almenos un folio","info")
            NEXT FIELD cmb_folio
         END IF
         -- se envian los parametros de consulta
         CALL fn_muestra_consulta_cifras_recibidas(v_folio                 ,
                                                   v_afore_cod             ,
                                                   v_dt_fecha_carga_inicial,
                                                   v_dt_fecha_carga_final) 
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_info_op98

END FUNCTION

{
======================================================================
Clave: DEOC01
Nombre: fn_muestra_consulta_cifras_recibidas
Fecha creacion: Diciembre 28, 2011
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_muestra_consulta_cifras_recibidas(p_folio                 ,
                                              p_afore_cod             ,
                                              p_dt_fecha_carga_inicial,
                                              p_dt_fecha_carga_final)
DEFINE p_folio                  STRING, -- folio
       p_afore_cod              LIKE cat_afore.afore_cod, -- afore elegida 
       p_dt_fecha_carga_inicial DATE, -- fecha de carga inicial
       p_dt_fecha_carga_final   DATE, -- fecha de carga final
       v_r_cifras RECORD -- registro para consultar los datos
        check_sel       SMALLINT, -- Elemento seleccionado
        afore_cod        LIKE cat_afore.afore_cod, -- codigo de la afore
        afore_desc       LIKE cat_afore.afore_desc, -- descripcion de la afore
        total_registros  SMALLINT, -- total de registros
        aiv_viv92        DECIMAL(20,6), -- aplicacion intereses vivienda 92
        aiv_viv97        DECIMAL(20,6), -- aplicacion intereses vivienda 97
        intereses_viv92  DECIMAL(20,2), -- intereses vivienda 92
        intereses_viv97  DECIMAL(20,2), -- intereses vivienda 97
        importe_viv92    DECIMAL(20,2), -- importe vivienda 92
        importe_viv97    DECIMAL(20,2), -- importe vivienda 97
        fecha_devolucion CHAR(10), -- LIKE deo_monto_af_deposita.f_devolucion -- fecha de devolucion
        folio            DECIMAL(9,0)
       END RECORD,
       v_r_cifras_totales RECORD -- registro para consultar los datos
        check_sel       SMALLINT, -- Elemento seleccionado
        afore_cod        LIKE cat_afore.afore_cod, -- codigo de la afore
        afore_desc       LIKE cat_afore.afore_desc, -- descripcion de la afore
        total_registros  SMALLINT, -- total de registros
        aiv_viv92        DECIMAL(20,6), -- aplicacion intereses vivienda 92
        aiv_viv97        DECIMAL(20,6), -- aplicacion intereses vivienda 97
        intereses_viv92  DECIMAL(20,2), -- intereses vivienda 92
        intereses_viv97  DECIMAL(20,2), -- intereses vivienda 97
        importe_viv92    DECIMAL(20,2), -- importe vivienda 92
        importe_viv97    DECIMAL(20,2), -- importe vivienda 97
        fecha_devolucion CHAR(10), -- fecha de devolucion
        folio            DECIMAL(9,0)
       END RECORD,
       v_ar_cifras  DYNAMIC ARRAY OF RECORD -- arreglo para desplegar consulta
        check_sel       SMALLINT, -- Elemento seleccionado
        afore_cod        LIKE cat_afore.afore_cod, -- codigo de la afore
        afore_desc       LIKE cat_afore.afore_desc, -- descripcion de la afore
        total_registros  SMALLINT, -- total de registros
        aiv_viv92        DECIMAL(20,6), -- aplicacion intereses vivienda 92
        aiv_viv97        DECIMAL(20,6), -- aplicacion intereses vivienda 97
        intereses_viv92  DECIMAL(20,2), -- intereses vivienda 92
        intereses_viv97  DECIMAL(20,2), -- intereses vivienda 97
        importe_viv92    DECIMAL(20,2), -- importe vivienda 92
        importe_viv97    DECIMAL(20,2), -- importe vivienda 97
        fecha_devolucion CHAR(10), -- LIKE deo_monto_af_deposita.f_devolucion -- fecha de devolucion
        folio            DECIMAL(9,0)
       END RECORD
       ,v_si_indice        SMALLINT -- indice de arreglo
       ,v_s_sql            STRING -- cadena con una instruccion SQL
       ,v_s_clausula_fecha STRING -- clausula para filtro por fechas
       --
       ,v_s_claves_afores  STRING -- Lista de claves de afore para consulta
       ,v_i_cont           INTEGER -- Contador general
       ,v_c_fechas         CHAR(10)
  
   -- se realiza la consulta para obtener las cifras
   LET v_s_sql =
       "SELECT 0, a.afore_cod, a.afore_desc, COUNT(a.afore_cod), SUM(b.acc_devol_viv92),"
       ,"\n SUM(b.acc_devol_viv97), SUM(b.pes_int_devol_viv92), SUM(b.pes_int_devol_viv97),"
       ,"\n SUM(b.pes_devol_viv92), SUM(b.pes_devol_viv97), b.f_valor_devol_inf, b.folio"
       ,"\n FROM cat_afore a, deo_det_op98 b"
       ,"\nWHERE a.afore_cod = b.cve_afore"

   -- si se envio AFORE
   IF ( p_afore_cod <> "-1" ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND b.cve_afore =",p_afore_cod
   END IF

   -- si se envio folio
   IF ( p_folio <> "-1" ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND b.folio = ", p_folio
   END IF
   
   -- si se enviaron las fechas de carga
   IF ( p_dt_fecha_carga_inicial IS NOT NULL AND p_dt_fecha_carga_final IS NOT NULL ) THEN
      -- se obtienen los folios correspondientes a este rango de fechas

      CALL fn_crea_clausula_fechas(p_dt_fecha_carga_inicial, p_dt_fecha_carga_final) 
      RETURNING v_s_clausula_fecha
      -- Agrega descripcion de folios de la consulta de fechas
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND ", v_s_clausula_fecha CLIPPED
   END IF
   
   LET v_s_sql = v_s_sql CLIPPED
       ,"\n  GROUP BY 2,3,11,12"
       ,"\n  ORDER BY 2, 11" 
   
   --DISPLAY "Esta es mi consulta:",v_s_sql CLIPPED

   -- se prepara y ejecuta la consulta
   PREPARE sid_cons_deo_gral FROM v_s_sql
   DECLARE cur_cons_deo_gral CURSOR FOR sid_cons_deo_gral
   
   OPEN WINDOW w_consulta_cifras_recibidas WITH FORM "DEOC011"
   
   
   -- se inicia el registro de cifras totales
   LET v_r_cifras_totales.afore_cod        = NULL
   LET v_r_cifras_totales.afore_desc       = "Totales"
   LET v_r_cifras_totales.aiv_viv92        = 0
   LET v_r_cifras_totales.aiv_viv97        = 0
   LET v_r_cifras_totales.fecha_devolucion = NULL
   LET v_r_cifras_totales.importe_viv92    = 0
   LET v_r_cifras_totales.importe_viv97    = 0
   LET v_r_cifras_totales.intereses_viv92  = 0
   LET v_r_cifras_totales.intereses_viv97  = 0
   LET v_r_cifras_totales.total_registros  = 0
   
   -- se llena el arreglo de despliegue
   LET v_si_indice = 0
   
   FOREACH cur_cons_deo_gral INTO v_r_cifras.*
      -- se incrementa el indice
      LET v_si_indice = v_si_indice + 1
      -- se transfieren los datos al arreglo
      LET v_ar_cifras[v_si_indice].* = v_r_cifras.*

      # Corrige fecha f_valor_recepcion
      LET v_c_fechas = v_ar_cifras[v_si_indice].fecha_devolucion
      LET v_c_fechas = v_c_fechas[4,5],'/',v_c_fechas[1,2],'/',v_c_fechas[7,10]
      LET v_ar_cifras[v_si_indice].fecha_devolucion = v_c_fechas
      
      LET v_r_cifras_totales.aiv_viv92        = v_r_cifras_totales.aiv_viv92       + v_r_cifras.aiv_viv92
      LET v_r_cifras_totales.aiv_viv97        = v_r_cifras_totales.aiv_viv97       + v_r_cifras.aiv_viv97
      LET v_r_cifras_totales.importe_viv92    = v_r_cifras_totales.importe_viv92   + v_r_cifras.importe_viv92
      LET v_r_cifras_totales.importe_viv97    = v_r_cifras_totales.importe_viv97   + v_r_cifras.importe_viv97
      LET v_r_cifras_totales.intereses_viv92  = v_r_cifras_totales.intereses_viv92 + v_r_cifras.intereses_viv92
      LET v_r_cifras_totales.intereses_viv97  = v_r_cifras_totales.intereses_viv97 + v_r_cifras.intereses_viv97
      LET v_r_cifras_totales.total_registros  = v_r_cifras_totales.total_registros + v_r_cifras.total_registros
      
      -- se acumulan numero de registros y montos en registro de cifras totales
   END FOREACH
   FREE cur_cons_deo_gral
   
   -- se incrementa el indice
   LET v_si_indice = v_si_indice + 1
   -- se agrega al final el registro de cifras totales
   LET v_ar_cifras[v_si_indice].* = v_r_cifras_totales.*
   
   -- Fechas
   LET v_c_fechas = p_dt_fecha_carga_inicial
   LET v_c_fechas = v_c_fechas[4,5],'/',v_c_fechas[1,2],'/',v_c_fechas[7,10]
   -- Mostrar fecha y folio
   DISPLAY p_folio, v_c_fechas TO txt_folio, txt_fecha_carga 

   ## FJNM
   ## 31-01-2012
   ## Se convierte en Input Array para obtener afores a consultar.
   ##DISPLAY ARRAY v_ar_cifras TO tbl_saldos.*
   INPUT ARRAY v_ar_cifras FROM tbl_saldos.* 
      ATTRIBUTES(WITHOUT DEFAULTS, UNBUFFERED, 
        INSERT ROW = FALSE, APPEND ROW = FALSE, DELETE ROW =FALSE,
        ACCEPT = FALSE, CANCEL=FALSE)
   
      ON ACTION regresa
         EXIT INPUT

      ON CHANGE col_sel
         IF (v_ar_cifras[arr_curr()].check_sel  = 1 AND
             v_ar_cifras[arr_curr()].afore_desc = "Totales")THEN
            -- No se permite capturar este dato
            LET v_ar_cifras[arr_curr()].check_sel  = 0
            
         END IF
      
      ON ACTION recdet
         -- se invoca el detalle de registros de la AFORE elegida
         #LET v_afore_cod = v_ar_cifras[ARR_CURR()].afore_cod
         
         IF ( ARR_CURR()<=0 ) THEN
            CALL fn_mensaje("Atención","Debe seleccionar una AFORE de la lista","stop")
            CONTINUE INPUT
         END IF         
         -- Armar la lista de afores seleccionada
         LET v_s_claves_afores = "-1"
         FOR v_i_cont = 1 TO v_ar_cifras.getLength()
            	IF(v_ar_cifras[v_i_cont].check_sel = 1)THEN
            	   --Se agrega a la lista
            	   LET v_s_claves_afores = v_s_claves_afores CLIPPED, ", ",
            	       v_ar_cifras[v_i_cont].afore_cod
            	END IF
         END FOR
         IF(v_s_claves_afores CLIPPED = "-1")THEN
            CALL fn_mensaje("Atención","Debe seleccionar una AFORE de la lista","stop")
            CONTINUE INPUT
         END IF
         
         CALL fn_muestra_consulta_cifras_recibidas_detalle(
              v_s_claves_afores, p_folio, p_dt_fecha_carga_inicial, 
              p_dt_fecha_carga_final)
   
   END INPUT
   ##END DISPLAY
   
   CLOSE WINDOW w_consulta_cifras_recibidas

END FUNCTION

{
======================================================================
Clave: DEOC01
Nombre: fn_muestra_consulta_cifras_recibidas_detalle
Fecha creacion: Diciembre 28, 2011
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_muestra_consulta_cifras_recibidas_detalle(
         p_s_claves_afores, p_folio, p_dt_fecha_carga_inicial, 
         p_dt_fecha_carga_final)
TYPE record_detalle          RECORD
       v_i_num                 INTEGER
      ,v_i_idreg               INTEGER
      ,cve_afore               LIKE cat_afore.afore_cod -- afore
      ,nss                     LIKE afi_derechohabiente.nss  -- numero de seguridad social
      ,curp                    LIKE afi_derechohabiente.curp -- CURP
      ,nombre_trabajador       LIKE afi_derechohabiente.nombre_af -- Nombre del trabajador
      ,proc_origen_devolucion  VARCHAR(10) -- tipo autocorreccion???
      ,f_afectacion_contable   CHAR(10)--DATE -- fecha de afectacion contable
      ,num_aplic_interes_92    DECIMAL(20,6) -- aiv viv 92
      ,num_aplic_interes_97    DECIMAL(20,6) -- aiv viv 97
      ,importe_interes_viv92   DECIMAL(20,2) -- intereses viv 92
      ,importe_interes_viv97   DECIMAL(20,2) -- intereses viv 97
      ,importe_viv92           DECIMAL(20,2) -- importe viv 92
      ,importe_viv97           DECIMAL(20,2) -- importe viv 97
      ,f_valor_recepcion       CHAR(10) -- DATE -- fecha valor recepcion
      ,f_valor_devolucion      CHAR(10) -- DATE -- fecha valor devolucion
      ,result_operacion        VARCHAR(10) -- resultado operacion
     END RECORD
DEFINE p_s_claves_afores         STRING -- codigo de afore
       ,p_folio                  STRING -- folio
       ,p_dt_fecha_carga_inicial CHAR(10) -- DATE -- fecha de carga inicial
       ,p_dt_fecha_carga_final   CHAR(10) -- DATE -- fecha de carga final
       --
       ,v_r_detalle              record_detalle -- registro de detalle  
       ,v_ar_detalle             DYNAMIC ARRAY OF record_detalle-- arreglo para desplegar consulta
       ,v_r_subtotal             record_detalle -- registro subtotal
       ,v_r_encabezado           record_detalle -- registro encabezado
       ,v_si_indice              SMALLINT -- indice de arreglo
       ,v_s_sql                  STRING -- cadena con una instruccion SQL
       ,v_s_clausula_fecha       STRING -- ruta del formulario
       --
       ,v_afoAnt                 LIKE cat_afore.afore_cod -- clave anterior
       ,v_afore_desc             LIKE cat_afore.afore_desc
       ,v_i_cont_registro        INTEGER
       ,v_i_cont_no              INTEGER
       ,v_c_fechas               CHAR(10)
       --
      ,v_r_autocorreccion RECORD
          tpo_autocorreccion   CHAR(3)
          ,des_corta           LIKE deo_cat_autocorreccion.des_larga
       END RECORD
       ,v_ar_autocorreccion DYNAMIC ARRAY OF RECORD
          -- se cambia a tipo char 3 para presentarlo en pantalla
          --tpo_autocorreccion   LIKE deo_cat_autocorreccion.tpo_autocorreccion
          tpo_autocorreccion   CHAR(3)
          ,des_corta           LIKE deo_cat_autocorreccion.des_larga
       END RECORD
       ,v_si_AutoIndice             INTEGER
       ,v_s_cadena_qry         STRING

  
   -- se realiza la consulta para obtener las cifras
   LET v_s_sql =
       "SELECT 0,0, b.cve_afore, a.nss, a.curp, b.nombre_trabajador,"
       ,"\n       b.tpo_autocorreccion, b.f_afectacion_contable, "
       ,"\n       SUM(b.acc_devol_viv92), SUM(b.acc_devol_viv97),"
       ,"\n       SUM(b.pes_int_devol_viv92), SUM(b.pes_int_devol_viv97), "
       ,"\n       SUM(b.pes_devol_viv92),SUM(b.pes_devol_viv97),"
       ,"\n       b.f_valor_recep_afore, b.f_valor_devol_inf, "
       ,"\n        b.estado_devolucion, r.afore_desc "
       ,"\n  FROM afi_derechohabiente a, deo_det_op98 b, cat_afore r "
       ,"\nWHERE b.id_derechohabiente = a.id_derechohabiente"
       ,"\n  AND b.cve_afore = r.afore_cod"
       ,"\n  AND b.cve_afore IN(", p_s_claves_afores CLIPPED,")"
       
   -- si se envio folio
   IF ( p_folio <> "-1" ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND b.folio = ", p_folio, ""
   END IF
   -- si se enviaron las fechas de carga
   IF ( p_dt_fecha_carga_inicial IS NOT NULL AND p_dt_fecha_carga_final IS NOT NULL ) THEN
      CALL fn_crea_clausula_fechas(p_dt_fecha_carga_inicial, p_dt_fecha_carga_final) 
      RETURNING v_s_clausula_fecha
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND ", v_s_clausula_fecha
   END IF
   LET v_s_sql = v_s_sql CLIPPED
          ,"\n  GROUP BY 3,4,5,6,7,8,15,16,17,18"
          ,"\n  ORDER BY 3,6,5"

   DISPLAY "-- ",v_s_sql
   -- se prepara y ejecuta la consulta
   PREPARE Prpr_cons_deo_detalle FROM v_s_sql CLIPPED
   DECLARE cur_cons_deo_detalle CURSOR FOR Prpr_cons_deo_detalle
      
   -- se llena el arreglo de despliegue
   LET v_si_indice = 0
   LET v_r_subtotal.curp = "SUB-TOTAL"
   LET v_r_subtotal.v_i_num   = NULL
   LET v_r_subtotal.v_i_idreg = NULL
   LET v_r_subtotal.cve_afore = NULL

   -- Registro de encabezado.
   LET v_r_encabezado.v_i_num   = NULL
   LET v_r_encabezado.v_i_idreg = NULL
   LET v_r_encabezado.cve_afore = NULL
   LET v_r_encabezado.num_aplic_interes_92  = NULL
   LET v_r_encabezado.num_aplic_interes_97  = NULL
   LET v_r_encabezado.importe_interes_viv92 = NULL
   LET v_r_encabezado.importe_interes_viv97 = NULL
   LET v_r_encabezado.importe_viv92         = NULL
   LET v_r_encabezado.importe_viv97         = NULL
   -- Registro de subtotales.
   LET v_r_subtotal.num_aplic_interes_92  = 0
   LET v_r_subtotal.num_aplic_interes_97  = 0
   LET v_r_subtotal.importe_interes_viv92 = 0
   LET v_r_subtotal.importe_interes_viv97 = 0
   LET v_r_subtotal.importe_viv92         = 0
   LET v_r_subtotal.importe_viv97         = 0
   
   LET v_i_cont_no = 0
   LET v_i_cont_registro = 0
   LET v_afoAnt = -1
   CALL v_ar_detalle.clear();
   FOREACH cur_cons_deo_detalle INTO v_r_detalle.*,v_afore_desc
      IF(v_afoAnt<>v_r_detalle.cve_afore)THEN
         IF(v_afoAnt <> -1)THEN
            -- Se agrega renglon de subtotal de afore anterior
            ## Insertar el subtotal anterior he inicializar
            -- se incrementa el indice
            LET v_si_indice = v_si_indice + 1
            LET v_ar_detalle[v_si_indice].* = v_r_subtotal.*         	  
            ## Reinicia el registro del subtotal.
            LET v_r_subtotal.num_aplic_interes_92  = 0
            LET v_r_subtotal.num_aplic_interes_97  = 0
            LET v_r_subtotal.importe_interes_viv92 = 0
            LET v_r_subtotal.importe_interes_viv97 = 0
            LET v_r_subtotal.importe_viv92         = 0
            LET v_r_subtotal.importe_viv97         = 0
         
         END IF         	  
         ## Se pone encabezado Clave de afore
         LET v_r_encabezado.curp = "Clave Afore"           
         ## Se pone encabezado Clave y Descripcion de afore
         LET v_r_encabezado.nombre_trabajador = v_r_detalle.cve_afore, "-", v_afore_desc CLIPPED
         LET v_si_indice = v_si_indice + 1
         -- se paso el encabezado al arreglo
         LET v_ar_detalle[v_si_indice].* = v_r_encabezado.*
         -- Se reinicia el numero de registros de afore.
         LET v_i_cont_no = 0
      END IF

      -- Contador de registros 
      LET v_i_cont_registro = v_i_cont_registro + 1
      LET v_i_cont_no = v_i_cont_no + 1
      LET v_r_detalle.v_i_num   = v_i_cont_no
      LET v_r_detalle.v_i_idreg = v_i_cont_registro

      -- se incrementa el indice
      LET v_si_indice = v_si_indice + 1
      -- se transfieren los datos al arreglo
      LET v_ar_detalle[v_si_indice].* = v_r_detalle.*
      # Corrige fecha contable
      LET v_c_fechas = v_ar_detalle[v_si_indice].f_afectacion_contable
      LET v_c_fechas = v_c_fechas[4,5],'/',v_c_fechas[1,2],'/',v_c_fechas[7,10]
      LET v_ar_detalle[v_si_indice].f_afectacion_contable = v_c_fechas
      # Corrige fecha f_valor_devolucion
      LET v_c_fechas = v_ar_detalle[v_si_indice].f_valor_devolucion
      LET v_c_fechas = v_c_fechas[4,5],'/',v_c_fechas[1,2],'/',v_c_fechas[7,10]
      LET v_ar_detalle[v_si_indice].f_valor_devolucion = v_c_fechas
      # Corrige fecha f_valor_recepcion
      LET v_c_fechas = v_ar_detalle[v_si_indice].f_valor_recepcion
      LET v_c_fechas = v_c_fechas[4,5],'/',v_c_fechas[1,2],'/',v_c_fechas[7,10]
      LET v_ar_detalle[v_si_indice].f_valor_recepcion = v_c_fechas
      
      -- Calcula subtotales
      LET v_r_subtotal.num_aplic_interes_92  = v_r_subtotal.num_aplic_interes_92  + v_r_detalle.num_aplic_interes_92  
      LET v_r_subtotal.num_aplic_interes_97  = v_r_subtotal.num_aplic_interes_97  + v_r_detalle.num_aplic_interes_97  
      LET v_r_subtotal.importe_interes_viv92 = v_r_subtotal.importe_interes_viv92 + v_r_detalle.importe_interes_viv92 
      LET v_r_subtotal.importe_interes_viv97 = v_r_subtotal.importe_interes_viv97 + v_r_detalle.importe_interes_viv97 
      LET v_r_subtotal.importe_viv92         = v_r_subtotal.importe_viv92         + v_r_detalle.importe_viv92         
      LET v_r_subtotal.importe_viv97         = v_r_subtotal.importe_viv97         + v_r_detalle.importe_viv97         
      
      -- Recuperar la clave anterior
      LET v_afoAnt = v_r_detalle.cve_afore
   END FOREACH
   FREE cur_cons_deo_detalle
   -- Verifica si se encontraron datos
   IF(v_si_indice>0)THEN
      LET v_s_cadena_qry =
          "SELECT tpo_autocorreccion, des_larga"
          ,"\n  FROM safre_viv:deo_cat_autocorreccion"
      PREPARE Prpr_ObtDatosAutoCorr FROM v_s_cadena_qry CLIPPED
      DECLARE Curr_ObtDatosAutoCorr CURSOR FOR Prpr_ObtDatosAutoCorr
   
      --Inicia indice
      LET v_si_AutoIndice = 0
      CALL v_ar_autocorreccion.CLEAR()
      FOREACH Curr_ObtDatosAutoCorr INTO v_r_autocorreccion.*
         -- Incrementa indice para guardar.
         LET v_si_AutoIndice = v_si_AutoIndice + 1
         LET v_ar_autocorreccion[v_si_AutoIndice].* = v_r_autocorreccion.*
         LET v_ar_autocorreccion[v_si_AutoIndice].tpo_autocorreccion = 
             v_r_autocorreccion.tpo_autocorreccion USING "&&&"
      END FOREACH


      OPEN WINDOW w_consulta_cifras_detalle WITH FORM "DEOC012"
      -- se incrementa el indice
      LET v_si_indice = v_si_indice + 1
      LET v_ar_detalle[v_si_indice].* = v_r_subtotal.*

      DIALOG 
         ATTRIBUTES(UNBUFFERED)
   
         DISPLAY ARRAY v_ar_detalle TO tbl_detalle.*
   
         END DISPLAY
         -- Arreglo de catalogo tipos de autocorreccion 
         DISPLAY ARRAY v_ar_autocorreccion TO sc_auto.*
      
         END DISPLAY

         ON ACTION regresa
            EXIT DIALOG
      END DIALOG
      
      CLOSE WINDOW w_consulta_cifras_detalle
   ELSE
      DISPLAY "No hay datos"
   END IF
   
END FUNCTION

#Objetivo: Generar la clausa IN para ser usada como filtro en la consulta
#          de datos cargados para devolucion por errores de operacion 
FUNCTION fn_crea_clausula_fechas(p_dt_fecha_carga_inicial, p_dt_fecha_carga_final)
DEFINE p_dt_fecha_carga_inicial LIKE glo_ctr_archivo.f_actualiza,
       p_dt_fecha_carga_final   LIKE glo_ctr_archivo.f_actualiza,
       v_folio                  LIKE glo_folio.folio, -- folio
       v_s_clausula             STRING, -- clausula construida
       v_si_indice              SMALLINT, -- contador
       v_s_sql                  STRING -- cadena con una instruccion SQL

   -- consulta de los folios que esten dentro del intervalo
   -- dado por la fecha inicial y final

   LET v_s_sql =            "SELECT UNIQUE(folio)\n"
   LET v_s_sql = v_s_sql || "FROM glo_ctr_archivo\n"
   LET v_s_sql = v_s_sql || "WHERE\n"
   LET v_s_sql = v_s_sql || " f_actualiza BETWEEN '" || p_dt_fecha_carga_inicial || "' AND '" || p_dt_fecha_carga_final || "'\n"
   LET v_s_sql = v_s_sql || "AND\n"
   LET v_s_sql = v_s_sql || " proceso_cod = " || g_proceso_cod_deo
   ## > Se valida que no se tomen folios nulos para evitar errores de consultas
   ## >  posteriores
   LET v_s_sql = v_s_sql || " AND folio IS NOT NULL\n"
   --LET v_s_sql = v_s_sql || "AND\n"
   --LET v_s_sql = v_s_sql || " opera_cod = 3\n"
   LET v_s_sql = v_s_sql || "ORDER BY\n"
   LET v_s_sql = v_s_sql || " folio\n"

   PREPARE sid_folioscarga FROM v_s_sql
   DECLARE cur_folioscarga CURSOR FOR sid_folioscarga

   LET v_s_clausula = " b.folio IN (\n"

   LET v_si_indice = 1
   
   FOREACH cur_folioscarga INTO v_folio
      LET v_s_clausula = v_s_clausula, v_folio, ",\n"
   END FOREACH

   LET v_s_clausula = v_s_clausula CLIPPED, " -1)\n"
  
   -- se devuelve la clausula construida
   RETURN v_s_clausula
END FUNCTION


