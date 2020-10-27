--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================
#########################################################################################
#Modulo       => DEO                                                                    #
#Programa     => DEOC04                                                                 #
#Objetivo     => Consultar los datos cargados de informacion recibida para devolucion   #
#                por errores de operacion por rechazos en errores de archivo            #
#Fecha inicio => Diciembre 28, 2011                                                     #
#########################################################################################

DATABASE safre_viv
GLOBALS
DEFINE g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS
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

   -- Inicializa datos de operacon
   LET g_proceso_cod = g_proceso_cod_deo
   
   
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   --CALL ui.Interface.setContainer("name_1")
   --CALL ui.Interface.setType("child")
    
   -- consulta de informacion recibida de OP98
   CALL fn_consulta_info_recha_op98(p_usuario_cod)

END MAIN

{ ======================================================================
Clave: DEOC04
Nombre: fn_consulta_info_recha_op98
Fecha creacion: Diciembre 28, 2011
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_info_recha_op98(p_usuario_cod)
DEFINE p_usuario_cod            LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_folio                  STRING, -- folio
       v_afore_cod              LIKE cat_afore.afore_cod, -- afore elegida 
       v_dt_fecha_carga_inicial DATE, -- fecha de carga inicial
       v_dt_fecha_carga_final   DATE, -- fecha de carga final
       v_cbx_folios             ui.ComboBox, -- combo de afores
       v_cbx_afores             ui.ComboBox, -- combo de afores
       v_r_cat_afore            RECORD LIKE cat_afore.*, -- registro de afore
       v_s_cadena               STRING, -- cadena de texto
       v_si_indice              SMALLINT, -- indice para arreglo
       v_r_glo_ctr_archivo      RECORD
          proceso_cod    LIKE glo_ctr_archivo.proceso_cod
          ,opera_cod      LIKE glo_ctr_archivo.opera_cod
          ,nombre_archivo LIKE glo_ctr_archivo.nombre_archivo
          ,folio          CHAR(10)
          ,estado         LIKE glo_ctr_archivo.estado
          ,f_actualiza    LIKE glo_ctr_archivo.f_actualiza
          ,usuario        LIKE glo_ctr_archivo.usuario
       END RECORD
       ,v_d_folio         DECIMAL(9,0)

   OPEN WINDOW w_consulta_recha_op98 WITH FORM "DEOC010"
   
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
   LET v_dt_fecha_carga_inicial = TODAY
   LET v_dt_fecha_carga_final   = TODAY
   
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
          ORDER BY afore_desc
         
         -- se llena el combo con los datos del catalogo de afores
         FOREACH cur_afores INTO v_r_cat_afore.*
            -- se agrega la afore al combo
            CALL v_cbx_afores.addItem(v_r_cat_afore.afore_cod,v_r_cat_afore.afore_desc)
         END FOREACH
         
         -- se libera el cursor
         FREE cur_afores
         
         -- De llena el arreglo de folios con los datos
         -- Combina con rechazados para verificar si hay datos rechazados 
         DECLARE cur_folios CURSOR FOR
           SELECT DISTINCT g.*
             FROM glo_ctr_archivo g
            WHERE g.proceso_cod = g_proceso_cod_deo -- devolucion de operaciones
              AND ( g.folio IN (
                   SELECT DISTINCT a.folio FROM deo_det_op98_rch a
                  )
                  OR g.folio IN (
                  SELECT DISTINCT b.folio FROM deo_cza_op98_rch b
                  )
                  )         
            --AND g.estado = 2 -- integrado 

         LET v_si_indice = 0
         FOREACH cur_folios INTO v_r_glo_ctr_archivo.*
            LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " -", v_r_glo_ctr_archivo.nombre_archivo 
            CALL v_cbx_folios.addItem(v_r_glo_ctr_archivo.folio  USING "##########", v_s_cadena)
            LET v_si_indice = v_si_indice + 1
         END FOREACH
         IF(v_si_indice <=0)THEN
         	 CALL fn_mensaje("Atención","No existen archivos rechazados","stop")
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
         LET v_d_folio = v_folio
         LET v_si_indice = 0
         -- >> Verificar el origen del folio en rechazado por archvo.
         SELECT COUNT(*)
           INTO v_si_indice
           FROM deo_cza_op98_rch 
          WHERE folio =  v_d_folio
         IF(v_si_indice <=0)THEN
            -- <Se muestran errores detalle de archivo>
            -- Se envian los parametros de consulta
            CALL fn_muestra_consulta_recha_detalle(v_folio                 ,
                                                   v_afore_cod             ,
                                                   v_dt_fecha_carga_inicial,
                                                   v_dt_fecha_carga_final)
         ELSE
            -- <Se muestran errores de sumario o encabezado de archivo>
            CALL fn_muestra_rechazo_sumario_encab(
                  v_folio, v_dt_fecha_carga_inicial 
                 ,v_dt_fecha_carga_final)
         END IF

      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_recha_op98

END FUNCTION

{
======================================================================
Clave: DEOC04
Nombre: fn_muestra_consulta_recha_detalle
Fecha creacion: Diciembre 28, 2011
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_muestra_consulta_recha_detalle(p_folio                 ,
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
        aiv_viv92        LIKE deo_mto_deposito.tot_pes_devolucion, -- aplicacion intereses vivienda 92
        aiv_viv97        LIKE deo_mto_deposito.tot_pes_devolucion, -- aplicacion intereses vivienda 97
        intereses_viv92  LIKE deo_mto_deposito.tot_pes_devolucion, -- intereses vivienda 92
        intereses_viv97  LIKE deo_mto_deposito.tot_pes_devolucion, -- intereses vivienda 97
        importe_viv92    LIKE deo_mto_deposito.tot_pes_devolucion, -- importe vivienda 92
        importe_viv97    LIKE deo_mto_deposito.tot_pes_devolucion, -- importe vivienda 97
        fecha_devolucion LIKE deo_mto_deposito.f_valor_devol_inf, -- fecha de devolucion
        folio            CHAR(10)
       END RECORD,
       v_r_cifras_totales RECORD -- registro para consultar los datos
        check_sel       SMALLINT, -- Elemento seleccionado
        afore_cod        LIKE cat_afore.afore_cod, -- codigo de la afore
        afore_desc       LIKE cat_afore.afore_desc, -- descripcion de la afore
        total_registros  SMALLINT, -- total de registros
        aiv_viv92        LIKE deo_mto_deposito.tot_pes_devolucion, -- aplicacion intereses vivienda 92
        aiv_viv97        LIKE deo_mto_deposito.tot_pes_devolucion, -- aplicacion intereses vivienda 97
        intereses_viv92  LIKE deo_mto_deposito.tot_pes_devolucion, -- intereses vivienda 92
        intereses_viv97  LIKE deo_mto_deposito.tot_pes_devolucion, -- intereses vivienda 97
        importe_viv92    LIKE deo_mto_deposito.tot_pes_devolucion, -- importe vivienda 92
        importe_viv97    LIKE deo_mto_deposito.tot_pes_devolucion, -- importe vivienda 97
        fecha_devolucion LIKE deo_mto_deposito.f_valor_devol_inf, -- fecha de devolucion
        folio            CHAR(10)
       END RECORD,
       v_ar_cifras  DYNAMIC ARRAY OF RECORD -- arreglo para desplegar consulta
        check_sel       SMALLINT, -- Elemento seleccionado
        afore_cod        LIKE cat_afore.afore_cod, -- codigo de la afore
        afore_desc       LIKE cat_afore.afore_desc, -- descripcion de la afore
        total_registros  SMALLINT, -- total de registros
        aiv_viv92        LIKE deo_mto_deposito.tot_pes_devolucion, -- aplicacion intereses vivienda 92
        aiv_viv97        LIKE deo_mto_deposito.tot_pes_devolucion, -- aplicacion intereses vivienda 97
        intereses_viv92  LIKE deo_mto_deposito.tot_pes_devolucion, -- intereses vivienda 92
        intereses_viv97  LIKE deo_mto_deposito.tot_pes_devolucion, -- intereses vivienda 97
        importe_viv92    LIKE deo_mto_deposito.tot_pes_devolucion, -- importe vivienda 92
        importe_viv97    LIKE deo_mto_deposito.tot_pes_devolucion, -- importe vivienda 97
        fecha_devolucion LIKE deo_mto_deposito.f_valor_devol_inf, -- fecha de devolucion
        folio            CHAR(10)
       END RECORD
       ,v_si_indice        SMALLINT -- indice de arreglo
       ,v_s_sql            STRING -- cadena con una instruccion SQL
       ,v_s_clausula_fecha STRING -- clausula para filtro por fechas
       --
       ,v_s_claves_afores   STRING -- Lista de claves de afore para consulta
       ,v_i_cont            INTEGER -- Contador general
  
   -- se realiza la consulta para obtener las cifras
   LET v_s_sql =
       "SELECT 0, a.afore_cod, a.afore_desc, COUNT(a.afore_cod), SUM(b.acc_devol_viv92),"
       ,"\n SUM(b.acc_devol_viv97), SUM(b.pes_int_devol_viv92), SUM(b.pes_int_devol_viv97),"
       ,"\n SUM(b.pes_devol_viv92), SUM(b.pes_devol_viv97), b.f_valor_devol_inf, b.folio"
       ,"\n FROM cat_afore a, deo_det_op98_rch b"
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

      CALL fn_obt_folios_x_fecha(p_dt_fecha_carga_inicial, p_dt_fecha_carga_final) 
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
   
   IF(v_si_indice<= 0)THEN
    -- No hay registros encontrados
    CALL fn_mensaje("Atención","No existen datos en archivo rechazado con los criterios indicados","stop")
    RETURN
   END IF
   
   -- se incrementa el indice
   LET v_si_indice = v_si_indice + 1
   -- se agrega al final el registro de cifras totales
   LET v_ar_cifras[v_si_indice].* = v_r_cifras_totales.*
   
   OPEN WINDOW w_consulta_recha_recibidas WITH FORM "DEOC043"
   DISPLAY p_folio, p_dt_fecha_carga_inicial TO TXT_FOLIO, TXT_FECHA_CARGA

   ## FJNM
   ## 31-01-2012
   ## Se convierte en Input Array para obtener afores a consultar.
   ##DISPLAY ARRAY v_ar_cifras TO tbl_saldos.*
   INPUT ARRAY v_ar_cifras FROM tbl_saldos.* 
      ATTRIBUTES(WITHOUT DEFAULTS, UNBUFFERED, 
      INSERT ROW = FALSE, APPEND ROW = FALSE, DELETE ROW =FALSE, 
      ACCEPT = FALSE, CANCEL = FALSE, ACCEPT = FALSE, CANCEL=FALSE)
   
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
         -- Se usa la poscion 1 porque tiene el mismo folio 
         CALL fn_muestra_rechazo_detalle_devol(
              v_s_claves_afores, v_ar_cifras[1].folio, 
              p_dt_fecha_carga_inicial, p_dt_fecha_carga_final)
              
   END INPUT
   ##END DISPLAY
   
   CLOSE WINDOW w_consulta_recha_recibidas

END FUNCTION -- fn_muestra_consulta_recha_detalle

{
======================================================================
Clave: DEOC04
Nombre: fn_muestra_consulta_recha_detalle_detalle
Fecha creacion: Diciembre 28, 2011
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_muestra_consulta_recha_detalle_detalle(
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
      ,f_afectacion_contable   DATE -- fecha de afectacion contable
      ,num_aplic_interes_92    LIKE deo_mto_deposito.tot_pes_devolucion -- aiv viv 92
      ,num_aplic_interes_97    LIKE deo_mto_deposito.tot_pes_devolucion -- aiv viv 97
      ,importe_interes_viv92   LIKE deo_mto_deposito.tot_pes_devolucion -- intereses viv 92
      ,importe_interes_viv97   LIKE deo_mto_deposito.tot_pes_devolucion -- intereses viv 97
      ,importe_viv92           LIKE deo_mto_deposito.tot_pes_devolucion -- importe viv 92
      ,importe_viv97           LIKE deo_mto_deposito.tot_pes_devolucion -- importe viv 97
      ,f_valor_recepcion       DATE -- fecha valor recepcion
      ,f_valor_devolucion      DATE -- fecha valor devolucion
      ,result_operacion        VARCHAR(10) -- resultado operacion
      ,cod_rechazo_1           SMALLINT
      ,cod_rechazo_2           SMALLINT
      ,cod_rechazo_3           SMALLINT
     END RECORD
DEFINE p_s_claves_afores         STRING -- codigo de afore
       ,p_folio                  STRING -- folio
       ,p_dt_fecha_carga_inicial DATE -- fecha de carga inicial
       ,p_dt_fecha_carga_final   DATE -- fecha de carga final
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

  
   -- se realiza la consulta para obtener las cifras
   LET v_s_sql =
       "SELECT 0,0, b.cve_afore, a.nss, a.curp, b.nombre_trabajador,"
       ,"\n       b.tipo_autocorreccion, b.f_afectacion_contable, "
       ,"\n       SUM(b.acc_devol_viv92), SUM(b.acc_devol_viv97),"
       ,"\n       SUM(b.pes_int_devol_viv92), SUM(b.pes_int_devol_viv97), "
       ,"\n       SUM(b.pes_devol_viv92),SUM(b.pes_devol_viv97),"
       ,"\n       b.f_valor_recep_afore, b.f_valor_devol_inf, "
       ,"\n       b.estado_devolucion, cod_rechazo_1, "
       ,"\n       cod_rechazo_2, cod_rechazo_3, r.afore_desc "
       ,"\n  FROM afi_derechohabiente a, deo_det_op98_rch b, cat_afore r "
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
      CALL fn_obt_folios_x_fecha(p_dt_fecha_carga_inicial, p_dt_fecha_carga_final) 
      RETURNING v_s_clausula_fecha
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND ", v_s_clausula_fecha
   END IF
   LET v_s_sql = v_s_sql CLIPPED
          ,"\n  GROUP BY 3,4,5,6,7,8,15,16,17,18,19,20,21"
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
      OPEN WINDOW w_consulta_recha_detalle WITH FORM "DEOC041"
      -- se incrementa el indice
      LET v_si_indice = v_si_indice + 1
      LET v_ar_detalle[v_si_indice].* = v_r_subtotal.*
   
      DISPLAY ARRAY v_ar_detalle TO tbl_detalle.*
   
         ON ACTION regresa
            EXIT DISPLAY
   
      END DISPLAY
      CLOSE WINDOW w_consulta_recha_detalle
   ELSE
      DISPLAY "No hay datos"
   END IF
   
END FUNCTION -- fn_muestra_consulta_recha_detalle_detalle

#Objetivo: Generar la clausa IN para ser usada como filtro en la consulta
#          de datos cargados para devolucion por errores de operacion 
FUNCTION fn_obt_folios_x_fecha(p_dt_fecha_carga_inicial, p_dt_fecha_carga_final)
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
   LET v_s_sql = v_s_sql || " f_actualiza BETWEEN '" || p_dt_fecha_carga_inicial 
       || "' AND '" || p_dt_fecha_carga_final || "'\n"
   LET v_s_sql = v_s_sql || "AND\n"
   LET v_s_sql = v_s_sql || " proceso_cod = " || g_proceso_cod_deo
   ## FJNM
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
      DISPLAY "folio encontrado:",v_folio,"-"
      LET v_s_clausula = v_s_clausula, v_folio, ",\n"
   END FOREACH

   LET v_s_clausula = v_s_clausula CLIPPED, " -1)\n"
  
   -- se devuelve la clausula construida
   RETURN v_s_clausula
END FUNCTION

{
======================================================================
Clave: DEOC04
Nombre: fn_muestra_rechazo_detalle_devol
Fecha creacion: Diciembre 28, 2011
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_muestra_rechazo_detalle_devol(
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
      ,f_afectacion_contable   DATE -- fecha de afectacion contable
      ,num_aplic_interes_92    LIKE deo_mto_deposito.tot_pes_devolucion -- aiv viv 92
      ,num_aplic_interes_97    LIKE deo_mto_deposito.tot_pes_devolucion -- aiv viv 97
      ,importe_interes_viv92   LIKE deo_mto_deposito.tot_pes_devolucion -- intereses viv 92
      ,importe_interes_viv97   LIKE deo_mto_deposito.tot_pes_devolucion -- intereses viv 97
      ,importe_viv92           LIKE deo_mto_deposito.tot_pes_devolucion -- importe viv 92
      ,importe_viv97           LIKE deo_mto_deposito.tot_pes_devolucion -- importe viv 97
      ,f_valor_recepcion       DATE -- fecha valor recepcion
      ,f_valor_devolucion      DATE -- fecha valor devolucion
      ,result_operacion        VARCHAR(10) -- resultado operacion
      ,cod_rechazo_1           SMALLINT
      ,cod_rechazo_2           SMALLINT
      ,cod_rechazo_3           SMALLINT
     END RECORD
DEFINE p_s_claves_afores       STRING -- codigo de afore
       ,p_folio                CHAR(10)
       ,p_dt_fecha_carga_inicial DATE -- fecha de carga inicial
       ,p_dt_fecha_carga_final DATE -- fecha de carga final
       --
       ,v_si_indice            SMALLINT -- indice de arreglo
       ,v_s_sql                STRING -- cadena con una instruccion SQL
       -- Arreglo de Nss.
       ,v_ar_concNss           DYNAMIC ARRAY OF RECORD
         afore_cod             LIKE cat_afore.afore_cod
        ,afore_desc            LIKE cat_afore.afore_desc
        ,v_c_nss               CHAR(11)
        ,v_s_nombre_trabajador CHAR(50)
       END RECORD
       ,v_reg_concNss          RECORD
         afore_cod             LIKE cat_afore.afore_cod
        ,afore_desc            LIKE cat_afore.afore_desc
        ,v_c_nss               CHAR(11)
        ,v_s_nombre_trabajador CHAR(50)
       END RECORD
       -- Arreglo de Rechazos
       ,v_ar_detRecha           DYNAMIC ARRAY OF RECORD
         v_d_rechazo            SMALLINT
        ,v_c_motivo            CHAR(50)
        ,v_c_decrip            CHAR(50)
        ,v_c_posicion          CHAR(50)
        ,v_c_valor             CHAR(50)
       END RECORD
       ,v_reg_detcNss          RECORD
         v_d_rechazo            SMALLINT
        ,v_c_motivo            CHAR(50)
        ,v_c_decrip            CHAR(50)
        ,v_c_posicion          CHAR(50)
        ,v_c_valor             CHAR(50)
       END RECORD
       ,v_s_qry                STRING   -- para consulta secundaria
       ,v_si_subIndice         SMALLINT


   -- Arma consulta para obtener los rechazos a nivel detalle de nss y folio
   LET v_s_qry =
       "SELECT i.cod_rechazo_1, 'motivo', r.des_corta"
       ,"\n      ,'['||l.pos_inicial||'-'||l.pos_final||']'"
       ,"\n      ,l.campo_desc"
       ,"\n FROM (deo_cat_rechazo r LEFT OUTER JOIN cat_campo l"
       ,"\n   ON r.layout_cod = l.layout_cod"
       ,"\n  AND r.registro   = l.registro"
       ,"\n  AND r.campo_cod  = l.campo_cod) RIGHT OUTER JOIN deo_det_op98_rch i"
       ,"\n   ON i.cod_rechazo_1 = r.cod_rechazo"
       ,"\nWHERE i.folio = ?"
       ,"\n  AND i.nss   = ?"

   PREPARE Prpr_ObtDatosUnoRecha FROM v_s_qry CLIPPED
   	
   LET v_s_qry =
       "SELECT i.cod_rechazo_2, 'motivo', r.des_corta"
       ,"\n      ,'['||l.pos_inicial||'-'||l.pos_final||']'"
       ,"\n      ,l.campo_desc"
       ,"\n FROM (deo_cat_rechazo r LEFT OUTER JOIN cat_campo l"
       ,"\n   ON r.layout_cod = l.layout_cod"
       ,"\n  AND r.registro   = l.registro"
       ,"\n  AND r.campo_cod  = l.campo_cod) RIGHT OUTER JOIN deo_det_op98_rch i"
       ,"\n   ON i.cod_rechazo_2 = r.cod_rechazo"
       ,"\nWHERE i.folio = ?"
       ,"\n  AND i.nss   = ?"

   PREPARE Prpr_ObtDatosDosRecha FROM v_s_qry CLIPPED
   
   LET v_s_qry =
       "SELECT i.cod_rechazo_3, 'motivo', r.des_corta"
       ,"\n      ,'['||l.pos_inicial||'-'||l.pos_final||']'"
       ,"\n      ,l.campo_desc"
       ,"\n FROM (deo_cat_rechazo r LEFT OUTER JOIN cat_campo l"
       ,"\n   ON r.layout_cod = l.layout_cod"
       ,"\n  AND r.registro   = l.registro"
       ,"\n  AND r.campo_cod  = l.campo_cod) RIGHT OUTER JOIN deo_det_op98_rch i"
       ,"\n   ON i.cod_rechazo_3 = r.cod_rechazo"
       ,"\nWHERE i.folio = ?"
       ,"\n  AND i.nss   = ?"
   PREPARE Prpr_ObtDatosTreRecha FROM v_s_qry CLIPPED
   
   LET v_s_sql =       
       -- trae unicamente los registros con ERROR
       "SELECT DISTINCT a.afore_cod, a.afore_desc, b.nss, b.nombre_trabajador"
       ,"\n  FROM deo_det_op98_rch b, cat_afore a"
       ,"\nWHERE b.cve_afore IN(", p_s_claves_afores CLIPPED,")"
       ,"\n  AND b.folio =", p_folio
       ,"\n  AND b.cve_afore = a.afore_cod"
       ,"\n  AND (b.cod_rechazo_1 <> 0 OR cod_rechazo_2 <> 0 OR cod_rechazo_3 <> 0)"
       ,"\n  ORDER BY 3"
       

   -- se prepara y ejecuta la consulta
   PREPARE Prpr_cons_NssTrab_detalle FROM v_s_sql CLIPPED
   DECLARE Curr_cons_NssTrab_detalle CURSOR FOR Prpr_cons_NssTrab_detalle
      
   -- se llena el arreglo de despliegue
   LET v_si_indice = 0
   
   CALL v_ar_concNss.clear();
   FOREACH Curr_cons_NssTrab_detalle INTO v_reg_concNss.*    
       -- Se incrementa el indice
      LET v_si_indice = v_si_indice + 1
      LET v_ar_concNss[v_si_indice].* = v_reg_concNss.*     
   END FOREACH
   FREE Curr_cons_NssTrab_detalle
   -- Verifica si se encontraron datos
   IF(v_si_indice>0)THEN
      OPEN WINDOW w_consulta_recha_detalle WITH FORM "DEOC042"
      
      DIALOG ATTRIBUTES(UNBUFFERED)
         -- Arreglo de NSS
         DISPLAY ARRAY v_ar_concNss TO tbl_nss.*
         
            --ON ACTION ACCEPT
            BEFORE DISPLAY
 -- Cuando cambia de renglo se prepara para cargar datos detalle
               CALL v_ar_detRecha.CLEAR()
               LET v_si_subIndice = 0

               EXECUTE Prpr_ObtDatosUnoRecha 
                 USING p_folio, v_ar_concNss[1].v_c_nss
                  INTO v_reg_detcNss.*

               IF NOT(v_reg_detcNss.v_d_rechazo IS NULL 
                  OR v_reg_detcNss.v_d_rechazo <=0)THEN

                  -- Generar valor de datos en archivo

                  LET v_s_qry = 
                      "SELECT FIRST 1 ",v_reg_detcNss.v_c_valor CLIPPED,""
                      ,"\n  FROM safre_tmp:tmp_detalle_op98 b"
                      ,"\n WHERE nss = ",v_ar_concNss[1].v_c_nss
                  --DISPLAY "final valor:",v_s_qry CLIPPED
                  PREPARE Prpr_ObtDatoaArch FROM v_s_qry CLIPPED
                  EXECUTE Prpr_ObtDatoaArch INTO v_reg_detcNss.v_c_valor
                  
               	  -- Acumula indice
               	  LET v_si_subIndice = v_si_subIndice  + 1
               	  LET v_ar_detRecha[v_si_subIndice].* = v_reg_detcNss.*
               	 
               
               END IF
               ---
               EXECUTE Prpr_ObtDatosDosRecha 
                 USING p_folio, v_ar_concNss[1].v_c_nss
                  INTO v_reg_detcNss.*

               IF NOT(v_reg_detcNss.v_d_rechazo IS NULL 
                  OR v_reg_detcNss.v_d_rechazo <=0)THEN
                  -- Generar valor de datos en archivo
  
                  LET v_s_qry = 
                      "SELECT FIRST 1 ",v_reg_detcNss.v_c_valor CLIPPED,""
                      ,"\n  FROM safre_tmp:tmp_detalle_op98 b"
                      ,"\n WHERE nss = ",v_ar_concNss[1].v_c_nss
                  PREPARE Prpr_ObtDato1aArch FROM v_s_qry CLIPPED
                  EXECUTE Prpr_ObtDato1aArch INTO v_reg_detcNss.v_c_valor
                  
               	  -- Acumula indice
               	  LET v_si_subIndice = v_si_subIndice  + 1
               	  LET v_ar_detRecha[v_si_subIndice].* = v_reg_detcNss.*

               END IF

               EXECUTE Prpr_ObtDatosTreRecha 
                 USING p_folio, v_ar_concNss[1].v_c_nss
                  INTO v_reg_detcNss.*

               IF NOT(v_reg_detcNss.v_d_rechazo IS NULL 
                  OR v_reg_detcNss.v_d_rechazo <=0)THEN
                  -- Generar valor de datos en archivo

                  LET v_s_qry = 
                      "SELECT FIRST 1 ",v_reg_detcNss.v_c_valor CLIPPED,""
                      ,"\n  FROM safre_tmp:tmp_detalle_op98 b"
                      ,"\n WHERE nss = ",v_ar_concNss[1].v_c_nss
                  PREPARE Prpr_ObtDato2aArch FROM v_s_qry CLIPPED
                  EXECUTE Prpr_ObtDato2aArch INTO v_reg_detcNss.v_c_valor
                  
               	  -- Acumula indice
               	  LET v_si_subIndice = v_si_subIndice  + 1
               	  LET v_ar_detRecha[v_si_subIndice].* = v_reg_detcNss.*
               END IF

            
            --ON ACTION ACCEPT 
            BEFORE ROW
               -- Cuando cambia de renglo se prepara para cargar datos detalle
               CALL v_ar_detRecha.CLEAR()
               LET v_si_subIndice = 0

               EXECUTE Prpr_ObtDatosUnoRecha 
                 USING p_folio, v_ar_concNss[ARR_CURR()].v_c_nss
                  INTO v_reg_detcNss.*

               IF NOT(v_reg_detcNss.v_d_rechazo IS NULL 
                  OR v_reg_detcNss.v_d_rechazo <=0)THEN

                  -- Generar valor de datos en archivo

                  LET v_s_qry = 
                      "SELECT FIRST 1 ",v_reg_detcNss.v_c_valor CLIPPED,""
                      ,"\n  FROM safre_tmp:tmp_detalle_op98 b"
                      ,"\n WHERE nss = ",v_ar_concNss[ARR_CURR()].v_c_nss
                  --DISPLAY "final valor:",v_s_qry CLIPPED
                  PREPARE Prpr_ObtDatoArch FROM v_s_qry CLIPPED
                  EXECUTE Prpr_ObtDatoArch INTO v_reg_detcNss.v_c_valor
                  
               	  -- Acumula indice
               	  LET v_si_subIndice = v_si_subIndice  + 1
               	  LET v_ar_detRecha[v_si_subIndice].* = v_reg_detcNss.*
               	 
               
               END IF
               ---
               EXECUTE Prpr_ObtDatosDosRecha 
                 USING p_folio, v_ar_concNss[ARR_CURR()].v_c_nss
                  INTO v_reg_detcNss.*

               IF NOT(v_reg_detcNss.v_d_rechazo IS NULL 
                  OR v_reg_detcNss.v_d_rechazo <=0)THEN
                  -- Generar valor de datos en archivo
  
                  LET v_s_qry = 
                      "SELECT FIRST 1 ",v_reg_detcNss.v_c_valor CLIPPED,""
                      ,"\n  FROM safre_tmp:tmp_detalle_op98 b"
                      ,"\n WHERE nss = ",v_ar_concNss[ARR_CURR()].v_c_nss
                  PREPARE Prpr_ObtDato1Arch FROM v_s_qry CLIPPED
                  EXECUTE Prpr_ObtDato1Arch INTO v_reg_detcNss.v_c_valor
                  
               	  -- Acumula indice
               	  LET v_si_subIndice = v_si_subIndice  + 1
               	  LET v_ar_detRecha[v_si_subIndice].* = v_reg_detcNss.*

               END IF

               EXECUTE Prpr_ObtDatosTreRecha 
                 USING p_folio, v_ar_concNss[ARR_CURR()].v_c_nss
                  INTO v_reg_detcNss.*

               IF NOT(v_reg_detcNss.v_d_rechazo IS NULL 
                  OR v_reg_detcNss.v_d_rechazo <=0)THEN
                  -- Generar valor de datos en archivo

                  LET v_s_qry = 
                      "SELECT FIRST 1 ",v_reg_detcNss.v_c_valor CLIPPED,""
                      ,"\n  FROM safre_tmp:tmp_detalle_op98 b"
                      ,"\n WHERE nss = ",v_ar_concNss[ARR_CURR()].v_c_nss
                  PREPARE Prpr_ObtDato2Arch FROM v_s_qry CLIPPED
                  EXECUTE Prpr_ObtDato2Arch INTO v_reg_detcNss.v_c_valor
                  
               	  -- Acumula indice
               	  LET v_si_subIndice = v_si_subIndice  + 1
               	  LET v_ar_detRecha[v_si_subIndice].* = v_reg_detcNss.*
               END IF
                    
         END DISPLAY
         -- Arreglo de rechazos de nss seleccionado
         DISPLAY ARRAY v_ar_detRecha TO tbl_rechazos.*
         
         END DISPLAY

         ON ACTION regresa
            EXIT DIALOG
      END DIALOG
      
      CLOSE WINDOW w_consulta_recha_detalle
   ELSE
      DISPLAY "No hay datos"
   END IF
   
END FUNCTION -- fn_muestra_rechazo_detalle_devol


{
======================================================================
Clave: DEOC04
Nombre: fn_muestra_rechazo_sumario_encab
Fecha creacion: Febrero 21, 2012
Autor: Felipe Nava
Narrativa del proceso que realiza:
 Muestra detalle de errores de tipo sumario y encabezado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_muestra_rechazo_sumario_encab(
         p_folio, p_dt_f_operacion_procesar_ini, 
         p_dt_f_operacion_procesar_fin)
DEFINE  p_folio                CHAR(10)
       ,p_dt_f_operacion_procesar_ini DATE -- fecha de carga inicial
       ,p_dt_f_operacion_procesar_fin DATE -- fecha de carga final
       --
       ,v_si_indice            SMALLINT -- indice de arreglo
       ,v_s_sql                STRING -- cadena con una instruccion SQL
       -- Arreglo de Rechazos
       ,v_ar_detRecha          DYNAMIC ARRAY OF RECORD
         v_d_rechazo           SMALLINT
        ,v_c_tabla             LIKE cat_layout.tabla
        ,v_c_decrip            CHAR(50)
        ,v_c_posicion          CHAR(50)
        ,v_c_valor             CHAR(50)
       END RECORD
       ,v_reg_detcNss          RECORD
         v_d_rechazo           SMALLINT
        ,v_c_tabla             LIKE cat_layout.tabla
        ,v_c_decrip            CHAR(50)
        ,v_c_posicion          CHAR(50)
        ,v_c_valor             CHAR(50)
       END RECORD
       ,v_s_qry                STRING   -- para consulta secundaria
       ,v_si_subIndice         SMALLINT
       ,v_s_clausula_fecha     STRING
       ,v_s_temCadena          STRING

   LET v_s_temCadena = " 1 = 1"
   -- si se enviaron las fechas de carga
   IF ( p_dt_f_operacion_procesar_ini IS NOT NULL 
   	    AND p_dt_f_operacion_procesar_fin IS NOT NULL ) THEN
      -- se obtienen los folios correspondientes a este rango de fechas

      CALL fn_obt_folios_x_fecha(p_dt_f_operacion_procesar_ini, p_dt_f_operacion_procesar_fin) 
      RETURNING v_s_clausula_fecha
      -- Agrega descripcion de folios de la consulta de fechas
      LET v_s_temCadena = v_s_sql CLIPPED
          ,"\n  ", v_s_clausula_fecha CLIPPED
   END IF
   
   -- Arma consulta para obtener los rechazos a nivel detalle de nss y folio
   LET v_s_qry =
       "SELECT b.cod_rechazo_1, y.tabla, r.des_corta"
       ,"\n      ,'['||l.pos_inicial||'-'||l.pos_final||']'"
       ,"\n      ,l.campo_desc"
       ,"\n FROM ((deo_cat_rechazo r LEFT OUTER JOIN cat_campo l"
       ,"\n   ON r.layout_cod = l.layout_cod"
       ,"\n  AND r.registro   = l.registro"
       ,"\n  AND r.campo_cod  = l.campo_cod) RIGHT OUTER JOIN deo_cza_op98_rch b"
       ,"\n   ON b.cod_rechazo_1 = r.cod_rechazo) LEFT OUTER JOIN cat_layout y"
       ,"\n   ON y.layout_cod = l.layout_cod"
       ,"\n  AND y.registro   = l.registro"
       ,"\nWHERE b.folio = ?"
       ,"\n  AND ",v_s_temCadena CLIPPED

   PREPARE Prpr_ObtREncbsUnoRec FROM v_s_qry CLIPPED
   
   LET v_s_qry =
        "SELECT b.cod_rechazo_2, y.tabla, r.des_corta"
       ,"\n      ,'['||l.pos_inicial||'-'||l.pos_final||']'"
       ,"\n      ,l.campo_desc"
       ,"\n FROM ((deo_cat_rechazo r LEFT OUTER JOIN cat_campo l"
       ,"\n   ON r.layout_cod = l.layout_cod"
       ,"\n  AND r.registro   = l.registro"
       ,"\n  AND r.campo_cod  = l.campo_cod) RIGHT OUTER JOIN deo_cza_op98_rch b"
       ,"\n   ON b.cod_rechazo_2 = r.cod_rechazo) LEFT OUTER JOIN cat_layout y"
       ,"\n   ON y.layout_cod = l.layout_cod"
       ,"\n  AND y.registro   = l.registro"
       ,"\nWHERE b.folio = ?"
       ,"\n  AND ",v_s_temCadena CLIPPED

   PREPARE Prpr_ObtREncbsDosRec FROM v_s_qry CLIPPED
   
   LET v_s_qry =
              "SELECT b.cod_rechazo_3, y.tabla, r.des_corta"
       ,"\n      ,'['||l.pos_inicial||'-'||l.pos_final||']'"
       ,"\n      ,l.campo_desc"
       ,"\n FROM ((deo_cat_rechazo r LEFT OUTER JOIN cat_campo l"
       ,"\n   ON r.layout_cod = l.layout_cod"
       ,"\n  AND r.registro   = l.registro"
       ,"\n  AND r.campo_cod  = l.campo_cod) RIGHT OUTER JOIN deo_cza_op98_rch b"
       ,"\n   ON b.cod_rechazo_3 = r.cod_rechazo) LEFT OUTER JOIN cat_layout y"
       ,"\n   ON y.layout_cod = l.layout_cod"
       ,"\n  AND y.registro   = l.registro"
       ,"\nWHERE b.folio = ?"
       ,"\n  AND ",v_s_temCadena CLIPPED
   PREPARE Prpr_ObtREncbsTreRec FROM v_s_qry CLIPPED

   -- Cargar datos detalle
   CALL v_ar_detRecha.CLEAR()
   LET v_si_subIndice = 0

   EXECUTE Prpr_ObtREncbsUnoRec 
     USING p_folio
      INTO v_reg_detcNss.*

   IF NOT(v_reg_detcNss.v_d_rechazo IS NULL 
      OR v_reg_detcNss.v_d_rechazo <=0)THEN

      -- Generar valor de datos en archivo

      LET v_s_qry = 
          "SELECT FIRST 1 ",v_reg_detcNss.v_c_valor CLIPPED,"\n"
          ,"FROM safre_tmp:",v_reg_detcNss.v_c_tabla CLIPPED
      DISPLAY "final valor:",v_s_qry CLIPPED
      PREPARE Prpr_ObtDato19Arch FROM v_s_qry CLIPPED
      EXECUTE Prpr_ObtDato19Arch INTO v_reg_detcNss.v_c_valor
      
      -- Acumula indice
      LET v_si_subIndice = v_si_subIndice  + 1
      LET v_ar_detRecha[v_si_subIndice].* = v_reg_detcNss.*
   END IF
   ---
   EXECUTE Prpr_ObtREncbsDosRec 
     USING p_folio
      INTO v_reg_detcNss.*

   IF NOT(v_reg_detcNss.v_d_rechazo IS NULL 
      OR v_reg_detcNss.v_d_rechazo <=0)THEN
      -- Generar valor de datos en archivo
  
      LET v_s_qry = 
          "SELECT FIRST 1 ",v_reg_detcNss.v_c_valor CLIPPED,""
          ,"\n  FROM safre_tmp:",v_reg_detcNss.v_c_tabla CLIPPED
      --DISPLAY "final valor:",v_s_qry CLIPPED
      PREPARE Prpr_ObtDato10Arch FROM v_s_qry CLIPPED
      EXECUTE Prpr_ObtDato10Arch INTO v_reg_detcNss.v_c_valor
      
   	  -- Acumula indice
   	  LET v_si_subIndice = v_si_subIndice  + 1
   	  LET v_ar_detRecha[v_si_subIndice].* = v_reg_detcNss.*

   END IF

   EXECUTE Prpr_ObtREncbsTreRec 
     USING p_folio
      INTO v_reg_detcNss.*

   IF NOT(v_reg_detcNss.v_d_rechazo IS NULL 
      OR v_reg_detcNss.v_d_rechazo <=0)THEN
      -- Generar valor de datos en archivo

      LET v_s_qry = 
          "SELECT FIRST 1 ",v_reg_detcNss.v_c_valor CLIPPED,""
          ,"\n  FROM safre_tmp:",v_reg_detcNss.v_c_tabla CLIPPED
      --DISPLAY "final valor:",v_s_qry CLIPPED
      PREPARE Prpr_ObtDato11Arch FROM v_s_qry CLIPPED
      EXECUTE Prpr_ObtDato11Arch INTO v_reg_detcNss.v_c_valor
      
   	  -- Acumula indice
   	  LET v_si_subIndice = v_si_subIndice  + 1
   	  LET v_ar_detRecha[v_si_subIndice].* = v_reg_detcNss.*
   END IF
   
   IF(v_si_subIndice > 0)THEN
      OPEN WINDOW win_enca_sumario WITH FORM "DEOC044"
      DIALOG 
         ATTRIBUTES(UNBUFFERED)
   
         DISPLAY ARRAY v_ar_detRecha TO tbl_rechazos.*

         END DISPLAY

         ON ACTION regresa
            EXIT DIALOG
      END DIALOG
      -- Arreglo de rechazos de nss seleccionado


      CLOSE WINDOW win_enca_sumario
   ELSE
      CALL fn_mensaje("Atención","No se encontraron los datos del error","stop")
   END IF
   
END FUNCTION -- fn_muestra_rechazo_sumario_encab
