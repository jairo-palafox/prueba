--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETC100                                                                #
#Objetivo     => Consulta de datos cargados de datamart                                 #
#Fecha inicio => Abril 12, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS


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
    
   -- consulta datos cargados de datamart (SPES)
   CALL fn_consulta_datos_cargados_datamart(p_usuario_cod)

END MAIN

{ ======================================================================
Clave: RETC100
Nombre: fn_consulta_datos_cargados_datamart
Fecha creacion: Abril 12, 2012
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Consulta los registros que fueron cargados en el datamart por carga
del SPES

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_datos_cargados_datamart(p_usuario_cod)
DEFINE p_usuario_cod            LIKE seg_usuario.usuario_cod, -- clave del usuario
       -- variables para la consulta principal
       v_nss                    LIKE afi_derechohabiente.nss,
       v_nombre_afore           LIKE ret_datamart.nombre_afore,
       v_paterno_afore          LIKE ret_datamart.paterno_afore,
       v_materno_afore          LIKE ret_datamart.materno_afore,
       v_folio                  LIKE glo_folio.folio,
       v_f_carga_datamart       DATE,
       v_f_carga_infonavit      DATE,
       v_nombre_archivo         LIKE glo_ctr_archivo.nombre_archivo,
       v_sec_pension            LIKE ret_datamart.sec_pension,
       v_regimen                LIKE ret_datamart.regimen,
       v_tpo_seguro             LIKE ret_datamart.tpo_seguro,
       v_tpo_pension            LIKE ret_datamart.tpo_pension,
       v_tpo_prestacion         LIKE ret_datamart.tpo_prestacion,
       v_f_inicio_pension       LIKE ret_datamart.f_inicio_pension,
       v_f_resolucion           LIKE ret_datamart.f_resolucion,
       v_diag_registro          LIKE ret_datamart.diag_registro, -- diagnostico
       v_cbx_regimen            ui.ComboBox, -- combo de regimenes
       v_cbx_tpo_seguro         ui.ComboBox, -- combo de tipo de seguro
       v_cbx_tpo_pension        ui.ComboBox, -- combo de tipo de pension
       v_cbx_tpo_prestacion     ui.ComboBox, -- combo de tipo de prestacion
       v_s_cadena               STRING, -- cadena de texto
       v_des_larga              LIKE ret_regimen.des_larga, -- descripcion del regimen
       v_si_indice              SMALLINT -- indice para arreglo

   OPEN WINDOW w_consulta_datamart WITH FORM "RETC4101"

   {-- se llena el combo de regimenes
   LET v_cbx_regimen = ui.ComboBox.forName("formonly.regimen")

   DECLARE cur_regimenes CURSOR FOR
   SELECT regimen, des_larga
   FROM ret_regimen
   ORDER BY regimen
   
   CALL v_cbx_regimen.clear()
   
   FOREACH cur_regimenes INTO v_regimen, v_des_larga
      LET v_s_cadena = v_regimen || " - " || v_des_larga CLIPPED
      CALL v_cbx_regimen.addItem(v_regimen, v_s_cadena)
   END FOREACH

   -- se llena el combo tipo de seguro
   LET v_cbx_tpo_seguro = ui.ComboBox.forName("formonly.tpo_seguro")

   DECLARE cur_tposeguros CURSOR FOR
   SELECT tpo_seguro, des_larga
   FROM ret_tipo_seguro
   ORDER BY tpo_seguro
   
   CALL v_cbx_tpo_seguro.clear()
   
   FOREACH cur_tposeguros INTO v_tpo_seguro, v_des_larga
      LET v_s_cadena = v_tpo_seguro || " - " || v_des_larga CLIPPED
      CALL v_cbx_tpo_seguro.addItem(v_tpo_seguro, v_s_cadena)
   END FOREACH
            
   -- se llena el combo tipo de pension
   LET v_cbx_tpo_pension = ui.ComboBox.forName("formonly.tpo_pension")

   DECLARE cur_tpopensiones CURSOR FOR
   SELECT tpo_pension, des_larga
   FROM  ret_tipo_pension
   ORDER BY tpo_pension
   
   CALL v_cbx_tpo_pension.clear()
   
   FOREACH cur_tpopensiones INTO v_tpo_pension, v_des_larga
      LET v_s_cadena = v_tpo_pension || " - " || v_des_larga CLIPPED
      CALL v_cbx_tpo_pension.addItem(v_tpo_pension, v_s_cadena)
   END FOREACH

   -- se llena el combo tipo de prestacion
   LET v_cbx_tpo_prestacion = ui.ComboBox.forName("formonly.tpo_prestacion")

   DECLARE cur_tpoprestacion CURSOR FOR
   SELECT tpo_prestacion, des_larga
   FROM  ret_tipo_prestacion
   ORDER BY tpo_prestacion
   
   CALL v_cbx_tpo_prestacion.clear()
   
   FOREACH cur_tpoprestacion INTO v_tpo_prestacion, v_des_larga
      LET v_s_cadena = v_tpo_prestacion || " - " || v_des_larga CLIPPED
      CALL v_cbx_tpo_prestacion.addItem(v_tpo_prestacion, v_s_cadena)
   END FOREACH}

   
   INPUT
      v_nss{                ,
      v_nombre_afore       ,
      v_paterno_afore      ,
      v_materno_afore      ,
      v_folio              ,
      v_f_carga_datamart   ,
      v_f_carga_infonavit  ,
      v_nombre_archivo     ,
      v_regimen            ,
      v_tpo_seguro         ,
      v_tpo_pension        ,
      v_tpo_prestacion     ,
      v_f_inicio_pension   ,
      v_f_resolucion       ,
      v_diag_registro}
   WITHOUT DEFAULTS
   FROM
      nss{                 ,
      nombre_afore        ,
      paterno_afore       ,
      materno_afore       ,
      folio               ,
      f_carga_datamart    ,
      f_carga_infonavit   ,
      nombre_archivo      ,
      regimen             ,
      tpo_seguro          ,
      tpo_pension         ,
      tpo_prestacion      ,
      f_inicio_pension    ,
      f_resolucion        ,
      diag_registro}
   ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE)
  
      BEFORE INPUT
         -- se asignan los valores por omision               
         LET v_nss               = NULL
         LET v_nombre_afore      = NULL
         LET v_paterno_afore     = NULL
         LET v_materno_afore     = NULL
         LET v_folio             = NULL
         LET v_f_carga_datamart  = NULL
         LET v_f_carga_infonavit = NULL
         LET v_nombre_archivo    = NULL
         LET v_regimen           = NULL
         LET v_tpo_seguro        = NULL
         LET v_tpo_pension       = NULL
         LET v_tpo_prestacion    = NULL
         LET v_f_inicio_pension  = NULL
         LET v_f_resolucion      = NULL
         LET v_diag_registro     = NULL
                
      ON ACTION consultar
         -- se debe tener al menos un criterio de busqueda
         IF ( (v_nss               IS NULL) {AND
              (v_nombre_afore      IS NULL) AND
              (v_paterno_afore     IS NULL) AND
              (v_materno_afore     IS NULL) AND
              (v_folio             IS NULL) AND
              (v_f_carga_datamart  IS NULL) AND
              (v_f_carga_infonavit IS NULL) AND
              (v_nombre_archivo    IS NULL) AND
              (v_regimen           IS NULL) AND
              (v_tpo_seguro        IS NULL) AND
              (v_tpo_pension       IS NULL) AND
              (v_tpo_prestacion    IS NULL) AND
              (v_f_inicio_pension  IS NULL) AND
              (v_f_resolucion      IS NULL) AND
              (v_diag_registro     IS NULL) }
              ) THEN
            -- se indica al usuario que no puede realizar la consulta asi
            CALL fn_mensaje("Atención","Debe capturar el NSS","bn_about")
            CONTINUE INPUT
         END IF
        
         CALL fn_muestra_consulta_rechazo_detalle(v_nss              ,
                                                  v_nombre_afore     ,
                                                  v_paterno_afore    ,
                                                  v_materno_afore    ,
                                                  v_folio            ,
                                                  v_f_carga_datamart ,
                                                  v_f_carga_infonavit,
                                                  v_nombre_archivo   ,
                                                  v_regimen          ,
                                                  v_tpo_seguro       ,
                                                  v_tpo_pension      ,
                                                  v_tpo_prestacion   ,
                                                  v_f_inicio_pension ,
                                                  v_f_resolucion     ,
                                                  v_diag_registro    )

      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_datamart

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_muestra_consulta_rechazo_detalle
Fecha creacion: Abril 12, 2012
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Consulta el detalle de rechazos de retiros por disposicion de recursos

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_muestra_consulta_rechazo_detalle(p_nss              ,
                                             p_nombre_afore     ,
                                             p_paterno_afore    ,
                                             p_materno_afore    ,
                                             p_folio            ,
                                             p_f_carga_datamart ,
                                             p_f_carga_infonavit,
                                             p_nombre_archivo   ,
                                             p_regimen          ,
                                             p_tpo_seguro       ,
                                             p_tpo_pension      ,
                                             p_tpo_prestacion   ,
                                             p_f_inicio_pension ,
                                             p_f_resolucion     ,
                                             p_diag_registro    )
DEFINE 
       p_nss                    LIKE afi_derechohabiente.nss,
       p_nombre_afore           LIKE ret_datamart.nombre_afore,
       p_paterno_afore          LIKE ret_datamart.paterno_afore,
       p_materno_afore          LIKE ret_datamart.materno_afore,
       p_folio                  LIKE glo_folio.folio,
       p_f_carga_datamart       DATE,
       p_f_carga_infonavit      DATE,
       p_nombre_archivo         LIKE glo_ctr_archivo.nombre_archivo,
       p_sec_pension            LIKE ret_datamart.sec_pension,
       p_regimen                LIKE ret_datamart.regimen,
       p_tpo_seguro             LIKE ret_datamart.tpo_seguro,
       p_tpo_pension            LIKE ret_datamart.tpo_pension,
       p_tpo_prestacion         LIKE ret_datamart.tpo_prestacion,
       p_f_inicio_pension       LIKE ret_datamart.f_inicio_pension,
       p_f_resolucion           LIKE ret_datamart.f_resolucion,
       p_diag_registro          LIKE ret_datamart.diag_registro,
       -- registro de consulta
       r_consulta               RECORD
         nss                      LIKE afi_derechohabiente.nss,
         nombre_afore             LIKE ret_datamart.nombre_afore,
         folio                    LIKE glo_folio.folio,
         diagnostico              LIKE ret_datamart.diag_registro,
         sec_pension              LIKE ret_datamart.sec_pension,
         regimen                  LIKE ret_datamart.regimen,
         tpo_seguro               LIKE ret_datamart.tpo_seguro,
         tpo_pension              LIKE ret_datamart.tpo_pension,
         tpo_prestacion           LIKE ret_datamart.tpo_prestacion,
         f_inicio_pension         LIKE ret_datamart.f_inicio_pension,
         f_resolucion             LIKE ret_datamart.f_resolucion,
         f_carga                  DATE,
         id_datamart              LIKE ret_datamart.id_datamart,
         --id_derechohabiente       LIKE afi_derechohabiente.id_derechohabiente,
         paterno_afore            LIKE ret_datamart.paterno_afore,
         materno_afore            LIKE ret_datamart.materno_afore
       END RECORD,
       -- arreglo de despliegue
       v_arreglo_consulta       DYNAMIC ARRAY OF RECORD
         nss                      LIKE afi_derechohabiente.nss,
         nombre_afore             LIKE ret_datamart.nombre_afore,
         folio                    LIKE glo_folio.folio,
         diagnostico              LIKE ret_datamart.diag_registro,
         sec_pension              LIKE ret_datamart.sec_pension,
         regimen                  LIKE ret_datamart.regimen,
         tpo_seguro               LIKE ret_datamart.tpo_seguro,
         tpo_pension              LIKE ret_datamart.tpo_pension,
         tpo_prestacion           LIKE ret_datamart.tpo_prestacion,
         f_inicio_pension         LIKE ret_datamart.f_inicio_pension,
         f_resolucion             LIKE ret_datamart.f_resolucion,
         f_carga                  DATE,
         id_datamart              LIKE ret_datamart.id_datamart,
         --id_derechohabiente       LIKE afi_derechohabiente.id_derechohabiente,
         paterno_afore            LIKE ret_datamart.paterno_afore,
         materno_afore            LIKE ret_datamart.materno_afore
       END RECORD,

       v_si_indice             SMALLINT , -- indice de arreglo
       v_sql                   STRING   , -- cadena con una instruccion SQL
       v_s_clausula_fecha      STRING   , -- clausula para filtro por fechas
       v_s_claves_afores       STRING   , -- Lista de claves de afore para consulta
       v_i_cont                INTEGER    -- Contador general
  
   OPEN WINDOW w_carga_datamart WITH FORM "RETC4102"
   
   -- se prepara la consulta
   LET v_sql = "SELECT               \n",
               "b.nss               ,\n", -- el id_derechohabiente
               "b.nombre_afore      ,\n",
               "b.folio             ,\n",
               "b.diag_registro     ,\n",  
               "b.sec_pension       ,\n",
               "b.regimen           ,\n",
               "b.tpo_seguro        ,\n",
               "b.tpo_pension       ,\n",
               "b.tpo_prestacion    ,\n",
               "b.f_inicio_pension  ,\n",
               "b.f_resolucion      ,\n",
               "c.f_carga_infonavit ,\n", -- fecha carga, del encabezado
               "b.id_datamart       ,\n", -- el id_derechohabiente
               --"a.id_derechohabiente,\n", -- el id_derechohabiente
               "b.paterno_afore     ,\n",
               "b.materno_afore      \n",
               --"FROM ret_datamart b, afi_derechohabiente a, ret_cza_datamart c\n",
               "FROM ret_datamart b, ret_cza_datamart c\n",
               "WHERE\n",
               "b.folio = c.folio\n"
   
   -- se verifican las clausulas de la consulta
   -- se dio NSS
   IF ( p_nss IS NOT NULL ) THEN
      LET v_sql = v_sql || "AND\n"
      LET v_sql = v_sql || "b.nss = '" || p_nss || "'\n"
   END IF
   
   {-- se dio folio
   IF ( p_folio IS NOT NULL ) THEN
      LET v_sql = v_sql || "AND\n"
      LET v_sql = v_sql || "b.folio = " || p_folio || "\n"
   END IF

   -- se envio fecha de carga de infonavit
   IF ( p_f_carga_infonavit IS NOT NULL ) THEN
      LET v_sql = v_sql || "AND\n"
      LET v_sql = v_sql || "c.f_carga_infonavit = '" || DATE(p_f_carga_infonavit) || "'\n"
   END IF

   -- se envio fecha de carga del datamart
   IF ( p_f_carga_datamart IS NOT NULL ) THEN
      LET v_sql = v_sql || "AND\n"
      LET v_sql = v_sql || "c.f_carga_datamart = '" || DATE(p_f_carga_datamart) || "'\n"
   END IF

   -- se envio nombre de archivo
   IF ( p_nombre_archivo IS NOT NULL ) THEN
      LET v_sql = v_sql || "AND\n"
      LET v_sql = v_sql || "c.nombre_archivo = '" || p_nombre_archivo || "'\n"
   END IF

   -- se envio regimen
   IF ( p_regimen IS NOT NULL ) THEN
      LET v_sql = v_sql || "AND\n"
      LET v_sql = v_sql || "b.regimen = " || p_regimen || "\n"
   END IF

   -- se envio tipo de seguro
   IF ( p_tpo_seguro IS NOT NULL ) THEN
      LET v_sql = v_sql || "AND\n"
      LET v_sql = v_sql || "b.tpo_seguro = '" || p_tpo_seguro || "'\n"
   END IF

   -- se envio tipo de pension
   IF ( p_tpo_pension IS NOT NULL ) THEN
      LET v_sql = v_sql || "AND\n"
      LET v_sql = v_sql || "b.tpo_pension = '" || p_tpo_pension || "'\n"
   END IF

   -- se envio tipo de prestacion
   IF ( p_tpo_prestacion IS NOT NULL ) THEN
      LET v_sql = v_sql || "AND\n"
      LET v_sql = v_sql || "b.tpo_prestacion = '" || p_tpo_prestacion || "'\n"
   END IF

   -- se envion fecha de inicio de pension   
   IF ( p_f_inicio_pension IS NOT NULL ) THEN
      LET v_sql = v_sql || "AND\n"
      LET v_sql = v_sql || "b.f_inicio_pension = '" || DATE(p_f_inicio_pension) || "'\n"
   END IF

   -- se envio fecha de resolucion
   IF ( p_f_resolucion IS NOT NULL ) THEN
      LET v_sql = v_sql || "AND\n"
      LET v_sql = v_sql || "b.f_resolucion = '" || DATE(p_f_resolucion) || "'\n"
   END IF

   -- si se tienen los datos del nombre
   -- Nombre de pila
   IF ( p_nombre_afore IS NOT NULL ) THEN
      LET v_sql = v_sql || "AND\n"
      LET v_sql = v_sql || "b.nombre_afore = '" || p_nombre_afore CLIPPED || "'\n"
   END IF
   
   -- apellido paterno
   IF ( p_paterno_afore IS NOT NULL ) THEN
      LET v_sql = v_sql || "AND\n"
      LET v_sql = v_sql || "b.paterno_afore = '" || p_paterno_afore CLIPPED || "'\n"
   END IF

   -- apellido materno
   IF ( p_materno_afore IS NOT NULL ) THEN
      LET v_sql = v_sql || "AND\n"
      LET v_sql = v_sql || "b.materno_afore = '" || p_materno_afore CLIPPED || "'\n"
   END IF   

   -- diagnostico del registro
   IF ( p_diag_registro IS NOT NULL ) THEN
      LET v_sql = v_sql || "AND\n"
      LET v_sql = v_sql || "b.diag_registro = '" || p_diag_registro CLIPPED || "'\n"
   END IF}
   
   DISPLAY v_sql
   
   -- se ejecuta la consulta
   PREPARE sid_datamart FROM v_sql
   DECLARE cur_datamart CURSOR FOR sid_datamart
   
   LET v_si_indice = 1
  
   FOREACH cur_datamart INTO r_consulta.*
      -- se transfieren los datos al arreglo de despliegue
      LET v_arreglo_consulta[v_si_indice].* = r_consulta.*
      
      LET v_si_indice = v_si_indice + 1

      -- si ya se llego al 501, se le indica al usuario que son muchos registros
      IF ( v_si_indice > 500 ) THEN
         CALL fn_mensaje("Atención", "La consulta es muy extensa; sólo se mostrarán los primeros 500 registros.\nAgregue más parámetros para reducir el número de resultados","exclamation")
         EXIT FOREACH
      END IF
      
   END FOREACH
   
   
   DISPLAY ARRAY v_arreglo_consulta TO tbl_datamart.*
   ATTRIBUTES ( UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
   
      ON ACTION detalle
      
        -- se obtienen los datos del registro elegido
        LET v_si_indice = ARR_CURR()
        
        LET r_consulta.* = v_arreglo_consulta[v_si_indice].*
      
        CALL f_consulta_detalle_carga_datamart(r_consulta.*)
      ON ACTION regresar
         EXIT DISPLAY
   END DISPLAY
      
   CLOSE WINDOW w_carga_datamart

END FUNCTION -- fn_muestra_consulta_recha_detalle

{
======================================================================
Clave: DEOC04
Nombre: f_consulta_detalle_carga_datamart
Fecha creacion: Diciembre 28, 2011
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION f_consulta_detalle_carga_datamart(p_nss               ,
                                           p_nombre_afore      ,
                                           p_folio             ,
                                           p_diagnostico       ,
                                           p_sec_pension       ,
                                           p_regimen           ,
                                           p_tpo_seguro        ,
                                           p_tpo_pension       ,
                                           p_tpo_prestacion    ,
                                           p_f_inicio_pension  ,
                                           p_f_resolucion      ,
                                           p_f_carga           ,
                                           p_id_datamart       ,
                                           --p_id_derechohabiente,
                                           p_paterno_afore     ,
                                           p_materno_afore     )
DEFINE 
       p_id_datamart            LIKE ret_datamart.id_datamart,
       p_id_derechohabiente     LIKE afi_derechohabiente.id_derechohabiente,
       p_paterno_afore          LIKE ret_datamart.paterno_afore,
       p_materno_afore          LIKE ret_datamart.materno_afore,
       p_nss                    LIKE afi_derechohabiente.nss,
       p_nombre_afore           LIKE ret_datamart.nombre_afore,
       p_folio                  LIKE glo_folio.folio,
       p_diagnostico            LIKE ret_datamart.diag_registro,
       p_f_carga_infonavit      DATE,
       p_nombre_archivo         LIKE glo_ctr_archivo.nombre_archivo,
       p_sec_pension            LIKE ret_datamart.sec_pension,
       p_regimen                LIKE ret_datamart.regimen,
       p_tpo_seguro             LIKE ret_datamart.tpo_seguro,
       p_tpo_pension            LIKE ret_datamart.tpo_pension,
       p_tpo_prestacion         LIKE ret_datamart.tpo_prestacion,
       p_f_inicio_pension       LIKE ret_datamart.f_inicio_pension,
       p_f_resolucion           LIKE ret_datamart.f_resolucion,
       p_f_carga                DATE,
       p_s_claves_afores         STRING -- codigo de afore
       ,p_dt_fecha_carga_inicial DATE -- fecha de carga inicial
       ,p_dt_fecha_carga_final   DATE -- fecha de carga final

       ,v_si_indice              SMALLINT -- indice de arreglo
       ,v_s_sql                  STRING -- cadena con una instruccion SQL
       ,v_s_clausula_fecha       STRING -- ruta del formulario
       --
       ,v_afoAnt                 LIKE cat_afore.afore_cod -- clave anterior
       ,v_afore_desc             LIKE cat_afore.afore_desc
       ,v_i_cont_registro        INTEGER
       ,v_i_cont_no              INTEGER
       ,r_ret_datamart           RECORD LIKE ret_datamart.* -- registro de datamart
       ,r_ret_cza_datamart       RECORD LIKE ret_cza_datamart.* -- registro de encabezado de datamart
       ,v_nombre_afore           STRING -- nombre concatenado
       ,v_regimen_des_larga      LIKE ret_regimen.des_larga
       ,v_seguro_des_larga       LIKE ret_regimen.des_larga
       ,v_pension_des_larga      LIKE ret_regimen.des_larga
       ,v_prestacion_des_larga   LIKE ret_regimen.des_larga
       
   -- se abre la ventana de consulta de detalle
   OPEN WINDOW w_detalle_datamart WITH FORM "RETC4103"

   -- se consulta el registro del datamart
   SELECT 
     id_datamart          , -- decimal(9,0) not null ,
     nss                  , -- decimal(9,0) not null ,
     sec_pension          , -- smallint not null ,
     regimen              , -- smallint not null ,
     tpo_seguro           , -- char(2) not null ,
     tpo_pension          , -- char(2) not null ,
     tpo_prestacion       , -- char(2) not null ,
     diag_registro        , -- char(3) not null ,
     folio                , -- decimal(9,0),
     curp                 , -- char(18),
     nombre_datamart      , -- char(40),
     nombre_afore         , -- char(40),
     paterno_afore        , -- char(40),
     materno_afore        , -- char(40),
     tpo_movimiento       , -- char(3),
     articulo_negativa    , -- char(3),
     fraccion_negativa    , -- char(2),
     num_considerando     , -- char(2),
     f_inicio_pension     , -- date,
     f_resolucion         , -- date,
     porcentaje_valuacion , -- decimal(5,2),
     semanas_cotizadas    , -- integer,
     estado_sub_viv       , -- smallint,
     aivs_viv97           , -- decimal(14,6),
     aivs_viv92           , -- decimal(14,6),
     importe_viv72        , -- decimal(14,2),
     cve_afore              -- smallint
   INTO 
     r_ret_datamart.id_datamart          , -- decimal(9,0) not null ,
     r_ret_datamart.nss                  , -- decimal(9,0) not null ,
     r_ret_datamart.sec_pension          , -- smallint not null ,
     r_ret_datamart.regimen              , -- smallint not null ,
     r_ret_datamart.tpo_seguro           , -- char(2) not null ,
     r_ret_datamart.tpo_pension          , -- char(2) not null ,
     r_ret_datamart.tpo_prestacion       , -- char(2) not null ,
     r_ret_datamart.diag_registro        , -- char(3) not null ,
     r_ret_datamart.folio                , -- decimal(9,0),
     r_ret_datamart.curp                 , -- char(18),
     r_ret_datamart.nombre_datamart      , -- char(40),
     r_ret_datamart.nombre_afore         , -- char(40),
     r_ret_datamart.paterno_afore        , -- char(40),
     r_ret_datamart.materno_afore        , -- char(40),
     r_ret_datamart.tpo_movimiento       , -- char(3),
     r_ret_datamart.articulo_negativa    , -- char(3),
     r_ret_datamart.fraccion_negativa    , -- char(2),
     r_ret_datamart.num_considerando     , -- char(2),
     r_ret_datamart.f_inicio_pension     , -- date,
     r_ret_datamart.f_resolucion         , -- date,
     r_ret_datamart.porcentaje_valuacion , -- decimal(5,2),
     r_ret_datamart.semanas_cotizadas    , -- integer,
     r_ret_datamart.estado_sub_viv       , -- smallint,
     r_ret_datamart.aivs_viv97           , -- decimal(14,6),
     r_ret_datamart.aivs_viv92           , -- decimal(14,6),
     r_ret_datamart.importe_viv72        , -- decimal(14,2),
     r_ret_datamart.cve_afore              -- smallint     
   FROM ret_datamart
   WHERE
     id_datamart = p_id_datamart
   
   -- se obtienen los datos del encabezado de carga de datamart
   SELECT
      folio            ,
      nombre_archivo   ,
      f_carga_datamart ,
      f_carga_infonavit,
      h_carga_infonavit,
      total_registros  ,
      usuario          
   INTO
      r_ret_cza_datamart.folio            ,
      r_ret_cza_datamart.nombre_archivo   ,
      r_ret_cza_datamart.f_carga_datamart ,
      r_ret_cza_datamart.f_carga_infonavit,
      r_ret_cza_datamart.h_carga_infonavit,
      r_ret_cza_datamart.total_registros  ,
      r_ret_cza_datamart.usuario          
   FROM
     ret_cza_datamart
   WHERE
     folio = p_folio
   
   DISPLAY "Nombre de archivo: ", r_ret_cza_datamart.nombre_archivo
   
   -- se concatena el nombre del trabajador
   LET v_nombre_afore = r_ret_datamart.paterno_afore CLIPPED, " ", r_ret_datamart.materno_afore CLIPPED, " ", r_ret_datamart.nombre_afore CLIPPED

   -- se obtienen las descripciones del seguro
   SELECT des_larga
   INTO v_regimen_des_larga
   FROM ret_regimen
   WHERE regimen = p_regimen
   
   SELECT des_larga 
   INTO v_seguro_des_larga
   FROM ret_tipo_seguro
   WHERE tpo_seguro = p_tpo_seguro
      
   SELECT des_larga 
   INTO v_pension_des_larga
   FROM ret_tipo_pension
   WHERE tpo_pension = p_tpo_pension
    
   SELECT des_larga
   INTO v_prestacion_des_larga
   FROM ret_tipo_prestacion
   WHERE tpo_prestacion = p_tpo_prestacion


   -- se despliegan los datos del datamart
   DISPLAY BY NAME
     r_ret_datamart.id_datamart          , -- decimal(9,0) not null ,
     r_ret_datamart.nss                  , -- decimal(9,0) not null ,
     r_ret_datamart.sec_pension          , -- smallint not null ,
     r_ret_datamart.regimen              , -- smallint not null ,
     r_ret_datamart.tpo_seguro           , -- char(2) not null ,
     r_ret_datamart.tpo_pension          , -- char(2) not null ,
     r_ret_datamart.tpo_prestacion       , -- char(2) not null ,
     r_ret_datamart.diag_registro        , -- char(3) not null ,
     r_ret_datamart.folio                , -- decimal(9,0),
     r_ret_datamart.curp                 , -- char(18),
     r_ret_datamart.nombre_datamart      , -- char(40),
     v_nombre_afore                      ,
     r_ret_datamart.tpo_movimiento       , -- char(3),
     r_ret_datamart.articulo_negativa    , -- char(3),
     r_ret_datamart.fraccion_negativa    , -- char(2),
     r_ret_datamart.num_considerando     , -- char(2),
     r_ret_datamart.f_inicio_pension     , -- date,
     r_ret_datamart.f_resolucion         , -- date,
     r_ret_datamart.porcentaje_valuacion , -- decimal(5,2),
     r_ret_datamart.semanas_cotizadas    , -- integer,
     --r_ret_datamart.estado_sub_viv       , -- smallint,
     r_ret_datamart.aivs_viv97           , -- decimal(14,6),
     r_ret_datamart.aivs_viv92           , -- decimal(14,6),
     r_ret_datamart.importe_viv72        , -- decimal(14,2),
     r_ret_datamart.cve_afore            ,  -- smallint     
     v_regimen_des_larga                 ,
     v_seguro_des_larga                  ,
     v_pension_des_larga                 ,
     v_prestacion_des_larga              

   -- se despliegan los datos de carga del datamart
   DISPLAY 
      p_nss                               ,
      r_ret_cza_datamart.nombre_archivo   ,
      r_ret_cza_datamart.f_carga_datamart ,
      r_ret_cza_datamart.f_carga_infonavit
   TO
      nss              ,
      nombre_archivo   ,
      f_carga_datamart ,
      f_carga_infonavit

   -- se actualiza la interfaz grafica
   CALL ui.interface.refresh()

   -- se genera un menu para mantener los datos en pantalla
   MENU
   
      COMMAND "Regresar"
         EXIT MENU
   
   END MENU
   
   -- se cierra la ventana
   CLOSE WINDOW w_detalle_datamart
  
END FUNCTION