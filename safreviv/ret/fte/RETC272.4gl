--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#MODULO       => RET                                                                    #
#PROGRAMA     => RETC272                                                                #
#OBJETIVO     => Consulta de control de solicitudes                                     #
#Fecha inicio => 11 abril 2014                                                       #
#Modificacion =>                                                                        #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS 
DEFINE g_arr_solicitudes_folio DYNAMIC ARRAY OF RECORD -- arreglo que contiene los folios elegidos para consultar
          --elegir             SMALLINT,
          id_solicitud       DECIMAL(9,0),
          id_derechohabiente DECIMAL(9,0),
          nss                CHAR(11),
          rfc                CHAR(13),
          modalidad_retiro   smallint, 
          caso_adai          CHAR(10),
          estado_solicitud   smallint,
          f_solicitud        DATE,
          f_consultada       DATE,
          f_caducidad        DATE,
          total_dias         SMALLINT          
       END RECORD,
       p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion    SMALLINT, -- forma como ejecutara el programa
       p_s_titulo          STRING   -- titulo de la ventana 
    
END GLOBALS 

{
======================================================================
Clave: 
Nombre: main
Fecha creacion:  10, 2014
Autor: Jairo Palafox, EFP
Narrativa del proceso que realiza:
Abre la ventana de captura de datos para realizar la consulta de retiro
por disposicion de recursos

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
MAIN 
DEFINE cbx_modalidad_retiro ui.ComboBox, -- combo de modalidad de retiro
       cbx_estado_solicitud ui.ComboBox, -- combo de estado de la solicitud
       cbx_cod_rechazo      ui.ComboBox, -- combo con los codigos de rechazo
       cb_modificar         SMALLINT,    -- verifica si se requiere los datos de la captura para la tabla dias_max       
       ar_ret_tipo_retiro      RECORD LIKE ret_tipo_retiro.*, -- registro con los tipos de retiro
       ar_ret_rechazo          RECORD LIKE ret_rechazo.*, -- registro con los codigos de rechazo
       -- parametros de consulta
       v_tipo_retiro       LIKE ret_tipo_retiro.tpo_retiro,
       v_etapa             SMALLINT, -- 1 Solicitud, 2 Preliquidacion, 3 Liquidacion
       v_nss               LIKE afi_derechohabiente.nss, 
       v_rfc               LIKE afi_derechohabiente.rfc,
       v_id_solicitud      LIKE ret_solicitud_generico.id_solicitud,
       v_folio             LIKE glo_folio.folio,
       v_folio_restitucion LIKE ret_solicitud_generico.folio_restitucion,
       v_estado            LIKE ret_estado_solicitud.estado_solicitud,
       v_cod_rechazo       SMALLINT, -- codigo de rechazo
       v_fecha_inicio      DATE, -- fecha de inicio de consulta
       v_fecha_fin         DATE, -- fecha fin de consulta
       v_cadena            STRING, -- cadena para concatenar
       v_modalidad_retiro  LIKE ret_modalidad_retiro.modalidad_retiro, -- modalidad de retiro
       v_r_ret_modalidad_retiro RECORD LIKE ret_modalidad_retiro.*, -- registro de modalidad de retiro
       v_caso_adai         LIKE ret_solicitud_generico.caso_adai, -- caso ADAI de la solicitud
       v_formulario        ui.Form, -- para modificar el formulario
       ar_ret_estado_solicitud RECORD LIKE ret_estado_solicitud.*
 DEFINE arr_estado_solicitud DYNAMIC ARRAY OF RECORD
         estado_solicitud     SMALLINT,
         max_dias             SMALLINT 
        END RECORD
 DEFINE lr_ret_generico_max_dias RECORD LIKE ret_generico_max_dias.*,       
        lb_ocultar          SMALLINT,
        ls_sql              STRING,
        li_indice           INTEGER,
        lb_consulta         SMALLINT
   -- se obtienen los parametros de ejecucion
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN
   -- se almancenan los datos recibidos en la tabla tempora
   --DROP TABLE tmp_dias_maximos
   CREATE  TEMP TABLE tmp_dias_maximos (estado_solicitud smallint,
                                                   max_dias SMALLINT )      

   -- se abre la ventana de consulta
   OPEN WINDOW w_consulta WITH FORM "RETC2721"

   DIALOG ATTRIBUTE (UNBUFFERED)   
   -- se capturan los datos de la consulta
   INPUT BY NAME
      v_modalidad_retiro ,
      v_nss              ,
      v_rfc              ,
      v_caso_adai        ,
      v_id_solicitud     ,
      v_folio            ,
      v_folio_restitucion,
      v_estado           ,
      v_cod_rechazo      ,
      v_fecha_inicio     ,
      v_fecha_fin        ,
      cb_modificar        
   
   
   
      BEFORE INPUT
         -- se obtiene control del formulario
         --LET v_formulario = DIALOG.getForm()

         -- se llenan los combos
         LET cbx_modalidad_retiro = ui.ComboBox.forName("formonly.v_modalidad_retiro")
         
         CALL cbx_modalidad_retiro.clear()
         
         DECLARE  cur_modalidad CURSOR FOR
         SELECT   *
         FROM     ret_modalidad_retiro
         WHERE    modalidad_retiro IN ( 2,3,10,9) -- fondo de ahorro, ley73, amort excedentes, aport voluntarias
         ORDER BY modalidad_retiro
   
         -- se agrega el valor nulo
         CALL cbx_modalidad_retiro.addItem(NULL, "Todos")
         
         -- para cada modalidad
         FOREACH cur_modalidad INTO v_r_ret_modalidad_retiro.*
            -- se agregan las modalidades
            LET v_cadena = v_r_ret_modalidad_retiro.modalidad_retiro || " - " || v_r_ret_modalidad_retiro.des_corta
            CALL cbx_modalidad_retiro.addItem(v_r_ret_modalidad_retiro.modalidad_retiro, v_cadena)
         END FOREACH
           
         -- se inicia con todas las modalidades
         LET v_modalidad_retiro = NULL
                
         -- no se tiene folio
         LET v_folio = NULL
         
         -- se llena el combo con los estados de la solicitud
         LET cbx_estado_solicitud = ui.ComboBox.forName("formonly.v_estado")
         
         CALL cbx_estado_solicitud.clear()
         -- se usan 3 conjuntos
         CALL cbx_estado_solicitud.addItem(NULL,"Todas")

         
         DECLARE cur_estadossol CURSOR FOR
         SELECT  *
         FROM    ret_estado_solicitud
         ORDER BY estado_solicitud
         
         FOREACH cur_estadossol INTO ar_ret_estado_solicitud.*
            LET v_cadena = ar_ret_estado_solicitud.estado_solicitud, " - ", ar_ret_estado_solicitud.des_corta
            CALL cbx_estado_solicitud.addItem(ar_ret_estado_solicitud.estado_solicitud, v_cadena)
         END FOREACH

{
         CALL cbx_estado_solicitud.addItem(2,"Aceptadas")
         CALL cbx_estado_solicitud.addItem(3,"Rechazadas")
}
         -- se asume que se desean todas
         LET v_estado = NULL
         
         -- se carga el combo de codigos de rechazo
         LET cbx_cod_rechazo = ui.ComboBox.forName("formonly.v_cod_rechazo")
         
         CALL cbx_cod_rechazo.clear()
         -- se agrega la agrupacion todas
         CALL cbx_cod_rechazo.addItem(NULL, "Todas")

         -- se agregan los codigos de rechazo acordados
         DECLARE cur_codrechazo CURSOR FOR
         SELECT *
         FROM   ret_rechazo
         WHERE  cod_rechazo IN 
                (99 , 98 , 999, 101, 77 , 97 , 20 , 10 , 50 , 40 , 90 ,
                 91 , 100, 200, 218, 102, 103, 104, 105, 300, 400, 500,
                 600, 650, 651, 54)
         ORDER BY cod_rechazo
         
         FOREACH cur_codrechazo INTO ar_ret_rechazo.*
            LET v_cadena = ar_ret_rechazo.cod_rechazo, " - ", ar_ret_rechazo.des_corta
            CALL cbx_cod_rechazo.addItem(ar_ret_rechazo.cod_rechazo, v_cadena)
         END FOREACH
         
         -- se inicia la consulta sin filtro de codigo de rechazo
         LET v_cod_rechazo = NULL

         -- la fecha de inicio y fecha fin se inician con la fecha del dia
         LET v_fecha_inicio = TODAY
         LET v_fecha_fin    = TODAY
                  
                  
      AFTER FIELD v_fecha_fin
         NEXT FIELD v_modalidad_retiro
         
      -- se asume que no se pide modificar el estado de la solicitud
      LET lb_consulta = FALSE
      -- se asume que no se pide modificar el catalogo
      --LET cb_modificar = FALSE
      
      ON CHANGE  cb_modificar 
         INPUT ARRAY arr_estado_solicitud FROM tbl_estado_solicitud.* ATTRIBUTES(CANCEL = false, ACCEPT = FALSE, INSERT ROW=FALSE, DELETE ROW=FALSE,WITHOUT DEFAULTS=TRUE, UNBUFFERED )
           BEFORE INPUT 
             -- esconder el boton appende en la forma
             CALL DIALOG.setActionHidden("append",true)
             AFTER FIELD estado_solicitud    
             -- se imprimen los datos a guardar
            { DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[1].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[2].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[3].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[4].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[5].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[6].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[7].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[8].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[9].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[10].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[11].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[12].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[13].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[14].*             
              }    
           ON ACTION aceptar  

              -- se imprimen los datos a guardar
             {DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[1].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[2].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[3].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[4].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[5].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[6].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[7].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[8].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[9].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[10].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[11].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[12].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[13].*
             DISPLAY "arr_estado_solicitud.*",arr_estado_solicitud[14].*             
            
            }
             LET lb_consulta = TRUE
             -- se guardan los datos capturados en la tabla
             FOR li_indice = 1 TO arr_estado_solicitud.getLength()
               DISPLAY "arr_estado_solicitud",arr_estado_solicitud[li_indice].*
               DELETE FROM tmp_dias_maximos
               WHERE estado_solicitud = arr_estado_solicitud[li_indice].estado_solicitud
               
               INSERT INTO tmp_dias_maximos VALUES (arr_estado_solicitud[li_indice].*)                          
             END FOR
             LET cb_modificar = FALSE
             ACCEPT INPUT
             ON ACTION cancelar
               EXIT INPUT
         END INPUT 
    
      
        
      ON ACTION aceptar
      
         -- el folio es forzoso
         {IF ( v_folio IS NULL ) THEN
            CALL fn_mensaje("Atención","Es necesario elegir un folio","stop")
            CONTINUE DIALOG           
         END IF}
      
         -- se validan los datos capturados
         IF ( v_nss IS NOT NULL AND LENGTH(v_nss CLIPPED) <> 11 ) THEN
            CALL fn_mensaje("Atención","La longitud del NSS debe ser de 11 caracteres","stop")
            CONTINUE DIALOG
         END IF
         
         -- deben venir ambas fechas
         IF ( v_fecha_inicio IS NULL OR v_fecha_fin IS NULL ) THEN
            CALL fn_mensaje("Atención","Debe capturar ambas fechas","stop")
            CONTINUE DIALOG
         ELSE
            -- si estan las dos, la fecha inicio no puede ser posterior a la fecha fin
            IF ( v_fecha_inicio > v_fecha_fin ) THEN
               CALL fn_mensaje("Atención","La fecha de inicio no puede ser posterior a la fecha de término","stop")
               CONTINUE DIALOG
            ELSE
               -- ninguna de las fechas puede ser posterior a la fecha actual
               IF ( v_fecha_inicio > TODAY OR v_fecha_fin > TODAY ) THEN
                  CALL fn_mensaje("Atención","La fecha de consulta de inicio o fin no pueden ser posteriores a la fecha actual","stop")
                  CONTINUE DIALOG
               END IF
            END IF
         END IF

       -- borrar tabla temporal si existe
       --DROP TEMPORARY TABLE IF  EXISTS  tmp_dias_maximos
       
       -- consultar control de solicitudes
       -- se envian los datos a la funcion de consulta por solicitud
       CALL fn_consulta_sol_retiro_generico(v_id_solicitud              ,
                                                v_nss                ,
                                                v_rfc                ,
                                                v_modalidad_retiro   , 
                                                v_caso_adai          ,
                                                v_folio              ,
                                                v_folio_restitucion  ,
                                                v_estado ,
                                                v_cod_rechazo      ,
                                                v_fecha_inicio       ,
                                                v_fecha_fin          ,
                                                lb_consulta )     
         
     -- cancelar
     ON ACTION cerrar
       EXIT DIALOG  
     END INPUT
      
     DISPLAY ARRAY arr_estado_solicitud TO tbl_estado_solicitud.*
        
     END DISPLAY
     
     BEFORE DIALOG
       LET ls_sql = "SELECT * FROM ret_generico_max_dias"       
       PREPARE pre_generico FROM ls_sql
       
       DECLARE cur_generico_dias  CURSOR FOR pre_generico 
       LET li_indice = 1    
       FOREACH cur_generico_dias INTO arr_estado_solicitud[li_indice].*
         LET lr_ret_generico_max_dias.* = arr_estado_solicitud[li_indice].*
         LET li_indice = li_indice + 1
         --DISPLAY "arr_estado_solicitud[li_indice].*",arr_estado_solicitud[li_indice].*
         
       END FOREACH

       DISPLAY "ls_sql generico: ",ls_sql
     
        
   END DIALOG      
  CLOSE WINDOW w_consulta
END MAIN


{
======================================================================
Clave: 
Nombre: fn_consulta_solicitud_generico
Fecha creacion: abril 11, 2014
Autor: Jairo Palafox, EFP
Narrativa del proceso que realiza:
Realiza la consulta de los datos de retiro generico para el control de las solicitudes

Registro de modificaciones:
Autor           Fecha          Descrip. cambio

======================================================================
}

FUNCTION fn_consulta_sol_retiro_generico(v_id_solicitud              ,
                                                v_nss                ,
                                                v_rfc                ,
                                                v_modalidad_retiro   , 
                                                v_caso_adai          ,
                                                v_folio              ,
                                                v_folio_restitucion  ,
                                                v_estado ,
                                                v_cod_rechazo      ,
                                                v_fecha_inicio       ,
                                                v_fecha_fin          ,
                                               lb_consulta )      
                                               
 DEFINE v_modalidad_retiro   LIKE ret_modalidad_retiro.modalidad_retiro,
       v_nss                LIKE afi_derechohabiente.nss, 
       v_rfc                LIKE afi_derechohabiente.rfc,
       v_caso_adai          LIKE ret_solicitud_generico.caso_adai,
       v_id_solicitud      LIKE ret_solicitud_generico.id_solicitud,
       v_folio              LIKE glo_folio.folio,
       v_folio_restitucion LIKE ret_solicitud_generico.folio_restitucion,
       v_estado             LIKE ret_estado_solicitud.estado_solicitud,
       v_cod_rechazo        SMALLINT, -- codigo de rechazo
       v_fecha_inicio       DATE, -- fecha de inicio de consulta
       v_fecha_fin          DATE, -- fecha fin de consulta
       v_fecha_actual       DATE, -- se obtiene la fecha actual del sistema
       v_indice             INTEGER, -- contador       
       v_sql                STRING, -- cadena con instruccion sql
       v_elementos_elegidos INTEGER, -- contador de elementos elegidos
       v_r_despliegue       RECORD                    
          id_solicitud       DECIMAL(9,0),
          id_derechohabiente DECIMAL(9,0),
          nss                CHAR(11),
          rfc                CHAR(13),
          modalidad_retiro   smallint, 
          caso_adai          CHAR(10),
          estado_solicitud   smallint,
          f_solicitud        DATE,
          f_consultada       DATE,
          f_caducidad        DATE,       
          total_dias         smallint   
       END RECORD,          
       v_arr_despliegue     DYNAMIC ARRAY OF RECORD
         -- elegir             smallint,
          id_solicitud       DECIMAL(9,0),
          id_derechohabiente DECIMAL(9,0),
          nss                CHAR(11),
          rfc                CHAR(13),
          modalidad_retiro   smallint, 
          caso_adai          CHAR(10),
          estado_solicitud   smallint,
          f_solicitud        DATE,
          f_consultada       DATE,
          f_caducidad        DATE,
          total_dias         smallint  
       END RECORD
       
 DEFINE v_array_peticion_marca DYNAMIC ARRAY OF RECORD 
         id_peticion         decimal(9,0),
         f_peticion          date,
         h_peticion          CHAR(10),         
         res_ejecucion       smallint,
         ind_marca           smallint,
         cod_rechazo         smallint,
         resp_estado_marca   smallint,
         resp_con_retiro     decimal(9,0),
         resp_cod_rechazo    smallint 
        END RECORD
 DEFINE v_array_solicitudes DYNAMIC ARRAY OF RECORD
         id_peticion           DECIMAL(9,0),
         f_peticion            DATE,
         h_peticion            CHAR(10),
         causal_retiro         SMALLINT,
         nrp                   CHAR(18),
         f_inicio_pension      CHAR(8),
         grupo_ley73           CHAR(20),
         num_credito           SMALLINT,
         resp_subcuenta        SMALLINT,
         resp_estado_solicitud SMALLINT,
         resp_cod_rechazo      SMALLINT,
         resp_monto_avis       DECIMAL(22,6),
         resp_monto_pesos      DECIMAl(22,2),
         referencia_dap        CHAR(10)         
        END RECORD,
       li_indice                 INTEGER,
       ls_sql                    STRING,
       lc_nombre_afi             CHAR(150),
       v_sql_filtro              STRING,
       ld_id_solicitud           DECIMAL(9,0),
       ld_id_derechohabiente     DECIMAL(9,0),
       ls_modalidad_retiro       smallint, 
       lc_nss                    CHAR(11),
       lc_rfc                    CHAR(13),
       ls_prueba                 STRING,
       ld_folio                  DECIMAL(9,0),
       ls_grupo_ventanilla       SMALLINT,
       lc_nombre_archi_envio     CHAR(50),
       li_clave_achi_envio       INTEGER,
       lc_nombre_archi_resp      CHAR(50),
       li_clave_achi_resp        INTEGER,       
       li_contador               INTEGER,
       lb_existe_datos           SMALLINT,
       lc_caso_adai              CHAR(10),
       ld_folio_restitucion      DECIMAL(9,0),
       ld_id_archivo_cancela_cxp decimal(9,0),
       ld_id_archivo_resp_cxp    decimal(9,0),
       lc_folio_afore            char(14),
       lc_archivo_cancela        char(50),
       lc_archivo_resp_cancela   char(50),
       lb_consulta               smallint,
       ls_tbl_consulta           string
              
    DEFINE lr_dias_max DYNAMIC ARRAY OF RECORD
           estado_solicitud    SMALLINT,
           total_dias          SMALLINT           
           END RECORD 
       

   
       
   -- ===================================================================================
   -- ===================================================================================
   -- REGISTROS DE AMORTIZACIONES EXCEDENTES
   -- ===================================================================================
   -- se asume que si existe informacion
   LET lb_existe_datos = FALSE

   -- se inicia el contador en 0
   LET li_contador = 0 

   -- se leen los datos de la tabla ret_generico max dias
   -- y se insertan en la tabla temporal incluyendo los nuevos ingresados por el usuario
   -- verifica si no existen los datos los inserta de lo contario no inserta nada
   INSERT INTO tmp_dias_maximos
   SELECT * FROM ret_generico_max_dias
   WHERE estado_solicitud NOT IN  ( SELECT UNIQUE estado_solicitud FROM tmp_dias_maximos )

   DECLARE cur_tmp CURSOR FOR SELECT * FROM tmp_dias_maximos

   LET li_indice = 1

   FOREACH cur_tmp INTO lr_dias_max[li_indice].*
     -- se incrementa el indice
     LET li_indice = li_indice + 1
     -- se muestra el resultado de la consulta
     --DISPLAY "lr_dias_max[li_indice].estado_solicitud",lr_dias_max[li_indice].estado_solicitud
     --DISPLAY "lr_dias_max[li_indice].total_dias",lr_dias_max[li_indice].total_dias
     
   END FOREACH
   DISPLAY "lb_consulta"
   IF (  lb_consulta ) THEN
      -- se consulta de la tabla de catalogo de la bd
      LET ls_tbl_consulta = "ret_generico_max_dias"
   ELSE
      -- se consulta de la temporal datos que ingresa y acepta el usuario
      LET ls_tbl_consulta = "tmp_dias_maximos"
   END IF
   -- se verifica si modalidad retiro no es nula que ejecute la consulta de lo contrario se pasa sin filtrar
   IF ( v_modalidad_retiro IS NOT NULL) THEN
     -- se verifica si existen datos
     select count(*) 
     INTO li_contador
     from ret_solicitud_generico a, ret_generico_max_dias b
     WHERE 
          a.estado_solicitud =  b.estado_solicitud
      AND a.modalidad_retiro = v_modalidad_retiro 

               
  -- se consultan los datos de la tabla
  LET v_sql = "\nselect a.id_solicitud",
                 "\n,a.id_derechohabiente",
                 "\n,a.nss",
                 "\n,a.rfc",
                 "\n,a.modalidad_retiro",
                 "\n,a.caso_adai",
                 "\n,a.estado_solicitud",
                 "\n,f_solicitud", 
                 "\n,TODAY",
                 "\n,(a.f_solicitud + b.max_dias)",
                 "\n,sum(today - a.f_solicitud) ",               
                 " from ret_solicitud_generico a,", ls_tbl_consulta," b",
                 " WHERE a.estado_solicitud =  b.estado_solicitud",
                 " AND a.f_solicitud BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'"
                 
    LET v_sql_filtro = ""
    -- si se recibio nss 
    IF ( v_nss IS NOT NULL ) THEN
      LET v_sql_filtro = v_sql_filtro, "\n AND a.nss = '", v_nss, "'"
    END IF

    -- si se recibio rfc
    IF ( v_rfc IS NOT NULL ) THEN
      LET v_sql_filtro = v_sql_filtro, "\n AND a.rfc = '", v_rfc, "'"
    END IF

    -- si se capturo numero de solicitud
    IF ( v_id_solicitud IS NOT NULL ) THEN
      LET v_sql_filtro = v_sql_filtro, "\n AND a.id_solicitud = '", v_id_solicitud, "'"
    END IF

    -- si se capturo folio de restitución
    IF ( v_folio_restitucion IS NOT NULL ) THEN
      LET v_sql_filtro = v_sql_filtro, "\n AND a.folio_restitucion = ", v_folio_restitucion
    END IF
   
    -- si se recibio caso adai
    IF ( v_caso_adai IS NOT NULL ) THEN
      LET v_sql_filtro = v_sql_filtro, "\n AND a.caso_adai = ", v_caso_adai
    END IF

    LET v_sql = v_sql,v_sql_filtro
    -- si se capturo folio
    IF ( v_folio IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.folio = ", v_folio
    END IF
   
    -- modalidad de retiro especifica
    IF ( v_modalidad_retiro IS NOT NULL ) THEN
      LET v_sql = v_sql || "\n AND a.modalidad_retiro = '", v_modalidad_retiro, "'"
    END IF

    -- si se recibio estado de la solicitud
    IF ( v_estado IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.estado_solicitud = ", v_estado
    END IF

    -- si se recibio codigo de rechazo especifico
    IF ( v_cod_rechazo IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.cod_rechazo = ", v_cod_rechazo
    END IF

    -- se concatena la agrupacion y ordenamiento en turno
    LET v_sql = v_sql, "\n GROUP BY 1,2,3,5,8,9,10,11",
                       "\n ORDER BY 2"
    DISPLAY "v_sql 1",v_sql
        
   ELSE
     -- se verifica si existen datos
     select count(*) 
     INTO li_contador
     from ret_solicitud_generico a, ret_generico_max_dias b
     WHERE 
          a.estado_solicitud =  b.estado_solicitud   
     AND
       a.f_solicitud BETWEEN v_fecha_inicio AND v_fecha_fin
    
      
     -- se consultan los datos de la tabla
     LET v_sql = " select a.id_solicitud,a.id_derechohabiente,a.nss,a.rfc,a.modalidad_retiro,a.caso_adai,a.estado_solicitud,",
               " f_solicitud, TODAY, (a.f_solicitud + b.max_dias), sum(today - a.f_solicitud) ",
               " from ret_solicitud_generico a,", ls_tbl_consulta," b",
               " WHERE a.estado_solicitud =  b.estado_solicitud",  
               " AND a.f_solicitud BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'"
    

     LET v_sql_filtro = ""
     -- si se recibio nss 
     IF ( v_nss IS NOT NULL ) THEN
       LET v_sql_filtro = v_sql_filtro, "\n AND a.nss = '", v_nss, "'"
     END IF

     -- si se recibio rfc
     IF ( v_rfc IS NOT NULL ) THEN
       LET v_sql_filtro = v_sql_filtro, "\n AND a.rfc = '", v_rfc, "'"
     END IF

     -- si se capturo numero de solicitud
     IF ( v_id_solicitud IS NOT NULL ) THEN
       LET v_sql_filtro = v_sql_filtro, "\n AND a.id_solicitud = '", v_id_solicitud, "'"
     END IF

     -- si se capturo folio de restitución
     IF ( v_folio_restitucion IS NOT NULL ) THEN
       LET v_sql_filtro = v_sql_filtro, "\n AND a.folio_restitucion = ", v_folio_restitucion
     END IF
   
     -- si se recibio caso adai
     IF ( v_caso_adai IS NOT NULL ) THEN
       LET v_sql_filtro = v_sql_filtro, "\n AND a.caso_adai = ", v_caso_adai
     END IF

     LET v_sql = v_sql,v_sql_filtro
     -- si se capturo folio
     IF ( v_folio IS NOT NULL ) THEN
       LET v_sql = v_sql, "\n AND a.folio = ", v_folio
     END IF
   
     -- modalidad de retiro especifica
     IF ( v_modalidad_retiro IS NOT NULL ) THEN
       LET v_sql = v_sql || "\n AND a.modalidad_retiro = '", v_modalidad_retiro, "'"
     END IF

     -- si se recibio estado de la solicitud
     IF ( v_estado IS NOT NULL ) THEN
       LET v_sql = v_sql, "\n AND a.estado_solicitud = ", v_estado
     END IF

     -- si se recibio codigo de rechazo especifico
     IF ( v_cod_rechazo IS NOT NULL ) THEN
       LET v_sql = v_sql, "\n AND a.cod_rechazo = ", v_cod_rechazo
     END IF

     -- se concatena la agrupacion y ordenamiento en turno
     LET v_sql = v_sql, "\n GROUP BY 1,2,3,4,5,6,7,8,9,10",
                      "\n ORDER BY 2"

     DISPLAY "v_sql 2",v_sql                     
   END IF 

   LET v_indice = 1


   IF SQLCA.SQLCODE = NOTFOUND THEN
     DISPLAY "SQLCA.SQLERRD[1]",SQLCA.SQLERRD[1]
     DISPLAY "SQLCA.SQLERRD[2]",SQLCA.SQLERRD[2]
     DISPLAY "SQLCA.SQLERRD[3]",SQLCA.SQLERRD[3]
     DISPLAY "SQLCA.SQLERRD[4]",SQLCA.SQLERRD[4]
     DISPLAY "SQLCA.SQLERRD[5]",SQLCA.SQLERRD[5]
     DISPLAY "SQLCA.SQLERRD[6]",SQLCA.SQLERRD[6]
     DISPLAY "SQLCA.SQLCODE",SQLCA.SQLCODE   
      DISPLAY "No row was found"
   END IF

   -- se verifica si no existe información termina la ejecución del programa
   IF ( li_contador = 0 OR li_contador IS NULL ) THEN
     -- se activa la bandera de no existencia de informacion a true
     LET lb_existe_datos = TRUE
   END IF
   -- se prepara la consulta general acumulada
   PREPARE sid_solicitudes1 FROM v_sql
   DECLARE cur_solicitudes1 CURSOR FOR sid_solicitudes1
   -- Se consulta la informacion para cada uno de los casos encontrados
   FOREACH cur_solicitudes1 INTO v_r_despliegue.*
        
     -- se verifica que la consulta a mostrar sea la que indique el catalogo de días maximos
     LET v_sql = "\nSELECT FIRST 1 max_dias",
                   "\nFROM tmp_dias_maximos",
                   "\nWHERE estado_solicitud ='", v_r_despliegue.estado_solicitud, "'"

     PREPARE pre_sol FROM v_sql
     EXECUTE  pre_sol INTO li_contador 
     
     -- se el estado de solicitud consultado es mayor o igual se muestra de lo contrario no
     --DISPLAY "entra foreach"
     DISPLAY "li contador", li_contador

     DISPLAY "v_r_despliegue.total_dias",v_r_despliegue.total_dias
     
     -- se verifica que el maximo de dias indicado en el catalogo coincida con la fecha consultada
     IF ( li_contador  < = v_r_despliegue.total_dias ) THEN       
       DISPLAY "entra if contador"       
       -- se transfieren los datos al arreglo que muestra la informacion e pantalla
       LET v_arr_despliegue[v_indice].id_solicitud       = v_r_despliegue.id_solicitud
       LET v_arr_despliegue[v_indice].id_derechohabiente = v_r_despliegue.id_derechohabiente     
       LET v_arr_despliegue[v_indice].nss                = v_r_despliegue.nss
       LET v_arr_despliegue[v_indice].rfc                = v_r_despliegue.rfc
       LET v_arr_despliegue[v_indice].modalidad_retiro   = v_r_despliegue.modalidad_retiro
       LET v_arr_despliegue[v_indice].caso_adai          = v_r_despliegue.caso_adai
       LET v_arr_despliegue[v_indice].estado_solicitud   = v_r_despliegue.estado_solicitud
       LET v_arr_despliegue[v_indice].f_solicitud        = v_r_despliegue.f_solicitud
       LET v_arr_despliegue[v_indice].f_consultada       = v_r_despliegue.f_consultada
       LET v_arr_despliegue[v_indice].f_caducidad        = v_r_despliegue.f_caducidad
       LET v_arr_despliegue[v_indice].total_dias         = v_r_despliegue.total_dias

       LET lb_existe_datos = TRUE
     
       {IF NOT lb_existe_datos THEN
         LET lb_consulta = TRUE
     END IF}
     ELSE
       LET lb_existe_datos = FALSE
       --CALL fn_mensaje("Atención","No existe información a mostrar","stop")      
     END IF
     --ELSE
     DISPLAY "lb_existe_datos",lb_existe_datos



       
     -- se tranfieren los datos del registro al arreglo global
     --LET g_arr_solicitudes_folio[v_indice].elegir             = 0
     LET g_arr_solicitudes_folio[v_indice].id_solicitud       = v_r_despliegue.id_solicitud
     LET g_arr_solicitudes_folio[v_indice].id_derechohabiente = v_r_despliegue.id_derechohabiente     
     LET g_arr_solicitudes_folio[v_indice].nss                = v_r_despliegue.nss
     LET g_arr_solicitudes_folio[v_indice].rfc                = v_r_despliegue.rfc
     LET g_arr_solicitudes_folio[v_indice].modalidad_retiro   = v_r_despliegue.modalidad_retiro
     LET g_arr_solicitudes_folio[v_indice].caso_adai          = v_r_despliegue.caso_adai
     LET g_arr_solicitudes_folio[v_indice].estado_solicitud   = v_r_despliegue.estado_solicitud
     LET g_arr_solicitudes_folio[v_indice].f_solicitud        = v_r_despliegue.f_solicitud
     LET g_arr_solicitudes_folio[v_indice].f_consultada       = v_r_despliegue.f_consultada
     LET g_arr_solicitudes_folio[v_indice].f_caducidad        = v_r_despliegue.f_caducidad
     LET g_arr_solicitudes_folio[v_indice].total_dias         = v_r_despliegue.total_dias

     -- se impreme el resultado de la consulta
     ---DISPLAY "g_arr_solicitudes_folio[v_indice].f_caducidad",g_arr_solicitudes_folio[v_indice].f_caducidad
     --DISPLAY "g_arr_solicitudes_folio[v_indice].total_dias",g_arr_solicitudes_folio[v_indice].total_dias
    
     
     -- se incrementa el indice del arreglo
     LET v_indice = v_indice + 1 
   END FOREACH
   -- si existe información se muestra la siguiente pantalla de lo contrario indica un mensaje de no existen datos a mostrar
   IF (  lb_existe_datos ) THEN   
     OPEN WINDOW w_consulta1 WITH FORM "RETC2722"
       DIALOG ATTRIBUTE (UNBUFFERED)

       DISPLAY ARRAY v_arr_despliegue 
       TO tbl_solicitudes_1.* 
   
       BEFORE ROW    
         LET  ld_id_solicitud = g_arr_solicitudes_folio[ARR_CURR()].id_solicitud
         LET  ld_id_derechohabiente = g_arr_solicitudes_folio[ARR_CURR()].id_derechohabiente      
         -- se recupera la modalidad para hacer las busquedas pertinentes
         LET  ls_modalidad_retiro = g_arr_solicitudes_folio[ARR_CURR()].modalidad_retiro
         LET  lc_nss = g_arr_solicitudes_folio[ARR_CURR()].nss
         LET  lc_rfc = g_arr_solicitudes_folio[ARR_CURR()].rfc
         LET  lc_caso_adai = g_arr_solicitudes_folio[ARR_CURR()].caso_adai
     

         -- se consulta el detalle de la solicitud
         -- obtieniendo folio  y grupoo ventanilla
     
         SELECT a.folio, a.grupo_ventanilla,a.id_archivo_envio,( select b.nombre_archivo FROM ret_ctr_archivo_fico b WHERE  a.id_archivo_envio = b.id_archivo ),
          a.id_archivo_respuesta,( select b.nombre_archivo FROM ret_ctr_archivo_fico b WHERE  a.id_archivo_respuesta = b.id_archivo ),
          a.folio_restitucion,a.id_archivo_cancela_cxp,(select b.nombre_archivo FROM ret_ctr_archivo_fico b WHERE  a.id_archivo_cancela_cxp = b.id_archivo ),
          a.id_archivo_resp_cxp,(select b.nombre_archivo FROM ret_ctr_archivo_fico b WHERE  a.id_archivo_resp_cxp = b.id_archivo ), folio_afore  
         INTO ld_folio, ls_grupo_ventanilla, li_clave_achi_envio, lc_nombre_archi_envio, li_clave_achi_resp, lc_nombre_archi_resp,
            ld_folio_restitucion, ld_id_archivo_cancela_cxp,lc_archivo_cancela, ld_id_archivo_resp_cxp, lc_archivo_resp_cancela,lc_folio_afore               
         FROM ret_solicitud_generico a
         WHERE a.id_solicitud = ld_id_solicitud
        
         -- se verifica si la modalidad es 2 corresponde a fondo retiro 72
         IF ( ls_modalidad_retiro = 2 ) THEN
           LET ls_sql = " SELECT nombre FROM afi_fondo72",
                     " WHERE rfc ='", lc_rfc,"'",
                     " AND nss = '", lc_nss,"'"
           --DISPLAY   "ls_sql retiro 2",ls_sql           
           -- se prepara la consulta
           PREPARE pre_afi_fondo72 FROM ls_sql
           -- se ejecuta la consulta usando el id_derechohabiente        
           EXECUTE   pre_afi_fondo72 INTO  lc_nombre_afi
           -- se libera
           FREE pre_afi_fondo72
         ELSE
           LET ls_sql = " SELECT nombre_imss FROM afi_derechohabiente",
                     " WHERE id_derechohabiente =", ld_id_derechohabiente                    
           --DISPLAY   "ls_sql retiros restantes",ls_sql           
           -- se prepara la consulta
           PREPARE pre_afi_derechohabiente FROM ls_sql
           -- se ejecuta la consulta usando el id_derechohabiente
        
           EXECUTE   pre_afi_derechohabiente INTO  lc_nombre_afi

           -- se libera
           FREE pre_afi_derechohabiente
         END IF
      
         -- se verifica si existe informacion
         IF ( lc_nombre_afi IS NULL ) THEN
           LET lc_nombre_afi = "Sin Información"
         END IF
         DISPLAY BY NAME lc_nombre_afi, ld_folio, ls_grupo_ventanilla, lc_nombre_archi_envio, li_clave_achi_envio, lc_nombre_archi_resp, li_clave_achi_resp,
                       ld_folio_restitucion, ld_id_archivo_cancela_cxp, ld_id_archivo_resp_cxp, lc_folio_afore               
                    
         -- se consultan los datos a mostrar
         LET ls_sql = " SELECT a.id_peticion, a.f_peticion, a.h_peticion, a.res_ejecucion, ",
                      " b.ind_marca, b.cod_rechazo, b.resp_estado_marca, b.resp_con_retiro, b.resp_cod_rechazo ",
                      " FROM ret_ws_peticion_marca a, ret_ws_det_peticion_marca b",
                      " WHERE a.id_peticion = b.id_peticion",  
                      " AND   a.nss = '",lc_nss, "'",
                      " AND   a.rfc = '",lc_rfc, "'",
                      " AND   a.caso_adai ='", lc_caso_adai,"'" 

         DISPLAY "Datos: Bitácora: ", ls_sql              
         PREPARE pre_marca FROM ls_sql             
         -- se declara el cursor para abrir el canal de consulta                    
         DECLARE cur_peticion CURSOR FOR pre_marca     
         -- se inicia el indice del arreglo  
         LET li_indice = 1
         --se inicializa el arreglo en nulo para que no guarde en memoria datos viejos
         INITIALIZE v_array_peticion_marca TO NULL
         
         -- se recorre para cada una de las peticiones de la marca
         FOREACH cur_peticion INTO v_array_peticion_marca[li_indice].*
           LET li_indice = li_indice + 1
           -- se muestran los datos en pantalla
           --DISPLAY v_array_peticion_marca[li_indice].*
         END FOREACH

         --DISPLAY "Number of rows found: ", li_indice

         -- se consultan todas las solicitudes creadas    
         LET ls_sql  = "select peticion.id_peticion, peticion.f_peticion, peticion.h_peticion, ",
                      " detalle.causal_retiro, detalle.nrp, detalle.f_inicio_pension, detalle.grupo_ley73, ",
                      " detalle.num_credito, resp.resp_subcuenta, resp.resp_estado_solicitud, ",
                      " resp.resp_estado_solicitud, resp.resp_cod_rechazo, resp.resp_monto_avis, ",
                      " resp.resp_monto_pesos, resp.referencia_dap ",
                      " FROM ret_ws_peticion_crea_solicitud peticion LEFT JOIN ret_ws_det_peticion_crea_solicitud detalle ",
                      " ON peticion.id_peticion = detalle.id_peticion ",
                      " LEFT JOIN ret_ws_det_peticion_crea_solicitud_resp resp ",
                      " ON peticion.id_peticion = resp.id_peticion ",
                      " WHERE peticion.nss = '", lc_nss ,"'",
                      "  AND peticion.rfc = '", lc_rfc ,"'",
                      "  AND peticion.caso_adai = '", lc_caso_adai ,"'"--,
                      --"  AND peticion.modalidad_retiro = '", ls_modalidad_retiro ,"'"
                      
         --DISPLAY "Crea Solicitud: ", ls_sql
         -- se prepara la consulta para su ejecucion        
         PREPARE pre_crea_solicitud FROM ls_sql

         DECLARE cur_solicitud CURSOR FOR pre_crea_solicitud

         -- se inicia el indice del arreglo 
         LET li_indice = 1
         --se inicializa el arreglo en nulo para que no guarde en memoria datos viejos
         INITIALIZE v_array_solicitudes TO NULL
        
         -- se recorre la informacion y se almacena en el arrelo para cada uno de los casos encontrados
         FOREACH cur_solicitud INTO v_array_solicitudes[li_indice].*
           -- se incrementa el indice del arreglo
           LET li_indice = li_indice + 1          
         END FOREACH            
         END DISPLAY    
         -- muestra los datos de la premarca
         DISPLAY ARRAY v_array_peticion_marca TO tbl_peticiones.*
          
         END DISPLAY 
        
         -- muestra los datos de la creacion de las solicitudes
         DISPLAY ARRAY v_array_solicitudes TO tbl_crea_solicitudes.*

         END DISPLAY

    ON ACTION cerrar
      EXIT DIALOG 
         
    END DIALOG
   CLOSE WINDOW w_consulta1
  ELSE
    CALL fn_mensaje("Atención","No existe información a mostrar","stop")
  END IF   
END FUNCTION
