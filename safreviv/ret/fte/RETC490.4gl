--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#MODULO       => RET                                                                    #
#PROGRAMA     => RETC463                                                                #
#OBJETIVO     => Consulta de solicitudes de retiro del Solo infonavit                   #
#Fecha inicio => 01 Abril 2019                                                          #
#Modificacion =>                                                                        #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS 
DEFINE g_arr_solicitudes_folio DYNAMIC ARRAY OF RECORD -- arreglo que contiene los folios elegidos para consultar
          elegir                 SMALLINT,
          modalidad_retiro       SMALLINT,
          desc_modalidad         VARCHAR(50),
          num_solicitudes        SMALLINT,
          folio                  DECIMAL(9,0),
          tanto_normal           DECIMAL(22,2),
          tanto_adicional        DECIMAL(22,2),
          estado_solicitud       CHAR(100),
          cod_rechazo            CHAR(100)
       END RECORD,
       p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion    SMALLINT, -- forma como ejecutara el programa
       p_s_titulo          STRING   -- titulo de la ventana 
    
END GLOBALS 

{
======================================================================
Clave: 
Nombre: main
Fecha creacion: Febrero 8, 2018
Autor: Ricardo P�rez
Narrativa del proceso que realiza:
Abre la ventana de captura de datos para realizar la consulta de retiro
de Ley 73

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
MAIN 
DEFINE cbx_estado_solicitud ui.ComboBox, -- combo de estado de la solicitud
       cbx_causal            ui.ComboBox, -- combo para las causales
       cbx_medio_pago        ui.ComboBox, -- combo para los medios de pago
       ar_chavtipo_retiro   RECORD LIKE ret_tipo_retiro.*, -- registro con los tipos de retiro
       ar_ret_rechazo       RECORD LIKE ret_rechazo.*, -- registro con los codigos de rechazo
       -- parametros de consulta
       v_tipo_retiro        LIKE ret_tipo_retiro.tpo_retiro,
       v_etapa              SMALLINT, -- 1 Solicitud, 2 Preliquidacion, 3 Liquidacion
       v_nss                LIKE afi_derechohabiente.nss, 
       v_rfc                LIKE afi_derechohabiente.rfc,
       v_id_solicitud       LIKE ret_solicitud_generico.id_solicitud,
       v_folio              LIKE glo_folio.folio,
       v_folio_restitucion  LIKE ret_solicitud_generico.folio_restitucion,
       v_estado             LIKE ret_estado_solicitud.estado_solicitud,
       v_cod_rechazo        SMALLINT, -- codigo de rechazo
       v_causal             SMALLINT, -- causal
       v_medio_pago         SMALLINT, -- medio pago
       v_fecha_inicio       DATE, -- fecha de inicio de consulta
       v_fecha_fin          DATE, -- fecha fin de consulta
       v_fecha_pago_ini     DATE, -- fecha de pago inicial
       v_fecha_pago_fin     DATE, -- fecha de pago final
       v_cadena             STRING, -- cadena para concatenar
       v_modalidad_retiro   LIKE ret_modalidad_retiro.modalidad_retiro, -- modalidad de retiro
       v_caso_adai         LIKE ret_solicitud_generico.caso_adai,   -- caso ADAI de la solicitud
       v_formulario        ui.Form, -- para modificar el formulario
       ar_ret_estado_solicitud RECORD LIKE ret_estado_solicitud.*,
       ar_ret_cat_medio_entrega RECORD LIKE ret_cat_medio_entrega.*

   UPDATE STATISTICS FOR TABLE ret_solicitud_generico;
   UPDATE STATISTICS FOR TABLE ret_pago_dap;
   UPDATE STATISTICS FOR TABLE ret_ctr_archivo_fico;
   UPDATE STATISTICS FOR TABLE ret_ws_consulta_pago_fico;
   UPDATE STATISTICS FOR TABLE ret_cat_edo_pago_fico;
   UPDATE STATISTICS FOR TABLE ret_modalidad_retiro;
   UPDATE STATISTICS FOR TABLE ret_rechazo_generico;
   UPDATE STATISTICS FOR TABLE ret_estado_solicitud;
   UPDATE STATISTICS FOR TABLE afi_fondo72;
   UPDATE STATISTICS FOR TABLE ret_ws_det_peticion_marca;
   UPDATE STATISTICS FOR TABLE ret_ws_peticion_marca;

   -- se obtienen los parametros de ejecucion
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETC480.log")
   
   CLOSE WINDOW SCREEN
   
   -- se abre la ventana de consulta
   OPEN WINDOW w_consulta WITH FORM "RETC4801"

   -- se capturan los datos de la consulta
   INPUT BY NAME
      v_nss              ,
      v_caso_adai        ,
      v_id_solicitud     ,
      v_folio            ,
      v_folio_restitucion,
      v_causal           ,
      v_medio_pago       ,
      v_estado           ,
      v_fecha_inicio     ,
      v_fecha_fin        ,
      v_fecha_pago_ini   ,
      v_fecha_pago_fin
   WITHOUT DEFAULTS
   ATTRIBUTES ( UNBUFFERED )
   
      BEFORE INPUT
         -- se obtiene control del formulario
         LET v_formulario = DIALOG.getForm()

         LET v_modalidad_retiro = 3

         -- no se tiene folio
         LET v_folio = NULL

         -- se llena el combo con las causales
         LET cbx_causal = ui.ComboBox.forName("formonly.v_causal")
         
         CALL cbx_causal.clear()
         CALL cbx_causal.addItem(NULL,"Todas")
         CALL cbx_causal.addItem(1, '1-T�rmino Relaci�n Laboral')
         CALL cbx_causal.addItem(2, '2-Pensi�n IMSS')
         CALL cbx_causal.addItem(3, '3-Plan Privado de Pensi�n')
         CALL cbx_causal.addItem(4, '4-Defunci�n')

         LET v_causal = NULL 

         -- se llena el combo con los Medios de Pago
         LET cbx_medio_pago = ui.ComboBox.forName("formonly.v_medio_pago")
         
         CALL cbx_medio_pago.clear()
         CALL cbx_medio_pago.addItem(NULL,"Todas")
         CALL cbx_medio_pago.addItem(1, 'Cuenta CLABE')
         CALL cbx_medio_pago.addItem(2, 'DAP')
         
         -- se llena el combo con los estados de la solicitud
         LET cbx_estado_solicitud = ui.ComboBox.forName("formonly.v_estado")
         
         CALL cbx_estado_solicitud.clear()
         -- se usan 3 conjuntos
         CALL cbx_estado_solicitud.addItem(NULL,"Todas")

         
         DECLARE cur_estadossol CURSOR FOR
         SELECT  estado_solicitud,des_corta
         FROM    ret_estado_solicitud
         WHERE   estado_solicitud IN (8,10,15,50,60,69,70,71,72,73,77,80,81,82,90,100,200,209,210,214,700)
         ORDER BY estado_solicitud
         
         FOREACH cur_estadossol INTO ar_ret_estado_solicitud.estado_solicitud, ar_ret_estado_solicitud.des_corta
            LET v_cadena = ar_ret_estado_solicitud.estado_solicitud, " - ", ar_ret_estado_solicitud.des_corta
            CALL cbx_estado_solicitud.addItem(ar_ret_estado_solicitud.estado_solicitud, v_cadena)
         END FOREACH

{
         CALL cbx_estado_solicitud.addItem(2,"Aceptadas")
         CALL cbx_estado_solicitud.addItem(3,"Rechazadas")
}
         -- se asume que se desean todas
         LET v_estado = NULL
         
         -- la fecha de inicio y fecha fin se inician con la fecha del dia
         LET v_fecha_inicio = TODAY
         LET v_fecha_fin    = TODAY
         LET v_fecha_pago_ini = TODAY
         LET v_fecha_pago_fin = TODAY
                  
      -- cancelar
      ON ACTION cancel
         EXIT INPUT
         
      ON ACTION accept
      {
         -- el folio es forzoso
         IF ( v_folio IS NULL ) THEN
            CALL fn_mensaje("Atenci�n","Es necesario elegir un folio","stop")
            CONTINUE INPUT           
         END IF
      }

         -- se validan los datos capturados
         IF ( v_nss IS NOT NULL AND LENGTH(v_nss CLIPPED) <> 11 ) THEN
            CALL fn_mensaje("Atenci�n","La longitud del NSS debe ser de 11 caracteres","stop")
            CONTINUE INPUT
         END IF
         
         -- deben venir ambas fechas
         IF v_fecha_inicio IS NOT NULL AND v_fecha_fin IS NOT NULL THEN 
            IF ( v_fecha_inicio IS NULL OR v_fecha_fin IS NULL ) THEN
               CALL fn_mensaje("Atenci�n","Debe capturar ambas fechas","stop")
               CONTINUE INPUT
            ELSE
               -- si estan las dos, la fecha inicio no puede ser posterior a la fecha fin
               IF ( v_fecha_inicio > v_fecha_fin ) THEN
                  CALL fn_mensaje("Atenci�n","La fecha de inicio no puede ser posterior a la fecha de t�rmino","stop")
                  CONTINUE INPUT
               ELSE
                  -- ninguna de las fechas puede ser posterior a la fecha actual
                  IF ( v_fecha_inicio > TODAY OR v_fecha_fin > TODAY ) THEN
                     CALL fn_mensaje("Atenci�n","La fecha de consulta de inicio o fin no pueden ser posteriores a la fecha actual","stop")
                     CONTINUE INPUT
                  END IF
               END IF
            END IF
         END IF 
         IF ( v_fecha_pago_ini IS NOT NULL AND v_fecha_pago_fin IS NOT NULL ) THEN
            -- si estan las dos, la fecha de pago inicio no puede ser posterior a la fecha de pago fin
            IF ( v_fecha_pago_ini > v_fecha_pago_fin ) THEN
               CALL fn_mensaje("Atenci�n","La fecha de pago inicial no puede ser posterior a la fecha de pago de t�rmino","stop")
               CONTINUE INPUT
            ELSE
               -- ninguna de las fechas puede ser posterior a la fecha actual
               IF ( v_fecha_pago_ini > TODAY OR v_fecha_pago_fin > TODAY ) THEN
                  CALL fn_mensaje("Atenci�n","La fecha de pago de inicio o fin no pueden ser posteriores a la fecha actual","stop")
                  CONTINUE INPUT
               END IF
            END IF
         END IF
         -- consultar solicitudes
         -- se envian los datos a la funcion de consulta por solicitud
         --DISPLAY "Modalidad y Causal Retiro >" || v_modalidad_retiro || "< >" || v_causal_retiro_trab || "<";
         CALL fn_consulta_solicitud_generico(v_modalidad_retiro    ,
                                                v_nss              ,
                                                v_caso_adai        ,
                                                v_id_solicitud     ,
                                                v_folio            ,
                                                v_folio_restitucion,
                                                v_causal           ,
                                                v_medio_pago       ,
                                                v_estado           ,
                                                v_cod_rechazo      ,
                                                v_fecha_inicio     ,
                                                v_fecha_fin        ,
                                                v_fecha_pago_ini   ,
                                                v_fecha_pago_fin    )
   
   END INPUT

  CLOSE WINDOW w_consulta
END MAIN


{
======================================================================
Clave: 
Nombre: fn_consulta_solicitud_generico
Fecha creacion: Febrero 8, 2018
Autor: Ricardo P�rez
Narrativa del proceso que realiza:
Realiza la consulta de los datos de retiro Ley 73

Registro de modificaciones:
Autor           Fecha          Descrip. cambio
======================================================================
}
FUNCTION fn_consulta_solicitud_generico(v_modalidad_retiro, v_nss, v_caso_adai,
                                           v_id_solicitud, v_folio, v_folio_restitucion,
                                           v_causal, v_medio_pago,
                                           v_estado, v_cod_rechazo,
                                           v_fecha_inicio, v_fecha_fin, v_fecha_pago_ini,
                                           v_fecha_pago_fin)
DEFINE v_modalidad_retiro   LIKE ret_modalidad_retiro.modalidad_retiro,
       v_nss                LIKE afi_derechohabiente.nss, 
       v_caso_adai          LIKE ret_solicitud_generico.caso_adai,
       v_id_solicitud       LIKE ret_solicitud_generico.id_solicitud,
       v_folio              LIKE glo_folio.folio,
       v_folio_restitucion  LIKE ret_solicitud_generico.folio_restitucion,
       v_causal             SMALLINT,
       v_medio_pago         SMALLINT,
       v_estado             LIKE ret_estado_solicitud.estado_solicitud,
       v_cod_rechazo        SMALLINT, -- codigo de rechazo
       v_fecha_inicio       DATE, -- fecha de inicio de consulta
       v_fecha_fin          DATE, -- fecha fin de consulta
       v_fecha_pago_ini     DATE, -- fecha de pago inicial
       v_fecha_pago_fin     DATE, -- fecha de pago final
       v_indice             INTEGER, -- contador       
       v_sql                STRING, -- cadena con instruccion sql
       v_elementos_elegidos INTEGER, -- contador de elementos elegidos
       v_r_despliegue       RECORD
          elegir             SMALLINT,
          modalidad_retiro   SMALLINT,
          desc_modalidad     VARCHAR(50),
          num_solicitudes    SMALLINT,
          folio              DECIMAL(9,0),
          tanto_normal       DECIMAL(22,2),
          tanto_adicional    DECIMAL(22,2),
          estado_solicitud   CHAR(100),
          cod_rechazo        CHAR(100)
       END RECORD,          
       v_arr_despliegue     DYNAMIC ARRAY OF RECORD
          elegir             SMALLINT,
          modalidad_retiro   SMALLINT,
          desc_modalidad     VARCHAR(50),
          num_solicitudes    SMALLINT,
          folio              DECIMAL(9,0),
          tanto_normal       DECIMAL(22,2),
          tanto_adicional    DECIMAL(22,2),
          estado_solicitud_d CHAR(100),
          cod_rechazo_d      CHAR(100)
       END RECORD,
       v_arr_des_det DYNAMIC ARRAY OF RECORD
          estado_solicitud   INT,
          cod_rechazo        INT
       END RECORD,
       v_sql_filtro STRING
DEFINE w ui.Window
DEFINE f ui.Form
   -- ===================================================================================
   -- ===================================================================================
   -- REGISTROS DEL FONDO DE AHORRO
   -- ===================================================================================
   -- se construye la cadena de consulta
LET v_sql = "\n SELECT                                                                    ",
            "\n 1                 ,                                                       ",
            "\n a.modalidad_retiro,                                                       ",
            "\n a.modalidad_retiro || ' - ' || e.des_corta,                               ",
            "\n COUNT(*)          ,                                                       ",
            "\n a.folio           ,                                                       ",
            "\n 0,                                                                        ",
            "\n 0,                                                                        ",
            "\n a.estado_solicitud || '-' || es.des_corta,                                ",
            "\n a.cod_rechazo || '-' || cr.des_corta,                                     ",
            "\n a.estado_solicitud,                                                       ",
            "\n a.cod_rechazo                                                             ",
            "\n FROM ret_solicitud_generico  a,                                           ",
            "\n ret_modalidad_retiro  e,                                                  ",
            "\n ret_estado_solicitud es,                                                  ",
            "\n ret_rechazo_generico cr                                                   ",
            "\n WHERE 1 = 1                                                               ",
            "\n AND   a.modalidad_retiro = e.modalidad_retiro                             ",
            "\n AND   a.modalidad_retiro = 1                                              ",   --- Solo infonavit
            "\n AND   a.estado_solicitud = es.estado_solicitud                            ",
            "\n AND   a.cod_rechazo      = cr.cod_rechazo                                 "

   LET v_sql_filtro = ""
   IF v_fecha_inicio IS NOT NULL AND v_fecha_fin IS NOT NULL THEN 
      LET v_sql_filtro = v_sql_filtro, "\n AND a.f_solicitud BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'"
   END IF 
   -- si se recibio nss 
   IF ( v_nss IS NOT NULL ) THEN
      LET v_sql_filtro = v_sql_filtro, "\n AND a.nss = '", v_nss, "'"
   END IF

   -- si se capturo numero de solicitud
   IF ( v_id_solicitud IS NOT NULL ) THEN
      LET v_sql_filtro = v_sql_filtro, "\n AND a.id_solicitud = '", v_id_solicitud, "'"
   END IF

   -- si se capturo folio de restituci�n
   IF ( v_folio_restitucion IS NOT NULL ) THEN
      LET v_sql_filtro = v_sql_filtro, "\n AND a.folio_restitucion = ", v_folio_restitucion
   END IF
   
   -- si se recibio caso adai
   IF ( v_caso_adai IS NOT NULL ) THEN
      LET v_sql_filtro = v_sql_filtro, "\n AND a.caso_adai = ", v_caso_adai
   END IF

   -- si se recibio grupo
   IF ( v_causal IS NOT NULL ) THEN
      LET v_sql_filtro = v_sql_filtro, "\n AND a.id_solicitud IN (SELECT id_solicitud FROM ret_fondo_ahorro_generico WHERE causal_retiro = ", v_causal, ")"
   END IF

   -- si se recibio medio entrega
   IF ( v_medio_pago IS NOT NULL ) THEN
      IF v_medio_pago = 1 THEN -- Cuenta CLABE
         LET v_sql_filtro = v_sql_filtro, "\n AND a.id_solicitud IN (SELECT id_solicitud FROM ret_pago_spei WHERE consec_beneficiario = 1 )"
      END IF 
      IF v_medio_pago = 2 THEN -- Pago v�a DAP
         LET v_sql_filtro = v_sql_filtro, "\n AND a.id_solicitud IN (SELECT id_solicitud FROM ret_pago_dap WHERE consec_beneficiario = 1 )"
      END IF 
   END IF
   
   LET v_sql = v_sql,v_sql_filtro
   -- si se capturo folio
   IF ( v_folio IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.folio = ", v_folio
   END IF
   
   -- si se recibio estado de la solicitud
   IF ( v_estado IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.estado_solicitud = ", v_estado
   END IF

   IF ( v_fecha_pago_ini IS NOT NULL AND v_fecha_pago_fin IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.folio       IN (SELECT folio                  ",
                         "\n                       FROM   bat_ctr_operacion      ",
                         "\n                       WHERE  proceso_cod  = 1503    ",
                         "\n                       AND    opera_cod  = 2         ",
                         "\n                       AND    estado_cod = 4         ",
                         "\n                       AND    fecha_ini IS NOT NULL  ",
                         "\n                       AND    DATE(fecha_ini)  BETWEEN '", v_fecha_pago_ini, "'",
                         "\n                       AND    '", v_fecha_pago_fin, "')"
   END IF
   

   
   -- se concatena la agrupacion y ordenamiento en turno
   LET v_sql = v_sql, "\n GROUP BY 1,2,3,5,8,9,10,11",
                      "\n ORDER BY 2"

   DISPLAY v_sql
   LET v_indice = 1
   -- se prepara la consulta general acumulada
   PREPARE sid_solicitudes1 FROM v_sql
   DECLARE cur_solicitudes1 CURSOR FOR sid_solicitudes1

   FOREACH cur_solicitudes1 INTO v_r_despliegue.*,v_arr_des_det[v_indice].*
      LET v_arr_despliegue[v_indice].* = v_r_despliegue.*

      --se genera la condici�n para la obteci�n de aivs y pesos
      LET v_sql = "\n WHERE id_solicitud IN (",
                  "\n    SELECT a.id_solicitud",
                  "\n    FROM ret_solicitud_generico a",
                  "\n    WHERE a.folio = ",v_r_despliegue.folio,
                  "\n     AND a.modalidad_retiro = ",v_r_despliegue.modalidad_retiro,
                  "\n     AND a.estado_solicitud = ",v_arr_des_det[v_indice].estado_solicitud,
                  "\n     AND a.cod_rechazo = ",v_arr_des_det[v_indice].cod_rechazo,
                  --"\n     AND a.f_solicitud BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'",
                   v_sql_filtro,")"  
      --se genera la consulta para la obteci�n del tanto normal y el tanto adicional
         LET v_sql = "SELECT sum(saldo_viv72),sum(tanto_adicional)",
                         "\n FROM ret_fondo_ahorro_generico ",v_sql
      --DISPLAY "Consulta Completa >" || v_sql || "<"
      --obteci�n de datos extras, acumulado de aivs y pesos
      PREPARE sid_sumas FROM v_sql
      EXECUTE sid_sumas INTO v_arr_despliegue[v_indice].tanto_normal,v_arr_despliegue[v_indice].tanto_adicional

      LET v_indice = v_indice + 1
   END FOREACH
   
   OPEN WINDOW w_consulta1 WITH FORM "RETC4802"
    LET w = ui.Window.getCurrent()
    LET f = w.getForm()
   
   INPUT ARRAY v_arr_despliegue WITHOUT DEFAULTS
   FROM tbl_solicitudes_1.* ATTRIBUTES ( UNBUFFERED, INSERT ROW = FALSE, DELETE ROW = FALSE, APPEND ROW = FALSE )
   BEFORE INPUT 
    --CALL f.setElementHidden("tbl_solicitudes_1.tanto_normal",1)       
    --CALL f.setElementHidden("tbl_solicitudes_1.tanto_adicional",1)       
    CALL f.setFieldHidden("tanto_normal",1)       
    CALL f.setFieldHidden("tanto_adicional",1)       
    
      ON ACTION accept
      
         LET v_elementos_elegidos = 0
         CALL g_arr_solicitudes_folio.clear()
      
         -- se verifica que al menos haya elegido un renglon
         FOR v_indice = 1 TO v_arr_despliegue.getLength()
            IF ( v_arr_despliegue[v_indice].elegir = 1 ) THEN
               LET v_elementos_elegidos = v_elementos_elegidos + 1
               
               LET g_arr_solicitudes_folio[v_elementos_elegidos].* = v_arr_despliegue[v_indice].* 
               LET g_arr_solicitudes_folio[v_elementos_elegidos].estado_solicitud = v_arr_des_det[v_indice].estado_solicitud
               LET g_arr_solicitudes_folio[v_elementos_elegidos].cod_rechazo      = v_arr_des_det[v_indice].cod_rechazo
            END IF
         END FOR
         
         -- si no se elegio ningun registro no se puede realizar la consulta
         IF ( v_elementos_elegidos < 1 ) THEN
            -- se le indica al usuario que debe elegir al menos un registro
            CALL fn_mensaje("Atenci�n","Es necesario elegir al menos un registro para ejecutar la consulta","stop")
            CONTINUE INPUT
         END IF
         
         -- se invoca la consulta de los elementos elegidos. Los elementos seleccionados estan en el arreglo global
         CALL fn_consulta_solicitudes_nivel2(v_nss, v_caso_adai, v_estado, v_cod_rechazo,
                                             v_id_solicitud,v_folio_restitucion,
                                             v_fecha_inicio, v_fecha_fin, v_fecha_pago_ini,
                                             v_fecha_pago_fin, v_causal, v_medio_pago)

      ON ACTION todos
         FOR v_indice = 1 TO v_arr_despliegue.getLength()
            LET v_arr_despliegue[v_indice].elegir = 1
         END FOR

      ON ACTION ninguno
         FOR v_indice = 1 TO v_arr_despliegue.getLength()
            LET v_arr_despliegue[v_indice].elegir = 0
         END FOR

                                             
      ON ACTION cancel
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta1
   
END FUNCTION




{
======================================================================
Nombre: fn_consulta_solicitudes_nivel2
Fecha creacion: junio 10, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Muestra el detalle de los registros de solicitud elegidos en la consulta de 
solicitudes de primer nivel que agrupa montos por folio y que se encuentran
en el arreglo global

Registro de modificaciones:
Autor           Fecha      Descrip. cambio
Eneas Armas     20140122   se cambia tabla ret_ley73 por ret_ley73_generico
                20140122   se cambia tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
======================================================================
}
FUNCTION fn_consulta_solicitudes_nivel2(v_nss, v_caso_adai, v_estado, v_cod_rechazo,
                                        v_id_solicitud_i,v_folio_restitucion_i,
                                        v_fecha_inicio, v_fecha_fin, v_fecha_pago_ini,
                                        v_fecha_pago_fin, v_causal, v_medio_pago)
DEFINE v_tipo_retiro         LIKE ret_tipo_retiro.tpo_retiro,
       v_etapa               SMALLINT, -- 1 Solicitud, 2 Preliquidacion, 3 Liquidacion
       v_nss                 LIKE afi_derechohabiente.nss,
       v_caso_adai           LIKE ret_solicitud_generico.caso_adai,
       v_folio               LIKE glo_folio.folio,
       v_id_solicitud_i      LIKE ret_solicitud_generico.id_solicitud,
       v_folio_restitucion_i LIKE ret_solicitud_generico.folio_restitucion,
       v_estado              LIKE ret_estado_solicitud.estado_solicitud,
       v_cod_rechazo         SMALLINT, -- codigo de rechazo
       v_causal              SMALLINT, -- causal
       v_medio_pago          SMALLINT, -- medio pago
       v_fecha_inicio        DATE, -- fecha de inicio de consulta
       v_fecha_fin           DATE, -- fecha fin de consulta
       v_fecha_pago_ini      DATE, -- fecha de pago inicial
       v_fecha_pago_fin      DATE, -- fecha de pago final
       v_indice              INTEGER, -- contador       
       v_sql                 STRING, -- cadena con instruccion sql
       v_sqlb                STRING, -- cadena con instruccion sql beneficiarios
       v_sqlr                STRING, -- cadena con instruccion sql repuestas
       v_elementos_elegidos  INTEGER, -- contador de elementos elegidos
       v_c_ruta_env_acr      LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ruta_bin            LIKE seg_modulo.ruta_bin, -- ruta donde estan los ejecutables
       v_nom_archivo                    STRING, -- nombre del archivo de salida
       v_extension_txt                  STRING, -- extension del archivo de salida
       v_archivo_txt                    STRING, -- nombre y extension del archivo con el detalle
       v_mensaje_archivo                STRING, -- mensaje de generacion de archivo
       v_v_ruta_nomarch                 STRING, -- ruta y nombre del archivo de salida
       v_ch_arch_ret_generico           BASE.CHANNEL,  -- manejador de apuntador hacia archivo
       v_conteo                         INTEGER, -- contador de registros
       v_s_detalle                      STRING,
       v_f_consulta                     DATE,
       v_h_consulta                     DATETIME HOUR TO SECOND,
       v_cuenta_pagos                   SMALLINT, 
       v_grupo                          SMALLINT,
       v_fecha_liquida                  DATETIME YEAR TO SECOND ,
       v_cont_va                        INTEGER, 
       
       v_r_despliegue        RECORD
         id_solicitud        LIKE ret_solicitud_generico.id_solicitud    ,
         modalidad_retiro    LIKE ret_solicitud_generico.modalidad_retiro,
         desc_modalidad      VARCHAR(200)                                ,
         nss                 LIKE afi_derechohabiente.nss                ,
         nombre              VARCHAR(120)                                ,
         rfc                 LIKE afi_derechohabiente.rfc                ,
         caso_adai           LIKE ret_solicitud_generico.caso_adai       ,
         folio               LIKE glo_folio.folio                        ,
         f_solicitud         LIKE ret_disposicion.f_solicitud            ,
         aivs                LIKE ret_solo_infonavit.aivs_viv97          ,
         pesos               LIKE ret_solo_infonavit.importe_viv97       ,
         ref_dap             CHAR(18)                                    ,
         estado_solicitud    VARCHAR(100)                                ,
         cod_rechazo         VARCHAR(100)                                ,
         medio_entrega       CHAR(10)                                    ,
         nombre_archivo_e    LIKE ret_ctr_archivo_fico.nombre_archivo    ,
         fecha_archivo_e     LIKE ret_ctr_archivo_fico.f_actualiza       ,
         nombre_archivo_r    LIKE ret_ctr_archivo_fico.nombre_archivo    ,
         fecha_archivo_r     LIKE ret_ctr_archivo_fico.f_actualiza       ,
         nombre_archivo_cc   LIKE ret_ctr_archivo_fico.nombre_archivo    ,
         fecha_archivo_cc    LIKE ret_ctr_archivo_fico.f_actualiza       ,
         nombre_archivo_rc   LIKE ret_ctr_archivo_fico.nombre_archivo    ,
         fecha_archivo_rc    LIKE ret_ctr_archivo_fico.f_actualiza       ,
         id_derechohabiente  LIKE ret_solicitud_generico.id_derechohabiente,
         folio_restitucion   LIKE ret_solicitud_generico.folio_restitucion,
         des_estado          LIKE ret_cat_edo_pago_fico.des_estado       ,
         rsp_referencia      LIKE ret_ws_consulta_pago_fico.rsp_referencia,
         rsp_f_pago          LIKE ret_ws_consulta_pago_fico.rsp_f_pago   ,
         cta_x_pagar         LIKE ret_ws_consulta_pago_fico.documento    ,
         sello               CHAR(64)                                    ,
         f_liquida           DATE                                        
       END RECORD,          
       v_arr_despliegue     DYNAMIC ARRAY OF RECORD
         id_solicitud         LIKE ret_solicitud_generico.id_solicitud    ,
         modalidad_retiro     LIKE ret_solicitud_generico.modalidad_retiro,
         desc_modalidad       VARCHAR(200)                                ,
         causal               CHAR(30),
         nss                  LIKE afi_derechohabiente.nss                ,
         rfc                  LIKE afi_derechohabiente.rfc                ,
         caso_adai            LIKE ret_solicitud_generico.caso_adai       ,
         f_solicitud          LIKE ret_disposicion.f_solicitud            ,
         ---------Se agrego este campo para el requerimiento 863----------
         f_autorizacion       LIKE ret_ws_peticion_marca.f_peticion       ,
         f_liquida            DATE                                        ,
         f_pago               DATE                                        ,
         aivs                LIKE ret_solo_infonavit.aivs_viv97           ,
         pesos               LIKE ret_solo_infonavit.importe_viv97        ,
         total_devolucion     DECIMAL(22,2)                               ,
         -----------------------------------------------------------------
         estado_solicitud     VARCHAR(100)                                ,
         cod_rechazo          VARCHAR(100)                                ,
         medio_entrega        CHAR(10)                                    ,
         cuenta_clabe_ref_dap CHAR(18)
       END RECORD,
       v_arr_despliegue_det     DYNAMIC ARRAY OF RECORD
         modalidad_retiro    LIKE ret_solicitud_generico.modalidad_retiro,
         nombre              VARCHAR(120)                                ,
         rfc                 LIKE afi_derechohabiente.rfc                ,
         folio               LIKE glo_folio.folio                        ,
         nombre_archivo_e    LIKE ret_ctr_archivo_fico.nombre_archivo    ,
         fecha_archivo_e     LIKE ret_ctr_archivo_fico.f_actualiza       ,
         nombre_archivo_r    LIKE ret_ctr_archivo_fico.nombre_archivo    ,
         fecha_archivo_r     LIKE ret_ctr_archivo_fico.f_actualiza       ,
         nombre_archivo_cc   LIKE ret_ctr_archivo_fico.nombre_archivo    ,
         fecha_archivo_cc    LIKE ret_ctr_archivo_fico.f_actualiza       ,
         nombre_archivo_rc   LIKE ret_ctr_archivo_fico.nombre_archivo    ,
         fecha_archivo_rc    LIKE ret_ctr_archivo_fico.f_actualiza       ,
         id_derechohabiente  LIKE ret_solicitud_generico.id_derechohabiente,
         folio_restitucion   LIKE ret_solicitud_generico.folio_restitucion,
         des_estado          LIKE ret_cat_edo_pago_fico.des_estado       ,
         rsp_referencia      LIKE ret_ws_consulta_pago_fico.rsp_referencia,
         rsp_f_pago          LIKE ret_ws_consulta_pago_fico.rsp_f_pago   ,
         cta_x_pagar         LIKE ret_ws_consulta_pago_fico.documento    ,
         sello               CHAR(64)
       END RECORD,
       v_valor_aiv                LIKE ret_cza_disposicion.precio_fondo,
       v_num_registros            INTEGER,
       v_total_tanto_normal       DECIMAL(22,2),
       v_total_tanto_adicional    DECIMAL(22,6),
       v_ed_sello                 CHAR(64),
       v_fecha_valuacion          LIKE ret_cza_disposicion.f_valor_transferencia,          
       v_r_beneficiario     RECORD --arreglo para los beneficiarios de la solicitud
         consec_beneficiario LIKE ret_beneficiario_generico.consec_beneficiario,
         beneficiario        LIKE ret_beneficiario_generico.tpo_beneficiario   ,
         tpo_pago            LIKE ret_beneficiario_generico.tpo_pago           ,
         cod_parentesco      LIKE ret_beneficiario_generico.cod_parentesco     ,
         ap_paterno          LIKE ret_beneficiario_generico.ap_paterno         ,
         ap_materno          LIKE ret_beneficiario_generico.ap_materno         ,
         nombre              LIKE ret_beneficiario_generico.nombre             ,
         telefono            LIKE ret_beneficiario_generico.telefono           ,
         correo              LIKE ret_beneficiario_generico.correo             ,
         porcentaje          LIKE ret_beneficiario_generico.porcentaje         ,
         aivs                LIKE ret_beneficiario_generico.aivs               ,
         importe             LIKE ret_beneficiario_generico.importe
       END RECORD,          
       v_arr_beneficiario  DYNAMIC ARRAY OF RECORD --arreglo para los beneficiarios de la solicitud
         consec_beneficiario LIKE ret_beneficiario_generico.consec_beneficiario,
         beneficiario        LIKE ret_beneficiario_generico.tpo_beneficiario   ,
         tpo_pago            LIKE ret_beneficiario_generico.tpo_pago           ,
         cod_parentesco      LIKE ret_beneficiario_generico.cod_parentesco     ,
         ap_paterno          LIKE ret_beneficiario_generico.ap_paterno         ,
         ap_materno          LIKE ret_beneficiario_generico.ap_materno         ,
         nombre              LIKE ret_beneficiario_generico.nombre             ,
         telefono            LIKE ret_beneficiario_generico.telefono           ,
         correo              LIKE ret_beneficiario_generico.correo             ,
         porcentaje          LIKE ret_beneficiario_generico.porcentaje         ,
         aivs                LIKE ret_beneficiario_generico.aivs               ,
         importe             LIKE ret_beneficiario_generico.importe
       END RECORD,          
       v_r_respuesta    RECORD --arreglo para las repuestas de la solicitud
         acreedor            LIKE ret_respuesta_fico.acreedor                  ,
         cta_clabe           LIKE ret_respuesta_fico.cta_clabe                 ,
         cta_x_pagar         LIKE ret_respuesta_fico.cta_x_pagar               ,
         anho                LIKE ret_respuesta_fico.anho                      ,
         bandera             LIKE ret_respuesta_fico.bandera                   ,
         acreedor_res        LIKE ret_respuesta_fico.acreedor_res              ,
         banco_inter         LIKE ret_respuesta_fico.banco_inter               ,
         des_error           LIKE ret_respuesta_fico.des_error
       END RECORD,
       v_arr_respuesta    DYNAMIC ARRAY OF RECORD --arreglo para las repuestas de la solicitud
         acreedor            LIKE ret_respuesta_fico.acreedor              ,
         cta_clabe           LIKE ret_respuesta_fico.cta_clabe                 ,
         cta_x_pagar         LIKE ret_respuesta_fico.cta_x_pagar               ,
         anho                LIKE ret_respuesta_fico.anho                      ,
         bandera             LIKE ret_respuesta_fico.bandera                   ,
         acreedor_res        LIKE ret_respuesta_fico.acreedor_res              ,
         banco_inter         LIKE ret_respuesta_fico.banco_inter               ,
         des_error           LIKE ret_respuesta_fico.des_error
       END RECORD,
       v_id_solicitud        INT,
       v_rfc_det             LIKE afi_derechohabiente.rfc                 ,
       v_folio_det           LIKE glo_folio.folio                         ,
       v_nombre_archivo_e    LIKE ret_ctr_archivo_fico.nombre_archivo     ,
       v_fecha_archivo_e     LIKE ret_ctr_archivo_fico.f_actualiza        ,
       v_nombre_archivo_r    LIKE ret_ctr_archivo_fico.nombre_archivo     ,
       v_fecha_archivo_r     LIKE ret_ctr_archivo_fico.f_actualiza        ,
       v_nombre_archivo_cc   LIKE ret_ctr_archivo_fico.nombre_archivo     ,
       v_fecha_archivo_cc    LIKE ret_ctr_archivo_fico.f_actualiza        ,
       v_nombre_archivo_rc   LIKE ret_ctr_archivo_fico.nombre_archivo     ,
       v_fecha_archivo_rc    LIKE ret_ctr_archivo_fico.f_actualiza        ,
       v_folio_restitucion   LIKE ret_solicitud_generico.folio_restitucion,
       v_nombre              VARCHAR (120),
       v_des_estado          LIKE ret_cat_edo_pago_fico.des_estado,
       v_rsp_referencia      LIKE ret_ws_consulta_pago_fico.rsp_referencia,
       v_rsp_f_pago          VARCHAR(20),
       v_manejador_rpt       om.SaxDocumentHandler,
       v_fecha_actual        DATE,
       v_usuario_desc        LIKE seg_usuario.usuario_desc
       DEFINE v_sql_nombre   STRING
DEFINE w ui.Window
DEFINE f ui.Form


   -- se inician las variables de acumulacion de cifras
   LET v_num_registros            = 0
   LET v_total_tanto_normal       = 0
   LET v_total_tanto_adicional    = 0
   LET v_arr_despliegue           = NULL
   CALL v_arr_despliegue.clear()
   LET v_arr_despliegue_det       = NULL 
   LET v_cuenta_pagos             = 0

   LET v_sql = "\n SELECT DISTINCT ef.des_estado, rw.rsp_referencia, rw.rsp_f_pago, rw.documento,    ",
               "\n                 rw.f_consulta, rw.h_consulta                                      ",
               "\n FROM   ret_ws_consulta_pago_fico AS rw,                                           ",
               "\n        (SELECT rrr.id_solicitud, MAX(rrr.f_consulta) AS fecha_cons                ",
               "\n         FROM   ret_ws_consulta_pago_fico rrr                                      ",
               "\n         WHERE  rrr.id_solicitud = ?                                               ",
               "\n         GROUP  BY rrr.id_solicitud) AS rw_max,                                    ",
               "\n        ret_cat_edo_pago_fico ef                                                   ",
               "\n WHERE  rw.id_solicitud = rw_max.id_solicitud                                      ",
               "\n AND    rw.f_consulta = rw_max.fecha_cons                                          ",
               "\n AND    rw.rsp_estatus IN (2,3,4,20,21,22,23,24)                                   ",
               "\n AND    rw.rsp_estatus = ef.estado_pago                                            ",
               "\n AND    rw.id_solicitud = ?                                                        ",
               "\n ORDER  BY rw.f_consulta DESC, rw.h_consulta DESC                                  "
   PREPARE sid_pago_fico FROM v_sql

   --DISPLAY "En el segundo nivel"
   -- ====================================================================================================
   -- ====================================================================================================
   -- SOLICITUDES  DEL FONDO DE AHORRO
   -- ====================================================================================================
   -- se obtienen las solicitudes de los folios elegidos y con las condiciones dadas en la captura de consulta  
   LET v_sql = "\n SELECT                                                                   ",
               "\n a.id_solicitud                                                           ,",
               "\n a.modalidad_retiro                                                       ,",
               "\n a.modalidad_retiro || ' - ' || e.des_corta                               ,",
               "\n a.nss                                                                    ,",
               "\n ' '                                                                      ,",
               "\n a.rfc                                                                    ,",
               "\n a.caso_adai                                                              ,",
               "\n a.folio                                                                  ,",
               "\n a.f_solicitud                                                            ,",
               "\nCASE                                                                       ", 
               "\n    WHEN a.estado_solicitud = 8 THEN 0                                     ",
               "\n    ELSE nvl(rsi.aivs_viv97,0)                                             ",
               "\nEND,                                                                       ",
               "\nCASE                                                                       ", 
               "\n    WHEN a.estado_solicitud = 8 THEN 0                                     ",
               "\n    ELSE nvl(rsi.importe_viv97,0)                                          ",
               "\nEND,                                                                       ",
               "\n nvl(rpd.cve_referencia,rps.cuenta_clabe)                                 ,",
               "\n a.estado_solicitud || '-' || es.des_corta                                ,",
               "\n a.cod_rechazo || '-' || cr.des_corta                                     ,",
               "\n rcme.descripcion                                                         ,",
               "\n ae.nombre_archivo                                                        ,",
               "\n ae.f_actualiza                                                           ,",
               "\n ar.nombre_archivo                                                        ,",
               "\n ar.f_actualiza                                                           ,",
               "\n cc.nombre_archivo                                                        ,",
               "\n cc.f_actualiza                                                           ,",
               "\n rc.nombre_archivo                                                        ,",
               "\n rc.f_actualiza                                                           ,",
               "\n a.id_derechohabiente                                                     ,", 
               "\n a.folio_restitucion                                                      ,",
               "\n '','','','',NVL(rsm.sello,'')                                             ",
               #"\n ef.des_estado                                                            ,",
               #"\n p.rsp_referencia                                                         ,",
               #"\n p.rsp_f_pago                                                             ,",
               #"\n p.documento                                                               ", 
               "\n FROM ret_solicitud_generico  a                                            ",
               "\n LEFT OUTER JOIN ret_solo_infonavit rsi                                    ",
               "\n ON a.id_solicitud = rsi.id_solicitud                                      ",
               "\n LEFT OUTER JOIN ret_sol_medio_entrega rsm                                 ",
               "\n ON a.id_solicitud = rsm.id_solicitud                                      ",
               "\n LEFT OUTER JOIN ret_cat_medio_entrega rcme                                ",
               "\n ON rsm.medio_entrega = rcme.medio_entrega                                 ",
               "\n LEFT OUTER JOIN ret_pago_dap rpd                                          ",
               "\n ON a.id_solicitud = rpd.id_solicitud                                      ",
               "\n LEFT OUTER JOIN ret_pago_spei rps                                          ",
               "\n ON a.id_solicitud = rps.id_solicitud                                      ",
               "\n LEFT OUTER JOIN ret_ctr_archivo_fico ae                                   ",
               "\n ON a.id_archivo_envio = ae.id_archivo                                     ",
               "\n LEFT OUTER JOIN ret_ctr_archivo_fico ar                                   ",
               "\n ON a.id_archivo_respuesta = ar.id_archivo                                 ",
               "\n LEFT OUTER JOIN ret_ctr_archivo_fico cc                                   ",
               "\n ON a.id_archivo_cancela_cxp =cc.id_archivo                                ",
               "\n LEFT OUTER JOIN ret_ctr_archivo_fico rc                                   ",
               "\n ON a.id_archivo_resp_cxp  = rc.id_archivo,                                ",
               --
               #"\n LEFT OUTER JOIN (SELECT rw.id_solicitud, rw.f_consulta,                   ", 
               #"\n                         rw.rsp_referencia, rw.rsp_f_pago,                 ",
               #"\n                         rw.rsp_estatus,                                   ",
               #"\n                         rw.documento                                      ",
               #"\n                    FROM ret_ws_consulta_pago_fico AS rw,                  ",
               #"\n                         (SELECT id_solicitud, MAX(f_consulta)             ",
               #"\n                                 AS fecha_cons                             ",
               #"\n                            FROM ret_ws_consulta_pago_fico                 ",
               #"\n                           GROUP BY id_solicitud) AS rw_max                ",
               #"\n                   WHERE rw.id_solicitud = rw_max.id_solicitud             ",
               #"\n                     AND rw.f_consulta = rw_max.fecha_cons)p               ",
               #"\n ON a.id_solicitud = p.id_solicitud                                        ",
               #"\n AND p.rsp_estatus IN (2,3,4,20,21,22,23,24)                               ",
               --estos estados se agregaron para evitar que se duplique informaci�n con estados no validos
               #"\n LEFT OUTER JOIN ret_cat_edo_pago_fico ef                                  ",
               #"\n ON p.rsp_estatus = ef.estado_pago ,                                       ",
               "\n ret_modalidad_retiro      e,                                              ",
               "\n ret_estado_solicitud      es,                                             ",
               "\n ret_rechazo_generico       cr                                             ",    
               "\n WHERE 1 = 1                                                               ",
               "\n AND   a.modalidad_retiro = e.modalidad_retiro                             ",
               "\n AND   a.estado_solicitud = es.estado_solicitud                            ",
               "\n AND   a.cod_rechazo      = cr.cod_rechazo                                 "

   -- si se recibio nss 
   IF v_fecha_inicio IS NOT NULL AND v_fecha_fin IS NOT NULL THEN 
      LET v_sql = v_sql, "\n AND a.f_solicitud BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'"
   END IF 
   IF ( v_nss IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.nss = '", v_nss, "'"
   END IF

   -- si se capturo numero de solicitud
   IF ( v_id_solicitud_i IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.id_solicitud = '", v_id_solicitud_i, "'"
   END IF

   -- si se capturo folio de restituci�n
   IF ( v_folio_restitucion_i IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.folio_restitucion = ", v_folio_restitucion_i
   END IF

   -- si se recibio grupo
   IF ( v_causal IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.id_solicitud IN (SELECT id_solicitud FROM ret_fondo_ahorro_generico WHERE causal_retiro = ", v_causal, ")"
   END IF

   -- si se recibio medio entrega
   IF ( v_medio_pago IS NOT NULL ) THEN
      IF v_medio_pago = 1 THEN -- Cuenta CLABE
         LET v_sql = v_sql, "\n AND a.id_solicitud IN (SELECT id_solicitud FROM ret_pago_spei WHERE consec_beneficiario = 1 )"
      END IF 
      IF v_medio_pago = 2 THEN -- Pago v�a DAP
         LET v_sql = v_sql, "\n AND a.id_solicitud IN (SELECT id_solicitud FROM ret_pago_dap WHERE consec_beneficiario = 1 )"
      END IF 
   END IF
   
   -- si se recibio caso adai
   IF ( v_caso_adai IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.caso_adai = ", v_caso_adai
   END IF


   LET v_sql = v_sql , "\n AND ("
   -- genera cadana de acuerdo a datos seleccionados
   FOR v_indice = 1 TO g_arr_solicitudes_folio.getLength()
      LET v_sql = v_sql || "\n (a.folio = " , g_arr_solicitudes_folio[v_indice].folio CLIPPED
                        || " AND a.modalidad_retiro = " , g_arr_solicitudes_folio[v_indice].modalidad_retiro CLIPPED
                        || " AND a.estado_solicitud = " , g_arr_solicitudes_folio[v_indice].estado_solicitud CLIPPED
                        || " AND a.cod_rechazo      = " , g_arr_solicitudes_folio[v_indice].cod_rechazo CLIPPED
                        || " )"
      -- si es el ultimo elemento se cierra el parentesis
      IF ( v_indice <> g_arr_solicitudes_folio.getLength() ) THEN
         LET v_sql = v_sql, " OR "
      END IF
   END FOR
   LET v_sql = v_sql, ")"

   --DISPLAY "Buscando solicitudes por tipo de retiro elegido:"
   DISPLAY v_sql
   LET v_indice = 1
   -- se transfieren los datos al arreglo de despligue
   PREPARE sid_solicitudesdet FROM v_sql
   DECLARE cur_solicitudesdet CURSOR FOR sid_solicitudesdet
   FOREACH cur_solicitudesdet INTO v_r_despliegue.*
      DISPLAY v_indice

      DECLARE cur_pago_fico CURSOR FOR sid_pago_fico 
      FOREACH cur_pago_fico USING v_r_despliegue.id_solicitud, v_r_despliegue.id_solicitud 
                             INTO v_r_despliegue.des_estado, v_r_despliegue.rsp_referencia,
                                  v_r_despliegue.rsp_f_pago, v_r_despliegue.cta_x_pagar,
                                  v_f_consulta, v_h_consulta
         LET v_cuenta_pagos = v_cuenta_pagos + 1
         IF v_cuenta_pagos >= 1 THEN 
            EXIT FOREACH 
         END IF 
      END FOREACH 
      CLOSE cur_pago_fico
      IF v_r_despliegue.medio_entrega = 'TABLETA' OR 
         v_r_despliegue.medio_entrega = 'DEV AUTO' THEN
         LET v_arr_despliegue[v_indice].f_autorizacion = v_r_despliegue.f_solicitud
      ELSE 
         --Se obtiene la fecha de actualizacion
         SELECT MAX(f_peticion)
         INTO v_arr_despliegue[v_indice].f_autorizacion
         FROM ret_ws_peticion_marca rp, ret_ws_det_peticion_marca rd
         WHERE rp.id_peticion = rd.id_peticion 
         AND ind_marca = 3 
         AND caso_adai = v_r_despliegue.caso_adai
      END IF 

      DISPLAY " La fecha de Autorizaci�n: ", v_arr_despliegue[v_indice].f_autorizacion
      -- Se obtiene la cuenta CLABE
      SELECT NVL(b.cuenta_clabe,c.cve_referencia)
      INTO   v_arr_despliegue[v_indice].cuenta_clabe_ref_dap
      FROM   ret_solicitud_generico a
             LEFT OUTER JOIN ret_pago_spei b
                          ON a.id_solicitud = b.id_solicitud
                         AND b.consec_beneficiario = 1
             LEFT OUTER JOIN ret_pago_dap c
                          ON a.id_solicitud = c.id_solicitud
                         AND c.consec_beneficiario = 1
      WHERE  a.id_solicitud = v_r_despliegue.id_solicitud

      --datos que se musetran en la tabla
      LET v_arr_despliegue[v_indice].id_solicitud         = v_r_despliegue.id_solicitud
      LET v_arr_despliegue[v_indice].modalidad_retiro     = v_r_despliegue.modalidad_retiro
      LET v_arr_despliegue[v_indice].desc_modalidad       = v_r_despliegue.desc_modalidad
      LET v_arr_despliegue[v_indice].nss                  = v_r_despliegue.nss
      LET v_arr_despliegue[v_indice].rfc                  = v_r_despliegue.rfc
      LET v_arr_despliegue[v_indice].caso_adai            = v_r_despliegue.caso_adai
      LET v_arr_despliegue[v_indice].f_solicitud          = v_r_despliegue.f_solicitud
      LET v_arr_despliegue[v_indice].f_liquida            = v_r_despliegue.f_liquida
      LET v_arr_despliegue[v_indice].f_pago               = fn_fecha(v_r_despliegue.rsp_f_pago)
      LET v_arr_despliegue[v_indice].aivs                 = v_r_despliegue.aivs
      LET v_arr_despliegue[v_indice].pesos                = v_r_despliegue.pesos
      LET v_arr_despliegue[v_indice].total_devolucion     = v_r_despliegue.pesos
      LET v_arr_despliegue[v_indice].estado_solicitud     = v_r_despliegue.estado_solicitud
      LET v_arr_despliegue[v_indice].cod_rechazo          = v_r_despliegue.cod_rechazo
      LET v_arr_despliegue[v_indice].medio_entrega        = v_r_despliegue.medio_entrega
      LET v_arr_despliegue[v_indice].cuenta_clabe_ref_dap = v_r_despliegue.ref_dap

      --datos que NO se muestran en la tabla pero se ven el detalle
      LET v_arr_despliegue_det[v_indice].modalidad_retiro   = v_r_despliegue.modalidad_retiro
      LET v_arr_despliegue_det[v_indice].nombre             = v_r_despliegue.nombre
      LET v_arr_despliegue_det[v_indice].rfc                = v_r_despliegue.rfc
      LET v_arr_despliegue_det[v_indice].folio              = v_r_despliegue.folio
      LET v_arr_despliegue_det[v_indice].nombre_archivo_e   = v_r_despliegue.nombre_archivo_e
      LET v_arr_despliegue_det[v_indice].fecha_archivo_e    = v_r_despliegue.fecha_archivo_e
      LET v_arr_despliegue_det[v_indice].nombre_archivo_r   = v_r_despliegue.nombre_archivo_r
      LET v_arr_despliegue_det[v_indice].fecha_archivo_r    = v_r_despliegue.fecha_archivo_r
      LET v_arr_despliegue_det[v_indice].nombre_archivo_cc  = v_r_despliegue.nombre_archivo_cc
      LET v_arr_despliegue_det[v_indice].fecha_archivo_cc   = v_r_despliegue.fecha_archivo_cc
      LET v_arr_despliegue_det[v_indice].nombre_archivo_rc  = v_r_despliegue.nombre_archivo_rc
      LET v_arr_despliegue_det[v_indice].fecha_archivo_rc   = v_r_despliegue.fecha_archivo_rc
      LET v_arr_despliegue_det[v_indice].id_derechohabiente = v_r_despliegue.id_derechohabiente
      LET v_arr_despliegue_det[v_indice].folio_restitucion  = v_r_despliegue.folio_restitucion
      LET v_arr_despliegue_det[v_indice].des_estado         = v_r_despliegue.des_estado
      LET v_arr_despliegue_det[v_indice].rsp_referencia     = v_r_despliegue.rsp_referencia
      LET v_arr_despliegue_det[v_indice].rsp_f_pago         = v_r_despliegue.rsp_f_pago
      LET v_arr_despliegue_det[v_indice].cta_x_pagar        = v_r_despliegue.cta_x_pagar
      LET v_arr_despliegue_det[v_indice].sello              = v_r_despliegue.sello

      DISPLAY "El resultado de la consulta en ws_consulta_pago_fico"
      DISPLAY " Estado :", v_r_despliegue.des_estado
      DISPLAY " Referencia:", v_r_despliegue.rsp_referencia
      DISPLAY " Fecha Pago:", v_r_despliegue.rsp_f_pago
      DISPLAY " Cta x pagar:", v_r_despliegue.cta_x_pagar

      --obteci�n de datos extras, nombre de la persona
      ## Se dejo el query de esta manera, porque de la forma anterior consumia mucho tiempo de ejecuci�n,
      ## y se pidi� revisar porque tardaban las consultas.
      
      SELECT TRIM(ap_paterno_af) || ' ' || TRIM(ap_materno_af)|| ' ' || TRIM(nombre_af)
      INTO v_arr_despliegue_det[v_indice].nombre
      FROM afi_derechohabiente
      WHERE id_derechohabiente = v_r_despliegue.id_derechohabiente

      ##########################################################################

--      IF v_arr_despliegue[v_indice].f_autorizacion IS NULL THEN
--          LET v_arr_despliegue[v_indice].f_autorizacion = v_arr_despliegue[v_indice].f_solicitud
--      END IF 
      --DISPLAY v_arr_despliegue[v_indice].f_inicio
      ##########################################################################

      --DISPLAY "obteci�n de datos extras de acuerdo a laa tabla que depende de la modalidad"
      --obteci�n de datos extras de acuerdo a laa tabla que depende de la modalidad

      {
      --- Se buscan los montos liquidados, si no se encuentran se presentan los de la solicitud de ret_ley73_generico
      SELECT SUM(importe) * (-1) --- Se busca el monto del tanto normal
      INTO   v_arr_despliegue[v_indice].tanto_normal
      FROM   ret_preliquida72
      WHERE  folio_liquida = v_arr_despliegue_det[v_indice].folio
      AND    subcuenta = 40
      AND    id_referencia = v_arr_despliegue[v_indice].id_solicitud
      AND    movimiento = 182
      IF v_arr_despliegue[v_indice].tanto_normal IS NULL THEN 
         LET v_arr_despliegue[v_indice].tanto_normal = 0
      END IF 
      SELECT SUM(importe) * (-1)  --- Se busca el monto del tanto adicional
      INTO   v_arr_despliegue[v_indice].tanto_adicional
      FROM   ret_preliquida72
      WHERE  folio_liquida = v_arr_despliegue_det[v_indice].folio
      AND    subcuenta = 40
      AND    id_referencia = v_arr_despliegue[v_indice].id_solicitud
      AND    movimiento = 422  
      IF v_arr_despliegue[v_indice].tanto_adicional IS NULL THEN 
         LET v_arr_despliegue[v_indice].tanto_adicional = 0
      END IF 
      IF ((v_arr_despliegue[v_indice].tanto_normal + v_arr_despliegue[v_indice].tanto_adicional ) = 0) THEN
         
         SELECT saldo_viv72, tanto_adicional
         INTO v_arr_despliegue[v_indice].tanto_normal, v_arr_despliegue[v_indice].tanto_adicional
         FROM ret_fondo_ahorro_generico
         WHERE id_solicitud = v_arr_despliegue[v_indice].id_solicitud
      END IF 
      }
      --- Se busca la fecha de liquidaci�n y el grupo
      LET v_arr_despliegue[v_indice].total_devolucion = v_arr_despliegue[v_indice].pesos
      
      LET v_causal = 0
      LET v_fecha_liquida = NULL
      SELECT f_actualiza 
      INTO   v_fecha_liquida
      FROM   glo_folio
      WHERE  folio = v_arr_despliegue_det[v_indice].folio

      SELECT causal_retiro
      INTO   v_causal
      FROM   ret_fondo_ahorro_generico
      WHERE  id_solicitud = v_arr_despliegue[v_indice].id_solicitud
      
      LET v_arr_despliegue[v_indice].causal = ""
      IF v_causal IS NOT NULL THEN
         CASE v_causal 
            WHEN 1 
               LET v_arr_despliegue[v_indice].causal = "1-T�rmino Relaci�n Laboral"
            WHEN 2 
               LET v_arr_despliegue[v_indice].causal = "2-Pensi�n IMSS"
            WHEN 3 
               LET v_arr_despliegue[v_indice].causal = "3-Plan Privado de Pensi�n"
            WHEN 4 
               LET v_arr_despliegue[v_indice].causal = "4-Defunci�n"
            OTHERWISE 
               LET v_arr_despliegue[v_indice].causal = ""
         END CASE 
      END IF 
      LET v_arr_despliegue[v_indice].f_liquida = NULL 
      IF v_fecha_liquida IS NOT NULL THEN 
         LET v_arr_despliegue[v_indice].f_liquida = v_fecha_liquida
      END IF 
       
      LET v_num_registros          = v_num_registros          + 1
      LET v_total_tanto_normal     = v_total_tanto_normal     + v_arr_despliegue[v_indice].aivs
      LET v_total_tanto_adicional  = v_total_tanto_adicional  + v_arr_despliegue[v_indice].pesos
      
      LET v_indice = v_indice + 1
   END FOREACH

   -- se abre la ventana de consulta detallada
   OPEN WINDOW w_consultadetallada WITH FORM "RETC4903"
    LET w = ui.Window.getCurrent()
    LET f = w.getForm()

   DIALOG ATTRIBUTE (UNBUFFERED)
   
   DISPLAY ARRAY v_arr_despliegue TO tbl_despliegue.*
   BEFORE DISPLAY  
    CALL f.setFieldHidden("tanto_adicional",1)       
    CALL f.setFieldHidden("causal",1)       
   	
      BEFORE ROW

      -- obtiene la informacion de detalle
      LET v_id_solicitud      = v_arr_despliegue[ARR_CURR( )].id_solicitud
      LET v_rfc_det           = v_arr_despliegue_det[ARR_CURR( )].rfc
      LET v_folio_det         = v_arr_despliegue_det[ARR_CURR( )].folio
      LET v_nombre_archivo_e  = v_arr_despliegue_det[ARR_CURR( )].nombre_archivo_e
      LET v_fecha_archivo_e   = v_arr_despliegue_det[ARR_CURR( )].fecha_archivo_e
      LET v_nombre_archivo_r  = v_arr_despliegue_det[ARR_CURR( )].nombre_archivo_r
      LET v_fecha_archivo_r   = v_arr_despliegue_det[ARR_CURR( )].fecha_archivo_r
      LET v_nombre_archivo_cc = v_arr_despliegue_det[ARR_CURR( )].nombre_archivo_cc
      LET v_fecha_archivo_cc  = v_arr_despliegue_det[ARR_CURR( )].fecha_archivo_cc
      LET v_nombre_archivo_rc = v_arr_despliegue_det[ARR_CURR( )].nombre_archivo_rc
      LET v_fecha_archivo_rc  = v_arr_despliegue_det[ARR_CURR( )].fecha_archivo_rc
      LET v_folio_restitucion = v_arr_despliegue_det[ARR_CURR( )].folio_restitucion
      LET v_nombre            = v_arr_despliegue_det[ARR_CURR( )].nombre
      LET v_des_estado        = v_arr_despliegue_det[ARR_CURR( )].des_estado
      LET v_rsp_referencia    = v_arr_despliegue_det[ARR_CURR( )].rsp_referencia
      LET v_ed_sello          = v_arr_despliegue_det[ARR_CURR( )].sello
      LET v_rsp_f_pago        = fn_fecha(v_arr_despliegue_det[ARR_CURR( )].rsp_f_pago)

      -- despliega la informacion de detalle
      DISPLAY BY NAME v_num_registros, v_total_tanto_normal,v_total_tanto_adicional
        ,v_rfc_det,v_folio_det , v_folio_restitucion,v_nombre
        ,v_nombre_archivo_e , v_fecha_archivo_e  ,v_nombre_archivo_r , v_fecha_archivo_r  
        ,v_nombre_archivo_cc, v_fecha_archivo_cc, v_nombre_archivo_rc, v_fecha_archivo_rc
        ,v_des_estado,v_rsp_referencia,v_rsp_f_pago,v_ed_sello

        --DISPLAY "Busca a los beneficiarios"
        
      --obtienen los beneficiarios
      LET v_sqlb ="\n SELECT                            ",
                  "\n a.consec_beneficiario            ,",
                  "\n a.tpo_beneficiario               ,",
                  "\n a.tpo_pago                       ,",
                  "\n a.cod_parentesco                 ,",
                  "\n a.ap_paterno                     ,",
                  "\n a.ap_materno                     ,",
                  "\n a.nombre                         ,",
                  "\n a.telefono                       ,",
                  "\n a.correo                         ,",
                  "\n a.porcentaje                     ,",
                  "\n a.aivs                           ,",
                  "\n a.importe                         ",
                  "\n FROM ret_beneficiario_generico  a ",
                  "\n WHERE a.id_solicitud =", v_id_solicitud -- beneficiarios de la solicitud

      PREPARE sid_benefiriariosdet FROM v_sqlb
      DECLARE cur_benefiriariosdet CURSOR FOR sid_benefiriariosdet
   
      -- se transfieren los datos al arreglo de beneficiarios
      LET v_indice = 1
      FOREACH cur_benefiriariosdet INTO v_r_beneficiario.*
         LET v_arr_beneficiario[v_indice].* = v_r_beneficiario.*
         LET v_indice = v_indice + 1
      END FOREACH

    --DISPLAY "Busca la respuesta FICO"
      --obtine las respuestas fico
      LET v_sqlr ="\n SELECT                     ",
                  "\n a.acreedor_res            ,",
                  "\n a.cta_clabe               ,",
                  "\n a.cta_x_pagar             ,",
                  "\n a.anho                    ,",
                  "\n a.bandera                 ,",
                  "\n a.acreedor_res            ,",
                  "\n a.banco_inter             ,",
                  "\n a.des_error                ",
                  "\n FROM ret_respuesta_fico  a ",
                  "\n WHERE a.referencia = ", v_id_solicitud -- beneficiarios de la solicitud

      PREPARE sid_respuestadet FROM v_sqlr
      DECLARE cur_respuestadet CURSOR FOR sid_respuestadet
   
      -- se transfieren los datos al arreglo de respuestas fico
      LET v_indice = 1
      FOREACH cur_respuestadet INTO v_r_respuesta.*
         LET v_arr_respuesta[v_indice].* = v_r_respuesta.*
         LET v_indice = v_indice + 1
      END FOREACH

   END DISPLAY

   DISPLAY ARRAY v_arr_beneficiario TO tblBeneficiario.*
   END DISPLAY

   DISPLAY ARRAY v_arr_respuesta TO tblrespuesta.*
   END DISPLAY
   
      BEFORE DIALOG 

      ON ACTION CANCEL
         EXIT DIALOG

      ON ACTION reporte
         IF (fgl_report_loadCurrentSettings("RETC480.4rp")) THEN
            CALL fgl_report_selectDevice("PDF")
            CALL fgl_report_selectPreview(TRUE)
            LET v_manejador_rpt = fgl_report_commitCurrentSettings()
         ELSE
            CALL fn_mensaje("AVISO","No se puede generar reporte","information")
            CONTINUE DIALOG
         END IF
         SELECT usuario_desc
           INTO v_usuario_desc
           FROM seg_usuario
          WHERE usuario_cod = p_usuario_cod
         LET v_fecha_actual = TODAY
         
         START REPORT rtp_solicitudes_encontradas TO XML HANDLER v_manejador_rpt
         FOR v_indice = 1 TO v_arr_despliegue.getLength()
            OUTPUT TO REPORT rtp_solicitudes_encontradas(p_usuario_cod,
                                                            v_usuario_desc,
                                                            v_fecha_actual,
                                                            v_fecha_inicio,
                                                            v_fecha_fin,
                                                            v_arr_despliegue[v_indice].*)

         END FOR
         FINISH REPORT rtp_solicitudes_encontradas
      ON ACTION Exportar
           -- se obtiene la ruta de envio y ejecutable
           SELECT ruta_envio, ruta_bin
           INTO   v_c_ruta_env_acr, v_ruta_bin
           FROM   seg_modulo
           WHERE  modulo_cod = "ret"

           -- las extensiones del archivo son TXT para el detalle y KEY para el hash
           LET v_extension_txt = ".TXT"

           -- los nombres son todo en mayusculas con la siguiente mascara
           -- SG_USUARIO_AAAAMMDD.TXT
           LET v_nom_archivo = "FA_",p_usuario_cod CLIPPED, "_", TODAY USING "yyyymmdd"
           LET v_archivo_txt = v_nom_archivo, v_extension_txt
           
           -- el archivo con ruta destino que contiene el detalle
           LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED , "/", v_archivo_txt
           LET v_mensaje_archivo = "Se generara el archivo:", v_v_ruta_nomarch
           CALL fn_mensaje("Atenci�n", v_mensaje_archivo, "information")
           -- nombre de archivo generado
           DISPLAY "~~~~~~~~~~~"
           DISPLAY "Archivo generado: ", v_v_ruta_nomarch

           -- se crea el manejador de archivo
           LET v_ch_arch_ret_generico = base.Channel.create()
           CALL v_ch_arch_ret_generico.setDelimiter(NULL)
           
           -- se crea archivo y se indica que se escribira en el mismo
           CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )

           -- se inicia el contador de registros
           LET v_conteo = 0
           LET v_s_detalle = "N�MERO DE SOLICITUD|MODALIDAD RETIRO|GRUPO|NSS|CASO CRM|FECHA SOLICITUD|FECHA AUTORIZACION|",
                              "FECHA LIQUIDACION|FECHA PAGO|TANTO NORMAL|TANTO ADICIONAL|TOTAL DEVOLUCI�N|ESTADO SOLICITUD|",
                              "CODIGO RECHAZO|MEDIO ENTREGA|CUENTA CLABE/REF DAP|"
           CALL v_ch_arch_ret_generico.write(v_s_detalle)
           
           FOR v_conteo = 1 TO v_arr_despliegue.getLength()
               LET v_s_detalle = v_arr_despliegue[v_conteo].id_solicitud                           ,"|",
                                 v_arr_despliegue[v_conteo].desc_modalidad                         ,"|",
                                 v_arr_despliegue[v_conteo].causal                                 ,"|",
                                 v_arr_despliegue[v_conteo].nss                                    ,"|",
                                 v_arr_despliegue[v_conteo].caso_adai                              ,"|",
                                 v_arr_despliegue[v_conteo].f_solicitud  USING "yyyymmdd"          ,"|",
                                 v_arr_despliegue[v_conteo].f_autorizacion  USING "yyyymmdd"       ,"|",
                                 v_arr_despliegue[v_conteo].f_liquida  USING "yyyymmdd"            ,"|",
                                 v_arr_despliegue[v_conteo].f_pago USING "yyyymmdd"                ,"|",
                                 v_arr_despliegue[v_conteo].aivs USING "&&&&&&&&&.&&"              ,"|",
                                 v_arr_despliegue[v_conteo].pesos USING "&&&&&&&&&.&&"             ,"|",
                                 v_arr_despliegue[v_conteo].total_devolucion USING "&&&&&&&&&.&&"  ,"|",
                                 v_arr_despliegue[v_conteo].estado_solicitud                       ,"|",
                                 v_arr_despliegue[v_conteo].cod_rechazo                            ,"|",
                                 v_arr_despliegue[v_conteo].medio_entrega                          ,"|",
                                 v_arr_despliegue[v_conteo].cuenta_clabe_ref_dap                   ,"|"

               CALL v_ch_arch_ret_generico.write(v_s_detalle)

           END FOR

           -- se cierra el archivo
           CALL v_ch_arch_ret_generico.close()
           LET v_mensaje_archivo = "Archivo generado exitosamente:", v_v_ruta_nomarch
           CALL fn_mensaje("Atenci�n", v_mensaje_archivo, "information")
   END DIALOG
   
   CLOSE WINDOW w_consultadetallada 
       
END FUNCTION

{===============================================================================
Clave: 
Nombre: rtp_solicitudes_encontradas
Fecha creacion: Noviembre 21, 2013
Autor: 
Narrativa del proceso que realiza:
 Reporte de solicitudes encontradas

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
REPORT rtp_solicitudes_encontradas(p_usuario_cod,p_usuario_desc,p_fecha_actual,p_fecha_inicio,p_fecha_fin,p_despliegue)
DEFINE p_usuario_cod              LIKE seg_usuario.usuario_cod,
       p_usuario_desc             LIKE seg_usuario.usuario_desc,
       p_fecha_actual             DATE,
       p_fecha_inicio             DATE,
       p_fecha_fin                DATE,
       p_despliegue     RECORD
         id_solicitud             LIKE ret_solicitud_generico.id_solicitud,
         modalidad_retiro         LIKE ret_solicitud_generico.modalidad_retiro,
         desc_modalidad           VARCHAR(200),
         grupo                    CHAR(30),
         nss                      LIKE afi_derechohabiente.nss,
         rfc                      LIKE afi_derechohabiente.rfc,
         caso_adai                LIKE ret_solicitud_generico.caso_adai,
         f_solicitud              LIKE ret_disposicion.f_solicitud,
         f_autorizacion           DATE,
         f_liquida                DATE,
         f_pago                   DATE                                        ,
         tanto_normal             DECIMAL(22,2),
         tanto_adicional          DECIMAL(22,2),
         total_devolucion         DECIMAL(22,2),
         estado_solicitud         VARCHAR(19),
         cod_rechazo              VARCHAR(14),
         medio_entrega            CHAR(10),
         cuenta_clabe_ref_dap     CHAR(18)
       END RECORD,
       v_num_pagina       SMALLINT


       
       
   ORDER BY p_despliegue.modalidad_retiro
   
   FORMAT
      
      FIRST PAGE HEADER
         
         PRINTX p_usuario_cod,
                p_usuario_desc,
                p_fecha_actual USING "dd-mm-yyyy",
                p_fecha_inicio USING "dd-mm-yyyy",
                p_fecha_fin    USING "dd-mm-yyyy"

      PAGE HEADER
         PRINTX p_usuario_cod,
                p_usuario_desc,
                p_fecha_actual USING "dd-mm-yyyy",
                p_fecha_inicio USING "dd-mm-yyyy",
                p_fecha_fin    USING "dd-mm-yyyy"

      BEFORE GROUP OF p_despliegue.modalidad_retiro
         PRINTX p_despliegue.desc_modalidad

      ON EVERY ROW
         PRINTX p_despliegue.id_solicitud,
                p_despliegue.desc_modalidad,
                p_despliegue.grupo,
                p_despliegue.nss,
                p_despliegue.rfc,
                p_despliegue.caso_adai,
                p_despliegue.f_solicitud USING "dd-mm-yyyy",
                p_despliegue.f_autorizacion USING "dd-mm-yyyy",
                p_despliegue.f_liquida USING "dd-mm-yyyy",
                p_despliegue.f_pago USING "dd-mm-yyyy",
                p_despliegue.tanto_normal ,
                p_despliegue.tanto_adicional,
                p_despliegue.total_devolucion,
                p_despliegue.estado_solicitud,
                p_despliegue.cod_rechazo,
                p_despliegue.medio_entrega,
                p_despliegue.cuenta_clabe_ref_dap
               
      PAGE TRAILER
         LET v_num_pagina = PAGENO
      

END REPORT

#Da formato a una fecha, el campo original es una cadena en fomato internacional aaaammdd y se regresa en formato M�xico dd-mm-aaaa
FUNCTION fn_fecha(p_fecha_text)
   DEFINE p_fecha_text VARCHAR(8)
      ,v_fecha varchar(20)
   IF LENGTH(p_fecha_text) > 0 THEN
      LET v_fecha = p_fecha_text[7,8],"-",p_fecha_text[5,6],"-",p_fecha_text[1,4]
   ELSE
      LET v_fecha = ""
   END IF
   RETURN v_fecha
END FUNCTION

PRIVATE  FUNCTION fn_genera_reporte(p_id_solicitud)

DEFINE p_id_solicitud  LIKE ret_solicitud_generico.id_solicitud

DEFINE p_arr_detalle RECORD
           nss                LIKE afi_derechohabiente.nss,
           curp               LIKE afi_derechohabiente.curp,
           tramite            CHAR(50),     -- Descripci�n del Tr�mite
	        grupo              CHAR(55),
           apePaterno         CHAR(40),     -- Apellido paterno
           apeMaterno         CHAR(40),     -- Apellido materno
           nombre             CHAR(40),     -- Nombre
           medioSolicitud     CHAR(10),     -- Medio por el cual se hizo la solicitud 
           conRetiro          LIKE ret_solicitud_generico.id_solicitud,
           pesosViv92         CHAR(18), -- Vivienda 92
           pesosViv97         CHAR(18), -- Vivienda 97
           pesosTotal         CHAR(18), -- Suma en pesos total devuelto
           fTramite           DATETIME YEAR TO MINUTE,     -- Fecha de la Solicitud
           sello              CHAR(64), -- Acuse Generado
           archivo            BYTE,
           estadoConsulta     SMALLINT,     -- Resultado de la Consulta
           codRechazo         SMALLINT,      -- C�digo de rechazo
           caso_crm           CHAR(10)    -- Caso CRM
END RECORD 
    DEFINE reporte          om.SaxDocumentHandler
    DEFINE i                SMALLINT
    DEFINE v_reporte        STRING
    DEFINE v_ruta_listados  CHAR(40)
    DEFINE v_ruta_reporte   STRING
    DEFINE f_inicio         DATE
    DEFINE f_fin            DATE
    DEFINE v_query          STRING 
    DEFINE v_nombre         CHAR(120)
    DEFINE v_rfc            CHAR(13)
    DEFINE v_curp           CHAR(18)
    DEFINE v_nombre_stg     STRING 
    DEFINE v_fecha_paso     CHAR(16)
    DEFINE v_fecha          CHAR(16)
    DEFINE v_pesos_viv92    DECIMAL(22,2)
    DEFINE v_pesos_viv97    DECIMAL(22,2)
    DEFINE v_pesos_total    DECIMAL(22,2)
    DEFINE v_aviso          CHAR(255)
    

    

    LET v_reporte= "RETC463_a.4rp"
    LET p_arr_detalle.pesosViv92 = ""
    LET p_arr_detalle.pesosViv97 = ""
    LET p_arr_detalle.pesosTotal = ""
 
    SELECT a.nss, b.curp, 'Devoluci�n del saldo de la subcuenta de vivienda' as tramite,
       'Uno (pensionados posteriores al 13 de enero de 2012)' as grupo, b.ap_paterno_af,
       b.ap_materno_af, b.nombre_af, 'Internet' as mediosolicitud, a.id_solicitud,
       c.importe_viv92, c.importe_viv97, c.importe_viv92 + c.importe_viv97, d.f_registro,
       d.sello, a.caso_adai
    INTO p_arr_detalle.nss,
         p_arr_detalle.curp,
         p_arr_detalle.tramite,
	      p_arr_detalle.grupo,
         p_arr_detalle.apePaterno,
         p_arr_detalle.apeMaterno,
         p_arr_detalle.nombre,
         p_arr_detalle.medioSolicitud,
         p_arr_detalle.conRetiro,
         v_pesos_viv92,
         v_pesos_viv97,
         v_pesos_total,
         p_arr_detalle.fTramite,
         p_arr_detalle.sello,
         p_arr_detalle.caso_crm
    FROM   ret_solicitud_generico a,
       afi_derechohabiente b,
       ret_ley73_generico c,
       ret_sol_medio_entrega d
    WHERE  a.id_solicitud = p_id_solicitud
    AND    a.id_solicitud = c.id_solicitud
    AND    a.nss = b.nss
    AND    a.id_solicitud = d.id_solicitud

   SELECT aviso
   INTO   v_aviso
   FROM   ret_sol_aviso_pdf 
   WHERE  id_solicitud = p_arr_detalle.conRetiro

    LET v_fecha_paso = p_arr_detalle.fTramite
    LET v_fecha      = v_fecha_paso[9,10],"/",v_fecha_paso[6,7],"/",v_fecha_paso[1,4], " ", v_fecha_paso[12,16]
    LET p_arr_detalle.pesosViv92 = v_pesos_viv92 USING "$$,$$$,$$&.&&"
    LET p_arr_detalle.pesosViv97 = v_pesos_viv97 USING "$$,$$$,$$&.&&"
    LET p_arr_detalle.pesosTotal = v_pesos_total USING "$$,$$$,$$&.&&"


   IF fgl_report_loadCurrentSettings(v_reporte) THEN
      CALL fgl_report_selectDevice("PDF")# PDF, XLS, HTML
      CALL fgl_report_selectPreview(TRUE)
      LET reporte = fgl_report_commitCurrentSettings()
      DISPLAY "NSS reporte ", p_arr_detalle.nss
      IF reporte IS NOT NULL THEN
         START REPORT pdf_acuse TO XML HANDLER reporte
            OUTPUT TO REPORT pdf_acuse(p_arr_detalle.*, v_fecha, v_aviso)
         FINISH REPORT pdf_acuse
      END IF
   END IF
  
END FUNCTION 

REPORT pdf_acuse(pm_arr_detalle, p_fecha, p_aviso) 
DEFINE pm_arr_detalle RECORD
           nss                LIKE afi_derechohabiente.nss,
           curp               LIKE afi_derechohabiente.curp,
           tramite            CHAR(50),     -- Descripci�n del Tr�mite
	        grupo              CHAR(55),
           apePaterno         CHAR(40),     -- Apellido paterno
           apeMaterno         CHAR(40),     -- Apellido materno
           nombre             CHAR(40),     -- Nombre
           medioSolicitud     CHAR(10),     -- Medio por el cual se hizo la solicitud 
           conRetiro          LIKE ret_solicitud_generico.id_solicitud,
           pesosViv92         CHAR(18), -- Vivienda 92
           pesosViv97         CHAR(18), -- Vivienda 97
           pesosTotal         CHAR(18), -- Suma en pesos total devuelto
           fTramite           DATETIME YEAR TO MINUTE,     -- Fecha de la Solicitud
           sello              CHAR(64), -- Acuse Generado
           archivo            BYTE,
           estadoConsulta     SMALLINT,     -- Resultado de la Consulta
           codRechazo         SMALLINT,      -- C�digo de rechazo
           caso_crm           CHAR(10)     -- Caso CRM
END RECORD 
DEFINE p_fecha                CHAR(16)
DEFINE v_nombre               CHAR(60)
DEFINE p_aviso                CHAR(255)

   FORMAT

   FIRST PAGE HEADER

      PRINTX pm_arr_detalle.nss
      PRINTX pm_arr_detalle.curp
      LET v_nombre = pm_arr_detalle.apePaterno CLIPPED, " ", pm_arr_detalle.apeMaterno CLIPPED, " ", pm_arr_detalle.nombre CLIPPED
      PRINTX v_nombre
      PRINTX pm_arr_detalle.tramite      
      PRINTX pm_arr_detalle.grupo
      PRINTX p_fecha
      PRINTX pm_arr_detalle.conRetiro
      PRINTX pm_arr_detalle.medioSolicitud
      PRINTX pm_arr_detalle.pesosViv92
      PRINTX pm_arr_detalle.pesosViv97
      PRINTX pm_arr_detalle.pesosTotal
      PRINTX pm_arr_detalle.sello
      PRINTX p_aviso
      PRINTX pm_arr_detalle.caso_crm


END REPORT
