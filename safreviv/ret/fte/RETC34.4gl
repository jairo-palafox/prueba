--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#MODULO       => RET                                                                    #
#PROGRAMA     => RETC34                                                                 #
#OBJETIVO     => Consulta de retiros por Transferencia de Recursos                      #
#Fecha inicio => 10 Junio 2013                                                          #
#Modificacion =>                                                                        #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS 
DEFINE g_arr_solicitudes_folio DYNAMIC ARRAY OF RECORD -- arreglo que contiene los folios elegidos para consultar
          elegir                 SMALLINT,
          modalidad_retiro       CHAR(1),
          desc_modalidad         VARCHAR(50),
          num_solicitudes        SMALLINT,
          folio                  DECIMAL(9,0),
          aivs_viv92             DECIMAL(24,6),
          aivs_viv97             DECIMAL(24,6)
       END RECORD
    
END GLOBALS 

{
======================================================================
Clave: 
Nombre: main
Fecha creacion: junio 10, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Abre la ventana de captura de datos para realizar la consulta de retiro
por Transferencia de recursos

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
MAIN 
DEFINE cbx_tipo_retiro      ui.ComboBox, -- combo de tipo de retiro
       cbx_estado_solicitud ui.ComboBox, -- combo de estado de la solicitud
       cbx_cod_rechazo      ui.ComboBox, -- combo con los codigos de rechazo
       cbx_folio            ui.ComboBox, -- combo con los codigos de rechazo
       p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion    SMALLINT, -- forma como ejecutara el programa
       p_s_titulo          STRING,   -- titulo de la ventana 
       t_totalgral_pesos   DECIMAL(19,6),
       t_totalgral_aivs92  DECIMAL(19,6),
       t_totalgral_aivs97  DECIMAL(19,6),
       r_total_pesos       DECIMAL(19,6),
       r_total_avis92      DECIMAL(19,6),
       r_total_avis97      DECIMAL(19,6),
       ar_ret_tipo_retiro      RECORD LIKE ret_tipo_retiro.*, -- registro con los tipos de retiro
       ar_ret_estado_solicitud RECORD LIKE ret_estado_solicitud.*, -- registro con los estados de la solicitud
       ar_ret_rechazo          RECORD LIKE ret_rechazo.*, -- registro con los codigos de rechazo
       -- parametros de consulta
       v_tipo_retiro       LIKE ret_tipo_retiro.tpo_retiro,
       v_etapa             SMALLINT, -- 1 Solicitud, 2 Preliquidacion, 3 Liquidacion
       v_nss               LIKE afi_derechohabiente.nss, 
       v_folio             LIKE glo_folio.folio,
       v_estado            LIKE ret_estado_solicitud.estado_solicitud,
       v_cod_rechazo       SMALLINT, -- codigo de rechazo
       v_fecha_inicio      DATE, -- fecha de inicio de consulta
       v_fecha_fin         DATE, -- fecha fin de consulta
       v_indice            INTEGER, -- contador       
       v_cadena            STRING, -- cadena para concatenar
       v_formulario        ui.Form -- para modificar el formulario

   -- se obtienen los parametros de ejecucion
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN
   
   -- se abre la ventana de consulta
   OPEN WINDOW w_consulta WITH FORM "RETC341"

   -- se capturan los datos de la consulta
   INPUT BY NAME
      v_tipo_retiro  ,
      v_etapa        ,
      v_nss          ,
      v_folio        ,
      v_estado       ,
      v_cod_rechazo  ,
      v_fecha_inicio ,
      v_fecha_fin    
   WITHOUT DEFAULTS
   ATTRIBUTES ( UNBUFFERED )
   
      BEFORE INPUT
         -- se obtiene control del formulario
         LET v_formulario = DIALOG.getForm()

         DISPLAY "TRANSFERENCIA" TO v_modalidad
   
         -- se llenan los combos
         LET cbx_tipo_retiro = ui.ComboBox.forName("formonly.v_tipo_retiro")
         
         CALL cbx_tipo_retiro.clear()
         
         DECLARE  cur_tiporetiro CURSOR FOR
         SELECT   *
         FROM     ret_tipo_retiro
         WHERE    modalidad_retiro = 6 -- transferencias
         ORDER BY tpo_retiro
   
         -- se agrega el valor nulo
         CALL cbx_tipo_retiro.addItem(NULL, "Todos")
         
         -- se agrega PMG
         FOREACH cur_tiporetiro INTO ar_ret_tipo_retiro.*
            -- se agregan los retiros
            LET v_cadena = ar_ret_tipo_retiro.tpo_retiro || " - " || ar_ret_tipo_retiro.des_corta
            CALL cbx_tipo_retiro.addItem(ar_ret_tipo_retiro.tpo_retiro, v_cadena)
         END FOREACH
           
         -- se inicia el tipo de retiro en nulo
         LET v_tipo_retiro = NULL
                 
         -- por omision la etapa elegida es solicitud
         LET v_etapa = 1
         
         -- se llena el combo con los folios de disposicion
         LET cbx_folio = ui.ComboBox.forName("formonly.v_folio")
         
         DECLARE cur_folios CURSOR FOR
         SELECT a.folio
         FROM   glo_folio a
         WHERE  a.proceso_cod = g_proceso_cod_ret_transferencia -- Transferencia
         AND    a.status >= 0
         ORDER BY folio DESC
         
         FOREACH cur_folios INTO v_folio
            CALL cbx_folio.addItem(v_folio, v_folio)
         END FOREACH
         
         -- no se tiene folio
         LET v_folio = NULL
         
         -- se llena el combo con los estados de la solicitud
         LET cbx_estado_solicitud = ui.ComboBox.forName("formonly.v_estado")
         
         CALL cbx_estado_solicitud.clear()
{         
         DECLARE cur_estadossol CURSOR FOR
         SELECT  *
         FROM    ret_estado_solicitud
         
         FOREACH cur_estadossol INTO ar_ret_estado_solicitud.*
            LET v_cadena = ar_ret_estado_solicitud.estado_solicitud, " - ", ar_ret_estado_solicitud.des_corta
            CALL cbx_estado_solicitud.addItem(ar_ret_estado_solicitud.estado_solicitud, v_cadena)
         END FOREACH
}
         -- se usan 3 conjuntos
         CALL cbx_estado_solicitud.addItem(1,"Todas")
         CALL cbx_estado_solicitud.addItem(2,"Aceptadas")
         CALL cbx_estado_solicitud.addItem(3,"Rechazadas")

         -- se asume que se desean todas
         LET v_estado = 1
         
         -- se carga el combo de codigos de rechazo
         LET cbx_cod_rechazo = ui.ComboBox.forName("formonly.v_cod_rechazo")
         
         CALL cbx_cod_rechazo.clear()
         -- se agrega la agrupacion todas
         CALL cbx_cod_rechazo.addItem(NULL, "Todas")
         
         DECLARE cur_codrechazo CURSOR FOR
         SELECT *
         FROM   ret_rechazo
         WHERE  cod_rechazo IN (49, 5, 597,961,599,598,600,100,201,758,759,760,768,766,767)
         ORDER BY cod_rechazo
         
         FOREACH cur_codrechazo INTO ar_ret_rechazo.*
            LET v_cadena = ar_ret_rechazo.cod_rechazo, " - ", ar_ret_rechazo.des_corta
            CALL cbx_cod_rechazo.addItem(ar_ret_rechazo.cod_rechazo, v_cadena)
         END FOREACH
         
         -- se inicia la consulta sin filtro de codigo de rechazo
         LET v_cod_rechazo = NULL

         -- la fecha de inicio y fecha fin se inician con la fecha del dia
         LET v_fecha_inicio = "01/01/1901"
         LET v_fecha_fin    = TODAY
                  
      -- al cambiar de etapa, se ocultan/muestran los campos para capturar estado de solicitud y codigo de rechazo
      ON CHANGE v_etapa
         IF ( v_etapa = 1 ) THEN
            CALL v_formulario.setElementHidden("formonly.v_estado",0)
            CALL v_formulario.setElementHidden("formonly.v_cod_rechazo",0)
         ELSE
            CALL v_formulario.setElementHidden("formonly.v_estado",1)
            CALL v_formulario.setElementHidden("formonly.v_cod_rechazo",1)
         END IF
                  
      AFTER FIELD v_fecha_fin
         NEXT FIELD v_tipo_retiro
                  
      -- cancelar
      ON ACTION cancel
         EXIT INPUT
         
      ON ACTION accept
         -- el folio es forzoso
         IF ( v_folio IS NULL ) THEN
            CALL fn_mensaje("Atención","Es necesario elegir un folio","stop")
            CONTINUE INPUT           
         END IF
      
         -- se validan los datos capturados
         IF ( v_nss IS NOT NULL AND LENGTH(v_nss CLIPPED) <> 11 ) THEN
            CALL fn_mensaje("Atención","La longitud del NSS debe ser de 11 caracteres","stop")
            CONTINUE INPUT
         END IF
         
         -- deben venir ambas fechas
         IF ( v_fecha_inicio IS NULL OR v_fecha_fin IS NULL ) THEN
            CALL fn_mensaje("Atención","Debe capturar ambas fechas","stop")
            CONTINUE INPUT
         ELSE
            -- si estan las dos, la fecha inicio no puede ser posterior a la fecha fin
            IF ( v_fecha_inicio > v_fecha_fin ) THEN
               CALL fn_mensaje("Atención","La fecha de inicio no puede ser posterior a la fecha de término","stop")
               CONTINUE INPUT
            ELSE
               -- ninguna de las fechas puede ser posterior a la fecha actual
               IF ( v_fecha_inicio > TODAY OR v_fecha_fin > TODAY ) THEN
                  CALL fn_mensaje("Atención","La fecha de consulta de inicio o fin no pueden ser posteriores a la fecha actual","stop")
                  CONTINUE INPUT
               END IF
            END IF
         END IF
      
         -- consultar solicitudes
         IF ( v_etapa = 1 ) THEN
            -- se envian los datos a la funcion de consulta por solicitud
            CALL fn_consulta_transferencia_solicitud(v_tipo_retiro  ,
                                           v_etapa        ,
                                           v_nss          ,
                                           v_folio        ,
                                           v_estado       ,
                                           v_cod_rechazo  ,
                                           v_fecha_inicio ,
                                           v_fecha_fin    )
         ELSE       
            -- se envian los datos a la funcion de consulta de preliquidacion/liquidacion
            CALL fn_consulta_transferencia_preliquidacion_liquidacion(v_tipo_retiro  ,
                                                            v_etapa        ,
                                                            v_nss          ,
                                                            v_folio        ,
                                                            v_estado       ,
                                                            v_cod_rechazo  ,
                                                            v_fecha_inicio ,
                                                            v_fecha_fin    )
         END IF
   
   END INPUT

  CLOSE WINDOW w_consulta
END MAIN


{
======================================================================
Clave: 
Nombre: fn_consulta_transferencia_solicitud
Fecha creacion: junio 10, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Realiza la consulta de los datos de retiro PMG de acuerdo con los parametros
de consulta capturados, en una ventana que muestra los montos totales
por el retiro por Transferencia

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     Julio 03, 2013        - Se agregan los registros rechazados de la tabla ret_disposicion_rch
                                      en la misma consulta
======================================================================
}
FUNCTION fn_consulta_transferencia_solicitud(v_tipo_retiro, v_etapa, v_nss, v_folio, v_estado, v_cod_rechazo,
                                  v_fecha_inicio, v_fecha_fin)
DEFINE v_tipo_retiro        LIKE ret_tipo_retiro.tpo_retiro,
       v_etapa              SMALLINT, -- 1 Solicitud, 2 Preliquidacion, 3 Liquidacion
       v_nss                LIKE afi_derechohabiente.nss, 
       v_folio              LIKE glo_folio.folio,
       v_estado             LIKE ret_estado_solicitud.estado_solicitud,
       v_cod_rechazo        SMALLINT, -- codigo de rechazo
       v_fecha_inicio       DATE, -- fecha de inicio de consulta
       v_fecha_fin          DATE, -- fecha fin de consulta
       v_indice             INTEGER, -- contador       
       v_sql                STRING, -- cadena con instruccion sql
       v_elementos_elegidos INTEGER, -- contador de elementos elegidos
       v_r_despliegue       RECORD
          elegir             SMALLINT,
          modalidad_retiro   CHAR(1),
          desc_modalidad     VARCHAR(50),
          num_solicitudes    SMALLINT,
          folio              DECIMAL(9,0),
          aivs_viv92         DECIMAL(24,6),
          aivs_viv97         DECIMAL(24,6)
       END RECORD,          
       v_arr_despliegue     DYNAMIC ARRAY OF RECORD
          elegir             SMALLINT,
          modalidad_retiro   CHAR(1),
          desc_modalidad     VARCHAR(50),
          num_solicitudes    SMALLINT,
          folio              DECIMAL(9,0),
          aivs_viv92         DECIMAL(24,6),
          aivs_viv97         DECIMAL(24,6)
       END RECORD
       
   -- ===================================================================================
   -- ===================================================================================
   -- REGISTROS ACEPTADOS
   -- ===================================================================================
   -- se construye la cadena de consulta
   LET v_sql = "\n SELECT                  ",
               "\n 1                 ,     ",
               "\n d.tpo_retiro,    ",
               "\n d.tpo_retiro || ' - ' || e.des_corta,    ",
               "\n COUNT(*)          ,     ",
               "\n a.folio           ,     ",
               "\n SUM(0) ,     ",
               "\n SUM(a.aivs_viv97)       ",
               "\n FROM ret_transferencia a,",
               "\n      glo_folio         b,",
               "\n      afi_derechohabiente c,",
               "\n      ret_matriz_derecho d,",
               "\n      ret_tipo_retiro    e ",
               -- "\n WHERE a.f_resolucion BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'",
               "\n WHERE 1 = 1 ",
               "\n AND d.tpo_retiro = e.tpo_retiro ",
               "\n AND a.id_derechohabiente = c.id_derechohabiente ",
               "\n AND a.folio = b.folio ",
               "\n AND b.proceso_cod = ", g_proceso_cod_ret_transferencia, -- transferencia
               "\n AND b.status >= 0 ", -- folios integrados a liquidados
               "\n AND a.id_ret_matriz_derecho = d.id_ret_matriz_derecho " -- tipos de retiro que coinciden
               
   -- si se recibio nss 
   IF ( v_nss IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND c.nss = '", v_nss, "'"
   END IF
   
   -- si se capturo folio
   IF ( v_folio IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.folio = ", v_folio
   END IF
   
   -- tipo de retiro especifico
   IF ( v_tipo_retiro IS NOT NULL ) THEN
      LET v_sql = v_sql || "\n AND d.tpo_retiro = '", v_tipo_retiro, "'"
   END IF

   -- si se capturaron fechas
   IF v_fecha_inicio IS NOT NULL AND v_fecha_fin IS NOT NULL THEN 
       LET v_sql = v_sql || "\n AND a.f_resolucion BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'"
   END IF 
   
   -- si se recibio estado de la solicitud
   --IF ( v_estado IS NOT NULL ) THEN
   --   LET v_sql = v_sql, "\n AND a.estado_solicitud = ", v_estado
   --END IF

   IF ( v_estado <> 1 ) THEN

      -- aceptadas
      IF ( v_estado = 2 ) THEN
         LET v_sql = v_sql, "\n AND a.estado_solicitud NOT IN (100,101)"
      ELSE
         -- rechazadas
         LET v_sql = v_sql, "\n AND a.estado_solicitud IN (100,101)"
      END IF
   
      --LET v_sql = v_sql, "\n AND a.estado_solicitud = ", v_estado
   END IF
   
   -- si se recibio codigo de rechazo especifico
   IF ( v_cod_rechazo IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.cod_rechazo = ", v_cod_rechazo
   END IF

   -- se concatena la agrupacion y ordenamiento en turno
   LET v_sql = v_sql, "\n GROUP BY 1,2,3,5",
                      "\n ORDER BY 3"

   DISPLAY v_sql

   -- se prepara la consulta
   PREPARE sid_solicitudes1 FROM v_sql
   
   DECLARE cur_solicitudes1 CURSOR FOR sid_solicitudes1
   
   LET v_indice = 1
   
   FOREACH cur_solicitudes1 INTO v_r_despliegue.*  
      -- se transfieren los datos al arreglo de despliegue
      LET v_arr_despliegue[v_indice].* = v_r_despliegue.*
   
      LET v_indice = v_indice + 1
   END FOREACH
   
   -- ===================================================================================
   -- ===================================================================================
   -- REGISTROS RECHAZADOS
   -- ===================================================================================
   
   -- se construye la cadena de consulta de rechazos
   LET v_sql = "\n SELECT                 ",
               "\n 1                 ,    ",
               "\n a.tpo_retiro,    ",
               "\n a.tpo_retiro || ' - ' || e.des_corta,    ",
               "\n COUNT(*)          ,    ",
               "\n a.folio           ,    ",
               "\n SUM(0) ,    ",
               "\n SUM(a.aivs_viv97)      ",
               "\n FROM ret_transferencia_rch a,",
               "\n      glo_folio       b,",
               "\n      ret_tipo_retiro e ",
               --"\n WHERE a.f_resolucion BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'",
               "\n WHERE 1 = 1 ",
               "\n AND a.tpo_retiro = e.tpo_retiro ",
               "\n AND a.folio = b.folio ",
               "\n AND b.proceso_cod = ", g_proceso_cod_ret_transferencia, -- transferencia
               "\n AND b.status >= 0 " -- folios integrados a liquidados
               
   -- si se recibio nss 
   IF ( v_nss IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.nss = '", v_nss, "'"
   END IF
   
   -- si se capturo folio
   IF ( v_folio IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.folio = ", v_folio
   END IF

   -- si se capturaron fechas
   IF v_fecha_inicio IS NOT NULL AND v_fecha_fin IS NOT NULL THEN 
       LET v_sql = v_sql || " AND a.f_resolucion BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'"
   END IF 
   
   -- tipo de retiro especifico
   IF ( v_tipo_retiro IS NOT NULL ) THEN
      LET v_sql = v_sql || "\n AND a.tpo_retiro = '", v_tipo_retiro, "'"
   END IF

{   
   -- si se recibio estado de la solicitud
   IF ( v_estado IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.estado_solicitud = ", v_estado
   END IF
 }  

   IF ( v_estado <> 1 ) THEN

      -- aceptadas
      IF ( v_estado = 2 ) THEN
         LET v_sql = v_sql, "\n AND a.estado_solicitud NOT IN (100,101)"
      ELSE
         -- rechazadas
         LET v_sql = v_sql, "\n AND a.estado_solicitud IN (100,101)"
      END IF
   
      --LET v_sql = v_sql, "\n AND a.estado_solicitud = ", v_estado
   END IF
 
   -- si se recibio codigo de rechazo especifico
   IF ( v_cod_rechazo IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.cod_rechazo_1 = ", v_cod_rechazo
   END IF

   -- se concatena la agrupacion y ordenamiento en turno
   LET v_sql = v_sql, "\n GROUP BY 1,2,3,5",
                      "\n ORDER BY 3"

   DISPLAY v_sql

   -- se prepara la consulta
   PREPARE sid_solicitudes1rch FROM v_sql
   
   DECLARE cur_solicitudes1rch CURSOR FOR sid_solicitudes1rch
   
   FOREACH cur_solicitudes1rch INTO v_r_despliegue.*  
      -- se transfieren los datos al arreglo de despliegue
      LET v_arr_despliegue[v_indice].* = v_r_despliegue.*
      
      -- se concatena RHC a la descripcion
      LET v_arr_despliegue[v_indice].desc_modalidad = v_arr_despliegue[v_indice].desc_modalidad CLIPPED, "-RCH"
   
      LET v_indice = v_indice + 1
   END FOREACH

      
   -- si la longitud del arreglo es 1, entonces se selecciona por omision
   IF ( v_arr_despliegue.getLength() = 1 ) THEN
      LET v_arr_despliegue[1].elegir = 1
   END IF
   
   OPEN WINDOW w_consulta1 WITH FORM "RETC342"
   
   INPUT ARRAY v_arr_despliegue WITHOUT DEFAULTS
   FROM tbl_solicitudes_1.* ATTRIBUTES ( UNBUFFERED, INSERT ROW = FALSE, DELETE ROW = FALSE, APPEND ROW = FALSE )
      ON ACTION accept
      
         LET v_elementos_elegidos = 0
      
         -- se verifica que al menos haya elegido un renglon
         FOR v_indice = 1 TO v_arr_despliegue.getLength()
            IF ( v_arr_despliegue[v_indice].elegir = 1 ) THEN
               LET v_elementos_elegidos = v_elementos_elegidos + 1
               
               LET g_arr_solicitudes_folio[v_elementos_elegidos].* = v_arr_despliegue[v_indice].*
            END IF
         END FOR
         
         -- si no se elegio ningun registro no se puede realizar la consulta
         IF ( v_elementos_elegidos < 1 ) THEN
            -- se le indica al usuario que debe elegir al menos un registro
            CALL fn_mensaje("Atención","Es necesario elegir al menos un registro para ejecutar la consulta","stop")
            CONTINUE INPUT
         END IF
         
         -- se invoca la consulta de los elementos elegidos. Los elementos seleccionados estan en el arreglo global
         CALL fn_consulta_solicitudes_nivel2(v_tipo_retiro, v_etapa, v_nss, v_folio, v_estado, v_cod_rechazo,
                                             v_fecha_inicio, v_fecha_fin)

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
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_solicitudes_nivel2(v_tipo_retiro, v_etapa, v_nss, v_folio, v_estado, v_cod_rechazo,
                                        v_fecha_inicio, v_fecha_fin)
DEFINE v_tipo_retiro        LIKE ret_tipo_retiro.tpo_retiro,
       v_etapa              SMALLINT, -- 1 Solicitud, 2 Preliquidacion, 3 Liquidacion
       v_nss                LIKE afi_derechohabiente.nss, 
       v_folio              LIKE glo_folio.folio,
       v_estado             LIKE ret_estado_solicitud.estado_solicitud,
       v_cod_rechazo        SMALLINT, -- codigo de rechazo
       v_fecha_inicio       DATE, -- fecha de inicio de consulta
       v_fecha_fin          DATE, -- fecha fin de consulta
       v_indice             INTEGER, -- contador       
       v_sql                STRING, -- cadena con instruccion sql
       v_elementos_elegidos INTEGER, -- contador de elementos elegidos
       v_r_despliegue       RECORD
         id_solicitud        LIKE ret_disposicion.id_solicitud ,
         nss                 VARCHAR(50)      ,
         f_solicitud         LIKE ret_disposicion.f_solicitud  ,
         aivs_viv92          LIKE ret_disposicion.aivs_viv92   ,
         aivs_viv97          LIKE ret_disposicion.aivs_viv97   ,
         pesos_viv92         LIKE ret_preliquida.monto_pesos   ,
         pesos_viv97         LIKE ret_preliquida.monto_pesos   ,
         estado_solicitud    VARCHAR(50)                       ,
         cod_rechazo         VARCHAR(100)
       END RECORD,          
       v_arr_despliegue     DYNAMIC ARRAY OF RECORD
         id_solicitud        LIKE ret_disposicion.id_solicitud ,
         nss                 VARCHAR(50)      ,
         f_solicitud         LIKE ret_disposicion.f_solicitud  ,
         aivs_viv92          LIKE ret_disposicion.aivs_viv92   ,
         aivs_viv97          LIKE ret_disposicion.aivs_viv97   ,
         pesos_viv92         LIKE ret_preliquida.monto_pesos   ,
         pesos_viv97         LIKE ret_preliquida.monto_pesos   ,
         estado_solicitud    VARCHAR(50)                       ,
         cod_rechazo         VARCHAR(100)
       END RECORD,
       v_valor_aiv           LIKE ret_cza_disposicion.precio_fondo,
       v_num_registros       INTEGER,
       v_total_viv92         DECIMAL(22,6),
       v_total_viv97         DECIMAL(22,6),
       v_fecha_valuacion     LIKE ret_cza_disposicion.f_valor_transferencia

   -- se inician las variables de acumulacion de cifras
   LET v_num_registros = 0
   LET v_total_viv92   = 0
   LET v_total_viv97   = 0


   -- se obtiene el valor de las AIVs de encabezado del folio
   SELECT precio_fondo, f_valor_transferencia
   INTO   v_valor_aiv, v_fecha_valuacion
   FROM   ret_cza_transferencia
   WHERE  folio = v_folio

   -- ====================================================================================================
   -- ====================================================================================================
   -- SOLICITUDES ACEPTADAS
   -- ====================================================================================================
   -- se obtienen las solicitudes de los folios elegidos y con las condiciones dadas en la captura de consulta  
   LET v_sql = "\n SELECT                                            ",
               "\n  a.id_solicitud     ,                             ",
               "\n  b.nss              ,                             ",
               "\n  a.f_resolucion     ,                             ",
               "\n  0       ,                             ",
               "\n  a.aivs_viv97       ,                             ",
               "\n  0                  ,                             ",
               "\n  0                  ,                             ",
               "\n  a.estado_solicitud || '-' || c.des_corta,        ",
               "\n  a.cod_rechazo || '-' || d.des_corta              ",
               "\n FROM ret_transferencia    a,                      ",
               "\n      afi_derechohabiente  b,                      ",
               "\n      ret_estado_solicitud c,                      ",
               "\n      ret_rechazo          d,                      ",
               "\n      ret_matriz_derecho   e                       ",
               "\n WHERE a.id_derechohabiente = b.id_derechohabiente ",
               -- "\n AND   a.f_resolucion BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'",
               "\n AND   a.id_ret_matriz_derecho = e.id_ret_matriz_derecho"

   -- si se recibio nss 
   IF ( v_nss IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND b.nss = '", v_nss, "'"
   END IF

   -- si se capturaron fechas
   IF v_fecha_inicio IS NOT NULL AND v_fecha_fin IS NOT NULL THEN 
       LET v_sql = v_sql || " AND a.f_resolucion BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'"
   END IF 

   -- se concatenan los folios elegidos del paso anterior
   LET v_sql = v_sql || "\n AND a.folio IN ("
   
   FOR v_indice = 1 TO g_arr_solicitudes_folio.getLength()
      LET v_sql = v_sql, g_arr_solicitudes_folio[v_indice].folio
      
      -- si es el ultimo elemento se cierra el parentesis
      IF ( v_indice = g_arr_solicitudes_folio.getLength() ) THEN
         LET v_sql = v_sql, ")"
      ELSE
         -- en otro caso se pone una coma
         LET v_sql = v_sql, ", "
      END IF
   END FOR

   -- el tipo de retiro elegido
   LET v_sql = v_sql || "\n AND e.tpo_retiro IN ('"
   
   FOR v_indice = 1 TO g_arr_solicitudes_folio.getLength()
      LET v_sql = v_sql, g_arr_solicitudes_folio[v_indice].modalidad_retiro
      
      -- si es el ultimo elemento se cierra el parentesis
      IF ( v_indice = g_arr_solicitudes_folio.getLength() ) THEN
         LET v_sql = v_sql, "')"
      ELSE
         -- en otro caso se pone una coma
         LET v_sql = v_sql, "', '"
      END IF
   END FOR

   
   LET v_sql = v_sql, "\n AND   a.estado_solicitud = c.estado_solicitud     ",
                      "\n AND   a.cod_rechazo = d.cod_rechazo               "
   
   -- si se recibio estado de la solicitud
   --IF ( v_estado IS NOT NULL ) THEN
      --LET v_sql = v_sql, "\n AND a.estado_solicitud = ", v_estado
   --END IF

   IF ( v_estado <> 1 ) THEN

      -- aceptadas
      IF ( v_estado = 2 ) THEN
         LET v_sql = v_sql, "\n AND a.estado_solicitud NOT IN (100,101)"
      ELSE
         -- rechazadas
         LET v_sql = v_sql, "\n AND a.estado_solicitud IN (100,101)"
      END IF
   
      --LET v_sql = v_sql, "\n AND a.estado_solicitud = ", v_estado
   END IF
   
   -- si se recibio codigo de rechazo especifico
   IF ( v_cod_rechazo IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.cod_rechazo = ", v_cod_rechazo
   END IF

   DISPLAY "Buscando solicitudes por tipo de retiro elegido:"
   DISPLAY v_sql

   PREPARE sid_solicitudespmg FROM v_sql
   
   DECLARE cur_solicitudespmg CURSOR FOR sid_solicitudespmg
   
   -- se transfieren los datos al arreglo de despligue
   LET v_indice = 1
   
   FOREACH cur_solicitudespmg INTO v_r_despliegue.*
   
      LET v_arr_despliegue[v_indice].* = v_r_despliegue.*

      -- se agregan espacios a la descripcion
      LET v_arr_despliegue[v_indice].cod_rechazo = "   ", v_arr_despliegue[v_indice].cod_rechazo
      LET v_arr_despliegue[v_indice].estado_solicitud = "   ", v_arr_despliegue[v_indice].estado_solicitud  
      
      -- se multiplican las AIVs por el valor de la accion
      LET v_arr_despliegue[v_indice].pesos_viv92 = v_r_despliegue.aivs_viv92 * v_valor_aiv
      LET v_arr_despliegue[v_indice].pesos_viv97 = v_r_despliegue.aivs_viv97 * v_valor_aiv
      
      LET v_num_registros = v_num_registros + 1
      LET v_total_viv92   = v_total_viv92   + v_r_despliegue.aivs_viv92
      LET v_total_viv97   = v_total_viv97   + v_r_despliegue.aivs_viv97
      
      LET v_indice = v_indice + 1
   END FOREACH
   
   
   -- ====================================================================================================
   -- ====================================================================================================
   -- SOLICITUDES RECHAZADAS
   -- ====================================================================================================
   -- se buscan los registros rechazados con las mismas caracteristicas
   LET v_sql = "\n SELECT                                            ",
               "\n  0     ,                             ",
               "\n  a.nss              ,                             ",
               "\n  a.f_resolucion     ,                             ",
               "\n  0       ,                             ",
               "\n  a.aivs_viv97       ,                             ",
               "\n  0                  ,                             ",
               "\n  0                  ,                             ",
               "\n  a.estado_solicitud || '-' || c.des_corta,        ",
               "\n  a.cod_rechazo_1 || '-' || d.des_corta            ",
               "\n FROM ret_transferencia_rch  a,                    ",
               "\n      ret_estado_solicitud c,                      ",
               "\n      ret_rechazo          d                       ",
               -- "\n WHERE a.f_resolucion BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'"
               "\n WHERE 1 = 1 "

   
   -- si se recibio nss 
   IF ( v_nss IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.nss = '", v_nss, "'"
   END IF

   -- si se capturaron fechas
   IF v_fecha_inicio IS NOT NULL AND v_fecha_fin IS NOT NULL THEN 
       LET v_sql = v_sql || " AND a.f_resolucion BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'"
   END IF 

   -- se concatenan los folios elegidos del paso anterior
   LET v_sql = v_sql || "\n AND a.folio IN ("
   
   FOR v_indice = 1 TO g_arr_solicitudes_folio.getLength()
      LET v_sql = v_sql, g_arr_solicitudes_folio[v_indice].folio
      
      -- si es el ultimo elemento se cierra el parentesis
      IF ( v_indice = g_arr_solicitudes_folio.getLength() ) THEN
         LET v_sql = v_sql, ")"
      ELSE
         -- en otro caso se pone una coma
         LET v_sql = v_sql, ", "
      END IF
   END FOR

   -- el tipo de retiro elegido
   LET v_sql = v_sql || "\n AND a.tpo_retiro IN ('"
   
   FOR v_indice = 1 TO g_arr_solicitudes_folio.getLength()
      LET v_sql = v_sql, g_arr_solicitudes_folio[v_indice].modalidad_retiro
      
      -- si es el ultimo elemento se cierra el parentesis
      IF ( v_indice = g_arr_solicitudes_folio.getLength() ) THEN
         LET v_sql = v_sql, "')"
      ELSE
         -- en otro caso se pone una coma
         LET v_sql = v_sql, "', '"
      END IF
   END FOR
   
   LET v_sql = v_sql, "\n AND   a.estado_solicitud = c.estado_solicitud     ",
                      "\n AND   a.cod_rechazo_1 = d.cod_rechazo               "
{   
   -- si se recibio estado de la solicitud
   IF ( v_estado IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.estado_solicitud = ", v_estado
   END IF
}   

   IF ( v_estado <> 1 ) THEN

      -- aceptadas
      IF ( v_estado = 2 ) THEN
         LET v_sql = v_sql, "\n AND a.estado_solicitud NOT IN (100,101)"
      ELSE
         -- rechazadas
         LET v_sql = v_sql, "\n AND a.estado_solicitud IN (100,101)"
      END IF
   
      --LET v_sql = v_sql, "\n AND a.estado_solicitud = ", v_estado
   END IF


   -- si se recibio codigo de rechazo especifico
   IF ( v_cod_rechazo IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.cod_rechazo_1 = ", v_cod_rechazo
   END IF

   DISPLAY "Buscando solicitudes rechazadas por tipo de retiro elegido:"
   DISPLAY v_sql

   PREPARE sid_solicitudespmg_rch FROM v_sql
   
   DECLARE cur_solicitudespmg_rch CURSOR FOR sid_solicitudespmg_rch
   
   -- se inicia el contador en donde se quedo el despliegue de los datos aceptados
   LET v_indice = v_arr_despliegue.getLength() + 1
   
   -- se transfieren los datos al arreglo de despligue  
   FOREACH cur_solicitudespmg_rch INTO v_r_despliegue.*
   
      LET v_arr_despliegue[v_indice].* = v_r_despliegue.*

      -- se agregan espacios a la descripcion
      LET v_arr_despliegue[v_indice].cod_rechazo = "   ", v_arr_despliegue[v_indice].cod_rechazo
      LET v_arr_despliegue[v_indice].estado_solicitud = "   ", v_arr_despliegue[v_indice].estado_solicitud  


      -- se multiplican las AIVs por el valor de la accion
      LET v_arr_despliegue[v_indice].pesos_viv92 = v_r_despliegue.aivs_viv92 * v_valor_aiv
      LET v_arr_despliegue[v_indice].pesos_viv97 = v_r_despliegue.aivs_viv97 * v_valor_aiv

      -- se acumulan los montos   
      LET v_num_registros = v_num_registros + 1
      LET v_total_viv92   = v_total_viv92   + v_r_despliegue.aivs_viv92
      LET v_total_viv97   = v_total_viv97   + v_r_despliegue.aivs_viv97
      
      LET v_indice = v_indice + 1
   END FOREACH

   -- se crean los registros de totales en AIVs
   LET v_arr_despliegue[v_indice + 1].nss = "TOTAL"
   LET v_arr_despliegue[v_indice + 1].aivs_viv92 = v_total_viv92
   LET v_arr_despliegue[v_indice + 1].aivs_viv97 = v_total_viv97
   LET v_arr_despliegue[v_indice + 1].pesos_viv92 = v_total_viv92 * v_valor_aiv
   LET v_arr_despliegue[v_indice + 1].pesos_viv97 = v_total_viv97 * v_valor_aiv
   
   -- se abre la ventana de consulta detallada
   OPEN WINDOW w_consultadetallada WITH FORM "RETC343"
   
   DISPLAY BY NAME v_num_registros, v_valor_aiv
   
   DISPLAY ARRAY v_arr_despliegue TO tbl_despliegue.*
   END DISPLAY
    
   CLOSE WINDOW w_consultadetallada
       
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_consulta_transferencia_preliquidacion_liquidacion
Fecha creacion: junio 10, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Realiza la consulta de los datos de retiro por transferencia de acuerdo con los parametros
de consulta capturados, en una ventana que muestra las solicitudes preliquidadas
o liquidadas de acuerdo a la etapa que se haya seleccionado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_transferencia_preliquidacion_liquidacion(v_tipo_retiro, v_etapa, v_nss, v_folio, v_estado, v_cod_rechazo,
                                  v_fecha_inicio, v_fecha_fin)
DEFINE v_tipo_retiro        LIKE ret_tipo_retiro.tpo_retiro,
       v_etapa              SMALLINT, -- 1 Solicitud, 2 Preliquidacion, 3 Liquidacion
       v_nss                LIKE afi_derechohabiente.nss, 
       v_folio              LIKE glo_folio.folio,
       v_estado             LIKE ret_estado_solicitud.estado_solicitud,
       v_cod_rechazo        SMALLINT, -- codigo de rechazo
       v_fecha_inicio       DATE, -- fecha de inicio de consulta
       v_fecha_fin          DATE, -- fecha fin de consulta
       v_indice             INTEGER, -- contador       
       v_sql                STRING, -- cadena con instruccion sql
       v_r_despliegue       RECORD
         id_referencia        LIKE ret_preliquida.id_referencia ,
         nss                  LIKE afi_derechohabiente.nss      ,
         movimiento           LIKE ret_preliquida.movimiento    ,
         f_liquida            LIKE ret_preliquida.f_liquida     ,
         folio_liquida        LIKE ret_preliquida.folio_liquida ,
         monto_acciones       LIKE ret_preliquida.monto_acciones,
         monto_pesos          LIKE ret_preliquida.monto_pesos   
       END RECORD,          
       v_arr_despliegue     DYNAMIC ARRAY OF RECORD
         id_referencia        LIKE ret_preliquida.id_referencia ,
         nss                  LIKE afi_derechohabiente.nss      ,
         movimiento           LIKE ret_preliquida.movimiento    ,
         f_liquida            LIKE ret_preliquida.f_liquida     ,
         folio_liquida        LIKE ret_preliquida.folio_liquida ,
         monto_acciones       LIKE ret_preliquida.monto_acciones,
         monto_pesos          LIKE ret_preliquida.monto_pesos   
       END RECORD
       
   -- se prepara la consulta
   LET v_sql = "\n SELECT             ",
               "\n a.id_referencia   ,",
               "\n b.nss             ,",
               "\n a.movimiento      ,",
               "\n a.f_liquida       ,",
               "\n a.folio_liquida   ,",
               "\n SUM(a.monto_acciones)  ,",
               "\n SUM(a.monto_pesos   )   "
   -- se verifica origen de los datos
   IF ( v_etapa = 2 ) THEN
      -- preliqidacion
      LET v_sql = v_sql, "\n FROM ret_preliquida a,"
   ELSE
      -- liquidacion
      LET v_sql = v_sql, "\n FROM ",
                         "\n      (SELECT id_derechohabiente, id_referencia, movimiento, ",
                         "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                         "\n         FROM cta_movimiento12 ",
                         "\n        WHERE f_liquida BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'",
                         "\n          AND folio_liquida = ", v_folio,
                         "\n       UNION ALL ",
                         "\n       SELECT id_derechohabiente, id_referencia, movimiento, ",
                         "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                         "\n         FROM cta_movimiento13 ",
                         "\n        WHERE f_liquida BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'",
                         "\n          AND folio_liquida = ", v_folio,
                         "\n       UNION ALL ",
                         "\n       SELECT id_derechohabiente, id_referencia, movimiento, ",
                         "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                         "\n         FROM cta_movimiento14 ",
                         "\n        WHERE f_liquida BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'",
                         "\n          AND folio_liquida = ", v_folio,
                         "\n       UNION ALL ",
                         "\n       SELECT id_derechohabiente, id_referencia, movimiento, ",
                         "\n              f_liquida, folio_liquida, monto_acciones, monto_pesos ",
                         "\n         FROM cta_movimiento ",
                         "\n        WHERE f_liquida BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'",
                         "\n          AND folio_liquida = ", v_folio,
                         "\n       ) a, "
   END IF
   
   -- se completa la consulta
   LET v_sql = v_sql, "\n afi_derechohabiente b,",
                      "\n glo_folio c           ",
                      "\n WHERE a.id_derechohabiente = b.id_derechohabiente ",
                      "\n AND   a.folio_liquida = c.folio                   ",
                      "\n AND   c.proceso_cod = ", g_proceso_cod_ret_transferencia, -- transferencia
                      "\n AND   c.status >= 1 ", -- preliquidados y/o liquidados
                      "\n AND   a.f_liquida BETWEEN '", v_fecha_inicio, "' AND '", v_fecha_fin, "'"

   -- si se recibio nss 
   IF ( v_nss IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND b.nss = '", v_nss, "'"
   END IF

   -- si se recibio folio
   IF ( v_folio IS NOT NULL ) THEN
      LET v_sql = v_sql, "\n AND a.folio_liquida = ", v_folio
   END IF   
   
   -- ordenado por folio y luego por NSS
   LET v_sql = v_sql, "\n GROUP BY 1,2,3,4,5"
   LET v_sql = v_sql, "\n ORDER BY 3, 2"
   
   -- se prepara y ejecuta la consulta
   PREPARE sid_preliquidaliquida FROM v_sql
   
   DECLARE cur_preliquidaliquida CURSOR FOR sid_preliquidaliquida
   
   LET v_indice = 1
   
   FOREACH cur_preliquidaliquida INTO v_r_despliegue.*
      -- se transfieren los datos al arreglo de despliegue
      LET v_arr_despliegue[v_indice].* = v_r_despliegue.*
      
      LET v_indice = v_indice + 1
   END FOREACH
   
   OPEN WINDOW w_consultapreliq WITH FORM "RETC344"
   
   DISPLAY ARRAY v_arr_despliegue TO tbl_despliegue.*
   END DISPLAY
   
   CLOSE WINDOW w_consultapreliq
   
END FUNCTION