#############################################################################
#Módulo          => RET                                                     #        
#Programa        => RETSI18-154                                             #
#Objetivo        => Programa carga de Históricos de Notifcación de Grupos   #
#                => 2, 3 y 4 Contingente REQUERIMIENTO SACI2018-154         #
#Fecha Inicio    => Octubre 16, 2018                                        #
#############################################################################
DATABASE safre_viv
GLOBALS 
DEFINE v_reg_cargados_x_tabla DYNAMIC ARRAY OF RECORD
        v_registro CHAR(2),  --LIKE cat_layout.registro
        v_tabla    CHAR(30), --LIKE cat_layout.tabla
        v_conteo   INTEGER
       END RECORD
       
END GLOBALS 
#Objetivo: Carga Archivo en proceso nohup 
MAIN
DEFINE p_nom_archivo       STRING,
       p_proceso           LIKE cat_proceso.proceso_cod,
       p_operacion         LIKE cat_operacion.opera_cod,
       p_pid               DECIMAL(9,0),
       p_usuario           CHAR(20),
       p_prog_a_lanzar       STRING,
       v_layout            LIKE cat_operacion.layout_cod,
       v_ruta_rescate      LIKE seg_modulo.ruta_rescate, -- ruta de rescate del modulo
       v_ruta_archivo      STRING, -- ruta completa del archivo leido
       v_usuario           LIKE seg_modulo.usuario,
       v_proceso_desc      LIKE cat_proceso.proceso_desc,
       v_extension         LIKE cat_operacion.extension,
       v_opera_desc        LIKE cat_operacion.opera_desc,
       v_indice            INTEGER,
       v_detalle_monitoreo STRING,
       v_archivo_monitoreo STRING,
       v_ruta_listados     LIKE seg_modulo.ruta_listados,
       v_reg_archivo       INTEGER,
       v_reg_aceptados     INTEGER,
       v_reg_rechazados    INTEGER,
       r_bnd_carga         BOOLEAN,
       r_resultado_opera   SMALLINT,
       g_reg_tab_cargados  INTEGER,
       v_comando           STRING,
       v_cadena_registros  STRING,
       v_canal             base.Channel,
       v_ltr_archivo       STRING,
       v_ltr_archivo_aux   STRING,
       v_reg_no_procesados INTEGER,
       v_fecha_inicio      DATETIME YEAR TO SECOND,
       v_mensaje           STRING,
       v_continua          BOOLEAN,
       v_sql               STRING,
       v_id_solicitud_envio DECIMAL(9,0),

       v_registro_archivo  RECORD    -- registro del archivo
           nss                       CHAR(11),
           indicador_cargo           CHAR(1),
           importe_total             DECIMAL(14,2),
           fch_pago                  DATE,
           grupo                     CHAR(4),
           valor_aiv                 DECIMAL(15,6),
           imp_gob_fed               DECIMAL(14,2),
           monto_pagado              DECIMAL(14,2),
           aivs_pagadas              DECIMAL(15,6),
           diag_procesar             CHAR(3),
           tipo_retiro               SMALLINT        
      END RECORD,
       v_registro_archivo_str  RECORD    -- registro del archivo
           nss                       STRING,
           indicador_cargo           STRING,
           importe_total             STRING,
           fch_pago                  STRING,
           grupo                     STRING,
           valor_aiv                 STRING,
           imp_gob_fed               STRING,
           monto_pagado              STRING,
           aivs_pagadas              STRING,
           diag_procesar             STRING,
           tipo_retiro               STRING
        END RECORD,
       v_registro_insert  RECORD    -- registro del archivo
           id_solicitud_envio        DECIMAL(9,0),
           id_solicitud_retiro       DECIMAL(9,0),
           estado_solicitud          SMALLINT,
           fch_envio                 DATE,
           fch_respuesta             DATE,
           nss                       CHAR(11),
           indicador_cargo           CHAR(1),
           importe_total             DECIMAL(14,2),
           fch_pago                  DATE,
           grupo                     CHAR(4),
           valor_aiv                 DECIMAL(15,6),
           imp_gob_fed               DECIMAL(14,2),
           monto_pagado              DECIMAL(14,2),
           aivs_pagadas              DECIMAL(15,6),
           diag_procesar             CHAR(3),
           tipo_retiro               SMALLINT
        END RECORD,
        
      v_ch_archivo        base.channel -- archivo que se carga
       
   #Parametros

   LET p_usuario       = 'SAFREVIV'
   LET p_pid           = 0
   LET p_proceso       = 0
   LET p_operacion     = 0
   LET p_nom_archivo   = 'DSSV_SPLIT.csvaa'
   LET p_prog_a_lanzar = ''

   LET p_nom_archivo    = ARG_VAL(1)
   
   -- se inicia el log del programa
   LET v_cadena_registros = p_usuario CLIPPED, ".", "RETSI18-154.log"
   CALL STARTLOG(v_cadena_registros)
   
   LET r_resultado_opera = 0
   LET v_indice = 0   
   LET v_mensaje = " "
   #Recuper la información necesaria para cargar el archivo seleccionado 
   #dependidendo del proceso y operación

   -- Se insertan los registros para que el proceso se vea en el monitor de procesos

   INSERT INTO glo_folio VALUES ((SELECT seq_glo_folio.nextval 
                                  FROM   systables 
                                  WHERE  tabid = 1), 1599, 1, 0,0,TODAY,'OPSISSACI');

   INSERT INTO glo_pid VALUES ((SELECT seq_glo_pid.NEXTVAL 
                                FROM   systables 
                                WHERE  tabid = 1),1599,1,0,'OPSISSACI', TODAY);
                                
   INSERT INTO bat_ctr_proceso VALUES ((SELECT seq_glo_pid.CURRVAL 
                                        FROM   systables 
                                        WHERE  tabid = 1),1599,(SELECT seq_glo_folio.currval 
                                                                FROM   systables 
                                                                WHERE  tabid = 1),
                                       CURRENT YEAR TO SECOND,CURRENT YEAR TO SECOND,NULL,0,NULL,
                                       NULL,NULL,4,TODAY,'OPSISSACI');
   INSERT INTO bat_ctr_operacion VALUES ((SELECT seq_glo_pid.CURRVAL 
                                          FROM   systables  
                                          WHERE  tabid = 1),1599,1,'RETL465',(SELECT seq_glo_folio.currval 
                                                                              FROM   systables 
                                                                              WHERE  tabid = 1),
                                       CURRENT YEAR TO SECOND,CURRENT YEAR TO SECOND,NULL,NULL,NULL,1,NULL,
                                       NULL,5,0,0,NULL,4,TODAY,'OPSISSACI');

   INSERT INTO bat_ctr_operacion VALUES ((SELECT seq_glo_pid.CURRVAL 
                                          FROM   systables  
                                          WHERE  tabid = 1),1599,2,'RETL466',(SELECT seq_glo_folio.currval 
                                                                              FROM   systables 
                                                                              WHERE  tabid = 1),
                                       CURRENT YEAR TO SECOND,CURRENT YEAR TO SECOND,NULL,NULL,NULL,1,NULL,
                                       NULL,5,0,0,NULL,4,TODAY,'OPSISSACI');

   
   DISPLAY "Inicia carga del archivo","\n",v_detalle_monitoreo 
   #Nombre del archivo de monitoreo
   LET v_archivo_monitoreo = "nohup:",p_pid USING "&&&&&",":",p_proceso USING "&&&&&",":",p_operacion USING "&&&&&"
   DISPLAY "========================",v_archivo_monitoreo

   #Se elimina los espacios al final de cada variable
   LET v_ruta_rescate = "/safreviv_req/SACI2018-154/"

   DISPLAY "Preparando lectura de archivo : ", p_nom_archivo

   -- se crea el apuntador para apertura y lectura de archivo
   LET v_ch_archivo = base.Channel.create() 

   -- la ruta completa del archivo es la ruta rescate mas el nombre del archivo
   LET v_ruta_archivo = p_nom_archivo

   DISPLAY "Ruta archivo: ", v_ruta_archivo

   -- se abre el archivo
   CALL v_ch_archivo.openFile(v_ruta_archivo,"r")

   -- los registros del archivo estan separados por pipes
   CALL v_ch_archivo.setDelimiter("|")

   -- se lee el archivo para guardar los datos
   WHILE ( v_ch_archivo.read([v_registro_archivo_str.*]) )

      CALL fn_elimina_espacios(v_registro_archivo_str.*) RETURNING v_registro_archivo.*
      -- se cuenta un registro leido
      LET v_reg_archivo = v_reg_archivo + 1
      IF v_reg_archivo MOD 10000 = 0 THEN 
         DISPLAY "Registros procesados : ", v_reg_archivo
      END IF
      SELECT seq_ret_notifica_gpo.nextval
      INTO   v_id_solicitud_envio
      FROM   systables
      WHERE  tabid = 1;
      LET v_registro_insert.nss                 = v_registro_archivo.nss
      LET v_registro_insert.indicador_cargo     = v_registro_archivo.indicador_cargo
      LET v_registro_insert.importe_total       = v_registro_archivo.importe_total
      LET v_registro_insert.fch_pago            = v_registro_archivo.fch_pago
      LET v_registro_insert.grupo               = v_registro_archivo.grupo
      LET v_registro_insert.valor_aiv           = v_registro_archivo.valor_aiv
      LET v_registro_insert.imp_gob_fed         = v_registro_archivo.imp_gob_fed
      LET v_registro_insert.monto_pagado        = v_registro_archivo.monto_pagado
      LET v_registro_insert.aivs_pagadas        = v_registro_archivo.aivs_pagadas
      LET v_registro_insert.diag_procesar       = v_registro_archivo.diag_procesar
      LET v_registro_insert.tipo_retiro         = v_registro_archivo.tipo_retiro
      LET v_registro_insert.id_solicitud_envio  = v_id_solicitud_envio
      LET v_registro_insert.id_solicitud_retiro = 0
      LET v_registro_insert.estado_solicitud    = 81
      LET v_registro_insert.fch_envio           = v_registro_archivo.fch_pago
      LET v_registro_insert.fch_respuesta       = v_registro_archivo.fch_pago
      
      -- se inserta un registro
      INSERT INTO ret_notifica_gpo_his
           VALUES (v_registro_insert.*);
        
           -- si no hubo error en la insercion
           IF ( SQLCA.SQLCODE = 0 ) THEN
               -- se cuenta un registro aceptado
               LET v_reg_aceptados = v_reg_aceptados + 1
           END IF
   END WHILE

   DISPLAY "Contadores: regs archivo >", v_reg_archivo, "< regs aceptados >", v_reg_aceptados, "<"
   -- si el numero de registros leidos es igual al de aceptados
   IF ( v_reg_archivo = (v_reg_aceptados)) THEN
      -- la carga es correcta
      DISPLAY "La carga se ha realizdo correctamente..."
      
      LET v_mensaje = "El proceso de carga ha finalizado correctamente"
   ELSE
      -- hubo un error en la carga
      DISPLAY "Hubo un error en la carga"
   END IF
END MAIN

FUNCTION fn_valida_datos(p_dato)
DEFINE p_dato      STRING 
DEFINE i           INTEGER 
DEFINE v_char      CHAR(1)
DEFINE v_respuesta STRING 

   LET v_respuesta = ""
   LET i           = 0
   LET v_char      = ""
   --- Valida si no tiene comillas la información
   IF p_dato.getLength() > 0 THEN 
      FOR i = 1 TO p_dato.getLength() 
         IF p_dato.subString(i,i) = ' ' OR (p_dato.subString(i,i) <> '"' AND p_dato.subString(i,i) <> '\'') THEN
            LET v_char =  p_dato.subString(i,i)
            LET v_respuesta = v_respuesta, v_char
         END IF 
      END FOR
   END IF 
   IF v_respuesta CLIPPED  = 'NULL' OR v_respuesta = 'NUL' THEN
      LET v_respuesta = ''
   END IF 

   RETURN v_respuesta 
   
END FUNCTION

  
FUNCTION fn_valida_fecha(p_fecha)
DEFINE p_fecha STRING 
DEFINE v_fecha DATE
DEFINE v_dia, v_mes, v_anio STRING   

--DISPLAY "Fecha ", p_fecha
   LET v_fecha = NULL 
   IF p_fecha.getLength() > 0 THEN
      LET v_dia = p_fecha.subString(1,2)
      LET v_mes = p_fecha.subString(4,5)
      LET v_anio = p_fecha.subString(7,10)
      IF v_dia  IS NULL THEN LET v_dia  = 0 END IF 
      IF v_mes  IS NULL THEN LET v_mes  = 0 END IF
      IF v_anio IS NULL THEN LET v_anio = 0 END IF  
      IF v_dia <> 0 AND v_mes <> 0 AND v_anio <> 0 THEN 
         LET v_fecha = MDY(v_mes,v_dia,v_anio)
      END IF 
   END IF 

RETURN v_fecha

END FUNCTION

FUNCTION fn_elimina_espacios(p_arreglo_str)
DEFINE p_arreglo_str  RECORD    -- registro del archivo
           nss                       STRING,
           indicador_cargo           STRING,
           importe_total             STRING,
           fch_pago                  STRING,
           grupo                     STRING,
           valor_aiv                 STRING,
           imp_gob_fed               STRING,
           monto_pagado              STRING,
           aivs_pagadas              STRING,
           diag_procesar             STRING,
           tipo_retiro               STRING
        END RECORD
DEFINE v_arreglo  RECORD    -- registro del archivo
           nss                       CHAR(11),
           indicador_cargo           CHAR(1),
           importe_total             DECIMAL(14,2),
           fch_pago                  DATE,
           grupo                     CHAR(4),
           valor_aiv                 DECIMAL(15,6),
           imp_gob_fed               DECIMAL(14,2),
           monto_pagado              DECIMAL(14,2),
           aivs_pagadas              DECIMAL(15,6),
           diag_procesar             CHAR(3),
           tipo_retiro               SMALLINT        
        END RECORD

   LET v_arreglo.nss               = p_arreglo_str.nss.trim()
   LET v_arreglo.indicador_cargo   = p_arreglo_str.indicador_cargo.trim()
   LET v_arreglo.importe_total     = p_arreglo_str.importe_total.trim()
   LET v_arreglo.fch_pago          = p_arreglo_str.fch_pago.trim()
   LET v_arreglo.grupo             = p_arreglo_str.grupo.trim()
   LET v_arreglo.valor_aiv         = p_arreglo_str.valor_aiv.trim()
   LET v_arreglo.imp_gob_fed       = p_arreglo_str.imp_gob_fed.trim()
   LET v_arreglo.monto_pagado      = p_arreglo_str.monto_pagado.trim()
   LET v_arreglo.aivs_pagadas      = p_arreglo_str.aivs_pagadas.trim()
   LET v_arreglo.diag_procesar     = p_arreglo_str.diag_procesar.trim()
   LET v_arreglo.tipo_retiro       = p_arreglo_str.tipo_retiro.trim()
        
   RETURN v_arreglo.*
        
END FUNCTION
 
