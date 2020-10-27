#############################################################################
#Módulo          => RET                                                     #        
#Programa        => RETSI17-36                                              #
#Objetivo        => Programa carga de Históricos de Devolución SSV          #
#                => REQUERIMIENTO SACI2017-36                               #
#Fecha Inicio    => Abril 23, 2018                                          #
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

       v_registro_archivo  RECORD    -- registro del archivo
           nss                       CHAR(11),
           estado_solicitud          SMALLINT,
           descripcion               CHAR(25),
           origen                    CHAR(25),
           via_pago                  CHAR(11),
           grupo                     CHAR(04),
           nombre                    CHAR(50),
           ap_paterno                CHAR(40),
           ap_materno                CHAR(40),
           beneficiario              CHAR(15),
           nombre_beneficiario       CHAR(40),
           ap_paterno_beneficiario   CHAR(40),
           ap_materno_beneficiario   CHAR(40),
           curp                      CHAR(18),
           rfc                       CHAR(13),
           entidad                   SMALLINT,
           fecha_tramite             CHAR(23),
           fecha_autorizacion        CHAR(23),
           documento                 CHAR(10),
           ejercicio                 CHAR(04),
           documento_pago            CHAR(10),
           fecha_pago                CHAR(23),
           importe_pago              DECIMAL(22,2),
           referencia_pago           CHAR(25),
           caso_adai                 CHAR(10),
           num_laudo                 CHAR(10),
           num_junta                 CHAR(10),
           importe_pago_anterior     DECIMAL(22,2),
           fch_pago_anterior         CHAR(23),
           clave_banco               CHAR(04),
           cuenta                    CHAR(18),
           importe_transf            DECIMAL(22,2),
           fch_transf                CHAR(23),
           ssv_dif                   DECIMAL(22,2),
           fch_marca_tj              CHAR(23),
           error_fico                CHAR(10),
           cve_afore                 CHAR(03),
           pesos_viv97               DECIMAL(22,2),
           aivs_viv97                DECIMAL(22,2),
           pesos_viv92               DECIMAL(22,2),
           aivs_viv92                DECIMAL(22,2),
           usuario                   CHAR(20),
           cabecera                  CHAR(100),
           acreedor                  CHAR(10),
           via_pago_ac               CHAR(10),
           bloqueo_pago              CHAR(10),
           anulado                   CHAR(10),
           ejercicio_anulacion       CHAR(04),
           fch_carga                 DATE
        END RECORD,
       v_registro_archivo_str  RECORD    -- registro del archivo
           nss                       STRING,
           estado_solicitud          STRING,
           descripcion               STRING,
           origen                    STRING,
           via_pago                  STRING,
           grupo                     STRING,
           nombre                    STRING,
           ap_paterno                STRING,
           ap_materno                STRING,
           beneficiario              STRING,
           nombre_beneficiario       STRING,
           ap_paterno_beneficiario   STRING,
           ap_materno_beneficiario   STRING,
           curp                      STRING,
           rfc                       STRING,
           entidad                   STRING,
           fecha_tramite             STRING,
           fecha_autorizacion        STRING,
           documento                 STRING,
           ejercicio                 STRING,
           documento_pago            STRING,
           fecha_pago                STRING,
           importe_pago              STRING,
           referencia_pago           STRING,
           caso_adai                 STRING,
           num_laudo                 STRING,
           num_junta                 STRING,
           importe_pago_anterior     STRING,
           fch_pago_anterior         STRING,
           clave_banco               STRING,
           cuenta                    STRING,
           importe_transf            STRING,
           fch_transf                STRING,
           ssv_dif                   STRING,
           fch_marca_tj              STRING,
           error_fico                STRING,
           cve_afore                 STRING,
           pesos_viv97               STRING,
           aivs_viv97                STRING,
           pesos_viv92               STRING,
           aivs_viv92                STRING,
           usuario                   STRING,
           cabecera                  STRING,
           acreedor                  STRING,
           via_pago_ac               STRING,
           bloqueo_pago              STRING,
           anulado                   STRING,
           ejercicio_anulacion       STRING,
           fch_carga                 STRING
        END RECORD,
       v_registro_insert  RECORD    -- registro del archivo
           nss                       CHAR(11),
           estado_solicitud          SMALLINT,
           descripcion               CHAR(25),
           origen                    CHAR(25),
           via_pago                  CHAR(11),
           grupo                     CHAR(04),
           nombre                    CHAR(50),
           ap_paterno                CHAR(40),
           ap_materno                CHAR(40),
           beneficiario              CHAR(15),
           nombre_beneficiario       CHAR(40),
           ap_paterno_beneficiario   CHAR(40),
           ap_materno_beneficiario   CHAR(40),
           curp                      CHAR(18),
           rfc                       CHAR(13),
           entidad                   SMALLINT,
           fecha_tramite             DATE,
           fecha_autorizacion        DATE,
           documento                 CHAR(10),
           ejercicio                 CHAR(04),
           documento_pago            CHAR(10),
           fecha_pago                DATE,
           importe_pago              DECIMAL(22,2),
           referencia_pago           CHAR(25),
           caso_adai                 CHAR(10),
           num_laudo                 CHAR(10),
           num_junta                 CHAR(10),
           importe_pago_anterior     DECIMAL(22,2),
           fch_pago_anterior         DATE,
           clave_banco               CHAR(04),
           cuenta                    CHAR(18),
           importe_transf            DECIMAL(22,2),
           fch_transf                DATE,
           ssv_dif                   DECIMAL(22,2),
           fch_marca_tj              DATE,
           error_fico                CHAR(10),
           cve_afore                 CHAR(03),
           pesos_viv97               DECIMAL(22,2),
           aivs_viv97                DECIMAL(22,2),
           pesos_viv92               DECIMAL(22,2),
           aivs_viv92                DECIMAL(22,2),
           usuario                   CHAR(20),
           cabecera                  CHAR(100),
           acreedor                  CHAR(10),
           via_pago_ac               CHAR(10),
           bloqueo_pago              CHAR(10),
           anulado                   CHAR(10),
           ejercicio_anulacion       CHAR(04),
           fch_carga                 DATE
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
   LET v_cadena_registros = p_usuario CLIPPED, ".", "RETSI17-36.log"
   CALL STARTLOG(v_cadena_registros)
   
   LET v_registro_archivo.fch_carga = TODAY 
   LET r_resultado_opera = 0
   LET v_indice = 0   
   LET v_mensaje = " "
   #Recuper la información necesaria para cargar el archivo seleccionado 
   #dependidendo del proceso y operación
                            
   DISPLAY "Inicia carga del archivo histórico de devolución del SSV","\n",v_detalle_monitoreo 
   #Nombre del archivo de monitoreo
   LET v_archivo_monitoreo = "nohup:",p_pid USING "&&&&&",":",p_proceso USING "&&&&&",":",p_operacion USING "&&&&&"
   DISPLAY "========================",v_archivo_monitoreo

   #Se elimina los espacios al final de cada variable
   LET v_ruta_rescate = "/safreviv_int/ret/rescate/"

   DISPLAY "Preparando lectura de archivo : ", p_nom_archivo

   -- se crea el apuntador para apertura y lectura de archivo
   LET v_ch_archivo = base.Channel.create() 

   -- se obtiene la ruta de rescate del proceso
   --   SELECT ruta_rescate
   --   INTO   v_ruta_rescate
   --   FROM   seg_modulo
   --   WHERE  modulo_cod = "ret"

   -- la ruta completa del archivo es la ruta rescate mas el nombre del archivo
   LET v_ruta_archivo = v_ruta_rescate CLIPPED, "/", p_nom_archivo

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
      IF v_reg_archivo MOD 1000 = 0 THEN 
         DISPLAY "Registros procesados : ", v_reg_archivo
      END IF 
      LET v_registro_insert.nss                     = fn_valida_datos(v_registro_archivo.nss)
      LET v_registro_insert.estado_solicitud        = v_registro_archivo.estado_solicitud
      LET v_registro_insert.descripcion             = fn_valida_datos(v_registro_archivo.descripcion)
      LET v_registro_insert.origen                  = fn_valida_datos(v_registro_archivo.origen)
      LET v_registro_insert.via_pago                = fn_valida_datos(v_registro_archivo.via_pago)
      LET v_registro_insert.grupo                   = fn_valida_datos(v_registro_archivo.grupo)
      LET v_registro_insert.nombre                  = fn_valida_datos(v_registro_archivo.nombre)
      LET v_registro_insert.ap_paterno              = fn_valida_datos(v_registro_archivo.ap_paterno)
      LET v_registro_insert.ap_materno              = fn_valida_datos(v_registro_archivo.ap_materno)
      LET v_registro_insert.beneficiario            = fn_valida_datos(v_registro_archivo.beneficiario)
      LET v_registro_insert.nombre_beneficiario     = fn_valida_datos(v_registro_archivo.nombre_beneficiario)
      LET v_registro_insert.ap_paterno_beneficiario = fn_valida_datos(v_registro_archivo.ap_paterno_beneficiario)
      LET v_registro_insert.ap_materno_beneficiario = fn_valida_datos(v_registro_archivo.ap_materno_beneficiario)
      LET v_registro_insert.curp                    = fn_valida_datos(v_registro_archivo.curp)
      LET v_registro_insert.rfc                     = fn_valida_datos(v_registro_archivo.rfc)
      LET v_registro_insert.entidad                 = v_registro_archivo.entidad
      IF v_registro_archivo.fecha_tramite IS NOT NULL AND v_registro_archivo.fecha_tramite <> "NULL" THEN 
         LET v_registro_insert.fecha_tramite           = fn_valida_fecha(v_registro_archivo.fecha_tramite)
      ELSE 
         LET v_registro_insert.fecha_tramite           = NULL
      END IF 
      IF v_registro_archivo.fecha_autorizacion IS NOT NULL AND v_registro_archivo.fecha_autorizacion <> "NULL" THEN
         LET v_registro_insert.fecha_autorizacion      = fn_valida_fecha(v_registro_archivo.fecha_autorizacion)
      ELSE 
         LET v_registro_insert.fecha_autorizacion      = NULL 
      END IF 
      LET v_registro_insert.documento               = fn_valida_datos(v_registro_archivo.documento)
      LET v_registro_insert.ejercicio               = fn_valida_datos(v_registro_archivo.ejercicio)
      LET v_registro_insert.documento_pago          = fn_valida_datos(v_registro_archivo.documento_pago)
      IF v_registro_archivo.fecha_pago IS NOT NULL AND v_registro_archivo.fecha_pago <> "NULL" THEN 
         LET v_registro_insert.fecha_pago              = fn_valida_fecha(v_registro_archivo.fecha_pago)
      ELSE 
         LET v_registro_insert.fecha_pago              = NULL 
      END IF 
      LET v_registro_insert.importe_pago            = v_registro_archivo.importe_pago
      LET v_registro_insert.referencia_pago         = fn_valida_datos(v_registro_archivo.referencia_pago)
      LET v_registro_insert.caso_adai               = fn_valida_datos(v_registro_archivo.caso_adai)
      LET v_registro_insert.num_laudo               = fn_valida_datos(v_registro_archivo.num_laudo)
      LET v_registro_insert.num_junta               = fn_valida_datos(v_registro_archivo.num_junta)
      LET v_registro_insert.importe_pago_anterior   = v_registro_archivo.importe_pago_anterior
      IF v_registro_archivo.fch_pago_anterior IS NOT NULL AND v_registro_archivo.fch_pago_anterior <> "NULL" THEN 
         LET v_registro_insert.fch_pago_anterior       = fn_valida_fecha(v_registro_archivo.fch_pago_anterior)
      ELSE 
         LET v_registro_insert.fch_pago_anterior       = NULL 
      END IF 
      LET v_registro_insert.clave_banco             = fn_valida_datos(v_registro_archivo.clave_banco)
      LET v_registro_insert.cuenta                  = fn_valida_datos(v_registro_archivo.cuenta)
      IF v_registro_archivo.fch_transf IS NOT NULL AND v_registro_archivo.fch_transf <> "NULL" THEN 
         LET v_registro_insert.fch_transf              = fn_valida_fecha(v_registro_archivo.fch_transf)
      ELSE 
         LET v_registro_insert.fch_transf              = NULL 
      END IF 
      IF v_registro_archivo.fch_marca_tj IS NOT NULL AND v_registro_archivo.fch_marca_tj <> "NULL" THEN 
         LET v_registro_insert.fch_marca_tj            = fn_valida_fecha(v_registro_archivo.fch_marca_tj)
      ELSE
         LET v_registro_insert.fch_marca_tj            = NULL 
      END IF 
      LET v_registro_insert.error_fico              = fn_valida_datos(v_registro_archivo.error_fico)
      LET v_registro_insert.cve_afore               = fn_valida_datos(v_registro_archivo.cve_afore)
      LET v_registro_insert.usuario                 = fn_valida_datos(v_registro_archivo.usuario)
      LET v_registro_insert.cabecera                = fn_valida_datos(v_registro_archivo.cabecera)
      LET v_registro_insert.acreedor                = fn_valida_datos(v_registro_archivo.acreedor)
      LET v_registro_insert.via_pago_ac             = fn_valida_datos(v_registro_archivo.via_pago_ac)
      LET v_registro_insert.bloqueo_pago            = fn_valida_datos(v_registro_archivo.bloqueo_pago)
      LET v_registro_insert.anulado                 = fn_valida_datos(v_registro_archivo.anulado)
      LET v_registro_insert.ejercicio_anulacion     = fn_valida_datos(v_registro_archivo.ejercicio_anulacion)
      LET v_registro_insert.importe_transf          = v_registro_archivo.importe_transf
      LET v_registro_insert.ssv_dif                 = v_registro_archivo.ssv_dif
      LET v_registro_insert.pesos_viv92             = v_registro_archivo.pesos_viv92
      LET v_registro_insert.pesos_viv97             = v_registro_archivo.pesos_viv97
      LET v_registro_insert.aivs_viv92              = v_registro_archivo.aivs_viv92
      LET v_registro_insert.aivs_viv97              = v_registro_archivo.aivs_viv97
      
      LET v_registro_insert.fch_carga               = TODAY 

--      DISPLAY "Reg Original >", v_registro_archivo.*, "<"
--      DISPLAY "Registro >",v_registro_insert.*, "<"
      
      -- se inserta un registro
      INSERT INTO ret_ley73_hist 
           VALUES (v_registro_insert.*);
        
           -- si no hubo error en la insercion
           IF ( SQLCA.SQLCODE = 0 ) THEN
               -- se cuenta un registro aceptado
               LET v_reg_aceptados = v_reg_aceptados + 1
           END IF
   END WHILE

   DISPLAY "Contadores: regs archivo >", v_reg_archivo, "< regs aceptados >", v_reg_aceptados + 3, "<"
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
      LET v_dia = p_fecha.subString(4,5)
      LET v_mes = p_fecha.subString(1,2)
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
           estado_solicitud          STRING,
           descripcion               STRING,
           origen                    STRING,
           via_pago                  STRING,
           grupo                     STRING,
           nombre                    STRING,
           ap_paterno                STRING,
           ap_materno                STRING,
           beneficiario              STRING,
           nombre_beneficiario       STRING,
           ap_paterno_beneficiario   STRING,
           ap_materno_beneficiario   STRING,
           curp                      STRING,
           rfc                       STRING,
           entidad                   STRING,
           fecha_tramite             STRING,
           fecha_autorizacion        STRING,
           documento                 STRING,
           ejercicio                 STRING,
           documento_pago            STRING,
           fecha_pago                STRING,
           importe_pago              STRING,
           referencia_pago           STRING,
           caso_adai                 STRING,
           num_laudo                 STRING,
           num_junta                 STRING,
           importe_pago_anterior     STRING,
           fch_pago_anterior         STRING,
           clave_banco               STRING,
           cuenta                    STRING,
           importe_transf            STRING,
           fch_transf                STRING,
           ssv_dif                   STRING,
           fch_marca_tj              STRING,
           error_fico                STRING,
           cve_afore                 STRING,
           pesos_viv97               STRING,
           aivs_viv97                STRING,
           pesos_viv92               STRING,
           aivs_viv92                STRING,
           usuario                   STRING,
           cabecera                  STRING,
           acreedor                  STRING,
           via_pago_ac               STRING,
           bloqueo_pago              STRING,
           anulado                   STRING,
           ejercicio_anulacion       STRING,
           fch_carga                 STRING
        END RECORD
DEFINE v_arreglo  RECORD    -- registro del archivo
           nss                       CHAR(11),
           estado_solicitud          SMALLINT,
           descripcion               CHAR(25),
           origen                    CHAR(25),
           via_pago                  CHAR(11),
           grupo                     CHAR(04),
           nombre                    CHAR(50),
           ap_paterno                CHAR(40),
           ap_materno                CHAR(40),
           beneficiario              CHAR(15),
           nombre_beneficiario       CHAR(40),
           ap_paterno_beneficiario   CHAR(40),
           ap_materno_beneficiario   CHAR(40),
           curp                      CHAR(18),
           rfc                       CHAR(13),
           entidad                   SMALLINT,
           fecha_tramite             CHAR(23),
           fecha_autorizacion        CHAR(23),
           documento                 CHAR(10),
           ejercicio                 CHAR(04),
           documento_pago            CHAR(10),
           fecha_pago                CHAR(23),
           importe_pago              DECIMAL(22,2),
           referencia_pago           CHAR(25),
           caso_adai                 CHAR(10),
           num_laudo                 CHAR(10),
           num_junta                 CHAR(10),
           importe_pago_anterior     DECIMAL(22,2),
           fch_pago_anterior         CHAR(23),
           clave_banco               CHAR(04),
           cuenta                    CHAR(18),
           importe_transf            DECIMAL(22,2),
           fch_transf                CHAR(23),
           ssv_dif                   DECIMAL(22,2),
           fch_marca_tj              CHAR(23),
           error_fico                CHAR(10),
           cve_afore                 CHAR(03),
           pesos_viv97               DECIMAL(22,2),
           aivs_viv97                DECIMAL(22,2),
           pesos_viv92               DECIMAL(22,2),
           aivs_viv92                DECIMAL(22,2),
           usuario                   CHAR(20),
           cabecera                  CHAR(100),
           acreedor                  CHAR(10),
           via_pago_ac               CHAR(10),
           bloqueo_pago              CHAR(10),
           anulado                   CHAR(10),
           ejercicio_anulacion       CHAR(04),
           fch_carga                 DATE
        END RECORD

   LET v_arreglo.nss                       = p_arreglo_str.nss.trim()
   LET v_arreglo.estado_solicitud          = p_arreglo_str.estado_solicitud.trim()
   LET v_arreglo.descripcion               = p_arreglo_str.descripcion.trim()
   LET v_arreglo.origen                    = p_arreglo_str.origen.trim()
   LET v_arreglo.via_pago                  = p_arreglo_str.via_pago.trim()
   LET v_arreglo.grupo                     = p_arreglo_str.grupo.trim()
   LET v_arreglo.nombre                    = p_arreglo_str.nombre.trim()
   LET v_arreglo.ap_paterno                = p_arreglo_str.ap_paterno.trim()
   LET v_arreglo.ap_materno                = p_arreglo_str.ap_materno.trim()
   LET v_arreglo.beneficiario              = p_arreglo_str.beneficiario.trim()
   LET v_arreglo.nombre_beneficiario       = p_arreglo_str.nombre_beneficiario.trim()
   LET v_arreglo.ap_paterno_beneficiario   = p_arreglo_str.ap_paterno_beneficiario.trim()
   LET v_arreglo.ap_materno_beneficiario   = p_arreglo_str.ap_materno_beneficiario.trim()
   LET v_arreglo.curp                      = p_arreglo_str.curp.trim()
   LET v_arreglo.rfc                       = p_arreglo_str.rfc.trim()
   LET v_arreglo.entidad                   = p_arreglo_str.entidad.trim()
   LET v_arreglo.fecha_tramite             = p_arreglo_str.fecha_tramite.trim()
   LET v_arreglo.fecha_autorizacion        = p_arreglo_str.fecha_autorizacion.trim()
   LET v_arreglo.documento                 = p_arreglo_str.documento.trim()
   LET v_arreglo.ejercicio                 = p_arreglo_str.ejercicio.trim()
   LET v_arreglo.documento_pago            = p_arreglo_str.documento_pago.trim()
   LET v_arreglo.fecha_pago                = p_arreglo_str.fecha_pago.trim()
   LET v_arreglo.importe_pago              = p_arreglo_str.importe_pago.trim()
   LET v_arreglo.referencia_pago           = p_arreglo_str.referencia_pago.trim()
   LET v_arreglo.caso_adai                 = p_arreglo_str.caso_adai.trim()
   LET v_arreglo.num_laudo                 = p_arreglo_str.num_laudo.trim()
   LET v_arreglo.num_junta                 = p_arreglo_str.num_junta.trim()
   LET v_arreglo.importe_pago_anterior     = p_arreglo_str.importe_pago_anterior.trim()
   LET v_arreglo.fch_pago_anterior         = p_arreglo_str.fch_pago_anterior.trim()
   LET v_arreglo.clave_banco               = p_arreglo_str.clave_banco.trim()
   LET v_arreglo.cuenta                    = p_arreglo_str.cuenta.trim()
   LET v_arreglo.importe_transf            = p_arreglo_str.importe_transf.trim()
   LET v_arreglo.fch_transf                = p_arreglo_str.fch_transf.trim()
   LET v_arreglo.ssv_dif                   = p_arreglo_str.ssv_dif.trim()
   LET v_arreglo.fch_marca_tj              = p_arreglo_str.fch_marca_tj.trim()
   LET v_arreglo.error_fico                = p_arreglo_str.error_fico.trim()
   LET v_arreglo.cve_afore                 = p_arreglo_str.cve_afore.trim()
   LET v_arreglo.pesos_viv97               = p_arreglo_str.pesos_viv97.trim()
   LET v_arreglo.aivs_viv97                = p_arreglo_str.aivs_viv97.trim()
   LET v_arreglo.pesos_viv92               = p_arreglo_str.pesos_viv92.trim()
   LET v_arreglo.aivs_viv92                = p_arreglo_str.aivs_viv92.trim()
   LET v_arreglo.usuario                   = p_arreglo_str.usuario.trim()
   LET v_arreglo.cabecera                  = p_arreglo_str.cabecera.trim()
   LET v_arreglo.acreedor                  = p_arreglo_str.acreedor.trim()
   LET v_arreglo.via_pago_ac               = p_arreglo_str.via_pago_ac.trim()
   LET v_arreglo.bloqueo_pago              = p_arreglo_str.bloqueo_pago.trim()
   LET v_arreglo.anulado                   = p_arreglo_str.anulado.trim()
   LET v_arreglo.ejercicio_anulacion       = p_arreglo_str.ejercicio_anulacion.trim()
   LET v_arreglo.fch_carga                 = p_arreglo_str.fch_carga.trim()
        
   RETURN v_arreglo.*
        
END FUNCTION
 
