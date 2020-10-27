--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 09/12/2013
--==============================================================================

################################################################################
#Módulo          => PRT                                                        #
#Programa        => PRTC02                                                     #
#Objetivo        => Consulta de tramites                                       #
#Fecha Inicio    => 27 Febrero 2015                                            #
################################################################################

DATABASE safre_viv

DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod,
       v_ruta_ejecutables   LIKE seg_modulo.ruta_bin,
       v_ruta_listados      LIKE seg_modulo.ruta_listados,
       g_detalle_bloque_solicitud DYNAMIC ARRAY OF RECORD
         v_id_col_entidad     DECIMAL(9,0),
         v_id_col_cat_entidad DECIMAL(9,0),
         v_etiqueta_det_sol   LIKE bus_detalle_solicitud.nombre_campo,
         v_etiqueta           LIKE bus_detalle_solicitud.nombre_campo,
         v_valor              LIKE bus_detalle_solicitud.valor
       END RECORD
       
DEFINE p_pid             LIKE bat_ctr_proceso.pid            ,
       p_proceso_cod     LIKE bat_ctr_proceso.proceso_cod    ,
       p_opera_cod       LIKE bat_ctr_operacion.opera_cod    ,
       p_folio           LIKE glo_ctr_archivo.folio          ,
       p_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo ,
       r_resultado_opera SMALLINT
       
################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función principal e inicialización de variables          #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
MAIN

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nom_archivo    = ARG_VAL(6) 
   
   # prueba con bd
   -- DATABASE safre_busd

   # Recupera las rutas del módulo BUS
   SELECT a.ruta_bin ,
          a.ruta_listados 
   INTO v_ruta_ejecutables ,
        v_ruta_listados 
   FROM seg_modulo a 
   WHERE a.modulo_cod = "prt"
   
   # inicializa las consultas utilizadas en el programa
   CALL fn_inicializa_consultas()
   
   # llama ventana para elegir modulo
   CALL fn_consulta_tramite()
   CALL fn_actualiza_opera_fin(p_pid,
                               p_proceso_cod,
                               p_opera_cod) RETURNING r_resultado_opera
   # si ocurri? un error con la actualizacion de la operacion operacion
   # muestra el mensaje
   IF( r_resultado_opera )THEN
      CALL fn_desplega_inc_operacion(r_resultado_opera)
   ELSE
      # Env?o de correo de notificaci?n de proceso finalizado
      CALL fn_correo_proceso(p_pid,
                             p_proceso_cod,
                             p_opera_cod,
                             '',
                             'GENERACIÓN INFORME REENVIOS PORTABILIDAD',
                             'ID Proceso   : '||p_pid||
                             'Proceso      : '||p_proceso_cod||
                             'Operacion    : '||p_opera_cod||
                             'Fecha Inicio : '||DATE||
                             'Fecha Fin    : '||DATE
                             )
   END IF


                               
END MAIN

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función principal e inicialización de variables          #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   LET v_consulta = " SELECT FIRST 1 modulo_desc",
                    "   FROM seg_modulo",
                    "  WHERE modulo_cod = ?",
                    "  ORDER BY 1"
   PREPARE prp_recupera_desc_modulo FROM v_consulta

   LET v_consulta = " SELECT desc_proceso_bus",    
                    "   FROM cat_bus_proceso",
                    "  WHERE id_cat_bus_proceso = ?"
   PREPARE prp_recupera_desc_proceso FROM v_consulta

   LET v_consulta = " SELECT desc_opera_bus",
                    "   FROM cat_bus_operacion",
                    "  WHERE id_cat_bus_operacion = ?"
   PREPARE prp_recupera_desc_operacion FROM v_consulta

   LET v_consulta = " SELECT desc_contrato",
                    "   FROM cat_bus_contrato",
                    "  WHERE id_cat_bus_contrato = ?"
   PREPARE prp_recupera_desc_contrato FROM v_consulta 

   # verifica si existe el folio a consultar
   LET v_consulta = " SELECT FIRST 1 NVL(1,0)",
                    "   FROM bus_tramite",
                    "  WHERE folio_procesar = ?"
   PREPARE prp_existe_folio_porcesar FROM v_consulta

   
   

   # funcion para recuperar las etiquetas de una entidad(tabla) y su valor
   LET v_consulta = "EXECUTE FUNCTION fn_glo_recupera_etiquetas_id(?,?,?,?,?,?,?)"
   PREPARE prp_recupera_etiquetas FROM v_consulta

   # funcion para recuperar las etiquetas de una entidad(tabla) y su valor
   LET v_consulta = "EXECUTE FUNCTION fn_prt_recupera_etiquetas_bloque(?,?,?,?,?,?,?)"
   PREPARE prp_recupera_etiquetas_bloque FROM v_consulta
   
   # Recupera los errores de solicitud
   LET v_consulta = " SELECT cod_error,",
                    "        desc_error",
                    "   FROM bus_error_solicitud",
                    "  WHERE id_bus_solicitud_tramite = ?"
   PREPARE prp_rec_errores_solicitud FROM v_consulta
   
   # Recupera los errores de respuesta
   LET v_consulta = " SELECT cod_error,",
                    "        desc_error",
                    "   FROM bus_error_respuesta",
                    "  WHERE id_bus_respuesta_tramite = ?"
   PREPARE prp_rec_errores_respuesta FROM v_consulta
   
   # Recupera los módulos
   LET v_consulta = " SELECT modulo_cod,",
                    "        modulo_desc",
                    "   FROM seg_modulo",
                    "  WHERE 1 = 1",
                    "  ORDER BY modulo_desc"
   PREPARE prp_recupera_modulos FROM v_consulta
   
   # Recupera datos del proceso
   LET v_consulta = " SELECT id_cat_bus_proceso,",
                    "        desc_proceso_bus",
                    "   FROM cat_bus_proceso",
                    "  WHERE modulo_cod = ?"
   PREPARE prp_recupera_proceso FROM v_consulta 
   
   # Recupera datos de la operación
   LET v_consulta = " SELECT id_cat_bus_operacion,",
                    "        desc_opera_bus",
                    "   FROM cat_bus_operacion",
                    "  WHERE id_cat_bus_proceso = ?"
   PREPARE prp_recupera_operacion FROM v_consulta
   
   # Recupera datos del contrato
   LET v_consulta = " SELECT id_cat_bus_contrato,",
                    "        desc_contrato",
                    "   FROM cat_bus_contrato",
                    "  WHERE id_cat_bus_operacion = ?"
   PREPARE prp_recupera_contrato FROM v_consulta
   
   # Recupera id de bloque relacionado al contrato. La relacion es uno a uno
   LET v_consulta = " SELECT id_cat_bus_bloque",
                    "   FROM cat_bus_bloque",
                    "  WHERE id_cat_bus_detalle_contrato = ?"
   PREPARE prp_recupera_id_cat_bloque FROM v_consulta

END FUNCTION


################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC01                                                   #
#Descripcion       => Función elegir tipo de consulta del tramite              #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                         #
################################################################################
FUNCTION fn_consulta_tramite()


      CALL fn_consulta_tramite_bus()

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para recuperar las descripciones de modulo,      #
#                     proceso, operación y contrato                            #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_recupera_descripciones_modulo(p_modulo_cod,
                                          p_id_cat_bus_proceso,
                                          p_id_cat_bus_operacion,
                                          p_id_cat_bus_contrato)
DEFINE p_modulo_cod           LIKE cat_bus_proceso.modulo_cod, 
       p_id_cat_bus_proceso   LIKE cat_bus_proceso.id_cat_bus_proceso,
       p_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
       p_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato,
       v_modulo_desc          LIKE seg_modulo.modulo_desc,
       v_proceso_desc         LIKE cat_bus_proceso.desc_proceso_bus,
       v_operacio_desc        LIKE cat_bus_operacion.desc_opera_bus,
       v_contrato_desc        LIKE cat_bus_contrato.desc_contrato,
       v_error_sql  INTEGER,
       v_error_isam INTEGER,
       v_msg_sql    CHAR(254)


   # Recupera descipción del módulo
   EXECUTE prp_recupera_desc_modulo USING p_modulo_cod
                                     INTO v_modulo_desc

   # Recupera descipción del proceso
   EXECUTE prp_recupera_desc_proceso USING p_id_cat_bus_proceso
                                      INTO v_proceso_desc  

   # Recupera descipción de la operación
   EXECUTE prp_recupera_desc_operacion USING p_id_cat_bus_operacion
                                        INTO v_operacio_desc

   # Recupera descipción del contrato
   EXECUTE prp_recupera_desc_contrato USING p_id_cat_bus_contrato
                                       INTO v_contrato_desc   

    RETURN v_modulo_desc,
           v_proceso_desc,
           v_operacio_desc,
           v_contrato_desc
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para verificar si existe el folio capturado      #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                        #
################################################################################
FUNCTION fn_consulta_tramite_bus()

DEFINE r_folio_procesar LIKE bus_tramite.folio_procesar,
       p_nss_consulta   CHAR(11),
       r_tramite DYNAMIC ARRAY OF RECORD
         v_consecutivo       INTEGER,--LIKE bus_tramite.id_bus_tramite,
         v_folio_procesar    LIKE bus_tramite.folio_procesar,
         v_origen            VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso      LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion    LIKE cat_bus_operacion.desc_opera_bus,         
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid      LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss               LIKE bus_solicitud_tramite.nss,
         v_curp              LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran     LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran        CHAR(19),--LIKE bus_solicitud_tramite.f_operacion,
         v_id_respuesta      LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack         LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack         CHAR(19),--LIKE bus_respuesta_tramite.f_operacion,
         v_cod_resp_ack      LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack  LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta    LIKE bus_respuesta_tramite.desc_respuesta,
         v_cod_ope_cliente   LIKE bus_respuesta_tramite.cod_oper_cliente,
         v_excepcion         LIKE bus_excepcion.descripcion
       END RECORD,
       r_detalle_solicitud DYNAMIC ARRAY OF RECORD
         v_id_col_entidad     DECIMAL(9,0),
         v_id_col_cat_entidad DECIMAL(9,0),
         v_etiqueta LIKE bus_detalle_solicitud.nombre_campo,
         v_valor    LIKE bus_detalle_solicitud.valor
       END RECORD,
       r_detalle_bloque DYNAMIC ARRAY OF RECORD
         v_id_col_entidad     DECIMAL(9,0),
         v_id_col_cat_entidad DECIMAL(9,0),
         v_etiqueta LIKE bus_detalle_solicitud.nombre_campo,
         v_valor    LIKE bus_detalle_solicitud.valor
       END RECORD,
       r_errores_solicitud DYNAMIC ARRAY OF RECORD
         v_codigo      LIKE bus_error_solicitud.cod_error,
         v_descripcion LIKE bus_error_solicitud.desc_error
       END RECORD,
       r_errores_respuesta DYNAMIC ARRAY OF RECORD
         v_codigo      LIKE bus_error_respuesta.cod_error,
         v_descripcion LIKE bus_error_respuesta.desc_error
       END RECORD,
       v_id_col_cat_bloque DECIMAL(9,0),
       v_indice   SMALLINT,
       v_mensaje  STRING,
       v_error    BOOLEAN

            CALL fn_recupera_tramite() RETURNING r_tramite
            IF( r_tramite.getLength() = 0 )THEN
               DISPLAY "No se encontraron registros con criterio dado"
            END IF

            CALL fn_genera_reporte_traza_bitacora("",
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  r_tramite)
            
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para verificar si existe el folio capturado      #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_recupera_tramite()

DEFINE p_folio_procesar LIKE bus_tramite.folio_procesar,
       p_nss_consulta   CHAR(11),
       v_tramite RECORD
         v_id_bus_tramite    LIKE bus_tramite.id_bus_tramite,
         v_folio_procesar    LIKE bus_tramite.folio_procesar,
         v_origen            VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso      LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion    LIKE cat_bus_operacion.desc_opera_bus,
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid      LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss               LIKE bus_solicitud_tramite.nss,
         v_curp              LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran     LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran        LIKE bus_solicitud_tramite.f_operacion,
         v_h_operacion_tran  LIKE bus_solicitud_tramite.h_operacion,
         v_id_respuesta      LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack         LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack         LIKE bus_respuesta_tramite.f_operacion,
         v_h_operacion_ack   LIKE bus_respuesta_tramite.h_operacion,
         v_cod_resp_ack      LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack  LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta    LIKE bus_respuesta_tramite.desc_respuesta,
         v_cod_ope_cliente   LIKE bus_respuesta_tramite.cod_oper_cliente,
         v_excepcion         LIKE bus_excepcion.descripcion
       END RECORD,
       v_tramites DYNAMIC ARRAY OF RECORD
         v_consecutivo         INTEGER,--LIKE bus_tramite.id_bus_tramite,
         v_folio_procesar      LIKE bus_tramite.folio_procesar,
         v_origen              VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso        LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion      LIKE cat_bus_operacion.desc_opera_bus,
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid        LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion   LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss                 LIKE bus_solicitud_tramite.nss,
         v_curp                LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran       LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran          CHAR(19),--LIKE bus_solicitud_tramite.f_operacion,
         v_id_respuesta        LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack           LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack           CHAR(19),--LIKE bus_respuesta_tramite.f_operacion,
         v_cod_resp_ack        LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack    LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta      LIKE bus_respuesta_tramite.desc_respuesta,
         v_cod_ope_cliente     LIKE bus_respuesta_tramite.cod_oper_cliente,
         v_excepcion           LIKE bus_excepcion.descripcion
       END RECORD,
       v_indice   SMALLINT,
       v_consulta STRING ,
       g_hoy      DATE

   LET v_indice = 0 
   CALL v_tramites.clear()

   # Recupera el tramite de la solicitud
   LET v_consulta = "\n SELECT DISTINCT tra.id_bus_tramite,",
                    "\n        tra.folio_procesar,",
                    "\n        CASE tra.origen",
                    "\n          WHEN 0 THEN 'PROCESAR'",
                    "\n          WHEN 1 THEN 'AFORE'",
                    "\n        END CASE,",
                    "\n        pro.desc_proceso_bus,",
                    "\n        ope.desc_opera_bus,",                    
                    "\n        con.id_cat_bus_contrato,",
                    "\n        con.desc_contrato,",
                    "\n        sol.id_bus_solicitud_tramite,",
                    "\n        sol.folio_transaccion,",
                    "\n        sol.nss,",
                    "\n        sol.curp,",
                    "\n        sol.cod_respuesta_opr,",
                    "\n        sol.f_operacion,",
                    "\n        sol.h_operacion,",
                    "\n        res.id_bus_respuesta_tramite,",
                    "\n        res.folio_ack,",
                    "\n        res.f_operacion,",
                    "\n        res.h_operacion,",
                    "\n        res.cod_respuesta,",
                    "\n        res.cod_respuesta_opr,",
                    "\n        res.desc_respuesta,",
                    "\n        res.cod_oper_cliente,",
                    "\n        exc.descripcion",
                    "\n   FROM bus_tramite tra LEFT OUTER JOIN cat_bus_contrato con",
                    "\n     ON con.id_cat_bus_contrato = tra.id_cat_bus_contrato",
                    "\n        LEFT OUTER JOIN cat_bus_operacion ope",
                    "\n     ON ope.id_cat_bus_operacion = con.id_cat_bus_operacion",
                    "\n        LEFT OUTER JOIN cat_bus_proceso pro",
                    "\n     ON pro.id_cat_bus_proceso = ope.id_cat_bus_proceso",
                    "\n        LEFT OUTER JOIN bus_solicitud_tramite sol",
                    "\n     ON sol.id_bus_tramite = tra.id_bus_tramite",
                    "\n        LEFT OUTER JOIN bus_excepcion exc", 
                    "\n     ON sol.id_bus_solicitud_tramite = exc.id_bus_solicitud_tramite",
                    "\n        LEFT OUTER JOIN bus_respuesta_tramite res",
                    "\n     ON res.id_bus_tramite = tra.id_bus_tramite"
      LET v_consulta = v_consulta," WHERE sol.nss  in ",
                                  " ( SELECT unique b.nss ",
                                  "   FROM prt_his_id_folio a, ",
                                  "        prt_solicitud_cedente b ",                                  
                                  "   WHERE a.tpo_solicitud = 1 ",
                                  "   AND   a.f_actualiza = ?   ",
                                  "   AND   a.id_prt_origen = b.id_prt_solicitud_cedente )"
        
   LET g_hoy = TODAY
   PREPARE prp_recupera_tramite FROM v_consulta
   DECLARE cur_recupera_tramite CURSOR FOR prp_recupera_tramite
   FOREACH cur_recupera_tramite USING g_hoy
                                INTO v_tramite.*
      LET v_indice = v_indice + 1
      
      LET v_tramites[v_indice].v_consecutivo         = v_indice --v_tramite.v_id_bus_tramite
      LET v_tramites[v_indice].v_folio_procesar      = v_tramite.v_folio_procesar 
      LET v_tramites[v_indice].v_origen              = v_tramite.v_origen 
      LET v_tramites[v_indice].v_desc_proceso        = v_tramite.v_desc_proceso
      LET v_tramites[v_indice].v_desc_operacion      = v_tramite.v_desc_operacion
      LET v_tramites[v_indice].v_id_cat_bus_contrato = v_tramite.v_id_cat_bus_contrato
      LET v_tramites[v_indice].v_desc_contrato = v_tramite.v_desc_contrato
      LET v_tramites[v_indice].v_id_solicitid        = v_tramite.v_id_solicitid
      LET v_tramites[v_indice].v_folio_transaccion   = v_tramite.v_folio_transaccion
      LET v_tramites[v_indice].v_nss                 = v_tramite.v_nss
      LET v_tramites[v_indice].v_curp                = v_tramite.v_curp
      LET v_tramites[v_indice].v_cod_resp_tran       = v_tramite.v_cod_resp_tran
      LET v_tramites[v_indice].v_fecha_tran          = v_tramite.v_fecha_tran USING "dd-mm-yyyy"," ",v_tramite.v_h_operacion_tran
      LET v_tramites[v_indice].v_id_respuesta        = v_tramite.v_id_respuesta
      LET v_tramites[v_indice].v_folio_ack           = v_tramite.v_folio_ack
      LET v_tramites[v_indice].v_fecha_ack           = v_tramite.v_fecha_ack USING "dd-mm-yyyy"," ",v_tramite.v_h_operacion_ack
      LET v_tramites[v_indice].v_cod_resp_ack        = v_tramite.v_cod_resp_ack
      LET v_tramites[v_indice].v_cod_resp_ope_ack    = v_tramite.v_cod_resp_ope_ack
      LET v_tramites[v_indice].v_desc_respuesta      = v_tramite.v_desc_respuesta
      LET v_tramites[v_indice].v_cod_ope_cliente     = v_tramite.v_cod_ope_cliente
      LET v_tramites[v_indice].v_excepcion           = v_tramite.v_excepcion
DISPLAY v_tramites[v_indice].v_folio_procesar
   END FOREACH
   
   FREE cur_recupera_tramite
   
   RETURN v_tramites
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para recuperar los errores de solicitud          #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_recupera_errores_solicitud(p_id_bus_solicitud_tramite)
DEFINE p_id_bus_solicitud_tramite LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
       v_errores DYNAMIC ARRAY OF RECORD
         v_codigo      LIKE bus_error_solicitud.cod_error,
         v_descripcion LIKE bus_error_solicitud.desc_error
       END RECORD,
       v_error RECORD
         v_codigo      LIKE bus_error_solicitud.cod_error,
         v_descripcion LIKE bus_error_solicitud.desc_error
       END RECORD,
       v_indice SMALLINT

   LET v_indice = 1
   DECLARE cur_rec_errores_solicitud CURSOR FOR prp_rec_errores_solicitud
   FOREACH cur_rec_errores_solicitud USING p_id_bus_solicitud_tramite
                                      INTO v_error.*
      LET v_errores[v_indice].v_codigo      = v_error.v_codigo
      LET v_errores[v_indice].v_descripcion = v_error.v_descripcion
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_rec_errores_solicitud
   
   RETURN v_errores
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para recuperar los errores de respuesta          #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_recupera_errores_respuesta(p_id_bus_respuesta_tramite)
DEFINE p_id_bus_respuesta_tramite LIKE bus_error_respuesta.id_bus_respuesta_tramite,
       v_errores DYNAMIC ARRAY OF RECORD
         v_codigo      LIKE bus_error_respuesta.cod_error,
         v_descripcion LIKE bus_error_respuesta.desc_error
       END RECORD,
       v_error RECORD
         v_codigo      LIKE bus_error_respuesta.cod_error,
         v_descripcion LIKE bus_error_respuesta.desc_error
       END RECORD,
       v_indice SMALLINT

   LET v_indice = 1
   DECLARE cur_rec_errores_respuesta CURSOR FOR prp_rec_errores_respuesta
   FOREACH cur_rec_errores_respuesta USING p_id_bus_respuesta_tramite
                                      INTO v_error.*
      LET v_errores[v_indice].v_codigo      = v_error.v_codigo
      LET v_errores[v_indice].v_descripcion = v_error.v_descripcion
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_rec_errores_respuesta
   
   RETURN v_errores
END FUNCTION


################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Reporte de tramite bus                                   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_genera_reporte_traza_bitacora(p_modulo_desc,
                                          p_proceso_desc,
                                          p_operacion_desc,
                                          p_contrato_desc,
                                          p_f_operacion_ini,
                                          p_f_operacion_fin,
                                          p_f_operacion_ack_ini,
                                          p_f_operacion_ack_fin,
                                          p_nss,
                                          p_curp,
                                          p_cod_transaccion_opr,
                                          p_cod_respuesta,
                                          p_cod_respuesta_opr,
                                          p_tramites)
DEFINE p_modulo_desc   LIKE seg_modulo.modulo_desc,
       p_proceso_desc  LIKE cat_bus_proceso.desc_proceso_bus,
       p_operacion_desc LIKE cat_bus_operacion.desc_opera_bus,
       p_contrato_desc LIKE cat_bus_contrato.desc_contrato,
       p_f_operacion_ini      LIKE bus_solicitud_tramite.f_operacion,
       p_f_operacion_fin      LIKE bus_solicitud_tramite.f_operacion,
       p_f_operacion_ack_ini  LIKE bus_respuesta_tramite.f_operacion,
       p_f_operacion_ack_fin  LIKE bus_respuesta_tramite.f_operacion,
       p_nss                  LIKE bus_solicitud_tramite.nss,
       p_curp                 LIKE bus_solicitud_tramite.curp,
       p_cod_transaccion_opr  LIKE bus_solicitud_tramite.cod_respuesta_opr,
       p_cod_respuesta        LIKE bus_respuesta_tramite.cod_respuesta,
       p_cod_respuesta_opr    LIKE bus_respuesta_tramite.cod_respuesta_opr,
       v_nom_reporte   STRING,
       v_manejador_rpt OM.SaxDocumentHandler,
       p_tramites DYNAMIC ARRAY OF RECORD
         v_consecutivo         INTEGER,--LIKE bus_tramite.id_bus_tramite,
         v_folio_procesar      LIKE bus_tramite.folio_procesar,
         v_origen              VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso        LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion      LIKE cat_bus_operacion.desc_opera_bus,
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid        LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion   LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss                 LIKE bus_solicitud_tramite.nss,
         v_curp                LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran       LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran          CHAR(19),--LIKE bus_solicitud_tramite.f_operacion,
         v_id_respuesta        LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack           LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack           CHAR(19),--LIKE bus_respuesta_tramite.f_operacion,
         v_cod_resp_ack        LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack    LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta      LIKE bus_respuesta_tramite.desc_respuesta,
         v_cod_ope_cliente     LIKE bus_respuesta_tramite.cod_oper_cliente,
         v_excepcion           LIKE bus_excepcion.descripcion
       END RECORD,
       v_indice  SMALLINT

   IF( fgl_report_loadCurrentSettings(v_ruta_ejecutables CLIPPED ||"/PRTI11.4rp") )THEN

      CALL fgl_report_selectDevice("PDF")
      
     LET v_nom_reporte = p_usuario_cod CLIPPED, "-",
                             "PRTI11","-",
                             p_pid USING "&&&&&", "-",
                             p_proceso_cod USING "&&&&&", "-",
                             p_opera_cod   USING "&&&&&"

                          
      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(v_ruta_listados CLIPPED||"/"||v_nom_reporte)
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
      
      START REPORT rpt_consultas_traza TO XML HANDLER v_manejador_rpt
         FOR v_indice = 1 TO p_tramites.getLength()
            OUTPUT TO REPORT rpt_consultas_traza(p_modulo_desc,
                                                 p_proceso_desc,
                                                 p_operacion_desc,
                                                 p_contrato_desc,
                                                 p_f_operacion_ini,
                                                 p_f_operacion_fin,
                                                 p_f_operacion_ack_ini,
                                                 p_f_operacion_ack_fin,
                                                 p_nss,
                                                 p_curp,
                                                 p_cod_transaccion_opr,
                                                 p_cod_respuesta,
                                                 p_cod_respuesta_opr,
                                                 v_indice,
                                                 p_tramites[v_indice].*)
         END FOR

      FINISH REPORT rpt_consultas_traza
   ELSE
      DISPLAY "No fue posible generar el reporte"

   END IF

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Reporte de tramite bus                                   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
REPORT rpt_consultas_traza(p_modulo_desc,
                           p_proceso_desc,
                           p_operacion_desc,
                           p_contrato_desc,
                           p_f_operacion_ini,
                           p_f_operacion_fin,
                           p_f_operacion_ack_ini,
                           p_f_operacion_ack_fin,
                           p_nss,
                           p_curp,
                           p_cod_transaccion_opr,
                           p_cod_respuesta,
                           p_cod_respuesta_opr,
                           p_indice,
                           p_tramite)
DEFINE p_modulo_desc          LIKE seg_modulo.modulo_desc,
       p_proceso_desc         LIKE cat_bus_proceso.desc_proceso_bus,
       p_operacion_desc       LIKE cat_bus_operacion.desc_opera_bus,
       p_contrato_desc        LIKE cat_bus_contrato.desc_contrato,
       p_f_operacion_ini      LIKE bus_solicitud_tramite.f_operacion,
       p_f_operacion_fin      LIKE bus_solicitud_tramite.f_operacion,
       p_f_operacion_ack_ini  LIKE bus_respuesta_tramite.f_operacion,
       p_f_operacion_ack_fin  LIKE bus_respuesta_tramite.f_operacion,
       p_nss                  LIKE bus_solicitud_tramite.nss,
       p_curp                 LIKE bus_solicitud_tramite.curp,
       p_cod_transaccion_opr  LIKE bus_solicitud_tramite.cod_respuesta_opr,
       p_cod_respuesta        LIKE bus_respuesta_tramite.cod_respuesta,
       p_cod_respuesta_opr    LIKE bus_respuesta_tramite.cod_respuesta_opr,
       p_indice               SMALLINT,
       p_tramite RECORD
         v_consecutivo         INTEGER,--LIKE bus_tramite.id_bus_tramite,
         v_folio_procesar      LIKE bus_tramite.folio_procesar,
         v_origen              VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso        LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion      LIKE cat_bus_operacion.desc_opera_bus,
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid        LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion   LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss                 LIKE bus_solicitud_tramite.nss,
         v_curp                LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran       LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran          CHAR(19),--LIKE bus_solicitud_tramite.f_operacion,
         v_id_respuesta        LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack           LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack           CHAR(19),--LIKE bus_respuesta_tramite.f_operacion,
         v_cod_resp_ack        LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack    LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta      LIKE bus_respuesta_tramite.desc_respuesta,
         v_cod_ope_cliente     LIKE bus_respuesta_tramite.cod_oper_cliente,
         v_excepcion           LIKE bus_excepcion.descripcion
       END RECORD,
       v_origen  CHAR(1),       
       v_pagina  SMALLINT,
       v_fecha_actual CHAR(10)
       

   FORMAT

      FIRST PAGE HEADER
         LET v_fecha_actual = TODAY USING "dd-mm-yyyy"
         
         PRINTX v_fecha_actual,
                p_modulo_desc,
                p_proceso_desc,
                p_operacion_desc,
                p_contrato_desc,
                p_f_operacion_ini,
                p_f_operacion_fin,
                p_f_operacion_ack_ini,
                p_f_operacion_ack_fin,
                p_nss,
                p_curp,
                p_cod_transaccion_opr,
                p_cod_respuesta,
                p_cod_respuesta_opr 

      BEFORE GROUP OF p_tramite.v_folio_procesar
         PRINTX p_tramite.v_folio_procesar

      ON EVERY ROW
         LET v_origen = p_tramite.v_origen[1] 
         PRINTX p_tramite.v_consecutivo,
                p_indice,
                p_tramite.v_folio_procesar,
                v_origen, # solo se imprime el primer caracter del origen
                p_tramite.v_desc_proceso,
                p_tramite.v_desc_operacion,
                p_tramite.v_id_cat_bus_contrato,
                p_tramite.v_desc_contrato,
                p_tramite.v_id_solicitid,
                p_tramite.v_folio_transaccion,
                p_tramite.v_nss,
                p_tramite.v_curp,
                p_tramite.v_cod_resp_tran,
                p_tramite.v_fecha_tran, --USING "dd-mm-yyyy",
                p_tramite.v_id_respuesta,
                p_tramite.v_folio_ack,
                p_tramite.v_fecha_ack, --USING "dd-mm-yyyy",
                p_tramite.v_cod_resp_ack,
                p_tramite.v_cod_resp_ope_ack,
                p_tramite.v_desc_respuesta,
                p_tramite.v_cod_ope_cliente,
                p_tramite.v_excepcion

      PAGE TRAILER
         # imprime número de la página
         LET v_pagina = PAGENO 
         PRINTX v_pagina

         
END REPORT

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Reporte de tramite bus y detalle tramite                 #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
REPORT rpt_consultas_notificacion(p_folio_procesar,p_tramite,p_detalle_solicitud,p_errores_solicitud,p_errores_respuesta)
DEFINE p_folio_procesar LIKE bus_tramite.folio_procesar,
       p_tramite RECORD
         v_consecutivo       INTEGER,--LIKE bus_tramite.id_bus_tramite,
         v_origen            VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso      LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion    LIKE cat_bus_operacion.desc_opera_bus,         
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid      LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss               LIKE bus_solicitud_tramite.nss,
         v_curp              LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran     LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran        CHAR(19),--LIKE bus_solicitud_tramite.f_operacion,
         v_id_respuesta      LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack         LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack         CHAR(19),--LIKE bus_respuesta_tramite.f_operacion,
         v_cod_resp_ack      LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack  LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta    LIKE bus_respuesta_tramite.desc_respuesta,
         v_excepcion         LIKE bus_excepcion.descripcion
       END RECORD,
       p_detalle_solicitud RECORD
         v_indice   SMALLINT,
         v_etiqueta LIKE bus_detalle_solicitud.nombre_campo, --VARCHAR(40),
         v_valor    LIKE bus_detalle_solicitud.valor --VARCHAR(200)
       END RECORD,
       p_errores_solicitud RECORD
         v_indice   SMALLINT,
         v_codigo      LIKE bus_error_solicitud.cod_error,
         v_descripcion LIKE bus_error_solicitud.desc_error
       END RECORD,
       p_errores_respuesta RECORD
         v_indice   SMALLINT,
         v_codigo      LIKE bus_error_respuesta.cod_error,
         v_descripcion LIKE bus_error_respuesta.desc_error
       END RECORD,
       v_pagina  SMALLINT,
       v_indice  SMALLINT,
       v_fecha_actual CHAR(10)
       

   FORMAT

      FIRST PAGE HEADER
         LET v_fecha_actual = TODAY USING "dd-mm-yyyy"
         PRINTX v_fecha_actual
         
         PRINTX p_folio_procesar,
                p_tramite.v_consecutivo,
                p_tramite.v_origen,
                p_tramite.v_desc_proceso,
                p_tramite.v_desc_operacion,
                p_tramite.v_id_cat_bus_contrato,
                p_tramite.v_desc_contrato,
                p_tramite.v_id_solicitid,
                p_tramite.v_folio_transaccion,
                p_tramite.v_nss,
                p_tramite.v_curp,
                p_tramite.v_cod_resp_tran,
                p_tramite.v_fecha_tran, -- USING "dd-mm-yyyy",
                p_tramite.v_id_respuesta,
                p_tramite.v_folio_ack,
                p_tramite.v_fecha_ack, -- USING "dd-mm-yyyy",
                p_tramite.v_cod_resp_ack,
                p_tramite.v_cod_resp_ope_ack,
                p_tramite.v_desc_respuesta,
                p_tramite.v_excepcion

      PAGE HEADER 
         PRINTX p_folio_procesar,
                p_tramite.v_consecutivo,
                p_tramite.v_origen,
                p_tramite.v_desc_proceso,
                p_tramite.v_desc_operacion,
                p_tramite.v_id_cat_bus_contrato,
                p_tramite.v_desc_contrato,
                p_tramite.v_id_solicitid,
                p_tramite.v_folio_transaccion,
                p_tramite.v_nss,
                p_tramite.v_curp,
                p_tramite.v_cod_resp_tran,
                p_tramite.v_fecha_tran, -- USING "dd-mm-yyyy",
                p_tramite.v_id_respuesta,
                p_tramite.v_folio_ack,
                p_tramite.v_fecha_ack, -- USING "dd-mm-yyyy",
                p_tramite.v_cod_resp_ack,
                p_tramite.v_cod_resp_ope_ack,
                p_tramite.v_desc_respuesta,
                p_tramite.v_excepcion

      ON EVERY ROW
         
         PRINTX p_detalle_solicitud.v_indice,
                p_detalle_solicitud.v_etiqueta,
                p_detalle_solicitud.v_valor,
                p_errores_solicitud.v_indice,
                p_errores_solicitud.v_codigo,
                p_errores_solicitud.v_descripcion,
                p_errores_respuesta.v_indice,
                p_errores_respuesta.v_codigo,
                p_errores_respuesta.v_descripcion
        

      PAGE TRAILER
         # imprime número de la página
         LET v_pagina = PAGENO 
         PRINTX v_pagina

      ON LAST ROW
         
         IF(g_detalle_bloque_solicitud.getLength() = 0)THEN
            LET g_detalle_bloque_solicitud[1].v_etiqueta = ""
            LET g_detalle_bloque_solicitud[1].v_valor    = "" 
         END IF
            # genera sub reporte de detalle de bloque para todo el detalle de solicitud
            START REPORT rpt_det_bloque_solicitud
               FOR v_indice = 1 TO g_detalle_bloque_solicitud.getLength()
                  OUTPUT TO REPORT rpt_det_bloque_solicitud(v_indice,
                                                            g_detalle_bloque_solicitud[v_indice].*)
               END FOR
            FINISH REPORT rpt_det_bloque_solicitud
            
         --END IF
         


END REPORT

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Sub reporte detalle de bloque del detalle de solicitud   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
REPORT rpt_det_bloque_solicitud(p_indice,p_detalle_bloque_solicitud)
DEFINE p_indice   SMALLINT,
       p_detalle_bloque_solicitud RECORD
         v_id_col_entidad     DECIMAL(9,0),
         v_id_col_cat_entidad DECIMAL(9,0),
         v_etiqueta_det_sol   LIKE bus_detalle_solicitud.nombre_campo,
         v_etiqueta           LIKE bus_detalle_solicitud.nombre_campo,
         v_valor              LIKE bus_detalle_solicitud.valor
       END RECORD,
       v_titulo STRING
   
   FORMAT

      FIRST PAGE HEADER
         LET v_titulo = "DETALLE BLOQUE NOTIFICACIÓN XML"
         PRINTX v_titulo

      PAGE HEADER
         PRINTX v_titulo 

      BEFORE GROUP OF p_detalle_bloque_solicitud.v_etiqueta_det_sol
         PRINTX p_detalle_bloque_solicitud.v_etiqueta_det_sol

      ON EVERY ROW
         PRINTX p_indice,
                p_detalle_bloque_solicitud.v_id_col_entidad,
                p_detalle_bloque_solicitud.v_id_col_cat_entidad,
                p_detalle_bloque_solicitud.v_etiqueta,
                p_detalle_bloque_solicitud.v_valor

END REPORT
