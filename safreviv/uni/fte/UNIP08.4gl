--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 06/06/2012
--===============================================================

#########################################################################################
#Modulo       => UNI                                                                    #
#Programa     => UNIP02-1                                                               #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la desmarca y la  #
#                actualizacion de indicadores de credito                                #
#Fecha inicio => 06/06/2012                                                             #
#########################################################################################
GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
        v_folio_lote         DECIMAL(9,0),
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion,
END GLOBALS

MAIN
DEFINE p_pid            LIKE bat_ctr_operacion.pid, -- PID del proceso      
       p_opera_cod      LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio          LIKE deo_preliquida.folio_liquida, -- folio de la operacion
       v_si_resultado   INTEGER, -- resultado del proceso
       p_titulo             STRING, -- titulo del mensaje enviado en el correo
       p_mensaje            STRING, -- cuerpo del mensaje enviado
       v_layout             LIKE cat_operacion.layout_cod,
       v_ruta_rescate       STRING,
       v_usuario            LIKE seg_modulo.usuario,
       v_proceso_desc       LIKE cat_proceso.proceso_desc,
       v_extension          LIKE cat_operacion.extension,
       v_opera_desc         LIKE cat_operacion.opera_desc,
       v_ruta_listados      LIKE seg_modulo.ruta_listados
       
   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
        RETURNING v_proceso_desc,
                  v_extension, 
                  v_opera_desc,
                  v_layout, 
                  v_ruta_rescate,
                  v_ruta_listados,
                  v_usuario
      
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- unificacion de cuentas
   LET g_opera_cod   = p_opera_cod -- indicadores

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   --Actualización de Indicadores
   CALL actualiza_indicadores_IMSS(p_nombre_archivo, p_usuario_cod, p_folio)      
  
   LET v_si_resultado = 0

   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - ACTUALIZACIÓN DE ÍNDICADORES Y DESMARCAS"

   CALL fn_correo_proceso(g_pid,
                          g_proceso_cod,
                          g_opera_cod,
                          "", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
END MAIN

{

 Obtiene la descripcion del error de la validacion y
  la muestra en mensaje para suario.
}
FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT
        ,v_descripcion LIKE cat_bat_parametro_salida.descripcion

   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
     INTO v_descripcion
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_resultado_opera

   -- Muestra el mensaje encontrado
   DISPLAY "Atención ",v_descripcion CLIPPED
   
   RETURN v_descripcion CLIPPED
END FUNCTION -- fn_mues_desc_valida

###############################################################################
FUNCTION  actualiza_indicadores_IMSS(p_nombre_archivo, p_usuario_cod, p_folio)
DEFINE p_nombre_archivo     CHAR(40),
          p_usuario_cod        CHAR(20),
          p_folio              DECIMAL(9,0),
          v_cadena             STRING,
          v_f_actualiza        DATE,
          r_bnd_fin_oper       SMALLINT,
          p_mensaje            STRING, -- cuerpo del mensaje enviado
          v_si_resultado       INTEGER, -- resultado del proceso
          isam_err             INTEGER,
          v_c_msj              CHAR(200),
          v_sqltxt             STRING,
          v_marca_unificador   INTEGER,
          v_marca_unificado    INTEGER,
          v_indx               INTEGER,
          bandera              INTEGER,
          bandera_uni          INTEGER,
          v_s_sql              STRING,
          v_resp_datos_credito INTEGER,
          v_seq_cre_archivo    BIGINT,
          v_ctr_folio_archivo  DECIMAL(9,0),
          v_num                SMALLINT

DEFINE v_arr_unificador DYNAMIC ARRAY OF RECORD
          v_id_unificador         LIKE uni_det_unificador.id_unificador,
          v_id_unificado          LIKE uni_det_unificado.id_unificado,
          v_folio_unificacion     LIKE uni_det_unificador.folio_unificacion,
          v_id_derecho_unificador LIKE uni_det_unificador.id_derechohabiente,
          v_id_derecho_unificado  LIKE uni_det_unificado.id_derechohabiente
END RECORD

   LET bandera     = 0
   LET bandera_uni = 0
   LET v_num       = 22

   SELECT seq_cre_archivo.NEXTVAL
   INTO   v_seq_cre_archivo
   FROM   systables
   WHERE  tabname = "cre_ctr_archivo"

   --Genera el folio de los índicadores y la desmarca   
   LET v_s_sql = "EXECUTE FUNCTION fn_genera_folio(?, ?, ?)"

   PREPARE sid_genera_folio FROM v_s_sql
   EXECUTE sid_genera_folio USING g_proceso_cod,
                                  v_num,
                                  p_usuario_cod
                             INTO v_ctr_folio_archivo

   --Recupera los ids que se van a desmarcar
   LET v_sqltxt="  SELECT a.id_unificador,",
                "\n       b.id_unificado,",
                "\n       a.folio_unificacion,",
                "\n       a.id_derechohabiente,",
                "\n       b.id_derechohabiente",
                "\n  FROM uni_det_unificador a,",
                "\n       uni_det_unificado b",
                "\n WHERE a.id_unificador = b.id_unificador",
                "\n   AND a.estado_familia = 1",
                "\n   AND a.folio_liquidacion = ",p_folio,
                "\n   AND a.diagnostico = 4"
                
   LET v_indx=1
   
   PREPARE Pr_unificador_imss FROM v_sqltxt
   DECLARE Curr_unificador_IMSS CURSOR FOR Pr_unificador_imss 
                           
   FOREACH Curr_unificador_IMSS INTO v_arr_unificador[v_indx].*
      --Ejecuta función de actualización de datos de crédito en tablas histórica
      LET v_s_sql = "EXECUTE FUNCTION fn_indicadores_desmarca(?,?,?,?,?,?,?,?,?,?,?,?)"
      PREPARE Prpr_dtos_credito FROM v_s_sql CLIPPED

      EXECUTE Prpr_dtos_credito  USING v_arr_unificador[v_indx].v_id_unificador,
                                       v_arr_unificador[v_indx].v_id_unificado,              
                                       v_arr_unificador[v_indx].v_id_derecho_unificado,
                                       v_arr_unificador[v_indx].v_id_derecho_unificador,
                                       v_arr_unificador[v_indx].v_folio_unificacion,
                                       v_f_actualiza,
                                       g_proceso_cod,
                                       p_nombre_archivo,
                                       p_usuario_cod,
                                       v_seq_cre_archivo,
                                       v_ctr_folio_archivo,
                                       g_proceso_cod
                                  INTO v_si_resultado, 
                                       isam_err, 
                                       v_c_msj

      IF(v_si_resultado = 0)THEN
         DISPLAY "# Proceso de Desmarcado terminado completamente"
      ELSE
         DISPLAY "# Error en Proceso de Desmarcado "
      END IF

      -- Se finaliza operacion aunque no se termine correctamente el error
      DISPLAY "# # # # # # # # # # # # "
      DISPLAY "# Msg:",v_c_msj
      DISPLAY "# Res:",v_si_resultado
      DISPLAY "# Isa:",isam_err
      DISPLAY "# # # # # # # # # # # # "

      LET v_indx =v_indx + 1
   END FOREACH
       
   IF ( v_si_resultado = 0 ) THEN
         DISPLAY "# Proceso de Desmarcado terminado completamente"
         -- se invoca la finalizacion de la operacion
         CALL fn_actualiza_opera_fin(g_pid,    --- Identificador del proceso
                                     g_proceso_cod, --- Clave del proceso
                                     g_opera_cod) --- Clave de la operación
            RETURNING r_bnd_fin_oper
         DISPLAY "Bandera:",r_bnd_fin_oper
         
         LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # # \n",
                         "# \n",
                         "#    La actualización de indicadores se terminó completamente. \n",
                         "# \n",
                         "#    El folio Lote: "||p_folio,"\n",
                         "# \n",
                         "# # # # # # # # # # # # # # # # # # # # # # # # # #"
   ELSE
      --Finaliza la operación con ERROR
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bnd_fin_oper
      --Muestra el error ocurrido   
      DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      DISPLAY "#    "
      DISPLAY "#    Error al procesar la actualización de indicadores"
      DISPLAY "#    El status de resultado es: ", v_si_resultado
      DISPLAY "#    "
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      
      -- se complementa el mensaje
      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de Preliquidación no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"        
   END IF                 

   CALL v_arr_unificador.deleteElement(v_arr_unificador.getLength())

END FUNCTION 