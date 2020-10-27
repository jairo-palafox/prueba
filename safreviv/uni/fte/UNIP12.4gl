--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 14/02/2013
--===============================================================

################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIP12                                                        #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la       #
#                preliquidacion para la unificacion de cuentas no liquidadas   #
#                de la carga inicial                                           #
#Fecha inicio => 14/02/2013                                                    #
################################################################################
GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
DEFINE g_reg_modulo   RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD
       
DEFINE arr_orig_credito DYNAMIC ARRAY OF RECORD
          v_tipo_originacion SMALLINT,
          v_tipo_origin_des  VARCHAR(25),
          v_tipo_credito     SMALLINT,
          v_tipo_credito_des VARCHAR(25),
          v_totales          INTEGER
END RECORD 

       
END GLOBALS

MAIN
DEFINE p_pid            LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod      LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio          LIKE deo_preliquida.folio_liquida, -- folio de la operacion
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
       v_s_sql          STRING, -- cadena con una instruccion SQL
       v_i_resultado    INTEGER -- resultado del proceso
       ,r_bnd_fin_oper SMALLINT
      --
       ,v_i_total_registros_encontrados INTEGER
       ,p_titulo            STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje           STRING -- cuerpo del mensaje enviado
       ,v_layout            LIKE cat_operacion.layout_cod
       ,v_ruta_rescate      STRING
       ,v_usuario           LIKE seg_modulo.usuario
       ,v_proceso_desc      LIKE cat_proceso.proceso_desc
       ,v_extension         LIKE cat_operacion.extension
       ,v_opera_desc        LIKE cat_operacion.opera_desc
       ,v_ruta_listados     LIKE seg_modulo.ruta_listados
       ,v_folio_liquida     LIKE dpe_preliquida.folio_liquida -- folio de la liquidación
       ,p_programa_cod      VARCHAR(10)
       ,v_error_isam        INTEGER
       ,v_mensaje_error     VARCHAR(255)
       
   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".UNIP12.log")

       
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


   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'
             
   -- se asigna proceso y operacion
   LET g_pid         = p_pid
   LET g_proceso_cod = p_proceso_cod
   LET g_opera_cod   = p_opera_cod

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   -- se asume que el proceso termina correctamente
   LET v_i_resultado = 0
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_uni_preliq_imss_no_liquidados(?, ?, ?, ?)"
   --se prepara la ejecucion del stored procedure para la preliquidacion
   PREPARE sid_preliquidauni FROM v_s_sql
   -- se ejecuta el stored procedure
   EXECUTE sid_preliquidauni USING p_folio,
                                   p_usuario_cod,
                                   g_pid,
                                   p_proceso_cod
                              INTO v_i_resultado,
                                   v_i_total_registros_encontrados,
                                   v_folio_liquida,
                                   v_error_isam,
                                   v_mensaje_error

   UPDATE bat_ctr_operacion
   SET    folio = v_folio_liquida
   WHERE  pid = g_pid
   AND    opera_cod = 2

   --Borra tabla temporal al finalizar la preliquidación
   DATABASE safre_tmp
   DROP TABLE safre_tmp:tmp_pre_no_liquidado
   DATABASE safre_viv

   IF ( v_i_resultado = 0 ) THEN      
      DISPLAY "#    Preliquidación realizada completamente"
      -- se invoca la finalizacion de la operacion
      CALL fn_actualiza_opera_fin(g_pid,    --- Identificador del proceso
                                  g_proceso_cod, --- Clave del proceso
                                  g_opera_cod) --- Clave de la operación
      RETURNING r_bnd_fin_oper

--      DISPLAY "OPERA FIN_FUNCION:  ", r_bnd_fin_oper
      LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # # \n",
                      "# \n",
                      "#    La preliquidación se terminó completamente. \n",
                      "# \n",
                      "#    El folio Lote: "||p_folio,"\n",
                      "# \n",
                      "#    El folio preliquidación: "||v_folio_liquida,"\n",
                      "# \n",
                      "# # # # # # # # # # # # # # # # # # # # # # # # # #"

      -- Se finaliza operacion aunque no se termine correctamente el error
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      DISPLAY "#    La preliquidación se terminó completamente."
      DISPLAY "#    "
      DISPLAY "#    El folio Lote: "||p_folio
      DISPLAY "#    "
      DISPLAY "#    El folio preliqudiación: "||v_folio_liquida
      DISPLAY "#    "
      DISPLAY "#    Total de registros: "||v_i_total_registros_encontrados
      DISPLAY "#    "
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"

      --Ejecuta la función de LIQUIDACIóN 
      DISPLAY ">>>>  SE INICIA LA LIQUIDACION "
      CALL fn_liquida_no_liquidados(v_folio_liquida, p_usuario_cod)

   ELSE
      -- Indica que error ocurrio
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bnd_fin_oper
      DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)
      DISPLAY "#    Error al procesar la Preliquidación"
      DISPLAY "#    El status de resultado es: ", v_i_resultado
      DISPLAY "#    Mensaje: ", v_mensaje_error
      DISPLAY "#    Sin saldo disponible para las solicitudes aceptadas"
      DISPLAY "#    Total registros encontrados: ",
              v_i_total_registros_encontrados
      -- se complementa el mensaje
      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de Preliquidación no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"
   END IF
   --ELSE
       
   --END IF
   SELECT programa_cod
     INTO p_programa_cod
     FROM cat_operacion
    WHERE proceso_cod = g_proceso_cod
      AND opera_cod   = g_opera_cod

   CALL fn_reporte_liquidacion(v_folio_liquida, "uni_preliquida",
                                  p_usuario_cod, g_pid, g_proceso_cod,
                                  g_opera_cod, p_programa_cod,
                                  FALSE)

   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - PRELIQUIDACION"


   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                       "/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                       p_titulo,
                       p_mensaje)                        
END MAIN

#OBJETIVO: Obtener la descripcion del error de la validacion y mostrar al usuario. 
FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT
        ,v_descripcion LIKE cat_bat_parametro_salida.descripcion
  

   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
     INTO v_descripcion
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_resultado_opera

   -- Muestra el mensaje encontrado
   --CALL fn_mensaje("Atención",v_descripcion CLIPPED,"information")
   DISPLAY "Atención ",v_descripcion CLIPPED
   
   RETURN v_descripcion CLIPPED
END FUNCTION -- fn_mues_desc_valida

#OBJETIVO: Invoca el programa que se encarga de ejecutar la liquidación
FUNCTION fn_liquida_no_liquidados(p_folio_liquidacion,
                                  p_usuario_cod)
DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio_unificacion LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando         STRING, -- cadena con una instruccion de consola
       v_nombre_archivo    LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       r_bnd_fin_oper      SMALLINT,
       r_bdn_valida_op     SMALLINT,
       r_bnd_inicializa    SMALLINT,
       r_bnd_oper_ini      SMALLINT,
       v_mensaje           STRING,
       v_opera_cod         SMALLINT,
       r_tot_desmarca_unificador INTEGER, 
       r_tot_desmarca_unificado  INTEGER,
       r_folio_liquidacion       DECIMAL(9,0),
       p_folio_liquidacion       DECIMAL(9,0),
       r_bnd_preliquida          SMALLINT,
       v_estado_cod              SMALLINT,
       i                         SMALLINT

   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "ARCHIVOAQUI"

   LET v_opera_cod = 2

   -- se verifica si se puede continuar con la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,v_opera_cod) 
   RETURNING r_bdn_valida_op 
     
   IF ( r_bdn_valida_op = 0 ) THEN
         -- Inicio operacion.
         CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,v_opera_cod,0 ,"UNIP12","",p_usuario_cod)
            RETURNING r_bnd_oper_ini   
      
         IF (r_bnd_oper_ini = 0) THEN

            SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
            INTO   g_reg_modulo.*
            FROM   seg_modulo s
            WHERE  s.modulo_cod = 'glo'
      
            LET v_s_comando = " fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/GLOG03 ",
                                p_usuario_cod, " ",
                                g_pid  , " " ,
                                g_proceso_cod , " " ,
                                v_opera_cod ," ",
                                p_folio_liquidacion ," ",
                                v_nombre_archivo ," ",
                                " 1>",seg_modulo_bat.ruta_listados clipped ,
                                "/nohup:",g_pid        USING "&&&&&",":",
                                g_proceso_cod USING "&&&&&",":",
                                v_opera_cod   USING "&&&&&" ,
                                " 2>&1 &"

            DISPLAY v_s_comando

            RUN v_s_comando

            DISPLAY "Atención, Se ha enviado la Liquidación."
            DISPLAY "Puede revisar el avance del proceso en el "
            DISPLAY "monitor de ejecución de procesos"

            DISPLAY ">> SE INICIAN LOS INDICADORES"  
            CALL fn_indicadores_credito_no_liquidados(p_folio_unificacion,
                                                      p_usuario_cod)

         ELSE
            CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) RETURNING v_mensaje
            DISPLAY " >> ATENCIÓN << \n"
            DISPLAY " >> ", v_mensaje
            --CALL fn_mensaje("Atención", v_mensaje, "stop")      
         END IF --Opera_ini
   ELSE
      CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) RETURNING v_mensaje
      DISPLAY "VALIDA LA OPERACION: ", v_mensaje
      --CALL fn_mensaje("Atención", v_mensaje, "stop")      
   END IF --Valida operacion 

END FUNCTION

#OBJETIVO : Mostrar un resúmen del indicador solo IMSS
FUNCTION fn_consulta_resumen_indicador(p_folio_unificacion,
                                       p_folio_liquidacion)
DEFINE p_folio                         DECIMAL(9,0),
       p_folio_unificacion             DECIMAL(9,0),
       p_folio_liquidacion             DECIMAL(9,0),
       v_folio_unificacion             DECIMAL(9,0),
       v_id_derechohabiente_unificador DECIMAL(9,0),
       v_id_derechohabiente_unificado  DECIMAL(9,0),
       v_id_unificador                 DECIMAL(9,0),
       v_nss_unificador                CHAR(11),
       v_s_sql                         STRING,
       v_cred_total                    SMALLINT,
       v_cred_total_1                  SMALLINT,
       v_cred_total_2                  SMALLINT,
       v_cred_total_3                  SMALLINT,
       v_desmarca_unificador           SMALLINT,
       v_total_desmarca_unificador     SMALLINT,
       v_desmarca_unificado            SMALLINT,
       v_total_desmarca_unificado      SMALLINT,
       v_indice                        INTEGER,
       v_indice_2                      INTEGER
DEFINE r_orig_credito RECORD
          v_tipo_originacion SMALLINT,
          v_tipo_origin_des  VARCHAR(25),
          v_tipo_credito     SMALLINT,
          v_tipo_credito_des VARCHAR(25),
          v_totales          INTEGER
END RECORD 
       
   LET v_cred_total    = 0
   LET v_cred_total_1  = 0
   LET v_cred_total_2  = 0
   LET v_cred_total_3  = 0
   LET v_desmarca_unificador = 0;
   LET v_desmarca_unificado = 0;
   LET v_total_desmarca_unificador = 0
   LET v_total_desmarca_unificado = 0
   
   SELECT folio_unificacion
     INTO v_folio_unificacion
   FROM uni_det_unificador
   WHERE folio_liquidacion = p_folio
   GROUP BY 1
--DISPLAY "FOLIO LIQUIDACION  ", p_folio_liquidacion
--DISPLAY "FOLIO UNIFICACION  ", p_folio_unificacion   
   -- Se selecciona el registro de uni det_unificador
   -- Para saber si cuenta con credito
   DECLARE Curr_creditos CURSOR FOR
    SELECT b.id_unificador,
           b.nss_unificador, 
           b.id_derechohabiente
      FROM uni_det_unificador b
     WHERE b.estado_familia = 1
       AND b.diagnostico = 4
       
   FOREACH Curr_creditos INTO v_id_unificador,
   	                          v_nss_unificador, 
                              v_id_derechohabiente_unificador
   	  
   	  DECLARE Curr_creditos_2 CURSOR FOR 
      
   	  SELECT id_derechohabiente
	    FROM uni_det_unificado
	   WHERE id_unificador = v_id_unificador
	    FOREACH Curr_creditos_2 INTO v_id_derechohabiente_unificado
        
      IF v_id_derechohabiente_unificado IS NOT NULL THEN
         LET v_s_sql = "\n SELECT ca.tpo_originacion,TRIM(tr.originacion_desc),",
                       "\n        ca.tpo_credito,TRIM(tc.desc_credito),COUNT(*)",
                       "\n   FROM cre_acreditado ca,",
                       "\n        cat_cre_originacion tr,",
                       "\n        cat_tipo_credito tc",
                       "\n  WHERE ca.id_derechohabiente IN", 
                       "\n                               (SELECT id_derechohabiente",
                       "\n                                FROM   uni_det_unificado ",
                       "\n                                WHERE  id_unificador IN ",
                       "\n                                                        (SELECT id_unificador      ",
                       "\n                                                         FROM   uni_det_unificador ",
                       "\n                                                         WHERE  folio_unificacion = ",v_folio_unificacion,
                       "\n                                                         AND    estado_familia = 1 ",
                       "\n                                                         AND    diagnostico = 4))  ",
                       "\n    AND ca.tpo_originacion = tr.tpo_originacion",
                       "\n    AND ca.tpo_credito = tc.tpo_credito",
                       "\n    AND ca.estado <> 230",
                       "\n    GROUP BY 1,2,3,4", 
                       "\n    ORDER BY 1,3" 
--DISPLAY "CONSULTA INDICADORES::::::::::::::::::::::::> ", v_s_sql CLIPPED
	       PREPARE Prp_derecho_cre FROM v_s_sql CLIPPED
	       DECLARE Curr_derecho_cre CURSOR FOR Prp_derecho_cre

           LET v_indice = 1

	       FOREACH Curr_derecho_cre INTO r_orig_credito.* 
              LET arr_orig_credito[v_indice].v_tipo_originacion = r_orig_credito.v_tipo_originacion
              LET arr_orig_credito[v_indice].v_tipo_origin_des =  r_orig_credito.v_tipo_origin_des
              LET arr_orig_credito[v_indice].v_tipo_credito =     r_orig_credito.v_tipo_credito
              LET arr_orig_credito[v_indice].v_tipo_credito_des = r_orig_credito.v_tipo_credito_des
              LET arr_orig_credito[v_indice].v_totales =          r_orig_credito.v_totales

              LET v_indice = v_indice + 1
	       END FOREACH

           LET v_indice_2 = 1

           FOR v_indice = 2 TO   arr_orig_credito.getLength()
              IF arr_orig_credito[v_indice].v_tipo_originacion = arr_orig_credito[v_indice_2].v_tipo_originacion  THEN
                 LET arr_orig_credito[v_indice].v_tipo_originacion = NULL
                 LET arr_orig_credito[v_indice].v_tipo_origin_des  = ""
              ELSE 
                 LET v_indice_2 = v_indice
              END IF 
           END FOR
         	  SELECT COUNT(*)
	            INTO v_desmarca_unificado
	            FROM sfr_marca_activa
	           WHERE marca = 502
	           AND id_derechohabiente = v_id_derechohabiente_unificado

	           LET v_total_desmarca_unificado = v_total_desmarca_unificado +
	                                             v_desmarca_unificado        	                                             
      END IF
   END FOREACH
            SELECT COUNT(*)
	            INTO v_desmarca_unificador
	            FROM sfr_marca_activa
	           WHERE marca = 501
	           AND id_derechohabiente = v_id_derechohabiente_unificador
	           
	           LET v_total_desmarca_unificador = v_total_desmarca_unificador +
	                                             v_desmarca_unificador


   END FOREACH

   --DISPLAY v_total_desmarca_unificador, v_total_desmarca_unificado
   --TO txt_tot_des_dor, txt_tot_des_do
   
   RETURN v_total_desmarca_unificador, v_total_desmarca_unificado
END FUNCTION

#OBJETIVO: Ejecutar los indicadores y la desmarca de los registros
FUNCTION fn_indicadores_credito_no_liquidados(p_folio,
                                              p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       r_bnd_fin_oper    SMALLINT,
       v_mensaje         STRING,
       v_opera_cod       SMALLINT,
       r_bdn_valida_op   SMALLINT

   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "ARCHIVOAQUI"

   LET v_opera_cod = 3 
   
   	  -- Inicio operacion.
      CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,v_opera_cod,p_folio,"UNIP12",
                                 "",p_usuario_cod)
      RETURNING r_bnd_fin_oper
      
      DISPLAY "Inicia Operacion:",r_bnd_fin_oper
      
      IF (r_bnd_fin_oper = 0) THEN

         SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
         INTO   g_reg_modulo.*
         FROM   seg_modulo s
         WHERE  s.modulo_cod = 'uni'


         SELECT b.ruta_listados
         INTO   seg_modulo_bat.ruta_listados
         FROM   seg_modulo b
         WHERE  b.modulo_cod = 'bat'
      
         LET v_s_comando = " fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/UNIP13 ",
                             p_usuario_cod, " ",
                             g_pid  , " " ,
                             g_proceso_cod , " " ,
                             v_opera_cod ," ",
                             p_folio ," ",
                             v_nombre_archivo ," ",
                             " 1>",seg_modulo_bat.ruta_listados clipped ,
                             "/nohup:",g_pid        USING "&&&&&",":",
                             g_proceso_cod USING "&&&&&",":",
                             v_opera_cod   USING "&&&&&" ,
                             " 2>&1 &"
                             
          DISPLAY v_s_comando

          RUN v_s_comando
          
          DISPLAY "Atención, Se ha enviado la actualización de indicadores"
          DISPLAY "Puede revisar el avance del proceso en el monitor de "
          DISPLAY "ejecución de procesos"
          
      ELSE
         CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) RETURNING v_mensaje
         DISPLAY v_mensaje
      END IF         
   --END IF
END FUNCTION 