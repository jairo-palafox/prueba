--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 02/01/2013
--===============================================================

#########################################################################################
#Modulo       => UNI                                                                    #
#Programa     => UNIP02                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la preliquidacion #
#                para la unificacion de cuentas solo INFONAVIT                          #
#Fecha inicio => 22/05/2012                                                             #
#########################################################################################
GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_pid            LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod      LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio          LIKE deo_preliquida.folio_liquida, -- folio de la operacion
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
       v_s_sql          STRING, -- cadena con una instruccion SQL
       v_i_resultado    INTEGER, -- resultado del proceso
       r_bnd_fin_oper  SMALLINT,
      --
       v_i_tot_registros_insertados INTEGER,
       v_i_registros_sin_saldo INTEGER,
       p_titulo            STRING, -- titulo del mensaje enviado en el correo
       p_mensaje           STRING, -- cuerpo del mensaje enviado
       v_layout            LIKE cat_operacion.layout_cod,
       v_ruta_rescate      STRING,
       v_usuario           LIKE seg_modulo.usuario,
       v_proceso_desc      LIKE cat_proceso.proceso_desc,
       v_extension         LIKE cat_operacion.extension,
       v_opera_desc        LIKE cat_operacion.opera_desc,
       v_ruta_listados     LIKE seg_modulo.ruta_listados,
       v_folio_liquida     LIKE dpe_preliquida.folio_liquida, -- folio de la liquidación
       p_programa_cod      VARCHAR (10),
       v_estado_marca      SMALLINT
       
   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario
      
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- devolucion por errores de operacion
   LET g_opera_cod   = p_opera_cod -- preliquidacion

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod
    
   -- se asume que el proceso termina correctamente
   LET v_i_resultado = 0
   
   ---- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_uni_preliq_infonavit(?,?)"
   ---- se prepara la ejecucion del stored procedure para la preliquidacion
   PREPARE sid_preliquidauni_INFONAVIT FROM v_s_sql
   
   ---- se ejecuta el stored procedure
   EXECUTE sid_preliquidauni_INFONAVIT USING p_folio, 
                                             p_proceso_cod  
                                        INTO v_i_resultado, 
                                             v_i_tot_registros_insertados,
                                             v_i_registros_sin_saldo,
                                             v_folio_liquida

      IF ( v_i_tot_registros_insertados >= 1) THEN  
         DISPLAY "#    Preliquidación realizada completamente"
         -- se invoca la finalizacion de la operacion
         CALL fn_actualiza_opera_fin(g_pid,    --- Identificador del proceso
              g_proceso_cod, --- Clave del proceso
              g_opera_cod) --- Clave de la operación
            RETURNING r_bnd_fin_oper
                        
            LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # # \n",
                            "# \n",
                            "#    La preliquidación se terminó completamente. \n",
                            "# \n",
                            "#    El folio Lote: "||p_folio,"\n",
                            "# \n",
                            "#    El folio preliquidiación: "||v_folio_liquida,"\n",
                            "# \n"
                            --"# # # # # # # # # # # # # # # # # # # # # # # # # #"

            --se obtiene el codigo de programa
            SELECT programa_cod
            INTO   p_programa_cod
            FROM   cat_operacion
            WHERE  proceso_cod = p_proceso_cod
            AND    opera_cod   = p_opera_cod
   
            CALL fn_reporte_liquidacion(p_folio, "uni_preliquida",p_usuario_cod, 
                                        p_pid, p_proceso_cod,p_opera_cod, "UNIL09", 
                                        FALSE)
                            
            IF v_i_registros_sin_saldo >= 1 THEN 
               LET p_mensaje = p_mensaje || "\n # Número de registros preliquidados : ", v_i_tot_registros_insertados,
                                            "\n # Número de registros sin saldo     : ", v_i_registros_sin_saldo
            END IF
      ELSE
         CALL fn_finaliza_proceso(v_i_resultado, v_i_registros_sin_saldo, p_folio, g_pid,g_proceso_cod,g_opera_cod)     
      END IF                                              
   CASE
      WHEN (SQLCA.SQLCODE < 0 )
         -- Indica que ocurrio
         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
            RETURNING r_bnd_fin_oper
         DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)
         DISPLAY "#    Error al procesar la Preliquidación"
         DISPLAY "#    El status de resultado es: ", v_i_resultado
         DISPLAY "#    "
         DISPLAY "#    Total registros encontrados: ", 
                 v_i_tot_registros_insertados
         -- se complementa el mensaje
         LET p_mensaje = " --- ERROR ---\n",
                         " El proceso de Preliquidación no terminó correctamente.\n",
                         " Código de error : ", r_bnd_fin_oper,"\n ",
                         " FECHA           : ",TODAY,"\n",
                         " HORA            : ",CURRENT HOUR TO SECOND,"\n"        
   END CASE
   DISPLAY "#    "
   
   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - PRELIQUIDACION"

   DISPLAY "GLOG01: SIN CORREO REGISTRADO"
   {
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                       "/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                       p_titulo,
                       p_mensaje)
   }
END MAIN

#OBJETIVO: Obtener la descripcion del error de la validacion y mostrar al usuario. 
FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT,
         v_descripcion LIKE cat_bat_parametro_salida.descripcion
  

   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
   INTO   v_descripcion
   FROM   cat_bat_parametro_salida
   WHERE  cod_salida = p_resultado_opera

   -- Muestra el mensaje encontrado
  DISPLAY "Atención",v_descripcion CLIPPED
   RETURN v_descripcion CLIPPED
END FUNCTION -- fn_mues_desc_valida

#OBJETIVO: Marcar todo el proceso como finalizado, aún cuando no haya registros a liquidar.
FUNCTION fn_finaliza_proceso(p_resultado, 
                             p_i_registros_sin_saldo, 
                             p_folio_unificacion,
                             p_pid, 
                             p_proceso_cod,
                             p_opera_cod)
DEFINE r_bnd_fin_oper SMALLINT,
       p_resultado    SMALLINT,
       p_i_registros_sin_saldo INTEGER,
       p_folio_unificacion DECIMAL(9,0),
       v_query        STRING,
       p_pid          DECIMAL(9,0),
       p_proceso_cod  SMALLINT,
       p_opera_cod    SMALLINT

   DISPLAY "#    Preliquidación no se termino completamente "
   DISPLAY "#    Registros sin saldo ", p_i_registros_sin_saldo
   DISPLAY "#    "
   --DISPLAY "#    No se preliquido ningún registro "
   --DISPLAY "#    "

   IF p_i_registros_sin_saldo >= 1  THEN
      DISPLAY "#    Los unificados no tienen saldo  \n"
      --Finaliza la PRELIQUIDACION
      LET p_proceso_cod = 2302
      LET p_opera_cod = 3

      LET v_query = "EXECUTE FUNCTION fn_actualiza_opera_fin(?,?,?)"
      PREPARE prp_fn_actualiza_opera_fin_pre FROM v_query
      EXECUTE prp_fn_actualiza_opera_fin_pre USING p_pid,
                                                   p_proceso_cod,
                                                   p_opera_cod
                                              INTO r_bnd_fin_oper                        
      --Inicia la LIQUIDACIÓN 
      CALL fn_actualiza_opera_ini(g_pid,
                                  g_proceso_cod,
                                  4,--g_opera_cod,
                                  0,
                                  "UNIL07",
                                  "",
                                  p_usuario_cod)
                        RETURNING r_bnd_fin_oper
      
      --Finaliza la LIQUIDACION
      LET p_proceso_cod = 2302
      LET p_opera_cod = 4

      LET v_query = "EXECUTE FUNCTION fn_actualiza_opera_fin(?,?,?)"
      PREPARE prp_fn_actualiza_opera_fin_liq FROM v_query
      EXECUTE prp_fn_actualiza_opera_fin_liq USING p_pid,
                                                   p_proceso_cod,
                                                   p_opera_cod
                                              INTO r_bnd_fin_oper    

      --Se actualiza STATUS del folio a liquidado
      UPDATE glo_folio
      SET status = 2
      WHERE folio = p_folio_unificacion;
    
   ELSE 
      DISPLAY "#    Revisar consulta de rechazos \n"
      --Finaliza la PRELIQUIDACION
      LET p_proceso_cod = 2302
      LET p_opera_cod = 3

      LET v_query = "EXECUTE FUNCTION fn_actualiza_opera_fin(?,?,?)"
      PREPARE prp_fn_opera_fin_pre FROM v_query
      EXECUTE prp_fn_opera_fin_pre USING p_pid,
                                                   p_proceso_cod,
                                                   p_opera_cod
                                              INTO r_bnd_fin_oper
      --Inicia la LIQUIDACIÓN
      CALL fn_actualiza_opera_ini(g_pid,
                                  g_proceso_cod,
                                  4,--g_opera_cod,
                                  0,
                                  "UNIL07",
                                  "",
                                  p_usuario_cod)
                        RETURNING r_bnd_fin_oper
      
      --Finaliza la LIQUIDACION
      LET p_proceso_cod = 2302
      LET p_opera_cod = 4

      LET v_query = "EXECUTE FUNCTION fn_actualiza_opera_fin(?,?,?)"
      PREPARE prp_fn_opera_fin_liq FROM v_query
      EXECUTE prp_fn_opera_fin_liq USING p_pid,
                                                   p_proceso_cod,
                                                   p_opera_cod
                                              INTO r_bnd_fin_oper    
      
      --Inicia la INDICADORES Y DESMARCA
      CALL fn_actualiza_opera_ini(g_pid,
                                  g_proceso_cod,
                                  5,--g_opera_cod,
                                  0,
                                  "UNIL23",
                                  "",
                                  p_usuario_cod)
                        RETURNING r_bnd_fin_oper
      --Finaliza INDICADORES Y DESMARCA
      LET p_proceso_cod = 2302
      LET p_opera_cod = 5

      LET v_query = "EXECUTE FUNCTION fn_actualiza_opera_fin(?,?,?)"
      PREPARE prp_fn_opera_fin_desm FROM v_query
      EXECUTE prp_fn_opera_fin_desm USING p_pid,
                                          p_proceso_cod,
                                          p_opera_cod
                                     INTO r_bnd_fin_oper    
                        
      --Inicia la GENERAR ARCHIVO
      CALL fn_actualiza_opera_ini(g_pid,
                                  g_proceso_cod,
                                  6,--g_opera_cod,
                                  0,
                                  "UNIL18",
                                  "",
                                  p_usuario_cod)
                        RETURNING r_bnd_fin_oper
      --Finaliza GENERAR ARCHIVO
      LET p_proceso_cod = 2302
      LET p_opera_cod = 6

      LET v_query = "EXECUTE FUNCTION fn_actualiza_opera_fin(?,?,?)"
      PREPARE prp_fn_opera_fin_arch FROM v_query
      EXECUTE prp_fn_opera_fin_arch USING p_pid,
                                          p_proceso_cod,
                                          p_opera_cod
                                     INTO r_bnd_fin_oper       
   END IF
END FUNCTION