--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 09/Mar/2016
--==============================================================================

################################################################################
#Proyecto     => SAFRE VIVIENDA                                                #
#Propietario  => E.F.P.                                                        #
#Modulo       => DAE                                                           #
#Programa     => DAEP09                                                        #
#Objetivo     => Lanzador de integración de Ajuste Individual Amortizaciones   #
#                Excedentes                                                    #
#Fecha inicio => 09/Mar/2016                                                   #
################################################################################

--LANZADOR: DAEL31

DATABASE safre_viv 

GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_pid                      LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod              LIKE bat_ctr_operacion.proceso_cod ,-- codigo del proceso
       p_opera_cod                LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario                  LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       v_s_sql                    STRING ,-- cadena con una instruccion SQL
       v_resultado              INTEGER ,-- resultado del proceso
       r_bnd_fin_oper             SMALLINT,
       v_si_correcto_integra      SMALLINT,
       p_nombre_archivo           LIKE glo_ctr_archivo.nombre_archivo,-- nombre dle archivo
       v_si_solicitudes_totales   INTEGER,
       v_msj_sql                  CHAR(200),
       v_folio                    LIKE deo_preliquida.folio_liquida,
       v_si_status_detalle_trabaj SMALLINT,
       p_titulo                   STRING ,-- titulo del mensaje enviado en el correo
       p_mensaje                  STRING ,-- cuerpo del mensaje enviado
       v_layout                   LIKE cat_operacion.layout_cod,
       v_ruta_rescate             STRING,
       v_usuario                  LIKE seg_modulo.usuario,
       v_proceso_desc             LIKE cat_proceso.proceso_desc,
       v_extension                LIKE cat_operacion.extension,
       v_opera_desc               LIKE cat_operacion.opera_desc,
       v_ruta_listados            LIKE seg_modulo.ruta_listados,
       v_total_rechazados         INTEGER,
       v_total_aceptadas          INTEGER,
       v_total_pendientes         INTEGER,
       r_bnd_ini_oper             SMALLINT,
       v_isam_error               INTEGER,
       v_mensaje                  STRING  

   ##Ejecuta prevalidación de saldos
   -- se recuperan los parametros la clave de usuario desde parametro 
   LET p_usuario        = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
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
   LET g_proceso_cod = p_proceso_cod -- devolucion por errores de operacion
   LET g_opera_cod   = p_opera_cod -- preliquidacion
   
   WHENEVER ERROR CONTINUE

   CALL fn_genera_folio(g_proceso_cod,g_opera_cod,p_usuario)
        RETURNING v_folio

   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_dae_integra_det_ajuste_manual(?, ?, ?, ?, ?) "

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_integra_dae FROM v_s_sql
   EXECUTE sid_integra_dae USING p_usuario, 
                                 g_pid, 
                                 p_nombre_archivo, 
                                 v_folio, --folio lote ajuste
                                 g_proceso_cod
           INTO v_resultado, 
                v_isam_error,
                v_msj_sql, 
                v_si_correcto_integra,
                v_si_solicitudes_totales,
                v_total_rechazados,
                v_total_aceptadas

   CASE
      WHEN (v_resultado = 0)
         IF ( v_resultado = 0 )THEN
            DISPLAY "  Integración manual realizada con exito"
         ELSE
            DISPLAY "  Integración realizada pero con errores de validación"
         END IF
         LET v_total_aceptadas = (v_si_solicitudes_totales - v_total_rechazados)

         IF ( v_si_correcto_integra = 0 AND v_si_solicitudes_totales > 0 ) THEN
            CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                RETURNING r_bnd_fin_oper

             --Si no hay registros ACEPTADOS finaliza la operación  
             IF v_total_aceptadas = 0 THEN
                CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,2,0,"DAEL40","",p_usuario)
                RETURNING r_bnd_ini_oper
                CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,2)
                RETURNING r_bnd_fin_oper
                CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,3,0,"DAEP12","",p_usuario)
                RETURNING r_bnd_ini_oper
                CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,3)
                RETURNING r_bnd_fin_oper
                
                DISPLAY "  No se puede Continuar con la Preliquidación "
                DISPLAY "  No existen registros Aceptados "
             ELSE
                DISPLAY "  Ya se puede Continuar con la Preliquidación"
             END IF 
             -- Genera cifras control por registro de patron.
             --CALL fn_obtiene_cifras_control(v_folio)
             LET p_mensaje = "  La integración se terminó completamente.","\n",
                             "  ","\n",
                             "  Integración realizada con exito","\n",
                             "  ","\n",
                             "  Folio lote o de integración : ",v_folio,"\n",
                             "  ","\n",
                             "  Total de aceptadas   : ",v_total_aceptadas,"\n",
                             "  Total de rechazadas  : ",v_total_rechazados,"\n",
                             "  Total de solicitudes : ",v_si_solicitudes_totales,"\n",
                             "  Estatus Resultado :",v_resultado,"\n",
                             "  Ya se puede Continuar con la Preliquidación","\n"
          ELSE
             CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                RETURNING r_bnd_fin_oper
                LET p_mensaje = " --- ERROR ---\n",
                                "  El proceso de integración no terminó correctamente.\n",
                                "  Código de error : ", r_bnd_fin_oper,"\n ",
                                "  FECHA           : ",TODAY,"\n",
                                "  HORA            : ",CURRENT HOUR TO SECOND,"\n"

             IF(v_resultado <> 0)THEN
                DISPLAY "  Error. No se integró ninguna solicitud"
             END IF

             LET p_mensaje = " --- ERROR ---\n",
                             "  El proceso de integración no terminó correctamente.\n",
                             "  Código de error : ", r_bnd_fin_oper,"\n ",
                             "  FECHA           : ",TODAY,"\n",
                             "  HORA            : ",CURRENT HOUR TO SECOND,"\n"           
          END IF
       
      WHEN (v_resultado = NOTFOUND)
          DISPLAY "NOT FOUND"
          CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
             RETURNING r_bnd_fin_oper
                CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) RETURNING v_mensaje
                CALL fn_mensaje("Atención", v_mensaje, "stop")    
          DISPLAY "  Error. No se integró ninguna solicitud"
          LET p_mensaje = "  --- ERROR ---\n",
                          "  El proceso de integración no terminó correctamente.\n",
                          "  Código de error : ", r_bnd_fin_oper,"\n ",
                          "  FECHA           : ",TODAY,"\n",
                          "  HORA            : ",CURRENT HOUR TO SECOND,"\n"
      WHEN (v_resultado  < 0)
          DISPLAY SQLERRMESSAGE
          CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
             RETURNING r_bnd_fin_oper
          DISPLAY "  Codigo Error SQL :",v_resultado
          DISPLAY "  Codigo ISAM      :",v_isam_error
          DISPLAY "  Mensaje          :",v_msj_sql 
          DISPLAY "  Error al procesar la integración"
          DISPLAY "  No se puede continuar..."
          LET p_mensaje = "  --- ERROR ---\n",
                          "  El proceso de integración no terminó correctamente.\n",
                          "  Código de error : ", r_bnd_fin_oper,"\n ",
                          "  FECHA           : ",TODAY,"\n",
                          "  HORA            : ",CURRENT HOUR TO SECOND,"\n"
    END CASE
   
   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - INTEGRACION"
   
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "",
                          p_titulo,
                          p_mensaje)      

END MAIN

# OBJETIVO: Obtener la descripción del error de la validacion y la muestra en mensaje para suario.
FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT
        ,v_descripcion LIKE cat_bat_parametro_salida.descripcion
   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
     INTO v_descripcion
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_resultado_opera
   -- Muestra el mensaje encontrado
   RETURN v_descripcion CLIPPED
END FUNCTION -- fn_mues_desc_valida

#OBJETIVO: Obtener cifras control para monitor de procesos
FUNCTION fn_obtiene_cifras_control(p_folio)
DEFINE p_folio  LIKE deo_preliquida.folio_liquida,
       v_total_solicitudes      INTEGER,          
       v_suma_importes_amort    DECIMAL(18,6),
       v_suma_aceptados_amort   DECIMAL(18,6),      
       v_total_aceptados        INTEGER,
       v_suma_rechazados_amort  DECIMAL(18,6),      
       v_total_rechazados       INTEGER,
       v_s_qry                  STRING

    LET v_s_qry = "\n SELECT COUNT(nss)",
                  "\n FROM   dae_det_ajuste ",
                  "\n WHERE  folio_lote = ", p_folio                  

   PREPARE prp_cifras_glob FROM v_s_qry CLIPPED
   DECLARE cur_cifras_glob CURSOR FOR prp_cifras_glob 

      DISPLAY "-----------------------------------------------------"
      DISPLAY "   CIFRAS GLOBALES   "
      DISPLAY " "
      DISPLAY "   FOLIO :",p_folio

   FOREACH cur_cifras_glob INTO --v_suma_importes_amort, 
                                v_total_solicitudes

       SELECT COUNT(nss)
       INTO   v_total_aceptados
       FROM   dae_det_ajuste
       WHERE  folio_lote = p_folio
       AND    resul_operacion = 1;
       
       SELECT COUNT(nss)
       INTO   v_total_rechazados
       FROM   dae_det_ajuste
       WHERE  folio_lote = p_folio
       AND    resul_operacion = 2;

      DISPLAY " " 
      DISPLAY "   TOTAL SOLICITUDES ACEPTADAS  : ",v_total_aceptados
      DISPLAY "   TOTAL SOLICITUDES RECHAZADAS : ",v_total_rechazados
      DISPLAY "   TOTAL SOLICITUDES            : ",v_total_solicitudes
   END FOREACH

   FREE cur_cifras_glob 

END FUNCTION -- fn_obtiene_cifras_control

#OBJETIVO: Obtien el estatus de las secciones integradas
FUNCTION fn_status_secciones_integradas(v_si_detalle)
   DEFINE 
     v_si_detalle        SMALLINT,
     v_c_mensaje CHAR(100)

   LET v_c_mensaje = ""

   IF(v_si_detalle = 1)THEN
      LET v_c_mensaje = v_c_mensaje CLIPPED, "   [ Error en Detalle ]" 
   END IF

   RETURN v_c_mensaje
   
END FUNCTION
