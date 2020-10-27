--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 20/09/2012
--===============================================================

################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIP10                                                        #
#Objetivo     => Programa lanzado de integraci�n respuesta sistemas INFONAVIT  #
#Fecha inicio => 30/07/2012                                                    #
################################################################################

GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_pid                       LIKE bat_ctr_operacion.pid -- PID del proceso
       ,p_proceso_cod              LIKE bat_ctr_operacion.proceso_cod -- codigo del proceso
       ,p_opera_cod                LIKE bat_ctr_operacion.opera_cod -- codigo de la operacion
       ,p_usuario_cod              LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_folio                    LIKE deo_preliquida.folio_liquida
       ,v_i_resultado              INTEGER -- resultado del proceso
       ,r_bnd_fin_oper             SMALLINT
       ,p_nombre_archivo           LIKE glo_ctr_archivo.nombre_archivo -- nombre dle archivo
       --
       ,v_si_solicitudes_aceptadas_unificadas  SMALLINT
       ,p_titulo                  STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje                 STRING -- cuerpo del mensaje enviado
       ,v_layout                  LIKE cat_operacion.layout_cod
       ,v_ruta_rescate            STRING
       ,v_usuario                 LIKE seg_modulo.usuario
       ,v_proceso_desc            LIKE cat_proceso.proceso_desc
       ,v_extension               LIKE cat_operacion.extension
       ,v_opera_desc              LIKE cat_operacion.opera_desc
       ,v_ruta_listados           LIKE seg_modulo.ruta_listados
       
DEFINE v_folio_int_resp_sist_inf  DECIMAL(9,0),
       v_cadena                   STRING,
       --Variables Control de errores
       v_resultado                SMALLINT, 
       v_isam                     SMALLINT,  
       v_mensaje                  VARCHAR(250),
       v_total_unificador         INTEGER,
       v_total_unificado          INTEGER  
        
   ##Ejecuta prevalidaci�n de saldos
   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_pid            = ARG_VAL(1)
   LET p_proceso_cod    = ARG_VAL(2)
   LET p_opera_cod      = ARG_VAL(3)
   LET p_usuario_cod    = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- Unificacion de cuentas
   LET g_opera_cod   = p_opera_cod -- preliquidacion

   CALL fn_genera_folio(g_proceso_cod,g_opera_cod,p_usuario_cod)
   RETURNING v_folio_int_resp_sist_inf
   
   WHENEVER ERROR CONTINUE

   LET v_cadena = "EXECUTE FUNCTION fn_uni_integra_resp_infonavit(?,?,?,?)"
                    
   PREPARE prp_integra_op22 FROM v_cadena
   EXECUTE prp_integra_op22 USING v_folio_int_resp_sist_inf,
                                  g_pid,
                                  g_proceso_cod,
                                  p_nombre_archivo                                  
                             INTO v_resultado, 
                                  v_isam,  
                                  v_mensaje,
                                  v_total_unificador,
                                  v_total_unificado                                         
                                  
            CASE
            WHEN (SQLCA.SQLCODE = 0)
               --Se finaliza aunque existan errores
               DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
               DISPLAY "#    "
               DISPLAY "#    La integraci�n se termin� completamente."
               DISPLAY "#    "
               DISPLAY "#    Folio integraci�n respuesta: "||v_folio_int_resp_sist_inf
               DISPLAY "#    "
               DISPLAY "#    Total de unificadores:          "||v_total_unificador
               DISPLAY "#    "
               DISPLAY "#    Total de unificados:            "||v_total_unificado
               DISPLAY "#    "
               DISPLAY "#  Integraci�n realizada con exito"
               DISPLAY "#  "
               
               IF v_i_resultado >= 0 THEN
                  CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                     RETURNING r_bnd_fin_oper
                  
                  LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # #","\n",
                                  "#  La integraci�n se termin� completamente.","\n",
                                  "#  ","\n",
                                  "#  Integraci�n realizada con exito","\n",
                                  "#  ","\n",
                                  "#  Folio integraci�n respuesta : ",v_folio_int_resp_sist_inf,"\n",
                                  "#  ","\n",
                                  "# # # # # # # # # # # # # # # # # # # # # # # # # #"
                                  
               ELSE
                  CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                     RETURNING r_bnd_fin_oper
                     
                   
                     LET p_mensaje = " --- ERROR ---\n",
                               " El proceso de Preliquidaci�n no termin� correctamente.\n",
                               " C�digo de error : ", r_bnd_fin_oper,"\n ",
                               " FECHA           : ",TODAY,"\n",
                               " HORA            : ",CURRENT HOUR TO SECOND,"\n"
                  DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)
                  IF(v_i_resultado <> 100)THEN
                     DISPLAY "#  Error. No se integr� ninguna solicitud"
                  ELSE
                     DISPLAY "#  ",fn_status_secciones_integradas(v_si_solicitudes_aceptadas_unificadas)
                  END IF
                  LET p_mensaje = " --- ERROR ---\n",
                               " El proceso de Integraci�n no termin� correctamente.\n",
                               " C�digo de error : ", r_bnd_fin_oper,"\n ",
                               " FECHA           : ",TODAY,"\n",
                               " HORA            : ",CURRENT HOUR TO SECOND,"\n"           
               END IF
               --DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
               DISPLAY "\n\n"

            WHEN (SQLCA.SQLCODE = NOTFOUND)
               DISPLAY "NOT FOUND"
               CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                  RETURNING r_bnd_fin_oper
               DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)
               DISPLAY "#  Error. No se integr� ninguna solicitud"
               LET p_mensaje = " --- ERROR ---\n",
                               " El proceso de Integraci�n no termin� correctamente.\n",
                               " C�digo de error : ", r_bnd_fin_oper,"\n ",
                               " FECHA           : ",TODAY,"\n",
                               " HORA            : ",CURRENT HOUR TO SECOND,"\n"
            WHEN (SQLCA.SQLCODE < 0)
               DISPLAY SQLERRMESSAGE
               DISPLAY  "\n ERROR: ", v_resultado, 
                        "\n ISAM: ",v_isam,  
                        "\n MENSAJE ",v_mensaje 

               
               CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                  RETURNING r_bnd_fin_oper
               DISPLAY "Codigo Error SQL:",SQLCA.SQLCODE
               DISPLAY "Error al procesar la integraci�n"
               DISPLAY "No se puede continuar..."
               LET p_mensaje = " --- ERROR ---\n",
                               " El proceso de Preliquidaci�n no termin� correctamente.\n",
                               " C�digo de error : ", r_bnd_fin_oper,"\n ",
                               " FECHA           : ",TODAY,"\n",
                               " HORA            : ",CURRENT HOUR TO SECOND,"\n"
              
            END CASE

      LET p_titulo = "Finalizaci�n de proceso - " || v_proceso_desc CLIPPED || " - INTEGRACION"
      
      CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
      
      
   WHENEVER ERROR STOP
END MAIN


{

 Obtiene la descripcion del error de la validacion y
  la muestra en mensaje para suario.
}
FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT
        ,v_descripcion LIKE cat_bat_parametro_salida.descripcion
  

   -- Obtiene la descripci�n resultado de la validacion
   SELECT descripcion
     INTO v_descripcion
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_resultado_opera

   -- Muestra el mensaje encontrado
   --CALL fn_mensaje("Atenci�n",v_descripcion CLIPPED,"information")
   RETURN v_descripcion CLIPPED
END FUNCTION -- fn_mues_desc_valida

FUNCTION fn_status_secciones_integradas(v_si_detalle)
   DEFINE 
     v_si_detalle        SMALLINT
    --
    ,v_c_mensaje CHAR(100)

   LET v_c_mensaje = ""

   IF(v_si_detalle = 1)THEN
      LET v_c_mensaje = v_c_mensaje CLIPPED, "   [ Error en Detalle ]" 
   END IF

   RETURN v_c_mensaje
   
END FUNCTION --  