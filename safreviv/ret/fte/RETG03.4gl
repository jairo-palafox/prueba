###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            =>                                                         #
#Programa          =>                                                         #
#Objetivo          =>                                                         #
#Fecha Inicio      =>                                                         #
###############################################################################

DATABASE safre_viv

DEFINE p_folio             DECIMAL(9,0)
DEFINE v_folio_lote        DECIMAL(9,0)
DEFINE p_usuario           CHAR(20)
DEFINE pid                 DECIMAL(9,0)
DEFINE p_proceso_cod       SMALLINT
DEFINE p_opera_cod         SMALLINT
DEFINE p_tabla             CHAR(40)


MAIN

   DEFINE v_cadena            STRING
   DEFINE v_layout            LIKE cat_operacion.layout_cod
   DEFINE v_ruta_rescate      STRING
   DEFINE v_usuario           LIKE seg_modulo.usuario
   DEFINE v_proceso_desc      LIKE cat_proceso.proceso_desc
   DEFINE v_extension         LIKE cat_operacion.extension
   DEFINE v_opera_desc        LIKE cat_operacion.opera_desc
   DEFINE v_ruta_listados     LIKE seg_modulo.ruta_listados
   DEFINE v_bandera           SMALLINT

   DEFINE v_subcuenta         LIKE cat_subcuenta.subcuenta
   DEFINE v_subcuenta_desc    LIKE cat_subcuenta.subcuenta_desc
   DEFINE v_movimiento        LIKE cat_movimiento.movimiento
   DEFINE v_movimiento_desc   LIKE cat_movimiento.movimiento_desc
   DEFINE v_sum_mto_acciones  LIKE cta_movimiento.monto_acciones
   DEFINE v_sum_mto_pesos     LIKE cta_movimiento.monto_pesos
   
   --DEFINE v_si_resultado      SMALLINT
   --DEFINE isam_err            INTEGER
   --DEFINE v_c_msj             CHAR(200)
   --DEFINE v_s_qry             STRING

   DEFINE p_titulo            STRING -- titulo del mensaje enviado en el correo
   DEFINE p_mensaje           STRING -- cuerpo del mensaje enviado

   --DEFINE v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados -- ruta listados de bat
   --DEFINE r_c_ruta_bin        LIKE seg_modulo.ruta_bin -- ruta del bin del módulo
   --DEFINE r_ruta_listados     LIKE seg_modulo.ruta_listados -- ruta de listados del módulo

   -- se recuperan los parametros
   LET p_usuario     = ARG_VAL(1)
   LET pid           = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET p_tabla       = ARG_VAL(6)

  
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario

      -- se despliega el inicio de la etapa 
   LET v_cadena = " PROCESO            : ",v_proceso_desc,"\n",
                  " OPERACIÓN          : ",v_opera_desc,"\n",
                  " FOLIO              : ",p_folio,"\n",
                  " FECHA              : ",TODAY,"\n",
                  " HORA               : ",TIME(CURRENT),"\n \n \n",
                  " INICIO ETAPA       : LIQUIDACION",
                  " FECHA              : ",TODAY,"\n",
                  " HORA               : ",TIME(CURRENT),"\n \n \n"

   DISPLAY v_cadena

	 # p_proceso_cod = 2 --> realiza la liquidacion solo para SAR92
   CASE p_proceso_cod
{    WHEN 1402  -- # p_proceso_cod = 2 --> realiza la liquidacion solo para SAR92
   	 		LET v_cadena = "EXECUTE FUNCTION fn_liquida_decreto (?,?,?,?,?,?)"
   	 WHEN 1701 -- # p_proceso_cod = 106 --> realiza la liquidacion solo Traspasos I-A
   	 		LET v_cadena = "EXECUTE PROCEDURE sp_liquida_decreto_movimiento (?,?,?,?,?,?)"}
     WHEN 1503 -- # ERV   p_proceso_cod = 1503 --> realiza la liquidacion Fondo Ahorro 
        LET v_cadena = "EXECUTE FUNCTION fn_liquida_folio_fondo72(?,?,?,?,?,?)"

     WHEN 1515 -- # ERV   p_proceso_cod = 1503 --> realiza la liquidacion Fondo Ahorro 
        LET v_cadena = "EXECUTE FUNCTION fn_liquida_folio_fondo72(?,?,?,?,?,?)"

     WHEN 1518 -- # ERV   p_proceso_cod = 1503 --> realiza la liquidacion Fondo Ahorro 
        LET v_cadena = "EXECUTE FUNCTION fn_liquida_folio_fondo72(?,?,?,?,?,?)"
        
   	 OTHERWISE		
   	    # realiza la liquidacion para cualquier modulo
   	    LET v_cadena = "EXECUTE FUNCTION fn_liquida_folio(?,?,?,?,?,?)"
     
   END CASE
   
   PREPARE prp_liquida FROM v_cadena
   EXECUTE prp_liquida USING p_folio      ,      
                             p_usuario    ,    
                             pid          ,   
                             p_proceso_cod,
                             p_opera_cod  ,
                             p_tabla
                        INTO v_bandera

   DISPLAY "\nProceso de liquidación finaliza con código: ", v_bandera, "\n"

   IF ( v_bandera = 0 ) THEN

      CASE p_proceso_cod 

          WHEN 1503
              DISPLAY "Finalizando estatus de solicitudes de Fondo de Ahorro WS"
              PREPARE prp_cam_stat_liquidacion FROM "EXECUTE PROCEDURE sp_status_retiro_fondo_ahorro_acti( ?, ?, ?, ?, ? )"
              EXECUTE prp_cam_stat_liquidacion USING  '0' , '60' , p_usuario , 'D' , p_folio -- rechazo, --estatus liquidacion
    
         WHEN 1515 
              DISPLAY "Finalizando estatus de solicitudes de Fondo de Ahorro Contingente"
              PREPARE prp_cam_liquidacion FROM "EXECUTE PROCEDURE sp_status_retiro_fondo_ahorro_acti_arch( ?, ?, ?, ?, ? )"
              EXECUTE prp_cam_liquidacion USING  '0' , '60' , p_usuario , 'D' , p_folio -- rechazo, --estatus liquidacion

         WHEN 1518 
              DISPLAY "Finalizando estatus de solicitudes de Fondo de Ahorro manual"
              PREPARE prp_manual_liquidacion FROM "EXECUTE PROCEDURE sp_status_retiro_fondo_ahorro_acti_manual( ?, ?, ?, ?, ? )"
              EXECUTE prp_manual_liquidacion USING  '0' , '60' , p_usuario , 'D' , p_folio -- rechazo, --estatus liquidacion
      END CASE 
      CALL fn_actualiza_opera_fin(pid,p_proceso_cod, p_opera_cod)
           RETURNING v_bandera

      LET v_cadena = " FIN ETAPA          : LIQUIDACION \n",
                     " FECHA              : ",TODAY,"\n",
                     " HORA               : ",TIME(CURRENT),"\n"

      -- se complementa el mensaje
      LET p_mensaje = "El proceso de Liquidación ha finalizado correctamente."

      

   ELSE
      -- el proceso no termino correctamente
      LET v_cadena = " --- ERROR ---\n",
                     " El proceso de Liquidación no terminó correctamente.\n",
                     " Código de error: ", v_bandera,"\n ",
                     " FECHA              : ",TODAY,"\n",
                     " HORA               : ",CURRENT HOUR TO SECOND,"\n"

      CALL fn_error_opera(pid, p_proceso_cod, p_opera_cod)
           RETURNING v_bandera

      -- se complementa el mensaje
      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de Liquidación no terminó correctamente.\n",
                      " Código de error : ", v_bandera,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"
   END IF
   -- se despliega la cadena con el mensaje de finalizacion
   DISPLAY v_cadena

   DISPLAY " = CIFRAS LIQUIDADAS = "
   -- se realiza la consulta de las cifras liquidadas
   LET v_cadena = " SELECT cta.subcuenta, cats.subcuenta_desc,\n",
                  "        cta.movimiento, catm.movimiento_desc,\n",
                  "        0, SUM(cta.importe)\n",
                  "   FROM cta_fondo72 cta, cat_movimiento catm, cat_subcuenta cats\n",
                  "  WHERE cta.folio_liquida = ",p_folio,"\n",
                  "    AND cta.subcuenta = cats.subcuenta\n",
                  "    AND cta.movimiento = catm.movimiento\n",
                  "  GROUP BY 1,2,3,4\n",
                  "  ORDER BY 1,3"
                  
   --DISPLAY v_cadena, " ERV"
   
   PREPARE prp_cifras_liq FROM v_cadena
   DECLARE cur_cifras_liq CURSOR FOR prp_cifras_liq

   FOREACH cur_cifras_liq INTO v_subcuenta, v_subcuenta_desc, v_movimiento,
                               v_movimiento_desc, v_sum_mto_acciones, v_sum_mto_pesos
      DISPLAY "SUBCUENTA       ", v_subcuenta, "-", v_subcuenta_desc
      DISPLAY "MOVIMIENTO      ", v_movimiento, "-", v_movimiento_desc
      DISPLAY "MONTO ACCIONES  ", v_sum_mto_acciones
      DISPLAY "MONTO PESOS     ", v_sum_mto_pesos
   END FOREACH

   -- se crea el titulo del mensaje
   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - LIQUIDACION"
   
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(pid, p_proceso_cod, p_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)

END MAIN