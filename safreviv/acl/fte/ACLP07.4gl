--===============================================================
-- Version: 1.1.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

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
DEFINE p_usuario           CHAR(20)
DEFINE pid                 DECIMAL(9,0)
DEFINE p_proceso_cod       SMALLINT
DEFINE p_opera_cod         SMALLINT
DEFINE p_tabla             CHAR(40)
DEFINE p_id_referencia     DECIMAL(9,0)


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
   DEFINE v_estatus           SMALLINT

   DEFINE p_titulo            STRING -- titulo del mensaje enviado en el correo
   DEFINE p_mensaje           STRING -- cuerpo del mensaje enviado
         ,p_programa_cod        VARCHAR(10)

   -- se recuperan los parametros
   LET p_usuario        = ARG_VAL(1)
   LET pid              = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_tabla          = ARG_VAL(6)
   LET p_id_referencia  = ARG_VAL(7)
   
 
  #validar operación (fn_valida)
  #inicializa operación
    #<> 0 si hace = inicialliza operación (por operación)
  #termina operacion
    
  
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario
   
   {-- se mueve el inicio de proceso al programa lanzador
   --Se marca el proceso como inciado
   LET v_estatus = fn_actualiza_opera_ini(pid
                                          ,p_proceso_cod
                                          ,p_opera_cod
                                          ,p_folio
                                          ,"ACLC10"
                                          ,"N/A"
                                          ,p_usuario)
   }
   
   CALL fn_display_proceso(0,"Liquidación aclaratorio")
   DISPLAY "v_estatus: ",v_estatus
   -- se despliega el inicio de la etapa 
   LET v_cadena = " PROCESO            : ",v_proceso_desc,"\n",
                  " OPERACIÓN          : ",v_opera_desc,"\n",
                  " FOLIO              : ",p_folio,"\n",
                  " TABLA              : ",p_tabla,"\n",
                  " ID REFERENCIA      : ",p_id_referencia,"\n",
                  " FECHA              : ",TODAY,"\n",
                  " HORA               : ",TIME(CURRENT),"\n \n \n",
                  " INICIO ETAPA       : LIQUIDACION",
                  " FECHA              : ",TODAY,"\n",
                  " HORA               : ",TIME(CURRENT),"\n \n \n"

  -- DISPLAY v_cadena

   # p_proceso_cod = 2 --> realiza la liquidacion para cualquier modulo
   LET v_cadena = "EXECUTE PROCEDURE sp_liquida_manual_acl(?,?,?,?,?,?)"
   
   
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
   
-- se obtiene el codigo de programa
                  SELECT programa_cod
                  INTO   p_programa_cod
                  FROM   cat_operacion
                  WHERE  proceso_cod = p_proceso_cod
                  AND    opera_cod   = p_opera_cod

                  CALL fn_reporte_liquidacion(p_folio, "cta_movimiento",
                                              p_usuario, pid,
                                              p_proceso_cod, p_opera_cod,
                                              p_programa_cod, FALSE)
   
   
      CALL fn_actualiza_opera_fin(pid,p_proceso_cod, p_opera_cod)
      RETURNING v_bandera
      LET v_cadena = " FIN ETAPA          : LIQUIDACION",
                     " FECHA              : ",TODAY,"\n",
                     " HORA               : ",TIME(CURRENT),"\n \n \n"
      LET p_mensaje = "Liquidación realizada con éxito.\nYa se puede continuar con la Generación del Archivo."
   ELSE
      -- el proceso no termino correctamente
      LET v_cadena = " --- ERROR ---\n",
                     " El proceso de liquidación no terminó correctamente.\n",
                     " Código de error: ", v_bandera,"\n \n",
                     " FECHA              : ",TODAY,"\n",
                     " HORA               : ",TIME(CURRENT),"\n \n \n"

      CALL fn_error_opera(pid, p_proceso_cod, p_opera_cod)
        RETURNING v_bandera
      LET p_mensaje = "El proceso de Liquidación ha finalizado pero con errores.\nNo se puede continuar con el proceso de Generación del Archivo."  
   END IF
   DISPLAY "función para enviar el correo"
   LET p_titulo = "Finalización de operación - Salida Aclaraciones - Liquidación"
   CALL fn_correo_proceso(pid, p_proceso_cod, 
                          p_opera_cod, 
                          NULL, p_titulo,p_mensaje)
   CALL fn_display_proceso(1,"Liquidación aclaratorio")
   -- se despliega la cadena con el mensaje de finalizacion
   DISPLAY v_cadena
   
END MAIN