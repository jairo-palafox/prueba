#################################################################################
#Modulo       => CBD                                                            #
#Programa     => CBDS03                                                         #
#Objetivo     => Genera el archivo Amortización                                 #        
#Fecha_inicio => 25/02/2016                                                     # 
#################################################################################


DATABASE safre_viv 

MAIN 
   
    DEFINE r_resultado_opera          INTEGER
    
    #Parametros generales del proceso
    DEFINE p_pid                      DECIMAL(9,0)      -- PID del proceso
    DEFINE p_proceso_cod              SMALLINT          -- codigo del proceso
    DEFINE p_opera_cod                SMALLINT          -- codigo de la operacion
    DEFINE p_usuario_cod              CHAR(20)          -- clave del usuario firmado
    DEFINE v_folio                    DECIMAL(9,0)

    DEFINE v_proceso_desc             CHAR(40)
    DEFINE v_extension                CHAR(10)
    DEFINE v_opera_desc               CHAR(40)
    DEFINE v_layout                   SMALLINT
    DEFINE v_usuario_proceso          CHAR(20)
    DEFINE v_ruta_rescate             STRING
    DEFINE v_ruta_listados            CHAR(40)
    DEFINE p_nombre_archivo           CHAR(40)                               

    -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)


    WHENEVER ERROR CONTINUE
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario_proceso 

    #se solicita el numero de folio asociado a la operacion
      CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
      RETURNING v_folio

      #Se actualiza el folio del proceso
      UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid

      UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid
    #Encabezado para el archivo de monitoreo
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

   DISPLAY "Generando archivo de AMORTIZACION..."
   WHENEVER ERROR CONTINUE
   CALL fn_exporta_archivo_amortizacion(today)
   WHENEVER ERROR STOP
    # Finaliza la operacion
    CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)

    RETURNING r_resultado_opera
    IF(r_resultado_opera <> 0)THEN         
                     # Actualiza a estado erróneo
                     DISPLAY "Ocurrio un ERROR al intentar actualizar el estado de la operacion..."
                     CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
                     RETURNING r_resultado_opera
                  END IF


    
     DISPLAY "*******************************************************************"
                  DISPLAY ""
                  DISPLAY "Termino la generacion del archivo: "
                  DISPLAY ""
                  DISPLAY " PROCESO            : ",v_proceso_desc
                  DISPLAY " OPERACIÓN          : ",v_opera_desc
                  DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
                  DISPLAY " HORA               : ",TIME(CURRENT)
                  DISPLAY ""
     DISPLAY "*******************************************************************"
     
END MAIN 


FUNCTION fn_exporta_archivo_amortizacion(p_fcorte)
   DEFINE p_fcorte            DATE 
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio
   DEFINE v_nombre_archivo    STRING
   DEFINE v_archivo           base.Channel
   DEFINE v_shell             base.Channel 
   DEFINE v_formato_fecha     STRING 
   DEFINE v_salida            STRING 
   DEFINE v_nombre_shell      STRING 
   DEFINE v_comando           STRING 
   DEFINE v_sum_acciones      decimal(15,3) 
   DEFINE v_sum_pesos         decimal(17,6)
   DEFINE v_cont              INTEGER 
   DEFINE v_c_sum_acciones    STRING  
   DEFINE v_c_sum_pesos       STRING  
   DEFINE v_c_cont            STRING  
   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'cbd'

   LET p_fcorte = MDY(MONTH(p_fcorte),1,YEAR(p_fcorte)) - 1;

   LET v_nombre_archivo = v_ruta_envio CLIPPED,"/EXPORTA_ARCHIVO_AMORTIZACION.sql"
   LET v_formato_fecha = p_fcorte USING 'yyyymmdd'
   LET v_salida = "SALDO_AMORTIZACION_" || v_formato_fecha || ".sdo_amort"
   
   LET v_archivo = base.Channel.create()
   CALL v_archivo.openFile(v_nombre_archivo,"w")
   CALL v_archivo.writeLine('SET PDQPRIORITY HIGH;')
   CALL v_archivo.writeLine('unload TO ' || v_ruta_envio CLIPPED || '/' || v_salida CLIPPED||" DELIMITER '|'")
   CALL v_archivo.writeLine('SELECT')
   CALL v_archivo.writeLine('afi.nss,')
   CALL v_archivo.writeLine('sum(cta.monto_acciones),')
   CALL v_archivo.writeLine('SUM(cta.monto_acciones * gf.precio_fondo) ')
   CALL v_archivo.writeLine('from cta_movimiento cta, afi_derechohabiente afi, glo_valor_fondo gf')
   CALL v_archivo.writeLine('where afi.id_derechohabiente=cta.id_derechohabiente and')
   CALL v_archivo.writeLine('cta.subcuenta=41 and ')
   CALL v_archivo.writeLine('gf.fondo=cta.fondo_inversion and')  
   CALL v_archivo.writeLine('gf.f_valuacion='||'"'||p_fcorte||'"'||' and')
   CALL v_archivo.writeLine('cta.f_liquida <='||'"'||p_fcorte||'"'||' and')
   CALL v_archivo.writeLine('cta.fondo_inversion = 10')
   CALL v_archivo.writeLine('GROUP BY afi.nss,')
   CALL v_archivo.writeLine('cta.subcuenta,')
   CALL v_archivo.writeLine('cta.fondo_inversion;')

   CALL v_archivo.close()

   LET v_comando = "dbaccess safre_viv ", v_nombre_archivo
   RUN v_comando

   LET v_comando = "rm ", v_nombre_archivo
   RUN v_comando

   
    LET v_comando="
                        SELECT
                            sum(cta.monto_acciones) sum1,
                            SUM(cta.monto_acciones * gf.precio_fondo) sum2
                        from cta_movimiento cta, afi_derechohabiente afi, glo_valor_fondo gf
                        where afi.id_derechohabiente=cta.id_derechohabiente and
                            cta.subcuenta=41 and
                            gf.fondo=cta.fondo_inversion and
                            gf.f_valuacion=? and
                            cta.f_liquida <=? and
                            cta.fondo_inversion = 10;"                     
    PREPARE prp_query_s FROM v_comando
    EXECUTE prp_query_s USING p_fcorte,p_fcorte INTO v_sum_acciones,v_sum_pesos
   
    LET v_c_sum_acciones=v_sum_acciones   
    LET v_c_sum_pesos= v_sum_pesos 
    LET v_c_sum_acciones = v_c_sum_acciones CLIPPED 
    LET v_c_sum_pesos= v_c_sum_pesos CLIPPED

    --Se genera un archivo shell para obtener en número de registros que hay en el archivo
    LET v_nombre_shell=v_ruta_envio CLIPPED,"/sumario_am.sh" 
    LET v_shell=base.channel.create()
    CALL v_shell.openFile(v_nombre_shell,"w")
    CALL v_shell.writeLine('for nm in `wc -l '||v_ruta_envio CLIPPED || '/' || v_salida CLIPPED||'`')
    CALL v_shell.writeLine('do')
    CALL v_shell.writeLine('echo $nm>am.txt')--El resultado es enviado a un archivo temporal para ser leido por el programa
    CALL v_shell.writeLine('break')
    CALL v_shell.writeLine('done')
    CALL v_shell.close()
    RUN "cd "||v_ruta_envio||";chmod 775 sumario_am.sh;./"||"sumario_am.sh"
    --Inicia la lectura del número de registros para su mnipulación desde el programa
    CALL v_shell.openFile(v_ruta_envio CLIPPED||"/am.txt", "r")
    LET v_c_cont = v_shell.readLine() 
    RUN "cd "||v_ruta_envio||";rm sumario_am.sh am.txt"
    --Se imprime el sumario en el archivo correspondiente
    LET v_comando='echo "S|'||v_c_sum_acciones||'|'||v_c_sum_pesos ||'|'||v_c_cont CLIPPED ||'|" >>' || v_ruta_envio CLIPPED || '/' || v_salida CLIPPED
    RUN v_comando
    DISPLAY "Se ha generado el archivo en la ruta: ",v_ruta_envio CLIPPED || '/' || v_salida CLIPPED
   


END FUNCTION 
