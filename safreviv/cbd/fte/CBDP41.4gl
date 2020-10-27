#################################################################################
#Modulo       => CBD                                                            #
#Programa     => CBDP41                                                         #
#Objetivo     => Genera el archivo de cuentas sin saldo                         #        
#Fecha_inicio => 02/12/2016                                                     # 
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

    DEFINE p_fecha                    DATE --fecha con la que se realizara el archivo

    -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
  -- LET v_folio          = ARG_VAL(5)
  -- LET p_nombre_archivo = ARG_VAL(6)
   LET p_fecha          = ARG_VAL(5)


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

   DISPLAY "Generando archivo de cuentas sin saldo..."
   WHENEVER ERROR CONTINUE
   CALL fn_exporta_archivo_cuentas_sin_saldo(p_fecha)
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

FUNCTION fn_exporta_archivo_cuentas_sin_saldo(p_fecha)
   DEFINE p_fecha             date  --Parametro de la fecha de corte
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio
   DEFINE v_nombre_archivo    STRING
   DEFINE v_archivo           base.Channel
   DEFINE v_shell             base.Channel 
   DEFINE v_formato_fecha     STRING 
   DEFINE v_salida            STRING 
   DEFINE v_nombre_shell      STRING 
   DEFINE v_comando           STRING 
   DEFINE v_c_cont            STRING  
   DEFINE v_tabla             VARCHAR(25)
   DEFINE v_query             STRING 
   
   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'cbd'

  --Se busca cual es la tabla activa de saldos
  SELECT tabla_saldo
    INTO v_tabla
    FROM safre_sdo@vivws_tcp:glo_saldo
    WHERE ind_saldo = 1

   LET v_nombre_archivo = v_ruta_envio CLIPPED,"/CUENTAS_SIN_SALDOS.sql"
   LET v_formato_fecha = today USING 'yyyymmdd'
   LET v_salida = "CUENTAS_SIN_SALDO_" || v_formato_fecha || ".cns"

   LET v_archivo = base.Channel.create()
   CALL v_archivo.openFile(v_nombre_archivo,"w")
   CALL v_archivo.writeLine('SET PDQPRIORITY HIGH;')
   CALL v_archivo.writeLine('unload TO ' || v_ruta_envio CLIPPED || '/' || v_salida CLIPPED|| " DELIMITER ' ' ")
   CALL v_archivo.writeLine('SELECT afi.nss')
   CALL v_archivo.writeLine('from afi_derechohabiente afi')
   CALL v_archivo.writeLine('WHERE afi.id_derechohabiente IN ')
   CALL v_archivo.writeLine('        (select   id_derechohabiente')
   CALL v_archivo.writeLine('			from  sfr_marca_activa')
   CALL v_archivo.writeLine('           where marca not in (150,151,160))')
   CALL v_archivo.writeLine('      and afi.id_derechohabiente NOT IN')
   CALL v_archivo.writeLine('        (select sdo.id_Derechohabiente')
   CALL v_archivo.writeLine('           from safre_sdo@vivws_tcp:'||v_tabla CLIPPED||' sdo )')
  -- let v_query=             '           where sdo.f_saldo<MDY(',month(p_fecha),',',day(p_fecha),',',year(p_fecha),'))'
   --CALL v_archivo.writeLine(v_query)
   CALL v_archivo.close()

   DISPLAY "Inicia la creación del archivo"  
   LET v_comando = "dbaccess safre_viv ", v_nombre_archivo
   RUN v_comando
   DISPLAY ""
   DISPLAY "Termina la creación del archivo"
   LET v_comando = "rm ", v_nombre_archivo
   RUN v_comando

    --Se genera un archivo shell para obtener en número de registros que hay en el archivo
    LET v_nombre_shell=v_ruta_envio CLIPPED,"/sumario.sh" 
    LET v_shell=base.channel.create()
    CALL v_shell.openFile(v_nombre_shell,"w")
    CALL v_shell.writeLine('for nm in `wc -l '||v_ruta_envio CLIPPED || '/' || v_salida CLIPPED||'`')
    CALL v_shell.writeLine('do')
    CALL v_shell.writeLine('echo $nm>am.txt')--El resultado es enviado a un archivo temporal para ser leido por el programa
    CALL v_shell.writeLine('break')
    CALL v_shell.writeLine('done')
    CALL v_shell.close()
    RUN "cd "||v_ruta_envio||";chmod 775 sumario.sh;./"||"sumario.sh"
    --Inicia la lectura del número de registros para su mnipulación desde el programa
    CALL v_shell.openFile(v_ruta_envio CLIPPED||"/am.txt", "r")
    LET v_c_cont = v_shell.readLine() 
    RUN "cd "||v_ruta_envio||";rm sumario.sh am.txt"
    --Se imprime el sumario en el archivo correspondiente
    LET v_comando='echo "S|'||v_c_cont CLIPPED ||'|" >>' || v_ruta_envio CLIPPED || '/' || v_salida CLIPPED
    RUN v_comando
    DISPLAY "Se ha generado el archivo en la ruta: ",v_ruta_envio CLIPPED || '/' || v_salida CLIPPED
    
END FUNCTION 