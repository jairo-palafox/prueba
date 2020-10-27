########################################################################################################
#Modulo       =>NOT                                                                                    #
#Programa     => NOT01                                                                                 #
#Objetivo     => Modifica el status de los archivos de notificación y envia los archivos por WS        #        
#Fecha_inicio => 20/01/2016                                                                             # 
#########################################################################################################
IMPORT security

GLOBALS "NOTW02.inc"
GLOBALS "NOTP06.inc"

DATABASE safre_viv

PRIVATE DEFINE p_usuario            CHAR(20)  -- parametro de entrada 
PRIVATE DEFINE v_pid                LIKE bat_ctr_proceso.pid  -- ID del proceso
PRIVATE DEFINE v_proceso_cod        SMALLINT 
PRIVATE DEFINE v_operacion_cod      SMALLINT
PRIVATE DEFINE v_folio              DECIMAL(9,0)
PRIVATE DEFINE v_archivo            VARCHAR(200)

PRIVATE DEFINE g_arr_inf DYNAMIC ARRAY OF pendientes

MAIN 
   DEFINE v_bandera        SMALLINT
   DEFINE v_resultado      SMALLINT 

    -- se recuperan los parametros del lanzador
   LET p_usuario = ARG_VAL(1)
   LET v_pid = ARG_VAL(2)
   LET v_proceso_cod = ARG_VAL(3)
   LET v_operacion_cod = ARG_VAL(4)
   LET v_folio = ARG_VAL(5)
   LET v_archivo = ARG_VAL(6)
   
   DISPLAY  "**********************************************************************************"
   DISPLAY  " PROCESO: ","ENVIAR ARCHIVOS DE NOTIFICACION"
   DISPLAY  " OPERACION: ","ENVIAR ARCHIVO"
   DISPLAY  " FECHA: ", MDY(MONTH(TODAY),DAY(TODAY),YEAR(TODAY))
   DISPLAY  " HORA:   ", CURRENT HOUR TO SECOND 
   DISPLAY  "**********************************************************************************"

   DISPLAY "Buscando los archivos ligados al folio de notificación..."
   CALL fn_carga_arr() RETURNING v_bandera
   IF (v_bandera == 0) THEN 
      DISPLAY " "
      DISPLAY "Inicia el proceso de envío de archivos..."
      CALL fn_envia_archivos()
      DISPLAY ""
      DISPLAY "Proceso de envío de archivos finalizado"

      CALL  fn_actualiza_opera_fin(v_pid,v_proceso_cod,v_operacion_cod)
      RETURNING v_resultado
      IF(v_resultado <> 0)THEN         
         # Actualiza a estado erróneo
         DISPLAY ""
         DISPLAY "Ocurrio un ERROR al intentar actualizar el estado de la operacion..."

         CALL fn_error_opera(v_pid,v_proceso_cod,v_operacion_cod) 
         RETURNING v_resultado
      ELSE 
         DISPLAY ""
         DISPLAY "*******************************************************************"
         DISPLAY ""
         DISPLAY "Termino la ejecución del proceso: "
         DISPLAY ""
         DISPLAY " PROCESO            : ","ENVIAR ARCHIVOS DE NOTIFICACION"
         DISPLAY " OPERACIÓN          : ","ENVIAR ARCHIVO"
         DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
         DISPLAY " HORA               : ",TIME(CURRENT)
         DISPLAY ""
         DISPLAY "*******************************************************************"
      END IF 

   ELSE 
      CALL fn_error_opera(v_pid,v_proceso_cod,v_operacion_cod) 
      RETURNING v_resultado
   END IF 
         
   WHENEVER ERROR STOP
END MAIN 


FUNCTION fn_carga_arr()
   DEFINE v_query     STRING 
   DEFINE v_cont      INTEGER 
   
   LET v_cont = 1

   LET v_query = "select ctr.folio_notifica,
                  ctr.folio_operativo,
                  det.id_not_detalle_archivo,
                  TRIM(det.nombre_archivo)
                  from not_ctr_archivo ctr,
                  not_detalle_archivo det
                  where ctr.folio_notifica = det.folio_notifica
                  and ctr.estado = 2"
   DECLARE cur_query CURSOR FROM v_query
   
   FOREACH cur_query INTO  g_arr_inf[v_cont].folio_not,
                           g_arr_inf[v_cont].folio_operativo,
                           g_arr_inf[v_cont].id_detalle_archivo,
                           g_arr_inf[v_cont].nombre_archivo
      LET v_cont = v_cont + 1
   END FOREACH 

   UPDATE not_ctr_archivo SET estado = 3 WHERE estado = 2

   RETURN 0
END FUNCTION

FUNCTION fn_envia_archivos()
   DEFINE archivo         STRING--Archivo que será enviado como string
   DEFINE v_cont          INTEGER
   DEFINE v_inn           tDT_CargarCampana_in
   DEFINE v_outt          tDT_CargarCampana_out
   DEFINE v_ruta          CHAR(40)
   DEFINE v_ruta_string   STRING 
   DEFINE v_codigo        VARCHAR(20)
   DEFINE v_des_resp      VARCHAR(200)

   SELECT TRIM(ruta_envio)
   INTO  v_ruta
   FROM seg_modulo
   WHERE modulo_cod = 'not'

    FOR v_cont=1 TO g_arr_inf.getLength() - 1
        LET v_ruta_string= v_ruta CLIPPED || "/" ||g_arr_inf[v_cont].nombre_archivo CLIPPED

        DISPLAY " "
        DISPLAY "*********************************************************************************"
        DISPLAY "Inicia la transferencia del archivo ", v_ruta_string CLIPPED

        CALL security.Base64.LoadBinary(v_ruta_string) RETURNING archivo
        LET v_inn.archivoB64 = archivo CLIPPED
        LET v_inn.fechaOperacion = TODAY USING 'yyyymmdd'
        LET v_inn.nombreArchivo = fn_elimina_extension(g_arr_inf[v_cont].nombre_archivo)
        LET v_inn.idCampana = g_arr_inf[v_cont].nombre_archivo[1,7]

        INITIALIZE v_outt TO NULL
        CALL fn_envia_notificacion(v_inn.*) RETURNING v_outt.*

        LET v_codigo = v_outt.codigoRetorno CLIPPED
        LET v_des_resp = v_outt.descripcion CLIPPED 
        
        IF v_outt.codigoRetorno.equalsIgnoreCase("00") THEN 
            UPDATE not_detalle_archivo
            SET estado=3
            WHERE id_not_detalle_archivo = g_arr_inf[v_cont].id_detalle_archivo
            
            INSERT INTO not_ctr_envio VALUES (seq_not_ctr_envio.nextval,g_arr_inf[v_cont].id_detalle_archivo,TODAY,v_codigo,v_des_resp,p_usuario)
        END IF 
        IF NOT v_outt.codigoRetorno.equalsIgnoreCase("00") AND v_outt.codigoRetorno IS NOT NULL THEN 
            UPDATE not_detalle_archivo
            SET estado = 4
            WHERE id_not_detalle_archivo = g_arr_inf[v_cont].id_detalle_archivo
            
            INSERT INTO not_ctr_envio VALUES (seq_not_ctr_envio.nextval,g_arr_inf[v_cont].id_detalle_archivo,TODAY,v_codigo,v_des_resp,p_usuario)
        ELSE 
            UPDATE not_detalle_archivo
            SET estado = 5
            WHERE id_not_detalle_archivo = g_arr_inf[v_cont].id_detalle_archivo

            LET v_des_resp = wsErr.description CLIPPED  

            INSERT INTO not_ctr_envio VALUES (seq_not_ctr_envio.nextval,g_arr_inf[v_cont].id_detalle_archivo,TODAY,'99',v_des_resp ,p_usuario)
            --LET v_codigo=99
        END IF

        DISPLAY " "
        DISPLAY "Codigo de retorno del servicio: ", v_codigo
        DISPLAY "Descripcion: ", v_des_resp
        DISPLAY " " 
        DISPLAY "*********************************************************************************"
    END  FOR
   DISPLAY " "
    
END FUNCTION  

FUNCTION fn_elimina_extension(p_nombre_archivo)
    DEFINE p_nombre_archivo STRING 
    DEFINE v_nom_arch       varchar(100)
    LET p_nombre_archivo=p_nombre_archivo.subString(1,p_nombre_archivo.getIndexOf(".",1)-1)
    LET v_nom_arch=p_nombre_archivo CLIPPED 
    RETURN v_nom_arch
END FUNCTION 