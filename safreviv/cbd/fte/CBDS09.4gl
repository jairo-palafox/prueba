################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 09/11/2017                                        #
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
#------------------------------------------------------------------------------#
#Modulo            => CBD                                                      #
#Programa          => CBDS09                                                   #
#Objetivo          => Programa para generar archivo de extracción de           #
#                     Conciliación RENAPO                                      #
#Fecha Inicio      => 09/11/2017                                               #
################################################################################

DATABASE safre_viv

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)   -- PID del proceso
PRIVATE DEFINE p_proceso_cod              LIKE cat_proceso.proceso_cod -- codigo del proceso
PRIVATE DEFINE p_opera_cod                LIKE cat_operacion.opera_cod -- codigo de operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)       -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(80)       -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)

PRIVATE DEFINE p_tot_reg                  DECIMAL(10,0)

PRIVATE DEFINE v_proceso_desc             CHAR(40)
PRIVATE DEFINE v_extension                CHAR(10)
PRIVATE DEFINE v_opera_desc               CHAR(40)
PRIVATE DEFINE v_layout                   SMALLINT
PRIVATE DEFINE v_usuario_proceso          CHAR(20)
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            CHAR(80)

MAIN
   DEFINE r_resultado_opera               INTEGER
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio

   DEFINE v_resultado                     SMALLINT

   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)   
   LET p_nombre_archivo = ARG_VAL(5)
   


   --WHENEVER ERROR CONTINUE

	CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario_proceso  

   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'cbd'
   
   #Encabezado para el archivo de monitoreo
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

   #Generando el folio del proceso
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
   RETURNING v_folio

      DROP TABLE IF EXISTS tmp_marca_inactiva;
         SELECT id_derechohabiente
           FROM sfr_marca_activa
          WHERE marca IN (150,151,160)
      INTO TEMP tmp_marca_inactiva

      UPDATE STATISTICS FOR TABLE tmp_marca_inactiva;

      PREPARE eje_prioridad FROM "SET PDQPRIORITY HIGH"
      EXECUTE eje_prioridad

      SELECT COUNT(*)
        INTO p_tot_reg
        FROM afi_derechohabiente ad
       WHERE ad.id_derechohabiente NOT IN ( SELECT id_derechohabiente FROM tmp_marca_inactiva)
         AND ad.curp IS NOT NULL AND ad.curp <> ' '

      PREPARE eje_noprioridad FROM "SET PDQPRIORITY LOW"
      EXECUTE eje_noprioridad    
      

   
   #Se actualiza el folio del proceso               
   UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid

   UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid

   DISPLAY ""
   DISPLAY "Inicia la generación del archivo de Conciliación RENAPO"   

   #Se genera el archivo de salida
   CALL fn_genera_salida() RETURNING v_resultado
   
   IF v_resultado = 0 THEN
      #Se genera el reporte con las cifras control
      CALL fn_genera_reporte()
      # Finaliza la operacion de generacion del archivo
      CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
      RETURNING r_resultado_opera
      IF(r_resultado_opera <> 0)THEN         
         # Actualiza a estado erróneo
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
      END IF
      DISPLAY "*******************************************************************"
      DISPLAY "Termino el proceso que exporta la Conciliación RENAPO"
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY ""
      DISPLAY "La salida se encuentra en el archivo:"
      DISPLAY v_ruta_envio CLIPPED,"/",p_nombre_archivo CLIPPED
      DISPLAY "*******************************************************************"
   ELSE
      DISPLAY "*******************************************************************"
      DISPLAY "Ocurrio un ERROR al generar el archivo"
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY "*******************************************************************"
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
   END IF
END MAIN

PRIVATE FUNCTION fn_genera_salida()
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio
   DEFINE v_nombre_archivo    STRING
   DEFINE v_archivo           base.Channel
   DEFINE v_query             STRING
   DEFINE v_linea             STRING 

   DEFINE v_campos_vacios     CHAR(32)
   DEFINE cve_dependencia     CHAR(20)
   DEFINE v_curp              CHAR(18)
   DEFINE v_nss               CHAR(11)
   DEFINE v_ap_paterno_af     CHAR(40)
   DEFINE v_ap_materno_af     CHAR(40)
   DEFINE v_nombre_af         CHAR(40)
   DEFINE v_sexo              CHAR(1)
   DEFINE v_sexo_val          CHAR(1)
   DEFINE v_f_nacimiento      DATE

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'cbd'


   LET v_campos_vacios = "@|@|@|@|@|@|@|@|@|@|@|@|@|@|@|"
   LET v_nombre_archivo = v_ruta_envio CLIPPED,"/",p_nombre_archivo

   --Se crea el manejador de archivo y se indica que se escribirá en el mismo
   LET v_archivo = base.Channel.create()
   CALL v_archivo.openFile(v_nombre_archivo,"w" )
   CALL v_archivo.setDelimiter("")


   EXECUTE eje_prioridad 

   
   DECLARE cur_sl_der CURSOR FOR 
        SELECT ad.curp, 
             ad.nss ,
             ad.ap_paterno_af,
             ad.ap_materno_af,
             ad.nombre_af    ,
             ad.sexo         ,
             ad.f_nacimiento
        FROM afi_derechohabiente ad
       WHERE ad.id_derechohabiente NOT IN ( SELECT id_derechohabiente FROM tmp_marca_inactiva)
         AND ad.curp IS NOT NULL AND ad.curp <> ' '

   FOREACH cur_sl_der INTO v_curp,
                           v_nss,
                           v_ap_paterno_af,
                           v_ap_materno_af,
                           v_nombre_af,
                           v_sexo, 
                           v_f_nacimiento


      IF v_sexo = '1' THEN 
	     LET v_sexo_val = 'H'
      ELSE
         IF v_sexo = '2' THEN 
            LET v_sexo_val = 'M'
         ELSE
            LET v_sexo_val = ' '
         END IF
      END IF                            

      LET v_linea = v_curp CLIPPED, "@|", 
                    v_nss CLIPPED, "@|",
                    v_ap_paterno_af CLIPPED, "@|",
                    v_ap_materno_af CLIPPED, "@|",
                    v_nombre_af CLIPPED, "@|",
                    v_sexo_val CLIPPED, "@|", 
                    v_f_nacimiento USING "dd/mm/yyyy", "@|",
                    v_curp[12,13] CLIPPED, "@|", 
                    v_campos_vacios CLIPPED
                      
      CALL v_archivo.write([v_linea CLIPPED])      
                           
   END FOREACH
   EXECUTE eje_noprioridad    
   CALL v_archivo.close()                           

   --CALL fn_mensaje("Información","Se ha generado el archivo de Información textual para confronta \n en la ruta "||v_nombre_archivo,"information") 
   
   RETURN 0
END FUNCTION

FUNCTION fn_genera_reporte()
   DEFINE preview             SMALLINT
   DEFINE vhandler            om.SaxDocumentHandler

   DEFINE v_ruta_ejecutable   LIKE seg_modulo.ruta_bin -- Ruta del ejecutable
   DEFINE v_nombre	         STRING

   SELECT ruta_bin
   INTO v_ruta_ejecutable
   FROM seg_modulo 
   WHERE modulo_cod = 'cbd'

   LET preview = FALSE
   INITIALIZE vhandler TO NULL

   LET v_nombre = v_ruta_ejecutable CLIPPED, "/CBDS09.4rp"
   LET vhandler = fn_configuracion(v_nombre, "PDF", preview )
   
   START REPORT rep_cifras TO XML HANDLER vhandler
      OUTPUT TO REPORT rep_cifras ()
   FINISH REPORT rep_cifras
   
   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY "Termino la generación del archivo de cifras control "
   DISPLAY ""
   DISPLAY "*******************************************************************"
END FUNCTION


FUNCTION fn_configuracion(v_reporte, v_formato, v_preview)
---------------------------------------------------------------------------

   DEFINE 
    v_reporte                STRING,
    v_formato                STRING,
    v_preview                INTEGER

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'cbd'

   LET v_ruta_listados = v_ruta_listados CLIPPED , "/" ,
                         p_usuario_cod CLIPPED , "-", -- usuario
                         "CBDS09" CLIPPED, "-", -- programa
                         p_pid USING "&&&&&","-", -- PID
                         p_proceso_cod USING "&&&&&", "-", -- codigo del proceso
                         p_opera_cod   USING "&&&&&",".pdf" -- codigo de la operación

 
   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- if  the file loaded OK
      CALL fgl_report_setOutputFileName(v_ruta_listados)
      CALL fgl_report_selectDevice(v_formato)
      CALL fgl_report_selectPreview(v_preview)
   ELSE
      DISPLAY "Error: No se pudo encontrar el archivo ", v_reporte
      EXIT PROGRAM
   END IF
  
   RETURN fgl_report_commitCurrentSettings()

END FUNCTION


REPORT rep_cifras () 
   DEFINE v_fecha DATE 
   
   FORMAT

   FIRST PAGE HEADER
      LET v_fecha = TODAY      

      PRINTX p_usuario_cod
      PRINTX v_fecha USING "dd-mm-yyyy"
      PRINTX v_folio
      
      PRINTX p_nombre_archivo
      PRINTX p_tot_reg
      
END REPORT
 