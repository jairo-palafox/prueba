#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => NOT                                                     #
#Programa          => NOTP02                                                  #
#Objetivo          => PROGRAMA PARA GENERAR LOS ARCHIVOS DE NOTIFICACIONES    #
#Fecha Inicio      => 04-FEBRERO-2015                                         #
###############################################################################
IMPORT os
DATABASE safre_viv

GLOBALS "NOTP02.inc"

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                               -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                               -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)
PRIVATE DEFINE v_folio_origen             DECIMAL(9,0)

PRIVATE DEFINE v_proceso_desc             CHAR(40)
PRIVATE DEFINE v_extension                CHAR(10)
PRIVATE DEFINE v_opera_desc               CHAR(40)
PRIVATE DEFINE v_layout                   SMALLINT
PRIVATE DEFINE v_usuario_proceso          CHAR(20)
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            CHAR(40)
PRIVATE DEFINE v_ruta_envio               CHAR(40) 

PRIVATE DEFINE v_nom_tabla                VARCHAR(100)
DEFINE g_arr_file_dir   DYNAMIC ARRAY OF RECORD 
    nombre_archivo      VARCHAR(34)
END RECORD 

MAIN
   DEFINE r_resultado_opera               INTEGER

   DEFINE v_valida_mensaje                STRING
   DEFINE v_nss_validacion                CHAR(11)

   DEFINE v_resultado                     SMALLINT
   DEFINE v_genera_reporte                BOOLEAN

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

   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'not'
   
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"
   
   SELECT folio_referencia INTO v_folio_origen FROM glo_folio where folio = v_folio

   SELECT cat.nom_tabla
   INTO v_nom_tabla
   FROM glo_folio fol
   INNER JOIN cat_notificacion cat ON cat.proceso_cod_notifica = fol.proceso_cod
   INNER JOIN glo_folio fol2 ON fol2.proceso_cod = cat.proceso_cod_origen
   WHERE fol.folio = v_folio
   AND fol2.folio = v_folio_origen;

   LET v_valida_mensaje = "SELECT FIRST 1 nss FROM ", v_nom_tabla, " WHERE folio = ?"
   PREPARE exe_valida_mensaje FROM v_valida_mensaje
   
   #Validamos si existe algun mensaje a enviar
   EXECUTE exe_valida_mensaje USING v_folio
                              INTO v_nss_validacion

   IF v_nss_validacion IS NULL OR v_nss_validacion = "" THEN
      DISPLAY ""
      DISPLAY "Para el folio ", v_folio CLIPPED, " no existen trabajadores con solicitud de notificación"
      DISPLAY ""
      LET v_resultado = 0
      LET v_genera_reporte = FALSE
      
   ELSE
      CALL fn_genera_salida() RETURNING v_resultado
      LET v_genera_reporte = TRUE
   END IF
   
   IF v_resultado = 0 THEN
      IF v_genera_reporte THEN 
         CALL fn_genera_reporte(v_folio)
      END IF
      
      CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
      RETURNING r_resultado_opera
      
      IF(r_resultado_opera <> 0)THEN         
         # Actualiza a estado erróneo
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
      ELSE
         #Actualizamos el estado del proceso a finalizado
         UPDATE not_ctr_proceso set estado = 3 WHERE folio = v_folio_origen AND proceso_cod_notifica = p_proceso_cod
            
         DISPLAY ""
         DISPLAY "*******************************************************************"
         DISPLAY ""
         DISPLAY "Termino la ejecución del proceso: "
         DISPLAY ""
         DISPLAY " PROCESO            : ",v_proceso_desc
         DISPLAY " OPERACIÓN          : ",v_opera_desc
         DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
         DISPLAY " HORA               : ",TIME(CURRENT)
         DISPLAY ""
         DISPLAY "*******************************************************************"
      END IF
   ELSE
      DISPLAY "*******************************************************************"
      DISPLAY ""
      DISPLAY "Ocurrio un ERROR al generar el archivo de Notificación"
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY ""
      DISPLAY "*******************************************************************"
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
      
      UPDATE not_ctr_proceso set estado = 9 WHERE folio = v_folio_origen AND proceso_cod_notifica = p_proceso_cod
   END IF
   
   CALL fn_valida_proceso_pendiente()
END MAIN

FUNCTION fn_genera_salida()
   DEFINE v_proceso_cod_origen  SMALLINT 
   DEFINE v_nom_tabla           VARCHAR(100)
   DEFINE v_id_cat_not_contrato DECIMAL(9,0)
   #DEFINE v_ruta_envio          VARCHAR(100)
   #DEFINE v_ruta_string_envio   STRING 
   DEFINE v_query               STRING 
   DEFINE v_consulta            STRING 
   DEFINE v_codigo              SMALLINT
   DEFINE v_cont                INTEGER   
   DEFINE channel_1             base.Channel
   DEFINE v_ruta_ch1            STRING
   DEFINE v_nombre_arch_not     STRING --Nombre del archivo not con ruta
   DEFINE v_nombre              STRING --nombre del archivo not sin ruta
   DEFINE arr_campos DYNAMIC ARRAY OF RECORD 
      nombre VARCHAR (80),
      orden  INTEGER
   END RECORD 

      #LET v_ruta_string_envio = v_ruta_envio
      
   --Archivo sql que generará el archivo de notificación     
   LET v_ruta_ch1 = v_ruta_envio CLIPPED, "/not_envio_", v_folio USING "&&&&&&&&&",".sql"
   LET channel_1 = base.Channel.create()
   CALL channel_1.openfile(v_ruta_ch1, "w")

   --Nombre de la tabla a realizar la consulta
   SELECT
      noti.proceso_cod_origen,
      noti.nom_tabla
   INTO
      v_proceso_cod_origen,
      v_nom_tabla
   FROM cat_notificacion noti
   INNER JOIN glo_folio fol ON fol.proceso_cod = noti.proceso_cod_origen
   WHERE fol.folio = v_folio_origen
   AND noti.proceso_cod_notifica = p_proceso_cod
      
   --Se obiene el código del contrato y el id_cat_not_contrato 
   SELECT 
      cont.id_cat_not_contrato,
      cont.codigo
   INTO 
      v_id_cat_not_contrato,
      v_codigo
   FROM 
      cat_not_contrato cont,
      not_contrato_notificacion connot,
      cat_notificacion          noti,
      glo_folio                 glo
   WHERE 
      noti.id_cat_notificacion = connot.id_cat_notificacion
      AND connot.id_cat_not_contrato = cont.id_cat_not_contrato
      AND noti.proceso_cod_origen = v_proceso_cod_origen 
      AND noti.proceso_cod_notifica = glo.proceso_cod 
      AND glo.folio = v_folio
      
  --Obtenemos el nombre y el orden de los campos que irán en el archivo      
      LET v_cont = 1  
      LET v_query="select nombre, 
                          orden 
                   from cat_not_campo 
                   where id_cat_not_contrato = ?
                   order by orden"
      PREPARE prp_query FROM v_query
      DECLARE cur_query CURSOR FOR prp_query
      FOREACH cur_query USING v_id_cat_not_contrato INTO arr_campos[v_cont].*
        LET v_cont=v_cont+1
      END FOREACH 
      
    --Archivo notificación
      LET v_nombre="SACI-",v_codigo USING "&&","_",TODAY USING "yyyymmdd","_",v_folio USING "&&&&&&&&&" ,".txt"
    --Archivo notificacion con ruta
      LET v_nombre_arch_not = v_ruta_envio CLIPPED,"/",v_nombre CLIPPED 

      LET v_query=""
    --Se obtiene los campos de la tabla en el orden asignado para ser ejecutado en una consulta
      FOR v_cont=1 TO arr_campos.getLength()-1
        IF v_cont<>arr_campos.getLength()-1 THEN 
            LET v_consulta=v_consulta,arr_campos[v_cont].nombre CLIPPED,","
        ELSE
            LET v_consulta=v_consulta,arr_campos[v_cont].nombre CLIPPED--Se evita concatenar ","
        END IF 
      END FOR 

      --Se escribe el query que creará el archivo notificación
      CALL channel_1.writeLine('unload to ' || v_nombre_arch_not)
      CALL channel_1.writeLine('SELECT ' || v_consulta || ' from ' || v_nom_tabla || ';')
      CALL channel_1.close()
      LET v_query="dbaccess safre_viv ",v_ruta_ch1
      RUN v_query--Se crea el archivo notificación
      
      DISPLAY "Creando archivos de notificación"
      CALL fn_divide_archivo(v_ruta_envio CLIPPED, v_nombre)
      DISPLAY "Se han creado los archivos de notificaciones en la ruta: ", v_ruta_envio CLIPPED
      CALL  fn_carga_arr_dir(v_ruta_envio CLIPPED, v_codigo,v_folio)
      RUN "rm "||v_ruta_ch1
      CALL fn_inserta_archivos_bd()--Carga los subarchivos creados en la BD
    RETURN 0
END FUNCTION 

FUNCTION fn_divide_archivo(v_ruta, p_nombre)--Crea un shell que divide el archivo notificacion en diferentes subarchivos más pequeños
    DEFINE channel_1          base.Channel 
    DEFINE v_no_lineas_arch   INTEGER 
    DEFINE v_ruta             STRING 
    DEFINE p_nombre           STRING 
    DEFINE v_nombre_sh        STRING 
    
    LET v_nombre_sh="divide_notificacion_",v_folio USING "&&&&&&&&&",".sh"
    LET channel_1=base.Channel.create()
    CALL channel_1.openfile(v_ruta CLIPPED||"/"||v_nombre_sh CLIPPED , "w")
    
    --Se obtine el número de lineas que contendran los subarchivos de notificación 
    CALL FGL_GETRESOURCE( "not.archivo.lineas" ) RETURNING v_no_lineas_arch
    --Se escriben los comandos shell
    CALL channel_1.writeLine("split -l "||v_no_lineas_arch||" -d -a 2 "||v_ruta CLIPPED ||"/"||p_nombre||" "||v_ruta CLIPPED||"/"||p_nombre||"_")
    CALL channel_1.writeLine("cd "||v_ruta CLIPPED||"/")
    CALL channel_1.writeLine("for var in `ls "||p_nombre||"_*`")
    CALL channel_1.writeLine("  do")
    CALL channel_1.writeLine("  for nm in `wc -l $var`")
    CALL channel_1.writeLine("     do")
    CALL channel_1.writeLine("     namef=`expr substr $var 1 17`")
    CALL channel_1.writeLine("     sname=`expr substr $var 18 13`")
    CALL channel_1.writeLine("     num=`expr substr $var 32 2`")
    CALL channel_1.writeLine('     name="$namef$num$sname"')
    CALL channel_1.writeLine('     mv $var $name')
    CALL channel_1.writeLine("     break")
    CALL channel_1.writeLine("  done")
    CALL channel_1.writeLine(" serial=`expr 1000000000 + $nm`")
    CALL channel_1.writeLine(" echo S${serial:1:9}>>$name")
    CALL channel_1.writeLine ("sed 's/|$//' $name > $var")
    CALL channel_1.writeLine ("sed 's/$/\r/' $var > $name")
    CALL channel_1.writeline("rm $var")
    CALL channel_1.writeLine("done")
    CALL channel_1.close()
    RUN "cd "|| v_ruta CLIPPED||"/;chmod 777 "||v_nombre_sh CLIPPED ||";./"||v_nombre_sh clipped||";rm "||p_nombre||";rm "||v_nombre_sh
    
END FUNCTION 

--Carga un arreglo con el nombre de todos los subarchivos de notificacion creados
FUNCTION fn_carga_arr_dir(v_ruta,v_codigo,v_folio)
    DEFINE v_ruta                   STRING
    DEFINE v_codigo                 SMALLINT
    DEFINE v_folio                  decimal(9,0)
    DEFINE channel_1                base.Channel
    DEFINE v_cont                   SMALLINT 
    DEFINE v_comando                STRING 
    DEFINE v_ruta_ch1               STRING 
    LET v_cont=1
    --Archivo que contendra el nombre de todos los subarchivos de notificación creados
    LET v_ruta_ch1=v_ruta CLIPPED,"/not_envio_",v_folio USING "&&&&&&&&&",".txt"
    LET v_comando="cd ",v_ruta CLIPPED ,"/;"," ls ","SACI-",v_codigo USING "&&","_",TODAY USING "yyyymmdd","*",v_folio USING "&&&&&&&&&",".txt > not_envio_",v_folio USING "&&&&&&&&&",".txt"
    RUN v_comando--Se crea el archivo con los nombres de los subarchivos
    LET channel_1=base.Channel.create()
    CALL channel_1.openFile(v_ruta_ch1, "r")
    --Se lee el archivo con los nombres de los subarchivos linea por linea y se guardan los nombres en un arreglo 
    WHILE TRUE
            LET g_arr_file_dir[v_cont].nombre_archivo = channel_1.readLine()  
            LET v_cont=v_cont+1
            IF channel_1.isEof() THEN 
             EXIT WHILE
            END if
    END WHILE

    RUN "rm "||v_ruta_ch1--Se elimina el archivo que contiene los nombres de los subarchivos
END FUNCTION 


FUNCTION fn_inserta_archivos_bd()--Inserta los subarchivos creados en la BD
    DEFINE v_cont   INTEGER 
    
    INSERT INTO not_ctr_archivo
    VALUES (v_folio,v_folio_origen,TODAY,1)
    --DISPLAY g_arr_file_dir.getLength()
    
    FOR v_cont=1 TO g_arr_file_dir.getLength()-1
        INSERT INTO not_detalle_archivo 
        VALUES (seq_not_detalle_archivo.NEXTVAL,v_folio,g_arr_file_dir[v_cont].nombre_archivo,1)
    END for
    

END FUNCTION 
---------------------------------------------------------------------
PRIVATE FUNCTION fn_valida_proceso_pendiente()
   DEFINE v_usuario              CHAR(20)
   DEFINE v_folio_pendiente      DECIMAL(9,0)
   DEFINE v_ruta_ejecutable      VARCHAR(40) -- Ruta del ejecutable
   DEFINE v_ruta_listados        VARCHAR(40) -- Ruta del log
   DEFINE r_resultado_opera      INTEGER
   DEFINE v_nom_archivo          CHAR(40)
   DEFINE v_opera_cod_notifica   SMALLINT -- codigo de operacion
   DEFINE v_pid                  DECIMAL(9,0) -- ID del proceso
   DEFINE v_comando              STRING
   DEFINE v_consulta             STRING

   #Se valida si existe algun proceso formado para ejecucion
   LET v_consulta =  "SELECT FIRST 1 folio, usuario_cod ",
                     "FROM not_ctr_proceso ",
                     "WHERE proceso_cod_notifica = ? ",
                     "AND estado = 1 "
   PREPARE exe_consulta FROM v_consulta
   EXECUTE exe_consulta USING p_proceso_cod INTO v_folio_pendiente, v_usuario
   

   IF (v_folio_pendiente IS NOT NULL AND v_folio_pendiente <> 0) THEN
      #Se encontro un folio por notificar
      
      #Obtiene las rutas ejecutable
      SELECT ruta_bin
      INTO v_ruta_ejecutable
      FROM seg_modulo 
      WHERE modulo_cod = 'not'

      --Obtiene ruta listados
      SELECT ruta_listados
      INTO v_ruta_listados
      FROM seg_modulo 
      WHERE modulo_cod = 'bat'

      LET v_opera_cod_notifica = 1
      LET v_nom_archivo = 'notifica_proceso'

      # se valida si se puede generar el proceso
      CALL fn_valida_operacion(0,p_proceso_cod,v_opera_cod_notifica) RETURNING r_resultado_opera
      IF ( r_resultado_opera <> 0 ) THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      ELSE
         # se genera el pid para el proceso
         INITIALIZE v_pid TO NULL
         CALL fn_genera_pid(p_proceso_cod,v_opera_cod_notifica,v_usuario)
                RETURNING v_pid

         CALL fn_inicializa_proceso(v_pid,p_proceso_cod,v_opera_cod_notifica,0,
                                                "NOTP01",v_nom_archivo,v_usuario)
                                       RETURNING r_resultado_opera
         IF ( r_resultado_opera <> 0 ) THEN
            CALL fn_muestra_inc_operacion(r_resultado_opera)
         ELSE
            # Inicia operación
            CALL fn_actualiza_opera_ini(v_pid,p_proceso_cod,v_opera_cod_notifica,v_folio_pendiente,"NOTP01",
                                  v_nom_archivo,v_usuario) RETURNING r_resultado_opera
            # En el caso de que exista una inconsistencia al iniciar el proceso, se
            # Muestra un mensaje con la descripcion
            IF(r_resultado_opera)THEN
               CALL fn_muestra_inc_operacion(r_resultado_opera)
            ELSE
               LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/NOTP01.42r ",
                                                v_usuario," ",
                                                v_pid," ",
                                                p_proceso_cod," ",
                                                v_opera_cod_notifica," ",
                                                v_folio_pendiente," '",
                                                v_nom_archivo,
                                                "' 1>", v_ruta_listados CLIPPED ,
                                                "/nohup:",v_pid USING "&&&&&",":",
                                                         p_proceso_cod USING "&&&&&",":",
                                                         v_opera_cod_notifica USING "&&&&&" ," 2>&1 &"

               RUN v_comando
            END IF
         END IF
      END IF
   END IF
END FUNCTION

PRIVATE FUNCTION fn_genera_reporte(v_folio)
   DEFINE preview             SMALLINT
   DEFINE vhandler            om.SaxDocumentHandler

   DEFINE v_ruta_exe          LIKE seg_modulo.ruta_bin         -- Ruta del ejecutable
   DEFINE v_ruta_listado      LIKE seg_modulo.ruta_listados    -- Ruta de listados
   DEFINE v_nombre	          STRING
   DEFINE v_folio             DECIMAL(9,0)
   DEFINE v_cont              INTEGER 

   SELECT ruta_bin, ruta_listados
     INTO v_ruta_exe,
          v_ruta_listado
     FROM seg_modulo 
    WHERE modulo_cod = 'not'

   LET preview = FALSE
   INITIALIZE vhandler TO NULL

   LET v_nombre = v_ruta_exe CLIPPED, "/NOTP02.4rp"
   LET vhandler = fn_configuracion(v_nombre, "PDF", preview )

   START REPORT rep_notifica_enviadas TO XML HANDLER vhandler
      FOR v_cont=1 TO g_arr_file_dir.getLength()-1
        OUTPUT TO REPORT rep_notifica_enviadas(v_folio,g_arr_file_dir[v_cont].nombre_archivo)
      END FOR 
    FINISH REPORT rep_notifica_enviadas
END FUNCTION

PRIVATE FUNCTION fn_configuracion(v_reporte, v_formato, v_preview)
   DEFINE v_ruta_listados    LIKE seg_modulo.ruta_listados    -- Ruta de listados
   DEFINE v_listado          STRING 
   
   DEFINE v_reporte          STRING
   DEFINE v_formato          STRING
   DEFINE v_preview          INTEGER
    
   -- /ds/safreviv_lst/not
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'not'
   
   LET v_listado = v_ruta_listados CLIPPED , "/" ,
                     p_usuario_cod CLIPPED , "-", -- usuario
                          "NOTP02" CLIPPED, "-", -- programa
                        p_pid         USING "&&&&&","-", -- PID
                        p_proceso_cod USING "&&&&&", "-", -- codigo del proceso
                        p_opera_cod   USING "&&&&&",".pdf" -- codigo de la operación
   
   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- if  the file loaded OK
      CALL fgl_report_setOutputFileName(v_listado)
      CALL fgl_report_selectDevice(v_formato)
      CALL fgl_report_selectPreview(v_preview)
   ELSE
       DISPLAY "Error: No se pudo encontrar el archivo ", v_reporte
       EXIT PROGRAM
   END IF

   RETURN fgl_report_commitCurrentSettings()
   
END FUNCTION

REPORT rep_notifica_enviadas(v_folio,p_nombre_archivo)
   DEFINE v_folio            DECIMAL(9,0)
   DEFINE v_fecha            DATE 
   DEFINE v_no_lineas_arch   INTEGER 

   DEFINE v_descripcion      VARCHAR(200)
   DEFINE v_f_actualiza      DATE
   DEFINE v_folio_refencia   DECIMAL(9,0)

   DEFINE v_count_uno        INTEGER 
   DEFINE v_count_dos        INTEGER

   DEFINE v_count_tot        INTEGER

   DEFINE p_nombre_archivo    STRING 

   DEFINE v_nombre_archivo    RECORD
        nombre STRING
   END record 
   
   FORMAT 

   FIRST PAGE HEADER
      LET v_fecha = TODAY
      LET v_f_actualiza = TODAY

--------------------------------------------------------------------------------
      # Folio de Referencia y Descripcion de proceso 
      SELECT glo2.folio, cat.proceso_desc
      INTO v_folio_refencia, v_descripcion
      FROM glo_folio glo1
      INNER JOIN glo_folio glo2 on glo2.folio = glo1.folio_referencia
      INNER JOIN cat_proceso cat ON cat.proceso_cod = glo2.proceso_cod
      WHERE glo1.folio = v_folio

--------------------------------------------------------------------------------
      # Registros con Notificaciones

      -- Registros SMS
      {LET v_registros_notif = 'SELECT COUNT(*) ',
                              '  FROM ',  v_nom_tabla  
                              --' WHERE tpo_envio = ?'
      PREPARE prp_registro FROM v_registros_notif
      EXECUTE prp_registro {USING SMS INTO v_count_uno

      -- Registros Correo
      EXECUTE prp_registro {USING CORREO INTO v_count_dos}
--------------------------------------------------------------------------------
      # Total de Archivos de Notificacion 
      
      LET v_count_tot=g_arr_file_dir.getLength()-1
--------------------------------------------------------------------------------
      # Número de lineas que contendran los subarchivos de notificación 
      
      CALL FGL_GETRESOURCE( "not.archivo.lineas" ) RETURNING v_no_lineas_arch
--------------------------------------------------------------------------------

   PRINTX v_no_lineas_arch
   PRINTX v_descripcion
   PRINTX v_f_actualiza
   PRINTX v_folio_refencia
   PRINTX v_count_uno
   PRINTX v_count_dos
   PRINTX v_count_tot

   PRINTX v_folio
   PRINTX p_usuario_cod
   PRINTX v_fecha USING "dd-mm-yyyy"

   ON EVERY ROW
      LET v_nombre_archivo.nombre    = p_nombre_archivo CLIPPED   
   PRINTX   v_nombre_archivo.nombre 
END REPORT 

FUNCTION fn_elimina_extension(p_nombre_archivo)
    DEFINE p_nombre_archivo STRING 
    DEFINE v_nom_arch       varchar(100)
    LET p_nombre_archivo=p_nombre_archivo.subString(1,p_nombre_archivo.getIndexOf(".",1)-1)
    LET v_nom_arch=p_nombre_archivo CLIPPED 
    RETURN v_nom_arch
END FUNCTION 