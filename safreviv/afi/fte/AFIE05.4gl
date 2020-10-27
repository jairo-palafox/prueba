####################################################################################################
# Modulo       => AFI                                                                              #
# Programa     => AFIE05                                                                           #      
# Objetivo     => Programa que realiza la lectura del archivo de Carga Afores                      # 
# Autor        =>                                                                                  #
# Fecha        => 24/ABRIL/2015                                                                    #
####################################################################################################
DATABASE safre_viv

   DEFINE g_usuario              CHAR (20)
   DEFINE g_pid                  DECIMAL (9,0)  --ID del proceso
   DEFINE g_proceso_cod          SMALLINT       --código del proceso
   DEFINE g_opera_cod            SMALLINT       --código de operacion
   DEFINE v_nom_archivo          STRING 
   DEFINE v_ruta_rescate         CHAR(40)
   DEFINE buf                    base.StringBuffer

MAIN
   DEFINE v_comando              STRING
   DEFINE v_estado_sql           SMALLINT
   DEFINE v_ruta_envio           CHAR(40)
   DEFINE v_pos                  SMALLINT
   DEFINE cant                   SMALLINT
   DEFINE v_nom_salida           STRING
   DEFINE v_ruta_listados        VARCHAR (40)
   DEFINE v_ruta_reporte         STRING
   DEFINE v_report_handler       om.SaxDocumentHandler -- handler para el reporte en PDF
   DEFINE v_registros_totales    INTEGER 
   DEFINE v_regs_rechazados      INTEGER
   DEFINE v_regs_aceptados       INTEGER
   DEFINE nom_archivo            CHAR (40)
   DEFINE v_contador             INTEGER
   DEFINE v_ruta_nom             STRING
   DEFINE v_cmd_dbload           STRING
   DEFINE v_arch_dbl             base.Channel
   DEFINE v_arch_format          STRING
   DEFINE v_nom_arch_sin_ext     base.StringBuffer
   DEFINE v_status               SMALLINT
   DEFINE v_hora_inicio          STRING
   DEFINE v_hora_fin             STRING

   LET g_usuario        = ARG_VAL (1)
   LET g_pid            = ARG_VAL (2)
   LET g_proceso_cod    = ARG_VAL (3)
   LET g_opera_cod      = ARG_VAL (4)
   LET v_nom_archivo    = ARG_VAL (5)

   -- Se obtiene el nombre del archivo sin extensión 
   LET v_nom_arch_sin_ext = base.StringBuffer.create()
   CALL v_nom_arch_sin_ext.append(v_nom_archivo)--bu.append(v_nom_archivo)
   CALL v_nom_arch_sin_ext.replace(".cvaf","",0)

   CALL STARTLOG(g_usuario CLIPPED||".AFIE05.log")

   LET v_comando = "SELECT ruta_rescate, ruta_envio
                      FROM seg_modulo
                     WHERE modulo_cod = 'afi' "

   PREPARE prp_modulo FROM v_comando
   EXECUTE prp_modulo INTO v_ruta_rescate,
                           v_ruta_envio

   IF (v_nom_archivo IS NULL) THEN
      DISPLAY "El archivo no existe"
   ELSE
      LET nom_archivo = v_nom_archivo CLIPPED

       #se ejecuta el proceso que crea la tabla temporal
      DATABASE safre_tmp  -- se cambia de bB por que se borran las tablas
      LET v_comando = " EXECUTE PROCEDURE sp_afore_tmp()"
      PREPARE prp_afore_tmp FROM v_comando
      EXECUTE prp_afore_tmp

      LET v_comando = "EXECUTE PROCEDURE sp_crea_temporal_afore()"
      PREPARE prp_crea_tmp_afore FROM v_comando
      EXECUTE prp_crea_tmp_afore

      LET v_ruta_nom = v_ruta_rescate CLIPPED ||"/"||v_nom_archivo
      LET v_arch_format = v_ruta_rescate CLIPPED || "/comands.dbl"

      LET v_arch_dbl = base.Channel.create()
      CALL v_arch_dbl.openFile(v_arch_format,"w")
      CALL v_arch_dbl.writeLine("FILE "|| v_ruta_nom CLIPPED || " DELIMITER '|' 3;")
      CALL v_arch_dbl.writeLine("INSERT INTO tmp_dh_afore;")

      --Se  arma el comando para dbload con un archivo de logs para errores, con commits de cada 100 000 registros
      --y se le da una salida al comando hacia un archivo log para posteriormente leer la utima linea
      LET v_cmd_dbload = "dbload -d safre_tmp -c " || v_arch_format || " -l " 
                       || v_ruta_rescate CLIPPED || "/" || v_nom_arch_sin_ext.toString() CLIPPED ||".err -n 100000" 
                       || " > " || v_ruta_rescate CLIPPED || "/" ||v_nom_arch_sin_ext.toString() CLIPPED || ".log"

      --DISPLAY "Comando DBLOAD    : ",v_cmd_dbload
      --DISPLAY "comands           : ",v_arch_format
      --DISPLAY "Nombre log        : ",v_nom_arch_sin_ext.toString() CLIPPED ||".log"
      --DISPLAY "Nombre arch error : ",v_nom_arch_sin_ext.toString() CLIPPED ||".err"
      --LET v_hora_inicio = CURRENT HOUR TO SECOND
      --DISPLAY " Hora inicio      : ",v_hora_inicio
      DISPLAY "\n*****************************************************************"
      DISPLAY "   Inicia carga de archivo                                       \n"

      RUN v_cmd_dbload

      LET v_comando = "tail -n 1 " ||v_ruta_rescate CLIPPED ||"/"|| v_nom_arch_sin_ext.toString() CLIPPED ||".log"
      --DISPLAY "Comando tail : " , v_comando
      RUN v_comando

      DISPLAY "\n   Finaliza carga de archivo                                    "
      DISPLAY "*****************************************************************\n"

      -- Se regresa a safe_viv
      DATABASE safre_viv

      --Se ejecuta el proceso que crea la tabla física
      LET v_comando = " EXECUTE PROCEDURE sp_afore_fisica()"

      PREPARE prp_afore_fisica FROM v_comando
      EXECUTE prp_afore_fisica

      LET v_comando = "SELECT COUNT(*)
                         FROM safre_tmp:tmp_dh_afore"

      PREPARE prp_cnt_tot FROM v_comando
      EXECUTE prp_cnt_tot INTO v_contador

      DISPLAY "Registros insertados  : ",v_contador
      DISPLAY "Archivo cargado       : ",v_nom_archivo
      DISPLAY "Ruta archivo a cargar : ",v_ruta_rescate CLIPPED ||"/"||v_nom_archivo

      DISPLAY "\n*****************************************************************"
      DISPLAY "   Inicia carga de tablas temporales                             \n"


      #se carga la tabla de tmp_afi con la informacion de larchivo     
      LET v_comando = "EXECUTE PROCEDURE sp_sube_archivo_afore()"
      PREPARE prp_sube_arch_afore FROM v_comando
      EXECUTE prp_sube_arch_afore

      LET v_comando = " EXECUTE PROCEDURE sp_carga_afore(?)"
      PREPARE prp_sp_carga_afore FROM v_comando
      EXECUTE prp_sp_carga_afore USING g_usuario

      LET v_estado_sql = SQLCA.SQLCODE

      DATABASE safre_tmp


      DISPLAY "   Finaliza carga de tablas temporales                           "
      DISPLAY "*****************************************************************\n"

      --LET v_hora_fin = CURRENT HOUR TO SECOND
      --DISPLAY " Hora fin      : ",v_hora_fin
      LET buf = base.StringBuffer.create()

      CALL buf.append(v_nom_archivo)

      LET cant         = LENGTH(v_nom_archivo)
      LET v_pos        = buf.getIndexof(".cvaf",1)
      LET v_nom_salida  = buf.subString(1,v_pos)
      LET v_nom_salida = v_nom_salida||"rcvaf"

      DISPLAY "Ruta del archivo de salida: ", v_ruta_envio CLIPPED ||"/"||v_nom_salida   --display para el monitor de procesos

      -- Se realiza el update de la descripción para la afore 531
      LET v_comando = "UPDATE safre_tmp:tmp_dh_afore
                         SET afore_desc = 'SIN AFORE REGISTRADA'
                        WHERE afore = 531"
      PREPARE prp_act_temporal FROM v_comando
      EXECUTE prp_act_temporal

      UNLOAD TO v_ruta_envio CLIPPED ||"/"||v_nom_salida
      SELECT n.nss,
             a.afore_desc,
             DECODE(cod_error,11,"NSS NO EXISTE",17,"NSS DUPLICADO")
        FROM safre_tmp:tmp_afi_afore_nss n,
             safre_tmp:tmp_dh_afore     a
       WHERE n.nss = a.nss

      IF SQLCA.SQLCODE < 0 THEN 
         DISPLAY "OCURRIÓ UN ERROR GENERAR EL ARCHIVO DE SALIDA"
      END IF 

      DATABASE safre_viv

      -- =====================================================================================
      --            REPORTE DE CIFRAS DE CONTROL DEL ARCHIVO DE CARGA DE AFORES
      -- =====================================================================================

      -- se obtiene el modulo del proceso
      SELECT ruta_listados
        INTO v_ruta_listados
        FROM seg_modulo
       WHERE modulo_cod = "afi"

      LET v_ruta_reporte = v_ruta_listados CLIPPED, "/",
                           g_usuario       CLIPPED, "-",        -- usuario
                           "AFIE05-"                   ,        -- programa
                           g_pid           USING "&&&&&","-",   -- PID
                           g_proceso_cod   USING "&&&&&", "-",  -- codigo del proceso
                           g_opera_cod     USING "&&&&&",".pdf" -- codigo de la operación

      DISPLAY "Ruta del reporte: ", v_ruta_reporte

      -- se indica que el reporte usara la plantilla creada
      IF ( fgl_report_loadCurrentSettings("../../afi/bin/AFIE05.4rp") ) THEN  -- if  the file loaded OK
         -- sin preview
         CALL fgl_report_selectPreview(0)
         -- se indica que se escriba en archivo
         CALL fgl_report_setOutputFileName(v_ruta_reporte)       

         LET v_report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings

         -- se obtiene el total de registros
         SELECT COUNT(*)
         INTO   v_registros_totales
         FROM   safre_tmp:tmp_dh_afore

         SELECT COUNT(*)
         INTO   v_regs_aceptados
         FROM   afi_afore

         SELECT COUNT(*)
         INTO   v_regs_rechazados
         FROM   safre_tmp:tmp_afi_afore_nss

         DISPLAY "\nTotal      : ",v_registros_totales
         DISPLAY "Aceptados  : ",v_regs_aceptados
         DISPLAY "Rechazados : ",v_regs_rechazados

         -- se inicia el reporte
         START REPORT rpt_cifras_control TO XML HANDLER v_report_handler

         -- se envian los datos al reporte
         OUTPUT TO REPORT rpt_cifras_control(v_nom_archivo        ,
                                             g_usuario            ,
                                             v_regs_aceptados     ,
                                             v_registros_totales  ,
                                             v_regs_rechazados
                                             )

         -- se finaliza
         FINISH REPORT rpt_cifras_control
      ELSE
         DISPLAY "No se puede leer la plantilla del reporte AFIE03.4rp"
      END IF

      IF v_estado_sql < 0 THEN 
         DISPLAY "Ocurrió un error "
         EXIT PROGRAM 
      END IF 
   END IF

   LET nom_archivo = v_nom_archivo CLIPPED 

   INSERT INTO glo_ctr_archivo
   VALUES (g_proceso_cod, 1, nom_archivo, 0, 2, TODAY, g_usuario)

   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_status

END MAIN 

REPORT rpt_cifras_control(v_nom_archivo     ,
                          p_usuario_cod     ,
                          p_regs_aceptados  ,
                          p_regs_totales    ,
                          p_regs_rechazados )

   DEFINE p_usuario_cod          CHAR(20) -- Clave de usuario
   DEFINE v_nom_archivo          STRING 
   DEFINE v_fecha_texto          VARCHAR(10)
   DEFINE  p_regs_aceptados      INTEGER -- numero de movimientos aceptados
   DEFINE  p_regs_totales        INTEGER -- numero de movimientos rechazados
   DEFINE  p_regs_rechazados     INTEGER

   FORMAT

      FIRST PAGE HEADER

         LET v_fecha_texto = TODAY USING "dd-mm-yyyy"

         -- se despliegan los datos del encabezado
         PRINTX v_nom_archivo, p_usuario_cod, v_fecha_texto
         PRINTX p_regs_aceptados,
                p_regs_totales,
                p_regs_rechazados

END REPORT
