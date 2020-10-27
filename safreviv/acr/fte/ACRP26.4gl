--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

#########################################################################
#Modulo            =>ACR                                                #
#Programa          =>ACRP26                                             #
#Objetivo          =Programa que realiza la conciliación de la informa- #
#                   ción de deudor vs tmp deudor saldo transferidos     #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>02 FEBRERO 2012                                    #
#Autor modifica    =>Emilio Abarca, EFP.                                #
#Fecha modifica    =>11 Mayo 2018.                                      #
#Objetivo modifica =>Adecuación a reporte PDF y generación archivo de   #
#                    de salida rechazos Operación 09 ACR.               #
#########################################################################

DATABASE safre_viv
GLOBALS "ACRG10.4gl"

   DEFINE p_v_usuario           LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid               LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod         LIKE cat_operacion.opera_cod -- codigo de la operacion
   DEFINE p_d_folio             VARCHAR(10)
   DEFINE p_v_arch_proceso      LIKE cre_ctr_archivo.nom_archivo -- nombre del archivo a integrar

   --variables para el reporte
   DEFINE v_ruta_bin            LIKE seg_modulo.ruta_bin
   DEFINE v_ruta_listados       LIKE seg_modulo.ruta_listados
   DEFINE v_si_operacion        LIKE cre_ctr_archivo.operacion -- operacion del proceso
   DEFINE v_d_id_cre_ctr_arch   LIKE cre_ctr_archivo.id_cre_ctr_archivo -- id del archiovo
   DEFINE v_r_cre_ctr_arch      RECORD LIKE cre_ctr_archivo.* -- registro de cre ctr archivo
   DEFINE v_r_bat_ctr_opera     RECORD LIKE bat_ctr_operacion.* -- registro de bat ctr operación
   DEFINE v_f_movimiento        DATE
   DEFINE v_ruta_rpt            STRING 
   DEFINE v_c_programa_cod      LIKE bat_ctr_operacion.programa_cod -- nombrel del programa
   DEFINE v_aux_porcentaje      DECIMAL(6,2)

   #RECORDS Y ARREGLOS PARA LA INFORMACIÓN DEL REPORTE
   DEFINE r_total_global    RECORD
      t_registros     INTEGER,
      aivs92          DECIMAL(20,2),
      aivs97          DECIMAL(20,2),
      porc_concatena  CHAR(12)
   END RECORD

   DEFINE arr_resumen DYNAMIC ARRAY OF RECORD 
      estado          SMALLINT,
      estado_desc     CHAR(40),
      t_registros     INTEGER,
      aivs92          DECIMAL(20,2),
      aivs97          DECIMAL(20,2),
      porc_concatena  CHAR(12)
   END RECORD

   DEFINE v_s_titulo_correo      STRING -- contiene el titulo del correo
   DEFINE v_s_archivo_correo     STRING -- ruta y nombre del archivo adjunto en el correo
   DEFINE v_s_mens_correo        STRING -- contiene el cuerpo del correo
   DEFINE r_b_valida             SMALLINT
   --variables para el archivo de salida
   DEFINE v_arh_salida           STRING
   DEFINE v_ruta_envio           CHAR(40)
   DEFINE v_s_qryTxt             STRING
   DEFINE v_d_pid_arch           LIKE bat_ctr_operacion.pid
   DEFINE v_reporte_bin          STRING
   DEFINE v_manejador_rpt        OM.SaxDocumentHandler -- Contenedor de Documentos para el reporte
   
# Objetivo: Conciliar la información de Rechazo de Saldos
MAIN
   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario         = ARG_VAL(1)
   LET p_d_pid             = ARG_VAL(2)
   LET p_i_proceso_cod     = ARG_VAL(3)
   LET p_i_opera_cod       = ARG_VAL(4)
   LET p_d_folio           = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".ACRP26.log")

   DISPLAY " "
   DISPLAY "=INICIA ACRP26="
   DISPLAY " CONCILIACIÓN DE SALDOS TRANSFERIDOS ACR"
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso
   DISPLAY " "

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'acr';
    
   -- Se inicializan variables
   LET v_si_operacion = 9 -- Saldos transferidos

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("acr") RETURNING v_ruta_bin, v_ruta_listados

   -- se consulta el folio del archivo
   LET v_s_qryTxt = " SELECT MAX(id_cre_ctr_archivo)\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_proceso = ",g_id_proceso_acr,"\n",
                    "    AND operacion = ",v_si_operacion

   PREPARE prp_folio_archivo FROM v_s_qryTxt
   EXECUTE prp_folio_archivo INTO v_d_id_cre_ctr_arch

   DISPLAY " Identificador del archivo: ",v_d_id_cre_ctr_arch
   DISPLAY " "

   -- se valida el identificador del archivo
   IF v_d_id_cre_ctr_arch IS NULL THEN
      DISPLAY " ERROR: No fue posible obtener el identificador del archivo"
      EXIT PROGRAM
   END IF

   -- se crea la sentencia sql que busca la información del archivo cargado
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",v_d_id_cre_ctr_arch

   PREPARE prp_cre_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_cre_ctr_arch INTO v_r_cre_ctr_arch.*
   
   LET v_d_pid_arch = fn_max_pid(g_proc_cod_acr_sdos_transf, 2)

   DISPLAY " PID del archivo: ",v_d_pid_arch
   DISPLAY " "

   CALL crea_temporal()

   CALL obtiene_info_rpt()
   DISPLAY " > OBTIENE INFORMACIÓN PARA EL REPORTE PDF ...COMPLETADO"
   DISPLAY ""
   CALL genera_archivo_salida()
   DISPLAY " > GENERA ARCHIVO DE SALIDA ...COMPLETADO"
   DISPLAY "  El archivo de salida de ha generado en /safreviv_int/acr/envio"
   DISPLAY "  con nombre: ",v_arh_salida           
   DISPLAY " "
   DISPLAY " > GENERA REPORTE PDF"

   -- actualiza la operacion a finalizada
   CALL fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod) RETURNING r_b_valida

   IF(r_b_valida <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)
      CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) RETURNING r_b_valida
   END IF

   -- se crea la sentencia sql que busca la información de la operación
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE pid = ",v_d_pid_arch,"\n",
                    "    AND proceso_cod = ",g_proc_cod_acr_sdos_transf,"\n",
                    "    AND opera_cod = 2"

   PREPARE prp_bat_ctr_opera FROM v_s_qryTxt
   EXECUTE prp_bat_ctr_opera INTO v_r_bat_ctr_opera.*

   #################################################
   #   CONFIGURACIÓN PARA SALIDA DEL REPORTE PDF   #
   #################################################   

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/ACRP261.4rp"
   LET v_ruta_rpt    = v_ruta_listados CLIPPED,"/",
                       p_v_usuario CLIPPED,"-",v_c_programa_cod CLIPPED,"-",
                       p_d_pid USING "&&&&&","-",
                       p_i_proceso_cod USING "&&&&&","-",
                       p_i_opera_cod USING "&&&&&",".pdf"

   IF (fgl_report_loadCurrentSettings(v_reporte_bin)) THEN 
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_rpt)
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      IF (v_manejador_rpt IS NOT NULL) THEN
 
         START REPORT genera_PDF TO XML HANDLER v_manejador_rpt

            OUTPUT TO REPORT genera_PDF()

         FINISH REPORT genera_PDF 

      END IF
   ELSE 
       DISPLAY " Advertencia: No fue posible abrir plantilla del reporte"
   END IF

   -- se actualiza el folio en el registro correspondiente a las tablas de control del monitor del proceso
   UPDATE bat_ctr_operacion
      SET folio = p_d_folio
    WHERE pid = p_d_pid
      AND proceso_cod = p_i_proceso_cod
      AND opera_cod   = p_i_opera_cod
 
   DISPLAY " ENVIA CORREO DEL REPORTE"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: CONCILIACIÓN DE SALDOS TRANSFERIDOS DE TRANSFERENCIA DE ACREDITADOS"
 
   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = v_ruta_rpt
 
   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",p_d_pid,"\n",
                          "Proceso      : CONCILIACIÓN\n",
                          "Operacion    : SALDOS TRANSFERIDOS ACR\n",
                          "Fecha Inicio : ",TODAY,"\n",
                          "Fecha Fin    : ",TODAY
 
   -- se invoca la función que envía por correo el elemento generado
   CALL fn_correo_proceso(p_d_pid,
                          p_i_proceso_cod,
                          p_i_opera_cod,
                          v_s_archivo_correo,
                          v_s_titulo_correo,
                          v_s_mens_correo)
 
   DISPLAY "=FIN="
       
END MAIN

FUNCTION obtiene_info_rpt()

   -- Record total global
   DEFINE r_op09_sal  RECORD 
      id_cre_acreditado  DECIMAL(9,0),
      id_derechohabiente DECIMAL(9,0),
      nss                CHAR(11),
      estado             SMALLINT     
   END RECORD 
   DEFINE r_saldo_92    RECORD
      subcuenta          SMALLINT,
      fondo_inversion    SMALLINT,
      acciones           DECIMAL(20,2),
      pesos              DECIMAL(20,2)
   END RECORD
   DEFINE r_saldo_97             RECORD
      subcuenta          SMALLINT,
      fondo_inversion    SMALLINT,
      acciones           DECIMAL(20,2),
      pesos              DECIMAL(20,2)
   END RECORD 
   DEFINE v_acciones92  DECIMAL(20,2)
   DEFINE v_pesos92     DECIMAL(20,2)
   DEFINE v_acciones97  DECIMAL(20,2)
   DEFINE v_pesos97     DECIMAL(20,2)
   DEFINE a             INTEGER

   --Obtenemos la fecha de movimiento
   LET v_f_movimiento = TODAY 

   ##################################
   # RECUPERA INFORMACIÓN PROCESADA #
   ##################################
   DECLARE crs_op09 CURSOR FOR 
   SELECT his.id_cre_acreditado,
           acr.id_derechohabiente,
           afi.nss,
           his.estado
     FROM cre_his_acreditado his,
          cre_acreditado acr,
          afi_derechohabiente afi
    WHERE his.id_cre_acreditado  = acr.id_cre_acreditado
      AND his.edo_procesar IN (120,5) --Se toma en cuenta los saldos transferidos y rechazos
      AND acr.id_derechohabiente = afi.id_derechohabiente
      AND his.id_cre_ctr_archivo = v_d_id_cre_ctr_arch

   INITIALIZE r_op09_sal.* TO NULL
   
   FOREACH crs_op09 INTO r_op09_sal.id_cre_acreditado,
                          r_op09_sal.id_derechohabiente,
                          r_op09_sal.nss,
                          r_op09_sal.estado
      
      --Inicializa variables
      LET v_acciones92 = 0 
      LET v_pesos92    = 0
      LET v_acciones97 = 0 
      LET v_pesos97    = 0
      
      --Obtiene aivs 92 
     
      PREPARE prp_saldo_92 FROM "EXECUTE FUNCTION fn_Saldo_actual(?,8,TODAY)"
      DECLARE crs_saldo_92 CURSOR FOR prp_saldo_92

      FOREACH crs_saldo_92 USING r_op09_sal.nss 
                            INTO r_saldo_92.subcuenta,
                                 r_saldo_92.fondo_inversion,
                                 r_saldo_92.acciones,
                                 r_saldo_92.pesos
                                 
         IF(r_saldo_92.fondo_inversion = 11) THEN
            LET v_acciones92 = r_saldo_92.acciones
            LET v_pesos92    = r_saldo_92.pesos
            EXIT FOREACH
         END IF
         
      END FOREACH 
      
      PREPARE prp_saldo_97 FROM "EXECUTE FUNCTION fn_Saldo_actual(?,4,TODAY)"
      DECLARE crs_saldo_97 CURSOR FOR prp_saldo_97

      FOREACH crs_saldo_97 USING r_op09_sal.nss 
                            INTO r_saldo_97.subcuenta,
                                 r_saldo_97.fondo_inversion,
                                 r_saldo_97.acciones,
                                 r_saldo_97.pesos
                                 
         IF(r_saldo_97.fondo_inversion = 11) THEN
            LET v_acciones97 = r_saldo_97.acciones
            LET v_pesos97    = r_saldo_97.pesos
            EXIT FOREACH
         END IF
         
      END FOREACH 

      -- Guarda en temporal
      INSERT INTO safre_tmp:tmp_op09_acr(
                                id_cre_acreditado ,
                                id_derechohabiente,
                                nss               ,
                                aivs92            ,
                                pesos92           ,
                                aivs97            ,
                                pesos97           ,
                                estado)
                       VALUES (r_op09_sal.id_cre_acreditado,
                                r_op09_sal.id_derechohabiente,
                                r_op09_sal.nss,
                                v_acciones92,
                                v_pesos92,
                                v_acciones97,
                                v_pesos97,
                                r_op09_sal.estado);
                                
   END FOREACH 
   
   ##################################
   #   INFORMACIÓN DETALLE GLOBAL   #
   ##################################

   --Inicializa valores
   LET r_total_global.t_registros = 0
   LET r_total_global.aivs92      = 0
   LET r_total_global.aivs97      = 0
   LET r_total_global.porc_concatena = "0.0 %"

   LET arr_resumen[1].estado         = NULL
   LET arr_resumen[1].estado_desc    = NULL 
   LET arr_resumen[1].t_registros    = 0
   LET arr_resumen[1].aivs92         = 0
   LET arr_resumen[1].aivs97         = 0
   LET arr_resumen[1].porc_concatena = "0.0 %"
   
   --Recupera la información de la temporal
   DECLARE crs_sal_tmp CURSOR FOR 
   SELECT cat.estado,
           cat.estado_desc,
           SUM(tmp.aivs92),
           SUM(tmp.aivs97),
           COUNT(*)
   FROM safre_tmp:tmp_op09_acr tmp,
        cat_maq_credito cat
  WHERE tmp.estado = cat.estado
  GROUP BY 1,2
  ORDER BY cat.estado;

   LET a  = 1
   
   FOREACH crs_sal_tmp INTO arr_resumen[a].estado,
                             arr_resumen[a].estado_desc,
                             arr_resumen[a].aivs92,
                             arr_resumen[a].aivs97,
                             arr_resumen[a].t_registros

      LET r_total_global.t_registros = r_total_global.t_registros + arr_resumen[a].t_registros
      LET r_total_global.aivs92      = r_total_global.aivs92 + arr_resumen[a].aivs92
      LET r_total_global.aivs97      = r_total_global.aivs97 + arr_resumen[a].aivs97
   
      LET a = a + 1
      
   END FOREACH 

   --Elimina fila en blanco del arreglo
   IF(arr_resumen[arr_resumen.getLength()].estado IS NULL) THEN
      CALL arr_resumen.deleteElement(arr_resumen.getLength()) 
   END IF 

   --calcula el porcentaje global
   LET v_aux_porcentaje = 0

   --prc total global
   LET v_aux_porcentaje = (r_total_global.t_registros / r_total_global.t_registros) * 100
   LET r_total_global.porc_concatena = v_aux_porcentaje CLIPPED,"%"
   
END FUNCTION 

#OBJETIVO: Genera el reporte de Rechazos
REPORT genera_PDF()

   DEFINE v_f_presenta     DATE
   DEFINE v_desc_operacion CHAR(60)
   DEFINE f                INTEGER
   DEFINE r_rch_integra    RECORD
      nss         CHAR(11),
      estado      SMALLINT,
      estado_desc CHAR(40) 
   END RECORD 

   FORMAT 
   FIRST PAGE HEADER
      LET v_f_presenta = TODAY
      LET v_desc_operacion = "Recepción Saldos Transferidos ACR"  

      #ENCABEZADO
      PRINTX p_v_usuario
      PRINTX p_d_folio
      PRINTX v_f_presenta USING "dd-mm-yyyy"
      PRINTX v_f_movimiento USING "dd-mm-yyyy" 
      
      #RESUMEN
      PRINTX p_v_arch_proceso            --Nombre del archivo
      PRINTX v_r_bat_ctr_opera.fecha_ini --fecha inicio proceso
      PRINTX v_r_bat_ctr_opera.fecha_fin --fecha fin proceso
      PRINTX v_desc_operacion            --Desc. operación
      
      #Totales globales
      PRINTX r_total_global.t_registros
      PRINTX r_total_global.aivs92       
      PRINTX r_total_global.aivs97           
      PRINTX r_total_global.porc_concatena 
   
   ON EVERY ROW
      #Detalle global
      FOR f=1 TO arr_resumen.getLength()
         PRINTX arr_resumen[f].estado_desc  
         PRINTX arr_resumen[f].t_registros       
         PRINTX arr_resumen[f].aivs92        
         PRINTX arr_resumen[f].aivs97   
         #calcula porcentaje
         LET v_aux_porcentaje = 0
         LET v_aux_porcentaje = (arr_resumen[f].t_registros / r_total_global.t_registros) * 100
         LET arr_resumen[f].porc_concatena = v_aux_porcentaje CLIPPED,"%"
         PRINTX arr_resumen[f].porc_concatena
      END FOR 
      
      #Detalle rechazos
      DECLARE crs_rch_integra CURSOR FOR 
      SELECT r.nss,
              r.estado,
              c.desc_estado
        FROM cre_rch_acreditado r,
              cat_rch_acreditado c
       WHERE r.id_cre_ctr_archivo = v_d_id_cre_ctr_arch
         AND r.estado = c.estado 

      FOREACH crs_rch_integra INTO  r_rch_integra.nss,
                                     r_rch_integra.estado,
                                     r_rch_integra.estado_desc
            PRINTX r_rch_integra.nss
            PRINTX r_rch_integra.estado_desc

      END FOREACH 

END REPORT

#OBJETIVO: Genera archivo de salida detalle rechazos.
FUNCTION genera_archivo_salida()

   DEFINE v_qry_salida    STRING
   DEFINE v_detalle       STRING
   DEFINE v_ruta_archivo  STRING 
   DEFINE archivo         base.Channel
   
   DEFINE r_arh_op09      RECORD
      nss         CHAR(11),
      aivs92      DECIMAL(13,2),
      aivs97      DECIMAL(13,2),
      estado      SMALLINT,
      tipo        CHAR(2)
   END RECORD  

   LET v_arh_salida   = "Detalle_TA09_",TODAY USING "yyyymmdd",".cta" CLIPPED 
   LET v_ruta_archivo = v_ruta_envio CLIPPED,"/",v_arh_salida

   #Recupera información de la temporal

   LET v_qry_salida = "SELECT nss,
                              aivs92,
                              aivs97,
                              estado
                         FROM safre_tmp:tmp_op09_acr;"
                            
      
   PREPARE prp_arh_op09 FROM v_qry_salida
   DECLARE crs_arh_op09 CURSOR FOR prp_arh_op09

   LET archivo = base.Channel.create()
   CALL archivo.openFile(v_ruta_archivo,"w")

   LET r_arh_op09.nss         = NULL
   LET r_arh_op09.aivs92      = 0 
   LET r_arh_op09.aivs97      = 0
   LET r_arh_op09.estado      = NULL
   LET r_arh_op09.tipo        = NULL
   LET v_detalle              = NULL
   
   FOREACH crs_arh_op09 INTO r_arh_op09.nss,
                              r_arh_op09.aivs92,
                              r_arh_op09.aivs97,
                              r_arh_op09.estado

      IF(r_arh_op09.aivs92 IS NULL) THEN
         LET r_arh_op09.aivs92 = 0
      END IF 

      IF(r_arh_op09.aivs97 IS NULL) THEN
         LET r_arh_op09.aivs97 = 0
      END IF 
      
      CASE 
         WHEN  r_arh_op09.estado = 20 OR r_arh_op09.estado = 140 
            LET r_arh_op09.tipo  = "DE"
         WHEN  r_arh_op09.estado = 25 OR r_arh_op09.estado = 145
            LET r_arh_op09.tipo  = "SR"
         WHEN  r_arh_op09.estado = 142
            LET r_arh_op09.tipo  = "CE"
      END CASE

      LET v_detalle = v_r_cre_ctr_arch.f_proceso USING "yyyymmdd",
                      r_arh_op09.nss,
                      r_arh_op09.aivs92,
                      r_arh_op09.aivs97,
                      r_arh_op09.tipo

      --Escribe en archivo de salida
      CALL archivo.writeLine(v_detalle)
      
   END FOREACH

   CALL archivo.close()
   
END FUNCTION 

FUNCTION crea_temporal()

   DATABASE safre_tmp
   
   WHENEVER ERROR CONTINUE 
      DROP TABLE tmp_op09_acr
   WHENEVER ERROR STOP
      CREATE TABLE tmp_op09_acr(id_cre_acreditado  DECIMAL(9,0),
                                 id_derechohabiente DECIMAL(9,0),
                                 nss                CHAR(11),
                                 aivs92             DECIMAL(20,2),
                                 pesos92            DECIMAL(20,2),
                                 aivs97             DECIMAL(20,2),
                                 pesos97            DECIMAL(20,2),
                                 estado             SMALLINT);

   DATABASE safre_viv

END FUNCTION 

#Objetivo: Obtiene la descripcion para el estado de rechazo
FUNCTION fn_obt_desc_rch_acreditado(p_si_estado)

   DEFINE p_si_estado          LIKE cat_rch_acreditado.estado
   DEFINE v_c_desc_estado      LIKE cat_rch_acreditado.desc_estado

   -- se obtiene la descripción del rechazo
   SELECT desc_estado
     INTO v_c_desc_estado
     FROM cat_rch_acreditado
    WHERE estado = p_si_estado

   -- se valida la descripción obtenida
   IF v_c_desc_estado IS NULL THEN
      LET v_c_desc_estado = "DESCRIPCIÓN NO CATALOGADA"
   END IF

   RETURN v_c_desc_estado

END FUNCTION
