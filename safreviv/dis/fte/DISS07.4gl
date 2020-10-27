################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 11/10/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISS07                                                   #
#Objetivo          => Programa que genera el reporte de inconsistentes         #
#                     al terminar la liquidación                               #
#Fecha inicio      => 11/10/2012                                               #
################################################################################

DATABASE
     safre_viv

GLOBALS 

   DEFINE
      v_folio           DECIMAL (9,0),
      v_folio_pag       DECIMAL (9,0),
      manejador_rpt     om.SaxDocumentHandler  -- Contenedor documentos reporte

   DEFINE
      p_proceso_cod        LIKE cat_proceso.proceso_cod,
      p_opera_cod          LIKE cat_operacion.opera_cod,
      p_usuario            LIKE seg_usuario.usuario_cod,
      p_pid                DECIMAL (9,0)
      
   DEFINE 
      arr_inconsistentes   DYNAMIC ARRAY OF RECORD
         v_id_derechohabiente DECIMAL (10,0),
         v_tipo_inco          SMALLINT,
         v_nombre             VARCHAR (60),
         v_nss                CHAR (11),
         v_num_cred           DECIMAL (10,0),
         v_destino            CHAR (1),
         v_tipo_orig          SMALLINT,
         v_periodo_pago       CHAR (6),
         v_imp_ap_pat         DECIMAL (12,2),
         v_aiv_ap_pat         DECIMAL (18,6),
         v_imp_am_cre         DECIMAL (12,2)
         
      END RECORD 

   DEFINE 
      v_nom_archivo        VARCHAR(40), -- nombre del archivo de salida
      v_ddmmaaaa           VARCHAR(08), -- fecha del archivo de salida
      v_cont_dia           SMALLINT, -- consecutivo por dia de archivo generado
      v_reg_dia            CHAR(03), -- Parametro consecutivo de registro por dia
      v_busca_nom_archivo  STRING, -- busca nombre de archivo
      v_ruta_nomarch       VARCHAR(100), -- ruta y nombre del archivo de salida
      v_ch_arch_salida     BASE.CHANNEL
         
END GLOBALS

MAIN

   --Asignación de parametros generales
   LET p_usuario        = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3) 
   LET p_opera_cod      = ARG_VAL(4)  
   LET v_folio          = ARG_VAL(5) -- Valor de folio

   --DISPLAY "Usuario: ",p_usuario
   --DISPLAY "Pid: ",p_pid
   --DISPLAY "Proceso: ",p_proceso_cod
   --DISPLAY "Operación: ",p_opera_cod
   --DISPLAY "Folio: ",v_folio

   CALL fn_genera_reporte_inconsistentes()
   
END MAIN

# Genera el reporte de inconsistentes al terminar la preliquidación
FUNCTION fn_genera_reporte_inconsistentes() 

   DEFINE 
      v_total_info      DECIMAL (10,0),
      v_ruta_listados   STRING, -- ruta de los listados
      v_ruta_ejecutable STRING, -- ruta del ejecutable
      v_ruta_reporte    STRING, -- ruta del archivo del reporte
      v_origen_datos    STRING,
      v_ind_info        INTEGER, --índice de inconsistentes
      v_query_ex        STRING 

   LET v_ind_info = 1

   LET v_query_ex = "SELECT count (*) FROM dis_info_inconsistente where folio_liquida = ",v_folio
   
                     
   PREPARE prp_valida_registro FROM v_query_ex
   EXECUTE prp_valida_registro INTO v_total_info
   

   IF v_total_info = 0 OR v_total_info IS NULL THEN 
      DISPLAY "No se pudo generar el reporte de inconsistencias debido a falta de información."
      EXIT PROGRAM
   END IF

   IF p_usuario IS NULL THEN

      LET p_usuario = "SAFREVIV"

   END IF 

   SELECT folio_referencia
   INTO   v_folio_pag
   FROM   glo_folio
   WHERE  folio = v_folio
   
   LET v_origen_datos = p_usuario CLIPPED 

   --Obtiene rutas para el reporte
   #CALL fn_rutas("dis") RETURNING v_ruta_ejecutable, v_ruta_listados
   
   {LET v_ruta_reporte = v_ruta_listados.trim(),
                      "/",
                      v_origen_datos.trim(),"-",
                      "DISS07","-",
                      p_pid USING "&&&&&","-",
                      p_proceso_cod USING "&&&&&","-",
                      p_opera_cod USING "&&&&&",".pdf"}
   
   
   --Se asigna la plantilla para generar el reporte
   #IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/DISS071.4rp") THEN

          --CALL fgl_report_selectDevice ("PDF")
          --CALL fgl_report_selectPreview(0)
          --CALL fgl_report_setOutputFileName(v_ruta_reporte)
       
          --LET manejador_rpt = fgl_report_commitCurrentSettings()

          ## Genera reporte de inconsisencias ##

          WHENEVER ERROR CONTINUE 
            DROP TABLE tmp_dis_inconsistentes;
            
            SELECT II.id_derechohabiente,
                   II.tpo_inconsistente,
                   rtrim(AF.nombre_af) ||" "|| rtrim(AF.ap_paterno_af) ||" "|| rtrim(AF.ap_materno_af) as nombre_completo,
                   AF.nss, 
                   II.num_crd_ifv, 
                   II.destino_ap_viv, 
                   II.tpo_originacion, 
                   CH.periodo_pago,
                   II.imp_ap_pat,
                   II.aiv_ap_pat,
                   II.imp_am_cre
            FROM   dis_info_inconsistente II, 
                   afi_derechohabiente AF, 
                   cta_his_pagos CH
            WHERE  II.folio_liquida      = v_folio
            AND    CH.folio              = v_folio_pag
            AND    II.id_derechohabiente = AF.id_derechohabiente
            AND    II.id_derechohabiente = CH.id_derechohabiente
            AND    II.id_referencia      = CH.id_referencia
            --GROUP BY 2,3,4,5,6,7,8,9,1
            ORDER BY 2,8 DESC, 4
            INTO TEMP tmp_dis_inconsistentes

            UPDATE STATISTICS FOR TABLE tmp_dis_inconsistentes

            DECLARE cur_consulta_inconsistentes CURSOR FOR
               SELECT *
               FROM   tmp_dis_inconsistentes

            FOREACH cur_consulta_inconsistentes INTO  arr_inconsistentes[v_ind_info].*    

               LET v_ind_info = v_ind_info + 1
            END FOREACH 

            --Borra el registro nulo
            CALL arr_inconsistentes.deleteElement(v_ind_info)

            {--Inicia el reporte de registros con rechazo
            START REPORT rpt_genera_inconsistentes TO XML HANDLER manejador_rpt
               FOR v_ind_info = 1 TO  arr_inconsistentes.getLength()
                  
                  OUTPUT TO REPORT rpt_genera_inconsistentes(arr_inconsistentes[v_ind_info].*)
                   
               END FOR    
            FINISH REPORT rpt_genera_inconsistentes}

            ### Se cambia reporte por archivo en texto plano separado por pipes
            
            CALL fn_genera_archivo_inconsistencias()
            

   #ELSE
            #DISPLAY "Error al generar el archivo de inconsistencias. Verifique la ruta asignada."

   #END IF
   
END FUNCTION   

#Genera el archivo de salida con los registros que presentaron inconsistencias al
#momento de llevar a cabo la dispersión
FUNCTION fn_genera_archivo_inconsistencias()

   DEFINE   
      r_ruta_rescate     CHAR(40),
      v_total_inc        INTEGER,
      v_comando_dos      STRING,
      v_dest_aportacion  VARCHAR (17),
      v_tipo_credito     VARCHAR (22),
      v_inconsistencia   VARCHAR (52),
      v_num_cred_formato CHAR (10)

   DEFINE
      v_estado                 INTEGER, 
      r_bnd_proceso            INTEGER, 
      v_status_err             INTEGER, 
      v_desc_err               CHAR(50),
      v_destino_dis            INTEGER

   LET v_total_inc = 1

   LET v_estado      = 101
   LET r_bnd_proceso = 0
   LET v_destino_dis = 13

   --Obtiene ruta de salida - Se colocará en la ruta de Rescate
   SELECT ruta_envio
   INTO  r_ruta_rescate
   FROM  seg_modulo 
   WHERE modulo_cod = "dis"

   --Se obtiene nombre del archivo - info_inconsistencias_ddmmaaaa.dis
   #se crea el nombre del archivo y posteriormente se concatena con la ruta
   LET v_nom_archivo = "/info_inconsistencias_"
   LET v_ddmmaaaa = TODAY USING "ddmmyyyy"
   LET v_busca_nom_archivo = "info_inconsistencias_" || v_ddmmaaaa
   
   --Obtine consecutivo para archivo por día
   CALL fn_crea_nombre_archivo(r_ruta_rescate,v_busca_nom_archivo)
        RETURNING v_cont_dia
   LET v_reg_dia = v_cont_dia USING "&&&"

   LET v_nom_archivo = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||".dis"

   LET v_ruta_nomarch = r_ruta_rescate CLIPPED || v_nom_archivo

   WHENEVER ERROR CONTINUE

   PREPARE prp_fn_transaccion24
   FROM    "EXECUTE FUNCTION sp_dis_transaccion24(?,?,?,?,?,?,?)"
   EXECUTE prp_fn_transaccion24 INTO r_bnd_proceso, v_status_err, v_desc_err                                    
                               USING p_proceso_cod,
                                     p_opera_cod, 
                                     v_nom_archivo, 
                                     v_folio,
                                     v_estado, 
                                     v_destino_dis,
                                     p_usuario

   WHENEVER ERROR STOP 

   DISPLAY "Función Transaccion 24 ",r_bnd_proceso
   DISPLAY "Código:",v_status_err         
   DISPLAY "mensaje", v_desc_err
         
   IF r_bnd_proceso <> 0 THEN
     DISPLAY "Error en la transacción 24 ",v_status_err," ",v_desc_err
     EXIT PROGRAM
   END IF
   
   -- se crea el manejador de archivo
   LET v_ch_arch_salida = base.Channel.create()
   
   -- se crea archivo y se indica que se escribirá en el mismo
   CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
   CALL v_ch_arch_salida.setDelimiter("|")
   

   --Escribe encabezado
   CALL v_ch_arch_salida.writeLine(" Fecha  |Folio")
   CALL v_ch_arch_salida.writeLine(TODAY USING "ddmmyyyy"||"|"||v_folio||"|"||" Inconsistencias Dispersión ")

   --Escribe nombre de las columnas
   CALL v_ch_arch_salida.writeLine("NSS|Nombre Completo|Num Crédito|Dest Aportación|Tipo Crédito|Periodo Pago|Aportación|AIVS|Amortización|Inconsistencia")
   
   --Escribe detalle
   FOR v_total_inc = 1 TO  arr_inconsistentes.getLength()

      --Verifica el tipo de destino
      IF arr_inconsistentes[v_total_inc].v_destino = 0 THEN 
         LET v_dest_aportacion = "0-No Identificado"
      END IF 

      IF arr_inconsistentes[v_total_inc].v_destino = 1 THEN 
         LET v_dest_aportacion = "1-Infonavit "
      END IF

      IF arr_inconsistentes[v_total_inc].v_destino = 2 THEN 
         LET v_dest_aportacion = "2-Afore"
      END IF

      --Verifica el Tipo Originación
      IF arr_inconsistentes[v_total_inc].v_tipo_orig = 0 OR 
         arr_inconsistentes[v_total_inc].v_tipo_orig IS NULL THEN 
         LET v_tipo_credito = "0-Sin Crédito"
      END IF

      IF arr_inconsistentes[v_total_inc].v_tipo_orig = 1 THEN 
         LET v_tipo_credito = "1-Crédito Tradicional"
      END IF

      IF arr_inconsistentes[v_total_inc].v_tipo_orig = 2 THEN 
         LET v_tipo_credito = "2-43 Bis"
      END IF

      --Verifica el tipo de inconsistencia
      IF arr_inconsistentes[v_total_inc].v_tipo_inco = 0 THEN 
         LET v_inconsistencia = "0-Sin Número de Crédito"
      END IF

      IF arr_inconsistentes[v_total_inc].v_tipo_inco = 1 THEN 
         LET v_inconsistencia = "1-Destino aportación Infonavit a Crédito 43 – Bis"
      END IF

      IF arr_inconsistentes[v_total_inc].v_tipo_inco = 2 THEN 
         LET v_inconsistencia = "2-Aclaratorio sin Destino"
      END IF

      IF arr_inconsistentes[v_total_inc].v_tipo_inco = 3 THEN 
         LET v_inconsistencia = "3-Aclaratorio Afore sin Marca Confirmada"
      END IF


      IF arr_inconsistentes[v_total_inc].v_nombre IS NULL THEN 
         LET arr_inconsistentes[v_total_inc].v_nombre = "Sin Nombre"
      END IF 
      
      LET v_num_cred_formato = arr_inconsistentes[v_total_inc].v_num_cred USING "&&&&&&&&&&"
      
      CALL v_ch_arch_salida.writeLine(arr_inconsistentes[v_total_inc].v_nss||"|"||
                                       arr_inconsistentes[v_total_inc].v_nombre||"|"||
                                       v_num_cred_formato||"|"||
                                       v_dest_aportacion||"|"||
                                       v_tipo_credito||"|"||
                                       arr_inconsistentes[v_total_inc].v_periodo_pago||"|"||
                                       arr_inconsistentes[v_total_inc].v_imp_ap_pat||"|"||
                                       arr_inconsistentes[v_total_inc].v_aiv_ap_pat||"|"||
                                       arr_inconsistentes[v_total_inc].v_imp_am_cre||"|"||
                                       v_inconsistencia
                                       )
       
   END FOR
   
   --Cierra el archivo
   CALL v_ch_arch_salida.close()
   

   --Cambia el formato del archivo a DOS
   LET v_comando_dos = "unix2dos ",r_ruta_rescate CLIPPED, " ", v_nom_archivo CLIPPED
   RUN v_comando_dos
   
   DISPLAY "Se ha generado el reporte de Inconsistencias: ",v_nom_archivo CLIPPED 
   
END FUNCTION 

#Objetivo: genera el número consecutivo por día para el archivo de salida
FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis,p_busca_nom_archivo)
DEFINE p_ruta_envio_dis     LIKE seg_modulo.ruta_envio,
       p_busca_nom_archivo  VARCHAR(40),
       v_cmd                STRING,
       v_consecutivo        INTEGER
DEFINE fn CHAR(22)
DEFINE ch base.Channel

    LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

    LET ch = base.Channel.create()
    CALL ch.setDelimiter(".")
    CALL ch.openPipe(v_cmd,"r")
    WHILE ch.read([fn])
       LET v_consecutivo = fn[17,19]
    END WHILE
    CALL ch.close()
    --DISPLAY "v_consecutivo: ",v_consecutivo
    
    LET v_consecutivo = v_consecutivo + 1


    --DISPLAY "v_consecutivo: ",v_consecutivo
    IF length(v_consecutivo) = 0 THEN
       LET v_consecutivo = 1
    END IF

    
    RETURN v_consecutivo

END FUNCTION


{
#Define la estructura del reporte 
REPORT rpt_genera_inconsistentes(p_arr_inconsistentes)

      DEFINE 
      p_arr_inconsistentes  RECORD 
         v_nombre       VARCHAR (60),
         v_nss          CHAR (11),
         v_num_cred     DECIMAL (10,0),
         v_destino      CHAR (1),
         v_tipo_orig    SMALLINT,
         v_imp_ap_pat   DECIMAL (12,2),
         v_imp_am_cre   DECIMAL (12,2),
         v_tipo_inco    SMALLINT
      END RECORD 

   DEFINE 
      v_fecha_reporte DATE

   FORMAT

   FIRST PAGE HEADER

      LET v_fecha_reporte = TODAY 
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX p_usuario

   ON EVERY ROW 
      PRINTX p_arr_inconsistentes.*

END REPORT }
