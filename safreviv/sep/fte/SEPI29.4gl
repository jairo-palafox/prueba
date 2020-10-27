--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18-04-2012
--===============================================================

####################################################################
#Modulo            =>SEPE                                          #
#Programa          =>SEPE11                                        #
#Objetivo          =>Programa de integracion de marca operacion 27 # 
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>11 MAYO    2012                               #
####################################################################
DATABASE safre_viv

FUNCTION fn_genera_reporte_contactos(p_estado, p_ind_contacto, p_contactado,p_v_usuario,p_d_pid,p_i_proceso_cod,p_i_opera_cod)
DEFINE p_estado        SMALLINT ,
       p_ind_contacto  SMALLINT ,
       p_contactado    SMALLINT 

   DEFINE p_v_usuario          LIKE seg_usuario.usuario, -- nombre del usuario
          p_d_pid              LIKE bat_ctr_proceso.pid, -- pid
          p_i_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod        LIKE cat_operacion.opera_cod, -- codigo de la operacion de la etapa
          p_d_folio            LIKE glo_ctr_archivo.folio, -- numero de folio
          r_b_valida           SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   DEFINE p_fec_ejecucion         DATE
   DEFINE v_v_nom_reporte   VARCHAR(80), -- nombre del reporte
          v_manejador_rpt   OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
          r_ruta_bin        LIKE seg_modulo.ruta_bin, -- rutal de bin
          r_ruta_listados   LIKE seg_modulo.ruta_listados -- ruta de listados
   DEFINE p_v_archivo       LIKE glo_ctr_archivo.nombre_archivo

   DEFINE v_datos_contacto RECORD
             v_ind             SMALLINT     ,
             v_diag            CHAR(3)      ,
             v_sql_error       INTEGER      ,
             v_id_expediente   DECIMAL(9,0) ,
             v_caso_adai       DECIMAL(9,0) ,
             v_estado          CHAR(40)     ,
             v_nss             CHAR(11)     ,
             v_tipo_trabajador CHAR(40)     ,
             v_nombre          CHAR(120)    ,
             v_tel1            DECIMAL(10,0),
             v_tel2            DECIMAL(10,0),
             v_cel             DECIMAL(10,0),
             v_correo_e        CHAR(40)     ,
             v_contactado      CHAR(40)     
   	     END RECORD           
   DEFINE v_arr_contacto RECORD
   	         consecutivo      INTEGER                                ,
   	         caso_adai        LIKE sep_expediente.caso_adai          ,
   	         estado           CHAR(40)                               ,
   	         nss              LIKE sep_nss_expediente.nss            ,
   	         tipo_trabajador  CHAR(40)                               ,
   	         nombre           LIKE sep_nss_expediente.nombre         ,
   	         tel_contacto1    STRING,--LIKE sep_nss_expediente.tel_contacto1  ,
   	         tel_contacto2    STRING,--LIKE sep_nss_expediente.tel_contacto2  ,
   	         tel_celular      STRING,--LIKE sep_nss_expediente.tel_celular    ,
   	         correo_e         LIKE sep_nss_expediente.correo_e       
   	     END RECORD

   DEFINE v_day              CHAR(2)
   DEFINE v_mes              CHAR(2)
   DEFINE v_ano              CHAR(4)
   DEFINE v_pos_rep_det      INTEGER
   DEFINE v_tot_det          INTEGER
   DEFINE v_consulta         STRING
   DEFINE v_indice           INTEGER
   DEFINE v_i_tot_reg_sol    INTEGER
   DEFINE v_fecha            CHAR(10)
   
   -- se recuperan los parametros que envia el programa lanzador
   {LET p_v_usuario = ARG_VAL(1)
   LET p_d_pid = ARG_VAL(2)
   LET p_i_proceso_cod = ARG_VAL(3)
   LET p_i_opera_cod = ARG_VAL(4)
   LET p_d_folio = ARG_VAL(5)
   LET p_v_archivo = ARG_VAL(6)} -- archivo procesado
  
   WHENEVER ERROR STOP
   
   -- se crear el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".SEPI29.log")
   
   LET p_fec_ejecucion = DATE
   LET v_v_nom_reporte = p_v_usuario CLIPPED, "-SEPS29-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"

         ---- recupera la ruta de listados en el que se enviara el archivo
         CALL fn_rutas("sep") RETURNING r_ruta_bin, r_ruta_listados
         --DISPLAY "Ruta bin - ", r_ruta_bin
         --DISPLAY "Ruta lst - ", r_ruta_listados

         -- se indica que el reporte usara la plantilla creada
         IF fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED||"/SEPI291.4rp") THEN
            -- se indica la salida del reporte
            CALL fgl_report_selectDevice("PDF") 
            
            CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_v_nom_reporte)
         
            -- sin indica que no es necesario el preview
            CALL fgl_report_selectPreview(0)
         
            -- se asigna la configuración en el menejo del reporte
            LET v_manejador_rpt = fgl_report_commitCurrentSettings()
         ELSE
            DISPLAY "no fue posible generar el reporte"
            CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                               RETURNING r_b_valida
         
            IF(r_b_valida <> 0)THEN
               # En caso de error se muestra un mensaje a usuario y no continua
               CALL fn_desplega_inc_operacion(r_b_valida)
               EXIT PROGRAM
            END IF
         END IF
         
         -- inicia el reporte de registros procesados
         START REPORT rpt_contactos_expediente TO XML HANDLER v_manejador_rpt
         
         LET v_day = DAY(TODAY) USING "&&"
         LET v_mes = MONTH(TODAY) USING "&&"
         LET v_ano = YEAR(TODAY) USING "&&&&"
         LET v_fecha = v_day,"-",v_mes,"-",v_ano
         
         LET v_indice = 1
                            
         LET v_consulta = "EXECUTE FUNCTION sp_sep_consulta_datos_contacto(",p_estado,",", p_ind_contacto,",", p_contactado,")"
         PREPARE prp_rec_contactos FROM v_consulta
         DECLARE cur_rec_contactos CURSOR  FOR prp_rec_contactos
         LET v_i_tot_reg_sol = 0
         FOREACH cur_rec_contactos INTO v_datos_contacto.*
            LET v_i_tot_reg_sol = v_i_tot_reg_sol + 1
            LET v_arr_contacto.consecutivo      = v_i_tot_reg_sol
         	  LET v_arr_contacto.caso_adai        = v_datos_contacto.v_caso_adai      
         	  LET v_arr_contacto.estado           = v_datos_contacto.v_estado         
         	  LET v_arr_contacto.nss              = v_datos_contacto.v_nss            
         	  LET v_arr_contacto.tipo_trabajador  = v_datos_contacto.v_tipo_trabajador
         	  LET v_arr_contacto.nombre           = v_datos_contacto.v_nombre         
         	  LET v_arr_contacto.tel_contacto1    = v_datos_contacto.v_tel1           
         	  LET v_arr_contacto.tel_contacto2    = v_datos_contacto.v_tel2           
         	  LET v_arr_contacto.tel_celular      = v_datos_contacto.v_cel            
         	  LET v_arr_contacto.correo_e         = v_datos_contacto.v_correo_e       
            OUTPUT TO REPORT rpt_contactos_expediente(v_arr_contacto.*, v_fecha)
         END FOREACH 
         FREE cur_rec_contactos
            
         -- finaliza el reporte
         FINISH REPORT rpt_contactos_expediente
         
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPI13                                                   #
#Objetivo          => Reporte de diagnostico operacion 27                      #
#Autor             => Alexandro Hollmann, EFP                                  #
#Fecha Inicio      => 01/06/2012                                               #
################################################################################
REPORT rpt_contactos_expediente(v_contactos, v_fecha)
DEFINE v_contactos RECORD
	         consecutivo      INTEGER                                ,
	         caso_adai        LIKE sep_expediente.caso_adai          ,
	         estado           CHAR(40)                               ,
	         nss              LIKE sep_nss_expediente.nss            ,
	         tipo_trabajador  CHAR(40)                               ,
	         nombre           LIKE sep_nss_expediente.nombre         ,
	         tel_contacto1    STRING,--LIKE sep_nss_expediente.tel_contacto1  ,
	         tel_contacto2    STRING,--LIKE sep_nss_expediente.tel_contacto2  ,
	         tel_celular      STRING,--LIKE sep_nss_expediente.tel_celular    ,
	         correo_e         LIKE sep_nss_expediente.correo_e       
	     END RECORD,
       v_totales         INTEGER,
       v_fecha           CHAR(10)

   FORMAT

      FIRST PAGE HEADER
         
         PRINTX v_fecha
         LET v_totales  = 0

      ON LAST ROW
         
         PRINTX v_totales

      ON EVERY ROW
         
            PRINTX v_contactos.*
            
            LET v_totales  = v_totales + 1
         
END REPORT
