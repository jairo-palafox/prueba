--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 27-06-2012
--===============================================================

####################################################################
#Modulo            =>SEPI                                          #
#Programa          =>SEPI23                                        #
#Objetivo          => # 
#Autor             =>                       #
#Fecha inicio      => Junio 28    2012                               #
####################################################################
DATABASE safre_viv

FUNCTION fn_genera_reporte_expedientes(p_estado, p_flujo_cod,p_v_usuario,p_d_pid,p_i_proceso_cod,p_i_opera_cod)
DEFINE p_estado        SMALLINT,
       p_ind_contacto  SMALLINT,
       p_contactado    SMALLINT,
       v_indice  SMALLINT 

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

   DEFINE  v_expediente DYNAMIC ARRAY OF RECORD
            v_id_num          INTEGER,
            v_expediente      STRING,-- LIKE sep_expediente.id_expediente,
            v_invadido        LIKE sep_nss_expediente.nss,
            v_asociado        LIKE sep_nss_expediente.nss,
            v_tipo_flujo      LIKE sep_cat_tipo_flujo.flujo_desc,
            v_canal           LIKE sep_cat_canal_recepcion_exp.canal_desc,
            v_fecha_recepcion LIKE sep_expediente.f_recepcion_infonavit,
            v_fecha_captura   LIKE sep_expediente.f_captura,
            --v_estado          LIKE sep_expediente.estado
            v_estado          LIKE sep_estado_expediente.descripcion
   END RECORD

   DEFINE v_exp_recuperados RECORD 
           v_ind           SMALLINT     ,   -- v_ind          
           v_diag          CHAR(3)      ,   -- vi_diag        
           v_sql_error     INTEGER      ,   -- v_sql_error    
           v_id_expediente VARCHAR(9),--DECIMAL(9,0) ,   -- id_expediente  
           v_invadido      CHAR(011)    ,   -- invadido       
           v_asociado      CHAR(011)    ,   -- asociado       
           v_flujo         CHAR(40)     ,   -- flujo          
           v_canal         CHAR(40)     ,   -- canal          
           v_f_recepcion   DATE         ,   -- fecha recepcion
           v_f_captura     DATE         ,   -- fecha captura  
           v_estado        CHAR(40)                           
        END RECORD           
   {DEFINE v_arr_contacto RECORD
   	         consecutivo      INTEGER                                ,
   	         caso_adai        LIKE sep_expediente.caso_adai          ,
   	         estado           CHAR(40)                               ,
   	         nss              LIKE sep_nss_expediente.nss            ,
   	         tipo_trabajador  CHAR(40)                               ,
   	         nombre           LIKE sep_nss_expediente.nombre         ,
   	         tel_contacto1    LIKE sep_nss_expediente.tel_contacto1  ,
   	         tel_contacto2    LIKE sep_nss_expediente.tel_contacto2  ,
   	         tel_celular      LIKE sep_nss_expediente.tel_celular    ,
   	         correo_e         LIKE sep_nss_expediente.correo_e       
   	     END RECORD}

   DEFINE v_day              CHAR(2)
   DEFINE v_mes              CHAR(2)
   DEFINE v_ano              CHAR(4)
   DEFINE v_pos_rep_det      INTEGER
   DEFINE v_tot_det          INTEGER
   DEFINE v_indiceonsulta         STRING
   DEFINE v_consulta STRING
   DEFINE v_i_tot_reg_sol    INTEGER
   DEFINE v_fecha            CHAR(10)
   DEFINE v_estado SMALLINT
   DEFINE p_flujo_cod SMALLINT
   
   -- se recuperan los parametros que envia el programa lanzador
   {LET p_v_usuario = ARG_VAL(1)
   LET p_d_pid = ARG_VAL(2)
   LET p_i_proceso_cod = ARG_VAL(3)
   LET p_i_opera_cod = ARG_VAL(4)
   LET p_d_folio = ARG_VAL(5)
   LET p_v_archivo = ARG_VAL(6)} -- archivo procesado
  
   WHENEVER ERROR STOP
   
   -- se crear el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".SEPI23.log")
   
   LET p_fec_ejecucion = DATE
   LET v_v_nom_reporte = p_v_usuario CLIPPED, "-SEPC23-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"

    ---- recupera la ruta de listados en el que se enviara el archivo
    CALL fn_rutas("sep") RETURNING r_ruta_bin, r_ruta_listados
    --DISPLAY "Ruta bin - ", r_ruta_bin
    --DISPLAY "Ruta lst - ", r_ruta_listados

    -- se indica que el reporte usara la plantilla creada
    IF fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED||"/SEPI231.4rp") THEN
       -- se indica la salida del reporte
       CALL fgl_report_selectDevice("PDF") 
         
       CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_v_nom_reporte)
      
       -- sin indica que no es necesario el preview
       CALL fgl_report_selectPreview(0)
         
       -- se asigna la configuración en el menejo del reporte
       LET v_manejador_rpt = fgl_report_commitCurrentSettings()

       -- inicia el reporte de registros procesados
       START REPORT rpt_expediente TO XML HANDLER v_manejador_rpt
         
          LET v_day = DAY(TODAY) USING "&&"
          LET v_mes = MONTH(TODAY) USING "&&"
          LET v_ano = YEAR(TODAY) USING "&&&&"
          LET v_fecha = v_day,"-",v_mes,"-",v_ano
         
          LET v_indice = 1

          LET v_consulta = "EXECUTE PROCEDURE   sp_sep_consulta_edo_expediente(?,?)"
          PREPARE prp_rec_espedientes FROM v_consulta
          DECLARE cur_rec_espedientes CURSOR FOR prp_rec_espedientes
        
          LET v_indice = 0
          FOREACH  cur_rec_espedientes USING p_estado,
                                             p_flujo_cod                                     
                                        INTO v_exp_recuperados.*
            LET v_indice = v_indice + 1
          
            LET v_expediente[v_indice].v_id_num          = v_indice
            LET v_expediente[v_indice].v_expediente      = v_exp_recuperados.v_id_expediente USING "#########"
            LET v_expediente[v_indice].v_invadido        = v_exp_recuperados.v_invadido
            LET v_expediente[v_indice].v_asociado        = v_exp_recuperados.v_asociado
            LET v_expediente[v_indice].v_tipo_flujo      = v_exp_recuperados.v_flujo
            LET v_expediente[v_indice].v_canal           = v_exp_recuperados.v_canal
            LET v_expediente[v_indice].v_fecha_recepcion = v_exp_recuperados.v_f_recepcion
            LET v_expediente[v_indice].v_fecha_captura   = v_exp_recuperados.v_f_captura
            LET v_expediente[v_indice].v_estado          = v_exp_recuperados.v_estado
            LET v_expediente[v_indice].v_estado          = v_exp_recuperados.v_estado

            OUTPUT TO REPORT rpt_expediente(v_expediente[v_indice].*, v_fecha)
                      
        END FOREACH
        FREE cur_rec_espedientes
            
       -- finaliza el reporte
       FINISH REPORT rpt_expediente

            
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
         
         
         
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPI23                                                   #
#Objetivo          =>                       #
#Autor             =>                                   #
#Fecha Inicio      => 01/06/2012                                               #
################################################################################
REPORT rpt_expediente(v_expediente, v_fecha)
DEFINE  v_expediente RECORD
            v_id_num          INTEGER,
            v_expediente      STRING, --LIKE sep_expediente.id_expediente,
            v_invadido        LIKE sep_nss_expediente.nss,
            v_asociado        LIKE sep_nss_expediente.nss,
            v_tipo_flujo      LIKE sep_cat_tipo_flujo.flujo_desc,
            v_canal           LIKE sep_cat_canal_recepcion_exp.canal_desc,
            v_fecha_recepcion LIKE sep_expediente.f_recepcion_infonavit,
            v_fecha_captura   LIKE sep_expediente.f_captura,
            --v_estado          LIKE sep_expediente.estado
            v_estado          LIKE sep_estado_expediente.descripcion
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
         
            PRINTX v_expediente.*
            
            LET v_totales  = v_totales + 1
         
END REPORT
