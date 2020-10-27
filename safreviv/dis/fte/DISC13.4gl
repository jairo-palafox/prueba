################################################################################
#Version                    => 1.1.0                                           #
#Fecha ultima modificacion  => 13/08/2013                                     #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                        #
#Programa          => DISC13                                                     #
#Objetivo          => Consultar y generar reporte de montos para las aportaciones#
#                     subsecuentes especiales                                    # 
#Fecha inicio      => 13/08/2013                                                 #
################################################################################
#Registro de modificaciones:
#Autor           Fecha         Descrip. cambio
#Eneas Armas     18/12/2013    Se agrega validación, que no se tenga la
#                              Preliquidación de Dispersión de Pagos ejecutándose
#Eneas Armas     20140123      Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa

DATABASE safre_viv

GLOBALS
  DEFINE g_proceso_cod       LIKE glo_ctr_archivo.folio
  DEFINE g_desc_edo_arch     VARCHAR(23)
  DEFINE g_desc_edo_oper     VARCHAR(44)
  DEFINE g_sum_imp_apo_pat   DECIMAL(22,2)--LIKE dis_ap_subsecuente.imp_apo_pat
  DEFINE g_sum_imp_amo_cred  DECIMAL(22,2)--LIKE dis_ap_subsecuente.imp_amo_cred
  DEFINE g_imp_apo_pat       DECIMAL(22,2)--LIKE dis_ctr_ap_subsecuente.tot_imp_apo_pat
  DEFINE g_imp_amo_cred      DECIMAL(22,2)--LIKE dis_ctr_ap_subsecuente.tot_imp_amo_cred
  DEFINE g_tot_reg           DECIMAL(22,2)--LIKE dis_ctr_ap_subsecuente.tot_registros
  DEFINE g_tot_reg_dif       INTEGER
  DEFINE g_indice            SMALLINT
  DEFINE g_tot_pag           DECIMAL(22,2)
  DEFINE g_tot_sub           DECIMAL(22,2)
  DEFINE g_tot_dif           DECIMAL(22,2)
 
  
  DEFINE arr_dif_apo         DYNAMIC ARRAY OF RECORD
    nss                      LIKE dis_ap_subsecuente.nss,
    folio_pago               LIKE dis_ap_subsecuente.folio_liquida,
    f_pago                   LIKE dis_ap_subsecuente.f_pago,
    periodo_pago             LIKE dis_ap_subsecuente.periodo_pago,
    folio_sua                DECIMAL(6,0),
    nrp                      CHAR(11),
    num_credito              DECIMAL(10,0),
    estado                   CHAR(53),
    apo_pat_pag              DECIMAL(22,2),--LIKE dis_ap_subsecuente.imp_apo_pat,
    --apo_pat_sub              DECIMAL(22,2),--LIKE dis_interface_ef.imp_ap_pat,
    aiv_apo_pat_sub          DECIMAL(22,2),--LIKE dis_interface_ef.imp_ap_pat,
    apo_pat_dif              DECIMAL(22,2)
  END RECORD

  DEFINE arr_ctr_subsecuente DYNAMIC ARRAY OF RECORD 
    v_folio                  DECIMAL(9,0),
    v_f_transferencia        DATE,
    v_tot_reg                DECIMAL(10,0),
    v_tot_aivs               DECIMAL(22,2),
    v_tot_apo                DECIMAL(22,2),
    v_tot_amo                DECIMAL(22,2),
    v_estado                 VARCHAR(40)
  END RECORD 

  DEFINE arr_det_ef DYNAMIC ARRAY OF RECORD
   r_folio_det_ef           DECIMAL(9,0),
   r_num_regs               DECIMAL(9,0),   
   r_tot_imp_apo_aiv        DECIMAL(22,6),--LIKE dis_ctr_ap_subsecuente.tot_imp_apo_pat,
   r_tot_imp_apo_pat        DECIMAL(22,6)	
  END RECORD


  DEFINE arr_detalle_folio   DYNAMIC ARRAY OF RECORD 
    v_folio                  DECIMAL(9,0),
    v_folio_liquida          DECIMAL(9,0),
    v_ind_liquida            VARCHAR(40),
    v_total_apo              DECIMAL(22,2),
    v_total_reg              DECIMAL(10,0)
  END RECORD 

  DEFINE 
    p_usuario_cod            LIKE seg_usuario.usuario_cod, -- Clave del usuario
    manejador_rpt            om.SaxDocumentHandler  -- Contenedor de Documentos para el reporte

  DEFINE 
    rp_folio                 DECIMAL(9,0),
    rp_f_transferencia       DATE,
    rp_total_reg_ctr         DECIMAL(10,0),
    rp_total_apo             DECIMAL(22,2),
    rp_total_aivs            DECIMAL(22,2),
    rp_total_amo             DECIMAL(22,2),
    rp_estado                VARCHAR(40),
    rp_folio_liquida         DECIMAL(9,0),
    rp_indicador             VARCHAR(20),
    rp_total_apo_det         DECIMAL(22,2),
    rp_total_reg_det         DECIMAL(10,0),
    rp_descripcion           VARCHAR(80)
      
   CONSTANT Por_Folio = 0
   CONSTANT Por_Fecha = 1
   CONSTANT Sin = 0
   DEFINE v_tbl_mov    VARCHAR(50) 

END GLOBALS

MAIN
  --Sección de Variables Locales
  DEFINE 
    v_folio_carga_ap_sub     LIKE dis_ctr_ap_subsecuente.folio,--Folio de Carga de Archivo de Aportaciones Subsecuentes
    v_fecha_trans            LIKE dis_ctr_ap_subsecuente.f_transferencia, -- Fecha liquidada
    v_nom_prog               VARCHAR(30)
    
  --Sección de Parámetros
  DEFINE 
    p_tipo_ejecucion         SMALLINT, -- Forma como ejecutara el programa
    p_s_titulo               STRING -- Titulo de la ventana

  --Seccion de Variables de Retorno
  DEFINE r_folio_valido      SMALLINT
  DEFINE r_fecha_valida      DATE
  DEFINE r_existe_ef         SMALLINT  --Variable que almacena el valor de la validación si exiten registros en ef
  DEFINE 
    f_ventana                ui.Window,   -- Define las propìedades de la Ventana
    f_forma                  ui.Form,     -- Define las propiedades de la forma
    v_descripcion            STRING 
    
  LET p_usuario_cod    = ARG_VAL(1)
  LET p_tipo_ejecucion = ARG_VAL(2)
  LET p_s_titulo       = ARG_VAL(3)
  LET v_nom_prog       = ARG_VAL(4)

   --validación que NO se tenga Preliquidación de Dispersión de Pagos ejecutándose
   IF f_existe_proceso_operacion_ejecutando(901, 1) THEN
      MENU "No se puede ejecutar"
         ATTRIBUTES ( STYLE="dialog", COMMENT="Preliquidación de Dispersión de Pagos ejecutándose,\ningrese a esta opción cuando finalice",
         IMAGE="information" )
         ON ACTION salir
            RETURN
      END MENU
   END IF

  LET v_fecha_trans    = ""
  LET g_proceso_cod    = 908

  -- si se obtuvo el titulo, se pone como titulo de programa
  IF (p_s_titulo IS NOT NULL) THEN
     CALL ui.Interface.setText(p_s_titulo)
  END IF

  PREPARE fn_tbl_mov FROM "execute function fn_tab_movimiento(?,?,?)"

  CLOSE WINDOW SCREEN    

  OPEN WINDOW vtn_cons_aportaciones WITH FORM "DISC131"   
    DIALOG   ATTRIBUTES(UNBUFFERED) 
      INPUT v_folio_carga_ap_sub
      FROM  f_folio_ap_subs

        BEFORE INPUT 
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()
          CALL f_forma.setElementHidden("gr_detalle_folio",TRUE) --Oculta sección de detalle

      END INPUT 
        
      ON ACTION cancelar
         EXIT PROGRAM 

      ON ACTION ACCEPT
         IF v_folio_carga_ap_sub IS NULL THEN
            CALL fn_mensaje("ATENCIÓN",
                            "No ha capturado ningún criterio de busqueda",
                            "about")
            NEXT FIELD f_folio_ap_subs
         ELSE
              --Valida que el folio exista en dis_interface_ef
              CALL fn_valida_existe_interface_ef(v_folio_carga_ap_sub)
              RETURNING r_existe_ef

            -- Si el folio no existe en dis_interface_ef se envía mensaje
            IF NOT r_existe_ef  THEN
               CALL fn_mensaje("ATENCIÓN", 
                               "No hay datos con los parámetros capturados",
                               "about")
               NEXT FIELD f_folio_ap_subs
            ELSE               
               LET r_folio_valido = v_folio_carga_ap_sub
               
               DISPLAY "Folio: ",r_folio_valido
                 --Se obtiene el detalle de interface_ef
               CALL fn_obtiene_detalle_interface_ef(r_folio_valido)

               CALL f_forma.setElementHidden("gr_detalle_folio",FALSE) --Muestra sección de detalle

              --Elimina el último registro
              CALL arr_det_ef.deleteElement(arr_det_ef.getLength())

              --Despliega el arreglo con el detalle
              DISPLAY ARRAY arr_det_ef TO scr_detalle.* 
                ON ACTION reporte
                   --Invoca el llamado a la función de generación de reporte
                   CALL fn_genera_reporte_interface_ef(r_folio_valido)
                 
                ON ACTION CANCEL 
                   EXIT PROGRAM
                         
                ON ACTION archivo
                   --Genera archivo de texto
                   CALL fn_genera_archivo(r_folio_valido)
              END DISPLAY
            END IF
         END IF
    END DIALOG  
  CLOSE WINDOW vtn_cons_aportaciones 
END MAIN

#OBJETIVO: Generar reporte con base en la selección del folio
FUNCTION fn_genera_reporte_interface_ef(f_folio)
DEFINE 
    f_folio                  DECIMAL(9,0),
    v_sum_det_apo            DECIMAL(12,2),--Suma aportacion detalle
    v_sum_det_acciones       DECIMAL(22,6),--Suma detalle en acciones
    v_sum_det_pesos          DECIMAL(18,2),--Suma detalle en pesos
    v_sum_aiv_pat            DECIMAL(22,6),
    v_sum_tot_reg            INTEGER,--Tot registros sumario
    v_f_pago                 DATE,--Fecha pago 
    v_sum_estado             SMALLINT,-- Estado del archivo
    v_det_nss                CHAR(11),--Nss trabajador
    v_fec_proc               DATE, --Fecha de proceso
    v_rec_dif                INTEGER, --Numero rechazos con fiferencia
    v_ruta_rep               STRING,
    v_tot_leidos             DECIMAL(10,0),
    v_subcuenta              SMALLINT,--Subcuenta obtenida de la cuenta movimiento          
    v_movimiento             SMALLINT,       --Movimiento de la cta_movimiento           
    v_desc_subcuenta         VARCHAR(40), ---Descripción de la subcuenta
    v_desc_movimiento        LIKE cat_movimiento.movimiento_desc, --Descripción del movimiento
    v_etiqueta_subcuenta     STRING,  --Etiqueta para concatenar la descripción de la subcuenta con su clave
    v_etiqueta_movimiento    STRING,  --Etiqueta movimiento para la descripción del movimiento con la clave
    v_sum_tot_acciones       DECIMAL(22,6),--Importe en pesos de cta_movimiento                                
    v_sum_tot_pesos          DECIMAL(22,6)--Movimiento en AIVS de cta_movimiento

  DEFINE 
    v_desc_edo_arch          CHAR(50),
    v_fol_archivo            SMALLINT,
    r_edo_archivo            CHAR(50),
    v_reg_no_dif             INTEGER,
    v_qry_detalle            STRING   --String de consulta del detalle del archivo
    
    
  DEFINE v_origen_datos      STRING
  DEFINE v_ruta_reporte      STRING -- ruta del archivo del reporte
  DEFINE v_ruta_listados     STRING -- ruta de los listados 
  DEFINE v_ruta_ejecutable   STRING -- ruta del ejecutable
  DEFINE v_edo_opera_cve     SMALLINT
  DEFINE v_edo_opera_des     LIKE cat_edo_ap_subsecuente.desc_edo_apo_sub
  DEFINE manejador_rpt       om.SaxDocumentHandler --Contenedor documentos para reporte 

      
  DEFINE 
    v_indice_1               INTEGER,    
    v_ind_rpt                INTEGER, --Indice para el reporte  
    v_cantidad               STRING --Variable auxiliar para cantidades

#DEBUG
--Obtiene el estado del archivo
  SELECT estado
  INTO   v_fol_archivo
  FROM   glo_ctr_archivo
  WHERE  proceso_cod = g_proceso_cod
  AND    folio       = f_folio 

  --Obtiene la descripcion del estado del archivo
  SELECT estado_descripcion
  INTO   v_desc_edo_arch
  FROM   cat_edo_archivo
  WHERE  estado_cod = v_fol_archivo

  --Concatena el estado y la descripcion del archivo
  LET r_edo_archivo = v_fol_archivo || '-' || v_desc_edo_arch CLIPPED
  
  --Obtiene la fecha de pago
  LET v_qry_detalle="SELECT FIRST 1 f_pago\n" ,    
                    "FROM dis_interface_ef \n",      
                    "WHERE folio_liquida=",f_folio
                    
  PREPARE stm_fec_pago FROM  v_qry_detalle
  EXECUTE stm_fec_pago INTO v_f_pago   
  
  --Obtiene total de registros leídos
  SELECT COUNT (*)
  INTO   v_tot_leidos
  FROM   dis_interface_ef
  WHERE folio_liquida=f_folio
  
  LET v_origen_datos = p_usuario_cod

  --Se asigna la plantilla para generar el reporte
  --IF fgl_report_loadCurrentSettings("arch_aport_subs.4rp") THEN
  IF fgl_report_loadCurrentSettings("DISC131.4rp") THEN  
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF
    
  --Inicializamos variables para suma de totales
  LET v_indice_1       = 1
  LET v_sum_det_apo    = 0
  LET v_sum_aiv_pat      = 0.0
  LET v_sum_det_acciones = 0.0
  LET v_sum_det_pesos    = 0.0
  
   --Query de totales de dis_interface_ef
   LET v_qry_detalle="SELECT SUM(imp_ap_pat),SUM(aiv_ap_pat)\n",
                      "FROM dis_interface_ef\n",
                      "WHERE folio_liquida=",f_folio,"\n"
   PREPARE stm_dis_intef FROM v_qry_detalle
   EXECUTE stm_dis_intef INTO v_sum_det_apo,v_sum_aiv_pat

   --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
   EXECUTE fn_tbl_mov USING Por_Folio,f_folio,Sin INTO v_tbl_mov --por folio
   
   --Query para obtener la suma de monto total en acciones y pesos, sólo subcuenta 4
   LET v_qry_detalle="SELECT SUM(monto_pesos),SUM(monto_acciones)\n",
                      "FROM ",v_tbl_mov,"\n",
                      "WHERE folio_liquida=",f_folio,"\n",
                      "AND subcuenta=4"
   
   PREPARE stm_sum_totales FROM v_qry_detalle
   EXECUTE stm_sum_totales INTO v_sum_tot_pesos, v_sum_tot_acciones
   
   --Query de totales de cta_movimiento por subcuenta y movimiento
   LET v_qry_detalle="SELECT subcuenta, movimiento, SUM(monto_pesos),SUM(monto_acciones)\n",
                     "FROM ",v_tbl_mov,"\n",
                     "WHERE folio_liquida=",f_folio,"\n",
                     "GROUP BY 1,2",
                     "ORDER BY 1,2"

   PREPARE prp_consulta_detalle FROM v_qry_detalle
   DECLARE cur_detalle CURSOR FOR prp_consulta_detalle
                    
   --Inicia el reporte
   START REPORT rp_cifras_esp TO XML HANDLER manejador_rpt
     
     FOREACH cur_detalle INTO   v_subcuenta,v_movimiento,v_sum_det_pesos,v_sum_det_acciones	
       --Ejecuta consulta de detalle consulta de descripción de subcuenta 
       LET v_qry_detalle="SELECT subcuenta_desc\n",
     	  	         "FROM cat_subcuenta\n",
     	  	         "WHERE subcuenta = ",v_subcuenta
       PREPARE stm_desc_subcta FROM v_qry_detalle
       EXECUTE stm_desc_subcta INTO v_desc_subcuenta
       	
       --Ejecuta query de selección de consulta de movimiento
      SELECT movimiento_desc
      INTO v_desc_movimiento
      FROM cat_movimiento
      WHERE movimiento = v_movimiento
     	  	 
      --Se concatena las etiquetas
      LET v_etiqueta_subcuenta  = v_subcuenta,"-",v_desc_subcuenta 
      LET v_etiqueta_movimiento = v_movimiento,"-",v_desc_movimiento

      --Se envía la info al reporte
      OUTPUT TO REPORT rp_cifras_esp(f_folio,
                                     p_usuario_cod,
                                     v_sum_det_apo,
                                     v_sum_aiv_pat,
                                     v_sum_tot_pesos,
                                     v_sum_tot_acciones,
                                     v_etiqueta_subcuenta,
                                     v_etiqueta_movimiento,
                                     v_sum_det_pesos,
                                     v_sum_det_acciones,
                                     v_tot_leidos,
                                     v_f_pago,
                                     r_edo_archivo)
     END FOREACH
   FINISH REPORT rp_cifras_esp 
    
END FUNCTION

#OBJETIVO: Consultar que la información a reversar exista en la tabla de
#          interface de entidades financieras
FUNCTION fn_valida_existe_interface_ef(p_folio)
   DEFINE 
    p_folio                  LIKE dis_interface_ef.folio_liquida,
    r_folio_valido           DECIMAL(9,0),  --LIKE dis_ctr_ap_subsecuente.folio,
    v_qry_txt                STRING,        --Query para consultar sobre dis_interface_ef
    v_existe_int_ef          DECIMAL(9,0),  --Variable para conocer si existen registros en dis_interface_ef
    v_existe_ac_liq          DECIMAL(9,0),  --Variable para conocer si se han ejecutado acciones de conciliación    
    v_bnd_existe_ef          SMALLINT       --Bandera para indicar si existe información con los criterios indicados 
    
   --Se inicializa las variables para conocer si existe folio
   LET v_existe_int_ef = 0
   LET v_existe_ac_liq = 0
   LET v_bnd_existe_ef = TRUE
    
  --Se consulta para conocer si hay registros en dis_interface_ef
  LET v_qry_txt="SELECT COUNT(*)\n ",
                "FROM dis_interface_ef \n",
                 "WHERE folio_liquida=",p_folio,"\n"

  
  PREPARE stm_count_interface_ef FROM v_qry_txt
  EXECUTE stm_count_interface_ef INTO v_existe_int_ef
  
  --Se valida que exista información para el folio indicado
  IF v_existe_int_ef > 0 THEN
  	
     --Existen datos y se ejecuta query de validación sobre la existencia de registros conciliados
     LET v_qry_txt="SELECT COUNT(*)\n",
                   "FROM dis_interface_ef \n",
                    "WHERE folio_liquida=",p_folio,"\n",
                    "AND ind_liquidacion <> 0"
     PREPARE stm_count_ind_liq FROM v_qry_txt
     EXECUTE stm_count_ind_liq INTO v_existe_ac_liq
     
     --Se valida la existencia de registros con acciones de conciliación
     IF v_existe_ac_liq > 0 THEN
     	--Existe al menos un registro con acciones de conciliación por lo tanto no se puede eliminar
     	LET v_bnd_existe_ef = FALSE   
     	
     END IF
  ELSE
    	--No existen registros para el folio
    	LET v_bnd_existe_ef = FALSE
  END IF
     
  --Regresa validación de existencia de información y sin registros con acciones de liquidación
  RETURN v_bnd_existe_ef                 
                 
END FUNCTION

#OBJETIVO: Obtiene la información de los registros en interface_ef
#          interface de entidades financieras
FUNCTION fn_obtiene_detalle_interface_ef(p_folio)
DEFINE p_folio             LIKE dis_interface_ef.folio_liquida,
       v_qry_dis_int_ef    STRING,
       v_indice_arreglo    INTEGER
       
  --Inicializa el arreglo
  LET  v_indice_arreglo = 1  

  --Elabora el query de selección    
  LET v_qry_dis_int_ef="SELECT folio_liquida,COUNT(*),SUM(imp_ap_pat),SUM(aiv_ap_pat)\n",
                       "FROM dis_interface_ef\n",
                       "WHERE folio_liquida=",p_folio,"\n",
                       "GROUP BY 1"
  --Preparación del query
  PREPARE stm_int_ef FROM v_qry_dis_int_ef
  DECLARE cur_int_ef CURSOR FOR stm_int_ef
  --Ejecución del query
  FOREACH cur_int_ef INTO arr_det_ef[v_indice_arreglo].*
    --Incrementa el indice en 1
    LET v_indice_arreglo= v_indice_arreglo +1
  END FOREACH

END FUNCTION

#OBJETIVO: Genera el reporte de cifras de control de las aportaciones subsecuentes especiales
REPORT rp_cifras_esp(v_rfolio,
                       v_rusurio,
                       v_rsum_det_apo,      
                       v_rsum_det_aiv_pat,
                       v_rsum_tot_pesos,
                       v_rsum_tot_acciones,  
                       v_subcuenta,
                       v_movimiento, 
                       v_rsum_det_pesos,    
                       v_rsum_det_acciones,
                       v_rsum_tot_reg,
                       v_rfec_pago,
                       r_redo_archivo
                      )              
                       
  DEFINE 
    v_rusurio                VARCHAR(30),--Usuario de proceso
    v_rfolio                 DECIMAL(9,0),
    v_rfec_pago              DATE,
    v_rsum_tot_pesos         DECIMAL (22,6),
    v_rsum_tot_acciones      DECIMAL (22,6), 
    v_rsum_det_apo           DECIMAL(22,6),
    v_rsum_det_aiv_pat       DECIMAL(22,6),
    v_rsum_det_pesos         DECIMAL(22,6),
    v_rsum_det_acciones      DECIMAL(22,6),
    v_subcuenta              STRING,
    v_movimiento             STRING,
    v_rsum_tot_reg           SMALLINT
    

  DEFINE r_redo_archivo      CHAR(50)--Descripción del estado del archivo

  FORMAT
    FIRST PAGE HEADER
      PRINTX v_rfolio
      PRINTX v_rusurio
      PRINTX v_rfec_pago     USING "dd-mm-yyyy" 
      PRINTX v_rsum_det_apo   
      PRINTX v_rsum_det_aiv_pat
      PRINTX v_rsum_tot_pesos  
      PRINTX v_rsum_tot_acciones       
      PRINTX v_rsum_tot_reg
      PRINTX r_redo_archivo
     

    ON EVERY ROW
       PRINTX v_subcuenta     
       PRINTX v_movimiento    
       PRINTX v_rsum_det_pesos
       PRINTX v_rsum_det_acciones   
  
END REPORT
#OBJETIVO: Elaborar el archivo de texto con el detalle de los registros obtenidos de dis_interface_ef y cta_movimiento
FUNCTION fn_genera_archivo(f_folio)
  DEFINE 
    f_folio                  LIKE dis_interface_ef.folio_liquida, --Folio de liquidación para el --
    v_qry_detalle            STRING,
    v_qry_subcuenta          STRING,    --Query para obtener las subcuentas de cta_movimiento
    v_subcuenta_mvto         LIKE cat_subcuenta.subcuenta,  --Subcuenta que se ontendrá de cta_movimiento

    v_rec_detalle            RECORD 
    v_det_nss                CHAR(11),
    v_num_credito            DECIMAL (11,0),
    v_det_periodo_pago       CHAR(6),                                  
    v_folio_sua              DECIMAL(6,0),                                              
    v_det_f_pago             DATE,                                                      
    v_imp_ap_pat             DECIMAL(15,6),--Importe de aportaciones patronales             
    v_imp_aiv_ap_pat         DECIMAL(18,6),--Importe de AIV's de aportaciones patronales    
    v_subcuenta              SMALLINT,--Subcuenta obtenida de la cuenta movimiento
    v_subcuenta_desc         VARCHAR(60), --Descripción de la subcuenta
    v_movimiento             SMALLINT,       --Movimiento de la cta_movimiento           
    v_movimiento_desc        VARCHAR(60),   --Descripción del movimiento
    v_monto_acciones         DECIMAL(16,6),--Importe en pesos de cta_movimiento                                
    v_monto_pesos            DECIMAL(12,6)--Movimiento en AIVS de cta_movimiento    
    END RECORD,

    v_nom_archivo            VARCHAR(100), -- nombre del archivo de salida  
    v_ch_arch_salida         base.Channel, --Variable para crear el archivo
    v_ruta_nomarch           VARCHAR(100), -- ruta y nombre del archivo de salida 	
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_detalle                STRING,  --Variable para concatenar y escribir el detalle
    v_detalle_aux            STRING,  --Variable de paso para eliminar espacios
    v_encabezado             STRING, --Variable para concatenar y escribir el encabezado de la función
    v_sumario                STRING, --Variable que almacena el sumario y escribe al final del archivo
    v_sumario_aux            STRING, --Variable para eliminar espacios del sumario
    v_comando                STRING, --Variable que almacena el comando a ejecutar
    v_fecha_archivo          DATE,  
    v_hora_archivo           DATETIME HOUR TO HOUR ,
    v_min_archivo            DATETIME MINUTE TO MINUTE,
    v_sec_archivo            DATETIME SECOND TO SECOND,
    v_hora                   STRING,
    v_num_registros          INTEGER,--Número de registros obtenidos
    v_desc_subcuenta         VARCHAR(40),  --Descripción de subcuenta
    v_desc_movimiento        LIKE cat_movimiento.movimiento_desc,  --Descripción de movimiento
    v_sum_ap_pat             DECIMAL (22,2), --Suma de los totales de ap_pat
    v_sum_aivs               DECIMAL(22,2),  --Suma de los totales de aivs
    v_sum_monto_pesos        DECIMAL(22,2),  --Suma monto en pesos
    v_sum_monto_acciones     DECIMAL(22,2),   --Suma monto en acciones

    v_arr_tot_subcta         DYNAMIC ARRAY OF RECORD  --Arreglo de totales las subcuenta
    v_desc_subcta            VARCHAR(60),  --Descripción de la subcuenta
    v_tot_monto_pesos_subcta DECIMAL(22,2),  --Suma monto en pesos
    v_sum_monto_acciones_subcta DECIMAL(22,2)   --Suma monto en acciones
    END RECORD,

    v_indice_arreglo         SMALLINT 
         	
    --Inicializa variables
    LET v_fecha_archivo     = TODAY 
    LET v_hora_archivo      = CURRENT HOUR TO HOUR
    LET v_min_archivo       = CURRENT MINUTE TO MINUTE
    LET v_sec_archivo       = CURRENT SECOND TO SECOND
    
          
    --Se inicializan los contadores de cada subcuenta
    LET v_indice_arreglo    = 1
    LET v_num_registros     = 1
    LET v_sum_ap_pat        = 0.0 
    LET v_sum_aivs          = 0.0
    LET v_sum_monto_pesos   = 0.0
    LET v_sum_monto_acciones= 0.0
       
    LET v_hora = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".dis"
      
    LET v_nom_archivo = "/aportaciones_subsec_esp_",v_hora      
       
    --Se obtiene la ruta de salida de los archivos para el módulo
    SELECT ruta_envio 
    INTO v_ruta_envio_dis
    FROM seg_modulo
    WHERE modulo_cod = "dis"
      
    --Se concatenan las rutas del archivo
    LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED 

   --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
   EXECUTE fn_tbl_mov USING Por_Folio,f_folio,Sin INTO v_tbl_mov --por folio

    --Query detalle 
    LET v_qry_detalle = " SELECT af.nss, \n",
                        "        ef.num_crd_ifv, \n",
                        "        ef.periodo_pago,       \n",
                        "        ef.folio_sua,          \n",
                        "        ef.f_pago,\n          ",
                        "        ef.imp_ap_pat,     \n",
                        "        ef.aiv_ap_pat,   \n",
                        "        cm.subcuenta,    \n",
                        "        cs.subcuenta_desc,\n ",
                        "        cm.movimiento,   \n",
                        "        ct.movimiento_desc,\n",
                        "        cm.monto_acciones,  \n",
                        "        cm.monto_pesos   \n",
                        " FROM   ",v_tbl_mov," cm,\n",
                        "        cat_subcuenta cs,\n",
                        "        cat_movimiento ct,\n",
                        "(dis_interface_ef ef LEFT OUTER JOIN \n",
                        "       afi_derechohabiente af\n",
                        "       ON af.id_derechohabiente=ef.id_derechohabiente)\n",
                        " WHERE ef.folio_liquida =",f_folio," \n",
                        " AND    cm.folio_liquida= ef.folio_liquida  \n",
                        " AND    cm.id_referencia=ef.id_dis_interface_ef\n",
                        " AND    cs.subcuenta=cm.subcuenta\n   ",
                        " AND    ct.movimiento=cm.movimiento\n ",             
                        " ORDER BY cm.subcuenta ASC"
    #DEBUG#
    DISPLAY "v_qry_detalle:\n", v_qry_detalle
    #END DEBUG# 
        
    PREPARE stm_detalle_archivo FROM v_qry_detalle
    DECLARE cur_detalle_archivo CURSOR FOR stm_detalle_archivo
       
    --Abre archivo para escritura
    LET v_ch_arch_salida = base.Channel.create()
    CALL v_ch_arch_salida.OpenFile(v_ruta_nomarch,"w")     
    CALL v_ch_arch_salida.setDelimiter("")
      
    --Recorre el cursor 	
    FOREACH cur_detalle_archivo INTO v_rec_detalle.*
      --En caso que sea el primer registro se escribe el encabezado
      IF v_num_registros = 1 THEN
         --Imprime encabezado
         LET  v_encabezado="Folio de pago :", f_folio
         CALL v_ch_arch_salida.write([v_encabezado])
         LET  v_encabezado="Fecha de pago: ",v_rec_detalle.v_det_f_pago
         CALL v_ch_arch_salida.write([v_encabezado])
         LET v_encabezado="NSS|NÚMERO DE CRÉDITO|PERIODO PAGO|FOLIO SUA|SUBCUENTA|MOVIMIENTO|APORTACIONES|AIVS|MONTO PESOS|MONTO ACCIONES"
         CALL v_ch_arch_salida.write([v_encabezado])
              
         --Arma los sumarios para el archivo por subcuenta
         LET v_subcuenta_mvto=v_rec_detalle.v_subcuenta
         LET v_arr_tot_subcta[v_indice_arreglo].v_desc_subcta=v_rec_detalle.v_subcuenta,"-",v_rec_detalle.v_subcuenta_desc
         LET v_arr_tot_subcta[v_indice_arreglo].v_tot_monto_pesos_subcta=v_rec_detalle.v_monto_pesos
         LET v_arr_tot_subcta[v_indice_arreglo].v_sum_monto_acciones_subcta=v_rec_detalle.v_monto_acciones 
      ELSE
         --Valida cambio de subcuenta
         IF v_subcuenta_mvto=v_rec_detalle.v_subcuenta THEN 
            LET v_arr_tot_subcta[v_indice_arreglo].v_tot_monto_pesos_subcta = v_arr_tot_subcta[v_indice_arreglo].v_tot_monto_pesos_subcta +
                                                                              v_rec_detalle.v_monto_pesos
                                                                                 
            LET v_arr_tot_subcta[v_indice_arreglo].v_sum_monto_acciones_subcta = v_arr_tot_subcta[v_indice_arreglo].v_sum_monto_acciones_subcta + 
                                                                                 v_rec_detalle.v_monto_acciones 
         ELSE
            --Cambia la subcuenta se agrega otro registro al arreglo
            LET v_indice_arreglo=v_indice_arreglo + 1
                
            --Se agrega un nuevo registro
            LET v_subcuenta_mvto=v_rec_detalle.v_subcuenta
            LET v_arr_tot_subcta[v_indice_arreglo].v_desc_subcta               = v_rec_detalle.v_subcuenta,"-",v_rec_detalle.v_subcuenta_desc
            LET v_arr_tot_subcta[v_indice_arreglo].v_tot_monto_pesos_subcta    = v_rec_detalle.v_monto_pesos
            LET v_arr_tot_subcta[v_indice_arreglo].v_sum_monto_acciones_subcta = v_rec_detalle.v_monto_acciones 
         END IF
      END IF 
               
      --Se concatena el detalle del archivo
      LET v_detalle_aux = v_rec_detalle.v_det_nss,"|"
      LET v_detalle     = v_detalle_aux.trim()
       	  	  	
      LET v_detalle_aux = v_rec_detalle.v_num_credito,"|"
      LET v_detalle     = v_detalle,v_detalle_aux.trim()
      
      LET v_detalle_aux = v_rec_detalle.v_det_periodo_pago
      LET v_detalle     = v_detalle,v_detalle_aux.trim(),"|"
       	  	  	
      LET v_detalle_aux = v_rec_detalle.v_folio_sua
      LET v_detalle     = v_detalle,v_detalle_aux.trim(),"|"
       	  	  	
      LET v_detalle_aux = v_rec_detalle.v_subcuenta,"-",v_rec_detalle.v_subcuenta_desc
      LET v_detalle     = v_detalle,v_detalle_aux.trim(),"|"
       	  	  	
      LET v_detalle_aux = v_rec_detalle.v_movimiento,"-",v_rec_detalle.v_movimiento_desc
      LET v_detalle     = v_detalle,v_detalle_aux.trim(),"|"
       	  	  	
      LET v_detalle_aux = v_rec_detalle.v_imp_ap_pat
      LET v_detalle     = v_detalle,v_detalle_aux.trim(),"|"
       	  	  	
      LET v_detalle_aux = v_rec_detalle.v_imp_aiv_ap_pat
      LET v_detalle     = v_detalle,v_detalle_aux.trim(),"|"
       	  	  	
      LET v_detalle_aux = v_rec_detalle.v_monto_pesos
      LET v_detalle     = v_detalle,v_detalle_aux.trim(),"|"   
       	  	  	
      LET v_detalle_aux = v_rec_detalle.v_monto_acciones
      LET v_detalle     = v_detalle,v_detalle_aux.trim(),"|"          	  	  	    	  	  	
 
      --Escribe en archivo el detalle
      CALL  v_ch_arch_salida.write([v_detalle])
       	  
      --Incrementa índice del recorrido
      LET v_num_registros = v_num_registros + 1
    END FOREACH 	
             
    FOR v_indice_arreglo = 1 TO v_arr_tot_subcta.getLength()
        --Se escribe los totales para cada subcuenta
        LET v_sumario = "TOTALES SUBCUENTA :"
               
        LET v_sumario_aux = v_arr_tot_subcta[v_indice_arreglo].v_desc_subcta
        LET v_sumario     = v_sumario,v_sumario_aux.trim(),"|"
               
        LET v_sumario_aux = v_arr_tot_subcta[v_indice_arreglo].v_tot_monto_pesos_subcta
        LET v_sumario     = v_sumario,v_sumario_aux.trim(),"|"
        
        LET v_sumario_aux = v_arr_tot_subcta[v_indice_arreglo].v_sum_monto_acciones_subcta
        LET v_sumario     = v_sumario,v_sumario_aux.trim()

        --Se escribe el sumario en el pie de página
        CALL v_ch_arch_salida.write([v_sumario])
    END FOR
            
    --Cierra el archivo
    CALL v_ch_arch_salida.close()
             	                    
    --Cambia los comandos
    LET v_comando = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
    RUN v_comando
  
    --Envía mensaje de notificación
    CALL fn_mensaje("Información","Se ha generado el archivo en la ruta "||v_ruta_nomarch,"information")
   
END FUNCTION
