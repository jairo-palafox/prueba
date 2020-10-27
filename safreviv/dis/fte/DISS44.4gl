################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 09/11/2016                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISS44                                                   #
#Objetivo          => Programa que genera la intefaz del extractor de          #
#                     Dispersión de Aclaratorios con Cambio de NSS             #
#Fecha inicio      => 07/09/2015                                               #
################################################################################
DATABASE safre_viv

GLOBALS
  DEFINE 
    p_usuario                  VARCHAR(30),                    --Almacena al usuario
    g_proceso_cod              LIKE cat_proceso.proceso_cod,   --codigo de proceso
    g_opera_cod                LIKE cat_operacion.opera_cod,   --codigo de operacion
    p_folio                    LIKE glo_folio.folio,           --Folio capturado por el usuario
    g_folio                    LIKE glo_folio.folio,           --Folio generado
    g_pid                      LIKE glo_pid.pid,
    g_arch_proceso             VARCHAR(100),
    g_qrytxt                   STRING ,                        --Prepara consultas
    p_periodo_pago             CHAR(6),
    g_proceso_cnt              SMALLINT    

  DEFINE v_modulo_cod          LIKE seg_modulo.modulo_cod
  DEFINE v_comando_dos         STRING
  DEFINE v_archivo_copia       VARCHAR(40)
  DEFINE g_mensaje             STRING
  DEFINE r_bandera             SMALLINT
   
END GLOBALS

MAIN
  LET p_folio        = ARG_VAL(1)
  LET g_pid          = ARG_VAL(2)   
  LET g_folio        = ARG_VAL(3)   
  LET p_usuario      = ARG_VAL(4)

  LET g_proceso_cod  = 930
  LET g_opera_cod    = 1

  --DISPLAY "p_folio -",p_folio,"-"
  --DISPLAY "g_pid -",g_pid,"-"
  --DISPLAY "g_folio -",g_folio,"-"
  --DISPLAY "p_usuario -",p_usuario,"-"
   
  CALL STARTLOG(p_usuario CLIPPED||".DISS44.log")

  LET v_modulo_cod = "dis"

  --Llama a la función que genera la interface de extractor de Dispersión de Aclaratorios con Cambio de NSS
  CALL fn_genera_interface_dispersion()

  CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
  RETURNING r_bandera

END MAIN

--Función que genera el archivo de salida del extractor de dispersión
FUNCTION fn_genera_interface_dispersion()
  DEFINE 
    r_bnd                      INTEGER, 
    v_status_err               INTEGER ,
    v_desc_err                 VARCHAR(200)
                    
  WHENEVER ERROR CONTINUE
    PREPARE ps_sp_acl_nss FROM "EXECUTE PROCEDURE fn_dis_ext_acl_cam_nss(?)"
    EXECUTE ps_sp_acl_nss USING p_folio
                          INTO r_bnd, v_status_err, v_desc_err
  WHENEVER ERROR STOP

  WHENEVER ERROR CONTINUE
    PREPARE ps_sp_acl_nss_des FROM "EXECUTE PROCEDURE fn_dis_ext_acl_cam_nss_des(?)"
    EXECUTE ps_sp_acl_nss_des USING p_folio
                              INTO r_bnd, v_status_err, v_desc_err
  WHENEVER ERROR STOP

  CALL arma_archivo()

END FUNCTION 

FUNCTION arma_archivo()
  DEFINE 
    v_ch_arch_salida           BASE.CHANNEL,
    v_ruta_envio_dis           LIKE seg_modulo.ruta_envio,
    v_ruta_nomarch             VARCHAR(100), --Ruta y nombre del archivo de salida
    v_nom_archivo              VARCHAR(100),  --Nombre del archivo de salida
    v_aaaammdd                 VARCHAR(08),  --Fecha del archivo de salida
    --v_hora_archivo             DATETIME HOUR TO HOUR,
    --v_min_archivo              DATETIME MINUTE TO MINUTE,
    --v_sec_archivo              DATETIME SECOND TO SECOND,
    v_hora_archivo             STRING,
    v_min_archivo              STRING,
    v_sec_archivo              STRING,

    v_hora                     STRING
    
  DEFINE 
    v_encabezado               STRING,
    v_detalle                  STRING
             
  DEFINE i                     INTEGER
  
  DEFINE v_arr_acl_cam_nss     DYNAMIC ARRAY OF RECORD
    folio_dispersion           DECIMAL(9,0),
    f_dispersion               DATE, 
    nss_dis                    CHAR(11),        
    num_cre_dis                DECIMAL(10,0),
    est_cre_dis                SMALLINT, 
    pes_viv_97_dis             DECIMAL(12,2), 
    acc_sal_viv_97_dis         DECIMAL(16,6),
    pes_viv_92_dis             DECIMAL(12,2), 
    acc_sal_viv_92_dis         DECIMAL(16,6),
    pes_amo_dis                DECIMAL(12,2), 
    acc_sal_amo_dis            DECIMAL(16,6),   
    est_cta_dis                SMALLINT,
    nss_no_dis                 CHAR(11),
    num_cre_no_dis             DECIMAL(10,0),
    est_cre_no_dis             SMALLINT,
    pes_viv_97_no_dis          DECIMAL(12,2), 
    acc_sal_viv_97_no_dis      DECIMAL(16,6),
    pes_viv_92_no_dis          DECIMAL(12,2), 
    acc_sal_viv_92_no_dis      DECIMAL(16,6),
    pes_amo_no_dis             DECIMAL(12,2), 
    acc_sal_amo_no_dis         DECIMAL(16,6),
    est_cta_no_dis             SMALLINT,      
    f_archivo                  DATE
  END RECORD

  LET v_aaaammdd     = TODAY USING "yyyymmdd"
  LET v_hora_archivo = CURRENT HOUR TO HOUR
  LET v_min_archivo  = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo  = CURRENT SECOND TO SECOND

  --Se obtienen la ruta envio del modulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  LET v_hora        = v_hora_archivo USING "&&", v_min_archivo USING "&&", v_sec_archivo USING "&&"

  LET v_nom_archivo = "/dis_ext_acl_cambio_nss_", v_aaaammdd,"_",v_hora,".",v_modulo_cod

  LET g_qrytxt      = "\n SELECT * ",
                      "\n FROM tmp_dis_cred_ant",
                      "\n ORDER BY 1, 2"
   
  PREPARE ps_dis_extractor_pr FROM g_qrytxt
  DECLARE cur_dis_extractor_pr CURSOR FOR ps_dis_extractor_pr

  --DISPLAY "v_ruta_nomarch -",v_ruta_nomarch,"-"  
  LET v_ruta_nomarch   = v_ruta_envio_dis CLIPPED || v_nom_archivo

  LET v_ch_arch_salida = base.Channel.create()
   
  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  --Imprime encabezado del archivo
  LET v_encabezado = "FOLIO DISPERSIÓN |FECHA DISPERSIÓN |NSS (DISPERSADO) |NÚMERO DE CRÉDITO (DISPERSADO) |ESTADO DEL CRÉDITO (DISPERSADO) |PESOS VIVIENDA 97 (DISPERSADO) |ACCIONES SALDO VIVIENDA 97 (DISPERSADO) |PESOS VIVIENDA 92 (DISPERSADO) |ACCIONES SALDO VIVIENDA 92 (DISPERSADO) |PESOS AMORTIZACIÓN (DISPERSADO) |ACCIONES SALDO AMORTIZACIÓN (DISPERSADO) |ESTADO DE LA CUENTA (DISPERSADO) |NSS (NO DISPERSADO) |NÚMERO DE CRÉDITO (NO DISPERSADO) |ESTADO DEL CRÉDITO (NO DISPERSADO) |PESOS VIVIENDA 97 (NO DISPERSADO) |ACCIONES SALDO VIVIENDA 97(NO DISPERSADO) |PESOS VIVIENDA 92 (NO DISPERSADO) |ACCIONES SALDO VIVIENDA 92 (NO DISPERSADO) |PESOS AMORTIZACIÓN (NO DISPERSADO) |ACCIONES SALDO AMORTIZACIÓN (NO DISPERSADO) |ESTADO DE LA CUENTA (NO DISPERSADO) |FECHA GENERACIÓN DEL ARCHIVO"
  CALL v_ch_arch_salida.write([v_encabezado])
  
  LET i = 1   
   
  FOREACH cur_dis_extractor_pr INTO v_arr_acl_cam_nss[i].folio_dispersion,      
                                    v_arr_acl_cam_nss[i].f_dispersion, 
                                    v_arr_acl_cam_nss[i].nss_dis,                                     
                                    v_arr_acl_cam_nss[i].num_cre_dis,
                                    v_arr_acl_cam_nss[i].est_cre_dis,                
                                    v_arr_acl_cam_nss[i].pes_viv_97_dis,       
                                    v_arr_acl_cam_nss[i].acc_sal_viv_97_dis,
                                    v_arr_acl_cam_nss[i].pes_viv_92_dis,       
                                    v_arr_acl_cam_nss[i].acc_sal_viv_92_dis, 
                                    v_arr_acl_cam_nss[i].pes_amo_dis,       
                                    v_arr_acl_cam_nss[i].acc_sal_amo_dis, 
                                    v_arr_acl_cam_nss[i].est_cta_dis,           
                                    v_arr_acl_cam_nss[i].nss_no_dis,       
                                    v_arr_acl_cam_nss[i].num_cre_no_dis,          
                                    v_arr_acl_cam_nss[i].est_cre_no_dis,                                                                            
                                    v_arr_acl_cam_nss[i].pes_viv_97_no_dis,     
                                    v_arr_acl_cam_nss[i].acc_sal_viv_97_no_dis,
                                    v_arr_acl_cam_nss[i].pes_viv_92_no_dis,     
                                    v_arr_acl_cam_nss[i].acc_sal_viv_92_no_dis,
                                    v_arr_acl_cam_nss[i].pes_amo_no_dis,     
                                    v_arr_acl_cam_nss[i].acc_sal_amo_no_dis,
                                    v_arr_acl_cam_nss[i].est_cta_no_dis,
                                    v_arr_acl_cam_nss[i].f_archivo    
      
    LET v_detalle = v_arr_acl_cam_nss[i].folio_dispersion, "|",
                    v_arr_acl_cam_nss[i].f_dispersion USING "yyyymmdd", "|",
                    v_arr_acl_cam_nss[i].nss_dis USING "&&&&&&&&&&&", "|",
                    v_arr_acl_cam_nss[i].num_cre_dis USING "&&&&&&&&&&", "|",
                    v_arr_acl_cam_nss[i].est_cre_dis, "|",                 
                    v_arr_acl_cam_nss[i].pes_viv_97_dis USING "&&&&&&&&&.&&", "|",
                    v_arr_acl_cam_nss[i].acc_sal_viv_97_dis USING "&&&&&&&&&.&&&&&&", "|",
                    v_arr_acl_cam_nss[i].pes_viv_92_dis USING "&&&&&&&&&.&&", "|",
                    v_arr_acl_cam_nss[i].acc_sal_viv_92_dis USING "&&&&&&&&&.&&&&&&", "|",
                    v_arr_acl_cam_nss[i].pes_amo_dis USING "&&&&&&&&&.&&", "|",
                    v_arr_acl_cam_nss[i].acc_sal_amo_dis USING "&&&&&&&&&.&&&&&&", "|",
                    v_arr_acl_cam_nss[i].est_cta_dis, "|",
                    v_arr_acl_cam_nss[i].nss_no_dis USING "&&&&&&&&&&&", "|",
                    v_arr_acl_cam_nss[i].num_cre_no_dis USING "&&&&&&&&&&", "|",
                    v_arr_acl_cam_nss[i].est_cre_no_dis, "|",                 
                    v_arr_acl_cam_nss[i].pes_viv_97_no_dis USING "&&&&&&&&&.&&", "|",
                    v_arr_acl_cam_nss[i].acc_sal_viv_97_no_dis USING "&&&&&&&&&.&&&&&&", "|",
                    v_arr_acl_cam_nss[i].pes_viv_92_no_dis USING "&&&&&&&&&.&&", "|",
                    v_arr_acl_cam_nss[i].acc_sal_viv_92_no_dis USING "&&&&&&&&&.&&&&&&", "|",
                    v_arr_acl_cam_nss[i].pes_amo_no_dis USING "&&&&&&&&&.&&", "|",
                    v_arr_acl_cam_nss[i].acc_sal_amo_no_dis USING "&&&&&&&&&.&&&&&&", "|",
                    v_arr_acl_cam_nss[i].est_cta_no_dis, "|",
                    v_arr_acl_cam_nss[i].f_archivo USING "yyyymmdd"
                                  
    CALL v_ch_arch_salida.write([v_detalle])        

    LET i = i + 1
  END FOREACH
     
  --  DISPLAY "Se encontraron ",i," de registros antes del cambio."    
  LET g_qrytxt = " SELECT * ",
                 "\n FROM tmp_dis_cred_des",
                 "\n ORDER BY 1, 2"
   
  PREPARE ps_dis_ext_des_pr FROM g_qrytxt
  DECLARE cur_dis_ext_des_pr CURSOR FOR ps_dis_ext_des_pr
  FOREACH cur_dis_ext_des_pr INTO v_arr_acl_cam_nss[i].folio_dispersion,      
                                  v_arr_acl_cam_nss[i].f_dispersion, 
                                  v_arr_acl_cam_nss[i].nss_dis,                                     
                                  v_arr_acl_cam_nss[i].num_cre_dis,
                                  v_arr_acl_cam_nss[i].est_cre_dis,                
                                  v_arr_acl_cam_nss[i].pes_viv_97_dis,       
                                  v_arr_acl_cam_nss[i].acc_sal_viv_97_dis,
                                  v_arr_acl_cam_nss[i].pes_viv_92_dis,
                                  v_arr_acl_cam_nss[i].acc_sal_viv_92_dis, 
                                  v_arr_acl_cam_nss[i].pes_amo_dis,
                                  v_arr_acl_cam_nss[i].acc_sal_amo_dis,
                                  v_arr_acl_cam_nss[i].est_cta_dis,           
                                  v_arr_acl_cam_nss[i].nss_no_dis,       
                                  v_arr_acl_cam_nss[i].num_cre_no_dis,          
                                  v_arr_acl_cam_nss[i].est_cre_no_dis,                                                                            
                                  v_arr_acl_cam_nss[i].pes_viv_97_no_dis,     
                                  v_arr_acl_cam_nss[i].acc_sal_viv_97_no_dis,
                                  v_arr_acl_cam_nss[i].pes_viv_92_no_dis,     
                                  v_arr_acl_cam_nss[i].acc_sal_viv_92_no_dis,
                                  v_arr_acl_cam_nss[i].pes_amo_no_dis,     
                                  v_arr_acl_cam_nss[i].acc_sal_amo_no_dis,
                                  v_arr_acl_cam_nss[i].est_cta_no_dis,
                                  v_arr_acl_cam_nss[i].f_archivo    
      
    LET v_detalle = v_arr_acl_cam_nss[i].folio_dispersion, "|",
                    v_arr_acl_cam_nss[i].f_dispersion USING "yyyymmdd", "|",
                    v_arr_acl_cam_nss[i].nss_dis USING "&&&&&&&&&&&", "|",
                    v_arr_acl_cam_nss[i].num_cre_dis USING "&&&&&&&&&&", "|",
                    v_arr_acl_cam_nss[i].est_cre_dis, "|",                 
                    v_arr_acl_cam_nss[i].pes_viv_97_dis USING "&&&&&&&&&.&&", "|",
                    v_arr_acl_cam_nss[i].acc_sal_viv_97_dis USING "&&&&&&&&&.&&&&&&", "|",
                    v_arr_acl_cam_nss[i].pes_viv_92_dis USING "&&&&&&&&&.&&", "|",
                    v_arr_acl_cam_nss[i].acc_sal_viv_92_dis USING "&&&&&&&&&.&&&&&&", "|",
                    v_arr_acl_cam_nss[i].pes_amo_dis USING "&&&&&&&&&.&&", "|",
                    v_arr_acl_cam_nss[i].acc_sal_amo_dis USING "&&&&&&&&&.&&&&&&", "|",
                    v_arr_acl_cam_nss[i].est_cta_dis, "|",
                    v_arr_acl_cam_nss[i].nss_no_dis USING "&&&&&&&&&&&", "|",
                    v_arr_acl_cam_nss[i].num_cre_no_dis USING "&&&&&&&&&&", "|",
                    v_arr_acl_cam_nss[i].est_cre_no_dis, "|",                 
                    v_arr_acl_cam_nss[i].pes_viv_97_no_dis USING "&&&&&&&&&.&&", "|",
                    v_arr_acl_cam_nss[i].acc_sal_viv_97_no_dis USING "&&&&&&&&&.&&&&&&", "|",
                    v_arr_acl_cam_nss[i].pes_viv_92_no_dis USING "&&&&&&&&&.&&", "|",
                    v_arr_acl_cam_nss[i].acc_sal_viv_92_no_dis USING "&&&&&&&&&.&&&&&&", "|",
                    v_arr_acl_cam_nss[i].pes_amo_no_dis USING "&&&&&&&&&.&&", "|",
                    v_arr_acl_cam_nss[i].acc_sal_amo_no_dis USING "&&&&&&&&&.&&&&&&", "|",
                    v_arr_acl_cam_nss[i].est_cta_no_dis, "|",
                    v_arr_acl_cam_nss[i].f_archivo USING "yyyymmdd"
                                  
    CALL v_ch_arch_salida.write([v_detalle])        

    LET i = i + 1
  END FOREACH

  LET i = i - 1   
  CALL v_arr_acl_cam_nss.deleteElement(v_arr_acl_cam_nss.getLength()) 

  -- DISPLAY "Se encontraron ",i," de registros después del cambio." 

  --Despliega información en el log
    
  DISPLAY ""
  DISPLAY "Nombre del archivo             : ", v_nom_archivo
  DISPLAY "Folio Dispersión               : ", p_folio
  DISPLAY "Total de registros             : ", i
  DISPLAY ""

  --Cierra el archivo
  CALL v_ch_arch_salida.close()
  --LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  LET v_comando_dos = "unix2dos ", v_ruta_nomarch
  RUN v_comando_dos

  LET g_mensaje = "Se ha generado el archivo de la interface del extractor de dispersión de aclaratorio por NSS",
                  "\nen la ruta: "||v_ruta_nomarch
                      
  DISPLAY g_mensaje

END FUNCTION