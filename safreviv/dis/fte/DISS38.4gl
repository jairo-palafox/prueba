################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 07/007/2015                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISS38                                                   #
#Objetivo          => Programa que genera la intefaz del extractor de          #
#                     Dispersión de Pagos.                                     #
#Fecha inicio      => 07/07/2015                                               #
################################################################################

DATABASE safre_viv

GLOBALS
  DEFINE 
    p_usuario                VARCHAR(30),                  --Almacena al usuario
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --codigo de proceso
    g_opera_cod              LIKE cat_operacion.opera_cod, --codigo de operacion
    p_folio                  LIKE dis_det_avance_pago.folio, --Folio generado
    g_pid                    LIKE glo_pid.pid,
    g_arch_proceso           VARCHAR(100),
    g_qrytxt                 STRING ,                       --Prepara consultas
    p_folio_dis              DECIMAL(9,0),
    g_proceso_cnt            SMALLINT,
    p_destino                SMALLINT 

   DEFINE v_modulo_cod        LIKE seg_modulo.modulo_cod

   DEFINE v_tot_registros          DECIMAL(9,0), -- Total de registros
          v_sum_aivs               DECIMAL(18,6),
          v_sum_aportacion         DECIMAL(12,2), 
          v_sum_amortizacion       DECIMAL(12,2)

   DEFINE v_sum_apo_ava            DECIMAL(12,2), 
          v_sum_amo_ava            DECIMAL(12,2),
          v_sum_apo_pag            DECIMAL(12,2),
          v_sum_amo_pag            DECIMAL(12,2),
          v_sum_dif_apo            DECIMAL(12,2),
          v_sum_dif_amo            DECIMAL(12,2)
          
   
   DEFINE v_comando_dos            STRING
   DEFINE v_archivo_copia          VARCHAR(40)

   DEFINE g_mensaje                STRING

   DEFINE g_proceso_cod   LIKE cat_proceso.proceso_cod, --codigo del proceso
          g_opera_cod     LIKE cat_operacion.opera_cod, --codigo de operacion
          g_pid           DECIMAL(9,0)
   
END GLOBALS

MAIN

   LET p_folio_dis    = ARG_VAL(1)
   LET p_destino      = ARG_VAL(2)
   LET g_pid          = ARG_VAL(3)
   LET g_opera_cod    = ARG_VAL(4)
   LET g_proceso_cod  = ARG_VAL(5)
   LET p_usuario      = ARG_VAL(6)
   
   CALL STARTLOG(p_usuario CLIPPED||".DISS38.log")

   LET v_modulo_cod = "dis"

   --Llama a la función que genera la interface de avance de pagos
   CALL fn_genera_interface_dispersion()

END MAIN


--Función que genera el archivo de salida del extractor de dispersión
FUNCTION fn_genera_interface_dispersion()
   DEFINE r_bnd                 INTEGER, 
          v_status_err          INTEGER ,
          v_desc_err            VARCHAR(200)


   DEFINE r_bandera              SMALLINT
          
  -- --------------- p_destino
  -- 1 PAGO REAL
  -- 2 ENTIDADES FINANCIERAS
  -- 3 INCONSISTENCIAS
  -- 4 APORTACIONES SUBSECUENTES
  -- 5 PORTABILIDAD

  WHENEVER ERROR CONTINUE 
    PREPARE ps_sp_dis_hs FROM "EXECUTE PROCEDURE fn_dis_extractor(?, ?)"
    EXECUTE ps_sp_dis_hs USING p_folio_dis, p_destino 
                          INTO r_bnd, v_status_err, v_desc_err
  WHENEVER ERROR STOP 


   CASE p_destino
      WHEN 1 
         CALL fn_extractor_pago_real()
      WHEN 2     
         CALL fn_extractor_ent_financieras()
      WHEN 3 
         CALL fn_extractor_inconsistencias()
      WHEN 4
         CALL fn_extractor_apo_subsecuentes()
      WHEN 5 
         CALL fn_extractor_portabilidad()
      WHEN 6
         CALL fn_extractor_avances_comp()
      OTHERWISE

   END CASE


   CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
   RETURNING r_bandera


END FUNCTION 



FUNCTION fn_extractor_pago_real()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_aaaammdd               VARCHAR(08),  --Fecha del archivo de salida
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_bnd_totales            INTEGER 

   DEFINE v_encabezado       STRING,
          v_detalle          STRING,
          v_sumario          STRING
          
   DEFINE v_precio_fondo     DECIMAL(19,14)
   DEFINE i                  DECIMAL(9,0)

   DEFINE v_estado_avance       SMALLINT
   DEFINE v_desc_destino        VARCHAR(30)
   DEFINE v_desc_edo_avance     VARCHAR(50)
   
   DEFINE v_arr_interface_hs DYNAMIC ARRAY OF RECORD
             nss                CHAR(11),
             num_credito        DECIMAL(10,0),
             periodo_pago       CHAR(6),
             f_pago             DATE, 
             nrp                CHAR(11),
             monto_apo          DECIMAL(12,2),
             aivs               DECIMAL(18,6),
             monto_amo          DECIMAL(12,2),
             folio_sua          DECIMAL(6,0),
             folio_liquida      DECIMAL(9,0),
             f_liquida          DATE,
             --estado_ava_pag     VARCHAR(40),
             f_interface        DATE,
             tipo_interface     CHAR(02)
         END RECORD

         
   LET v_desc_destino = "PAGO REAL"
   LET v_aaaammdd  = TODAY USING "yyyymmdd"
   
         
   LET g_qrytxt = " SELECT hs.* ",
                  "\n FROM tmp_dis_hs hs",
                  "\n ORDER BY  hs.periodo_pago "

   LET v_nom_archivo       = "/dis_ext_pr_", v_aaaammdd,"_"
   --LET v_busca_nom_archivo = "dis_ext_pr_", v_aaaammdd,"_"
         
   --DISPLAY g_qrytxt

   PREPARE ps_dis_extractor_pr FROM g_qrytxt
   DECLARE cur_dis_extractor_pr CURSOR FOR ps_dis_extractor_pr
  
  --Se obtienen la ruta envio del modulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  --Obtiene consecutivo para archivo por día
  --CALL fn_crea_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  --RETURNING v_cont_dia
  
  --LET v_reg_dia  = v_cont_dia USING "&&&"


  LET v_nom_archivo  = v_nom_archivo CLIPPED ||p_folio_dis CLIPPED||"."|| v_modulo_cod
  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo

  --DISPLAY "v_nom_archivo: ", v_nom_archivo
  --DISPLAY "v_ruta_nomarch: ", v_ruta_nomarch

  LET v_ch_arch_salida = base.Channel.create()
   
  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

 --Imprime encabezado del archivo
   {LET v_encabezado = " FECHA:  ",TODAY
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_encabezado = " FOLIO DE DISPERSIÓN:  ",p_folio_dis
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_encabezado = " DESTINO:  ",v_desc_destino
   CALL v_ch_arch_salida.write([v_encabezado])

   --LET v_encabezado = " VALOR DE FONDO:  ",v_precio_fondo
   --CALL v_ch_arch_salida.write([v_encabezado])}


   --LET v_encabezado = " NSS |NÚMERO DE CREDITO |PERIODO PAGO |FECHA PAGO |NRP |MONTO APORTACIÓN |AIVS |MONTO AMORTIZACIÓN |FOLIO SUA |FOLIO LIQUIDACIÓN |FECHA LIQUIDACIÓN |ESTADO AVANCE DE PAGO | FECHA INTERFACE |TIPO INTERFACE"
   LET v_encabezado = " NSS |NÚMERO DE CREDITO |PERIODO PAGO |FECHA PAGO |NRP |MONTO APORTACIÓN |AIVS |MONTO AMORTIZACIÓN |FOLIO SUA |FOLIO LIQUIDACIÓN |FECHA LIQUIDACIÓN | FECHA INTERFACE |TIPO INTERFACE"
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_tot_registros    = 0
   LET v_sum_aivs         = 0
   LET v_sum_aportacion   = 0
   LET v_sum_amortizacion = 0

   
   LET i = 1
   FOREACH cur_dis_extractor_pr INTO v_arr_interface_hs[i].nss, 
                                     v_arr_interface_hs[i].num_credito,
                                     v_arr_interface_hs[i].periodo_pago,      
                                     v_arr_interface_hs[i].f_pago,
                                     v_arr_interface_hs[i].nrp,                
                                     v_arr_interface_hs[i].monto_apo,       
                                     v_arr_interface_hs[i].aivs,            
                                     v_arr_interface_hs[i].monto_amo,       
                                     v_arr_interface_hs[i].folio_sua,          
                                     v_arr_interface_hs[i].folio_liquida,      
                                     v_arr_interface_hs[i].f_liquida,       
                                     --v_estado_avance,   
                                     v_arr_interface_hs[i].f_interface,     
                                     v_arr_interface_hs[i].tipo_interface  



      {IF v_estado_avance = 0 THEN
         LET v_arr_interface_hs[i].estado_ava_pag = v_estado_avance CLIPPED||" - SIN AVANCE DE PAGO"            
      ELSE

         SELECT desc_edo_avance
         INTO v_desc_edo_avance
         FROM cat_edo_avance_pago
         WHERE cod_edo_avance = v_estado_avance

         LET v_arr_interface_hs[i].estado_ava_pag = v_estado_avance CLIPPED||" - "||v_desc_edo_avance CLIPPED

      END IF}
      
      
      LET v_detalle = v_arr_interface_hs[i].nss USING "&&&&&&&&&&&", "|", 
                      v_arr_interface_hs[i].num_credito , "|",
                      v_arr_interface_hs[i].periodo_pago USING "&&&&&&", "|",      
                      v_arr_interface_hs[i].f_pago USING "yyyymmdd", "|",
                      v_arr_interface_hs[i].nrp, "|",                 
                      (v_arr_interface_hs[i].monto_apo * 100) USING "&&&&&&&&&&&&", "|",          
                      (v_arr_interface_hs[i].aivs * 100) USING "&&&&&&&&&&&&&&&&&&", "|",               
                      (v_arr_interface_hs[i].monto_amo * 100) USING "&&&&&&&&&&&&", "|",          
                      v_arr_interface_hs[i].folio_sua USING "&&&&&&", "|",          
                      v_arr_interface_hs[i].folio_liquida , "|",      
                      v_arr_interface_hs[i].f_liquida USING "yyyymmdd", "|",          
                      --v_arr_interface_hs[i].estado_ava_pag, "|",     
                      v_arr_interface_hs[i].f_interface USING "yyyymmdd", "|",        
                      v_arr_interface_hs[i].tipo_interface  
                                  
      CALL v_ch_arch_salida.write([v_detalle])

      
      LET v_tot_registros    = v_tot_registros    + 1
      LET v_sum_aivs         = v_sum_aivs         + v_arr_interface_hs[i].aivs
      LET v_sum_aportacion   = v_sum_aportacion   + v_arr_interface_hs[i].monto_apo
      LET v_sum_amortizacion = v_sum_amortizacion + v_arr_interface_hs[i].monto_amo
      LET i = i + 1
   END FOREACH

   LET i = i - 1
   CALL v_arr_interface_hs.deleteElement(v_arr_interface_hs.getLength())
   LET v_tot_registros = i
   
   --Escribe el sumario
   {LET v_sumario = "TOTALES|",v_tot_registros USING "&&&&&&&&&","| | | | ",
                              v_sum_aportacion USING "&&&&&&&&&", "|",
                              v_sum_aivs USING "&&&&&&&&&", "|",
                              v_sum_amortizacion USING "&&&&&&&&&"
                  
   CALL v_ch_arch_salida.write([v_sumario])}

   --DISPLAY "\nSe ha enviado la interface del extractor de dispersión de "|| v_desc_destino||"\n"

   --Cierra el archivo
   CALL v_ch_arch_salida.close()
   LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
   RUN v_comando_dos

    --Despliega información en el log
   DISPLAY "\n ############### INTERFACE ##################"
   DISPLAY " ############## "||v_desc_destino||  " ###############"
   DISPLAY ""
   DISPLAY "Nombre del archivo             : ", v_nom_archivo
   DISPLAY "Folio Dispersión               : ", p_folio_dis
   DISPLAY "Suma Monto Aportación          : ", v_sum_aportacion
   DISPLAY "Suma Monto AIVS                : ", v_sum_aivs
   DISPLAY "Suma Monto Amortización        : ", v_sum_amortizacion
   DISPLAY "\n ########################################################"
   
   LET g_mensaje = "Se ha generado el archivo de la interface del extractor de dispersión de "||v_desc_destino|| 
                   "\nen la ruta: "||v_ruta_nomarch
                   
   --CALL fn_mensaje("Información",g_mensaje,"information")
   DISPLAY g_mensaje

   --DISPLAY ""
   --DISPLAY "El archivo ha sido generado exitosamente."
   --DISPLAY "Ruta del archivo: ", v_ruta_nomarch
  
   --Genera una copia de la interface con el nombre corto
   --CALL fn_genera_copia_interface(v_ruta_nomarch,v_ruta_envio_dis)

END FUNCTION


FUNCTION fn_extractor_ent_financieras()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_aaaammdd               VARCHAR(08),  --Fecha del archivo de salida
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_bnd_totales            INTEGER 

   DEFINE v_encabezado       STRING,
          v_detalle          STRING,
          v_sumario          STRING
          
   DEFINE v_precio_fondo     DECIMAL(19,14)
   DEFINE i                  DECIMAL(9,0)

   DEFINE v_ind_liquidacion     SMALLINT
   DEFINE v_desc_destino        VARCHAR(30)
   DEFINE v_desc_edo_avance     VARCHAR(50)
   
   DEFINE v_arr_interface_ef DYNAMIC ARRAY OF RECORD
             nss                CHAR(11),
             --num_credito        DECIMAL(10,0),
             periodo_pago       CHAR(6),
             f_pago             DATE, 
             nrp                CHAR(11),
             monto_apo          DECIMAL(12,2),
             aivs               DECIMAL(18,6),
             folio_sua          DECIMAL(6,0),
             folio_liquida      DECIMAL(9,0),
             f_liquida          DATE,
             ind_liquidacion    VARCHAR(40),
             f_interface        DATE,
             tipo_interface     CHAR(02)
         END RECORD

         
   LET v_desc_destino = "ENTIDADES FINANCIERAS"
   LET v_aaaammdd  = TODAY USING "yyyymmdd"
   
   LET g_qrytxt = " SELECT ef.* ",
                  "\n FROM tmp_dis_ef ef",
                  "\n ORDER BY ef.ind_liquidacion DESC, ef.periodo_pago "

   LET v_nom_archivo       = "/dis_ext_ef_", v_aaaammdd,"_"
   --LET v_busca_nom_archivo = "dis_ext_pr_", v_aaaammdd,"_"

   PREPARE ps_dis_extractor_ef FROM g_qrytxt
   DECLARE cur_dis_extractor_ef CURSOR FOR ps_dis_extractor_ef
  
  --Se obtienen la ruta envio del modulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  --Obtiene consecutivo para archivo por día
  --CALL fn_crea_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  --RETURNING v_cont_dia
  
  --LET v_reg_dia  = v_cont_dia USING "&&&"


  LET v_nom_archivo  = v_nom_archivo CLIPPED ||p_folio_dis CLIPPED||"."|| v_modulo_cod
  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo

  --DISPLAY "v_nom_archivo: ", v_nom_archivo
  --DISPLAY "v_ruta_nomarch: ", v_ruta_nomarch

  LET v_ch_arch_salida = base.Channel.create()
   
  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

 --Imprime encabezado del archivo
   {LET v_encabezado = " FECHA:  ",TODAY
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_encabezado = " FOLIO DE DISPERSIÓN:  ",p_folio_dis
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_encabezado = " DESTINO:  ",v_desc_destino
   CALL v_ch_arch_salida.write([v_encabezado])

   --LET v_encabezado = " VALOR DE FONDO:  ",v_precio_fondo
   --CALL v_ch_arch_salida.write([v_encabezado])}


   LET v_encabezado = " NSS |PERIODO PAGO |FECHA PAGO |NRP |MONTO APORTACIÓN |AIVS |FOLIO SUA |FOLIO LIQUIDACIÓN |FECHA LIQUIDACIÓN |INDICADOR DE CONCILIACION | FECHA INTERFACE |TIPO INTERFACE"
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_tot_registros    = 0
   LET v_sum_aivs         = 0
   LET v_sum_aportacion   = 0
   LET v_sum_amortizacion = 0

   
   LET i = 1
   FOREACH cur_dis_extractor_ef INTO v_arr_interface_ef[i].nss, 
                                  --v_arr_interface_ef[i].num_credito,
                                  v_arr_interface_ef[i].periodo_pago,      
                                  v_arr_interface_ef[i].f_pago,
                                  v_arr_interface_ef[i].nrp,                
                                  v_arr_interface_ef[i].monto_apo,       
                                  v_arr_interface_ef[i].aivs,                 
                                  v_arr_interface_ef[i].folio_sua,          
                                  v_arr_interface_ef[i].folio_liquida,      
                                  v_arr_interface_ef[i].f_liquida,       
                                  v_ind_liquidacion,   
                                  v_arr_interface_ef[i].f_interface,     
                                  v_arr_interface_ef[i].tipo_interface  


      CASE v_ind_liquidacion
         WHEN 0
            LET v_arr_interface_ef[i].ind_liquidacion = "0 - SIN CONCILIAR"            
         WHEN 1
            LET v_arr_interface_ef[i].ind_liquidacion = "1 - CONCILIADO"            
         WHEN 2
            LET v_arr_interface_ef[i].ind_liquidacion = "2 - CONCILIADO CON DIFERENCIAS"            
         WHEN 100
            LET v_arr_interface_ef[i].ind_liquidacion = "100 - APO SUBS DUPLICADO"            
         OTHERWISE
         
      END CASE
      
      LET v_detalle = v_arr_interface_ef[i].nss USING "&&&&&&&&&&&", "|", 
                      --v_arr_interface_ef[i].num_credito USING "&&&&&&&&&&", "|",
                      v_arr_interface_ef[i].periodo_pago USING "&&&&&&", "|",      
                      v_arr_interface_ef[i].f_pago USING "yyyymmdd", "|",
                      v_arr_interface_ef[i].nrp, "|",                 
                      (v_arr_interface_ef[i].monto_apo * 100) USING "&&&&&&&&&&&&", "|",          
                      (v_arr_interface_ef[i].aivs * 100) USING "&&&&&&&&&&&&&&&&&&", "|",               
                      v_arr_interface_ef[i].folio_sua USING "&&&&&&", "|",          
                      v_arr_interface_ef[i].folio_liquida , "|",      
                      v_arr_interface_ef[i].f_liquida USING "yyyymmdd", "|",          
                      v_arr_interface_ef[i].ind_liquidacion, "|",     
                      v_arr_interface_ef[i].f_interface USING "yyyymmdd", "|",        
                      v_arr_interface_ef[i].tipo_interface  
                                  
      CALL v_ch_arch_salida.write([v_detalle])

      
      LET v_tot_registros    = v_tot_registros    + 1
      LET v_sum_aivs         = v_sum_aivs         + v_arr_interface_ef[i].aivs
      LET v_sum_aportacion   = v_sum_aportacion   + v_arr_interface_ef[i].monto_apo
      --LET v_sum_amortizacion = v_sum_amortizacion + v_arr_interface_ef[i].monto_amo
      LET i = i + 1
   END FOREACH

   LET i = i - 1
   CALL v_arr_interface_ef.deleteElement(v_arr_interface_ef.getLength())
   LET v_tot_registros = i
   
   --Escribe el sumario
   {LET v_sumario = "TOTALES|",v_tot_registros USING "&&&&&&&&&","| | | | ",
                              v_sum_aportacion USING "&&&&&&&&&", "|",
                              v_sum_aivs USING "&&&&&&&&&", "|",
                              v_sum_amortizacion USING "&&&&&&&&&"
                  
   CALL v_ch_arch_salida.write([v_sumario])}

   --DISPLAY "\nSe ha enviado la interface del extractor de dispersión de "|| v_desc_destino||"\n"

   --Cierra el archivo
   CALL v_ch_arch_salida.close()
   LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
   RUN v_comando_dos

   --Despliega información en el log
   DISPLAY "\n ############### INTERFACE ##################"
   DISPLAY " ############## "||v_desc_destino||  " ###############"
   DISPLAY ""
   DISPLAY "Nombre del archivo             : ", v_nom_archivo
   DISPLAY "Folio Dispersión               : ", p_folio_dis
   DISPLAY "Suma Monto Aportación          : ", v_sum_aportacion
   --DISPLAY "Suma Monto Amortización        : ", v_sum_amortizacion
   DISPLAY "Suma Monto AIVS                : ", v_sum_aivs
   DISPLAY "\n ########################################################"

   LET g_mensaje = "Se ha generado el archivo de la interface del extractor de dispersión de "||v_desc_destino|| 
                   "\nen la ruta: "||v_ruta_nomarch
                   
   --CALL fn_mensaje("Información",g_mensaje,"information")
   DISPLAY g_mensaje

   --DISPLAY ""
   --DISPLAY "El archivo ha sido generado exitosamente."
   --DISPLAY "Ruta del archivo: ", v_ruta_nomarch
  
   --Genera una copia de la interface con el nombre corto
   --CALL fn_genera_copia_interface(v_ruta_nomarch,v_ruta_envio_dis)

END FUNCTION



FUNCTION fn_extractor_inconsistencias()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_aaaammdd               VARCHAR(08),  --Fecha del archivo de salida
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_bnd_totales            INTEGER 

   DEFINE v_encabezado       STRING,
          v_detalle          STRING,
          v_sumario          STRING
          
   DEFINE v_precio_fondo     DECIMAL(19,14)
   DEFINE i                  DECIMAL(9,0)

   DEFINE v_desc_destino        VARCHAR(30)

   DEFINE v_destino_ap_viv       CHAR(01)
   DEFINE v_tpo_originacion      SMALLINT
   DEFINE v_desc_originacion     VARCHAR(30)
   
   DEFINE v_tpo_inconsistencia   SMALLINT 
   
   DEFINE v_arr_interface_inconsis DYNAMIC ARRAY OF RECORD
             nss                CHAR(11),
             num_credito        DECIMAL(10,0),
             periodo_pago       CHAR(6),
             f_pago             DATE, 
             nrp                CHAR(11),
             monto_apo          DECIMAL(12,2),
             aivs               DECIMAL(18,6),
             monto_amo          DECIMAL(12,2),
             folio_sua          DECIMAL(6,0),
             folio_liquida      DECIMAL(9,0),
             f_liquida          DATE,
             destino_ap_viv     VARCHAR(20),
             tipo_originacion   VARCHAR(30),
             tipo_inconsis      VARCHAR(40),
             f_interface        DATE,
             tipo_interface     CHAR(02)
         END RECORD

         
   LET v_desc_destino = "INCONSISTENCIAS"
   LET v_aaaammdd  = TODAY USING "yyyymmdd"
   
   LET g_qrytxt = " SELECT inc.* ",
                  "\n FROM tmp_dis_in inc",
                  "\n ORDER BY inc.tpo_inconsistente DESC, inc.periodo_pago "

   LET v_nom_archivo       = "/dis_ext_in_", v_aaaammdd,"_"
   --LET v_busca_nom_archivo = "dis_ext_pr_", v_aaaammdd,"_"

   PREPARE ps_dis_extractor_in FROM g_qrytxt
   DECLARE cur_dis_extractor_in CURSOR FOR ps_dis_extractor_in
  
  --Se obtienen la ruta envio del modulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  --Obtiene consecutivo para archivo por día
  --CALL fn_crea_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  --RETURNING v_cont_dia
  
  --LET v_reg_dia  = v_cont_dia USING "&&&"


  LET v_nom_archivo  = v_nom_archivo CLIPPED ||p_folio_dis CLIPPED||"."|| v_modulo_cod
  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo

  --DISPLAY "v_nom_archivo: ", v_nom_archivo
  --DISPLAY "v_ruta_nomarch: ", v_ruta_nomarch

  LET v_ch_arch_salida = base.Channel.create()
   
  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

 --Imprime encabezado del archivo
   {LET v_encabezado = " FECHA:  ",TODAY
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_encabezado = " FOLIO DE DISPERSIÓN:  ",p_folio_dis
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_encabezado = " DESTINO:  ",v_desc_destino
   CALL v_ch_arch_salida.write([v_encabezado])

   --LET v_encabezado = " VALOR DE FONDO:  ",v_precio_fondo
   --CALL v_ch_arch_salida.write([v_encabezado])}


   LET v_encabezado = " NSS |NÚMERO DE CREDITO |PERIODO PAGO |FECHA PAGO |NRP |MONTO APORTACIÓN |AIVS |MONTO AMORTIZACIÓN |FOLIO SUA |FOLIO LIQUIDACIÓN |FECHA LIQUIDACIÓN |DESTINO APORTACIÓN |TIPO ORIGINACIÓN |TIPO INCONSISTENCIA | FECHA INTERFACE |TIPO INTERFACE"
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_tot_registros    = 0
   LET v_sum_aivs         = 0
   LET v_sum_aportacion   = 0
   LET v_sum_amortizacion = 0

   
   LET i = 1
   FOREACH cur_dis_extractor_in INTO v_arr_interface_inconsis[i].nss, 
                                     v_arr_interface_inconsis[i].num_credito,
                                     v_arr_interface_inconsis[i].periodo_pago,      
                                     v_arr_interface_inconsis[i].f_pago,
                                     v_arr_interface_inconsis[i].nrp,                
                                     v_arr_interface_inconsis[i].monto_apo,       
                                     v_arr_interface_inconsis[i].aivs,
                                     v_arr_interface_inconsis[i].monto_amo,                 
                                     v_arr_interface_inconsis[i].folio_sua,          
                                     v_arr_interface_inconsis[i].folio_liquida,      
                                     v_arr_interface_inconsis[i].f_liquida,       
                                     v_destino_ap_viv,
                                     v_tpo_originacion, 
                                     v_tpo_inconsistencia, 
                                     v_arr_interface_inconsis[i].f_interface,     
                                     v_arr_interface_inconsis[i].tipo_interface  
     
      CASE v_destino_ap_viv
         WHEN "1"
            LET v_arr_interface_inconsis[i].destino_ap_viv = "1 - INFONAVIT"            
         WHEN "2"
            LET v_arr_interface_inconsis[i].destino_ap_viv = "2 - SAFRE" 
        WHEN "3" 
            LET v_arr_interface_inconsis[i].destino_ap_viv = "3 - PORTABILIDAD" 
         OTHERWISE
            LET v_arr_interface_inconsis[i].destino_ap_viv = "4 - SIN DESTINO APORTACIÓN"    
      END CASE

      IF v_tpo_originacion = 0 THEN
         LET v_arr_interface_inconsis[i].tipo_originacion = v_tpo_originacion CLIPPED||" - SIN CRÉDITO"            
      ELSE

         SELECT originacion_desc
         INTO v_desc_originacion
         FROM cat_cre_originacion
         WHERE tpo_originacion = v_tpo_originacion

         LET v_arr_interface_inconsis[i].tipo_originacion = v_tpo_originacion CLIPPED||" - "||v_desc_originacion CLIPPED

      END IF


      CASE v_tpo_inconsistencia
         WHEN 0
            LET v_arr_interface_inconsis[i].tipo_inconsis = "0 - SIN NÚMERO DE CRÉDITO"  
         WHEN 1
            LET v_arr_interface_inconsis[i].tipo_inconsis = "1 - DEST APO INFO-CRÉD 43 BIS"  
         WHEN 2
            LET v_arr_interface_inconsis[i].tipo_inconsis = "2 - ACLARATORIO SIN DESTINO"
         WHEN 3
            LET v_arr_interface_inconsis[i].tipo_inconsis = "3 - ACLARATORIO AFO SIN MARCA CONF"
         WHEN 10
            LET v_arr_interface_inconsis[i].tipo_inconsis = "10 - LIQUIDADO"
         OTHERWISE

      END CASE
      
      LET v_detalle = v_arr_interface_inconsis[i].nss USING "&&&&&&&&&&&", "|", 
                      v_arr_interface_inconsis[i].num_credito , "|",
                      v_arr_interface_inconsis[i].periodo_pago USING "&&&&&&", "|",      
                      v_arr_interface_inconsis[i].f_pago USING "yyyymmdd", "|",
                      v_arr_interface_inconsis[i].nrp, "|",                 
                      (v_arr_interface_inconsis[i].monto_apo * 100) USING "&&&&&&&&&&&&", "|",          
                      (v_arr_interface_inconsis[i].aivs * 100) USING "&&&&&&&&&&&&&&&&&&", "|", 
                      (v_arr_interface_inconsis[i].monto_amo * 100) USING "&&&&&&&&&&&&", "|",              
                      v_arr_interface_inconsis[i].folio_sua USING "&&&&&&", "|",          
                      v_arr_interface_inconsis[i].folio_liquida , "|",      
                      v_arr_interface_inconsis[i].f_liquida USING "yyyymmdd", "|",          
                      v_arr_interface_inconsis[i].destino_ap_viv, "|",   
                      v_arr_interface_inconsis[i].tipo_originacion, "|",
                      v_arr_interface_inconsis[i].tipo_inconsis, "|",
                      v_arr_interface_inconsis[i].f_interface USING "yyyymmdd", "|",        
                      v_arr_interface_inconsis[i].tipo_interface  
                                  
      CALL v_ch_arch_salida.write([v_detalle])

      
      LET v_tot_registros    = v_tot_registros    + 1
      LET v_sum_aivs         = v_sum_aivs         + v_arr_interface_inconsis[i].aivs
      LET v_sum_aportacion   = v_sum_aportacion   + v_arr_interface_inconsis[i].monto_apo
      LET v_sum_amortizacion = v_sum_amortizacion + v_arr_interface_inconsis[i].monto_amo
      LET i = i + 1
   END FOREACH

   LET i = i - 1
   CALL v_arr_interface_inconsis.deleteElement(v_arr_interface_inconsis.getLength())
   LET v_tot_registros = i
   
   --Escribe el sumario
   {LET v_sumario = "TOTALES|",v_tot_registros USING "&&&&&&&&&","| | | | ",
                              v_sum_aportacion USING "&&&&&&&&&", "|",
                              v_sum_aivs USING "&&&&&&&&&", "|",
                              v_sum_amortizacion USING "&&&&&&&&&"
                  
   CALL v_ch_arch_salida.write([v_sumario])}

   --DISPLAY "\nSe ha enviado la interface del extractor de dispersión de "|| v_desc_destino||"\n"
  
   --Cierra el archivo
   CALL v_ch_arch_salida.close()
   LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
   RUN v_comando_dos

   --Despliega información en el log
   DISPLAY "\n ############### INTERFACE ##################"
   DISPLAY " ############## "||v_desc_destino||  " ###############"
   DISPLAY ""
   DISPLAY "Nombre del archivo             : ", v_nom_archivo
   DISPLAY "Folio Dispersión               : ", p_folio_dis
   DISPLAY "Suma Monto Aportación          : ", v_sum_aportacion
   DISPLAY "Suma Monto AIVS                : ", v_sum_aivs
   DISPLAY "Suma Monto Amortización        : ", v_sum_amortizacion
   DISPLAY "\n ########################################################"

   LET g_mensaje = "Se ha generado el archivo de la interface del extractor de dispersión de "||v_desc_destino|| 
                   "\nen la ruta: "||v_ruta_nomarch
                   
   --CALL fn_mensaje("Información",g_mensaje,"information")
   DISPLAY g_mensaje

   --DISPLAY ""
   --DISPLAY "El archivo ha sido generado exitosamente."
   --DISPLAY "Ruta del archivo: ", v_ruta_nomarch
  
   --Genera una copia de la interface con el nombre corto
   --CALL fn_genera_copia_interface(v_ruta_nomarch,v_ruta_envio_dis)

END FUNCTION



FUNCTION fn_extractor_apo_subsecuentes()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_aaaammdd               VARCHAR(08),  --Fecha del archivo de salida
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_bnd_totales            INTEGER 

   DEFINE v_encabezado       STRING,
          v_detalle          STRING,
          v_sumario          STRING
          
   DEFINE v_precio_fondo     DECIMAL(19,14)
   DEFINE i                  DECIMAL(9,0)

   DEFINE v_desc_destino        VARCHAR(30)
   DEFINE v_ind_concilia        SMALLINT
   DEFINE v_estado              SMALLINT
   DEFINE v_desc_edo            VARCHAR(50)
   
   DEFINE v_arr_apo_subsecuentes DYNAMIC ARRAY OF RECORD
             nss                CHAR(11),
             --num_credito        DECIMAL(10,0),
             periodo_pago       CHAR(6),
             f_pago             DATE, 
             nrp                CHAR(11),
             monto_apo          DECIMAL(12,2),
             aivs               DECIMAL(18,6),
             monto_amo          DECIMAL(12,2),
             folio_sua          DECIMAL(6,0),
             folio_archivo      DECIMAL(9,0),
             folio_liquida      DECIMAL(9,0),
             f_liquida          DATE,
             ind_concilia       VARCHAR(40),
             estado             VARCHAR(50),
             f_interface        DATE,
             tipo_interface     CHAR(02)
         END RECORD

         
   LET v_desc_destino = "APORTACIONES SUBSECUENTES"
   LET v_aaaammdd  = TODAY USING "yyyymmdd"
   
   LET g_qrytxt = " SELECT aps.* ",
                  "\n FROM tmp_dis_as aps",
                  "\n ORDER BY aps.ind_concilia DESC, aps.periodo_pago "

   LET v_nom_archivo       = "/dis_ext_as_", v_aaaammdd,"_"
   --LET v_busca_nom_archivo = "dis_ext_pr_", v_aaaammdd,"_"

   PREPARE ps_dis_extractor_as FROM g_qrytxt
   DECLARE cur_dis_extractor_as CURSOR FOR ps_dis_extractor_as
  
  --Se obtienen la ruta envio del modulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  --Obtiene consecutivo para archivo por día
  --CALL fn_crea_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  --RETURNING v_cont_dia
  
  --LET v_reg_dia  = v_cont_dia USING "&&&"


  LET v_nom_archivo  = v_nom_archivo CLIPPED ||p_folio_dis CLIPPED||"."|| v_modulo_cod
  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo

  --DISPLAY "v_nom_archivo: ", v_nom_archivo
  --DISPLAY "v_ruta_nomarch: ", v_ruta_nomarch

  LET v_ch_arch_salida = base.Channel.create()
   
  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

 --Imprime encabezado del archivo
   {LET v_encabezado = " FECHA:  ",TODAY
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_encabezado = " FOLIO DE DISPERSIÓN:  ",p_folio_dis
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_encabezado = " DESTINO:  ",v_desc_destino
   CALL v_ch_arch_salida.write([v_encabezado])

   --LET v_encabezado = " VALOR DE FONDO:  ",v_precio_fondo
   --CALL v_ch_arch_salida.write([v_encabezado])}


   LET v_encabezado = " NSS |PERIODO PAGO |FECHA PAGO |NRP |MONTO APORTACIÓN |AIVS | MONTO AMORTIZACIÓN |FOLIO SUA |FOLIO ARCHIVO |FOLIO LIQUIDACIÓN |FECHA LIQUIDACIÓN |INDICADOR CONCILIACIÓN |ESTADO | FECHA INTERFACE |TIPO INTERFACE"
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_tot_registros    = 0
   LET v_sum_aivs         = 0
   LET v_sum_aportacion   = 0
   LET v_sum_amortizacion = 0

   
   LET i = 1
   FOREACH cur_dis_extractor_as INTO v_arr_apo_subsecuentes[i].nss, 
                                     --v_arr_apo_subsecuentes[i].num_credito,
                                     v_arr_apo_subsecuentes[i].periodo_pago,      
                                     v_arr_apo_subsecuentes[i].f_pago,
                                     v_arr_apo_subsecuentes[i].nrp,                
                                     v_arr_apo_subsecuentes[i].monto_apo,       
                                     v_arr_apo_subsecuentes[i].aivs, 
                                     v_arr_apo_subsecuentes[i].monto_amo,              
                                     v_arr_apo_subsecuentes[i].folio_sua,
                                     v_arr_apo_subsecuentes[i].folio_archivo, 
                                     v_arr_apo_subsecuentes[i].folio_liquida,      
                                     v_arr_apo_subsecuentes[i].f_liquida,       
                                     v_ind_concilia,
                                     v_estado,   
                                     v_arr_apo_subsecuentes[i].f_interface,     
                                     v_arr_apo_subsecuentes[i].tipo_interface  


      CASE v_ind_concilia
         WHEN 0
            LET v_arr_apo_subsecuentes[i].ind_concilia = "0 - SIN CONCILIAR"            
         WHEN 1
            LET v_arr_apo_subsecuentes[i].ind_concilia = "1 - CONCILIADO" 
         WHEN 2
            LET v_arr_apo_subsecuentes[i].ind_concilia = "2 - CONCILIADO CON DIFERENCIAS"
         WHEN 100
            LET v_arr_apo_subsecuentes[i].ind_concilia = "100 - APO SUBS DUPLICADO"
         OTHERWISE
         
      END CASE

      IF v_estado = 0 THEN
         LET v_arr_apo_subsecuentes[i].estado = v_estado CLIPPED||" - ESTADO DESCONOCIDO"            
      ELSE

         SELECT desc_edo_apo_sub
         INTO v_desc_edo
         FROM cat_edo_ap_subsecuente
         WHERE cod_edo_apo_sub = v_estado

         LET v_arr_apo_subsecuentes[i].estado = v_estado CLIPPED||" - "||v_desc_edo CLIPPED

      END IF
      
      LET v_detalle = v_arr_apo_subsecuentes[i].nss USING "&&&&&&&&&&&", "|", 
                      --v_arr_apo_subsecuentes[i].num_credito USING "&&&&&&&&&&", "|",
                      v_arr_apo_subsecuentes[i].periodo_pago USING "&&&&&&", "|",      
                      v_arr_apo_subsecuentes[i].f_pago USING "yyyymmdd", "|",
                      v_arr_apo_subsecuentes[i].nrp, "|",                 
                      (v_arr_apo_subsecuentes[i].monto_apo * 100) USING "&&&&&&&&&&&&", "|",          
                      (v_arr_apo_subsecuentes[i].aivs * 100) USING "&&&&&&&&&&&&&&&&&&", "|", 
                      (v_arr_apo_subsecuentes[i].monto_amo * 100) USING "&&&&&&&&&&&&", "|",                        
                      --v_arr_apo_subsecuentes[i].folio_sua USING "&&&&&&", "|",
                      v_arr_apo_subsecuentes[i].folio_sua USING "&&&&&&", "|",    
                      v_arr_apo_subsecuentes[i].folio_archivo, "|",        
                      v_arr_apo_subsecuentes[i].folio_liquida , "|",      
                      v_arr_apo_subsecuentes[i].f_liquida USING "yyyymmdd", "|",          
                      v_arr_apo_subsecuentes[i].ind_concilia, "|",  
                      v_arr_apo_subsecuentes[i].estado, "|",   
                      v_arr_apo_subsecuentes[i].f_interface USING "yyyymmdd", "|",        
                      v_arr_apo_subsecuentes[i].tipo_interface  
                                  
      CALL v_ch_arch_salida.write([v_detalle])

      
      LET v_tot_registros    = v_tot_registros    + 1
      LET v_sum_aportacion   = v_sum_aportacion   + v_arr_apo_subsecuentes[i].monto_apo
      LET v_sum_aivs         = v_sum_aivs         + v_arr_apo_subsecuentes[i].aivs
      LET v_sum_amortizacion = v_sum_amortizacion + v_arr_apo_subsecuentes[i].monto_amo
      {DISPLAY ""
      DISPLAY i
      DISPLAY "APORTACIONES: ", v_sum_aportacion
      DISPLAY "AIVS: ", v_sum_aivs
      DISPLAY "AMORTIZACIONES: ", v_sum_amortizacion}
      
      LET i = i + 1
   END FOREACH

   LET i = i - 1
   CALL v_arr_apo_subsecuentes.deleteElement(v_arr_apo_subsecuentes.getLength())
   LET v_tot_registros = i
   
   --Escribe el sumario
   {LET v_sumario = "TOTALES|",v_tot_registros USING "&&&&&&&&&","| | | | ",
                              v_sum_aportacion USING "&&&&&&&&&", "|",
                              v_sum_aivs USING "&&&&&&&&&", "|",
                              v_sum_amortizacion USING "&&&&&&&&&"
                  
   CALL v_ch_arch_salida.write([v_sumario])}

   --DISPLAY "\nSe ha enviado la interface del extractor de dispersión de "|| v_desc_destino||"\n"
  
   --Cierra el archivo
   CALL v_ch_arch_salida.close()
   LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
   RUN v_comando_dos

   --Despliega información en el log
   DISPLAY "\n ############### INTERFACE ##################"
   DISPLAY " ############## "||v_desc_destino||  " ###############"
   DISPLAY ""
   DISPLAY "Nombre del archivo             : ", v_nom_archivo
   DISPLAY "Folio Dispersión               : ", p_folio_dis
   DISPLAY "Suma Monto Aportación          : ", v_sum_aportacion
   DISPLAY "Suma Monto AIVS                : ", v_sum_aivs
   DISPLAY "Suma Monto Amortización        : ", v_sum_amortizacion
   DISPLAY "\n ########################################################"
   
   LET g_mensaje = "Se ha generado el archivo de la interface del extractor de dispersión de "||v_desc_destino|| 
                   "\nen la ruta: "||v_ruta_nomarch
                   
   --CALL fn_mensaje("Información",g_mensaje,"information")
   DISPLAY g_mensaje

   --DISPLAY ""
   --DISPLAY "El archivo ha sido generado exitosamente."
   --DISPLAY "Ruta del archivo: ", v_ruta_nomarch
  
   --Genera una copia de la interface con el nombre corto
   --CALL fn_genera_copia_interface(v_ruta_nomarch,v_ruta_envio_dis)

END FUNCTION


FUNCTION fn_extractor_portabilidad()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_aaaammdd               VARCHAR(08),  --Fecha del archivo de salida
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_bnd_totales            INTEGER 

   DEFINE v_encabezado       STRING,
          v_detalle          STRING,
          v_sumario          STRING
          
   DEFINE v_precio_fondo     DECIMAL(19,14)
   DEFINE i                  DECIMAL(9,0)

   DEFINE v_tipo_prt            SMALLINT
   DEFINE v_desc_destino        VARCHAR(30)
   DEFINE v_desc_edo_avance     VARCHAR(50)
   
   DEFINE v_arr_interface_pt DYNAMIC ARRAY OF RECORD
             nss                CHAR(11),
             num_credito        DECIMAL(10,0),
             periodo_pago       CHAR(6),
             f_pago             DATE, 
             nrp                CHAR(11),
             monto_apo          DECIMAL(12,2),
             aivs               DECIMAL(18,6),
             monto_amo          DECIMAL(12,2),
             folio_sua          DECIMAL(6,0),
             folio_liquida      DECIMAL(9,0),
             f_liquida          DATE,
             tipo_prt           VARCHAR(20),
             f_interface        DATE,
             tipo_interface     CHAR(02)
         END RECORD

         
   LET v_desc_destino = "PORTABILIDAD"
   LET v_aaaammdd  = TODAY USING "yyyymmdd"
   
   LET g_qrytxt = " SELECT pt.* ",
                  "\n FROM tmp_dis_pt pt",
                  "\n ORDER BY pt.tipo_prt DESC, pt.periodo_pago "

   LET v_nom_archivo       = "/dis_ext_pt_", v_aaaammdd,"_"
   --LET v_busca_nom_archivo = "dis_ext_pr_", v_aaaammdd,"_"

   PREPARE ps_dis_extractor_pt FROM g_qrytxt
   DECLARE cur_dis_extractor_pt CURSOR FOR ps_dis_extractor_pt
  
  --Se obtienen la ruta envio del modulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  --Obtiene consecutivo para archivo por día
  --CALL fn_crea_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  --RETURNING v_cont_dia
  
  --LET v_reg_dia  = v_cont_dia USING "&&&"


  LET v_nom_archivo  = v_nom_archivo CLIPPED ||p_folio_dis CLIPPED||"."|| v_modulo_cod
  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo

  --DISPLAY "v_nom_archivo: ", v_nom_archivo
  --DISPLAY "v_ruta_nomarch: ", v_ruta_nomarch

  LET v_ch_arch_salida = base.Channel.create()
   
  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

 --Imprime encabezado del archivo
   {LET v_encabezado = " FECHA:  ",TODAY
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_encabezado = " FOLIO DE DISPERSIÓN:  ",p_folio_dis
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_encabezado = " DESTINO:  ",v_desc_destino
   CALL v_ch_arch_salida.write([v_encabezado])

   --LET v_encabezado = " VALOR DE FONDO:  ",v_precio_fondo
   --CALL v_ch_arch_salida.write([v_encabezado])}


   LET v_encabezado = " NSS |NÚMERO DE CREDITO |PERIODO PAGO |FECHA PAGO |NRP |MONTO APORTACIÓN |AIVS | MONTO AMORTIZACIÓN |FOLIO SUA |FOLIO LIQUIDACIÓN |FECHA LIQUIDACIÓN |TIPO PORTABILIDAD | FECHA INTERFACE |TIPO INTERFACE"
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_tot_registros    = 0
   LET v_sum_aivs         = 0
   LET v_sum_aportacion   = 0
   LET v_sum_amortizacion = 0

   
   LET i = 1
   FOREACH cur_dis_extractor_pt INTO v_arr_interface_pt[i].nss, 
                                     v_arr_interface_pt[i].num_credito,
                                     v_arr_interface_pt[i].periodo_pago,      
                                     v_arr_interface_pt[i].f_pago,
                                     v_arr_interface_pt[i].nrp,                
                                     v_arr_interface_pt[i].monto_apo,       
                                     v_arr_interface_pt[i].aivs, 
                                     v_arr_interface_pt[i].monto_amo,              
                                     v_arr_interface_pt[i].folio_sua,          
                                     v_arr_interface_pt[i].folio_liquida,      
                                     v_arr_interface_pt[i].f_liquida,       
                                     v_tipo_prt,   
                                     v_arr_interface_pt[i].f_interface,     
                                     v_arr_interface_pt[i].tipo_interface  


      CASE v_tipo_prt
         WHEN 1
            LET v_arr_interface_pt[i].tipo_prt = "1 - CEDENTE"            
         WHEN 2
            LET v_arr_interface_pt[i].tipo_prt = "2 - RECEPTORA"                     
         OTHERWISE
         
      END CASE
      
      LET v_detalle = v_arr_interface_pt[i].nss USING "&&&&&&&&&&&", "|", 
                      v_arr_interface_pt[i].num_credito, "|",
                      v_arr_interface_pt[i].periodo_pago USING "&&&&&&", "|",      
                      v_arr_interface_pt[i].f_pago USING "yyyymmdd", "|",
                      v_arr_interface_pt[i].nrp, "|",                 
                      (v_arr_interface_pt[i].monto_apo * 100) USING "&&&&&&&&&&&&", "|",          
                      (v_arr_interface_pt[i].aivs * 100) USING "&&&&&&&&&&&&&&&&&&", "|", 
                      (v_arr_interface_pt[i].monto_amo * 100) USING "&&&&&&&&&&&&", "|",                        
                      v_arr_interface_pt[i].folio_sua USING "&&&&&&", "|",          
                      v_arr_interface_pt[i].folio_liquida, "|",      
                      v_arr_interface_pt[i].f_liquida USING "yyyymmdd", "|",          
                      v_arr_interface_pt[i].tipo_prt, "|",     
                      v_arr_interface_pt[i].f_interface USING "yyyymmdd", "|",        
                      v_arr_interface_pt[i].tipo_interface  
                                  
      CALL v_ch_arch_salida.write([v_detalle])

      
      LET v_tot_registros    = v_tot_registros    + 1
      LET v_sum_aportacion   = v_sum_aportacion   + v_arr_interface_pt[i].monto_apo
      LET v_sum_aivs         = v_sum_aivs         + v_arr_interface_pt[i].aivs
      LET v_sum_amortizacion = v_sum_amortizacion + v_arr_interface_pt[i].monto_amo
      DISPLAY ""
      DISPLAY i
      DISPLAY "APORTACIONES: ", v_sum_aportacion
      DISPLAY "AIVS: ", v_sum_aivs
      DISPLAY "AMORTIZACIONES: ", v_sum_amortizacion
      
      LET i = i + 1
   END FOREACH

   LET i = i - 1
   CALL v_arr_interface_pt.deleteElement(v_arr_interface_pt.getLength())
   LET v_tot_registros = i
   
   --Escribe el sumario
   {LET v_sumario = "TOTALES|",v_tot_registros USING "&&&&&&&&&","| | | | ",
                              v_sum_aportacion USING "&&&&&&&&&", "|",
                              v_sum_aivs USING "&&&&&&&&&", "|",
                              v_sum_amortizacion USING "&&&&&&&&&"
                  
   CALL v_ch_arch_salida.write([v_sumario])}

   --DISPLAY "\nSe ha enviado la interface del extractor de dispersión de "|| v_desc_destino||"\n"

   --Cierra el archivo
   CALL v_ch_arch_salida.close()
   LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
   RUN v_comando_dos

   --Despliega información en el log
   DISPLAY "\n ############### INTERFACE ##################"
   DISPLAY " ############## "||v_desc_destino||  " ###############"
   DISPLAY ""
   DISPLAY "Nombre del archivo             : ", v_nom_archivo
   DISPLAY "Folio Dispersión               : ", p_folio_dis
   DISPLAY "Suma Monto Aportación          : ", v_sum_aportacion
   DISPLAY "Suma Monto AIVS                : ", v_sum_aivs
   DISPLAY "Suma Monto Amortización        : ", v_sum_amortizacion
   DISPLAY "\n ########################################################"

   
   LET g_mensaje = "Se ha generado el archivo de la interface del extractor de dispersión de "||v_desc_destino|| 
                   "\nen la ruta: "||v_ruta_nomarch
                   
   --CALL fn_mensaje("Información",g_mensaje,"information")
   DISPLAY g_mensaje

   --DISPLAY ""
   --DISPLAY "El archivo ha sido generado exitosamente."
   --DISPLAY "Ruta del archivo: ", v_ruta_nomarch
  
   --Genera una copia de la interface con el nombre corto
   --CALL fn_genera_copia_interface(v_ruta_nomarch,v_ruta_envio_dis)

END FUNCTION



FUNCTION fn_extractor_avances_comp()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_aaaammdd               VARCHAR(08),  --Fecha del archivo de salida
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_bnd_totales            INTEGER 

   DEFINE v_encabezado       STRING,
          v_detalle          STRING,
          v_sumario          STRING
          
   DEFINE v_precio_fondo     DECIMAL(19,14)
   DEFINE i                  DECIMAL(9,0)

   DEFINE v_tipo_prt            SMALLINT
   DEFINE v_desc_destino        VARCHAR(30)
   DEFINE v_desc_edo_avance     VARCHAR(50)
   
   DEFINE v_arr_interface_ca DYNAMIC ARRAY OF RECORD
             nss                CHAR(11),
             num_credito        DECIMAL(10,0),
             periodo_pago       CHAR(6),
             f_pago             DATE, 
             nrp                CHAR(11),
             monto_apo_ava      DECIMAL(12,2),
             monto_amo_ava      DECIMAL(12,2),
             estado_avance      VARCHAR(55),
             monto_apo_pag      DECIMAL(12,2),
             monto_amo_pag      DECIMAL(12,2),
             monto_dif_apo      DECIMAL(12,2),
             monto_dif_amo      DECIMAL(12,2),
             estado_comp_apo    VARCHAR(45),
             estado_comp_amo    VARCHAR(45),
             folio_liquida      DECIMAL(9,0),
             f_liquida          DATE,
             f_interface        DATE,
             tipo_interface     CHAR(02)
         END RECORD

   DEFINE v_edo_avance        SMALLINT
   DEFINE v_edo_comp_apo      SMALLINT
   DEFINE v_edo_comp_amo      SMALLINT
         
   LET v_desc_destino = "AVANCES COMPENSADOS"
   LET v_aaaammdd  = TODAY USING "yyyymmdd"
   
   LET g_qrytxt = " SELECT ca.* ",
                  "\n FROM tmp_dis_ca ca",
                  "\n ORDER BY ca.periodo_pago "

   LET v_nom_archivo       = "/dis_ext_ca_", v_aaaammdd,"_"
   --LET v_busca_nom_archivo = "dis_ext_pr_", v_aaaammdd,"_"

   PREPARE ps_dis_extractor_ca FROM g_qrytxt
   DECLARE cur_dis_extractor_ca CURSOR FOR ps_dis_extractor_ca
  
  --Se obtienen la ruta envio del modulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  --Obtiene consecutivo para archivo por día
  --CALL fn_crea_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  --RETURNING v_cont_dia
  
  --LET v_reg_dia  = v_cont_dia USING "&&&"


  LET v_nom_archivo  = v_nom_archivo CLIPPED ||p_folio_dis CLIPPED||"."|| v_modulo_cod
  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo

  --DISPLAY "v_nom_archivo: ", v_nom_archivo
  --DISPLAY "v_ruta_nomarch: ", v_ruta_nomarch

  LET v_ch_arch_salida = base.Channel.create()
   
  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

 --Imprime encabezado del archivo
   {LET v_encabezado = " FECHA:  ",TODAY
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_encabezado = " FOLIO DE DISPERSIÓN:  ",p_folio_dis
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_encabezado = " DESTINO:  ",v_desc_destino
   CALL v_ch_arch_salida.write([v_encabezado])

   --LET v_encabezado = " VALOR DE FONDO:  ",v_precio_fondo
   --CALL v_ch_arch_salida.write([v_encabezado])}


   LET v_encabezado = " NSS |NÚMERO DE CREDITO |PERIODO PAGO |FECHA PAGO |NRP |APORTACIÓN AVANCE |AMORTIZACIÓN AVANCE | ESTADO AVANCE |APORTACIÓN PAGO |AMORTIZACIÓN PAGO |APORTACIÓN DIFERENCIA |AMORTIZACIÓN DIFERENCIA |ESTADO APORTACIÓN DIFERENCIA |ESTADO AMORTIZACIÓN DIFERENCIA |FOLIO LIQUIDACIÓN |FECHA LIQUIDACIÓN |FECHA INTERFACE |TIPO INTERFACE"
   CALL v_ch_arch_salida.write([v_encabezado])

   LET v_tot_registros     = 0

   LET v_sum_apo_ava       = 0
   LET v_sum_amo_ava       = 0
   LET v_sum_apo_pag       = 0
   LET v_sum_amo_pag       = 0   
   LET v_sum_dif_apo       = 0   
   LET v_sum_dif_amo       = 0

   LET v_edo_avance        = 0
   LET v_edo_comp_apo      = 0
   LET v_edo_comp_amo      = 0

   
   LET i = 1
   FOREACH cur_dis_extractor_ca INTO v_arr_interface_ca[i].nss            ,         
                                     v_arr_interface_ca[i].num_credito    ,   
                                     v_arr_interface_ca[i].periodo_pago   ,  
                                     v_arr_interface_ca[i].f_pago         ,   
                                     v_arr_interface_ca[i].nrp            ,   
                                     v_arr_interface_ca[i].monto_apo_ava  ,  
                                     v_arr_interface_ca[i].monto_amo_ava  ,  
                                     v_edo_avance                         ,
                                     v_arr_interface_ca[i].monto_apo_pag  ,  
                                     v_arr_interface_ca[i].monto_amo_pag  ,  
                                     v_arr_interface_ca[i].monto_dif_apo  ,  
                                     v_arr_interface_ca[i].monto_dif_amo  ,  
                                     v_edo_comp_apo                       ,
                                     v_edo_comp_amo                       ,
                                     v_arr_interface_ca[i].folio_liquida  ,  
                                     v_arr_interface_ca[i].f_liquida      ,       
                                     v_arr_interface_ca[i].f_interface    ,  
                                     v_arr_interface_ca[i].tipo_interface 


      SELECT desc_edo_avance
      INTO  v_arr_interface_ca[i].estado_avance
      FROM cat_edo_avance_pago
      WHERE cod_edo_avance = v_edo_avance

      SELECT tpo_deudor_desc
      INTO  v_arr_interface_ca[i].estado_comp_apo
      FROM dis_cat_tpo_deudor
      WHERE tpo_deudor = v_edo_comp_apo

      SELECT tpo_deudor_desc
      INTO  v_arr_interface_ca[i].estado_comp_amo
      FROM dis_cat_tpo_deudor
      WHERE tpo_deudor = v_edo_comp_amo

      LET v_arr_interface_ca[i].estado_avance = v_edo_avance CLIPPED || " - "||v_arr_interface_ca[i].estado_avance
      LET v_arr_interface_ca[i].estado_comp_apo = v_edo_comp_apo CLIPPED || " - "||v_arr_interface_ca[i].estado_comp_apo
      LET v_arr_interface_ca[i].estado_comp_amo = v_edo_comp_amo CLIPPED || " - "||v_arr_interface_ca[i].estado_comp_amo
      
      LET v_detalle = v_arr_interface_ca[i].nss USING "&&&&&&&&&&&", "|", 
                      v_arr_interface_ca[i].num_credito, "|",
                      v_arr_interface_ca[i].periodo_pago USING "&&&&&&", "|",      
                      v_arr_interface_ca[i].f_pago USING "yyyymmdd", "|",
                      v_arr_interface_ca[i].nrp, "|",                 
                      (v_arr_interface_ca[i].monto_apo_ava * 100) USING "&&&&&&&&&&&&", "|",          
                      (v_arr_interface_ca[i].monto_amo_ava * 100) USING "&&&&&&&&&&&&", "|", 
                      v_arr_interface_ca[i].estado_avance, "|",
                      (v_arr_interface_ca[i].monto_apo_pag * 100) USING "&&&&&&&&&&&&", "|",
                      (v_arr_interface_ca[i].monto_amo_pag * 100) USING "&&&&&&&&&&&&", "|", 
                      (v_arr_interface_ca[i].monto_dif_apo * 100) USING "&&&&&&&&&&&&", "|", 
                      (v_arr_interface_ca[i].monto_dif_amo * 100) USING "&&&&&&&&&&&&", "|",
                      v_arr_interface_ca[i].estado_comp_apo, "|",
                      v_arr_interface_ca[i].estado_comp_amo, "|",  
                      v_arr_interface_ca[i].folio_liquida, "|",      
                      v_arr_interface_ca[i].f_liquida USING "yyyymmdd", "|",          
                      v_arr_interface_ca[i].f_interface USING "yyyymmdd", "|",        
                      v_arr_interface_ca[i].tipo_interface  
                                  
      CALL v_ch_arch_salida.write([v_detalle])

      
      LET v_tot_registros    = v_tot_registros    + 1

      LET v_sum_apo_ava = v_sum_apo_ava + v_arr_interface_ca[i].monto_apo_ava
      LET v_sum_amo_ava = v_sum_amo_ava + v_arr_interface_ca[i].monto_amo_ava
      LET v_sum_apo_pag = v_sum_apo_pag + v_arr_interface_ca[i].monto_apo_pag   
      LET v_sum_amo_pag = v_sum_amo_pag + v_arr_interface_ca[i].monto_amo_pag
      LET v_sum_dif_apo = v_sum_dif_apo + v_arr_interface_ca[i].monto_dif_apo
      LET v_sum_dif_amo = v_sum_dif_amo + v_arr_interface_ca[i].monto_dif_amo
      
      {DISPLAY ""
      DISPLAY i
      DISPLAY "v_sum_apo_ava: ", v_sum_apo_ava
      DISPLAY "v_sum_amo_ava: ", v_sum_amo_ava
      DISPLAY "v_sum_apo_pag: ", v_sum_apo_pag
      DISPLAY "v_sum_amo_pag: ", v_sum_amo_pag
      DISPLAY "v_sum_dif_apo: ", v_sum_dif_apo
      DISPLAY "v_sum_dif_amo: ", v_sum_dif_amo}
      
      LET i = i + 1
   END FOREACH

   LET i = i - 1
   CALL v_arr_interface_ca.deleteElement(v_arr_interface_ca.getLength())
   LET v_tot_registros = i
   
   --Escribe el sumario
   {LET v_sumario = "TOTALES|",v_tot_registros USING "&&&&&&&&&","| | | | ",
                              v_sum_aportacion USING "&&&&&&&&&", "|",
                              v_sum_aivs USING "&&&&&&&&&", "|",
                              v_sum_amortizacion USING "&&&&&&&&&"
                  
   CALL v_ch_arch_salida.write([v_sumario])}

   --DISPLAY "\nSe ha enviado la interface del extractor de dispersión de "|| v_desc_destino||"\n"

   --Cierra el archivo
   CALL v_ch_arch_salida.close()
   LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
   RUN v_comando_dos

   --Despliega información en el log
   DISPLAY "\n ############### INTERFACE ##################"
   DISPLAY " ############## "||v_desc_destino||  " ###############"
   DISPLAY ""
   DISPLAY "Nombre del archivo                  : ", v_nom_archivo
   DISPLAY "Folio Dispersión                    : ", p_folio_dis
   DISPLAY "Suma Monto Aportación Avance        : ", v_sum_apo_ava
   DISPLAY "Suma Monto Amortización Avance      : ", v_sum_amo_ava
   DISPLAY "Suma Monto Aportación Pago          : ", v_sum_apo_pag
   DISPLAY "Suma Monto Amortización Pago        : ", v_sum_amo_pag
   DISPLAY "Suma Monto Aportación Diferencia    : ", v_sum_dif_apo
   DISPLAY "Suma Monto Amortización Diferencia  : ", v_sum_dif_amo
   DISPLAY "\n ########################################################"

   
   LET g_mensaje = "Se ha generado el archivo de la interface del extractor de dispersión de "||v_desc_destino|| 
                   "\nen la ruta: "||v_ruta_nomarch
                   
   --CALL fn_mensaje("Información",g_mensaje,"information")
   DISPLAY g_mensaje

   --DISPLAY ""
   --DISPLAY "El archivo ha sido generado exitosamente."
   --DISPLAY "Ruta del archivo: ", v_ruta_nomarch
  
   --Genera una copia de la interface con el nombre corto
   --CALL fn_genera_copia_interface(v_ruta_nomarch,v_ruta_envio_dis)

END FUNCTION


#Objetivo: genera el número consecutivo por día para el archivo de salida
--FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis, p_busca_nom_archivo)
  --DEFINE 
    --p_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    --p_busca_nom_archivo      VARCHAR(40),
    --v_cmd                    STRING,
    --v_consecutivo            INTEGER
--
  --DEFINE fn                  CHAR(32)
  --DEFINE ch                  base.Channel
--
  --LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"
--
  --LET ch    = base.Channel.create()
--
  --CALL ch.setDelimiter(".")
  --CALL ch.openPipe(v_cmd,"r")
--
  --WHILE ch.read([fn])
    --LET v_consecutivo = fn[28,30]
  --END WHILE
--
  --CALL ch.close()
--
  --LET v_consecutivo = v_consecutivo + 1
--
  --IF length(v_consecutivo) = 0 THEN
     --LET v_consecutivo = 1
  --END IF
   --
  --RETURN v_consecutivo
--
--END FUNCTION

--Genera la copia del archivo generado
{FUNCTION fn_genera_copia_interface(p_archivo_envio, p_ruta_destino)
  DEFINE 
    v_cmd                    STRING,
    p_archivo_envio          VARCHAR(100),
    p_ruta_destino           VARCHAR(40)

  LET v_archivo_copia = "dis_cred_cero_sal"
  LET v_archivo_copia = v_archivo_copia CLIPPED,".dis"     
  LET v_cmd           = "cat ", 
                        p_archivo_envio CLIPPED, " > ",
                        p_ruta_destino CLIPPED, "/", 
                        v_archivo_copia CLIPPED 
  RUN v_cmd

  --LET v_comando_dos = "unix2dos ",p_ruta_destino CLIPPED, " ", v_archivo_copia CLIPPED
  --RUN v_comando_dos

  DISPLAY "Se ha realizado la copia de la interface de Creditos Ceros: ",v_archivo_copia
  DISPLAY ""

  {DISPLAY "\n\nEjecutando scripts de transferencia de archivos (Avance de Pagos Regla 27)"
  LET v_cmd = " sh /opt/Interpel/Scripts/avr2.sh"
  --LET v_cmd = "sh ", p_ruta_destino CLIPPED, "/avr2.sh"
  RUN v_cmd}
  
--END FUNCTION 
