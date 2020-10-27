################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 20/08/2015                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISS43                                                   #
#Objetivo          => Programa que genera la intefaz del extractor de          #
#                     Avance Abierto por NRP.                                  #
#Fecha inicio      => 18/08/2015                                               #
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
    p_periodo_pago           CHAR(06),
    g_proceso_cnt            SMALLINT,
    p_destino                SMALLINT 

   DEFINE v_modulo_cod        LIKE seg_modulo.modulo_cod

   DEFINE v_tot_registros          DECIMAL(9,0), -- Total de registros
          v_sum_aivs               DECIMAL(18,6),
          v_sum_aportacion         DECIMAL(12,2), 
          v_sum_amortizacion       DECIMAL(12,2)

   DEFINE v_sum_imp_ap_pat         DECIMAL(12,2), 
          v_sum_imp_am_cre         DECIMAL(12,2)

   DEFINE v_comando_dos            STRING
   DEFINE v_archivo_copia          VARCHAR(40)

   DEFINE g_mensaje                STRING

   DEFINE g_proceso_cod   LIKE cat_proceso.proceso_cod, --codigo del proceso
          g_opera_cod     LIKE cat_operacion.opera_cod, --codigo de operacion
          g_pid           DECIMAL(9,0)

   DEFINE g_tot_ava_nrp             INTEGER
   
END GLOBALS

MAIN

   LET p_periodo_pago = ARG_VAL(1)
   --LET p_destino      = ARG_VAL(2)
   LET g_pid          = ARG_VAL(2)
   LET g_opera_cod    = ARG_VAL(3)
   LET g_proceso_cod  = ARG_VAL(4)
   LET g_tot_ava_nrp  = ARG_VAL(5)
   LET p_usuario      = ARG_VAL(6)
   
   CALL STARTLOG(p_usuario CLIPPED||".DISS43.log")

   LET v_modulo_cod = "dis"

   --Llama a la función que genera la interface de avance de pagos
   CALL fn_genera_interface_avance_cred_liq()

END MAIN


--Función que genera el archivo de salida del extractor de dispersión
FUNCTION fn_genera_interface_avance_cred_liq()
   DEFINE r_bnd                 INTEGER, 
          v_status_err          INTEGER ,
          v_desc_err            VARCHAR(200)
          
   DEFINE r_bandera              SMALLINT
          
  WHENEVER ERROR CONTINUE 
    DISPLAY "INICIA fn_dis_extractor_avance_nrp: ", CURRENT HOUR TO SECOND 
    PREPARE ps_sp_dis_avance_nrp FROM "EXECUTE PROCEDURE fn_dis_extractor_avance_nrp(?)"
    EXECUTE ps_sp_dis_avance_nrp USING p_periodo_pago
                                 INTO r_bnd, v_status_err, v_desc_err
    DISPLAY "TERMINA fn_dis_extractor_avance_nrp: ", CURRENT HOUR TO SECOND 
  WHENEVER ERROR STOP 

   CALL fn_extractor_avance_nrp()

   CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
   RETURNING r_bandera


END FUNCTION 



FUNCTION fn_extractor_avance_nrp()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_ruta_nomarch_det       VARCHAR(100),
    v_ruta_nomarch_tot       VARCHAR(100),
    v_nom_archivo            VARCHAR(50),  --Nombre del archivo de salida
    v_nom_archivo_det        VARCHAR(50),
    v_nom_archivo_tot        VARCHAR(50),
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
   
   DEFINE v_arr_avance_nrp DYNAMIC ARRAY OF RECORD
            folio_liquida        DECIMAL(9,0),
            nss	               CHAR(11),
            periodo_pago         CHAR(06),
            num_credito          DECIMAL(10,0),
            nrp                  CHAR(11),
            monto_apo            DECIMAL(12,2),
            monto_amo            DECIMAL(12,2),
            f_liquida            DATE,
            folio_pago			   DECIMAL(9,0),
            folio_sua            DECIMAL(6,0),
            f_pago               DATE, 
            periodo_pag          CHAR(06),
            imp_ap_pat           DECIMAL(12,2),
            imp_am_cre           DECIMAL(12,2)
         END RECORD

         
   LET v_desc_destino = "AVANCE ABIERTO POR NRP"
   LET v_aaaammdd  = TODAY USING "yyyymmdd"
   
         
   {LET g_qrytxt = " SELECT anrp.* ",
                  "\n FROM tmp_avance_nrp anrp",
                  "\n ORDER BY anrp.nss, anrp.nrp, anrp.periodo_pago "

   
   --LET v_busca_nom_archivo = "dis_ext_pr_", v_aaaammdd,"_"
         
   --DISPLAY g_qrytxt

   PREPARE ps_dis_extractor_avance_nrp FROM g_qrytxt
   DECLARE cur_dis_extractor_avance_nrp CURSOR FOR ps_dis_extractor_avance_nrp}
  
  --Se obtienen la ruta envio del modulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod


 --TITULOS
   LET v_nom_archivo   = "/dis_ext_avance_nrp_", v_aaaammdd,"_"
   LET v_nom_archivo   = v_nom_archivo CLIPPED ||p_periodo_pago CLIPPED||"."|| v_modulo_cod
   LET v_ruta_nomarch      = v_ruta_envio_dis CLIPPED || v_nom_archivo

   --DISPLAY "v_nom_archivo  : ", v_nom_archivo
   --DISPLAY "v_ruta_nomarch : ", v_ruta_nomarch

   UNLOAD TO v_ruta_nomarch
   SELECT UNIQUE("FOLIO LIQUIDA"), 
          "NSS", 
          "PERIODO PAGO (AVANCE)", 
          "NÚMERO CRÉDITO", 
          "NRP",
          "APORTACIÓN (AVANCE)",
          "AMORTIZACIÓN (AVANCE)" ,
          "FECHA LIQUIDACIÓN",
          "FOLIO PAGO", 
          "FOLIO SUA",
          "FECHA PAGO",
          "PERIODO PAGO",
          "APORTACIÓN (PAGO)",
          "AMORTIZACIÓN (PAGO)" 
   FROM tmp_avance_nrp
  

  --DETALLE
   LET v_nom_archivo_det       = "/dis_ext_avance_nrp_det", v_aaaammdd,"_"
   LET v_nom_archivo_det  = v_nom_archivo_det CLIPPED ||p_periodo_pago CLIPPED||"."|| v_modulo_cod
   LET v_ruta_nomarch_det = v_ruta_envio_dis CLIPPED || v_nom_archivo_det

   --DISPLAY "v_nom_archivo: ", v_nom_archivo
   --DISPLAY "v_ruta_nomarch_det: ", v_ruta_nomarch_det

   UNLOAD TO v_ruta_nomarch_det
   SELECT anrp.* 
   FROM tmp_avance_nrp anrp
   ORDER BY anrp.nss, anrp.nrp, anrp.periodo_pago
   


   LET v_comando_dos = "cat ",v_ruta_nomarch_det CLIPPED, " >> ", v_ruta_nomarch CLIPPED
   RUN v_comando_dos

   
   --TOTALES
   LET v_nom_archivo_tot   = "/dis_ext_avance_nrp_tot"
   LET v_nom_archivo_tot   = v_nom_archivo_tot CLIPPED ||p_periodo_pago CLIPPED||"."|| v_modulo_cod
   LET v_ruta_nomarch_tot  = v_ruta_envio_dis CLIPPED || v_nom_archivo_tot

   --DISPLAY "v_nom_archivo_tot: ", v_nom_archivo_tot
   --DISPLAY "v_ruta_nomarch_tot:", v_ruta_nomarch_tot

   UNLOAD TO v_ruta_nomarch_tot
   SELECT "TOTALES",
          COUNT(*),
          "","","", 
          SUM(monto_apo), 
          SUM(monto_amo),
          "","","","","",
          SUM(imp_ap_pat), 
          SUM(imp_am_cre) 
   FROM tmp_avance_nrp

   LET v_comando_dos = "cat ",v_ruta_nomarch_tot CLIPPED, " >> ", v_ruta_nomarch CLIPPED
   RUN v_comando_dos

   LET v_comando_dos = "rm ",v_ruta_nomarch_tot CLIPPED
   RUN v_comando_dos

   LET v_comando_dos = "rm ",v_ruta_nomarch_det CLIPPED
   RUN v_comando_dos


  --Obtiene consecutivo para archivo por día
  --CALL fn_crea_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  --RETURNING v_cont_dia
  
  --LET v_reg_dia  = v_cont_dia USING "&&&"




  {LET v_ch_arch_salida = base.Channel.create()
   
  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

 --Imprime encabezado del archivo
   --LET v_encabezado = " FECHA:  ",TODAY
   --CALL v_ch_arch_salida.write([v_encabezado])

   --LET v_encabezado = " FOLIO DE DISPERSIÓN:  ",p_folio_dis
   --CALL v_ch_arch_salida.write([v_encabezado])

   --LET v_encabezado = " DESTINO:  ",v_desc_destino
   --CALL v_ch_arch_salida.write([v_encabezado])

   --LET v_encabezado = " VALOR DE FONDO:  ",v_precio_fondo
   --CALL v_ch_arch_salida.write([v_encabezado])


   LET v_encabezado = " FOLIO LIQUIDA| NSS |PERIODO PAGO (AVANCE) |NÚMERO CRÉDITO |NRP |APORTACIÓN (AVANCE) |AMORTIZACIÓN (AVANCE) |FECHA LIQUIDACIÓN |FOLIO PAGO |FOLIO SUA |FECHA PAGO |PERIODO PAGO |APORTACIÓN (PAGO) |AMORTIZACIÓN (PAGO)"
   CALL v_ch_arch_salida.write([v_encabezado])}

   LET v_tot_registros    = 0
   LET v_sum_aivs         = 0
   LET v_sum_aportacion   = 0
   LET v_sum_amortizacion = 0

   LET v_sum_imp_ap_pat   = 0 
   LET v_sum_imp_am_cre   = 0


   LET g_qrytxt = " SELECT COUNT(*), ",
                      " \n SUM(monto_apo), ",
                      " \n SUM(monto_amo), ",
                      " \n SUM(imp_ap_pat), ",
                      " \n SUM(imp_am_cre) ",
                      " \n FROM tmp_avance_nrp "

   PREPARE ps_totales_avance_nrp FROM g_qrytxt
   EXECUTE ps_totales_avance_nrp INTO v_tot_registros,
                                      v_sum_aportacion,
                                      v_sum_amortizacion,
                                      v_sum_imp_ap_pat,
                                      v_sum_imp_am_cre
   
   {LET i = 1
   FOREACH cur_dis_extractor_avance_nrp INTO v_arr_avance_nrp[i].folio_liquida, 
                                             v_arr_avance_nrp[i].nss,
                                             v_arr_avance_nrp[i].periodo_pago,      
                                             v_arr_avance_nrp[i].num_credito,
                                             v_arr_avance_nrp[i].nrp,                
                                             v_arr_avance_nrp[i].monto_apo,       
                                             v_arr_avance_nrp[i].monto_amo, 
                                             v_arr_avance_nrp[i].f_liquida, 
                                             v_arr_avance_nrp[i].folio_pago,
                                             v_arr_avance_nrp[i].folio_sua,          
                                             v_arr_avance_nrp[i].f_pago, 
                                             v_arr_avance_nrp[i].periodo_pag,
                                             v_arr_avance_nrp[i].imp_ap_pat,     
                                             v_arr_avance_nrp[i].imp_am_cre      
      
      LET v_detalle = v_arr_avance_nrp[i].folio_liquida, "|",
                      v_arr_avance_nrp[i].nss USING "&&&&&&&&&&&", "|", 
                      v_arr_avance_nrp[i].periodo_pago USING "&&&&&&", "|", 
                      v_arr_avance_nrp[i].num_credito , "|",
                      v_arr_avance_nrp[i].nrp, "|",                 
                      (v_arr_avance_nrp[i].monto_apo * 100) USING "&&&&&&&&&&&&", "|",          
                      (v_arr_avance_nrp[i].monto_amo * 100) USING "&&&&&&&&&&&&", "|", 
                      v_arr_avance_nrp[i].f_liquida USING "yyyymmdd", "|", 
                      v_arr_avance_nrp[i].folio_pago, "|",        
                      v_arr_avance_nrp[i].folio_sua USING "&&&&&&", "|",          
                      v_arr_avance_nrp[i].f_pago USING "yyyymmdd", "|", 
                      v_arr_avance_nrp[i].periodo_pag USING "&&&&&&", "|",       
                      (v_arr_avance_nrp[i].imp_ap_pat * 100) USING "&&&&&&&&&&&&", "|",          
                      (v_arr_avance_nrp[i].imp_am_cre * 100) USING "&&&&&&&&&&&&" 
                                  
      --CALL v_ch_arch_salida.write([v_detalle])

      
      LET v_tot_registros    = v_tot_registros    + 1
      --LET v_sum_aivs         = v_sum_aivs         + v_arr_avance_nrp[i].aivs
      LET v_sum_aportacion   = v_sum_aportacion   + v_arr_avance_nrp[i].monto_apo
      LET v_sum_amortizacion = v_sum_amortizacion + v_arr_avance_nrp[i].monto_amo

      LET v_sum_imp_ap_pat   = v_sum_imp_ap_pat + v_arr_avance_nrp[i].imp_ap_pat
      LET v_sum_imp_am_cre   = v_sum_imp_am_cre + v_arr_avance_nrp[i].imp_am_cre 
   
      LET i = i + 1
   END FOREACH

   LET i = i - 1
   CALL v_arr_avance_nrp.deleteElement(v_arr_avance_nrp.getLength())
   LET v_tot_registros = i
   
   --Escribe el sumario
   LET v_sumario = "TOTALES|",v_tot_registros USING "&&&&&&&&&","| | | | ",
                              (v_sum_aportacion * 100) USING "&&&&&&&&&&&&", "|",
                              --v_sum_aivs USING "&&&&&&&&&", "|",
                              (v_sum_amortizacion * 100) USING  "&&&&&&&&&&&&","| | | | | |",
                              (v_sum_imp_ap_pat * 100) USING "&&&&&&&&&&&&", "|",
                              (v_sum_imp_am_cre * 100) USING "&&&&&&&&&&&&"
                  
   CALL v_ch_arch_salida.write([v_sumario])

   --DISPLAY "\nSe ha enviado la interface del extractor de dispersión de "|| v_desc_destino||"\n"

   --Cierra el archivo
   CALL v_ch_arch_salida.close()
   LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
   RUN v_comando_dos}

   IF p_periodo_pago IS NULL OR p_periodo_pago = 0 THEN
      LET p_periodo_pago = ""
   END IF
   
    --Despliega información en el log
   DISPLAY "\n ############### INTERFACE ##################"
   DISPLAY " ############## "||v_desc_destino||  " ###############"
   DISPLAY ""
   DISPLAY "Nombre del archivo             : ", v_nom_archivo
   DISPLAY "Periodo Pago                   : ", p_periodo_pago
   DISPLAY "Registros con Avance por NRP   : ", g_tot_ava_nrp
   DISPLAY "Registros después Validaciones : ", v_tot_registros
   DISPLAY "Suma Monto Aportación          : ", v_sum_aportacion
   --DISPLAY "Suma Monto AIVS                : ", v_sum_aivs
   DISPLAY "Suma Monto Amortización        : ", v_sum_amortizacion
   DISPLAY "Suma Monto Aportación Pago     : ", v_sum_imp_ap_pat
   DISPLAY "Suma Monto Amortización Pago   : ", v_sum_imp_am_cre
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
