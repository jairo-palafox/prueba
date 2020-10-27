################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 20/07/2015                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISS40                                                   #
#Objetivo          => Programa que genera la intefaz del extractor de          #
#                     Avance de Pagos por periodo de pago.                     #
#Fecha inicio      => 20/07/2015                                               #
################################################################################

DATABASE safre_viv

GLOBALS
   DEFINE 
      p_usuario                VARCHAR(30),                    --Almacena al usuario
      g_proceso_cod            LIKE cat_proceso.proceso_cod,   --codigo de proceso
      g_opera_cod              LIKE cat_operacion.opera_cod,   --codigo de operacion
      p_folio                  LIKE dis_det_avance_pago.folio, --Folio generado
      g_pid                    LIKE glo_pid.pid,
      g_arch_proceso           VARCHAR(100),
      g_qrytxt                 STRING ,                        --Prepara consultas
      p_periodo_pago           CHAR(6),
      g_proceso_cnt            SMALLINT    

   DEFINE v_modulo_cod        LIKE seg_modulo.modulo_cod

   DEFINE v_comando_dos            STRING
   DEFINE v_archivo_copia          VARCHAR(40)

   DEFINE g_mensaje                STRING
   
END GLOBALS

MAIN

   LET p_periodo_pago = ARG_VAL(1)   
   LET p_folio        = ARG_VAL(2)   
   LET p_usuario      = ARG_VAL(3)
   
   CALL STARTLOG(p_usuario CLIPPED||".DISS40.log")

   LET v_modulo_cod = "dis"

   --Llama a la función que genera la interface de avance de pagos
   CALL fn_genera_interface_dispersion()

END MAIN


--Función que genera el archivo de salida del extractor de dispersión
FUNCTION fn_genera_interface_dispersion()
   DEFINE r_bnd                 INTEGER, 
          v_status_err          INTEGER ,
          v_desc_err            VARCHAR(200)
          

   WHENEVER ERROR CONTINUE 
   PREPARE ps_sp_dis_hs FROM "EXECUTE PROCEDURE fn_dis_ext_ava_pag(?)"
   EXECUTE ps_sp_dis_hs USING p_periodo_pago
                          INTO r_bnd, v_status_err, v_desc_err
   WHENEVER ERROR STOP 

   CALL arma_archivo() 

END FUNCTION 


FUNCTION arma_archivo()
   DEFINE 
      v_ch_arch_salida         BASE.CHANNEL,
      v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
      v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
      v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
      v_aaaammdd               VARCHAR(08)  --Fecha del archivo de salida

   DEFINE v_encabezado       STRING,
          v_detalle          STRING
             
   DEFINE i                  INTEGER
  
   DEFINE v_arr_int_ava_pag DYNAMIC ARRAY OF RECORD
      periodo_pago        CHAR(6),      
      nss                 CHAR(11),     
      num_credito         DECIMAL(10,0),
      f_pago              DATE,
      nrp                 CHAR(11),     
      monto_aportacion    DECIMAL(12,2),
      monto_amortizacion  DECIMAL(12,6),
      estado              SMALLINT,
      desc_edo_avance     CHAR(50),     
      folio               DECIMAL (9,0),
      f_actualiza         DATE,   
      f_interface         DATE,      
      tipo_interface 	  CHAR(4)      
   END RECORD

          
   LET v_aaaammdd  = TODAY USING "yyyymmdd"
   
         
   LET g_qrytxt = " SELECT * ",
                  "\n FROM tmp_dis_ava_pag"

   LET v_nom_archivo       = "/dis_ext_avpp_", v_aaaammdd,"_"   
            

   PREPARE ps_dis_extractor_pr FROM g_qrytxt
   DECLARE cur_dis_extractor_pr CURSOR FOR ps_dis_extractor_pr
  
   --Se obtienen la ruta envio del modulo
   SELECT ruta_envio 
     INTO v_ruta_envio_dis
     FROM seg_modulo
    WHERE modulo_cod = v_modulo_cod


   LET v_nom_archivo  = v_nom_archivo CLIPPED ||p_folio CLIPPED||"."|| v_modulo_cod
   LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo

   LET v_ch_arch_salida = base.Channel.create()
   
   --Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
   CALL v_ch_arch_salida.setDelimiter("")

   --Imprime encabezado del archivo
   LET v_encabezado = "PERIODO DE PAGO |NSS |NÚMERO DE CREDITO |FECHA PAGO |NRP |APORTACIÓN |AMORTIZACIÓN |ESTADO AVANCE DE PAGO |FOLIO |FECHA PROCESO DEL FOLIO| FECHA INTERFACE |TIPO INTERFACE"
   CALL v_ch_arch_salida.write([v_encabezado])
  
   LET i = 1   
   
   FOREACH cur_dis_extractor_pr INTO v_arr_int_ava_pag[i].periodo_pago,      
                                     v_arr_int_ava_pag[i].nss, 
                                     v_arr_int_ava_pag[i].num_credito,                                     
                                     v_arr_int_ava_pag[i].f_pago,
                                     v_arr_int_ava_pag[i].nrp,                
                                     v_arr_int_ava_pag[i].monto_aportacion,       
                                     v_arr_int_ava_pag[i].monto_amortizacion, 
                                     v_arr_int_ava_pag[i].estado,           
                                     v_arr_int_ava_pag[i].desc_edo_avance,       
                                     v_arr_int_ava_pag[i].folio,          
                                     v_arr_int_ava_pag[i].f_actualiza,                                                                            
                                     v_arr_int_ava_pag[i].f_interface,     
                                     v_arr_int_ava_pag[i].tipo_interface  


      IF v_arr_int_ava_pag[i].estado = 80 THEN 
         LET v_arr_int_ava_pag[i].monto_aportacion = 0.00      
      END IF

      LET v_detalle = v_arr_int_ava_pag[i].periodo_pago USING "&&&&&&", "|",            
                                     v_arr_int_ava_pag[i].nss USING "&&&&&&&&&&&", "|", 
                                     v_arr_int_ava_pag[i].num_credito USING "&&&&&&&&&&", "|",
                                     v_arr_int_ava_pag[i].f_pago USING "yyyymmdd", "|",
                                     v_arr_int_ava_pag[i].nrp, "|",                 
                                     (v_arr_int_ava_pag[i].monto_aportacion * 100) USING "&&&&&&&&&&&&&&", "|",
                                     (v_arr_int_ava_pag[i].monto_amortizacion * 100) USING "&&&&&&&&&&&&&&", "|",
                                     v_arr_int_ava_pag[i].estado CLIPPED, " - ", v_arr_int_ava_pag[i].desc_edo_avance CLIPPED, "|",       
                                     v_arr_int_ava_pag[i].folio, "|",
                                     v_arr_int_ava_pag[i].f_actualiza USING "yyyymmdd", "|",
                                     v_arr_int_ava_pag[i].f_interface USING "yyyymmdd", "|",
                                     v_arr_int_ava_pag[i].tipo_interface 
                                  
      CALL v_ch_arch_salida.write([v_detalle])        

      LET i = i + 1
   END FOREACH

   LET i = i - 1   
   CALL v_arr_int_ava_pag.deleteElement(v_arr_int_ava_pag.getLength())   
   
   --Despliega información en el log
    
   DISPLAY ""
   DISPLAY "Nombre del archivo             : ", v_nom_archivo
   DISPLAY "Folio Dispersión               : ", p_folio
   DISPLAY "Total de registros             : ", i
   DISPLAY ""

   --Cierra el archivo
   CALL v_ch_arch_salida.close()
   LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
   RUN v_comando_dos

   LET g_mensaje = "Se ha generado el archivo de la interface del extractor de dispersión de Avance de Pagos por periodo de pago",
                   "\nen la ruta: "||v_ruta_nomarch
                      
   DISPLAY g_mensaje

END FUNCTION