################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 24/02/2015                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISS33                                                   #
#Objetivo          => Programa que genera la intefaz de la liquidación de      #
#                     Créditos Ceros.                                          #
#Fecha inicio      => 04/03/2015                                               #
################################################################################

DATABASE safre_viv

GLOBALS
  DEFINE 
    g_usuario                VARCHAR(30),                  --Almacena al usuario
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --codigo de proceso
    g_opera_cod              LIKE cat_operacion.opera_cod, --codigo de operacion
    g_folio                  LIKE dis_det_avance_pago.folio, --Folio generado
    g_pid                    LIKE glo_pid.pid,
    g_arch_proceso           VARCHAR(100),
    g_qrytxt                 STRING ,                       --Prepara consultas
    g_folio_cons             DECIMAL(9,0),
    g_proceso_cnt            SMALLINT

   DEFINE v_modulo_cod        LIKE seg_modulo.modulo_cod

   DEFINE v_tot_registros          DECIMAL(9,0), -- Total de registros
          v_sum_aivs               DECIMAL(18,6),
          v_sum_aportacion         DECIMAL(12,2), 
          v_sum_amortizacion       DECIMAL(12,2)

   DEFINE v_comando_dos            STRING
   DEFINE v_archivo_copia          VARCHAR(40)

   DEFINE g_mensaje                STRING
   DEFINE v_ruta_origen       CHAR(40)
   DEFINE v_comando           STRING
   
END GLOBALS

MAIN

   LET g_usuario      = ARG_VAL(1)
   LET g_pid          = ARG_VAL(2)
   LET g_proceso_cod  = ARG_VAL(3)
   LET g_opera_cod    = ARG_VAL(4)
   LET g_folio        = ARG_VAL(5)
   LET g_folio_cons   = ARG_VAL(6)
   LET g_proceso_cnt  = ARG_VAL(7)
   
   CALL STARTLOG(g_usuario CLIPPED||".DISS33.log")

   LET v_modulo_cod = "dis"

   --Llama a la función que genera la interface de avance de pagos
   CALL fn_genera_interface_cred_cero()

END MAIN


--Función que genera el archivo de salida de avance de pagos (Regla 27).
FUNCTION fn_genera_interface_cred_cero()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_ddmmaaaa               VARCHAR(08),  --Fecha del archivo de salida
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_bnd_totales            INTEGER 

   DEFINE v_encabezado       STRING,
          v_detalle          STRING,
          v_sumario          STRING          
          
   DEFINE v_precio_fondo     DECIMAL(19,14)
   DEFINE i                  INTEGER

   DEFINE v_arr_num_cred_cero DYNAMIC ARRAY OF RECORD
               folio             DECIMAL(9,0),
               nss               CHAR(11),
               num_credito		   DECIMAL(10),
               periodo_pago	   CHAR(6),
               f_pago	         DATE,
               ent_recaudadora   CHAR(03),
               nrp	            CHAR(11),
               aportacion	      DECIMAL(12,2),
               amortizacion	   DECIMAL(12,2),
               folio_sua	     	DECIMAL(6,0),
               inconsistencia    CHAR(30),
               estado            CHAR(30),
               num_credito_act   DECIMAL(10,0),
               aportacion_liq	   DECIMAL(12,2),
               amortizacion_liq  DECIMAL(12,2),
               aivs_liq	         DECIMAL(22,2)
         END RECORD

   DEFINE v_estado   CHAR(01)

   DEFINE
     v_est                    INTEGER, 
     r_bnd_proceso            INTEGER, 
     v_status_err             INTEGER, 
     v_desc_err               CHAR(50),
     v_destino_dis            INTEGER


  LET v_bnd_totales = 0
  LET v_est         = 101
  LET r_bnd_proceso = 0 
  LET v_destino_dis = 14

  --Valida que exista información para generar el archivo 
  SELECT COUNT(*)
  INTO   v_bnd_totales
  FROM   dis_arh_num_cred_0
  
  IF v_bnd_totales  = 0    OR 
     v_bnd_totales IS NULL THEN
     DISPLAY "Error: No existe información para generar el archivo."
     EXIT  PROGRAM   
  END IF    


   LET g_qrytxt = " SELECT dco.folio, ",
                       "\n dco.nss, ",
                       "\n dco.num_credito, ",
                       "\n dco.periodo_pago, ",
                       "\n dco.f_pago, ",
                       "\n '180', ",
                       "\n dco.nrp, ",
                       "\n NVL(dco.aportacion,0), ",
                       "\n NVL(dco.amortizacion,0), ",
                       "\n dco.folio_sua, ",
                       "\n dco.inconsistencia, ",
                       "\n dco.estado, ",
                       "\n dli.num_credito, ",
                       "\n NVL(dli.aportacion,0), ",
                       "\n NVL(dli.amortizacion,0), ",
                       "\n NVL(dli.aivs,0) ",
                  "\n FROM dis_arh_num_cred_0 dco, OUTER dis_liq_inconsistente dli",
                  "\n WHERE dco.id_dis_arh_num_cred = dli.id_dis_arh_num_cred ",
                  "\n AND   dco.folio = dli.folio_arh_num_cred ",
                  "\n AND   dco.folio = ",g_folio_cons,
                  "\n ORDER BY dco.estado, dco.periodo_pago "

   --DISPLAY g_qrytxt

   PREPARE ps_dis_cred_cero FROM g_qrytxt
   DECLARE cur_dis_cred_cero CURSOR FOR ps_dis_cred_cero

   --Obtiene el valor de fondo (AIVS)
   SELECT f.precio_fondo
   INTO   v_precio_fondo
   FROM   glo_valor_fondo f
   WHERE  f.fondo       = 11
   AND    f.f_valuacion = TODAY

  --Se obtienen la ruta envio del modulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod
  
  SELECT ruta_bin
  INTO   v_ruta_origen
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy"
  
  LET v_nom_archivo       = "/dis_cred_cero_sal_"
  LET v_busca_nom_archivo = "dis_cred_cero_sal_", v_ddmmaaaa

  --Obtiene consecutivo para archivo por día
  CALL fn_crea_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  RETURNING v_cont_dia

  LET v_reg_dia  = v_cont_dia USING "&&&"
  LET v_ddmmaaaa = TODAY USING "ddmmyyyy"
   
  LET v_nom_archivo  = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo

  WHENEVER ERROR CONTINUE

  PREPARE prp_fn_transaccion24
  FROM    "EXECUTE FUNCTION sp_dis_transaccion24(?,?,?,?,?,?,?)"
  EXECUTE prp_fn_transaccion24 INTO r_bnd_proceso, v_status_err, v_desc_err                                    
                             USING g_proceso_cod,
                                   g_opera_cod, 
                                   v_nom_archivo, 
                                   g_folio,
                                   v_est, 
                                   v_destino_dis,
                                   g_usuario

  WHENEVER ERROR STOP 

  DISPLAY "Función Transaccion 24 ",r_bnd_proceso
  DISPLAY "Código:",v_status_err         
  DISPLAY "mensaje", v_desc_err
         
  IF r_bnd_proceso <> 0 THEN
    DISPLAY "Error en la transacción 24 ",v_status_err," ",v_desc_err
    EXIT PROGRAM
  END IF

  LET v_ch_arch_salida = base.Channel.create()
   
  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

 {--Imprime encabezado del archivo
   LET v_encabezado = " FECHA ",TODAY
   CALL v_ch_arch_salida.write([v_encabezado])

   --Si se solicitó el folio de dispersión se incluye en el encabezado
   LET v_encabezado = " FOLIO DE CONSULTA ",g_folio
   CALL v_ch_arch_salida.write([v_encabezado])
    
   --Si se solicitó número de seguridad social se incluye en el encabezado

   LET v_encabezado = " VALOR DE FONDO ",v_precio_fondo
   CALL v_ch_arch_salida.write([v_encabezado])


   LET v_encabezado = " NSS |NÚMERO DE CREDITO |PERIODO PAGO |FECHA PAGO |NRP |MONTO APORTACIÓN |MONTO AMORTIZACIÓN |FOLIO SUA |INCONSISTENCIAS |ESTADO DE CONSULTA| NÚMERO DE CRÉDITO ACTIVO |MONTO APORTACIÓN LIQUIDADO | MONTO AMORTIZACION LIQUIDADO | AIVS"
   CALL v_ch_arch_salida.write([v_encabezado])}

   LET v_tot_registros    = 0
   LET v_sum_aivs         = 0
   LET v_sum_aportacion   = 0
   LET v_sum_amortizacion = 0

   LET i = 1
   FOREACH cur_dis_cred_cero INTO v_arr_num_cred_cero[i].*

      {LET v_estado =  v_arr_num_cred_cero[i].estado
      
      CASE v_estado
         WHEN "0"
            LET v_arr_num_cred_cero[i].estado = v_estado CLIPPED||" LIQUIDADO." CLIPPED
         WHEN "1"
            LET v_arr_num_cred_cero[i].estado = v_estado CLIPPED||" RECHAZADO." CLIPPED
      END CASE}
   
      LET v_detalle = "01",
                      v_arr_num_cred_cero[i].nss USING "&&&&&&&&&&&",
                      v_arr_num_cred_cero[i].num_credito USING "&&&&&&&&&&",
                      v_arr_num_cred_cero[i].periodo_pago USING "&&&&&&",
                      v_arr_num_cred_cero[i].f_pago USING "yyyymmdd",
                      v_arr_num_cred_cero[i].ent_recaudadora USING "&&&",
                      v_arr_num_cred_cero[i].nrp,
                      (v_arr_num_cred_cero[i].aportacion * 100) USING "&&&&&&&&&&",
                      (v_arr_num_cred_cero[i].amortizacion * 100) USING "&&&&&&&&&&",
                      v_arr_num_cred_cero[i].folio_sua USING "&&&&&&",
                      v_arr_num_cred_cero[i].inconsistencia  USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&",
                      --v_arr_num_cred_cero[i].estado,"| ",
                      v_arr_num_cred_cero[i].num_credito_act USING "&&&&&&&&&&",
                      (v_arr_num_cred_cero[i].aportacion_liq * 100) USING "&&&&&&&&&&",
                      (v_arr_num_cred_cero[i].amortizacion_liq * 100)USING "&&&&&&&&&&",
                      (v_arr_num_cred_cero[i].aivs_liq * 100)USING "&&&&&&&&&&"
                                  
      CALL v_ch_arch_salida.write([v_detalle])

      
      LET v_tot_registros    = v_tot_registros    + 1
      LET v_sum_aivs         = v_sum_aivs         + v_arr_num_cred_cero[i].aivs_liq
      LET v_sum_aportacion   = v_sum_aportacion   + v_arr_num_cred_cero[i].aportacion_liq
      LET v_sum_amortizacion = v_sum_amortizacion + v_arr_num_cred_cero[i].amortizacion_liq
      LET i = i + 1
   END FOREACH

   LET i = i - 1
   CALL v_arr_num_cred_cero.deleteElement(v_arr_num_cred_cero.getLength())
   LET v_tot_registros = i
   
   --Escribe el sumario
   {LET v_sumario = "TOTALES|",v_tot_registros,"| | | | | | | | | | |",
                  v_sum_aportacion, "|",
                  v_sum_amortizacion, "|",
                  v_sum_aivs
   CALL v_ch_arch_salida.write([v_sumario])}

   DISPLAY "\nINICIA LIQUIDACIÓN\n"

   DISPLAY "\nSe ha enviado la interface Liquidación Créditos Ceros.\n"

   
   
   IF g_proceso_cnt = 0 THEN
      DISPLAY "El registro contable se realizó exitosamente."
   ELSE
      IF g_proceso_cnt = 1 THEN
         DISPLAY "Error: No existe monto de abono en el registro contable."
      END IF 
      IF g_proceso_cnt = 2 THEN
         DISPLAY "Error: No existe monto de cargo en el registro contable."
      END IF 
      IF g_proceso_cnt = 3 THEN
         DISPLAY "Error: Diferencia de montos abono - cargo."
      END IF 
   END IF 
   
   --Despliega información en el log
   DISPLAY "\n ############### INTERFACE ##################"
   DISPLAY " ############## CRÉDITOS CEROS  ###############"
   DISPLAY ""
   DISPLAY "Nombre del archivo             : ", v_nom_archivo
   DISPLAY "Folio Creditos Cero            : ", g_folio
   DISPLAY "Suma Monto Aportación          : ", v_sum_aportacion
   DISPLAY "Suma Monto Amortización        : ", v_sum_amortizacion
   DISPLAY "Suma Monto AIVS                : ", v_sum_aivs
   DISPLAY "\n ########################################################"


   --Cierra el archivo
   CALL v_ch_arch_salida.close()
   LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
   RUN v_comando_dos

   LET g_mensaje = "Se ha generado el archivo de la interface Números de Créditos igual a Cero \n en la ruta "||v_ruta_nomarch
   --CALL fn_mensaje("Información",g_mensaje,"information")
   DISPLAY g_mensaje

   DISPLAY ""
   DISPLAY "El archivo ha sido generado exitosamente."
   DISPLAY "Ruta del archivo: ", v_ruta_nomarch
  
   --Genera una copia de la interface con el nombre corto
   CALL fn_genera_copia_interface(v_ruta_nomarch,v_ruta_envio_dis)


   DISPLAY "\n### Generación de Interfaces ###"
               
   --Generar el archivo o interface de Pago REAL HS
   LET v_comando = "fglrun ",v_ruta_origen CLIPPED,"/DISS01.42r ",g_folio
   RUN v_comando

   ----Generar el archivo o interface a las Entidades Financieras y/o Servicios
   LET v_comando = "fglrun ",v_ruta_origen CLIPPED,"/DISS03.42r ",g_folio
   RUN v_comando

   --Generar el archivo o interface a las diferencias positivas por avance (Cargo para HS)
   LET v_comando = "fglrun ",v_ruta_origen CLIPPED,"/DISS04.42r ",g_folio, " ", g_proceso_cod
   RUN v_comando

   --Generar el archivo o interface a las diferencias negativas por avance (Abono para HS)
   LET v_comando = "fglrun ",v_ruta_origen CLIPPED,"/DISS05.42r ",g_folio
   RUN v_comando

   --Generar el archivo o interface a las diferencias positivas por avance (Cargo para HS) <=$2
   LET v_comando = "fglrun ",v_ruta_origen CLIPPED,"/DISS08.42r ",g_folio, " ", g_proceso_cod
   RUN v_comando

   --Generar el archivo o interface a las diferencias negativas por avance (Abono para HS) <=$2
   LET v_comando = "fglrun ",v_ruta_origen CLIPPED,"/DISS09.42r ",g_folio
   RUN v_comando
   
   DISPLAY "\n### Generación de Reporte de Inconsistencias ###"
   
   --Generar el reporte de inconsistencias
   LET v_comando = "fglrun ",v_ruta_origen CLIPPED,"/DISS07.42r ",g_usuario," ",
                                                                  g_pid," ", 
                                                                  g_proceso_cod, " ",
                                                                  g_opera_cod, " ",
                                                                  g_folio
   RUN v_comando

   DISPLAY "\n################################################"

END FUNCTION 

#Objetivo: genera el número consecutivo por día para el archivo de salida
FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis, p_busca_nom_archivo)
  DEFINE 
    p_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    p_busca_nom_archivo      VARCHAR(40),
    v_cmd                    STRING,
    v_consecutivo            INTEGER

  DEFINE fn                  CHAR(32)
  DEFINE ch                  base.Channel

  LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

  LET ch    = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")

  WHILE ch.read([fn])
    LET v_consecutivo = fn[28,30]
  END WHILE

  CALL ch.close()

  LET v_consecutivo = v_consecutivo + 1

  IF length(v_consecutivo) = 0 THEN
     LET v_consecutivo = 1
  END IF
   
  RETURN v_consecutivo

END FUNCTION

--Genera la copia del archivo generado
FUNCTION fn_genera_copia_interface(p_archivo_envio, p_ruta_destino)
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
  
END FUNCTION 
