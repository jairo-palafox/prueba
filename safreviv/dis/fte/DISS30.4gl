################################################################################
#Versión                    => 1.0.0                                           #
#Fecha última modificacion  => 06/08/2018                                      #
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISS30                                                    #
#Objetivo         => Generar la interface de los avances de pago vs registro de#
#                    pagos compensación. (Por folio de avance de pago o masivo)#
#Fecha de Inicio  => 28/07/2014                                                #
################################################################################
DATABASE safre_viv
GLOBALS

  --Sección de variables del programa
  DEFINE 
    p_nom_prog               VARCHAR(30),
    p_tipo_proceso           SMALLINT,
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    p_proceso_cod            LIKE cat_proceso.proceso_cod,
    p_opera_cod              LIKE cat_operacion.opera_cod,
    p_usuario                LIKE seg_usuario.usuario_cod,
    p_pid                    DECIMAL(9,0)

  DEFINE 
    v_tot_registros          DECIMAL(9,0), --Total de registros
    v_sum_apo_ava            DECIMAL(22,2), 
    v_sum_amo_ava            DECIMAL(22,2),
    v_sum_apo_pag            DECIMAL(22,2), 
    v_sum_amo_pag            DECIMAL(22,2)
    
  --Record para información del detalle de avances vs pagos compensación
  DEFINE v_reg_ava_vs_comp   RECORD
    v_reg_folio_ava          DECIMAL(9,0),
    v_reg_nss                CHAR(11),
    v_reg_num_credito        DECIMAL(10,0),
    v_reg_periodo_pago       CHAR(6),
    v_reg_nrp                CHAR(11),     
    v_reg_monto_apo_ava      DECIMAL(12,2),
    v_reg_monto_amo_ava      DECIMAL(12,2), 
    v_reg_monto_apo_pag      DECIMAL(12,2),
    v_reg_monto_amo_pag      DECIMAL(12,2), 
    v_reg_f_pago             DATE, 
    v_reg_edo_comp_apo       VARCHAR(30),    
    v_reg_edo_comp_amo       VARCHAR(30),    
    v_reg_edo_avance         VARCHAR(50) 
  END RECORD

  --Variables auxiliares
  DEFINE 
    g_sql_txt                STRING,
    v_qwery_ibx              STRING, 
    v_folio_dis              VARCHAR(9),
    v_nss                    CHAR(11),
    v_nrp                    CHAR(11),
    v_periodo_pago           CHAR(6),
    v_monto_apo_avance       VARCHAR(14),
    v_monto_amo_avance       VARCHAR(14),
    v_folio_pago             VARCHAR(9),
    v_monto_apo_pag          VARCHAR(14),
    v_monto_amo_pag          VARCHAR(14),
    v_folio_sua              VARCHAR(9),
    v_f_pago                 VARCHAR(8),
    v_edo_comp_apo           SMALLINT,
    v_edo_comp_amo           SMALLINT,
    v_desc_edo_comp_apo      VARCHAR(30),
    v_desc_edo_comp_amo      VARCHAR(30)
      
  DEFINE 
    f_folio                  DECIMAL(9,0),
    v_total_registros        BIGINT,
    f_tot_ava_apo            DECIMAL(22,2),
    f_tot_ava_amo            DECIMAL(22,2),
    f_tot_pag_apo            DECIMAL(22,2),
    f_tot_pag_amo            DECIMAL(22,2)

  DEFINE 
    v_tot_ava_apo            VARCHAR(22),
    v_tot_ava_amo            VARCHAR(22),
    v_tot_pag_apo            VARCHAR(22),
    v_tot_pag_amo            VARCHAR(22)

  DEFINE 
    v_folio_reg_pag          DECIMAL(9,0)

  DEFINE
    v_archivo_copia          VARCHAR (30),
    v_comando_dos            STRING

END GLOBALS 

MAIN
  DEFINE 
    r_bandera                SMALLINT
    
  --Asignación de parametros generales
  LET p_usuario        = ARG_VAL(1)
  LET p_pid            = ARG_VAL(2)
  LET p_proceso_cod    = ARG_VAL(3)
  LET p_opera_cod      = ARG_VAL(4)
  LET f_folio          = ARG_VAL(5) --Valor de argumento uno de DISL27

  LET v_modulo_cod = "dis"

  LET v_total_registros = 0

  DISPLAY "Inicia la generación de la interfaces de Avance de Pago vs Pago Compensación. ", TIME
  DISPLAY ""

  DISPLAY "   Inicia etapa 1 identificación de datos. ", TIME
  CALL fn_crea_temporal_folio()
  DISPLAY "   Finaliza etapa 1 identificación de datos. ", TIME

  DISPLAY ""
  DISPLAY "   Inicia etapa 2 salida de datos. ", TIME
  CALL fn_genera_interfase_avances()
  DISPLAY "   Finaliza etapa 2 salida de datos. ", TIME

  --Finaliza la operación
  CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
  RETURNING r_bandera

  IF r_bandera = 0 THEN
     DISPLAY ""
     DISPLAY "Finalizó la generación de la interface de Avance de Pago vs Pago Compensación. ", TIME
     EXIT PROGRAM
  ELSE --Si ocurrió error
     CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) RETURNING r_bandera
     CALL fn_desplega_inc_operacion(r_bandera)
     EXIT PROGRAM
  END IF

END MAIN

#Función para crear la temporal con el filtro de entrada
FUNCTION fn_crea_temporal_folio()
  DEFINE r_bnd               INTEGER,
         v_status_err        INTEGER ,
         v_desc_err          VARCHAR(200)

  WHENEVER ERROR CONTINUE
    PREPARE ps_sp_dis_hs FROM "EXECUTE PROCEDURE fn_ext_ava_vs_comp(?)"
    EXECUTE ps_sp_dis_hs USING f_folio
                          INTO r_bnd, v_status_err, v_desc_err
  WHENEVER ERROR STOP

  DISPLAY "     Función Ext Ava vs Comp ",r_bnd
  DISPLAY "     Código                  ",v_status_err
  DISPLAY "     Mensaje                 ", v_desc_err

  IF r_bnd <> 0 THEN
    DISPLAY "Error en la Ext Ava vs Comp ",v_status_err," ",v_desc_err
    EXIT PROGRAM
  END IF

END FUNCTION 

# Genera un archivo txt con los datos del arreglo
FUNCTION fn_genera_interfase_avances()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    v_nom_archivo_orig       VARCHAR(40),  --Nombre del archivo de salida orig
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_nom_archivo_enc        VARCHAR(40),  --Nombre del archivo de salida enc
    v_nom_archivo_det        VARCHAR(40),  --Nombre del archivo de salida det
    v_nom_archivo_sum        VARCHAR(40),  --Nombre del archivo de salida sum
    v_ddmmaaaa               VARCHAR(08),  --Fecha del archivo de salida
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING,
    v_indice                 INTEGER

  DEFINE
    v_ch_arch_salida_enc     BASE.CHANNEL,
    v_ruta_nomarch_enc       VARCHAR(100), --Ruta y nombre del archivo de salida enc
    v_ruta_nomarch_det       VARCHAR(100), --Ruta y nombre del archivo de salida det
    v_ruta_nomarch_sum       VARCHAR(100)  --Ruta y nombre del archivo de salida sum 

  LET v_modulo_cod = "dis"

  --Se obtienen la ruta envío del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  --Se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/ava_compensa_"       --nombre de archivo
  LET v_nom_archivo_orig  = v_nom_archivo
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy" --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = "ava_compensa_" || v_ddmmaaaa --Concatena nombre a buscar

  --Obtiene consecutivo para archivo por día
  CALL fn_crea_nombre_archivo(v_ruta_envio_dis,v_busca_nom_archivo)
  RETURNING v_cont_dia

  LET v_reg_dia         = v_cont_dia USING "&&&"  --Consecutivo del día de numerico a char
  LET v_nom_archivo     = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
  LET v_ruta_nomarch    = v_ruta_envio_dis CLIPPED || v_nom_archivo

  --ENCABEZADO
  --LET v_ch_arch_salida     = base.Channel.create()
  LET v_nom_archivo_enc    = v_nom_archivo_orig CLIPPED || v_ddmmaaaa || v_reg_dia||"_enc."|| v_modulo_cod
  LET v_ruta_nomarch_enc   = v_ruta_envio_dis CLIPPED || v_nom_archivo_enc
  --DISPLAY "Ruta encabezado: ", v_ruta_nomarch_enc
  LET v_ch_arch_salida_enc = base.Channel.create()

  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida_enc.openFile(v_ruta_nomarch_enc,"w" )
  CALL v_ch_arch_salida_enc.setDelimiter("")

  --Imprime encabezado del archivo
  LET v_encabezado = " FECHA DE CORTE |", TODAY USING "dd-mm-yyyy"
  CALL v_ch_arch_salida_enc.write([v_encabezado])

  --Si se solicitó el folio de dispersión se incluye en el encabezado
  IF f_folio <> 0 THEN
     DISPLAY "Folio Avance de Pagos ", f_folio
     DISPLAY " "
     LET v_encabezado = " FOLIO AVANCE |", f_folio
     CALL v_ch_arch_salida_enc.write([v_encabezado])
  ELSE
     LET v_encabezado = " ARCHIVO MASIVO |"
     CALL v_ch_arch_salida_enc.write([v_encabezado])
  END IF
 
  LET v_encabezado = " FOLIO AVANCE |NSS |NÚMERO CRÉDITO AVANCE |PERIODO PAGO AVANCE |NRP AVANCE |MONTO APORTACIÓN AVANCE |MONTO AMORTIZACIÓN AVANCE |MONTO APORTACIÓN PAGO |MONTO AMORTIZACIÓN PAGO |FECHA PAGO |ESTADO COMPENSACIÓN APORTACIÓN |ESTADO COMPENSACIÓN AMORTIZACIÓN |ESTADO AVANCE |"
  CALL v_ch_arch_salida_enc.write([v_encabezado])

  --Cierra el archivo ENCABEZADO
  CALL v_ch_arch_salida_enc.close()

  LET v_comando_dos = "cat ", v_ruta_nomarch_enc CLIPPED, " >> ", v_ruta_nomarch CLIPPED
  RUN v_comando_dos

  --DETALLE
  LET v_nom_archivo_det  = v_nom_archivo_orig CLIPPED || v_ddmmaaaa || v_reg_dia||"_det."|| v_modulo_cod
  LET v_ruta_nomarch_det = v_ruta_envio_dis CLIPPED || v_nom_archivo_det
  --DISPLAY "Ruta detalle: ", v_ruta_nomarch_det
  
  UNLOAD TO v_ruta_nomarch_det
  SELECT a.folio,
         a.nss,
         a.numero_credito,
         a.periodo_pago,
         a.nrp,
         a.monto_aportacion,
         a.monto_amortizacion,
         a.monto_apo_pag,
         a.monto_amo_pag,
         TO_CHAR(a.f_pago, "%d"  ||'-'||  "%m" ||"-" || "%Y") AS f_pago,
         TRIM(a.edo_compensa_apo || '-' || b.tpo_deudor_desc) AS edo_compensa_apo,
         TRIM(a.edo_compensa_amo || '-' || c.tpo_deudor_desc) AS edo_compensa_amo,
         estado
  FROM   tmp_dis_ava_vs_comp_int a,
  OUTER  dis_cat_tpo_deudor b,
  OUTER  dis_cat_tpo_deudor c
  WHERE  1=1
  AND    a.edo_compensa_apo = b.tpo_deudor
  AND    a.edo_compensa_amo = c.tpo_deudor
  --AND    a.estado           like "70%"
  --AND    a.f_pago           between '01012016' and '12312016'
  --AND    a.nss              = '49806120462'
  
  LET v_comando_dos = "cat ", v_ruta_nomarch_det CLIPPED, " >> ", v_ruta_nomarch CLIPPED
  RUN v_comando_dos
  
  --SUMARIO
  LET v_nom_archivo_sum  = v_nom_archivo_orig CLIPPED || v_ddmmaaaa || v_reg_dia||"_sum."|| v_modulo_cod
  LET v_ruta_nomarch_sum = v_ruta_envio_dis CLIPPED || v_nom_archivo_sum
  --DISPLAY "Ruta sumario: ", v_ruta_nomarch_sum
  
  UNLOAD TO v_ruta_nomarch_sum
  SELECT "TOTALES",
         COUNT(*),
         "","","",
         SUM(monto_aportacion),
         SUM(monto_amortizacion),
         SUM(monto_apo_pag),
         SUM(monto_amo_pag)
  FROM   tmp_dis_ava_vs_comp_int
  --WHERE  estado           like "70%"
  --AND    f_pago           between '01012016' and '12312016'
  --AND    nss = '49806120462'

  LET v_comando_dos = "cat ", v_ruta_nomarch_sum CLIPPED, " >> ", v_ruta_nomarch CLIPPED
  RUN v_comando_dos

  LET v_comando_dos = "rm ", v_ruta_nomarch_enc CLIPPED
  RUN v_comando_dos

  LET v_comando_dos = "rm ", v_ruta_nomarch_det CLIPPED
  RUN v_comando_dos

  LET v_comando_dos = "rm ", v_ruta_nomarch_sum CLIPPED
  RUN v_comando_dos

  {LET v_indice           = 0
  LET v_tot_registros    = 0
  LET v_sum_apo_ava      = 0.00
  LET v_sum_amo_ava      = 0.00
  LET v_sum_apo_pag      = 0.00
  LET v_sum_amo_pag      = 0.00

  DECLARE cur_int_ava_vs_comp CURSOR FOR
  SELECT *
  FROM   tmp_dis_ava_vs_comp_int
  WHERE  1=1
  --ORDER BY 1,2
  FOREACH cur_int_ava_vs_comp INTO v_reg_ava_vs_comp.v_reg_folio_ava,
                                   v_reg_ava_vs_comp.v_reg_nss,
                                   v_reg_ava_vs_comp.v_reg_num_credito,
                                   v_reg_ava_vs_comp.v_reg_periodo_pago,
                                   v_reg_ava_vs_comp.v_reg_nrp,
                                   v_reg_ava_vs_comp.v_reg_monto_apo_ava,
                                   v_reg_ava_vs_comp.v_reg_monto_amo_ava,
                                   v_reg_ava_vs_comp.v_reg_monto_apo_pag,
                                   v_reg_ava_vs_comp.v_reg_monto_amo_pag,
                                   v_reg_ava_vs_comp.v_reg_f_pago,
                                   v_edo_comp_apo,
                                   v_edo_comp_amo,
                                   v_reg_ava_vs_comp.v_reg_edo_avance

    IF v_reg_ava_vs_comp.v_reg_monto_apo_pag IS NULL THEN
       LET v_reg_ava_vs_comp.v_reg_monto_apo_pag = 0
    END IF

    IF v_reg_ava_vs_comp.v_reg_monto_amo_pag IS NULL THEN
       LET v_reg_ava_vs_comp.v_reg_monto_amo_pag = 0
    END IF

    IF v_edo_comp_apo IS NOT NULL THEN
       SELECT ct.tpo_deudor_desc
       INTO   v_desc_edo_comp_apo   
       FROM   dis_cat_tpo_deudor ct
       WHERE  ct.tpo_deudor = v_edo_comp_apo
       IF v_desc_edo_comp_apo IS NULL THEN
          LET v_reg_ava_vs_comp.v_reg_edo_comp_apo = NULL 
       ELSE
          LET v_reg_ava_vs_comp.v_reg_edo_comp_apo = v_edo_comp_apo, "-", v_desc_edo_comp_apo
       END IF
    ELSE
       LET v_reg_ava_vs_comp.v_reg_edo_comp_apo = NULL 
    END IF

    IF v_edo_comp_amo IS NOT NULL THEN
       SELECT ct.tpo_deudor_desc
       INTO   v_desc_edo_comp_amo   
       FROM   dis_cat_tpo_deudor ct
       WHERE  ct.tpo_deudor = v_edo_comp_amo
       IF v_desc_edo_comp_amo IS NULL THEN
          LET v_reg_ava_vs_comp.v_reg_edo_comp_amo = NULL
       ELSE
          LET v_reg_ava_vs_comp.v_reg_edo_comp_amo = v_edo_comp_amo, "-", v_desc_edo_comp_amo
       END IF
    ELSE
       LET v_reg_ava_vs_comp.v_reg_edo_comp_amo = NULL
    END IF

    LET v_tot_registros = v_tot_registros + 1
    LET v_sum_apo_ava   = v_sum_apo_ava   + v_reg_ava_vs_comp.v_reg_monto_apo_ava
    LET v_sum_amo_ava   = v_sum_amo_ava   + v_reg_ava_vs_comp.v_reg_monto_amo_ava
    LET v_sum_apo_pag   = v_sum_apo_pag   + v_reg_ava_vs_comp.v_reg_monto_apo_pag
    LET v_sum_amo_pag   = v_sum_amo_pag   + v_reg_ava_vs_comp.v_reg_monto_amo_pag
    LET v_indice        = v_indice        + 1

    --Concatenación del detalle
    LET v_detalle = v_reg_ava_vs_comp.v_reg_folio_ava, "|",
                    v_reg_ava_vs_comp.v_reg_nss, "|",
                    v_reg_ava_vs_comp.v_reg_num_credito USING "&&&&&&&&&&", "|",
                    v_reg_ava_vs_comp.v_reg_periodo_pago, "|",
                    v_reg_ava_vs_comp.v_reg_nrp, "|",                     
                    v_reg_ava_vs_comp.v_reg_monto_apo_ava, "|",
                    v_reg_ava_vs_comp.v_reg_monto_amo_ava, "|",
                    v_reg_ava_vs_comp.v_reg_monto_apo_pag, "|",
                    v_reg_ava_vs_comp.v_reg_monto_amo_pag, "|", 
                    v_reg_ava_vs_comp.v_reg_f_pago USING "dd-mm-yyyy", "|",
                    v_reg_ava_vs_comp.v_reg_edo_comp_apo CLIPPED, "|",
                    v_reg_ava_vs_comp.v_reg_edo_comp_amo CLIPPED, "|",
                    v_reg_ava_vs_comp.v_reg_edo_avance CLIPPED, "|"
    CALL v_ch_arch_salida.write([v_detalle])
  END FOREACH

  --Escribe el sumario
  LET v_sumario = "TOTALES|",v_tot_registros,"| | | | ",
                  v_sum_apo_ava, "|",
                  v_sum_amo_ava, "|",
                  v_sum_apo_pag, "|",
                  v_sum_amo_pag                  
  CALL v_ch_arch_salida.write([v_sumario])
   
  --Cierra el archivo
  CALL v_ch_arch_salida.close()}
   
  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  DISPLAY "Se ha generado el archivo de Avances de Pagos vs Pagos Compensación\nen la ruta ",v_ruta_nomarch

  --Genera una copia de la interface con el nombre corto
  CALL fn_genera_copia_interface_ava_c(v_ruta_nomarch,v_ruta_envio_dis)
   
END FUNCTION 

FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis,p_busca_nom_archivo)
  DEFINE 
    p_ruta_envio_dis         LIKE seg_modulo.ruta_envio, --Ruta donde se genera archivo
    p_busca_nom_archivo      VARCHAR(40),  --Nombre del archivo a buscar(nombre||fecha)
    v_cmd                    STRING,       --Cadena de comando a ejecutar
    v_consecutivo            INTEGER       --Consecutivo del archivo por día

  DEFINE 
    fn                       CHAR(24)      --Almacena el nombre completo del nombre del archivo en el servidor con su extensión

  DEFINE 
    ch                       base.Channel  --Canal de lectura

  LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

  LET ch = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")

  WHILE ch.read([fn])
    LET v_consecutivo = fn[22,24] --Posición del consecutivo dentro de la cadena
  END WHILE

  CALL ch.close()
  LET v_consecutivo = v_consecutivo + 1  --Incrementa consecutivo del día

  IF length(v_consecutivo) = 0 THEN  --Si es el primero del día
     LET v_consecutivo = 1
  END IF

  RETURN v_consecutivo  --Regresa el consecutivo del siguiente archivo del día

END FUNCTION

--Genera la copia del archivo generado
FUNCTION fn_genera_copia_interface_ava_c(p_archivo_envio,p_ruta_destino)
  DEFINE
    v_cmd                    STRING,
    p_archivo_envio          VARCHAR(100),
    p_ruta_destino           VARCHAR(40)

  LET v_archivo_copia = "ava_compensa"
  LET v_archivo_copia = v_archivo_copia CLIPPED,".dis"

  LET v_cmd = "cat ",p_archivo_envio CLIPPED, " > ",p_ruta_destino CLIPPED, "/",v_archivo_copia CLIPPED
  RUN v_cmd

  LET v_comando_dos = "unix2dos ",p_ruta_destino CLIPPED, " ", v_archivo_copia CLIPPED
  RUN v_comando_dos

  DISPLAY "Se ha realizado la copia de la interface de Avances de Pagos vs Pagos Compensación: ",v_archivo_copia

END FUNCTION