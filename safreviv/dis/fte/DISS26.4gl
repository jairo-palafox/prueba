################################################################################
#Versión                    => 1.0.0                                           #
#Fecha última modificacion  => 29/04/2014                                      #
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISS26                                                    #
#Objetivo         => Generar la interface masiva de los avances abiertos con   #
#                    pago por cubrir.                                          #
#Fecha de Inicio  => 29/04/2014                                                #
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
    v_sum_aportacion         DECIMAL(12,2), 
    v_sum_amortizacion       DECIMAL(12,2),
    v_sum_aivs               DECIMAL(18,6),
    v_sum_apo_pag            DECIMAL(12,2), 
    v_sum_amo_pag            DECIMAL(12,2)
    
  --Arreglo para información del detalle de avances abiertos
  DEFINE v_arr_ava_por_cub   DYNAMIC ARRAY OF RECORD
    v_arr_nss                CHAR(11),
    v_arr_nombre_completo    VARCHAR(50),
    v_arr_folio_dis          DECIMAL(9,0),
    v_arr_periodo_pago       CHAR(6),
    v_arr_nrp                CHAR(11),     
    v_arr_num_credito        DECIMAL(10,0),
    v_arr_monto_aportacion   DECIMAL(12,2),
    v_arr_monto_amortizacion DECIMAL(12,2), 
    v_arr_folio_pag          DECIMAL(9,0),
    v_arr_aivs               DECIMAL(18,6),
    v_arr_monto_apo_pag      DECIMAL(12,2),
    v_arr_monto_amo_pag      DECIMAL(12,2),     
    v_arr_id_derecho         DECIMAL(9,0)
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
    v_f_pago                 VARCHAR(8)
      
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
  LET f_folio          = ARG_VAL(5) --Valor de argumento uno de DISL18

  LET v_modulo_cod = "dis"

  LET v_total_registros = 0
   
  CALL fn_crea_temporal_folio()
  CALL fn_info_avances() 
  CALL fn_genera_interfase_avances()

  --Finaliza la operación
  CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
  RETURNING r_bandera

  IF r_bandera = 0 THEN
     DISPLAY "Finalizo la generación de la interface de Avance Abiertos con Pagos por Cubrir."
     EXIT PROGRAM
  ELSE --Si ocurrió error
     CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) RETURNING r_bandera
     CALL fn_desplega_inc_operacion(r_bandera)
     EXIT PROGRAM
  END IF

END MAIN

#Función para crear la temporal con el filtro de entrada
FUNCTION fn_crea_temporal_folio()

  DATABASE safre_viv
  ##### Se añade modificación de la variable de informix para optimización de consulta #####
   
  --Actualizamos variable de informix para maximizar prioridad de la BD
  LET v_qwery_ibx = "SET PDQPRIORITY HIGH;"
  PREPARE prp_performance FROM v_qwery_ibx
  EXECUTE prp_performance

  WHENEVER ERROR CONTINUE;
  DROP TABLE tmp_dis_ava_con_pago_mas;

  IF f_folio = 0 THEN
     SELECT af.nss nss,
            rtrim(af.nombre_af) ||' '||
            rtrim(af.ap_paterno_af) ||' '||
            rtrim(af.ap_materno_af) nombre_completo,
            av.folio,
            av.periodo_pago,
            av.nrp,
            av.num_credito,
            av.monto_aportacion,
            av.monto_amortizacion,
            af.id_derechohabiente
     FROM   dis_det_avance_pago av,
            afi_derechohabiente af
     WHERE  av.id_derechohabiente = af.id_derechohabiente
     AND    av.estado             = 30
     INTO TEMP tmp_dis_ava_con_pago_mas
  ELSE
     SELECT af.nss nss,
            rtrim(af.nombre_af) ||' '||
            rtrim(af.ap_paterno_af) ||' '||
            rtrim(af.ap_materno_af) nombre_completo,
            av.folio,
            av.periodo_pago,
            av.nrp,
            av.num_credito,
            av.monto_aportacion,
            av.monto_amortizacion,
            af.id_derechohabiente
     FROM   dis_det_avance_pago av,
            afi_derechohabiente af
     WHERE  av.folio              = f_folio
     AND    av.id_derechohabiente = af.id_derechohabiente
     AND    av.estado             = 30
     INTO TEMP tmp_dis_ava_con_pago_mas  
  END IF
  
  EXECUTE IMMEDIATE "CREATE INDEX xdis_ava_con_pag_mas ON tmp_dis_ava_con_pago_mas(id_derechohabiente, periodo_pago, nrp) in dis_ix_dbs;"

  UPDATE STATISTICS FOR TABLE tmp_dis_ava_con_pago_mas;

END FUNCTION 

#Llena arreglo con información de la temporal
FUNCTION fn_info_avances()
  DEFINE 
    v_indice                 INTEGER,
    v_val_fol                DECIMAL(9,0)

  LET g_sql_txt = "\n SELECT ap.nss, ",
                  "\n        ap.nombre_completo, ",
                  "\n        ap.folio, ",
                  "\n        ap.periodo_pago, ",
                  "\n        ap.nrp, ",
                  "\n        ap.num_credito, ",
                  "\n        ap.monto_aportacion, ",
                  "\n        ap.monto_amortizacion, ",
                  "\n        pg.folio, ",
                  "\n        pg.aiv_ap_pat, ",
                  "\n        pg.imp_ap_pat, ",
                  "\n        pg.imp_am_cre, ",
                  "\n        pg.id_derechohabiente ",
                  "\n FROM   tmp_dis_ava_con_pago_mas ap, ",
                  "\n        cta_his_pagos pg ",
                  "\n WHERE  pg.id_derechohabiente             = ap.id_derechohabiente ",
                  "\n AND    fn_bimestre_pago(pg.periodo_pago) = ap.periodo_pago ",
                  "\n AND    pg.nrp                            = ap.nrp ",
                  "\n AND    pg.ind_liquidacion           NOT IN (1,6) ",
                  "\n AND    pg.destino_ap_viv                 = 1",
                  "\n ORDER BY 1,3"

  PREPARE prp_sql_inconsistencia FROM g_sql_txt
  
  LET v_indice           = 1
  LET v_tot_registros    = 1
  LET v_sum_aportacion   = 0.00
  LET v_sum_amortizacion = 0.00
  LET v_sum_apo_pag      = 0.00
  LET v_sum_amo_pag      = 0.00
  LET v_sum_aivs         = 0.00

  --Iteración de registros con base en la consulta temporal
  DECLARE cur_inconsistencia CURSOR FOR prp_sql_inconsistencia
  FOREACH cur_inconsistencia INTO v_arr_ava_por_cub[v_indice].v_arr_nss,
                                  v_arr_ava_por_cub[v_indice].v_arr_nombre_completo,
                                  v_arr_ava_por_cub[v_indice].v_arr_folio_dis,
                                  v_arr_ava_por_cub[v_indice].v_arr_periodo_pago,
                                  v_arr_ava_por_cub[v_indice].v_arr_nrp,
                                  v_arr_ava_por_cub[v_indice].v_arr_num_credito,
                                  v_arr_ava_por_cub[v_indice].v_arr_monto_aportacion,
                                  v_arr_ava_por_cub[v_indice].v_arr_monto_amortizacion,
                                  v_arr_ava_por_cub[v_indice].v_arr_folio_pag,
                                  v_arr_ava_por_cub[v_indice].v_arr_aivs,
                                  v_arr_ava_por_cub[v_indice].v_arr_monto_apo_pag,
                                  v_arr_ava_por_cub[v_indice].v_arr_monto_amo_pag,
                                  v_arr_ava_por_cub[v_indice].v_arr_id_derecho

    IF v_arr_ava_por_cub[v_indice].v_arr_aivs IS NULL THEN
       LET v_arr_ava_por_cub[v_indice].v_arr_aivs = 0
    END IF

    IF v_arr_ava_por_cub[v_indice].v_arr_monto_amo_pag IS NULL THEN
       LET v_arr_ava_por_cub[v_indice].v_arr_monto_amo_pag = 0
    END IF

    --Si las aivs y amortizaciones de los pagos son menores o
    --iguales a cero no se despliega el registro
    IF (v_arr_ava_por_cub[v_indice].v_arr_aivs          <= 0.00  AND 
        v_arr_ava_por_cub[v_indice].v_arr_monto_amo_pag <= 0.00) THEN
       CONTINUE FOREACH
    END IF

    SELECT gf.folio
    INTO   v_val_fol
    FROM   glo_folio gf
    WHERE  gf.folio        = v_arr_ava_por_cub[v_indice].v_arr_folio_pag
    AND    gf.folio       IN ( SELECT fg.folio_referencia
                               FROM   glo_folio fg
                               WHERE  fg.proceso_cod           = 901
                               AND    fg.status                = 2
                               AND    fg.folio_referencia IS NOT NULL)
    AND    gf.status       = 2
    AND    gf.proceso_cod IN (1401,1403)
    IF v_val_fol IS NULL THEN
       CONTINUE FOREACH
    ELSE
       LET v_val_fol = NULL
    END IF

    LET v_tot_registros    = v_tot_registros    + 1
    LET v_sum_aivs         = v_sum_aivs         + v_arr_ava_por_cub[v_indice].v_arr_aivs
    LET v_sum_aportacion   = v_sum_aportacion   + v_arr_ava_por_cub[v_indice].v_arr_monto_aportacion
    LET v_sum_amortizacion = v_sum_amortizacion + v_arr_ava_por_cub[v_indice].v_arr_monto_amortizacion
    LET v_sum_apo_pag      = v_sum_apo_pag      + v_arr_ava_por_cub[v_indice].v_arr_monto_apo_pag
    LET v_sum_amo_pag      = v_sum_amo_pag      + v_arr_ava_por_cub[v_indice].v_arr_monto_amo_pag
    LET v_indice           = v_indice           + 1

  END FOREACH

  CALL v_arr_ava_por_cub.deleteElement(v_indice)
  LET v_tot_registros    = v_tot_registros - 1

END FUNCTION 

# Genera un archivo txt con los datos del arreglo
FUNCTION fn_genera_interfase_avances()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_ddmmaaaa               VARCHAR(08),  --Fecha del archivo de salida
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING,
    v_indice                 INTEGER

  LET v_modulo_cod = "dis"

  --Se obtienen la ruta envío del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  --Se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/cons_mas_ava_por_cub" --nombre de archivo
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy"  --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = "cons_mas_ava_por_cub" || v_ddmmaaaa  --Concatena nombre a buscar

  --Obtiene consecutivo para archivo por día
  CALL fn_crea_nombre_archivo(v_ruta_envio_dis,v_busca_nom_archivo)
  RETURNING v_cont_dia

  LET v_reg_dia        = v_cont_dia USING "&&&"  --Consecutivo del día de numerico a char
  LET v_nom_archivo    = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
  LET v_ruta_nomarch   = v_ruta_envio_dis CLIPPED || v_nom_archivo
  LET v_ch_arch_salida = base.Channel.create()
   
  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  --Imprime encabezado del archivo
  LET v_encabezado = " FECHA DE CORTE |",TODAY
  CALL v_ch_arch_salida.write([v_encabezado])

  --Si se solicitó el folio de dispersión se incluye en el encabezado
  IF f_folio <> 0 THEN
     DISPLAY "Folio Avance de Pagos ", f_folio
     DISPLAY " "
     LET v_encabezado = " FOLIO AVANCE |",f_folio
     CALL v_ch_arch_salida.write([v_encabezado])
  END IF
  
  LET v_encabezado = " NSS |NOMBRE COMPLETO |FOLIO AVANCE |PERIODO PAGO AVANCE|NRP AVANCE |NÚMERO DE CRÉDITO AVANCE |MONTO APORTACIÓN AVANCE |MONTO AMORTIZACIÓN AVANCE |FOLIO PAGOS |AIVS |MONTO APORTACIÓN PAGO |MONTO AMORTIZACIÓN PAGO|"
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_indice = 1
 
  FOR v_indice = 1 TO v_arr_ava_por_cub.getLength()
      --Concatenación del detalle
      LET v_detalle = v_arr_ava_por_cub[v_indice].v_arr_nss, "|",
                      v_arr_ava_por_cub[v_indice].v_arr_nombre_completo, "|",
                      v_arr_ava_por_cub[v_indice].v_arr_folio_dis, "|",
                      v_arr_ava_por_cub[v_indice].v_arr_periodo_pago, "|",
                      v_arr_ava_por_cub[v_indice].v_arr_nrp, "|",                     
                      v_arr_ava_por_cub[v_indice].v_arr_num_credito USING "&&&&&&&&&&", "|",
                      v_arr_ava_por_cub[v_indice].v_arr_monto_aportacion, "|",
                      v_arr_ava_por_cub[v_indice].v_arr_monto_amortizacion, "|",
                      v_arr_ava_por_cub[v_indice].v_arr_folio_pag, "|",
                      v_arr_ava_por_cub[v_indice].v_arr_aivs, "|",
                      v_arr_ava_por_cub[v_indice].v_arr_monto_apo_pag, "|",
                      v_arr_ava_por_cub[v_indice].v_arr_monto_amo_pag, "|"      
      CALL v_ch_arch_salida.write([v_detalle])
  END FOR

  --Escribe el sumario
  LET v_sumario = "TOTALES|",v_tot_registros,"| | | | | ",
                  v_sum_aportacion, "|",
                  v_sum_amortizacion, "| |",
                  v_sum_aivs, "|",
                  v_sum_apo_pag, "|",
                  v_sum_amo_pag                  
  CALL v_ch_arch_salida.write([v_sumario])
   
  --Cierra el archivo
  CALL v_ch_arch_salida.close()
   
  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  DISPLAY "Se ha generado el archivo de Avances Abiertos con Pagos por Cubrir\nen la ruta ",v_ruta_nomarch

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
    fn                       CHAR(22)      --Almacena el nombre completo del nombre del archivo en el servidor con su extensión

  DEFINE 
    ch                       base.Channel  --Canal de lectura

  LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

  LET ch = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")

  WHILE ch.read([fn])
    LET v_consecutivo = fn[16,18] --Posición del consecutivo dentro de la cadena
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

  LET v_archivo_copia = "cons_mas_ava_por_cub"
  LET v_archivo_copia = v_archivo_copia CLIPPED,".dis"

  LET v_cmd = "cat ",p_archivo_envio CLIPPED, " > ",p_ruta_destino CLIPPED, "/",v_archivo_copia CLIPPED
  RUN v_cmd

  LET v_comando_dos = "unix2dos ",p_ruta_destino CLIPPED, " ", v_archivo_copia CLIPPED
  RUN v_comando_dos

  DISPLAY "Se ha realizado la copia de la interface de Avances Abiertos con Pagos por Cubrir: ",v_archivo_copia

END FUNCTION