################################################################################
#Versión                    => 1.0.0                                           #
#Fecha última modificacion  => 13/06/2014                                      #
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISS28                                                    #
#Objetivo         => Realizar la interface de pagos por avance                 #
#Fecha de Inicio  => 13/06/2014                                                #
################################################################################
DATABASE safre_viv
GLOBALS

  --Sección de variables del programa
  DEFINE 
    p_usuario                LIKE seg_usuario.usuario_cod,
    p_nom_prog               VARCHAR(30),
    p_tipo_proceso           SMALLINT
      
  --Arreglo para información del detalle de avances cerrados
  DEFINE 
    arr_pago_avance          DYNAMIC ARRAY OF RECORD 
    v_nss                    CHAR(11),
    v_num_credito            DECIMAL(10,0),
    v_bimestre               CHAR(6),
    v_ava_aportacion         DECIMAL(12,2),
    v_ava_amortizacion       DECIMAL(12,2),
    v_pag_aivs               DECIMAL(18,6),
    v_pag_aportacion         DECIMAL(12,2),
    v_pag_amortizacion       DECIMAL(12,2),
    v_folio_sua              DECIMAL(6,0),
    v_ava_apo_pendiente      DECIMAL(12,2),
    v_ava_amo_pendiente      DECIMAL(12,2),
    v_resultado              CHAR(40)
    END RECORD 

  --Variables auxiliares
  DEFINE 
    v_folio_dis              VARCHAR(9),
    v_nss                    CHAR(11),
    v_num_credito            DECIMAL(10,0),
    v_bimestre               CHAR(6),
    v_ava_aportacion         DECIMAL(12,2),
    v_ava_amortizacion       DECIMAL(12,2),
    v_pag_aivs               DECIMAL(18,6),
    v_pag_aportacion         DECIMAL(12,2),
    v_pag_amortizacion       DECIMAL(12,2),
    v_folio_sua              DECIMAL(6,0),
    v_ava_apo_pendiente      DECIMAL(12,2),
    v_ava_amo_pendiente      DECIMAL(12,2),
    v_resultado              CHAR(40)
      
  DEFINE 
    f_folio                  DECIMAL(9,0),
    v_total_registros        BIGINT,
    f_tot_ava_apo            DECIMAL(22,2),
    f_tot_ava_amo            DECIMAL(22,2),
    f_tot_pag_apo            DECIMAL(22,2),
    f_tot_pag_amo            DECIMAL(22,2)

  DEFINE 
    v_tot_registros          VARCHAR(22),
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
   LET f_folio           = ARG_VAL(1) 
   LET v_total_registros = 0
   
   CALL fn_info_avances() RETURNING v_total_registros
   IF v_total_registros = 0 THEN
      DISPLAY "Sin datos para la generación de la interface"
   ELSE
      CALL fn_genera_interfase_avances()
   END IF
END MAIN

#Llena arreglo con información de la temporal
FUNCTION fn_info_avances()

  DEFINE 
    v_total_avances          BIGINT

  DISPLAY "Consulta Interface"
  DISPLAY "" 

  LET v_total_avances = 1

  DECLARE cur_consulta_pag_ava CURSOR FOR
  SELECT pa.nss,
         pa.num_credito,
         pa.bimestre,
         pa.ava_aportacion,
         pa.ava_amortizacion,
         pa.pag_aivs,
         pa.pag_aportacion,
         pa.pag_amortizacion,
         pa.folio_sua,
         pa.ava_apo_pendiente,
         pa.ava_amo_pendiente,
         pa.resultado
  FROM   dis_pago_avance pa
  ORDER BY 1,3
  FOREACH cur_consulta_pag_ava INTO arr_pago_avance[v_total_avances].v_nss,
                                    arr_pago_avance[v_total_avances].v_num_credito,
                                    arr_pago_avance[v_total_avances].v_bimestre,
                                    arr_pago_avance[v_total_avances].v_ava_aportacion,
                                    arr_pago_avance[v_total_avances].v_ava_amortizacion,
                                    arr_pago_avance[v_total_avances].v_pag_aivs,
                                    arr_pago_avance[v_total_avances].v_pag_aportacion,
                                    arr_pago_avance[v_total_avances].v_pag_amortizacion,
                                    arr_pago_avance[v_total_avances].v_folio_sua,
                                    arr_pago_avance[v_total_avances].v_ava_apo_pendiente,
                                    arr_pago_avance[v_total_avances].v_ava_amo_pendiente,
                                    arr_pago_avance[v_total_avances].v_resultado

    LET v_total_avances = v_total_avances + 1
  END FOREACH 

  CALL arr_pago_avance.deleteElement(v_total_avances)
  LET v_total_avances = v_total_avances - 1

  RETURN v_total_avances
END FUNCTION 


# Genera un archivo txt con los datos del arreglo
FUNCTION fn_genera_interfase_avances()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    v_nom_archivo            VARCHAR(40), --nombre del archivo de salida
    v_ddmmaaaa               VARCHAR(08), --fecha del archivo de salida
    v_busca_nom_archivo      STRING,      --busca nombre de archivo
    v_cont_dia               SMALLINT, --consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03), --Parametro consecutivo de registro por dia
    v_ruta_nomarch           VARCHAR(100), --ruta y nombre del archivo de salida
    v_encabezado             STRING,
    v_detalle                STRING,
    v_recorre_arreglo        BIGINT,
    v_precio_fondo           DECIMAL(19,14)

  LET v_modulo_cod = "dis"

  SELECT f.precio_fondo
  INTO   v_precio_fondo
  FROM   glo_valor_fondo f
  WHERE  f.fondo       = 11
  AND    f.f_valuacion = TODAY

  --Se obtienen la ruta envío del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  -- se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/pagos_avance_sal_" --nombre de archivo
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy"  --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = "pagos_avance_sal_" || v_ddmmaaaa  --Concatena nombre a buscar

  --Obtiene consecutivo para archivo por día
  CALL fn_crea_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  RETURNING v_cont_dia

  LET v_reg_dia        = v_cont_dia USING "&&&"  --Consecutivo del día de numerico a char
  LET v_nom_archivo    = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
  LET v_ruta_nomarch   = v_ruta_envio_dis CLIPPED || v_nom_archivo
  LET v_ch_arch_salida = base.Channel.create()
   
  -- se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch, "w" )
  CALL v_ch_arch_salida.setDelimiter("")

  --Imprime encabezado del archivo
  LET v_encabezado = " FECHA DE ARCHIVO |", TODAY USING "dd-mm-yyyy"
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_encabezado = " PRECIO FONDO AIVS |", v_precio_fondo
  CALL v_ch_arch_salida.write([v_encabezado])
  
  LET v_encabezado = " NSS |NÚMERO CRÉDITO AVANCE |PERIODO PAGO AVANCE |MONTO APORTACIÓN AVANCE |MONTO AMORTIZACIÓN AVANCE |MONTO AIVS PAGO |MONTO APORTACIÓN PAGO |MONTO AMORTIZACIÓN PAGO |FOLIO SUA PAGO |DIFERENCIAS APORTACIÓN |DIFERENCIAS AMORTIZACIÓN |RESULTADO |"
  CALL v_ch_arch_salida.write([v_encabezado])

  #Escribe en archivo datos de los avances
  FOR v_recorre_arreglo = 1 TO arr_pago_avance.getLength()
      LET v_nss               = arr_pago_avance[v_recorre_arreglo].v_nss USING "&&&&&&&&&&&"
      LET v_num_credito       = arr_pago_avance[v_recorre_arreglo].v_num_credito USING "&&&&&&&&&&"
      LET v_bimestre          = arr_pago_avance[v_recorre_arreglo].v_bimestre
      LET v_ava_aportacion    = arr_pago_avance[v_recorre_arreglo].v_ava_aportacion
      LET v_ava_amortizacion  = arr_pago_avance[v_recorre_arreglo].v_ava_amortizacion
      LET v_pag_aivs          = arr_pago_avance[v_recorre_arreglo].v_pag_aivs
      LET v_pag_aportacion    = arr_pago_avance[v_recorre_arreglo].v_pag_aportacion
      LET v_pag_amortizacion  = arr_pago_avance[v_recorre_arreglo].v_pag_amortizacion
      LET v_folio_sua         = arr_pago_avance[v_recorre_arreglo].v_folio_sua --USING "&&&&&&&&&"
      LET v_ava_apo_pendiente = arr_pago_avance[v_recorre_arreglo].v_ava_apo_pendiente
      LET v_ava_amo_pendiente = arr_pago_avance[v_recorre_arreglo].v_ava_amo_pendiente
      LET v_resultado         = arr_pago_avance[v_recorre_arreglo].v_resultado

      LET v_detalle           = v_nss, "|",
                                v_num_credito, "|",
                                v_bimestre, "|",
                                v_ava_aportacion, "|",
                                v_ava_amortizacion, "|",
                                v_pag_aivs, "|",
                                v_pag_aportacion, "|",
                                v_pag_amortizacion, "|",
                                v_folio_sua, "|",
                                v_ava_apo_pendiente, "|", 
                                v_ava_amo_pendiente, "|", 
                                v_resultado

      CALL v_ch_arch_salida.write([v_detalle])
  END FOR 

  --Cierra el archivo
  CALL v_ch_arch_salida.close()
   
  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ", v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  DISPLAY "Se ha generado el archivo de Pagos por Avance\nen la ruta ", v_ruta_nomarch

  --Genera una copia de la interface con el nombre corto
  CALL fn_genera_copia_interface_ava_c(v_ruta_nomarch,v_ruta_envio_dis)
   
END FUNCTION 

FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis, p_busca_nom_archivo)
  DEFINE 
    p_ruta_envio_dis         LIKE seg_modulo.ruta_envio,  --Ruta donde se genera archivo
    p_busca_nom_archivo      VARCHAR(40),  --Nombre del archivo a buscar(nombre||fecha)
    v_cmd                    STRING,  --Cadena de comando a ejecutar
    v_consecutivo            INTEGER  --Consecutivo del archivo por día

  DEFINE 
    fn                       CHAR(28)  --Almacena el nombre completo del nombre del archivo en el servidor con su extensión

  DEFINE 
    ch                       base.Channel  --Canal de lectura

  LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

  LET ch = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")

  WHILE ch.read([fn])
    LET v_consecutivo = fn[26,28]  --Posición del consecutivo dentro de la cadena
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
        v_cmd                STRING,
        p_archivo_envio      VARCHAR(100),
        p_ruta_destino       VARCHAR(40)

    --DISPLAY "p_archivo_envio: ",p_archivo_envio
    --DISPLAY "p_ruta_destino: ",p_ruta_destino
    
    LET v_archivo_copia = "pagos_avance_sal"
    LET v_archivo_copia = v_archivo_copia CLIPPED,".dis"
    --DISPLAY "v_archivo_copia -- ",v_archivo_copia

    LET v_cmd = "cat ",p_archivo_envio CLIPPED, " > ",p_ruta_destino CLIPPED, "/",v_archivo_copia CLIPPED

    --DISPLAY "v_cmd -- ",v_cmd

    RUN v_cmd

    LET v_comando_dos = "unix2dos ",p_ruta_destino CLIPPED, " ", v_archivo_copia CLIPPED
    RUN v_comando_dos

    DISPLAY ""
    DISPLAY "Se ha realizado la copia del archivo de Pagos por Avance: ",v_archivo_copia

END FUNCTION