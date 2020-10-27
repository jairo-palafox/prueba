################################################################################
#Versión                    => 1.0.0                                           #
#Fecha última modificacion  => 26/01/2014                                      5
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISS34                                                    #
#Objetivo         => Realizar la interface de dispersión cartera               #
#Fecha de Inicio  => 26/01/2015                                                #
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
    arr_dis_cartera          DYNAMIC ARRAY OF RECORD 
         v_nss                CHAR(11),
         v_num_credito        DECIMAL(10,0),
         v_nrp                CHAR(11),
         v_f_pago             DATE, 
         v_monto_apo          DECIMAL(12,2),
         v_monto_amo          DECIMAL(12,2),
         v_folio_sua          DECIMAL(6,0),
         v_periodo_pago       CHAR(6),
         v_estado             SMALLINT
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

   CALL STARTLOG("DISS34.log")
   
   CALL fn_info_avances() RETURNING v_total_registros
   IF v_total_registros = 0 THEN
      DISPLAY "Sin datos para la generación de la interface"
   ELSE
      CALL fn_genera_interfase_dis_cartera()
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
  SELECT  *
  FROM   tmp_dis_cons_hs 
  ORDER BY estado ASC, nss ASC, periodo_pago DESC
  FOREACH cur_consulta_pag_ava INTO arr_dis_cartera[v_total_avances].v_nss,
                                    arr_dis_cartera[v_total_avances].v_num_credito,
                                    arr_dis_cartera[v_total_avances].v_nrp,
                                    arr_dis_cartera[v_total_avances].v_f_pago,
                                    arr_dis_cartera[v_total_avances].v_monto_apo,
                                    arr_dis_cartera[v_total_avances].v_monto_amo,
                                    arr_dis_cartera[v_total_avances].v_folio_sua,
                                    arr_dis_cartera[v_total_avances].v_periodo_pago,
                                    arr_dis_cartera[v_total_avances].v_estado

    LET v_total_avances = v_total_avances + 1
  END FOREACH 

  CALL arr_dis_cartera.deleteElement(v_total_avances)
  LET v_total_avances = v_total_avances - 1

  RETURN v_total_avances
END FUNCTION 


# Genera un archivo txt con los datos del arreglo
FUNCTION fn_genera_interfase_dis_cartera()
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
    v_sumario                STRING,
    v_recorre_arreglo        BIGINT,
    v_precio_fondo           DECIMAL(19,14)

DEFINE   v_nss                CHAR(11),
         v_num_credito        DECIMAL(10,0),
         v_nrp                CHAR(11),
         v_f_pago             DATE, 
         v_monto_apo          DECIMAL(12,2),
         v_monto_amo          DECIMAL(12,2),
         v_folio_sua          DECIMAL(6,0),
         v_periodo_pago       CHAR(6),
         v_estado             SMALLINT

DEFINE v_desc_estado          VARCHAR(30)

DEFINE v_tot_nss              DECIMAL(9,0),
       v_sum_pag_apo          DECIMAL(12,2),
       v_sum_pag_amo          DECIMAL(12,2)

  LET v_modulo_cod = "dis"

  --Se obtienen la ruta envío del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  -- se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/dis_cons_cartera_sal_" --nombre de archivo
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy"  --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = "dis_cons_cartera_sal_" || v_ddmmaaaa  --Concatena nombre a buscar

  --Obtiene consecutivo para archivo por día
  CALL fn_crea_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  RETURNING v_cont_dia

  
  SELECT COUNT(dis_hs.nss),
         SUM(dis_hs.monto_apo),
         SUM(dis_hs.monto_amo) 
  INTO   v_tot_nss,
         v_sum_pag_apo,
         v_sum_pag_amo 
  FROM   tmp_dis_cons_hs dis_hs

  
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
  
  LET v_encabezado = " NSS |NÚMERO CRÉDITO |NRP |FECHA DE PAGO |MONTO APORTACIÓN |MONTO APORTACIÓN |FOLIO SUA |PERIODO PAGO |ESTADO |"
  CALL v_ch_arch_salida.write([v_encabezado])

  #Escribe en archivo datos de los avances
  FOR v_recorre_arreglo = 1 TO arr_dis_cartera.getLength()
  
      LET v_nss            = arr_dis_cartera[v_recorre_arreglo].v_nss  USING "&&&&&&&&&&&"
      LET v_num_credito    = arr_dis_cartera[v_recorre_arreglo].v_num_credito USING "&&&&&&&&&&"
      LET v_nrp            = arr_dis_cartera[v_recorre_arreglo].v_nrp
      LET v_f_pago         = arr_dis_cartera[v_recorre_arreglo].v_f_pago USING "dd-mm-yyyy"
      LET v_monto_apo      = arr_dis_cartera[v_recorre_arreglo].v_monto_apo USING "---,---,---,--&.&&"
      LET v_monto_amo      = arr_dis_cartera[v_recorre_arreglo].v_monto_amo USING "---,---,---,--&.&&"
      LET v_folio_sua      = arr_dis_cartera[v_recorre_arreglo].v_folio_sua
      LET v_periodo_pago   = arr_dis_cartera[v_recorre_arreglo].v_periodo_pago
      LET v_estado         = arr_dis_cartera[v_recorre_arreglo].v_estado


      INITIALIZE v_desc_estado TO NULL
      CASE v_estado
         WHEN 0
            LET v_desc_estado = " - NSS NO EXISTE EN BD"
         WHEN 1
            LET v_desc_estado = " - NSS DISP A CARTERA"
         WHEN 2
            LET v_desc_estado = " - NSS DEV SALDOS"
         WHEN 3
            LET v_desc_estado = " - NSS SIN DISPERSIÓN"
      END CASE
      
      LET v_detalle  = v_nss CLIPPED, "| ",
                       v_num_credito CLIPPED , " | ",
                       v_nrp CLIPPED, " | ",
                       v_f_pago CLIPPED, " | ",
                       v_monto_apo CLIPPED, " | ",
                       v_monto_amo CLIPPED, " | ",
                       v_folio_sua CLIPPED, " | ",
                       v_periodo_pago CLIPPED, " | ",
                       v_estado CLIPPED," ",v_desc_estado CLIPPED, " |"

      CALL v_ch_arch_salida.write([v_detalle])
  END FOR 

  --Imprime sumario del archivo
  LET v_sumario = " "
  CALL v_ch_arch_salida.write([v_sumario])
  
  LET v_sumario = " TOTAL DE REGISTROS GENERADOS: | ", v_tot_nss USING "###,###,##&"
  CALL v_ch_arch_salida.write([v_sumario])
  
  LET v_sumario = " MONTO TOTAL APORTACIÓN: | ", v_sum_pag_apo USING "---,###,###,##&.&&"
  CALL v_ch_arch_salida.write([v_sumario])
   
  LET v_sumario = " MONTO TOTAL AMORTIZACIÓN: | ", v_sum_pag_amo USING "---,###,###,##&.&&"
  CALL v_ch_arch_salida.write([v_sumario])

  --Cierra el archivo
  CALL v_ch_arch_salida.close()
   
  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ", v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  DISPLAY "Se ha generado el archivo de Dispersión a Cartera\nen la ruta ", v_ruta_nomarch

  --Genera una copia de la interface con el nombre corto
  CALL fn_genera_copia_interface_dis_cartera(v_ruta_nomarch,v_ruta_envio_dis)
   
END FUNCTION 

FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis, p_busca_nom_archivo)
  DEFINE 
    p_ruta_envio_dis         LIKE seg_modulo.ruta_envio,  --Ruta donde se genera archivo
    p_busca_nom_archivo      VARCHAR(40),  --Nombre del archivo a buscar(nombre||fecha)
    v_cmd                    STRING,  --Cadena de comando a ejecutar
    v_consecutivo            INTEGER  --Consecutivo del archivo por día

  DEFINE 
    fn                       CHAR(32)  --Almacena el nombre completo del nombre del archivo en el servidor con su extensión

  DEFINE 
    ch                       base.Channel  --Canal de lectura

  LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

  LET ch = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")

  WHILE ch.read([fn])
    LET v_consecutivo = fn[30,32]  --Posición del consecutivo dentro de la cadena
  END WHILE

  CALL ch.close()
  LET v_consecutivo = v_consecutivo + 1  --Incrementa consecutivo del día

  IF length(v_consecutivo) = 0 THEN  --Si es el primero del día
     LET v_consecutivo = 1
  END IF

  RETURN v_consecutivo  --Regresa el consecutivo del siguiente archivo del día

END FUNCTION

--Genera la copia del archivo generado
FUNCTION fn_genera_copia_interface_dis_cartera(p_archivo_envio,p_ruta_destino)
    DEFINE
        v_cmd                STRING,
        p_archivo_envio      VARCHAR(100),
        p_ruta_destino       VARCHAR(40)

    --DISPLAY "p_archivo_envio: ",p_archivo_envio
    --DISPLAY "p_ruta_destino: ",p_ruta_destino
    
    LET v_archivo_copia = "dis_cons_cartera_sal"
    LET v_archivo_copia = v_archivo_copia CLIPPED,".dis"
    --DISPLAY "v_archivo_copia -- ",v_archivo_copia

    LET v_cmd = "cat ",p_archivo_envio CLIPPED, " > ",p_ruta_destino CLIPPED, "/",v_archivo_copia CLIPPED

    --DISPLAY "v_cmd -- ",v_cmd

    RUN v_cmd

    LET v_comando_dos = "unix2dos ",p_ruta_destino CLIPPED, " ", v_archivo_copia CLIPPED
    RUN v_comando_dos

    DISPLAY ""
    DISPLAY "Se ha realizado la copia del archivo de Dispersión a Cartera: ",v_archivo_copia

END FUNCTION