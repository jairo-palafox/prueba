################################################################################
#Versión                    => 1.0.0                                           #
#Fecha última modificacion  => 29/07/2015                                      5
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISS41                                                    #
#Objetivo         => Realizar la interface de la Consulta de Pagos             #
#                    Históricos.                                               #
#Fecha de Inicio  => 27/07/2015                                                #
################################################################################
DATABASE safre_viv
GLOBALS

  --Sección de variables del programa
   DEFINE 
    p_usuario                LIKE seg_usuario.usuario_cod,
    p_nom_prog               VARCHAR(30),
    p_tipo_proceso           SMALLINT
      
  --Arreglo para información del detalle de consulta de pagos históricos
   DEFINE arr_dis_cons_pag_hist   DYNAMIC ARRAY OF RECORD 
            tpo_registro         SMALLINT,
            nss                  CHAR(11),
            num_credito          DECIMAL(10,0),
            periodo_pago         CHAR(06),
            f_pago               DATE,
            nrp                  CHAR(11),
            monto_apo            DECIMAL(12,2),
            monto_amo            DECIMAL(12,2),
            folio_sua            DECIMAL(6,0),
            edo_pago             VARCHAR(80),
            folio_proceso        DECIMAL(9,0),
            f_folio_proceso      DATE,
            f_interface          DATE,
            tpo_interface        CHAR(04),
            id_dis_cons_pag_hist DECIMAL(9,0)
          END RECORD 

   DEFINE arr_dis_mov_cons_pag_h DYNAMIC ARRAY OF RECORD
            tpo_registro         SMALLINT,
            nss                  CHAR(11),
            folio                DECIMAL(9,0),
            f_pag_mov            DATE,
            movimiento           VARCHAR(50),
            id_dis_cons_pag_hist DECIMAL(9,0)
         END RECORD

  --Variables auxiliares
   DEFINE v_tpo_registro         SMALLINT,
          v_nss                  CHAR(11),
          v_num_credito          DECIMAL(10,0),
          v_periodo_pago         CHAR(06),
          v_f_pago               DATE,
          v_nrp                  CHAR(11),
          v_monto_apo            DECIMAL(12,2),
          v_monto_amo            DECIMAL(12,2),
          v_folio_sua            DECIMAL(6,0),
          v_edo_pago             VARCHAR(80),
          v_folio_proceso        DECIMAL(9,0),
          v_f_folio_proceso      DATE,
          v_f_interface          DATE,
          v_tpo_interface        CHAR(04)

   DEFINE v_tpo_registro_h       SMALLINT,
          v_nss_h                CHAR(11),
          v_folio                DECIMAL(9,0),
          v_f_pag_mov            DATE,
          v_movimiento           VARCHAR(50)
            
   DEFINE f_folio                  DECIMAL(9,0),
          v_total_registros        BIGINT,
          v_total_mov              INTEGER,
          f_tot_ava_apo            DECIMAL(22,2),
          f_tot_ava_amo            DECIMAL(22,2),
          f_tot_pag_apo            DECIMAL(22,2),
          f_tot_pag_amo            DECIMAL(22,2)

   DEFINE v_tot_registros          VARCHAR(22),
          v_tot_ava_apo            VARCHAR(22),
          v_tot_ava_amo            VARCHAR(22),
          v_tot_pag_apo            VARCHAR(22),
          v_tot_pag_amo            VARCHAR(22)

   DEFINE v_folio_reg_pag          DECIMAL(9,0)

   DEFINE v_archivo_copia          VARCHAR (30),
          v_comando_dos            STRING

END GLOBALS 

MAIN 
   LET f_folio           = ARG_VAL(1) 
   LET v_total_registros = 0
   LET v_total_mov       = 0

   CALL STARTLOG ("DISS41.log")
   
   CALL fn_info_avances() RETURNING v_total_registros, v_total_mov
   
   IF v_total_registros = 0 THEN
      DISPLAY "Sin datos para la generación de la interface"
   ELSE
      CALL fn_genera_interface_pag_hist(v_total_registros, v_total_mov)
   END IF
END MAIN

#Llena arreglo con información de la temporal
FUNCTION fn_info_avances()

   DEFINE v_ind    BIGINT,
          v_ind1   INTEGER

   DISPLAY "Consulta Interface"
   DISPLAY "" 

   LET v_ind = 1

   DECLARE cur_cons_pag_hist CURSOR FOR
   SELECT 1,
          pagh.nss,
          pagh.num_credito, 
          pagh.periodo_pago,
          pagh.f_pago,
          pagh.nrp,
          pagh.monto_aportacion,
          pagh.monto_amortizacion,
          pagh.folio_sua,
          pagh.estado,
          pagh.folio,
          glo.f_actualiza,
          TODAY,
          "CPHT",
          pagh.id_dis_cons_pag_hist
   FROM  dis_cons_pag_hist pagh,
         glo_folio glo
   WHERE glo.folio = pagh.folio
   ORDER BY pagh.nss ASC, pagh.id_dis_cons_pag_hist ASC, pagh.estado ASC, pagh.periodo_pago DESC
  
   FOREACH cur_cons_pag_hist INTO arr_dis_cons_pag_hist[v_ind].tpo_registro,
                                  arr_dis_cons_pag_hist[v_ind].nss,
                                  arr_dis_cons_pag_hist[v_ind].num_credito,
                                  arr_dis_cons_pag_hist[v_ind].periodo_pago,
                                  arr_dis_cons_pag_hist[v_ind].f_pago,
                                  arr_dis_cons_pag_hist[v_ind].nrp,
                                  arr_dis_cons_pag_hist[v_ind].monto_apo,
                                  arr_dis_cons_pag_hist[v_ind].monto_amo,
                                  arr_dis_cons_pag_hist[v_ind].folio_sua,
                                  arr_dis_cons_pag_hist[v_ind].edo_pago,
                                  arr_dis_cons_pag_hist[v_ind].folio_proceso,
                                  arr_dis_cons_pag_hist[v_ind].f_folio_proceso,
                                  arr_dis_cons_pag_hist[v_ind].f_interface,
                                  arr_dis_cons_pag_hist[v_ind].tpo_interface,
                                  arr_dis_cons_pag_hist[v_ind].id_dis_cons_pag_hist

      LET v_ind = v_ind + 1
   END FOREACH 

  CALL arr_dis_cons_pag_hist.deleteElement(v_ind)
  LET v_ind = v_ind - 1

   LET v_ind1 = 1

   DECLARE cur_cons_mov_pag_h CURSOR FOR
   SELECT 2,
          afi.nss,
          mov.folio,
          mov.f_referencia, 
          mov.movimiento,
          mov.id_dis_cons_pag_hist
   FROM  dis_mov_cons_pag_h mov,
         afi_derechohabiente afi
   WHERE afi.id_derechohabiente = mov.id_derechohabiente
   ORDER BY nss ASC, id_dis_cons_pag_hist ASC, movimiento ASC
  
   FOREACH cur_cons_mov_pag_h INTO arr_dis_mov_cons_pag_h[v_ind1].tpo_registro,
                                   arr_dis_mov_cons_pag_h[v_ind1].nss,
                                   arr_dis_mov_cons_pag_h[v_ind1].folio,
                                   arr_dis_mov_cons_pag_h[v_ind1].f_pag_mov,
                                   arr_dis_mov_cons_pag_h[v_ind1].movimiento,
                                   arr_dis_mov_cons_pag_h[v_ind1].id_dis_cons_pag_hist

      LET v_ind1 = v_ind1 + 1
   END FOREACH 

  CALL arr_dis_mov_cons_pag_h.deleteElement(v_ind1)
  LET v_ind1 = v_ind1 - 1

  RETURN v_ind, v_ind1
END FUNCTION 


# Genera un archivo txt con los datos del arreglo
FUNCTION fn_genera_interface_pag_hist(p_pag_tot, p_mov_tot)
   DEFINE p_pag_tot     INTEGER, 
          p_mov_tot     INTEGER
      
   DEFINE v_ch_arch_salida         BASE.CHANNEL,
          v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
          v_modulo_cod             LIKE seg_modulo.modulo_cod,
          v_nom_archivo            VARCHAR(60), --nombre del archivo de salida
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

   DEFINE v_desc_mov          VARCHAR(50)

   DEFINE v_tot_nss              DECIMAL(9,0),
          v_sum_pag_apo          DECIMAL(12,2),
          v_sum_pag_amo          DECIMAL(12,2)

   DEFINE i    INTEGER

  LET v_modulo_cod = "dis"

  --Se obtienen la ruta envío del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  LET v_nom_archivo       = "/dis_ext_cpht_" --nombre de archivo
  LET v_ddmmaaaa          = TODAY USING "yyyymmdd"  --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = v_nom_archivo||v_ddmmaaaa||"_"||f_folio||".dis"  --Concatena nombre a buscar

  --CALL fn_obtiene_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo, f_folio)
  --RETURNING v_nom_archivo

  LET v_nom_archivo = v_busca_nom_archivo
  --DISPLAY "v_nom_archivo: ", v_nom_archivo

  --LET v_reg_dia        = v_cont_dia USING "&&&"  --Consecutivo del día de numerico a char
  --LET v_nom_archivo    = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod

   LET v_ruta_nomarch   = v_ruta_envio_dis CLIPPED || v_nom_archivo
   LET v_ch_arch_salida = base.Channel.create()
   
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_salida.openFile(v_ruta_nomarch, "w" )
   CALL v_ch_arch_salida.setDelimiter("")

   {--Imprime encabezado del archivo
   LET v_encabezado = " FECHA DE ARCHIVO |", TODAY USING "dd-mm-yyyy"
   CALL v_ch_arch_salida.write([v_encabezado])
  
   LET v_encabezado = " NSS |NÚMERO CRÉDITO |NRP |FECHA DE PAGO |MONTO APORTACIÓN |MONTO APORTACIÓN |FOLIO SUA |PERIODO PAGO |ESTADO |"
   CALL v_ch_arch_salida.write([v_encabezado])}

  #Escribe en archivo datos de los avances
  FOR v_recorre_arreglo = 1 TO arr_dis_cons_pag_hist.getLength()
  
      LET v_tpo_registro   = arr_dis_cons_pag_hist[v_recorre_arreglo].tpo_registro    
      LET v_nss            = arr_dis_cons_pag_hist[v_recorre_arreglo].nss      
      LET v_num_credito    = arr_dis_cons_pag_hist[v_recorre_arreglo].num_credito    
      LET v_periodo_pago   = arr_dis_cons_pag_hist[v_recorre_arreglo].periodo_pago      
      LET v_f_pago         = arr_dis_cons_pag_hist[v_recorre_arreglo].f_pago     
      LET v_nrp            = arr_dis_cons_pag_hist[v_recorre_arreglo].nrp      
      LET v_monto_apo      = arr_dis_cons_pag_hist[v_recorre_arreglo].monto_apo 
      LET v_monto_amo      = arr_dis_cons_pag_hist[v_recorre_arreglo].monto_amo
      LET v_folio_sua      = arr_dis_cons_pag_hist[v_recorre_arreglo].folio_sua  
      --LET v_edo_pago       = arr_dis_cons_pag_hist[v_recorre_arreglo].edo_pago  
      LET v_folio_proceso  = arr_dis_cons_pag_hist[v_recorre_arreglo].folio_proceso      
      LET v_f_folio_proceso   = arr_dis_cons_pag_hist[v_recorre_arreglo].f_folio_proceso     
      LET v_f_interface       = arr_dis_cons_pag_hist[v_recorre_arreglo].f_interface    
      LET v_tpo_interface     = arr_dis_cons_pag_hist[v_recorre_arreglo].tpo_interface 


      
      CASE arr_dis_cons_pag_hist[v_recorre_arreglo].edo_pago
         WHEN 0
            LET v_edo_pago = arr_dis_cons_pag_hist[v_recorre_arreglo].edo_pago CLIPPED ||" - NSS NO EXISTE COMO DERECHOHABIENTE"
         WHEN 10
            LET v_edo_pago = arr_dis_cons_pag_hist[v_recorre_arreglo].edo_pago CLIPPED ||" - PAGO NO ESTA REGISTRADO"
         WHEN 20
            LET v_edo_pago = arr_dis_cons_pag_hist[v_recorre_arreglo].edo_pago CLIPPED ||" - PAGO REGISTRADO"
         WHEN 30
            LET v_edo_pago = arr_dis_cons_pag_hist[v_recorre_arreglo].edo_pago CLIPPED ||" - ABONO POR REGISTRO DE PAGOS"
         WHEN 40
            LET v_edo_pago = arr_dis_cons_pag_hist[v_recorre_arreglo].edo_pago CLIPPED ||" - CARGO POR RETIROS"
         WHEN 50
            LET v_edo_pago = arr_dis_cons_pag_hist[v_recorre_arreglo].edo_pago CLIPPED ||" - CARGO POR DISPERSIÓN"
         OTHERWISE
         
      END CASE
      
      LET v_detalle  =  v_tpo_registro CLIPPED, "| ",  
                        v_nss USING "&&&&&&&&&&&" CLIPPED, "| ",           
                        v_num_credito USING "&&&&&&&&&&" CLIPPED, "| ",   
                        v_periodo_pago CLIPPED, "| ",  
                        v_f_pago USING "yyyymmdd" CLIPPED, "| ",        
                        v_nrp CLIPPED, "| ",            
                        v_monto_apo USING "---,---,---,--&.&&" CLIPPED, "| ",     
                        v_monto_amo USING "---,---,---,--&.&&" CLIPPED, "| ",     
                        v_folio_sua CLIPPED, "| ",     
                        v_edo_pago CLIPPED, "| ",      
                        v_folio_proceso CLIPPED, "| ", 
                        v_f_folio_proceso USING "yyyymmdd" CLIPPED, "| ",  
                        v_f_interface USING "yyyymmdd" CLIPPED, "| ",      
                        v_tpo_interface CLIPPED, "| "


      --CALL v_ch_arch_salida.write([v_detalle])

      IF arr_dis_cons_pag_hist.getLength() = 1 THEN  -- SOlo existe un registro en los pagos historicos
      
         CALL v_ch_arch_salida.write([v_detalle])
         
         IF p_mov_tot >= 1 THEN -- EXISTEN REGISTROS en la tabla dis_mov_cons_pag_h

            FOR i = 1 TO arr_dis_mov_cons_pag_h.getLength()
            
               IF (arr_dis_cons_pag_hist[v_recorre_arreglo].nss = arr_dis_mov_cons_pag_h[i].nss) AND 
                   (arr_dis_cons_pag_hist[v_recorre_arreglo].id_dis_cons_pag_hist = 
                    arr_dis_mov_cons_pag_h[i].id_dis_cons_pag_hist) THEN


                  LET v_tpo_registro_h  = arr_dis_mov_cons_pag_h[i].tpo_registro      
                  LET v_nss_h           = arr_dis_mov_cons_pag_h[i].nss   
                  LET v_folio           = arr_dis_mov_cons_pag_h[i].folio   
                  LET v_f_pag_mov       = arr_dis_mov_cons_pag_h[i].f_pag_mov    
                  --LET v_movimiento      = arr_dis_mov_cons_pag_h[i].movimiento

                  IF arr_dis_mov_cons_pag_h[i].movimiento = "0" THEN
                     LET v_desc_mov = "PAGO REGISTRADO"
                  ELSE
                     INITIALIZE v_desc_mov TO NULL
                     SELECT movimiento_desc
                     INTO v_desc_mov
                     FROM cat_movimiento
                     WHERE movimiento = arr_dis_mov_cons_pag_h[i].movimiento
                  END IF

                  LET v_movimiento      = arr_dis_mov_cons_pag_h[i].movimiento CLIPPED||" - "||v_desc_mov

                  LET v_detalle  =  v_tpo_registro_h CLIPPED, "| ",  
                                    v_nss_h USING "&&&&&&&&&&&" CLIPPED, "| ",           
                                    v_folio CLIPPED, "| ",   
                                    v_f_pag_mov  USING "yyyymmdd" CLIPPED, "| ",  
                                    v_movimiento CLIPPED, "| " 

                  CALL v_ch_arch_salida.write([v_detalle])
                  
               END IF -- IF Existen registros en tabla dis_mov_cons_pag_h con mismo NSS

            END FOR  --FOR tabla dis_mov_cons_pag_h

         END IF

      ELSE  -- Existen varios registros de pagos históricos
      
         IF v_recorre_arreglo < arr_dis_cons_pag_hist.getLength()THEN

            CALL v_ch_arch_salida.write([v_detalle])
            
            --IF arr_dis_cons_pag_hist[v_recorre_arreglo].nss <> arr_dis_cons_pag_hist[v_recorre_arreglo + 1].nss THEN

               IF p_mov_tot >= 1 THEN -- EXISTEN REGISTROS en la tabla dis_mov_cons_pag_h

                  FOR i = 1 TO arr_dis_mov_cons_pag_h.getLength()
                  
                    IF (arr_dis_cons_pag_hist[v_recorre_arreglo].nss = arr_dis_mov_cons_pag_h[i].nss) AND 
                       (arr_dis_cons_pag_hist[v_recorre_arreglo].id_dis_cons_pag_hist = 
                        arr_dis_mov_cons_pag_h[i].id_dis_cons_pag_hist) THEN


                        LET v_tpo_registro_h  = arr_dis_mov_cons_pag_h[i].tpo_registro      
                        LET v_nss_h           = arr_dis_mov_cons_pag_h[i].nss   
                        LET v_folio           = arr_dis_mov_cons_pag_h[i].folio   
                        LET v_f_pag_mov       = arr_dis_mov_cons_pag_h[i].f_pag_mov    
                        --LET v_movimiento      = arr_dis_mov_cons_pag_h[i].movimiento

                        IF arr_dis_mov_cons_pag_h[i].movimiento = "0" THEN
                           LET v_desc_mov = "PAGO REGISTRADO"
                        ELSE
                           INITIALIZE v_desc_mov TO NULL
                           SELECT movimiento_desc
                           INTO v_desc_mov
                           FROM cat_movimiento
                           WHERE movimiento = arr_dis_mov_cons_pag_h[i].movimiento
                        END IF

                        LET v_movimiento      = arr_dis_mov_cons_pag_h[i].movimiento CLIPPED||" - "||v_desc_mov

                        LET v_detalle  =  v_tpo_registro_h CLIPPED, "| ",  
                                          v_nss_h USING "&&&&&&&&&&&" CLIPPED, "| ",           
                                          v_folio CLIPPED, "| ",   
                                          v_f_pag_mov  USING "yyyymmdd" CLIPPED, "| ",  
                                          v_movimiento CLIPPED, "| " 

                        CALL v_ch_arch_salida.write([v_detalle])
                        
                     END IF -- IF Existen registros en tabla dis_mov_cons_pag_h con mismo NSS

                  END FOR  --FOR tabla dis_mov_cons_pag_h

               END IF

            --END IF  -- IF CAMBIA DE NSS
            
         
         ELSE  --- ULTIMO REGISTRO DE pagos
         
            CALL v_ch_arch_salida.write([v_detalle])

            IF p_mov_tot >= 1 THEN -- EXISTEN REGISTROS en la tabla dis_mov_cons_pag_h

               FOR i = 1 TO arr_dis_mov_cons_pag_h.getLength()
               
                  IF (arr_dis_cons_pag_hist[v_recorre_arreglo].nss = arr_dis_mov_cons_pag_h[i].nss) AND 
                     (arr_dis_cons_pag_hist[v_recorre_arreglo].id_dis_cons_pag_hist = 
                      arr_dis_mov_cons_pag_h[i].id_dis_cons_pag_hist) THEN


                     LET v_tpo_registro_h  = arr_dis_mov_cons_pag_h[i].tpo_registro      
                     LET v_nss_h           = arr_dis_mov_cons_pag_h[i].nss   
                     LET v_folio           = arr_dis_mov_cons_pag_h[i].folio   
                     LET v_f_pag_mov       = arr_dis_mov_cons_pag_h[i].f_pag_mov    
                     --LET v_movimiento      = arr_dis_mov_cons_pag_h[i].movimiento

                     IF arr_dis_mov_cons_pag_h[i].movimiento = "0" THEN
                        LET v_desc_mov = "PAGO REGISTRADO"
                     ELSE
                        INITIALIZE v_desc_mov TO NULL
                        SELECT movimiento_desc
                        INTO v_desc_mov
                        FROM cat_movimiento
                        WHERE movimiento = arr_dis_mov_cons_pag_h[i].movimiento
                     END IF

                     LET v_movimiento      = arr_dis_mov_cons_pag_h[i].movimiento CLIPPED||" - "||v_desc_mov

                     LET v_detalle  =  v_tpo_registro_h CLIPPED, "| ",  
                                       v_nss_h USING "&&&&&&&&&&&" CLIPPED, "| ",           
                                       v_folio CLIPPED, "| ",   
                                       v_f_pag_mov  USING "yyyymmdd" CLIPPED, "| ",  
                                       v_movimiento CLIPPED, "| " 

                     CALL v_ch_arch_salida.write([v_detalle])
                     
                  END IF -- IF Existen registros en tabla dis_mov_cons_pag_h con mismo NSS

               END FOR  --FOR tabla dis_mov_cons_pag_h

            END IF
            
         END IF

      END IF

      
      
      {IF p_mov_tot >= 1 THEN -- EXISTEN REGISTROS en la tabla dis_mov_cons_pag_h

         IF arr_dis_cons_pag_hist.getLength() > 1 THEN --Existe mas de un NSS

            CALL v_ch_arch_salida.write([v_detalle])
            
            IF v_recorre_arreglo > 1 THEN --Indice >= 2
            
               IF arr_dis_cons_pag_hist[v_recorre_arreglo].nss <> arr_dis_cons_pag_hist[v_recorre_arreglo - 1].nss THEN

                  FOR i = 1 TO arr_dis_mov_cons_pag_h.getLength()
                     IF arr_dis_cons_pag_hist[v_recorre_arreglo].nss = arr_dis_mov_cons_pag_h[i].nss THEN


                        LET v_tpo_registro_h  = arr_dis_mov_cons_pag_h[i].tpo_registro      
                        LET v_nss_h           = arr_dis_mov_cons_pag_h[i].nss   
                        LET v_folio           = arr_dis_mov_cons_pag_h[i].folio   
                        LET v_f_pag_mov       = arr_dis_mov_cons_pag_h[i].f_pag_mov    
                        --LET v_movimiento      = arr_dis_mov_cons_pag_h[i].movimiento

                        INITIALIZE v_desc_mov TO NULL
                        SELECT movimiento_desc
                        INTO v_desc_mov
                        FROM cat_movimiento
                        WHERE movimiento = arr_dis_mov_cons_pag_h[i].movimiento

                        LET v_movimiento      = arr_dis_mov_cons_pag_h[i].movimiento CLIPPED||" - "||v_desc_mov

                        LET v_detalle  =  v_tpo_registro_h CLIPPED, "| ",  
                                          v_nss_h USING "&&&&&&&&&&&" CLIPPED, "| ",           
                                          v_folio CLIPPED, "| ",   
                                          v_f_pag_mov  USING "yyyymmdd" CLIPPED, "| ",  
                                          v_movimiento CLIPPED, "| " 

                        CALL v_ch_arch_salida.write([v_detalle])
                        
                     END IF -- IF Existen registros en tabla dis_mov_cons_pag_h con mismo NSS

                  END FOR  --FOR tabla dis_mov_cons_pag_h

               END IF  -- IF CAMBIA DE NSS

            END IF
            
         ELSE --UN solo NSS pero existe informacion en tabla dis_mov_cons_pag_h

            CALL v_ch_arch_salida.write([v_detalle])
            
            FOR i = 1 TO arr_dis_mov_cons_pag_h.getLength()
               IF arr_dis_cons_pag_hist[v_recorre_arreglo].nss = arr_dis_mov_cons_pag_h[i].nss THEN


                  LET v_tpo_registro_h  = arr_dis_mov_cons_pag_h[i].tpo_registro      
                  LET v_nss_h           = arr_dis_mov_cons_pag_h[i].nss   
                  LET v_folio           = arr_dis_mov_cons_pag_h[i].folio   
                  LET v_f_pag_mov       = arr_dis_mov_cons_pag_h[i].f_pag_mov    
                  --LET v_movimiento      = arr_dis_mov_cons_pag_h[i].movimiento

                  INITIALIZE v_desc_mov TO NULL
                  SELECT movimiento_desc
                  INTO v_desc_mov
                  FROM cat_movimiento
                  WHERE movimiento = arr_dis_mov_cons_pag_h[i].movimiento

                  LET v_movimiento      = arr_dis_mov_cons_pag_h[i].movimiento CLIPPED||" - "||v_desc_mov

                  LET v_detalle  =  v_tpo_registro_h CLIPPED, "| ",  
                                    v_nss_h USING "&&&&&&&&&&&" CLIPPED, "| ",           
                                    v_folio CLIPPED, "| ",   
                                    v_f_pag_mov  USING "yyyymmdd" CLIPPED, "| ",  
                                    v_movimiento CLIPPED, "| " 

                  CALL v_ch_arch_salida.write([v_detalle])
                  
               END IF -- IF Existen registros en tabla dis_mov_cons_pag_h con mismo NSS

            END FOR  --FOR tabla dis_mov_cons_pag_h

         END IF --IF mas de un NSS

      ELSE
         CALL v_ch_arch_salida.write([v_detalle])
      END IF --IF existe info en tabla dis_mov_cons_pag_h}
      
  END FOR 

  {--Imprime sumario del archivo
  LET v_sumario = " "
  CALL v_ch_arch_salida.write([v_sumario])
  
  LET v_sumario = " TOTAL DE REGISTROS GENERADOS: | ", v_tot_nss USING "###,###,##&"
  CALL v_ch_arch_salida.write([v_sumario])
  
  LET v_sumario = " MONTO TOTAL APORTACIÓN: | ", v_sum_pag_apo USING "---,###,###,##&.&&"
  CALL v_ch_arch_salida.write([v_sumario])
   
  LET v_sumario = " MONTO TOTAL AMORTIZACIÓN: | ", v_sum_pag_amo USING "---,###,###,##&.&&"
  CALL v_ch_arch_salida.write([v_sumario])
  }

  --Cierra el archivo
  CALL v_ch_arch_salida.close()
   
  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ", v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  DISPLAY "Se ha generado el archivo de Consulta Pagos Históricos\nen la ruta ", v_ruta_nomarch

  --Genera una copia de la interface con el nombre corto
  --CALL fn_genera_copia_interface_cons_pag_hist(v_ruta_nomarch,v_ruta_envio_dis)
   
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
FUNCTION fn_genera_copia_interface_cons_pag_hist(p_archivo_envio,p_ruta_destino)
    DEFINE
        v_cmd                STRING,
        p_archivo_envio      VARCHAR(100),
        p_ruta_destino       VARCHAR(40)

    --DISPLAY "p_archivo_envio: ",p_archivo_envio
    --DISPLAY "p_ruta_destino: ",p_ruta_destino
    
    LET v_archivo_copia = "dis_ext_cpht"
    LET v_archivo_copia = v_archivo_copia CLIPPED,".dis"
    --DISPLAY "v_archivo_copia -- ",v_archivo_copia

    LET v_cmd = "cat ",p_archivo_envio CLIPPED, " > ",p_ruta_destino CLIPPED, "/",v_archivo_copia CLIPPED

    --DISPLAY "v_cmd -- ",v_cmd

    RUN v_cmd

    LET v_comando_dos = "unix2dos ",p_ruta_destino CLIPPED, " ", v_archivo_copia CLIPPED
    RUN v_comando_dos

    DISPLAY ""
    DISPLAY "Se ha realizado la copia del archivo de Consulta Pagos Históricos: ",v_archivo_copia

END FUNCTION