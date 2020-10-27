






CREATE FUNCTION "safreviv".fn_deo_integra_op98(p_usuario_cod CHAR(20), p_pid DECIMAL(9,0), p_folio  DECIMAL(10), p_nombre_archivo CHAR(40))
   RETURNING SMALLINT,     -- sql_error
             INTEGER,      -- isam_error
             VARCHAR(255), -- mensaje
             SMALLINT,     -- numero de afores aceptadas
             SMALLINT      -- numero de afores rechazadas

-- folio que quedara en la operacion
DEFINE v_id_detalle                    DECIMAL(11,0);
-- variables para validar totales por afore
DEFINE v_cve_afore_archivo             SMALLINT;
DEFINE v_importe_archivo               DECIMAL(20,2);
DEFINE v_importe_capturado             DECIMAL(20,2);
DEFINE v_f_valor_devolucion            CHAR(8); -- la fecha viene como un char se tiene que convertir
DEFINE v_op98_f_afectacion_contable    CHAR(8); -- la fecha viene como un char se tiene que convertir
DEFINE v_dte_f_valor_devolucion        DATE; -- fecha en formato DATE
-- detalle Op98 en safre_viv
DEFINE v_detop98_id_detalle            DECIMAL(11,0) ;
DEFINE v_detop98_folio                 DECIMAL(9,0) ;
DEFINE v_detop98_curp                  CHAR(18)     ;
DEFINE v_detop98_tipo_autocorreccion   SMALLINT     ;
DEFINE v_detop98_id_derechohabiente    DECIMAL(9,0) ;
DEFINE v_detop98_acc_devol_viv97       DECIMAL(16,6); -- cambio precision
DEFINE v_detop98_nombre_trabajador     CHAR(50)     ;
DEFINE v_detop98_acc_devol_viv92       DECIMAL(16,6); -- cambio precision
DEFINE v_detop98_remanente             SMALLINT     ;
DEFINE v_detop98_f_valor_devolucion    DATE         ;
DEFINE v_detop98_pes_devol_viv97       DECIMAL(12,2); -- cambio precision
DEFINE v_detop98_pes_devol_viv92       DECIMAL(12,2); -- cambio precision
DEFINE v_detop98_pes_int_devol_viv92   DECIMAL(12,2); -- cambio precision
DEFINE v_detop98_f_valor_recep_afore   DATE         ;
DEFINE v_detop98_f_valor_devol_inf     DATE         ;
DEFINE v_detop98_pes_int_devol_viv97   DECIMAL(12,2); -- cambio precision
DEFINE v_detop98_rfc                   CHAR(13)     ;
DEFINE v_detop98_estado_devolucion     SMALLINT     ;
DEFINE v_detop98_resultado_operacion   SMALLINT     ;
DEFINE v_detop98_cve_afore             SMALLINT     ;
DEFINE v_detop98_f_afectacion_contable DATE         ;

-- variables de detalle de op98 en safre_tmp
DEFINE v_op98_tpo_registro             char(2)      ;
DEFINE v_op98_id_servicio              char(2)      ;
DEFINE v_op98_id_operacion             char(2)      ;
DEFINE v_op98_nss                      char(11)     ;
DEFINE v_op98_curp                     char(18)     ;
DEFINE v_op98_nombre_trabajador        char(50)     ;
DEFINE v_op98_proc_origen_devolucion   CHAR(3)      ;
DEFINE v_op98_num_aplic_interes_97     decimal(16,6); -- cambio precision
DEFINE v_op98_num_aplic_interes_92     decimal(16,6); -- cambio precision
DEFINE v_op98_filler1                  char(26)     ;
DEFINE v_op98_remanente                char(2)      ;
DEFINE v_op98_filler2                  char(10)     ;
DEFINE v_op98_f_valor_devolucion       char(8)      ;
DEFINE v_op98_cve_afore                decimal(3,0) ;
DEFINE v_op98_filler3                  char(8)      ;
DEFINE v_op98_importe_viv97            decimal(20,2) ;
DEFINE v_op98_importe_viv92            decimal(20,2) ;
DEFINE v_op98_importe_interes_viv92    decimal(20,2) ;
DEFINE v_op98_f_valor_recepcion        char(8)      ;
DEFINE v_op98_filler4                  char(14)     ;
DEFINE v_op98_importe_interes_viv97    decimal(20,2) ;
DEFINE v_op98_filler5                  char(26)     ;
DEFINE v_op98_rfc_trabajador           char(13)     ;
DEFINE v_op98_result_operacion         char(2)      ;
DEFINE v_op98_motivo_rechazo1          char(3)      ;
DEFINE v_op98_motivo_rechazo2          char(3)      ;
DEFINE v_op98_motivo_rechazo3          char(3)      ;
DEFINE v_op98_filler6                  char(19)     ;
-- ====================================================
-- encabezado en produccion deo_cza_op98
DEFINE v_deocza_op98_folio                decimal(9,0);
DEFINE v_deocza_op98_nombre_archivo       char(18)    ;
DEFINE v_deocza_op98_f_operacion_procesar date        ;
DEFINE v_deocza_op98_f_carga_afore        date        ;

-- =============================================
-- variables de control del stored procedure
DEFINE v_b_exito                       SMALLINT; -- booleana para indicar si el proceso termino bien
DEFINE v_b_registro_correcto           SMALLINT; -- booleana para indicar si un registro se inserta
DEFINE v_id_derechohabiente            DECIMAL(9,0); -- ID de derechohabiente asociado a un NSS
DEFINE v_si_dia                        SMALLINT;
DEFINE v_si_mes                        SMALLINT;
DEFINE v_si_ano                        SMALLINT;
--
DEFINE v_si_rechazo_1                  SMALLINT;
DEFINE v_si_rechazo_2                  SMALLINT;
DEFINE v_si_rechazo_3                  SMALLINT;
DEFINE v_si_contador                   SMALLINT;
DEFINE v_si_cont_nss                   SMALLINT; -- verifica que todos los nss esten registrados
DEFINE v_si_rachazo_afore              SMALLINT;
DEFINE v_si_rachazo_total_afore        SMALLINT;
DEFINE v_id_detalle_a                  DECIMAL(11,0);
DEFINE v_id_detalle_b                  DECIMAL(11,0);

DEFINE v_si_correcto_integra           SMALLINT;
DEFINE v_si_estado                     SMALLINT;
DEFINE v_d_importe_ivs                 DECIMAL(16,2); -- cambio precision
DEFINE v_c_cadena_errores              VARCHAR(26);
DEFINE v_si_conteo_errores             SMALLINT;
DEFINE v_si_correcciones               SMALLINT;
DEFINE v_si_contErroers                SMALLINT;

-- numero de afores aceptadas y rechazadas
DEFINE v_afores_aceptadas  SMALLINT;
DEFINE v_afores_rechazadas SMALLINT;

 -- Control de Excepciones
 DEFINE sql_err                         INTEGER;
 DEFINE isam_err                        INTEGER;
 DEFINE err_txt                         VARCHAR(255);
 DEFINE v_c_msj                         VARCHAR(255);
 DEFINE v_i_resultado                   SMALLINT;

  -- se establece el valor de retorno de las excepciones
 ON EXCEPTION SET sql_err, isam_err, err_txt
    LET v_i_resultado = sql_err;
    
    RETURN v_i_resultado, isam_err, err_txt, v_afores_aceptadas, v_afores_rechazadas;
 END EXCEPTION

 --SET DEBUG FILE TO 'trace.11.02.2012.txt';
 
 -- se asume que el proceso termino correctamente
 LET v_i_resultado = 0;
 LET isam_err = 0;
 LET v_c_msj = 'El proceso finalizó correctamente';
 
 -- se inicia sin afores aceptadas o rechazadas
 LET v_afores_aceptadas  = 0;
 LET v_afores_rechazadas = 0;
 
 -- Se asigna el folio al archivo y se indica que ha sido integrado
 UPDATE glo_ctr_archivo
    SET folio = p_folio,
        estado = 2 -- integrado
  WHERE proceso_cod    = 801
    AND opera_cod      = 1 -- archivo cargado
    AND estado         = 1; -- etapa de carga
 
 UPDATE bat_ctr_operacion
    SET folio = p_folio
  WHERE proceso_cod = 801
    AND opera_cod   = 2
    AND pid         = p_pid;

 -- Contador de errores
 LET v_si_conteo_errores = 0;
  
 -- Indica que NO hay registros correctamente integrados.
 LET v_si_correcto_integra = 0;

 -- se preparan los datos para insertar el registro de encabezado
 LET v_deocza_op98_folio                = p_folio; --decimal(9,0)
 LET v_deocza_op98_nombre_archivo       = p_nombre_archivo; --char(18)    
 LET v_deocza_op98_f_operacion_procesar = TODAY; --date        
 LET v_deocza_op98_f_carga_afore        = TODAY; --date        

 -- se inserta el registro de encabezado
 INSERT INTO deo_cza_op98(
             folio                ,
             nombre_archivo       ,
             f_operacion_procesar ,
             f_carga_afore        )
        VALUES(
          v_deocza_op98_folio               ,
          v_deocza_op98_nombre_archivo      ,
          v_deocza_op98_f_operacion_procesar,
          v_deocza_op98_f_carga_afore        );

   -- Se obtiene el identificador de detalle para cada registro integrado.
   ---  En registros correctos
   SELECT MAX(id_detalle)
     INTO v_id_detalle_a
     FROM deo_det_op98;
   
   IF ( v_id_detalle_a IS NULL ) THEN
      LET v_id_detalle_a = 0;
   END IF
   
   -- Se obtiene el identificador de detalle para cada registro integrado.
   -- En registros rechazados
   SELECT MAX(id_detalle)
     INTO v_id_detalle_b
     FROM deo_det_op98_rch;
   
   IF ( v_id_detalle_b IS NULL ) THEN
      LET v_id_detalle_b = 0;
   END IF
   
   -- Obtener el mayor del registro id_detalle
   IF ( v_id_detalle_a > v_id_detalle_b ) THEN
      LET v_id_detalle = v_id_detalle_a;
   ELSE
   	  LET v_id_detalle = v_id_detalle_b;
   END IF

   -- Inicializa lista de errores.
   LET v_c_cadena_errores= '                          ';
   
 LET v_si_contErroers = 0;
 -- se validan las cifras totales por afore
 FOREACH
   SELECT cve_afore, f_valor_devolucion, 
          SUM( (importe_viv97/100) + (importe_viv92/100) )
     INTO v_cve_afore_archivo, v_f_valor_devolucion,
          v_importe_archivo
   FROM safre_tmp:tmp_detalle_op98
   GROUP BY cve_afore, f_valor_devolucion
   ORDER BY cve_afore, f_valor_devolucion

   -- se transforma la fecha consulta en formato de tipo de dato DATE
   LET v_si_dia = v_f_valor_devolucion[7,8];
   LET v_si_mes = v_f_valor_devolucion[5,6];
   LET v_si_ano = v_f_valor_devolucion[1,4];
   LET v_dte_f_valor_devolucion = MDY(v_si_mes, v_si_dia, v_si_ano);

   LET v_si_contErroers = v_si_contErroers + 1;  
   
   -- se obtiene el monto capturado para la afore dada y fecha de devolucion
   SELECT SUM(tot_pes_devolucion)
     INTO v_importe_capturado
     FROM deo_mto_deposito
    WHERE cve_afore = v_cve_afore_archivo
      AND f_valor_devol_inf = v_dte_f_valor_devolucion;
    
   -- Se inicializan Importes totales de afore correctos
   LET v_si_rachazo_total_afore = 0;
   
   --TRACE("Antes ->v_si_contErroers  :"||v_si_contErroers);
   --TRACE("v_si_rachazo_total_afore    :"||v_si_rachazo_total_afore);
   
   -- si los montos no coinciden, la AFORE no se puede procesar
   IF ( v_importe_archivo <> v_importe_capturado ) THEN
      -- Se envían todos los registros de la afore a rechazados
      LET v_si_rachazo_total_afore = 1;
      --TRACE("# # # # # # # # # # # # # # :");
      --TRACE("Rechazo Total diferencia importes:");
      --TRACE("v_importe_archivo           :"||v_importe_archivo);
      --TRACE("v_importe_capturado         :"||v_importe_capturado);
      
      -- el importe del archivo es diferente al importe capturado
      LET isam_err = 0;
      LET v_c_msj = 'El importe encontrado en el archivo es diferente al importe capturado';
   END IF
   
   --------------------------------------------------------------
   -- VERIFICA RECHAZO TOTAL MEDIANTE VALIDACION DE IMPORTES.
   --------------------------------------------------------------
   -- Validar toda la afore para el caso de que un registro
   -- tenga algun importe invalido.
   -- Para este caso se verfican los datos cargados de la afore
   -- y se rechazan todos sus registros
   --------------------------------------------------------------
   
   LET v_si_contador = 0;
   SELECT COUNT(*)
     INTO v_si_contador
     FROM TABLE(MULTISET(
   SELECT cve_afore, rechazo1, rechazo2, rechazo3
     FROM TABLE(MULTISET(
    SELECT cve_afore,
       (CASE  WHEN (importe_viv97 <= 0 AND importe_viv92 <= 0)
         THEN 2 ELSE 0 END) AS rechazo1
       , 0 AS rechazo2 --(CASE WHEN (importe_viv97 > 0 ) THEN
           --( CASE WHEN (importe_interes_viv97 <= 0) THEN 3 ELSE 0 END )   --- Ya no se validan los intereses SACI2018-23
         --ELSE 0 END) AS rechazo2
       , 0 AS rechazo3 --(CASE WHEN (importe_viv92 > 0 ) THEN 
          --( CASE WHEN (importe_interes_viv92 <= 0) THEN 4 ELSE 0 END )    --- Ya no se validan los intereses SACI2018-23
        --ELSE 0 END) AS rechazo3
      FROM safre_tmp:tmp_detalle_op98 
     WHERE cve_afore = v_cve_afore_archivo
       AND f_valor_devolucion = v_f_valor_devolucion
     ))
     GROUP BY 1,2,3,4
     HAVING rechazo1 =2 OR rechazo2 = 3 OR rechazo3=4));
   
   IF ( v_si_contador>=1 ) THEN
      -- Se envían todos los registros de la afore a rechazados
      LET v_si_rachazo_total_afore = 1;
      --LET v_i_resultado = 1;
      
      --TRACE("# # # # # # # # # # # # # # :");
      --TRACE("Rechazo Total afore por importes:");
      --TRACE("v_si_contador             :"||v_si_contador);
      --TRACE("v_f_valor_devolucion         :"||v_f_valor_devolucion);
      
      -- los importes de alguna de las afores son inconsistentes
      --LET v_i_resultado = 1;
      LET isam_err = 0;
      LET v_c_msj = 'Se encontraron AFORES rechazadas por inconsistencias en sus importes. Verificar consulta de rechazos';
   END IF
   
   --------------------------------------------------------------
   -- VERIFICA RECHAZO TOTAL
   
   --  1.- CON NSS               ] : Debe existir en afi_derechohabiente
   --  2.- ID_DERECHOHABIENTE    ] : Debe existir en afi_derechohabiente
   --  3.- CVE_AFORE             ] : Debe estar catalogada en cat_afore
   --  4.- F_VALOR_DEVOLUCION ] : Debe ser mayor o igual a >= 01/07/1997
   --  5.- F_VALOR_RECEPCION     ] : Debe ser menor al dìa actual.
   --  6.- F_VALOR_DEVOLUCION    ] : 
   --      Debe ser el primer dia natural del siguiente mes.
   --       Ejemplo: Fecha actual OR TODAY = 14-02-2012
   --          Valor valido : 01-03-2012
   --  7.- REMANENTE             ] : Debe ser '00' o '01' cualquier otro es invalido
   
   -- Verificar que todos los nss del archivo esten registrados 
   --   en afi_derechohabiente
   --------------------------------------------------------------
    LET v_si_cont_nss = 0;
    SELECT COUNT(*)
      INTO v_si_cont_nss
      FROM ((safre_tmp:tmp_detalle_op98 a 
             LEFT OUTER JOIN afi_derechohabiente b
                          ON a.nss = b.nss) 
             LEFT OUTER JOIN cat_afore f
                          ON a.cve_afore = f.afore_cod) 
             LEFT OUTER JOIN deo_cat_autocorreccion t
                          ON a.proc_origen_devolucion = t.tpo_autocorreccion
     WHERE(b.nss IS NULL 
        OR b.id_derechohabiente IS NULL
        OR f.afore_cod IS NULL
        OR sp_cambia_formato_fecha(a.f_afectacion_contable) < '07/01/1997'
        OR sp_cambia_formato_fecha(a.f_valor_recepcion) >= TODAY
--        OR sp_cambia_formato_fecha(a.f_valor_devolucion) <> sp_obtiene_fecha_mes_siguiente_primero() ya no se revisa contra la fecha del mes siguiente
        OR a.remanente NOT IN ('00','01')
        OR t.tpo_autocorreccion IS NULL
        OR a.num_aplic_interes_97 <0
        OR a.num_aplic_interes_92 <0
        )
       AND a.f_valor_devolucion = v_f_valor_devolucion
       AND a.cve_afore = v_cve_afore_archivo;
      
   IF ( v_si_cont_nss >=1 ) THEN
      -- Puede indicar algunos de los siguientes errores
      -- 1.- Indica que alguno de los nss NO esta registrado en afi_derechohabiente
      --  por lo que se tiene que rechazar toda la afore.
      -- 2.- Indica que NO existe el id_derechohabiente
      -- 3.- Indica que NO esta catalogada la clave de afore
      -- 4.- f_afectacion_contable Debe ser mayor o igual a >= 01/07/1997
      -- 5.- F_VALOR_RECEPCION     ] : Debe ser menor al dìa actual.
      -- 6.- F_VALOR_DEVOLUCION    ] : 
      --     Debe ser el primer dia natural del siguiente mes.
      --      Ejemplo: Fecha actual OR TODAY = 14-02-2012
      --         Valor valido : 01-03-2012
      -- 7.- REMANENTE             ] : Debe ser '00' o '01' cualquier otro es invalido
      -- 8.- Tipo Autocorrección   ] : Debe estar catalogado.
      -- 9.- Numero de acciones 97 ] : Las acciones deben ser igual o mayores a cero
      --10.- numero de acciones 92 ] : Las acciones deben ser igual o mayores a cero
      LET v_si_rachazo_total_afore = 1;
      --LET v_i_resultado = 1;
      
      --TRACE("# # # # # # # # # # # # # # :");
      --TRACE("Rechazo Total Varios errores:");
      --TRACE("v_si_cont_nss           :"||v_si_cont_nss);
      --TRACE("v_si_cont_nss           :"||v_si_cont_nss);
      
      -- la validacion de NSS y sus solicitudes no fue correcta
      --LET v_i_resultado = 1;
      LET isam_err = 0;
      LET v_c_msj = 'Se encontraron rechazos por invalidez de NSS, Fecha de afectación contable, clave de afore válida, fecha de valor de recepción y/o devolución.\nRevisar consulta de rechazos.';
   END IF
   
   -- ####>>> VALIDA SI HAY ERROR TOTAL DE AFORE POR ERRORES EN AIVS ####
   -- Recorre todos los regstros de afores para verficar si se 
   -- rechanza completamente por ERROR en calculo de las AIV
   FOREACH --cur_tmp_detalle FOR
     SELECT sp_cambia_formato_fecha(a.f_valor_devolucion)
            ,num_aplic_interes_97/1000000, importe_viv97/100
            ,num_aplic_interes_92/1000000, importe_viv92/100
       INTO v_detop98_f_valor_devol_inf
            ,v_detop98_acc_devol_viv97, v_op98_importe_viv97
            ,v_detop98_acc_devol_viv92, v_op98_importe_viv92
       FROM safre_tmp:tmp_detalle_op98 a
      WHERE a.f_valor_devolucion = v_f_valor_devolucion
        AND a.cve_afore = v_cve_afore_archivo


      --- >
      IF ( v_op98_importe_viv97>0 ) THEN
         LET v_si_estado = 0;
         LET v_d_importe_ivs = 0;
         -- Validar importe de acciones devoluciones viv97
         EXECUTE FUNCTION fn_del_cal_total_acciones(
                   v_detop98_f_valor_devol_inf, 
                   v_detop98_acc_devol_viv97,
                   11)
            INTO v_si_estado, v_d_importe_ivs;

         IF ( v_d_importe_ivs <> v_op98_importe_viv97 ) THEN
            -- Rechaza afore completa
            LET v_si_rachazo_total_afore = 1;
            --LET v_i_resultado = 1;
            --TRACE("# # # # # # # # # # # # # # :");
            --TRACE("Rechazo Total AIV's:  1");
            --TRACE("v_detop98_f_valor_devol_inf :"||v_detop98_f_valor_devol_inf);
            --TRACE("v_detop98_acc_devol_viv97   :"||v_detop98_acc_devol_viv97);
            --TRACE("v_d_importe_ivs             :"||v_d_importe_ivs);
            --TRACE("v_op98_importe_viv97        :"||v_op98_importe_viv97);
            
            -- el importe de intereses de vivienda es diferente al de viv97
            --LET v_i_resultado = 1;
            LET isam_err = 0;
            LET v_c_msj = 'El importe de IVS es diferente al importe de vivienda 97: ' || v_d_importe_ivs || "<>" || v_op98_importe_viv97;

            EXIT FOREACH;
         END IF
      END IF
      --- <
      -- Validar importe de acciones devoluciones viv92
      --- >
      IF ( v_op98_importe_viv92>0 ) THEN
         LET v_si_estado = 0;
         LET v_d_importe_ivs = 0;
         EXECUTE FUNCTION fn_del_cal_total_acciones(
                   v_detop98_f_valor_devol_inf, 
                   v_detop98_acc_devol_viv92,
                   11)
          INTO v_si_estado, v_d_importe_ivs;
         
         IF ( v_d_importe_ivs <> v_op98_importe_viv92 ) THEN
            -- Rechaza afore completa
            LET v_si_rachazo_total_afore = 1;
            --LET v_i_resultado = 1;
            --TRACE("# # # # # # # # # # # # # # :");
            --TRACE("Rechazo Total AIV's:  2");
            --TRACE("v_detop98_f_valor_devol_inf :"||v_detop98_f_valor_devol_inf);
            --TRACE("v_detop98_acc_devol_viv92   :"||v_detop98_acc_devol_viv92);
            --TRACE("v_d_importe_ivs             :"||v_d_importe_ivs);
            --TRACE("v_op98_importe_viv92        :"||v_op98_importe_viv92);
      
            -- el importe de intereses de vivienda es diferente al de viv92
            --LET v_i_resultado = 1;
            LET isam_err = 0;
            LET v_c_msj = 'El importe de IVS es diferente al importe de vivienda 92: ' || v_d_importe_ivs || "<>" || v_op98_importe_viv92;
      
            EXIT FOREACH;
         END IF
      END IF      
   END FOREACH
   
   -- ####<<< VALIDA SI HAY ERROR TOTAL DE AFORE POR ERRORES EN AIVS ####
   
   
   --TRACE("Despues ->v_si_contErroers  :"||v_si_contErroers);
   --TRACE("v_si_rachazo_total_afore    :"||v_si_rachazo_total_afore);
   
   -- PARA PRUEBAS ##################
   -- se obtienen todos los registros de la tabla temporal
   -- para la afore en cuestion
   FOREACH --cur_tmp_detalle FOR
     SELECT      
       a.tpo_registro          ,
       a.id_servicio           ,
       a.id_operacion          ,
       a.nss                   ,
       a.curp                  ,
       a.nombre_trabajador     ,
       a.proc_origen_devolucion,
       a.num_aplic_interes_97/1000000  ,
       a.num_aplic_interes_92/1000000  ,
       a.filler1               ,
       a.remanente             ,
       a.filler2               ,
       a.f_afectacion_contable ,
       a.cve_afore             ,
       a.filler3               ,
       a.importe_viv97/100         ,
       a.importe_viv92/100         ,
       a.importe_interes_viv92/100 ,
       a.f_valor_recepcion     ,
       a.f_valor_devolucion    ,
       a.filler4               ,
       a.importe_interes_viv97/100 ,
       a.filler5               ,
       a.rfc_trabajador        ,
       a.result_operacion      ,
       a.motivo_rechazo1       ,
       a.motivo_rechazo2       ,
       a.motivo_rechazo3       ,
       a.filler6               ,
       b.id_derechohabiente 
     INTO
       v_op98_tpo_registro          ,
       v_op98_id_servicio           ,
       v_op98_id_operacion          ,
       v_op98_nss                   ,
       v_op98_curp                  ,
       v_op98_nombre_trabajador     ,
       v_op98_proc_origen_devolucion,
       v_op98_num_aplic_interes_97  ,
       v_op98_num_aplic_interes_92  ,
       v_op98_filler1               ,
       v_op98_remanente             ,
       v_op98_filler2               ,
       v_op98_f_afectacion_contable ,
       v_op98_cve_afore             ,
       v_op98_filler3               ,
       v_op98_importe_viv97         ,
       v_op98_importe_viv92         ,
       v_op98_importe_interes_viv92 ,
       v_op98_f_valor_recepcion     ,
       v_op98_f_valor_devolucion    ,
       v_op98_filler4               ,
       v_op98_importe_interes_viv97 ,
       v_op98_filler5               ,
       v_op98_rfc_trabajador        ,
       v_op98_result_operacion      ,
       v_op98_motivo_rechazo1       ,
       v_op98_motivo_rechazo2       ,
       v_op98_motivo_rechazo3       ,
       v_op98_filler6               ,
       v_id_derechohabiente         
     FROM safre_tmp:tmp_detalle_op98 a LEFT OUTER JOIN afi_derechohabiente b
       ON a.nss = b.nss
    WHERE a.cve_afore = v_cve_afore_archivo
     
      -- Se incrementa detalle de registro
      -- porque debe ser unico.
      LET v_id_detalle = v_id_detalle + 1;
      
      -- se asume que el registro es correcto
      LET v_b_registro_correcto = 0;
      
      LET v_si_rechazo_1 = 0;
      LET v_si_rechazo_2 = 0;
      LET v_si_rechazo_3 = 0;
      
      IF ( v_id_derechohabiente IS NULL ) THEN
      -- Inserta registro de rechazo para le folio y nss
         LET v_b_registro_correcto = 1;
         LET v_si_conteo_errores = v_si_conteo_errores + 1;
         --LET v_c_cadena_errores[v_si_conteo_errores, v_si_conteo_errores]='01';
         
         IF ( v_si_rechazo_1 = 0 ) THEN
            LET v_si_rechazo_1 = 1;
         ELSE
           IF ( v_si_rechazo_2 = 0 ) THEN
              LET v_si_rechazo_2 = 1;
           ELSE
              IF ( v_si_rechazo_3 = 0 ) THEN
                 LET v_si_rechazo_3 = 1;
              END IF
           END IF
         END IF
         
      END IF
      
      -- ############ se validan los registros ############ --
      -- si no tiene importes, se rechaza
      IF ( v_op98_importe_viv97 <= 0 AND v_op98_importe_viv92 <= 0) THEN
         LET v_b_registro_correcto = 1;
         --LET v_i_resultado = 1;
         
         -- Rechazo para le folio y nss
         LET v_si_conteo_errores = v_si_conteo_errores + 1;
         --LET v_c_cadena_errores[v_si_conteo_errores -1, v_si_conteo_errores]='07';
         IF ( v_si_rechazo_1 = 0 ) THEN
            LET v_si_rechazo_1 = 7;
         ELSE
           IF ( v_si_rechazo_2 = 0 ) THEN
              LET v_si_rechazo_2 = 7;
           ELSE
              IF ( v_si_rechazo_3 = 0 ) THEN
                 LET v_si_rechazo_3 = 7;
              END IF
           END IF
         END IF
         
      END IF
      
      -- si el registro tiene importe, debe tener intereses acorde al importe
      --- Se dejan de validar los intereses SACI2018-23
--      IF ( v_op98_importe_viv97 > 0 ) THEN
--         IF ( v_op98_importe_interes_viv97 <= 0 ) THEN
--            -- el registro no tiene intereses
--            LET v_b_registro_correcto = 1;
--            --LET v_i_resultado = 2;
--            
--            -- el importe de intereses de vivienda es diferente al de viv92
--            LET isam_err = 0;
--            LET v_c_msj = 'Se tienen registros con importe de vivienda 97 pero sin intereses de vivienda 97. Ver consulta de rechazos';
--
--            
--            -- Rechazo por tener importe viv97 y NO tener importe interes_viv97
--            LET v_si_conteo_errores = v_si_conteo_errores + 1;
--            --LET v_c_cadena_errores[v_si_conteo_errores -1, v_si_conteo_errores]='06';
--            IF ( v_si_rechazo_1 = 0 ) THEN
--               LET v_si_rechazo_1 = 6;
--            ELSE
--              IF ( v_si_rechazo_2 = 0 ) THEN
--                 LET v_si_rechazo_2 = 6;
--              ELSE
--                 IF ( v_si_rechazo_3 = 0 ) THEN
--                    LET v_si_rechazo_3 = 6;
--                 END IF
--              END IF
--            END IF
--            
--         END IF
--      END IF
      
      -- si el registro tiene importe, debe tener intereses acorde al importe
      --- Se dejan de validar los intereses SACI2018-23
--      IF ( v_op98_importe_viv92 > 0 ) THEN
--         IF ( v_op98_importe_interes_viv92 <= 0 ) THEN
--            -- el registro no tiene intereses
--            LET v_b_registro_correcto = 1;
--            --LET v_i_resultado = 3;
--            
--            -- el importe de intereses de vivienda es diferente al de viv92
--            LET isam_err = 0;
--            LET v_c_msj = 'Se tienen registros con importe de vivienda 92 pero sin intereses de vivienda 92. Ver consulta de rechazos';
--
--            
--            -- Rechazo por tener importe viv92 y NO tener importe interes_viv92
--            LET v_si_conteo_errores = v_si_conteo_errores + 1;
--            --LET v_c_cadena_errores[v_si_conteo_errores -1, v_si_conteo_errores]='05';
--            IF ( v_si_rechazo_1 = 0 ) THEN
--               LET v_si_rechazo_1 = 5;
--            ELSE
--              IF ( v_si_rechazo_2 = 0 ) THEN
--                 LET v_si_rechazo_2 = 5;
--              ELSE
--                 IF ( v_si_rechazo_3 = 0 ) THEN
--                    LET v_si_rechazo_3 = 5;
--                 END IF
--              END IF
--            END IF
--         END IF
--      END IF
   
      -- se transfieren los datos al registro que se insertara
      LET v_detop98_id_detalle            = v_id_detalle; --DECIMAL(9,0) 
      LET v_detop98_folio                 = p_folio; --DECIMAL(9,0) 
      LET v_detop98_curp                  = v_op98_curp; --CHAR(18)     
      LET v_detop98_tipo_autocorreccion   = v_op98_proc_origen_devolucion; --SMALLINT
      
      -- Validar el tipo de autocorreccion.
      LET v_si_correcciones = 0;
      SELECT COUNT(*)
        INTO v_si_correcciones
        FROM deo_cat_autocorreccion
       WHERE tpo_autocorreccion = v_detop98_tipo_autocorreccion;
       
      IF ( v_si_correcciones <=0 ) THEN
         -- ERROR al NO estar catalogado el tipo de autocorreccion
         --LET v_i_resultado = 3;
         LET v_b_registro_correcto = 1;
         LET v_si_conteo_errores = v_si_conteo_errores + 1;
         --LET v_c_cadena_errores[v_si_conteo_errores -1, v_si_conteo_errores]='02';
         IF ( v_si_rechazo_1 = 0 ) THEN
            LET v_si_rechazo_1 = 2;
         ELSE
           IF ( v_si_rechazo_2 = 0 ) THEN
              LET v_si_rechazo_2 = 2;
           ELSE
              IF ( v_si_rechazo_3 = 0 ) THEN
                 LET v_si_rechazo_3 = 2;
              END IF
           END IF
         END IF
      END IF
      
      -- [3] Valida las claves de afore esten catalogadas.
      LET v_si_cont_nss = 0;
      SELECT COUNT(*)
      INTO v_si_cont_nss
      FROM safre_tmp:tmp_detalle_op98 a LEFT OUTER JOIN cat_afore f
        ON a.cve_afore = f.afore_cod
     WHERE(f.afore_cod IS NULL)
       AND a.f_valor_devolucion = v_f_valor_devolucion
       AND a.cve_afore = v_cve_afore_archivo;
      
      IF ( v_si_cont_nss >0 ) THEN
         --LET v_i_resultado = 3;
         LET v_b_registro_correcto = 1;
         LET v_si_conteo_errores = v_si_conteo_errores + 1;
         --LET v_c_cadena_errores[v_si_conteo_errores -1, v_si_conteo_errores]='10';
         IF ( v_si_rechazo_1 = 0 ) THEN
            LET v_si_rechazo_1 = 10;
         ELSE
           IF ( v_si_rechazo_2 = 0 ) THEN
              LET v_si_rechazo_2 = 10;
           ELSE
              IF ( v_si_rechazo_3 = 0 ) THEN
                 LET v_si_rechazo_3 = 10;
              END IF
           END IF
         END IF
      END IF
      
      -- [4] Valida la fecha de afectación contable para su registro en rechazo
      LET v_si_cont_nss = 0;
      SELECT COUNT(*)
      INTO v_si_cont_nss
      FROM safre_tmp:tmp_detalle_op98 a 
     WHERE sp_cambia_formato_fecha(a.f_afectacion_contable) < '07/01/1997'
       AND a.f_valor_devolucion = v_f_valor_devolucion
       AND a.cve_afore = v_cve_afore_archivo;
      
      IF ( v_si_cont_nss >0 ) THEN
         --LET v_i_resultado = 3;
         LET v_b_registro_correcto = 1;
         LET v_si_conteo_errores = v_si_conteo_errores + 1;
         --LET v_c_cadena_errores[v_si_conteo_errores -1, v_si_conteo_errores]='11';
         IF ( v_si_rechazo_1 = 0 ) THEN
            LET v_si_rechazo_1 = 11;
         ELSE
           IF ( v_si_rechazo_2 = 0 ) THEN
              LET v_si_rechazo_2 = 11;
           ELSE
              IF ( v_si_rechazo_3 = 0 ) THEN
                 LET v_si_rechazo_3 = 11;
              END IF
           END IF
         END IF
      END IF
      
      -- [5] Valida la f_valor_recepcion para su registro en rechazo
      LET v_si_cont_nss = 0;
      SELECT COUNT(*)
      INTO v_si_cont_nss
      FROM safre_tmp:tmp_detalle_op98 a 
     WHERE sp_cambia_formato_fecha(a.f_valor_recepcion) >= TODAY
       AND a.f_valor_devolucion = v_f_valor_devolucion
       AND a.cve_afore = v_cve_afore_archivo;
      
      IF ( v_si_cont_nss >0 ) THEN
         --LET v_i_resultado = 3;
         LET v_b_registro_correcto = 1;
         LET v_si_conteo_errores = v_si_conteo_errores + 1;
         --LET v_c_cadena_errores[v_si_conteo_errores -1, v_si_conteo_errores]='12';
         IF ( v_si_rechazo_1 = 0 ) THEN
            LET v_si_rechazo_1 = 12;
         ELSE
           IF ( v_si_rechazo_2 = 0 ) THEN
              LET v_si_rechazo_2 = 12;
           ELSE
              IF ( v_si_rechazo_3 = 0 ) THEN
                 LET v_si_rechazo_3 = 12;
              END IF
           END IF
         END IF
      END IF
      
      -- [6] Valida la f_valor_devolucion para su registro en rechazo
      -- =======================================================================
      -- AGOSTO 14, 2012  LA FECHA DE DEVOLUCION NO TIENE QUE SER EL PRIMER DIA DEL MES SIGUIENTE
      -- =======================================================================
     -- LET v_si_cont_nss = 0;
     -- SELECT COUNT(*)
     -- INTO v_si_cont_nss
     -- FROM safre_tmp:tmp_detalle_op98 a 
     --WHERE sp_cambia_formato_fecha(a.f_valor_devolucion) <> 
     --      sp_obtiene_fecha_mes_siguiente_primero()
     --  AND a.f_valor_devolucion = v_f_valor_devolucion
     --  AND a.cve_afore = v_cve_afore_archivo;
     -- 
     -- IF ( v_si_cont_nss >0 ) THEN
     --    LET v_b_registro_correcto = 1;
     --    --LET v_i_resultado = 3;
     --    LET v_si_conteo_errores = v_si_conteo_errores + 1;
     --    --LET v_c_cadena_errores[v_si_conteo_errores -1, v_si_conteo_errores]='13';
     --    IF ( v_si_rechazo_1 = 0 ) THEN
     --       LET v_si_rechazo_1 = 13;
     --    ELSE
     --      IF ( v_si_rechazo_2 = 0 ) THEN
     --         LET v_si_rechazo_2 = 13;
     --      ELSE
     --         IF ( v_si_rechazo_3 = 0 ) THEN
     --            LET v_si_rechazo_3 = 13;
     --         END IF
     --      END IF
     --    END IF
     -- END IF
      
      -- [7] Valida remanente para su registro en rechazo
      LET v_si_cont_nss = 0;
      SELECT COUNT(*)
      INTO v_si_cont_nss
      FROM safre_tmp:tmp_detalle_op98 a 
     WHERE a.remanente NOT IN ('00','01')
       AND a.f_valor_devolucion = v_f_valor_devolucion
       AND a.cve_afore = v_cve_afore_archivo;
      
      IF ( v_si_cont_nss >0 ) THEN
         LET v_b_registro_correcto = 1;
         --LET v_i_resultado = 3;
         LET v_si_conteo_errores = v_si_conteo_errores + 1;
         --LET v_c_cadena_errores[v_si_conteo_errores -1, v_si_conteo_errores]='09';
         IF ( v_si_rechazo_1 = 0 ) THEN
            LET v_si_rechazo_1 = 9;
         ELSE
           IF ( v_si_rechazo_2 = 0 ) THEN
              LET v_si_rechazo_2 = 9;
           ELSE
              IF ( v_si_rechazo_3 = 0 ) THEN
                 LET v_si_rechazo_3 = 9;
              END IF
           END IF
         END IF
      END IF
      
      LET v_detop98_id_derechohabiente    = v_id_derechohabiente; --VARCHAR(11)  
      LET v_detop98_acc_devol_viv97       = v_op98_num_aplic_interes_97; --DECIMAL(18,6)
      LET v_detop98_nombre_trabajador     = v_op98_nombre_trabajador; --CHAR(50)     
      LET v_detop98_acc_devol_viv92       = v_op98_num_aplic_interes_92; --DECIMAL(20,2)
      LET v_detop98_remanente             = v_op98_remanente; --SMALLINT     
      --LET v_detop98_f_afectacion_contable = v_op98_f_afectacion_contable; --DATE
      -- Corrige fecha de YYYYMMDD a MMDDYYY
      EXECUTE PROCEDURE sp_cambia_formato_fecha(v_op98_f_afectacion_contable)
         INTO v_detop98_f_afectacion_contable;
      
      LET v_detop98_pes_devol_viv97       = v_op98_importe_viv97; --DECIMAL(10,2)
      LET v_detop98_pes_devol_viv92       = v_op98_importe_viv92; --DECIMAL(20,2)
      LET v_detop98_pes_int_devol_viv92   = v_op98_importe_interes_viv92; --DECIMAL(10,2)
      --LET v_detop98_f_valor_recep_afore   = v_op98_f_valor_recepcion; --DATE
      -- Corrige fecha de YYYYMMDD a MMDDYYY
      EXECUTE PROCEDURE sp_cambia_formato_fecha(v_op98_f_valor_recepcion)
         INTO v_detop98_f_valor_recep_afore;
      
      --LET v_detop98_f_valor_devol_inf     = v_op98_f_valor_devolucion; --DATE
      -- Corrige fecha de YYYYMMDD a MMDDYYY
      EXECUTE PROCEDURE sp_cambia_formato_fecha(v_op98_f_valor_devolucion)
         INTO v_detop98_f_valor_devol_inf;
      
      --LET v_detop98_f_valor_recep_afore   = NULL; --DATE         
      --LET v_detop98_f_valor_devol_inf     = NULL; --DATE         
      LET v_detop98_pes_int_devol_viv97   = v_op98_importe_interes_viv97; --DECIMAL(10,2)
      LET v_detop98_rfc                   = v_op98_rfc_trabajador; --CHAR(13)     
      LET v_detop98_cve_afore             = v_op98_cve_afore; --SMALLINT
      
      

      -- [9] Valida que numero de acciones viv97 sea NO negativo
      IF ( v_detop98_acc_devol_viv97 < 0 AND v_op98_importe_viv97 > 0 ) THEN
         LET v_b_registro_correcto = 1;
         --LET v_i_resultado = 3;
         LET v_si_conteo_errores = v_si_conteo_errores + 1;
         --LET v_c_cadena_errores[v_si_conteo_errores -1, v_si_conteo_errores]='04';
         IF ( v_si_rechazo_1 = 0 ) THEN
            LET v_si_rechazo_1 = 4;
         ELSE
           IF ( v_si_rechazo_2 = 0 ) THEN
              LET v_si_rechazo_2 = 4;
           ELSE
              IF ( v_si_rechazo_3 = 0 ) THEN
                 LET v_si_rechazo_3 = 4;
              END IF
           END IF
         END IF
      END IF
      -- [10] Valida que numero de acciones viv92 sea NO negativo
      IF ( v_detop98_acc_devol_viv92 < 0 AND v_op98_importe_viv92 > 0 ) THEN
         LET v_b_registro_correcto = 1;
         --LET v_i_resultado = 3;
         LET v_si_conteo_errores = v_si_conteo_errores + 1;
         --LET v_c_cadena_errores[v_si_conteo_errores -1, v_si_conteo_errores]='03';
         IF ( v_si_rechazo_1 = 0 ) THEN
            LET v_si_rechazo_1 = 3;
         ELSE
           IF ( v_si_rechazo_2 = 0 ) THEN
              LET v_si_rechazo_2 = 3;
           ELSE
              IF ( v_si_rechazo_3 = 0 ) THEN
                 LET v_si_rechazo_3 = 3;
              END IF
           END IF
         END IF
      END IF

      -- Validar importe de acciones devoluciones viv97
      --- >
      EXECUTE FUNCTION fn_del_cal_total_acciones(
                v_detop98_f_valor_devol_inf, 
                v_detop98_acc_devol_viv97,
                11)
       INTO v_si_estado, v_d_importe_ivs;
             

      IF ( v_d_importe_ivs <> v_op98_importe_viv97 ) THEN
         -- Rechazo por ERROR en importes de aivs y el importe en pesos
         LET v_b_registro_correcto = 1;
         --LET v_i_resultado = 3;
         LET v_si_conteo_errores = v_si_conteo_errores + 1;
         --LET v_c_cadena_errores[v_si_conteo_errores -1, v_si_conteo_errores]='97';
         IF ( v_si_rechazo_1 = 0 ) THEN
            LET v_si_rechazo_1 = 15;
         ELSE
           IF ( v_si_rechazo_2 = 0 ) THEN
              LET v_si_rechazo_2 =15;
           ELSE
              IF ( v_si_rechazo_3 = 0 ) THEN
                 LET v_si_rechazo_3 = 15;
              END IF
           END IF
         END IF
      END IF
           
      --- <
      -- Validar importe de acciones devoluciones viv92
      --- >
      EXECUTE FUNCTION fn_del_cal_total_acciones(
                v_detop98_f_valor_devol_inf, 
                v_detop98_acc_devol_viv92,
                11)
       INTO v_si_estado, v_d_importe_ivs;

      IF ( v_d_importe_ivs <> v_op98_importe_viv92 ) THEN
         -- Rechazo por ERROR en importes de aivs y el importe en pesos
         LET v_b_registro_correcto = 1;
         --LET v_i_resultado = 3;
         LET v_si_conteo_errores = v_si_conteo_errores + 1;
         --LET v_c_cadena_errores[v_si_conteo_errores -1, v_si_conteo_errores]='14';
         IF ( v_si_rechazo_1 = 0 ) THEN
            LET v_si_rechazo_1 = 14;
         ELSE
           IF ( v_si_rechazo_2 = 0 ) THEN
              LET v_si_rechazo_2 = 14;
           ELSE
              IF ( v_si_rechazo_3 = 0 ) THEN
                 LET v_si_rechazo_3 = 14;
              END IF
           END IF
         END IF
      END IF
      --- <
      
      -- si el registro es correcto se inserta en la tabla de produccion
      IF ( v_b_registro_correcto = 0 AND v_si_rachazo_total_afore = 0) THEN
         -- se insertan en la tabla de detalle de op98 en la tabla
         -- productiva de safre_viv
         LET v_detop98_resultado_operacion   = 2; --SMALLINT definido en reunion
         LET v_detop98_estado_devolucion     = 2; --SMALLINT Pendiente definir     
         --TRACE("Inserta correcto: el detalle:");
         --TRACE(v_detop98_id_detalle);
         INSERT INTO deo_det_op98
                (id_detalle           ,
                 folio                ,
                 tpo_autocorreccion  ,
                 cve_afore            ,
                 id_derechohabiente   ,
                 rfc                  ,
                 curp                 ,
                 nombre_trabajador    ,
                 acc_devol_viv97      ,
                 acc_devol_viv92      ,
                 pes_devol_viv97      ,
                 pes_devol_viv92      ,
                 pes_int_devol_viv97  ,
                 pes_int_devol_viv92  ,
                 remanente            ,
                 f_afectacion_contable,
                 f_valor_recep_afore  ,
                 f_valor_devol_inf    ,
                 estado_devolucion
                )
         VALUES (v_detop98_id_detalle           ,
                 v_detop98_folio                ,
                 v_detop98_tipo_autocorreccion  ,
                 v_detop98_cve_afore            ,
                 v_detop98_id_derechohabiente   ,
                 v_detop98_rfc                  ,
                 v_detop98_curp                 ,
                 v_detop98_nombre_trabajador    ,
                 v_detop98_acc_devol_viv97      ,
                 v_detop98_acc_devol_viv92      ,
                 v_detop98_pes_devol_viv97      ,
                 v_detop98_pes_devol_viv92      ,
                 v_detop98_pes_int_devol_viv97  ,
                 v_detop98_pes_int_devol_viv92  ,
                 v_detop98_remanente            ,
                 v_detop98_f_afectacion_contable,
                 v_detop98_f_valor_recep_afore  ,
                 v_detop98_f_valor_devol_inf    ,
                 v_detop98_estado_devolucion
                );
                
         -- Indica que si hay al menos una afore correctamente integrada
         LET v_si_correcto_integra = 1;
         
         
      ELSE
         -- se inserta en la tabla de rechazos de la base
         -- productiva de safre_viv
         LET v_detop98_resultado_operacion   = 999; --SMALLINT definido en reunion
         LET v_detop98_estado_devolucion     = 999; --SMALLINT Pendiente definir     
         --TRACE("Inserta erroneo: el detalle:");
         --TRACE(v_detop98_id_detalle);
         --TRACE("---------------------------:");
         --TRACE("v_si_rechazo_1:"||v_si_rechazo_1);
         --TRACE("v_si_rechazo_2:"||v_si_rechazo_2);
         --TRACE("v_si_rechazo_3:"||v_si_rechazo_3);

         -- Los errores
         INSERT INTO deo_det_op98_rch
                (id_detalle           ,
                 folio                ,
                 cve_afore            ,
                 tpo_autocorreccion  ,
                 id_derechohabiente   ,
                 nss                  ,
                 curp                 ,
                 rfc                  ,
                 nombre_trabajador    ,
                 acc_devol_viv97      ,
                 acc_devol_viv92      ,
                 pes_devol_viv97      ,
                 pes_devol_viv92      ,
                 pes_int_devol_viv97  ,
                 pes_int_devol_viv92  ,
                 remanente            ,
                 f_valor_recep_afore  ,
                 f_valor_devol_inf    ,
                 f_afectacion_contable,
                 estado_devolucion    ,
                 resultado_operacion  ,
                 cod_rechazo_1        ,
                 cod_rechazo_2        ,
                 cod_rechazo_3        
                )
         VALUES (v_detop98_id_detalle           ,
                 v_detop98_folio                ,
                 v_detop98_cve_afore            ,
                 v_detop98_tipo_autocorreccion  ,
                 v_detop98_id_derechohabiente   ,
                 v_op98_nss                     ,
                 v_detop98_curp                 ,
                 v_detop98_rfc                  ,
                 v_detop98_nombre_trabajador    ,
                 v_detop98_acc_devol_viv97      ,
                 v_detop98_acc_devol_viv92      ,
                 v_detop98_pes_devol_viv97      ,
                 v_detop98_pes_devol_viv92      ,
                 v_detop98_pes_int_devol_viv97  ,
                 v_detop98_pes_int_devol_viv92  ,
                 v_detop98_remanente            ,
                 v_detop98_f_valor_recep_afore  ,
                 v_detop98_f_valor_devol_inf    ,
                 v_detop98_f_afectacion_contable,
                 v_detop98_estado_devolucion    ,
                 v_detop98_resultado_operacion  ,
                 v_si_rechazo_1                 ,
                 v_si_rechazo_2                 ,
                 v_si_rechazo_3                 
                );
        
      END IF
   END FOREACH      
   
   IF ( v_si_rachazo_total_afore = 0 ) THEN
      -- Actualizar estatus de saldos cuando la afore es valida.
      UPDATE deo_mto_deposito
         SET estado_devolucion = 2 -- procesado
       WHERE cve_afore = v_cve_afore_archivo
         AND f_valor_devol_inf = v_dte_f_valor_devolucion
         AND estado_devolucion = 1; -- procesar
         
      -- se cuenta una afore aceptada
      LET v_afores_aceptadas = v_afores_aceptadas + 1;

   ELSE
   	  -- Sino se acepto completa la afore, NO se actualiza
  	  -- el estatus para poder ser utilizada posteriormente
      
      -- se cuenta una afore rechazada
      LET v_afores_rechazadas = v_afores_rechazadas + 1;
   END IF
 END FOREACH 

 -- se devuelve el resultado de la operacion
 RETURN v_i_resultado, isam_err, v_c_msj, v_afores_aceptadas, v_afores_rechazadas;
END FUNCTION;


