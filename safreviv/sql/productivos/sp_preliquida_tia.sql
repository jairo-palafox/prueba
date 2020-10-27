






CREATE PROCEDURE "safreviv".sp_preliquida_tia(p_folio                   DECIMAL(9,0) ,
                                   p_usuario                 CHAR(20)     ,
                                   p_preliq_id_referencia    DECIMAL(9,0) ,
                                   p_preliq_monto_pesos      DECIMAL(18,2),
                                   p_preliq_id_decreto       DECIMAL(9,0) ,
                                   p_f_movimiento            DATE         ,
                                   p_nci_icefa               CHAR(30)     ,
                                   p_aivs_viv92              DECIMAL(16,6),
                                   p_ind_consistencia        SMALLINT     ,
                                   p_nss_afo_recep           CHAR(11),
                                   p_origen_traspaso         CHAR(02),
                                   p_curp                    CHAR(18)
                                   )

   DEFINE v_preliq_f_liquida       DATE;                     --f_liquida            date
   DEFINE v_preliq_subcuenta       SMALLINT;                 --subcuenta            smallint
   DEFINE v_preliq_fondo_inversion SMALLINT;                 --fondo_inversion      smallint
   DEFINE v_preliq_movimiento      SMALLINT;                 --movimiento           smallint
   DEFINE v_preliq_monto_acciones  DECIMAL(18,2);            --monto_acciones       decimal(22,2)
   DEFINE v_preliq_monto_pesos     DECIMAL(18,2);            --monto_pesos          decimal(22,2)
   DEFINE v_preliq_f_valor         DATE;                     --f_valor              date
   DEFINE v_preliq_f_registro      DATE;                     --f_registro           date
   DEFINE v_preliq_h_registro      DATETIME HOUR TO SECOND;  --h_registro           datetime hour to second
   DEFINE v_preliq_origen          CHAR(20);                 --origen               char(20)
   DEFINE v_error                  SMALLINT ;
   DEFINE v_ide_derechohabiente    DECIMAL(9,0);
   DEFINE v_existe_derechohabiente SMALLINT;
   DEFINE v_cont_curp              SMALLINT;    --tia01

   ON EXCEPTION SET v_error

   END EXCEPTION --WITH RESUME

   LET v_ide_derechohabiente = 0;
   LET v_preliq_f_liquida    = TODAY;
   LET v_preliq_f_valor      = TODAY;
   LET v_preliq_f_registro   = TODAY; --en caso de realizar la preliquidacion con muchos registros, actualiza cada registro
   LET v_preliq_h_registro   = CURRENT HOUR TO SECOND; --actualiza tiempo para cada registro

   -- validación por que existe nss con blancos.
  -- IF p_nss_afo_recep = '           ' THEN
   --   LET p_nss_afo_recep = NULL;
  -- END IF

   SELECT COUNT(*)
   INTO   v_existe_derechohabiente
   FROM   afi_derechohabiente
   WHERE  nss = p_nss_afo_recep;

   IF v_existe_derechohabiente<>0 THEN
      --SE VERIFICA QUE NO SE EL NSS NULO
      SELECT COUNT(*)
      INTO   v_existe_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = p_nss_afo_recep
      AND    id_derechohabiente=52112212;
      IF v_existe_derechohabiente<>0 THEN
         LET v_ide_derechohabiente=0;
      ELSE
         --OBTIENE ID_DERECHOHABIENTE AL CUAL SE LE VA HACER EL ABONO
         SELECT id_derechohabiente
         INTO   v_ide_derechohabiente
         FROM   afi_derechohabiente
         WHERE  nss = p_nss_afo_recep;
      END IF
   ELSE
      LET v_ide_derechohabiente=0;
   END IF

--======== tia01 ==========
   IF v_ide_derechohabiente=0 THEN                         --tia01

      -- Cumplimiento con req208 10 ABR 2014
      IF p_origen_traspaso = "42" THEN

         SELECT COUNT(*)                                   --tia01
         INTO   v_cont_curp                                --tia01
         FROM   afi_derechohabiente                        --tia01
         WHERE  curp  = p_curp;                            --tia01
         
         IF (v_cont_curp = 0) OR (v_cont_curp > 1) THEN    --tia01
            UPDATE tia_det_traspaso
            SET    result_operacion = "08"  --CURP no existe en afi_derechohabiente
            WHERE  folio            = p_folio
            AND    id_referencia    = p_preliq_id_referencia
            AND    result_operacion in ("01","10");
            RETURN;
         END IF                                            --tia01

         IF v_cont_curp = 1 THEN                           --tia01
            SELECT id_derechohabiente
            INTO   v_ide_derechohabiente
            FROM   afi_derechohabiente
            WHERE  curp  = p_curp; 
         ELSE                                              --tia01
            UPDATE tia_det_traspaso
            SET    result_operacion = "08"  --CURP no existe en afi_derechohabiente
            WHERE  folio            = p_folio
            AND    id_referencia    = p_preliq_id_referencia
            AND    result_operacion in ("01","10");
            RETURN;         
         END IF                                            --tia01

      ELSE                                                 --tia01
         UPDATE tia_det_traspaso                           --tia01
         SET    result_operacion = "04"  --NSS no existe en afi_derechohabiente
         WHERE  folio            = p_folio
         AND    id_referencia    = p_preliq_id_referencia
         AND    result_operacion in ("01","10");
         RETURN;
      END IF                                               --tia01
   END IF                                                  --tia01
   
{ --tia01

   IF v_ide_derechohabiente=0 THEN

      -- Cumplimiento con req208 10 ABR 2014
      IF p_origen_traspaso = "42" THEN

 --  	     IF p_curp = "" THEN
 --  	     	 LET p_curp = NULL;
 --  	     END IF

         SELECT FIRST 1 id_derechohabiente                 --tia01
         INTO   v_ide_derechohabiente                      --tia01
         FROM   afi_derechohabiente                        --tia01
         WHERE  curp  = p_curp;                            --tia01

         --si no se encuentra el v_ide_derechohabiente estado=08 de que no existe curp
         IF v_ide_derechohabiente IS NULL THEN             --tia01
            UPDATE tia_det_traspaso
            SET    result_operacion = "08"  --CURP no existe en afi_derechohabiente
            WHERE  folio            = p_folio
            AND    id_referencia    = p_preliq_id_referencia
            AND    result_operacion in ("01","10");
            RETURN;
         END IF                                            --tia01
      ELSE                                                 --tia01
         UPDATE tia_det_traspaso                           --tia01
         SET    result_operacion = "04"  --NSS no existe en afi_derechohabiente
         WHERE  folio            = p_folio
         AND    id_referencia    = p_preliq_id_referencia
         AND    result_operacion in ("01","10");
         RETURN;
      END IF                                               --tia01
   END IF                                                  --tia01
}

   LET v_preliq_subcuenta       = 48;
   LET v_preliq_fondo_inversion = 11 ;
   LET v_preliq_monto_acciones  = p_aivs_viv92 * (-1);
   LET v_preliq_monto_pesos     = p_preliq_monto_pesos * (-1);

   IF p_ind_consistencia =1 THEN       ---COSISTENTE
      LET v_preliq_movimiento = 362;   --CARGO CONSISTENTE
   ELSE
      LET v_preliq_movimiento = 592;   --CARGO NO CONSISTENTE
   END IF;

   INSERT INTO tia_preliquida
            (f_liquida,
             id_decreto,
             subcuenta,
             fondo_inversion,
             movimiento,
             folio_liquida,
             id_referencia,
             monto_acciones,
             monto_pesos,
             f_valor,
             f_registro,
             h_registro,
             origen)
            VALUES(v_preliq_f_liquida          --f_liquida
                  ,p_preliq_id_decreto         --id_decreto
                  ,v_preliq_subcuenta          --subcuenta
                  ,v_preliq_fondo_inversion    --fondo_inversion
                  ,v_preliq_movimiento         --movimiento
                  ,p_folio                     --folio_liquida
                  ,p_preliq_id_referencia      --id_referencia
                  ,v_preliq_monto_acciones     --monto_acciones
                  ,v_preliq_monto_pesos        --monto_pesos
                  ,p_f_movimiento              --f_valor
                  ,v_preliq_f_registro         --f_registro
                  ,v_preliq_h_registro         --h_registro
                  ,p_nci_icefa);

     -- abono
     LET v_preliq_subcuenta       = 8;
     LET v_preliq_fondo_inversion = 11;

     LET v_preliq_monto_acciones  = 0;

     IF p_ind_consistencia =1 THEN      ---COSISTENTE
        LET v_preliq_movimiento = 111;  --ABONO CONSISTENTE
     ELSE
        LET v_preliq_movimiento = 231;  --ABONO NO CONSISTENTE
     END IF;

     INSERT INTO tia_preliquida
            (f_liquida,
             id_decreto,
             subcuenta,
             fondo_inversion,
             movimiento,
             folio_liquida,
             id_referencia,
             monto_acciones,
             monto_pesos,
             f_valor,
             f_registro,
             h_registro,
             origen)
            VALUES(v_preliq_f_liquida          --f_liquida
                  ,v_ide_derechohabiente       --id_decreto
                  ,v_preliq_subcuenta          --subcuenta
                  ,v_preliq_fondo_inversion    --fondo_inversion
                  ,v_preliq_movimiento         --movimiento
                  ,p_folio                     --folio_liquida
                  ,p_preliq_id_referencia      --id_referencia
                  ,p_aivs_viv92                --monto_acciones
                  ,p_preliq_monto_pesos        --monto_pesos
                  ,p_f_movimiento              --f_valor
                  ,v_preliq_f_registro         --f_registro
                  ,v_preliq_h_registro         --h_registro
                  ,p_nci_icefa);
                  

   -- Se agrega sentencia de update statics a las tablas temporales
   UPDATE STATISTICS FOR TABLE tia_preliquida   ;

END PROCEDURE
;


