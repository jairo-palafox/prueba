DATABASE safre_viv

MAIN
DEFINE v_opcion SMALLINT

   LET v_opcion = ARG_VAL(1)

   CASE v_opcion
      WHEN 1
         CALL fn_genera_rpt_4cat()

      WHEN 2
         CALL fn_genera_rpt_amo_apo()

      WHEN 3
         CALL fn_genera_rpt_comp()

      WHEN 4
         CALL fn_genera_rpt_aclara()

      OTHERWISE
         CALL fn_genera_rpt_4cat()

   END CASE

END MAIN

FUNCTION fn_genera_rpt_4cat()
DEFINE p_datos_personales RECORD
          v_nombre   VARCHAR(40),
          v_paterno  VARCHAR(40),
          v_materno  VARCHAR(40),
          v_nss      CHAR(11),
          v_curp     CHAR(18),
          v_rfc      CHAR(13)
       END RECORD,
       p_periodo_busqueda RECORD
          v_f_ini DATE,
          v_f_fin DATE
       END RECORD,
       p_movimientos DYNAMIC ARRAY OF RECORD
          v_f_aplicacion DATE,
          v_cve_movimiento CHAR(5),
          v_desc_concepto STRING,
          v_tpo_movimiento STRING,
          v_cargo DECIMAL(16,2),
          v_abono DECIMAL(16,2)
       END RECORD,
       p_mov_aclara DYNAMIC ARRAY OF RECORD
          v_bimestre VARCHAR(7),
          v_reg_patronal VARCHAR(11),
          v_nombre_patron VARCHAR(40),
          v_f_pago DATE,
          v_aportacion DECIMAL(16,2),
          v_amortizacion DECIMAL(16,2)
       END RECORD,
       p_adicionales RECORD
          v_aviso VARCHAR(255),
          v_cadena_original STRING,
          v_sello_digital STRING
       END RECORD,
       v_indice SMALLINT,
       r_nom_reporte     STRING,
       r_ruta_lst        LIKE seg_modulo.ruta_listados
   

   LET p_datos_personales.v_nombre = "HUGO CESAR"
   LET p_datos_personales.v_paterno = "RAMIREZ"
   LET p_datos_personales.v_materno = "GARCIA"
   LET p_datos_personales.v_nss = "12345678901"
   LET p_datos_personales.v_curp = "RAGH880125HZSMRG01"
   LET p_datos_personales.v_rfc = "RAGH880125DA6"

   LET p_periodo_busqueda.v_f_ini = "04/06/2018"
   LET p_periodo_busqueda.v_f_fin = "05/01/2018"

   FOR v_indice = 1 TO 17
   LET p_movimientos[v_indice].v_f_aplicacion = "03/28/2018"
   LET p_movimientos[v_indice].v_cve_movimiento = "01"
   LET p_movimientos[v_indice].v_desc_concepto = "Aportación al saldo de la subcuenta de vivienda\n201801- NRP 11029289102\nINFONAVIT"
   LET p_movimientos[v_indice].v_tpo_movimiento = "vivienda 97"
   LET p_movimientos[v_indice].v_cargo = "100.00"
   LET p_movimientos[v_indice].v_abono = "250.00"
   END FOR

   FOR v_indice = 1 TO 48
   LET p_mov_aclara[v_indice].v_bimestre = "2018-05"
   LET p_mov_aclara[v_indice].v_reg_patronal = "11029289102"
   LET p_mov_aclara[v_indice].v_nombre_patron = "INFONAVIT"
   LET p_mov_aclara[v_indice].v_f_pago = "11/15/2018"
   LET p_mov_aclara[v_indice].v_aportacion = "4383.00"
   LET p_mov_aclara[v_indice].v_amortizacion = "678.54"
   END FOR

   LET p_adicionales.v_aviso = "En marzo, el @INEGI_INFORMA iniciará el Censo 2020. Colaboremos de manera segura desde nuestra casa, es muy importante brindar de manera amable y correcta la información que nos soliciten los entrevistadores. En el Censo participamos todas y todos."
   LET p_adicionales.v_cadena_original = "||3.3|FTVN|41|2017-08-14T00:00:00|01|30001000000300023699|10000|0|JPY|0.166123|11600|I|PUE|45070|SFE0807172W8|SFERP SC|601|XEXX010101000|Jorge Delgado Flores|G01|25101800|23534855|1|EA|PZA|Concepto con divisa JPY|10000|10000|10000|002|Tasa|0.160000|1600|0|002|Tasa|0.160000|1600|1600||"
   LET p_adicionales.v_sello_digital = "sello='f0Tb/489LdeGja/SeZWWe/FnFcH6fo08Vj5MeO67pEPg81bWvSRYaaf2OSD9TV10aoKFh+VpIrOBgj6bsx6sg+ UBByzb5iE6HxsSTSLT9GanGDi4Gw27t1KnGkeV915YsU58SBg4Er7H0vIw0XOBD5+d6YvNiwMZ2UBB5u20U75ekJKDQ9N oEY87DwumJTjtPtEYblyCs9aolVgPXHvr7SfZJZa1Rq0XUEVvsjfrSXrgP6FXWdpSDlUQRhTogamgrGHy5MXrfcPa+PUg p12zw3X31knyzFavOHyuqVLD9BnmrbY4Hj7jyzniFOj77tJ5HtthiGYro2AE08aBEJzmog=='"

   CALL fn_genera_rpt_movimientos_4cat(p_datos_personales.*,
                                       p_periodo_busqueda.*,
                                       p_movimientos,
                                       p_mov_aclara,
                                       p_adicionales.*)
   RETURNING r_ruta_lst,
             r_nom_reporte

   DISPLAY r_nom_reporte
END FUNCTION


FUNCTION fn_genera_rpt_amo_apo()
DEFINE p_datos_personales RECORD
          v_nombre   VARCHAR(40),
          v_paterno  VARCHAR(40),
          v_materno  VARCHAR(40),
          v_nss      CHAR(11),
          v_curp     CHAR(18),
          v_rfc      CHAR(13)
       END RECORD,
       p_periodo_busqueda RECORD
          v_f_ini DATE,
          v_f_fin DATE
       END RECORD,
       p_movimientos DYNAMIC ARRAY OF RECORD
          v_f_aplicacion DATE,
          v_cve_movimiento CHAR(5),
          v_desc_concepto STRING,
          v_tpo_movimiento STRING,
          v_cargo DECIMAL(16,2),
          v_abono DECIMAL(16,2)
       END RECORD,
       p_adicionales RECORD
          v_aviso VARCHAR(255),
          v_cadena_original STRING,
          v_sello_digital STRING
       END RECORD,
       v_indice SMALLINT,
       r_nom_reporte     STRING,
       r_ruta_lst        LIKE seg_modulo.ruta_listados
   

   LET p_datos_personales.v_nombre = "HUGO CESAR"
   LET p_datos_personales.v_paterno = "RAMIREZ"
   LET p_datos_personales.v_materno = "GARCIA"
   LET p_datos_personales.v_nss = "12345678901"
   LET p_datos_personales.v_curp = "RAGH880125HZSMRG01"
   LET p_datos_personales.v_rfc = "RAGH880125DA6"

   LET p_periodo_busqueda.v_f_ini = "04/06/2018"
   LET p_periodo_busqueda.v_f_fin = "05/01/2018"

   FOR v_indice = 1 TO 60
   LET p_movimientos[v_indice].v_f_aplicacion = "03/28/2015"
   LET p_movimientos[v_indice].v_cve_movimiento = "42"
   LET p_movimientos[v_indice].v_desc_concepto = "Aportación al saldo de la subcuenta de vivienda\n201706- NRP 11029289102\nINFONAVIT"
   LET p_movimientos[v_indice].v_tpo_movimiento = "vivienda 97"
   LET p_movimientos[v_indice].v_cargo = "10000.00"
   LET p_movimientos[v_indice].v_abono = "250.00"
   END FOR

   LET p_adicionales.v_aviso = "En marzo, el @INEGI_INFORMA iniciará el Censo 2020. Colaboremos de manera segura desde nuestra casa, es muy importante brindar de manera amable y correcta la información que nos soliciten los entrevistadores. En el Censo participamos todas y todos."
   LET p_adicionales.v_cadena_original = "||3.3|FTVN|41|2017-08-14T00:00:00|01|30001000000300023699|10000|0|JPY|0.166123|11600|I|PUE|45070|SFE0807172W8|SFERP SC|601|XEXX010101000|Jorge Delgado Flores|G01|25101800|23534855|1|EA|PZA|Concepto con divisa JPY|10000|10000|10000|002|Tasa|0.160000|1600|0|002|Tasa|0.160000|1600|1600||"
   LET p_adicionales.v_sello_digital = "sello='f0Tb/489LdeGja/SeZWWe/FnFcH6fo08Vj5MeO67pEPg81bWvSRYaaf2OSD9TV10aoKFh+VpIrOBgj6bsx6sg+ UBByzb5iE6HxsSTSLT9GanGDi4Gw27t1KnGkeV915YsU58SBg4Er7H0vIw0XOBD5+d6YvNiwMZ2UBB5u20U75ekJKDQ9N oEY87DwumJTjtPtEYblyCs9aolVgPXHvr7SfZJZa1Rq0XUEVvsjfrSXrgP6FXWdpSDlUQRhTogamgrGHy5MXrfcPa+PUg p12zw3X31knyzFavOHyuqVLD9BnmrbY4Hj7jyzniFOj77tJ5HtthiGYro2AE08aBEJzmog=='"

   CALL fn_genera_rpt_aportacion_amotizacion(p_datos_personales.*,
                                             p_periodo_busqueda.*,
                                             p_movimientos,
                                             p_adicionales.*)
   RETURNING r_ruta_lst,
             r_nom_reporte
   DISPLAY r_nom_reporte
END FUNCTION

FUNCTION fn_genera_rpt_comp()
DEFINE p_datos_personales RECORD
          v_nombre   VARCHAR(40),
          v_paterno  VARCHAR(40),
          v_materno  VARCHAR(40),
          v_nss      CHAR(11),
          v_curp     CHAR(18),
          v_rfc      CHAR(13)
       END RECORD,
       p_periodo_busqueda RECORD
          v_f_ini DATE,
          v_f_fin DATE
       END RECORD,
       p_movimientos DYNAMIC ARRAY OF RECORD
          v_f_aplicacion DATE,
          v_cve_movimiento CHAR(5),
          v_desc_concepto STRING,
          v_tpo_movimiento STRING,
          v_cargo DECIMAL(16,2),
          v_abono DECIMAL(16,2)
       END RECORD,
       p_adicionales RECORD
          v_aviso VARCHAR(255),
          v_cadena_original STRING,
          v_sello_digital STRING
       END RECORD,
       v_indice SMALLINT,
       r_nom_reporte     STRING,
       r_ruta_lst        LIKE seg_modulo.ruta_listados
   

   LET p_datos_personales.v_nombre = "HUGO CESAR"
   LET p_datos_personales.v_paterno = "RAMIREZ"
   LET p_datos_personales.v_materno = "GARCIA"
   LET p_datos_personales.v_nss = "12345678901"
   LET p_datos_personales.v_curp = "RAGH880125HZSMRG01"
   LET p_datos_personales.v_rfc = "RAGH880125DA6"

   LET p_periodo_busqueda.v_f_ini = "04/06/2018"
   LET p_periodo_busqueda.v_f_fin = "05/01/2018"

   FOR v_indice = 1 TO 17
   LET p_movimientos[v_indice].v_f_aplicacion = "03/28/2015"
   LET p_movimientos[v_indice].v_cve_movimiento = "172"
   LET p_movimientos[v_indice].v_desc_concepto = "Devolución de saldo (pensión)"
   LET p_movimientos[v_indice].v_tpo_movimiento = "vivienda 97"
   LET p_movimientos[v_indice].v_cargo = "250"
   LET p_movimientos[v_indice].v_abono = "400"
   END FOR

   LET p_adicionales.v_aviso = "En marzo, el @INEGI_INFORMA iniciará el Censo 2020. Colaboremos de manera segura desde nuestra casa, es muy importante brindar de manera amable y correcta la información que nos soliciten los entrevistadores. En el Censo participamos todas y todos."
   LET p_adicionales.v_cadena_original = "||3.3|FTVN|41|2017-08-14T00:00:00|01|30001000000300023699|10000|0|JPY|0.166123|11600|I|PUE|45070|SFE0807172W8|SFERP SC|601|XEXX010101000|Jorge Delgado Flores|G01|25101800|23534855|1|EA|PZA|Concepto con divisa JPY|10000|10000|10000|002|Tasa|0.160000|1600|0|002|Tasa|0.160000|1600|1600||"
   LET p_adicionales.v_sello_digital = "sello='f0Tb/489LdeGja/SeZWWe/FnFcH6fo08Vj5MeO67pEPg81bWvSRYaaf2OSD9TV10aoKFh+VpIrOBgj6bsx6sg+ UBByzb5iE6HxsSTSLT9GanGDi4Gw27t1KnGkeV915YsU58SBg4Er7H0vIw0XOBD5+d6YvNiwMZ2UBB5u20U75ekJKDQ9N oEY87DwumJTjtPtEYblyCs9aolVgPXHvr7SfZJZa1Rq0XUEVvsjfrSXrgP6FXWdpSDlUQRhTogamgrGHy5MXrfcPa+PUg p12zw3X31knyzFavOHyuqVLD9BnmrbY4Hj7jyzniFOj77tJ5HtthiGYro2AE08aBEJzmog=='"

   CALL fn_genera_rpt_movimientos_comp(p_datos_personales.*,
                                       p_periodo_busqueda.*,
                                       p_movimientos,
                                       p_adicionales.*)
   RETURNING r_ruta_lst,
             r_nom_reporte
   DISPLAY r_nom_reporte
END FUNCTION

FUNCTION fn_genera_rpt_aclara()
DEFINE p_datos_personales RECORD
          v_nombre   VARCHAR(40),
          v_paterno  VARCHAR(40),
          v_materno  VARCHAR(40),
          v_nss      CHAR(11),
          v_curp     CHAR(18),
          v_rfc      CHAR(13)
       END RECORD,
       p_periodo_busqueda RECORD
          v_f_ini DATE,
          v_f_fin DATE
       END RECORD,
       p_mov_aclara DYNAMIC ARRAY OF RECORD
          v_bimestre VARCHAR(7),
          v_reg_patronal VARCHAR(11),
          v_nombre_patron VARCHAR(40),
          v_f_pago DATE,
          v_aportacion DECIMAL(16,2),
          v_amortizacion DECIMAL(16,2)
       END RECORD,
       p_adicionales RECORD
          v_aviso VARCHAR(255),
          v_cadena_original STRING,
          v_sello_digital STRING
       END RECORD,
       v_indice SMALLINT,
       r_nom_reporte     STRING,
       r_ruta_lst        LIKE seg_modulo.ruta_listados
   

   LET p_datos_personales.v_nombre = "HUGO CESAR"
   LET p_datos_personales.v_paterno = "RAMIREZ"
   LET p_datos_personales.v_materno = "GARCIA"
   LET p_datos_personales.v_nss = "12345678901"
   LET p_datos_personales.v_curp = "RAGH880125HZSMRG01"
   LET p_datos_personales.v_rfc = "RAGH880125DA6"

   LET p_periodo_busqueda.v_f_ini = "04/06/2018"
   LET p_periodo_busqueda.v_f_fin = "05/01/2018"

   FOR v_indice = 1 TO 48
   LET p_mov_aclara[v_indice].v_bimestre = "2018-04"
   LET p_mov_aclara[v_indice].v_reg_patronal = "11029289102"
   LET p_mov_aclara[v_indice].v_nombre_patron = "INFONAVIT"
   LET p_mov_aclara[v_indice].v_f_pago = "09/21/2018"
   LET p_mov_aclara[v_indice].v_aportacion = "4383.00"
   LET p_mov_aclara[v_indice].v_amortizacion = "678.54"
   END FOR

   LET p_adicionales.v_aviso = "En marzo, el @INEGI_INFORMA iniciará el Censo 2020. Colaboremos de manera segura desde nuestra casa, es muy importante brindar de manera amable y correcta la información que nos soliciten los entrevistadores. En el Censo participamos todas y todos."
   LET p_adicionales.v_cadena_original = "||3.3|FTVN|41|2017-08-14T00:00:00|01|30001000000300023699|10000|0|JPY|0.166123|11600|I|PUE|45070|SFE0807172W8|SFERP SC|601|XEXX010101000|Jorge Delgado Flores|G01|25101800|23534855|1|EA|PZA|Concepto con divisa JPY|10000|10000|10000|002|Tasa|0.160000|1600|0|002|Tasa|0.160000|1600|1600||"
   LET p_adicionales.v_sello_digital = "sello='f0Tb/489LdeGja/SeZWWe/FnFcH6fo08Vj5MeO67pEPg81bWvSRYaaf2OSD9TV10aoKFh+VpIrOBgj6bsx6sg+ UBByzb5iE6HxsSTSLT9GanGDi4Gw27t1KnGkeV915YsU58SBg4Er7H0vIw0XOBD5+d6YvNiwMZ2UBB5u20U75ekJKDQ9N oEY87DwumJTjtPtEYblyCs9aolVgPXHvr7SfZJZa1Rq0XUEVvsjfrSXrgP6FXWdpSDlUQRhTogamgrGHy5MXrfcPa+PUg p12zw3X31knyzFavOHyuqVLD9BnmrbY4Hj7jyzniFOj77tJ5HtthiGYro2AE08aBEJzmog=='"

   CALL fn_genera_rpt_movimientos_aclara(p_datos_personales.*,
                                         p_periodo_busqueda.*,
                                         p_mov_aclara,
                                         p_adicionales.*)
   RETURNING r_ruta_lst,
             r_nom_reporte
   DISPLAY r_nom_reporte
END FUNCTION