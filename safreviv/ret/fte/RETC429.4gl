--===============================================================
-- Version: 1.0.0
--===============================================================

#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETC429                                                                #
#Objetivo     => Consulta Detalle Cargos a la SSV via SIAFF                             #
#Fecha inicio => Junio 15, 2017                                                         # 
#########################################################################################

DATABASE safre_viv  

GLOBALS "RETG01.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
	DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
          ,v_ventana                ui.WINDOW
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   --Se inicia el log del programa
   CALL STARTLOG (p_usuario_cod CLIPPED||".RETC428.log")
   
   -- consulta de informacion recibida 
   CALL fn_consulta_detalle(p_usuario_cod)

END MAIN

{ ============================================================================
Clave: RETC428
Nombre: fn_consulta_detalle
Fecha creacion: Noviembre 5, 2016
Registro de modificaciones:
Descrip: Consulta Detalle Cargos a la SSV via SIAFF
==============================================================================
}
FUNCTION fn_consulta_detalle(p_usuario_cod)
DEFINE 
      p_usuario_cod         LIKE seg_usuario.usuario_cod, -- clave del usuario
      v_folio               DECIMAL(9,0), -- folio
      v_folio_archivo       DECIMAL(9,0), -- folio
      v_archivo             LIKE glo_ctr_archivo.nombre_archivo,
      v_archivo_consulta    LIKE glo_ctr_archivo.nombre_archivo,
      v_tipo_solicitud      CHAR(4),
      v_desc_tipo_sol       CHAR(30),
      v_contador            INTEGER,
      v_cbx_folios          ui.ComboBox, -- combo de folios
      v_cbx_tipo_solicitud  ui.ComboBox, -- combo de folios
      v_cbx_folios_archivo  ui.ComboBox, -- combo de folios
      cmb_folios            SMALLINT,
      cmb_tipo_solicitud    CHAR(4),
      cmb_folios_archivo    SMALLINT,
      v_ed_nss              CHAR(11),
      v_d_f_pago_ini        DATE,
      v_d_f_pago_fin        DATE,
      v_d_f_liquida_ini     DATE,
      v_d_f_liquida_fin     DATE,
      v_ed_doc_fico         CHAR(10),
      v_resultado           SMALLINT,
      v_nss_paso            CHAR(11), 
      v_r_grupo   DYNAMIC ARRAY OF RECORD -- registro de despliegue del agrupador
         nss              CHAR(11),
         rfc              CHAR(13),
         doc_fico         CHAR(10),
         ejercicio        CHAR(4),
         fch_contable     DATE,
         fch_liquida      DATE,
         desc_tipo_sol    CHAR(25),
         folio            DECIMAL(9,0),
         subcta_47        SMALLINT,
         pesos_tesofe     DECIMAL(18,2),
         subcta_4         SMALLINT,
         acciones_viv97   DECIMAL(18,6),
         subcta_8         SMALLINT,
         acciones_viv92   DECIMAL(18,6),
         estado_solicitud SMALLINT
      END RECORD,

      v_r_grupo_indiv   RECORD -- registro de despliegue del agrupador
         nss              CHAR(11),
         rfc              CHAR(13),
         doc_fico         CHAR(10),
         ejercicio        CHAR(4),
         fch_contable     DATE,
         fch_liquida      DATE,
         desc_tipo_sol    CHAR(25),
         folio            DECIMAL(9,0),
         subcta_47        SMALLINT,
         pesos_tesofe     DECIMAL(18,2),
         subcta_4         SMALLINT,
         acciones_viv97   DECIMAL(18,6),
         subcta_8         SMALLINT,
         acciones_viv92   DECIMAL(18,6),
         estado_solicitud SMALLINT
      END RECORD,

      v_arr_reg_detalle         RECORD
         ed_tot_reg               INTEGER                   ,
         ed_pesos_debita          DECIMAL(14,2)                                 ,
         ed_aivs_debita           DECIMAL(18,6),
         ed_total_pagos           INTEGER, 
         ed_sin_dif               INTEGER,
         ed_fch_carga             DATE,
         ed_aivs_viv_97           DECIMAL(18,6),
         ed_aivs_viv_92           DECIMAL(18,6),
         ed_pesos_viv_97          DECIMAL(14,2),
         ed_pesos_viv_92          DECIMAL(14,2),
         ed_nss_no_loc            INTEGER,
         ed_pesos_nss_no_loc      DECIMAL(14,2),
         ed_aivs_nss_no_loc       DECIMAL(18,6)
       END RECORD,
       v_query                       STRING, -- detalle
       v_indice                      INTEGER, -- indice de arreglo
       v_precio_fondo                LIKE ret_cza_transferencia.precio_fondo

       
   OPEN WINDOW w_consulta_saldos WITH FORM "RETC4291"
   LET  v_ventana = UI.WINDOW.GETCURRENT()
   CALL v_ventana.SETTEXT("Consulta Cifras Control")

   -- Se llenan los combobox
   LET  v_cbx_folios = ui.ComboBox.forName("cmb_folios")
   CALL v_cbx_folios.clear()

   -- Se obtienen los folios
   LET v_query = " SELECT a.folio, b.nombre_archivo " ||
                 " FROM   glo_folio a, glo_ctr_archivo b " ||
                 " WHERE  a.folio = b.folio " ||
                 " AND    a.proceso_cod = 1586 "
   PREPARE prp_folios FROM v_query
   DECLARE cur_folios CURSOR FOR prp_folios

   LET v_contador = 0
   CALL v_cbx_folios.addItem(0,"Seleccione un folio")
   LET v_contador = v_contador + 1
   FOREACH cur_folios INTO v_folio,v_archivo
      CALL v_cbx_folios.addItem(v_folio,v_folio||" - "||v_archivo)
      LET v_contador = v_contador + 1
   END FOREACH

   IF v_contador > 0  THEN
   CALL v_cbx_folios.removeItem(v_contador)
   END IF

      -- Se llenan los combobox
   LET  v_cbx_tipo_solicitud = ui.ComboBox.forName("cmb_tipo_solicitud")
   CALL v_cbx_tipo_solicitud.clear()

   -- Se obtienen los tipos de Solicitud
   LET v_query = " SELECT tipo_sol, desc_tipo_sol " ||
                 " FROM   ret_cat_tipo_sol_ssv " 
   PREPARE prp_tipo_solicitud FROM v_query
   DECLARE cur_tipo_solicitud CURSOR FOR prp_tipo_solicitud

   LET v_contador = 0
   CALL v_cbx_tipo_solicitud.addItem(0,"Seleccione un tipo de Solicitud")
   LET v_contador = v_contador + 1
   FOREACH cur_tipo_solicitud INTO v_tipo_solicitud,v_desc_tipo_sol
      CALL v_cbx_tipo_solicitud.addItem(v_tipo_solicitud,v_tipo_solicitud||" - "||v_desc_tipo_sol)
      LET v_contador = v_contador + 1
   END FOREACH

   IF v_contador > 0  THEN
   CALL v_cbx_tipo_solicitud.removeItem(v_contador)
   END IF

   LET  v_cbx_folios_archivo = ui.ComboBox.forName("cmb_folios_archivo")
   CALL v_cbx_folios_archivo.clear()

   -- Se obtienen los folios de los archivos para consulta
   LET v_query = " SELECT a.folio, b.nombre_archivo " ||
                 " FROM   glo_folio a, glo_ctr_archivo b " ||
                 " WHERE  a.folio = b.folio " ||
                 " AND    a.proceso_cod = 1587 "
   PREPARE prp_folios_archivo FROM v_query
   DECLARE cur_folios_archivo CURSOR FOR prp_folios_archivo

   LET v_contador = 0
   CALL v_cbx_folios_archivo.addItem(0,"Seleccione un folio")
   LET v_contador = v_contador + 1
   FOREACH cur_folios_archivo INTO v_folio_archivo,v_archivo_consulta
      CALL v_cbx_folios_archivo.addItem(v_folio_archivo, v_folio_archivo||" - "||v_archivo_consulta)
      LET v_contador = v_contador + 1
   END FOREACH

   IF v_contador > 0  THEN
   CALL v_cbx_folios_archivo.removeItem(v_contador)
   END IF
   
   INPUT v_folio, v_tipo_solicitud, v_ed_nss, v_d_f_pago_ini, v_d_f_pago_fin,
         v_d_f_liquida_ini, v_d_f_liquida_fin, v_ed_doc_fico, v_folio_archivo
      FROM cmb_folios, cmb_tipo_solicitud, ed_nss, d_f_pago_ini, d_f_pago_fin,
         d_f_liquida_ini, d_f_liquida_fin, ed_doc_fico, cmb_folios_archivo
      ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)

      BEFORE INPUT 
         -- se limpian las variables
         LET v_folio = 0
         LET v_tipo_solicitud = "";
         LET v_folio_archivo = 0;
         LET v_d_f_liquida_fin = NULL;
         LET v_d_f_liquida_ini = NULL;
         LET v_d_f_pago_fin    = NULL;
         LET v_d_f_pago_ini    = NULL;
         
         CALL v_r_grupo.clear() 
         CALL f_limpia_arreglo(v_arr_reg_detalle.*) RETURNING v_arr_reg_detalle.*

      ON ACTION ACCEPT

         -- se borran los arreglos de despliegue
         CALL v_r_grupo.clear()
         CALL f_limpia_arreglo(v_arr_reg_detalle.*) RETURNING v_arr_reg_detalle.*

         --modificación de validación de  captura de parametros
         --valida que se ingrese al menos un parametro
         IF ( (v_folio IS NULL OR v_folio = 0) AND 
              (v_tipo_solicitud IS NULL OR v_tipo_solicitud = "" ) AND
              (v_ed_nss IS NULL OR v_ed_nss = "" ) AND
              (v_d_f_pago_ini IS NULL) AND
              (v_d_f_pago_fin IS NULL) AND 
              (v_d_f_liquida_ini IS NULL) AND 
              (v_d_f_liquida_fin IS NULL) AND 
              (v_ed_doc_fico IS NULL )    AND
              (v_folio_archivo IS NULL OR v_folio_archivo = 0)) THEN
            CALL fn_mensaje("Consulta","Debe de ingresar al menos un criterio de búsqueda","about")
            CONTINUE INPUT
         ELSE 
            IF (v_d_f_liquida_ini IS NOT NULL AND v_d_f_liquida_fin IS NULL ) OR
               (v_d_f_liquida_ini IS NULL AND v_d_f_liquida_fin IS NOT NULL ) THEN 
               CALL fn_mensaje("Consulta","Debe Capturar ambas fechas de liquidación","about")
               CONTINUE INPUT 
            ELSE 
               IF (v_d_f_pago_ini IS NOT NULL AND v_d_f_pago_fin IS NULL ) OR
                  (v_d_f_pago_ini IS NULL AND v_d_f_pago_fin IS NOT NULL ) THEN 
                  CALL fn_mensaje("Consulta","Debe Capturar ambas fechas de pago","about")
                  CONTINUE INPUT 
               ELSE 
                  IF v_folio_archivo IS NOT NULL AND v_folio_archivo <> 0 THEN 
                     LET v_query = "\n SELECT a.nss, a.rfc, a.no_documento, a.ejercicio, a.fch_contable, ",
                                   "\n        CASE WHEN b.f_liquida IS NULL THEN (CASE WHEN d.f_liquida IS NULL THEN e.f_liquida ELSE d.f_liquida END) ELSE b.f_liquida END, ",
                                   "\n        CASE WHEN f.desc_tipo_sol IS NULL THEN 'SIN DESCRIPCIÓN' ELSE f.desc_tipo_sol END, a.folio, ",
                                   "\n        b.subcuenta, NVL(b.monto_acciones,0), d.subcuenta, NVL(d.monto_acciones,0), e.subcuenta, NVL(e.monto_acciones,0), a.estado_solicitud ",
                                   "\n FROM   ret_cargos_ssv_siaff a ",
                                   "\n        LEFT OUTER JOIN cta_movimiento b ",
                                   "\n                     ON b.id_referencia      = a.id_solicitud ",
                                   "\n                    AND b.folio_liquida      = a.folio ",
                                   "\n                    AND b.subcuenta          = 47 ",
                                   "\n                    AND b.movimiento IN (1802,1842,1862,1882,1902,1922,1942, ",
                                   "\n                                         1832,1852,1872,1892,1912,1932,1952) ",
                                   "\n        LEFT OUTER JOIN cta_movimiento d ",
                                   "\n                    ON d.id_referencia      = a.id_solicitud ",
                                   "\n                   AND d.folio_liquida      = a.folio ",
                                   "\n                   AND d.subcuenta          = 4 ",
                                   "\n                   AND d.movimiento IN (1802,1842,1862,1882,1902,1922,1942, ",
                                   "\n                                        1832,1852,1872,1892,1912,1932,1952) ",
                                   "\n       LEFT OUTER JOIN cta_movimiento e ",
                                   "\n                    ON e.id_referencia      = a.id_solicitud ",
                                   "\n                   AND e.folio_liquida      = a.folio ",
                                   "\n                   AND e.subcuenta          = 8 ",
                                   "\n                   AND e.movimiento IN (1802,1842,1862,1882,1902,1922,1942, ",
                                   "\n                                        1832,1852,1872,1892,1912,1932,1952) ",
                                   "\n       LEFT OUTER JOIN ret_cat_tipo_sol_ssv f ",
                                   "\n                    ON a.tipo_sol = f.tipo_sol ",
                                   "\n WHERE  1 = 1 ",
                                   "\n AND    a.nss IN  (",
                                   "\n                   SELECT nss",
                                   "\n                   FROM   ret_det_cargo_ssv_consulta",
                                   "\n                   WHERE  folio = ", v_folio_archivo, ")"
                                   -- Modificacion omite importes en cero 06/10/2020
                                   LET v_query = v_query CLIPPED, " AND ((b.monto_acciones is not null)"
                                   LET v_query = v_query CLIPPED, " OR (d.monto_acciones is not null)"
                                   LET v_query = v_query CLIPPED, " OR (e.monto_acciones is not null))"
                  ELSE 
                     LET v_query = "\n SELECT a.nss, a.rfc, a.no_documento, a.ejercicio, a.fch_contable, ",
                                   "\n        CASE WHEN b.f_liquida IS NULL THEN (CASE WHEN d.f_liquida IS NULL THEN e.f_liquida ELSE d.f_liquida END) ELSE b.f_liquida END AS fecha_liquida, ",
                                   "\n        CASE WHEN f.desc_tipo_sol IS NULL THEN 'SIN DESCRIPCIÓN' ELSE f.desc_tipo_sol END AS tipo_solicitud, a.folio, ",
                                   "\n        b.subcuenta AS subtesofe, NVL(b.monto_acciones,0) AS pesos_tesofe, d.subcuenta AS subviv97, NVL(d.monto_acciones,0) AS accviv97, ",
                                   "\n        e.subcuenta AS subviv92, NVL(e.monto_acciones,0) AS accviv92, a.estado_solicitud ",
                                   "\n FROM   ret_cargos_ssv_siaff a ",
                                   "\n        LEFT OUTER JOIN cta_movimiento b ",
                                   "\n                     ON b.id_referencia      = a.id_solicitud ",
                                   "\n                    AND b.folio_liquida      = a.folio ",
                                   "\n                    AND b.movimiento IN (1802,1842,1862,1882,1902,1922,1942, ",
                                   "\n                                         1832,1852,1872,1892,1912,1932,1952) ",
                                   "\n                    AND b.subcuenta          = 47 "
                     IF v_d_f_liquida_ini IS NOT NULL AND v_d_f_liquida_fin IS NOT NULL THEN  
                        LET v_query = v_query CLIPPED, "\n        AND b.f_liquida BETWEEN '", v_d_f_liquida_ini, "' AND '", v_d_f_liquida_fin, "'"
                     END IF 
                     LET v_query = v_query CLIPPED, "\n        LEFT OUTER JOIN cta_movimiento d ",
                                   "\n                    ON d.id_referencia      = a.id_solicitud ",
                                   "\n                   AND d.folio_liquida      = a.folio ",
                                   "\n                   AND d.movimiento IN (1802,1842,1862,1882,1902,1922,1942, ",
                                   "\n                                        1832,1852,1872,1892,1912,1932,1952) ",
                                   "\n                   AND d.subcuenta          = 4 "
                     IF v_d_f_liquida_ini IS NOT NULL AND v_d_f_liquida_fin IS NOT NULL THEN  
                        LET v_query = v_query CLIPPED, "\n        AND d.f_liquida BETWEEN '", v_d_f_liquida_ini, "' AND '", v_d_f_liquida_fin, "'"
                     END IF 
                     LET v_query = v_query CLIPPED, "\n       LEFT OUTER JOIN cta_movimiento e ",
                                   "\n                    ON e.id_referencia      = a.id_solicitud ",
                                   "\n                   AND e.folio_liquida      = a.folio ",
                                   "\n                   AND e.movimiento IN (1802,1842,1862,1882,1902,1922,1942, ",
                                   "\n                                        1832,1852,1872,1892,1912,1932,1952) ",
                                   "\n                   AND e.subcuenta          = 8 "
                     IF v_d_f_liquida_ini IS NOT NULL AND v_d_f_liquida_fin IS NOT NULL THEN  
                        LET v_query = v_query CLIPPED, "\n        AND e.f_liquida BETWEEN '", v_d_f_liquida_ini, "' AND '", v_d_f_liquida_fin, "'"
                     END IF 
                     LET v_query = v_query CLIPPED, "\n       LEFT OUTER JOIN ret_cat_tipo_sol_ssv f ",
                                   "\n                    ON a.tipo_sol = f.tipo_sol "
                     LET v_query = v_query CLIPPED, "\n WHERE  1 = 1 "
                     IF v_folio IS NOT NULL AND v_folio <> 0 THEN 
                        LET v_query = v_query CLIPPED, "\n AND    a.folio = ", v_folio
                     END IF 
                     IF v_ed_nss IS NOT NULL THEN 
                        LET v_query = v_query CLIPPED, "\n AND    a.nss = '", v_ed_nss ,"'"
                     END IF 
                     IF v_d_f_pago_ini IS NOT NULL AND v_d_f_pago_fin IS NOT NULL THEN 
                        LET v_query = v_query CLIPPED, "\n AND    a.fch_contable BETWEEN '", v_d_f_pago_ini, "' AND '", v_d_f_pago_fin, "'"
                     END IF 
                     IF v_ed_doc_fico IS NOT NULL THEN 
                        LET v_query = v_query CLIPPED, "\n AND    a.no_documento = ", v_ed_doc_fico 
                     END IF 
                     IF v_tipo_solicitud IS NOT NULL THEN 
                        LET v_query = v_query CLIPPED, "\n AND    a.tipo_sol = '", v_tipo_solicitud, "'"
                     END IF 
                     -- Modificacion omite importes en cero 06/10/2020
                     LET v_query = v_query CLIPPED, " AND ((b.monto_acciones is not null)"
                     LET v_query = v_query CLIPPED, " OR (d.monto_acciones is not null)"
                     LET v_query = v_query CLIPPED, " OR (e.monto_acciones is not null))"
                     IF v_d_f_liquida_ini IS NOT NULL AND v_d_f_liquida_fin IS NOT NULL THEN 
                        LET v_query = " SELECT * FROM (" ,v_query CLIPPED, ")",
                                      "\n WHERE fecha_liquida IS NOT NULL"
                     END IF 
                  END IF 
               END IF 
            END IF 
            
            -- consulta
            DISPLAY "Consulta:\n", v_query

            -- se llena el arreglo 
            PREPARE sid_query FROM v_query
            DECLARE cur_query CURSOR FOR sid_query
            
            LET v_indice = 1
            LET v_nss_paso = NULL
            -- se transfieren los datos al arreglo de despliegue agrupador
            --FOREACH cur_query INTO v_r_grupo_indiv.*
            FOREACH cur_query INTO v_r_grupo[v_indice].*
               DISPLAY "entra"
               {IF v_nss_paso = v_r_grupo_indiv.nss THEN 
                  LET v_r_grupo[v_indice].acciones_viv97 = v_r_grupo[v_indice].acciones_viv97 + v_r_grupo_indiv.acciones_viv97
               ELSE
                  LET v_nss_paso =  v_r_grupo_indiv.nss}
                  LET v_indice = v_indice + 1
                --  LET v_r_grupo[v_indice].* = v_r_grupo_indiv.*
                  
              -- END IF
               -- se incrementa el indice
               IF v_indice > 90000 THEN 
                  CALL fn_mensaje("Atención", "La consulta arrojo demasiados registros,\n solo se mostrarán 90,000 \n el total se encuentra en el archivo generado", "information")
                  EXIT FOREACH 
               END IF 
            END FOREACH
            IF v_indice > 0 THEN 
               DISPLAY ARRAY v_r_grupo TO tbl_detalle.*
               ON ACTION Exporta
                  CALL fn_exporta_archivo(v_query) RETURNING v_resultado;
               ON ACTION CANCEL 
                  EXIT DISPLAY 
               END DISPLAY 
            ELSE 
               CALL fn_mensaje("Atención", "La consulta no arrojo resultados", "information")
            END IF 
         END IF           
         
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_saldos

END FUNCTION

{ ======================================================================
Clave: 
Nombre: rpt_detalle_saldos_insuf
Fecha creacion: Junio 20, 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Genera el reporte de los saldos insuficientes 

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
REPORT rpt_detalle_saldos_insuf(p_indice, v_r_despliegue, p_usuario_cod, p_agrupador_folio_fecha_Afore)
DEFINE    v_r_despliegue          RECORD -- registro de consulta
           v_folio                  LIKE ret_transferencia.folio,
           v_f_carga                LIKE ret_cza_transferencia.f_carga,
           v_nss                    LIKE afi_derechohabiente.nss,
           v_nombre_trabajador      STRING, 
           v_aivs92_sol             DECIMAL(24,6),
           v_aivs97_sol             DECIMAL(24,6),
           v_aivs92_sol_pesos       DECIMAL(22,2),
           v_aivs97_sol_pesos       DECIMAL(22,2),
           v_aivs92_sdo             DECIMAL(24,6),
           v_aivs97_sdo             DECIMAL(24,6),
           v_aivsvol_sdo            DECIMAL(24,6),
           v_aivssum97vol_sdo       DECIMAL(24,6),
           v_aivs92_sdo_pesos       DECIMAL(22,2),
           v_aivs97_sdo_pesos       DECIMAL(22,2),
           v_aivsvol_sdo_pesos      DECIMAL(22,2),
           v_aivssum97vol_sdo_pesos DECIMAL(22,2),
           v_aivs92_dif             DECIMAL(24,6),
           v_aivs97_dif             DECIMAL(24,6),
           v_aivs92_dif_pesos       DECIMAL(22,2),        
           v_aivs97_dif_pesos       DECIMAL(22,2)
          END RECORD,
          p_indice                DECIMAL(9,0),
          p_usuario_cod           LIKE seg_usuario.usuario_cod, -- usuario en linea
          v_fecha                 STRING, -- fecha de emision del reporte
          v_nombre_usuario        VARCHAR(100),
          -- variables para acumular TOTAL PARCIAL
          v_total_afore_aivs92_sol             DECIMAL(24,6),
          v_total_afore_aivs97_sol             DECIMAL(24,6),
          v_total_afore_aivs92_sol_pesos       DECIMAL(22,2),
          v_total_afore_aivs97_sol_pesos       DECIMAL(22,2),
          v_total_afore_aivs92_sdo             DECIMAL(24,6),
          v_total_afore_aivs97_sdo             DECIMAL(24,6),
          v_total_afore_aivsvol_sdo            DECIMAL(24,6),
          v_total_afore_aivssum97vol_sdo       DECIMAL(24,6),
          v_total_afore_aivs92_sdo_pesos       DECIMAL(22,2),
          v_total_afore_aivs97_sdo_pesos       DECIMAL(22,2),
          v_total_afore_aivsvol_sdo_pesos      DECIMAL(22,2),
          v_total_afore_aivssum97vol_sdo_pesos DECIMAL(22,2),
          v_total_afore_aivs92_dif             DECIMAL(24,6),
          v_total_afore_aivs97_dif             DECIMAL(24,6),
          v_total_afore_aivs92_dif_pesos       DECIMAL(22,2),
          v_total_afore_aivs97_dif_pesos       DECIMAL(22,2),
          p_total_afore_regs                   DECIMAL(9,0),
          -- variables para acumular GRAN TOTAL
          v_total_aivs92_sol             DECIMAL(24,6),
          v_total_aivs97_sol             DECIMAL(24,6),
          v_total_aivs92_sol_pesos       DECIMAL(22,2),
          v_total_aivs97_sol_pesos       DECIMAL(22,2),
          v_total_aivs92_sdo             DECIMAL(24,6),
          v_total_aivs97_sdo             DECIMAL(24,6),
          v_total_aivsvol_sdo            DECIMAL(24,6),
          v_total_aivssum97vol_sdo       DECIMAL(24,6),
          v_total_aivs92_sdo_pesos       DECIMAL(22,2),
          v_total_aivs97_sdo_pesos       DECIMAL(22,2),
          v_total_aivsvol_sdo_pesos      DECIMAL(22,2),
          v_total_aivssum97vol_sdo_pesos DECIMAL(22,2),
          v_total_aivs92_dif             DECIMAL(24,6),
          v_total_aivs97_dif             DECIMAL(24,6),
          v_total_aivs92_dif_pesos       DECIMAL(22,2),
          v_total_aivs97_dif_pesos       DECIMAL(22,2),
          p_total_regs                   DECIMAL(9,0),
          v_fecha_carga                  STRING,
          p_agrupador_folio_fecha_Afore  STRING
          
FORMAT

   FIRST PAGE HEADER
      
      -- variables para acumular gran total
      LET v_total_afore_aivs92_sol             = 0
      LET v_total_afore_aivs97_sol             = 0
      LET v_total_afore_aivs92_sol_pesos       = 0
      LET v_total_afore_aivs97_sol_pesos       = 0
      LET v_total_afore_aivs92_sdo             = 0
      LET v_total_afore_aivs97_sdo             = 0
      LET v_total_afore_aivsvol_sdo            = 0
      LET v_total_afore_aivssum97vol_sdo       = 0
      LET v_total_afore_aivs92_sdo_pesos       = 0
      LET v_total_afore_aivs97_sdo_pesos       = 0
      LET v_total_afore_aivsvol_sdo_pesos      = 0
      LET v_total_afore_aivssum97vol_sdo_pesos = 0
      LET v_total_afore_aivs92_dif             = 0
      LET v_total_afore_aivs97_dif             = 0
      LET v_total_afore_aivs92_dif_pesos       = 0
      LET v_total_afore_aivs97_dif_pesos       = 0
      LET p_total_afore_regs                   = 0
      
      -- variables para acumular por afore, fecha y folio
      LET v_total_aivs92_sol              = 0
      LET v_total_aivs97_sol              = 0
      LET v_total_aivs92_sol_pesos        = 0
      LET v_total_aivs97_sol_pesos        = 0
      LET v_total_aivs92_sdo              = 0
      LET v_total_aivs97_sdo              = 0
      LET v_total_aivsvol_sdo             = 0
      LET v_total_aivssum97vol_sdo        = 0
      LET v_total_aivs92_sdo_pesos        = 0
      LET v_total_aivs97_sdo_pesos        = 0
      LET v_total_aivsvol_sdo_pesos       = 0
      LET v_total_aivssum97vol_sdo_pesos  = 0
      LET v_total_aivs92_dif              = 0
      LET v_total_aivs97_dif              = 0
      LET v_total_aivs92_dif_pesos        = 0
      LET v_total_aivs97_dif_pesos        = 0
      LET p_total_regs                    = 0

      -- se envia folio, usuario y fecha
      LET v_fecha = TODAY USING "dd-mm-yyyy"
      -- se obtiene el nombre del usuario
      SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
      WHERE usuario_cod = p_usuario_cod

      LET v_nombre_usuario = v_nombre_usuario CLIPPED
      
      PRINTX p_usuario_cod, v_fecha, v_nombre_usuario
      
   --BEFORE GROUP OF v_r_despliegue.v_afore
   BEFORE GROUP OF p_agrupador_folio_fecha_Afore
      LET v_fecha_carga = v_r_despliegue.v_f_carga USING "dd-mm-yyyy"
      
      -- se reinician los totales por afore, fecha y folio   
      LET v_total_afore_aivs92_sol             = 0
      LET v_total_afore_aivs97_sol             = 0
      LET v_total_afore_aivs92_sol_pesos       = 0
      LET v_total_afore_aivs97_sol_pesos       = 0
      LET v_total_afore_aivs92_sdo             = 0
      LET v_total_afore_aivs97_sdo             = 0
      LET v_total_afore_aivsvol_sdo            = 0
      LET v_total_afore_aivssum97vol_sdo       = 0
      LET v_total_afore_aivs92_sdo_pesos       = 0
      LET v_total_afore_aivs97_sdo_pesos       = 0
      LET v_total_afore_aivsvol_sdo_pesos      = 0
      LET v_total_afore_aivssum97vol_sdo_pesos = 0
      LET v_total_afore_aivs92_dif             = 0
      LET v_total_afore_aivs97_dif             = 0
      LET v_total_afore_aivs92_dif_pesos       = 0
      LET v_total_afore_aivs97_dif_pesos       = 0
      LET p_total_afore_regs                   = 0
   
      PRINTX v_r_despliegue.v_folio,
             v_fecha_carga
             

   ON EVERY ROW
      PRINTX v_r_despliegue.*
      DISPLAY v_r_despliegue.*
      
      -- se acumulan los montos para total por afore, folio y fecha de carga
      LET v_total_afore_aivs92_sol             = v_total_afore_aivs92_sol             + v_r_despliegue.v_aivs92_sol      
      LET v_total_afore_aivs97_sol             = v_total_afore_aivs97_sol             + v_r_despliegue.v_aivs97_sol      
      LET v_total_afore_aivs92_sol_pesos       = v_total_afore_aivs92_sol_pesos       + v_r_despliegue.v_aivs92_sol_pesos
      LET v_total_afore_aivs97_sol_pesos       = v_total_afore_aivs97_sol_pesos       + v_r_despliegue.v_aivs97_sol_pesos
      LET v_total_afore_aivs92_sdo             = v_total_afore_aivs92_sdo             + v_r_despliegue.v_aivs92_sdo      
      LET v_total_afore_aivs97_sdo             = v_total_afore_aivs97_sdo             + v_r_despliegue.v_aivs97_sdo      
      LET v_total_afore_aivsvol_sdo            = v_total_afore_aivsvol_sdo            + v_r_despliegue.v_aivsvol_sdo      
      LET v_total_afore_aivssum97vol_sdo       = v_total_afore_aivssum97vol_sdo       + v_r_despliegue.v_aivssum97vol_sdo      
      LET v_total_afore_aivs92_sdo_pesos       = v_total_afore_aivs92_sdo_pesos       + v_r_despliegue.v_aivs92_sdo_pesos
      LET v_total_afore_aivs97_sdo_pesos       = v_total_afore_aivs97_sdo_pesos       + v_r_despliegue.v_aivs97_sdo_pesos
      LET v_total_afore_aivsvol_sdo_pesos      = v_total_afore_aivsvol_sdo_pesos      + v_r_despliegue.v_aivsvol_sdo_pesos
      LET v_total_afore_aivssum97vol_sdo_pesos = v_total_afore_aivssum97vol_sdo_pesos + v_r_despliegue.v_aivssum97vol_sdo_pesos
      LET v_total_afore_aivs92_dif             = v_total_afore_aivs92_dif             + v_r_despliegue.v_aivs92_dif      
      LET v_total_afore_aivs97_dif             = v_total_afore_aivs97_dif             + v_r_despliegue.v_aivs97_dif      
      LET v_total_afore_aivs92_dif_pesos       = v_total_afore_aivs92_dif_pesos       + v_r_despliegue.v_aivs92_dif_pesos
      LET v_total_afore_aivs97_dif_pesos       = v_total_afore_aivs97_dif_pesos       + v_r_despliegue.v_aivs97_dif_pesos
      LET p_total_afore_regs                   = p_total_afore_regs                   + 1
      
      
      -- se acumulan los montos para gran total
      LET v_total_aivs92_sol             = v_total_aivs92_sol             + v_r_despliegue.v_aivs92_sol      
      LET v_total_aivs97_sol             = v_total_aivs97_sol             + v_r_despliegue.v_aivs97_sol      
      LET v_total_aivs92_sol_pesos       = v_total_aivs92_sol_pesos       + v_r_despliegue.v_aivs92_sol_pesos
      LET v_total_aivs97_sol_pesos       = v_total_aivs97_sol_pesos       + v_r_despliegue.v_aivs97_sol_pesos
      LET v_total_aivs92_sdo             = v_total_aivs92_sdo             + v_r_despliegue.v_aivs92_sdo      
      LET v_total_aivs97_sdo             = v_total_aivs97_sdo             + v_r_despliegue.v_aivs97_sdo     
      LET v_total_aivsvol_sdo            = v_total_aivsvol_sdo            + v_r_despliegue.v_aivsvol_sdo     
      LET v_total_aivssum97vol_sdo       = v_total_aivssum97vol_sdo       + v_r_despliegue.v_aivssum97vol_sdo      
      LET v_total_aivs92_sdo_pesos       = v_total_aivs92_sdo_pesos       + v_r_despliegue.v_aivs92_sdo_pesos
      LET v_total_aivs97_sdo_pesos       = v_total_aivs97_sdo_pesos       + v_r_despliegue.v_aivs97_sdo_pesos
      LET v_total_aivsvol_sdo_pesos      = v_total_aivsvol_sdo_pesos      + v_r_despliegue.v_aivsvol_sdo_pesos
      LET v_total_aivssum97vol_sdo_pesos = v_total_aivssum97vol_sdo_pesos + v_r_despliegue.v_aivssum97vol_sdo_pesos
      LET v_total_aivs92_dif             = v_total_aivs92_dif             + v_r_despliegue.v_aivs92_dif      
      LET v_total_aivs97_dif             = v_total_aivs97_dif             + v_r_despliegue.v_aivs97_dif      
      LET v_total_aivs92_dif_pesos       = v_total_aivs92_dif_pesos       + v_r_despliegue.v_aivs92_dif_pesos
      LET v_total_aivs97_dif_pesos       = v_total_aivs97_dif_pesos       + v_r_despliegue.v_aivs97_dif_pesos
      LET p_total_regs                   = p_total_regs                   + 1

   --AFTER GROUP OF v_r_despliegue.v_afore
   AFTER GROUP OF p_agrupador_folio_fecha_Afore
      PRINTX v_total_afore_aivs92_sol             ,
             v_total_afore_aivs97_sol             ,
             v_total_afore_aivs92_sol_pesos       ,
             v_total_afore_aivs97_sol_pesos       ,
             v_total_afore_aivs92_sdo             ,
             v_total_afore_aivs97_sdo             ,
             v_total_afore_aivsvol_sdo            ,
             v_total_afore_aivssum97vol_sdo       ,
             v_total_afore_aivs92_sdo_pesos       ,
             v_total_afore_aivs97_sdo_pesos       ,
             v_total_afore_aivsvol_sdo_pesos      ,
             v_total_afore_aivssum97vol_sdo_pesos ,
             v_total_afore_aivs92_dif             ,
             v_total_afore_aivs97_dif             ,
             v_total_afore_aivs92_dif_pesos       ,
             v_total_afore_aivs97_dif_pesos       ,
             p_total_afore_regs            
                                                          
   
   ON LAST ROW 
      PRINTX p_total_regs                   ,
             v_total_aivs92_sol             ,
             v_total_aivs97_sol             ,
             v_total_aivs92_sol_pesos       ,
             v_total_aivs97_sol_pesos       ,
             v_total_aivs92_sdo             ,
             v_total_aivs97_sdo             ,
             v_total_aivsvol_sdo            ,
             v_total_aivssum97vol_sdo       ,
             v_total_aivs92_sdo_pesos       ,
             v_total_aivs97_sdo_pesos       ,
             v_total_aivsvol_sdo_pesos      ,
             v_total_aivssum97vol_sdo_pesos ,
             v_total_aivs92_dif             ,
             v_total_aivs97_dif             ,
             v_total_aivs92_dif_pesos       ,
             v_total_aivs97_dif_pesos 

END REPORT

FUNCTION f_limpia_arreglo(p_arr_detalle)
   DEFINE 
      p_arr_detalle         RECORD
         ed_tot_reg               INTEGER,
         ed_pesos_debita          DECIMAL(14,2),
         ed_aivs_debita           DECIMAL(18,6),
         ed_total_pagos           INTEGER, 
         ed_sin_dif               INTEGER,
         ed_fch_carga             DATE,
         ed_aivs_viv_97           DECIMAL(18,6),
         ed_aivs_viv_92           DECIMAL(18,6),
         ed_pesos_viv_97          DECIMAL(14,2),
         ed_pesos_viv_92          DECIMAL(14,2),
         ed_nss_no_loc            INTEGER,
         ed_pesos_nss_no_loc      DECIMAL(14,2),
         ed_aivs_nss_no_loc       DECIMAL(18,6)
      END RECORD

   LET p_arr_detalle.ed_tot_reg = 0;
   LET p_arr_detalle.ed_pesos_debita = 0;
   LET p_arr_detalle.ed_aivs_debita = 0;
   LET p_arr_detalle.ed_total_pagos = 0;
   LET p_arr_detalle.ed_sin_dif = 0;
   LET p_arr_detalle.ed_fch_carga = NULL;
   LET p_arr_detalle.ed_aivs_viv_97 = 0;
   LET p_arr_detalle.ed_aivs_viv_92 = 0;
   LET p_arr_detalle.ed_pesos_viv_97 = 0;
   LET p_arr_detalle.ed_pesos_viv_92 = 0;
   LET p_arr_detalle.ed_nss_no_loc = 0;
   LET p_arr_detalle.ed_pesos_nss_no_loc = 0;
   LET p_arr_detalle.ed_aivs_nss_no_loc = 0;


   RETURN p_arr_detalle.*
 
END FUNCTION

--Función que exporta datos a un archivo
FUNCTION fn_exporta_archivo(p_query)
DEFINE p_r_grupo   DYNAMIC ARRAY OF RECORD -- registro de despliegue del agrupador
         nss              CHAR(11),
         rfc              CHAR(13),
         doc_fico         CHAR(10),
         ejercicio        CHAR(4),
         fch_contable     DATE,
         fch_liquida      DATE,
         desc_tipo_sol    CHAR(25),
         folio            DECIMAL(9,0),
         subcta_47        SMALLINT,
         pesos_tesofe     DECIMAL(18,2),
         subcta_4         SMALLINT,
         acciones_viv97   DECIMAL(18,6),
         subcta_8         SMALLINT,
         acciones_viv92   DECIMAL(18,6),
         estado_solicitud SMALLINT
      END RECORD
DEFINE p_query             STRING 
DEFINE p_detalle_exp      RECORD
         nss              CHAR(11),
         rfc              CHAR(13),
         doc_fico         CHAR(10),
         ejercicio        CHAR(4),
         fch_contable     DATE,
         fch_liquida      DATE,
         desc_tipo_sol    CHAR(25),
         folio            DECIMAL(9,0),
         subcta_47        SMALLINT,
         pesos_tesofe     DECIMAL(18,2),
         subcta_4         SMALLINT,
         acciones_viv97   DECIMAL(18,6),
         subcta_8         SMALLINT,
         acciones_viv92   DECIMAL(18,6),
         estado_solicitud SMALLINT      END RECORD
   DEFINE p_exporta_opcion       CHAR(15)
   DEFINE
      v_c_ruta_env            LIKE seg_modulo.ruta_envio,
      v_extension_txt         STRING,
      v_nom_archivo           STRING,
      v_archivo_txt           STRING,
      v_v_ruta_nomarch        STRING,
      v_v_ruta_nomarch2       STRING,
      v_mensaje_archivo       STRING,
      v_s_detalle             STRING,
      v_ch_arch_ret_generico  BASE.CHANNEL,  -- manejador de apuntador hacia archivo
      v_cuenta                INTEGER,
      v_solicitud             DECIMAL(9,0),
      v_tipo                  CHAR(1),
      v_hora                  CHAR(8),
      v_regresa               SMALLINT,
      v_contador              INTEGER,
      v_query                 STRING,
      v_nss_paso              CHAR(11),
      v_suma_aivs_97          DECIMAL(18,6),
      v_i                     INTEGER,
      v_indice                INTEGER 

   LET v_regresa = FALSE
   LET v_i       = 0
   -- se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio
   INTO   v_c_ruta_env 
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

--   DISPLAY "el folio a procesar es >", p_folio, "<"
   
   -- las extensiones del archivo son csv para el detalle
   LET v_extension_txt = ".csv"
   LET v_hora = CURRENT HOUR TO SECOND

   -- Se genera el nombre del archivo de Rechazos
   LET v_nom_archivo = "Detalle_Cargos_SSV_", TODAY USING "yyyymmdd"
   LET v_archivo_txt = v_nom_archivo CLIPPED, v_extension_txt

   -- El archivo con ruta destino que contiene el detalle 
   LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se muestra mensaje
   LET v_mensaje_archivo = "Se generará el archivo:\n\n\t", v_v_ruta_nomarch
   
   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
   -- nombre de archivo generado
   
   -- se obtienen los registros del detalle
   PREPARE sid_query_exporta FROM p_query
   DECLARE cur_query_exporta CURSOR FOR sid_query_exporta
   
   LET v_indice = 0
   LET v_nss_paso = NULL
   -- se transfieren los datos al arreglo de despliegue agrupador
   FOREACH cur_query_exporta INTO p_detalle_exp.*
      IF v_nss_paso = p_detalle_exp.nss THEN 
         LET p_r_grupo[v_indice].acciones_viv97 = p_r_grupo[v_indice].acciones_viv97 + p_detalle_exp.acciones_viv97
      ELSE
         LET v_nss_paso =  p_detalle_exp.nss
         LET v_indice = v_indice + 1
         LET p_r_grupo[v_indice].* = p_detalle_exp.*
      END IF
   END FOREACH

   
   -- Se crea el manejador de archivo
   LET v_ch_arch_ret_generico = base.Channel.create()
   CALL v_ch_arch_ret_generico.setDelimiter(NULL)

   -- Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )
   -- Escribe el encabezado del archivo de Rechazados

   LET v_s_detalle = "NSS|RFC|DOC FICO|EJERCICIO|FECHA CONTABLE|FECHA LIQUIDACION|TIPO SOLICITUD|FOLIO|PESOS TESOFE|AIVS VIV 97|AIVS VIV 92"

   CALL v_ch_arch_ret_generico.write(v_s_detalle)

   DISPLAY "La consulta de rechazados es >", p_query, "<"
   -- se llena el arreglo 
--   PREPARE s_rch_query FROM p_query
--   DECLARE cur_rch_query CURSOR FOR s_rch_query

   -- se mandan al archivo los registros rechazados
   LET v_nss_paso = NULL
   FOR v_i = 1 TO p_r_grupo.getLength()
      LET v_s_detalle = p_r_grupo[v_i].nss USING "&&&&&&&&&&&","|",
                        p_r_grupo[v_i].rfc,"|",
                        p_r_grupo[v_i].doc_fico,"|",
                        p_r_grupo[v_i].ejercicio,"|",
                        p_r_grupo[v_i].fch_contable USING "dd-mm-yyyy","|",
                        p_r_grupo[v_i].fch_liquida USING "dd-mm-yyyy","|",
                        p_r_grupo[v_i].desc_tipo_sol, "|",
                        p_r_grupo[v_i].folio,"|",
                        p_r_grupo[v_i].pesos_tesofe USING "################&.&&","|",
                        p_r_grupo[v_i].acciones_viv97 USING "################&.&&" ,"|",
                        p_r_grupo[v_i].acciones_viv92 USING "################&.&&"
      CALL v_ch_arch_ret_generico.write(v_s_detalle)
   END FOR 
                           
   -- Se cierra el archivo de Rechazados
   CALL v_ch_arch_ret_generico.close()

   LET v_mensaje_archivo = "El archivo fue generado exitosamente:\n\n\t", v_v_ruta_nomarch

   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")

   RETURN v_regresa
   
END FUNCTION
