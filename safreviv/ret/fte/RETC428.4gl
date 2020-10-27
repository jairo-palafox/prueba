--===============================================================
-- Version: 1.0.0
--===============================================================

#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETC428                                                                #
#Objetivo     => Consulta Cargos a la SSV via SIAFF                                     #
#Fecha inicio => Noviembre 05, 2016                                                    # 
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
      p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario
      v_folio          DECIMAL(9,0), -- folio
      v_archivo        LIKE glo_ctr_archivo.nombre_archivo,
      v_contador       INTEGER,
      v_cbx_folios     ui.ComboBox, -- combo de folios
      cmb_folios       SMALLINT,
      v_resultado      SMALLINT,
       
      v_r_grupo   DYNAMIC ARRAY OF RECORD -- registro de despliegue del agrupador
         grupo            LIKE glo_folio.folio,
         aivs             DECIMAL(18,6),
         pesos            DECIMAL(14,2)
      END RECORD,

      v_r_grupo_indiv   RECORD -- registro de despliegue del agrupador
         grupo            LIKE glo_folio.folio,
         aivs             DECIMAL(18,6),
         pesos            DECIMAL(14,2)
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
       v_indice                      SMALLINT, -- indice de arreglo
       v_precio_fondo                LIKE ret_cza_transferencia.precio_fondo

       
   OPEN WINDOW w_consulta_saldos WITH FORM "RETC4281"
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


   INPUT v_folio
      FROM cmb_folios
      ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)

      BEFORE INPUT 
         -- se limpian las variables
         LET v_folio = 0
         CALL v_r_grupo.clear() 
         CALL f_limpia_arreglo(v_arr_reg_detalle.*) RETURNING v_arr_reg_detalle.*

      ON ACTION ACCEPT

--        DISPLAY "variables capturadas"
--        DISPLAY "@..", v_folio, v_afore, v_nss, v_f_inicial, v_f_final

         -- se borran los arreglos de despliegue
         CALL v_r_grupo.clear()
         CALL f_limpia_arreglo(v_arr_reg_detalle.*) RETURNING v_arr_reg_detalle.*

         --modificación de validación de  captura de parametros
         --valida que se ingrese al menos un parametro
         IF ( (v_folio IS NULL OR v_folio = 0) ) THEN
            CALL fn_mensaje("Consulta","Debe de ingresar el Folio a consultar","about")
            CONTINUE INPUT
         ELSE 
            -- Se obtiene el total de registros del folio 
            SELECT COUNT(*), b.f_actualiza
            INTO   v_arr_reg_detalle.ed_tot_reg, v_arr_reg_detalle.ed_fch_carga
            FROM   ret_cargos_ssv_siaff a, glo_ctr_archivo b
            WHERE  a.folio = b.folio
            AND    a.folio = v_folio
            GROUP BY b.f_actualiza;
            
            -- Se obtiene la suma de los pagos realizados en pesos y en aivs
            SELECT SUM(monto_pesos) * (-1), SUM(monto_acciones)  * (-1)
            INTO   v_arr_reg_detalle.ed_pesos_debita, v_arr_reg_detalle.ed_aivs_debita
            FROM   ret_preliquida
            WHERE  folio_liquida = v_folio;

            -- Se obtiene la cantidad de registros con pago
            SELECT COUNT(DISTINCT id_referencia)
            INTO   v_arr_reg_detalle.ed_total_pagos
            FROM   ret_preliquida
            WHERE  folio_liquida = v_folio;

            -- Se obtienen los montos en AIV's y en pesos debitados para la subcuenta de Vivienda 97 
            SELECT SUM(monto_acciones) * (-1), SUM(monto_pesos) * (-1) 
            INTO   v_arr_reg_detalle.ed_aivs_viv_97, v_arr_reg_detalle.ed_pesos_viv_97
            FROM   ret_preliquida
            WHERE  folio_liquida = v_folio
            AND    subcuenta = 4;

            -- Se obtienen los montos en AIV's y en pesos debitados para la subcuenta de Vivienda 92 
            SELECT SUM(monto_acciones) * (-1), SUM(monto_pesos) * (-1) 
            INTO   v_arr_reg_detalle.ed_aivs_viv_92, v_arr_reg_detalle.ed_pesos_viv_92
            FROM   ret_preliquida
            WHERE  folio_liquida = v_folio
            AND    subcuenta = 8;

            --Se obtiene la cantidad de registros no encontrados 
            SELECT COUNT(*)
            INTO v_arr_reg_detalle.ed_nss_no_loc
            FROM   ret_cargos_ssv_siaff
            WHERE  folio = v_folio
            AND    estado_solicitud = 100
            AND    cod_rechazo = 1;

            --Se obtiene la cantidad de registros no encontrados 
            SELECT COUNT(*)
            INTO v_arr_reg_detalle.ed_nss_no_loc
            FROM   ret_cargos_ssv_siaff
            WHERE  folio = v_folio
            AND    estado_solicitud = 100
            AND    cod_rechazo = 1;

            SELECT SUM(importe), SUM(importe/precio_fondo)
            INTO   v_arr_reg_detalle.ed_pesos_nss_no_loc, v_arr_reg_detalle.ed_aivs_nss_no_loc
            FROM   ret_cargos_ssv_siaff a, glo_valor_fondo b
            WHERE  MDY(MONTH(a.fch_contable), 1, YEAR(a.fch_contable)) = b.f_valuacion
            AND    b.fondo = 11
            AND    a.folio = v_folio
            AND    a.estado_solicitud = 100
            AND    a.cod_rechazo = 1;
            
            -- query para obtener los totales de pesos y aivs por grupo


            LET v_query = "\n SELECT a.grupo, SUM(monto_acciones) * (-1), SUM(monto_pesos) * (-1) ",
                          "\n FROM   ret_cargos_ssv_siaff a, ret_preliquida b                     ",
                          "\n WHERE  a.id_solicitud = b.id_referencia                             ",
                          "\n AND    a.folio = ", v_folio,
                          "\n AND    a.folio = b.folio_liquida                                    ",
                          "\n GROUP BY a.grupo                                                    "

            -- consulta
            DISPLAY "Consulta:\n", v_query

            -- se llena el arreglo 
            PREPARE sid_query FROM v_query
            DECLARE cur_query CURSOR FOR sid_query
            
            LET v_indice = 1
            
            -- se transfieren los datos al arreglo de despliegue agrupador
            FOREACH cur_query INTO v_r_grupo_indiv.*
               LET v_r_grupo[v_indice].* = v_r_grupo_indiv.*
               -- se incrementa el indice
               LET v_indice = v_indice + 1
            END FOREACH
            
               DISPLAY BY NAME v_arr_reg_detalle.*
               DISPLAY ARRAY v_r_grupo TO tbl_detalle.*
               ON ACTION Exporta
                  CALL fn_exporta_archivo(v_folio) RETURNING v_resultado;
               ON ACTION CANCEL 
                  EXIT DISPLAY 
               END DISPLAY 
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
FUNCTION fn_exporta_archivo(p_folio)
DEFINE p_folio             DECIMAL(9,0)
DEFINE p_detalle_exp      RECORD
         id_solicitud        DECIMAL(9,0),
         folio               DECIMAL(9,0),
         estado_solicitud    SMALLINT,
         desc_estado         CHAR(40),
         cod_rechazo         SMALLINT,
         desc_codigo         CHAR(40),
         nss                 CHAR(11),
         rfc                 CHAR(13),
         compensa            CHAR(8),
         doc_comp            CHAR(10),
         ejercicio           CHAR(4),
         no_documento        CHAR(10),
         fch_contable        DATE,
         fch_doc             DATE,
         referencia          CHAR(16),
         clase_doc           CHAR(2),
         periodo             CHAR(2),
         dh                  CHAR(2),
         importe             DECIMAL(14,2),
         cta_mayor           CHAR(10),
         vp                  CHAR(1),
         titular_cuenta      CHAR(60),
         grupo               SMALLINT,
         via_pago            CHAR(8),
         delegacion          CHAR(24),
         clabe               CHAR(18),
         desc_tipo_sol       CHAR(40)
      END RECORD
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
      v_query                 STRING

   LET v_regresa = FALSE
   
   -- se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio
   INTO   v_c_ruta_env 
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   DISPLAY "el folio a procesar es >", p_folio, "<"
   
   -- las extensiones del archivo son csv para el detalle
   LET v_extension_txt = ".csv"
   LET v_hora = CURRENT HOUR TO SECOND

   -- Se genera el nombre del archivo de Rechazos
   LET v_nom_archivo = "Cargos_SSV_Rechazos_", TODAY USING "yyyymmdd"
   LET v_archivo_txt = v_nom_archivo CLIPPED, v_extension_txt

   -- El archivo con ruta destino que contiene el detalle de Rechazados
   LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se genera el nombre del archivo de Aceptados
   LET v_nom_archivo = "Cargos_SSV_Aceptados_", TODAY USING "yyyymmdd"
   LET v_archivo_txt = v_nom_archivo CLIPPED, v_extension_txt

   -- El archivo con ruta destino que contiene el detalle de Aceptados
   LET v_v_ruta_nomarch2 = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se muestra mensaje
   LET v_mensaje_archivo = "Se generaran los siguientes archivos:\n\n\t", v_v_ruta_nomarch, "\n\t", v_v_ruta_nomarch2
   
   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
   -- nombre de archivo generado

   -- Se crea el manejador de archivo
   LET v_ch_arch_ret_generico = base.Channel.create()
   CALL v_ch_arch_ret_generico.setDelimiter(NULL)

   -- Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )
   -- Escribe el encabezado del archivo de Rechazados
   LET v_s_detalle = "ID_SOLICITUD|FOLIO|CVE_ESTADO|DESC_ESTADO|COD_RECHAZO|DESC_COD_RECHAZO|NSS|RFC|COMPENSA|DOC_COMP|EJERCICIO|NO_DOCUMENTO|",
                     "FCH_CONTABLE|FCH_DOC|REFERENCIA|CLASE_DOC|PERIODO|DH|IMPORTE|CTA_MAYOR|VP|TITULAR_CUENTA|GRUPO|",
                     "VIA_PAGO|DELEGACION|CLABE|TIPO_PAGO"

   CALL v_ch_arch_ret_generico.write(v_s_detalle)

   -- Ahora imprimo todos los registros Rechazados
   LET v_query = " SELECT                       ",
                 "        a.id_solicitud,       ",
                 "        a.folio,              ",
                 "        a.estado_solicitud,   ",
                 "        b.des_larga,          ",
                 "        a.cod_rechazo,        ",
                 "        c.des_larga,          ",
                 "        a.nss,                ",
                 "        a.rfc,                ",
                 "        a.compensa,           ",
                 "        a.doc_comp,           ",
                 "        a.ejercicio,          ",
                 "        a.no_documento,       ",
                 "        a.fch_contable,       ",
                 "        a.fch_doc,            ",
                 "        a.referencia,         ",
                 "        a.clase_doc,          ",
                 "        a.periodo,            ",
                 "        a.dh,                 ",
                 "        a.importe,            ",
                 "        a.cta_mayor,          ",
                 "        a.vp,                 ",
                 "        a.titular_cuenta,     ",
                 "        a.grupo,              ",
                 "        a.via_pago,           ",
                 "        a.delegacion,         ",
                 "        a.clabe,              ",
                 "        d.desc_tipo_sol       ",
                 "   FROM ret_cargos_ssv_siaff a                                         ",
                 "        LEFT OUTER JOIN ret_estado_solicitud b                         ",
                 "                     ON a.estado_solicitud = b.estado_solicitud        ",
                 "        LEFT OUTER JOIN ret_rechazo c                                  ",
                 "                     ON a.cod_rechazo = c.cod_rechazo                  ",
                 "        LEFT OUTER JOIN ret_cat_tipo_sol_ssv d                         ",
                 "                     ON a.tipo_sol = d.tipo_sol                        ",
                 "  WHERE a.folio =  ", p_folio,
                 "    AND a.estado_solicitud = 100                                       "
   DISPLAY "La consulta de rechazados es >", v_query, "<"
   -- se llena el arreglo 
   PREPARE s_rch_query FROM v_query
   DECLARE cur_rch_query CURSOR FOR s_rch_query

   -- se mandan al archivo los registros rechazados
   FOREACH cur_rch_query INTO p_detalle_exp.*
                 
         LET v_s_detalle = p_detalle_exp.id_solicitud,"|",
                           p_detalle_exp.folio,"|",
                           p_detalle_exp.estado_solicitud,"|",
                           p_detalle_exp.desc_estado CLIPPED ,"|",
                           p_detalle_exp.cod_rechazo,"|",
                           p_detalle_exp.desc_codigo CLIPPED ,"|",
                           p_detalle_exp.nss USING "&&&&&&&&&&&","|",
                           p_detalle_exp.rfc CLIPPED ,"|",
                           p_detalle_exp.compensa,"|",
                           p_detalle_exp.doc_comp,"|",
                           p_detalle_exp.ejercicio,"|",
                           p_detalle_exp.no_documento,"|",
                           p_detalle_exp.fch_contable USING "dd-mm-yyyy","|",
                           p_detalle_exp.fch_doc USING "dd-mm-yyyy","|",
                           p_detalle_exp.referencia, "|",
                           p_detalle_exp.clase_doc,"|",
                           p_detalle_exp.periodo,"|",
                           p_detalle_exp.dh,"|",
                           p_detalle_exp.importe USING "##########.##", "|",
                           p_detalle_exp.cta_mayor,"|",
                           p_detalle_exp.vp,"|",
                           p_detalle_exp.titular_cuenta CLIPPED,"|",
--                           p_detalle_exp.grupo,"|",
                           p_detalle_exp.via_pago,"|",
                           p_detalle_exp.delegacion,"|",
                           p_detalle_exp.clabe,"|",
                           p_detalle_exp.desc_tipo_sol
                           
         CALL v_ch_arch_ret_generico.write(v_s_detalle)
         --LET v_contador = v_contador + 1
   END FOREACH

   -- Se cierra el archivo de Rechazados
   CALL v_ch_arch_ret_generico.close()

   -- Se crea el manejador de archivo
   LET v_ch_arch_ret_generico = base.Channel.create()
   CALL v_ch_arch_ret_generico.setDelimiter(NULL)

   -- Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch2, "w" )
   -- Escribe el encabezado del archivo de Aceptados
   LET v_s_detalle = "ID_SOLICITUD|FOLIO|CVE_ESTADO|DESC_ESTADO|COD_RECHAZO|DESC_COD_RECHAZO|NSS|RFC|COMPENSA|DOC_COMP|EJERCICIO|NO_DOCUMENTO|",
                     "FCH_CONTABLE|FCH_DOC|REFERENCIA|CLASE_DOC|PERIODO|DH|IMPORTE|CTA_MAYOR|VP|TITULAR_CUENTA|GRUPO|",
                     "VIA_PAGO|DELEGACION|CLABE|TIPO_SOL"

   CALL v_ch_arch_ret_generico.write(v_s_detalle)

   -- Ahora imprimo todos los registros Aceptados
   LET v_query = " SELECT                       ",
                 "        a.id_solicitud,       ",
                 "        a.folio,              ",
                 "        a.estado_solicitud,   ",
                 "        b.des_larga,          ",
                 "        a.cod_rechazo,        ",
                 "        c.des_larga,          ",
                 "        a.nss,                ",
                 "        a.rfc,                ",
                 "        a.compensa,           ",
                 "        a.doc_comp,           ",
                 "        a.ejercicio,          ",
                 "        a.no_documento,       ",
                 "        a.fch_contable,       ",
                 "        a.fch_doc,            ",
                 "        a.referencia,         ",
                 "        a.clase_doc,          ",
                 "        a.periodo,            ",
                 "        a.dh,                 ",
                 "        a.importe,            ",
                 "        a.cta_mayor,          ",
                 "        a.vp,                 ",
                 "        a.titular_cuenta,     ",
                 "        a.grupo,              ",
                 "        a.via_pago,           ",
                 "        a.delegacion,         ",
                 "        a.clabe,              ",
                 "        d.desc_tipo_sol       ",
                 "   FROM ret_cargos_ssv_siaff a                                         ",
                 "        LEFT OUTER JOIN ret_estado_solicitud b                         ",
                 "                     ON a.estado_solicitud = b.estado_solicitud        ",
                 "        LEFT OUTER JOIN ret_rechazo c                                  ",
                 "                     ON a.cod_rechazo = c.cod_rechazo                  ",
                 "        LEFT OUTER JOIN ret_cat_tipo_sol_ssv d                         ",
                 "                     ON a.tipo_sol = d.tipo_sol                        ",
                 "  WHERE a.folio =  ", p_folio,
                 "    AND a.estado_solicitud <> 100                                      "

   DISPLAY "el query de aceptados >", v_query, "<"
   -- se llena el arreglo 
   PREPARE s_acep_query FROM v_query
   DECLARE cur_acep_query CURSOR FOR s_acep_query

   -- se mandan al archivo los registros aceptados
   FOREACH cur_acep_query INTO p_detalle_exp.*
                 
         LET v_s_detalle = p_detalle_exp.id_solicitud,"|",
                           p_detalle_exp.folio,"|",
                           p_detalle_exp.estado_solicitud,"|",
                           p_detalle_exp.desc_estado CLIPPED ,"|",
                           p_detalle_exp.cod_rechazo,"|",
                           p_detalle_exp.desc_codigo CLIPPED ,"|",
                           p_detalle_exp.nss USING "&&&&&&&&&&&","|",
                           p_detalle_exp.rfc CLIPPED ,"|",
                           p_detalle_exp.compensa,"|",
                           p_detalle_exp.doc_comp,"|",
                           p_detalle_exp.ejercicio,"|",
                           p_detalle_exp.no_documento,"|",
                           p_detalle_exp.fch_contable USING "dd-mm-yyyy","|",
                           p_detalle_exp.fch_doc USING "dd-mm-yyyy","|",
                           p_detalle_exp.referencia, "|",
                           p_detalle_exp.clase_doc,"|",
                           p_detalle_exp.periodo,"|",
                           p_detalle_exp.dh,"|",
                           p_detalle_exp.importe USING "###########.##","|",
                           p_detalle_exp.cta_mayor,"|",
                           p_detalle_exp.vp,"|",
                           p_detalle_exp.titular_cuenta CLIPPED,"|",
                           p_detalle_exp.grupo,"|",
                           p_detalle_exp.via_pago,"|",
                           p_detalle_exp.delegacion,"|",
                           p_detalle_exp.clabe,"|",
                           p_detalle_exp.desc_tipo_sol
                           
         CALL v_ch_arch_ret_generico.write(v_s_detalle)
         --LET v_contador = v_contador + 1
   END FOREACH

   -- Se cierra el archivo de Aceptados
   CALL v_ch_arch_ret_generico.close()

   LET v_mensaje_archivo = "Los archivos fueron generados exitosamente:\n\n\t", v_v_ruta_nomarch, "\n\t", v_v_ruta_nomarch2

   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")

   RETURN v_regresa
   
END FUNCTION
