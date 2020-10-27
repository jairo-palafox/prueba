--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10 Octubre, 2012
--===============================================================

#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETC422                                                                #
#Objetivo     => Consulta anexo 1 NSS                                                   #
#Fecha inicio => Septiembre 19, 2016                                                       # 
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
   CALL STARTLOG (p_usuario_cod CLIPPED||".RETC422.log")
   
   -- consulta de informacion recibida 
   CALL fn_consulta_detalle(p_usuario_cod)

END MAIN

{ ============================================================================
Clave: RETC422
Nombre: fn_consulta_detalle
Fecha creacion: Septiembre 19, 2016
Registro de modificaciones:
Descrip: Consulta NSS Anexo 1
==============================================================================
}
FUNCTION fn_consulta_detalle(p_usuario_cod)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_folio          DECIMAL(9,0), -- folio
       v_afore          INTEGER,
       v_nss            CHAR(11), 
       v_f_inicial      LIKE ret_cza_transferencia.f_carga,
       v_f_final        LIKE ret_cza_transferencia.f_carga,
       v_cbx_folios     ui.ComboBox, -- combo de folios
       v_s_cadena       STRING, -- cadena de texto
       v_r_glo_folio    RECORD LIKE glo_folio.*,
       v_r_cat_afore    RECORD 
         v_afore_cod      LIKE cat_afore.afore_cod,
         v_afore_desc     LIKE cat_afore.afore_desc
        END RECORD,
       v_r_agrupador   RECORD -- registro de despliegue del agrupador
        folio            LIKE glo_folio.folio,
        f_carga          DATE
       END RECORD,
       v_arr_agrupador  DYNAMIC ARRAY OF RECORD -- arreglo de despliegue del agrupador
        folio            LIKE glo_folio.folio,
        f_carga          DATE
       END RECORD,
       v_arr_reg_detalle         RECORD
         ed_nss                   LIKE ret_his_anexo1.nss                  ,
         ed_nombre                CHAR(50)                                 ,
         ed_edo_transferencia     CHAR(40)                                 ,
         ed_ssv_transferido       LIKE ret_his_anexo1.pesos_viv97_transf   , 
         ed_ssv_sin_transferir    LIKE ret_his_anexo1.pesos_viv97_no_transf,
         ed_fch_transferencia     LIKE ret_his_anexo1.f_transferencia      ,
         ed_tpo_tra_pensionado    CHAR(40)                                 ,
         ed_estatus_demanda       CHAR(40)                                 ,
         ed_num_juicio            LIKE ret_his_anexo1.num_juicio           ,
         ed_anio_juicio           LIKE ret_his_anexo1.ano_juicio           ,
         ed_juzgado               LIKE ret_his_anexo1.juzgado              ,
         ed_num_expediente        LIKE ret_his_anexo1.num_expediente       ,
         ed_fch_liquida           LIKE cta_movimiento.f_liquida            ,
         ed_folio_liquida         LIKE cta_movimiento.folio_liquida        ,
         ed_nom_archivo           LIKE glo_ctr_archivo.nombre_archivo      ,
         ed_duplicado             CHAR(20)
       END RECORD,
       arr_reg_detalle         DYNAMIC ARRAY OF RECORD
         ed_nss                   LIKE ret_his_anexo1.nss                  ,
         ed_nombre                CHAR(50)                                 ,
         ed_edo_transferencia     CHAR(40)                                 ,
         ed_ssv_transferido       LIKE ret_his_anexo1.pesos_viv97_transf   , 
         ed_ssv_sin_transferir    LIKE ret_his_anexo1.pesos_viv97_no_transf,
         ed_fch_transferencia     LIKE ret_his_anexo1.f_transferencia      ,
         ed_tpo_tra_pensionado    CHAR(40)                                 ,
         ed_estatus_demanda       CHAR(40)                                 ,
         ed_num_juicio            LIKE ret_his_anexo1.num_juicio           ,
         ed_anio_juicio           LIKE ret_his_anexo1.ano_juicio           ,
         ed_juzgado               LIKE ret_his_anexo1.juzgado              ,
         ed_num_expediente        LIKE ret_his_anexo1.num_expediente       ,
         ed_fch_liquida           LIKE cta_movimiento.f_liquida            ,
         ed_folio_liquida         LIKE cta_movimiento.folio_liquida        ,
         ed_nom_archivo           LIKE glo_ctr_archivo.nombre_archivo      ,
         ed_duplicado             CHAR(20)
       END RECORD,
       arr_reporte              DYNAMIC ARRAY OF RECORD
         v_folio                  LIKE glo_folio.folio,
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
       v_query                       STRING, -- detalle
       v_query_agrupador             STRING, -- agrupador por folio y afore
       v_id_derechohabiente          LIKE afi_derechohabiente.id_derechohabiente,
       v_indice                      SMALLINT, -- indice de arreglo
       v_afore_cod                   LIKE cat_afore.afore_cod,
       v_afore_desc                  LIKE cat_afore.afore_desc, 
       v_nombre_trab                 LIKE ret_transferencia.nombre_afore,
       v_paterno_trab                LIKE ret_transferencia.paterno_afore,
       v_materno_trab                LIKE ret_transferencia.materno_afore,
       v_precio_fondo                LIKE ret_cza_transferencia.precio_fondo,
       v_ruta_reporte                STRING ,-- ruta del archivo del reporte       
       v_ruta_listados               STRING ,-- ruta de los listados
       v_ruta_ejecutable             STRING ,-- ruta del ejecutable
       manejador_rpt                 om.SaxDocumentHandler ,
       v_indice_reporte              SMALLINT,
       v_id_solicitud                LIKE ret_disposicion.id_solicitud,
       v_agrupador_folio_fecha_Afore STRING

       
   OPEN WINDOW w_consulta_saldos WITH FORM "RETC4221"
   LET  v_ventana = UI.WINDOW.GETCURRENT()
   CALL v_ventana.SETTEXT("Consulta Detalle Anexo 1")



   INPUT v_nss
      FROM e_nss
      ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)

      BEFORE INPUT 
         -- se limpian las variables
         LET v_nss       = NULL    
         
      ON ACTION ACCEPT

--        DISPLAY "variables capturadas"
--        DISPLAY "@..", v_folio, v_afore, v_nss, v_f_inicial, v_f_final

         -- se borran los arreglos de despliegue
         CALL v_arr_agrupador.clear()
         CALL arr_reg_detalle.clear()
         CALL arr_reporte.clear()

         --modificación de validación de  captura de parametros
         --valida que se ingrese al menos un parametro
         IF ( (v_nss IS NULL OR v_nss <= 0) ) THEN
            CALL fn_mensaje("Consulta","Debe de ingresar el NSS a consultar","about")
            CONTINUE INPUT
         ELSE 
            -- se verifica si se recibio NSS
            IF ( v_nss IS NOT NULL ) THEN
               -- se obtiene el id_derechohabiente para realizar la búsqueda
               SELECT id_derechohabiente
                 INTO v_id_derechohabiente
                 FROM afi_derechohabiente
                WHERE nss = v_nss
            
               IF ( v_id_derechohabiente IS NULL ) THEN 
                  CALL fn_mensaje("Atención", "El NSS proporcionado no existe en la base de datos", "stop")
                  CONTINUE INPUT 
               END IF 
            END IF 
             
            -- query para obtener los grupos
            LET v_query = "\n SELECT a.nss, TRIM(a.nombre_af) || ' ' || TRIM(a.ap_paterno_af) || ' ' || TRIM(a.ap_materno_af), ",
                          "\n        a.estado_transferencia || '-' || e.descripcion, a.pesos_viv97_transf,                     ",
                          "\n        a.pesos_viv97_no_transf, a.f_transferencia,                                               ",
                          "\n        a.tpo_pensionado || '-' || f.descripcion, a.estado_demanda_amparo || '-' || g.descripcion,",
                          "\n        a.num_juicio, a.ano_juicio, a.juzgado,                                                    ",
                          "\n        a.num_expediente, b.f_liquida, b.folio_liquida, c.nombre_archivo,                         ", 
                          "\n        case when ( d.nss is  null )  then  'SIN DUPLICIDAD' else 'DUPLICADO' end                 ", 
                          "\n   FROM ret_his_anexo1 a                                                                          ",                          
                          "\n        LEFT OUTER JOIN cta_movimiento b                                                          ",
                          "\n                     ON a.folio = b.folio_liquida                                                 ",
                          "\n                    AND a.id_solicitud = b.id_referencia                                          ",
                          "\n        LEFT OUTER JOIN glo_ctr_archivo c                                                         ",
                          "\n                     ON a.folio = c.folio                                                         ",
                          "\n        LEFT OUTER JOIN ret_his_anexo1_contracargos d                                             ",
                          "\n                     ON a.nss = d.nss                                                             ",
                          "\n        LEFT OUTER JOIN ret_cat_estado_transferencia e                                            ",
                          "\n                     ON a.estado_transferencia = e.estado_transferencia                           ",
                          "\n        LEFT OUTER JOIN ret_cat_trab_pensionado f                                                 ",
                          "\n                     ON a.tpo_pensionado = f.tpo_pensionado                                       ",
                          "\n        LEFT OUTER JOIN ret_cat_demanda_amparo g                                                  ",
                          "\n                     ON a.estado_demanda_amparo = g.estado_demanda_amparo                         ",
                          "\n  WHERE a.nss = '", v_nss , "'"

            -- consulta
            DISPLAY "Consulta:\n", v_query

            -- se llena el arreglo 
            PREPARE sid_query FROM v_query
            DECLARE cur_query CURSOR FOR sid_query
            
            LET v_indice = 1
            
            -- se transfieren los datos al arreglo de despliegue agrupador
            FOREACH cur_query INTO v_arr_reg_detalle.*
               LET arr_reg_detalle[v_indice].* = v_arr_reg_detalle.*
               
               -- se incrementa el indice
               LET v_indice = v_indice + 1
            END FOREACH
            
            -- se cuentan cuantos registros hay
            IF ( arr_reg_detalle.getLength() < 1 ) THEN
               CALL fn_mensaje("Atención","No se encontraron datos con los parámetros dados","exclamation")
               CONTINUE INPUT
            END IF
            
            
            -- se abre la ventana de resultados
            OPEN WINDOW w_detalle WITH FORM "RETC4222"
            
            -- se abre un dialog para realizar el despliegue de los resultados
            DIALOG
            ATTRIBUTES ( UNBUFFERED )
            
               DISPLAY ARRAY arr_reg_detalle TO tbl_detalle.*
               ON ACTION regresar
                  EXIT DIALOG
               END DISPLAY 
            END DIALOG 
            CLOSE WINDOW w_detalle
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
