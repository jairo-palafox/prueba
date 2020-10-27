--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 14 Diciembre, 2015
--===============================================================

#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETC13                                                                 #
#Objetivo     => Consulta sdos insuficientes en solicitudes de retiro por Disposicion   #
#Fecha inicio => Octubre 10, 2012                                                       # 
#########################################################################################

DATABASE safre_viv

GLOBALS "RETG01.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
	DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
          ,v_ventana                ui.WINDOW
          ,v_forma                  ui.form 
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
   
   -- consulta de informacion recibida 
   CALL fn_consulta_registros(p_usuario_cod)

END MAIN

{ ============================================================================
Clave: RETC423
Nombre: fn_consulta_registros
Fecha creacion: Septiembre 30, 2016
Registro de modificaciones:
Descrip: CONSULTA REGISTROS ACEPTADOS Y RECHAZADOS DE LAS MODALIDADES DE 
         DISPOSICION, PMG Y TRANSFERENCIA

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
==============================================================================
}
FUNCTION fn_consulta_registros(p_usuario_cod)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_nss            CHAR(11), -- NSS
       v_modalidad      INTEGER,
       v_tpo_retiro     CHAR(2),
       v_radio          CHAR(15),
       v_radio_exporta  CHAR(15),
       v_f_inicial      LIKE ret_cza_transferencia.f_carga,
       v_f_final        LIKE ret_cza_transferencia.f_carga,
       v_cbx_modalidad  ui.ComboBox, -- combo de Modalidad
       v_cbx_tpo_retiro ui.ComboBox, -- combo de Tipos de Retiro
       v_s_cadena       STRING, -- cadena de texto
       v_c_ruta_env     LIKE seg_modulo.ruta_envio,
       v_extension_txt  STRING, -- extension del archivo de salida
       v_nom_archivo    STRING, -- nombre del archivo de salida
       v_archivo_txt    STRING, -- nombre y extension del archivo con el detalle
       v_v_ruta_nomarch STRING, -- ruta y nombre del archivo de salida
       v_mensaje_archivo STRING, -- mensaje de generacion de archivo
       v_ch_arch_ret_generico BASE.CHANNEL,  -- manejador de apuntador hacia archivo
       v_conteo         INTEGER, -- contador de registros
       v_s_detalle      STRING,
       v_contador_rechazados       INTEGER,
       v_contador_aceptados        INTEGER, 
       v_r_modalidad    RECORD 
          modalidad_retiro    LIKE ret_modalidad_retiro.modalidad_retiro,
          des_larga           LIKE ret_modalidad_retiro.des_larga
       END RECORD ,
       v_r_tpo_retiro    RECORD 
         v_tpo_retiro      LIKE ret_tipo_retiro.tpo_retiro,
         v_des_larga       LIKE ret_tipo_retiro.des_larga
        END RECORD,
       v_r_agrupador   RECORD -- registro de despliegue del agrupador
        folio            LIKE glo_folio.folio,
        f_carga          DATE,
        afore_cod        LIKE cat_afore.afore_cod,
        afore_desc       LIKE cat_afore.afore_desc
       END RECORD,
       v_arr_agrupador  DYNAMIC ARRAY OF RECORD -- arreglo de despliegue del agrupador
        folio            LIKE glo_folio.folio,
        f_carga          DATE,
        afore_cod        LIKE cat_afore.afore_cod,
        afore_desc       LIKE cat_afore.afore_desc
       END RECORD,
       arr_reg_sds               RECORD
         folio                   LIKE ret_disposicion.folio,
         id_solicitud            LIKE ret_disposicion.id_solicitud,
         f_carga                 LIKE ret_cza_transferencia.f_carga,
         afore_cod               LIKE cat_afore.afore_cod,
         afore_desc              LIKE cat_afore.afore_desc,
         nss                     CHAR(11),
         nombre_trab             LIKE ret_transferencia.nombre_afore,
         paterno_trab            LIKE ret_transferencia.paterno_afore,
         materno_trab            LIKE ret_transferencia.materno_afore,
         aivs92_sol              DECIMAL(24,6),
         aivs97_sol              LIKE ret_transferencia.aivs_viv97,   
         precio_fondo            LIKE ret_cza_transferencia.precio_fondo,
         aivs92_sdo              DECIMAL(24,6),
         aivs92_sdo_pesos        DECIMAL(24,6),
         aivs97_sdo              DECIMAL(24,6),
         aivs97_sdo_pesos        DECIMAL(24,6),
         aivsvol_sdo             DECIMAL(24,6),
         aivsvol_sdo_pesos       DECIMAL(24,6),
         aivssum97vol_sdo        DECIMAL(24,6),
         aivssum97vol_sdo_pesos  DECIMAL(22,2),
         aivs92_sol_pesos        DECIMAL(22,2),
         aivs97_sol_pesos        DECIMAL(22,2),
         aivs92_dif              DECIMAL(24,6),
         aivs97_dif              DECIMAL(24,6),
         aivs92_dif_pesos        DECIMAL(22,2),        
         aivs97_dif_pesos        DECIMAL(22,2)
       END RECORD,
       arr_aceptados        DYNAMIC ARRAY OF RECORD
         nss                     CHAR(11),
         nombre                  CHAR(50),
         tpo_retiro              CHAR(1),
         tpo_seguro              CHAR(2),
         aivs_97                 DECIMAL(15,6),
         pesos_97                DECIMAL(12,2),
         aivs_92                 DECIMAL(15,6),
         pesos_92                DECIMAL(12,2),
         aceptado                CHAR(1),
         afore                   CHAR(30)
       END RECORD,
       arr_rechazados       DYNAMIC ARRAY OF RECORD
         fch_rechazo             DATE,
         modalidad               CHAR(40),
         tpo_retiro              CHAR(40),
         tpo_seguro              CHAR(30),
         mascara                 CHAR(40),
         cod_rechazo             SMALLINT,
         desc_rechazo            CHAR(40),
         cve_afore               CHAR(3),
         desc_afore              CHAR(40),
         nss                     CHAR(11),
         nombre                  CHAR(50),
         aivs_92_sol             DECIMAL(15,6),
         aivs_97_sol             DECIMAL(15,6),
         aivs_92_saldo           DECIMAL(15,6),
         aivs_97_saldo           DECIMAL(15,6),
         aivs_92_dif             DECIMAL(15,6),
         aivs_97_dif             DECIMAL(15,6),
         pesos_92_sol            DECIMAL(12,2),
         pesos_97_sol            DECIMAL(12,2),
         pesos_92_saldo          DECIMAL(12,2),
         pesos_97_saldo          DECIMAL(12,2),
         pesos_92_dif            DECIMAL(12,2),
         pesos_97_dif            DECIMAL(12,2)
       END RECORD,
       arr_reporte,
       arr_export                DYNAMIC ARRAY OF RECORD
         v_folio                LIKE ret_transferencia.folio,
         v_f_carga              LIKE ret_cza_transferencia.f_carga,
         v_afore                STRING,
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
       v_query_where                 STRING, -- clausula where
       v_query_agrupador             STRING, -- agrupador por folio y afore
       v_id_derechohabiente          LIKE afi_derechohabiente.id_derechohabiente,
       v_indice                      SMALLINT, -- indice de arreglo
       v_ruta_reporte                STRING ,-- ruta del archivo del reporte       
       v_ruta_listados               STRING ,-- ruta de los listados
       v_ruta_ejecutable             STRING ,-- ruta del ejecutable
       manejador_rpt                 om.SaxDocumentHandler,
       v_indice_reporte              SMALLINT,
       v_agrupador_folio_fecha_Afore STRING,
       v_es_valido                   SMALLINT,
       v_aivs_diferencia             DECIMAL(24,6),
       v_resultado                   SMALLINT 

   CLOSE WINDOW SCREEN     
   OPEN WINDOW w_consulta WITH FORM "RETC4231"
   LET  v_ventana = UI.WINDOW.GETCURRENT()
   LET v_forma = v_ventana.getForm()

   
   CALL v_ventana.SETTEXT("Consulta Registros Aceptados y Rechazados")

   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_modalidad = ui.ComboBox.forName("formonly.cmb_modalidad")

   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_tpo_retiro = ui.ComboBox.forName("formonly.cmb_tipo_retiro")

   -- se inician los combobox en blanco
   CALL v_cbx_modalidad.clear()
   CALL v_cbx_tpo_retiro.clear()

   INPUT v_nss, v_modalidad, v_tpo_retiro, v_radio, v_f_inicial, v_f_final 
      FROM e_nss, cmb_modalidad, cmb_tipo_retiro, radiogroup1,  date_f_inicial, date_f_final    
      ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)

      BEFORE INPUT 
         -- se limpian las variables
         LET v_nss        = NULL    
         LET v_modalidad  = NULL 
         LET v_tpo_retiro = NULL 
         LET v_radio      = NULL 
         LET v_f_inicial  = NULL      
         LET v_f_final    = NULL
         
         -- se llena el arreglo de la modalidad de retiro
         DECLARE cur_modalidad CURSOR FOR
         SELECT a.modalidad_retiro, a.des_larga
         FROM   ret_modalidad_retiro a
         WHERE  a.modalidad_retiro IN (5, 6, 8)

         LET v_s_cadena = "Todos"
         CALL v_cbx_modalidad.addItem(0, v_s_cadena)
         FOREACH cur_modalidad INTO v_r_modalidad.*
            LET v_s_cadena = v_r_modalidad.des_larga CLIPPED 
            CALL v_cbx_modalidad.addItem(v_r_modalidad.modalidad_retiro, v_s_cadena)
         END FOREACH

         FREE cur_modalidad
         LET v_modalidad = 0
         CALL v_cbx_tpo_retiro.clear()

         ON CHANGE cmb_modalidad
            -- se llena el arreglo de afores
            IF v_modalidad <> 0 THEN 
               CALL v_cbx_tpo_retiro.clear()
               DECLARE  cur_tpo_retiro CURSOR FOR
               SELECT tpo_retiro, des_larga
               FROM   ret_tipo_retiro
               WHERE modalidad_retiro = v_modalidad
               ORDER BY tpo_retiro
               FOREACH cur_tpo_retiro INTO v_r_tpo_retiro.*
                  LET v_s_cadena = v_r_tpo_retiro.v_des_larga
                  CALL v_cbx_tpo_retiro.addItem(v_r_tpo_retiro.v_tpo_retiro, v_s_cadena)
               END FOREACH

               FREE cur_tpo_retiro
            ELSE 
               CALL v_cbx_tpo_retiro.clear()
            END IF 

      ON ACTION ACCEPT

         --DISPLAY "variables capturadas"
         --DISPLAY "@..", v_folio, v_afore, v_nss, v_f_inicial, v_f_final

         -- se borran los arreglos de despliegue
         CALL arr_rechazados.clear()
         CALL arr_aceptados.clear()
         CALL arr_reporte.clear()
         LET v_contador_aceptados = 1

         --modificación de validación de  captura de parametros
         --valida que se ingrese al menos un parametro
         DISPLAY "Los valores son :"
         DISPLAY "v_modalidad ", v_modalidad
         DISPLAY "v_nss ", v_nss
         DISPLAY "v_f_inicial ", v_f_inicial
         DISPLAY "v_f_final ", v_f_final
         DISPLAY "v_radio ", v_radio

         
         IF ( (v_modalidad  IS NULL ) AND 
               (v_nss       IS NULL ) AND 
               (v_f_inicial IS NULL ) AND 
               (v_f_final   IS NULL ) AND 
               (v_radio     IS NULL ) ) THEN

            CALL fn_mensaje("Consulta","Debe de ingresar al menos un criterio de búsqueda","about")
            CONTINUE INPUT
         END IF
         DISPLAY "Dentro del Accept y antes de validar el nss y el periodo de fechas"
         -- 
         IF v_nss IS NOT NULL OR (v_f_inicial IS NOT NULL AND v_f_final IS NOT NULL) THEN 
            LET v_contador_aceptados = 1
            LET v_contador_rechazados = 1
            DISPLAY "Antes del case, v_modalidad <", v_modalidad, "> v_radio <", v_radio, ">"
            CASE v_modalidad 
               WHEN 5 -- Disposición
                  IF v_radio = 1 THEN 
                     CALL fn_busca_disposicion_aceptados(arr_aceptados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_aceptados) RETURNING arr_aceptados, v_contador_aceptados
                  END IF 
                  IF v_radio = 2 THEN
                  CALL fn_busca_disposicion_rechazados(arr_rechazados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_rechazados) RETURNING arr_rechazados, v_contador_rechazados
                  END IF
                  IF v_radio = 3 THEN
                     CALL fn_busca_disposicion_aceptados(arr_aceptados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_aceptados) RETURNING arr_aceptados, v_contador_aceptados
                     CALL fn_busca_disposicion_rechazados(arr_rechazados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_rechazados) RETURNING arr_rechazados, v_contador_rechazados
                  END IF 
               WHEN 6 -- Transferencia
                  IF v_radio = 1 THEN 
                     CALL fn_busca_transferencia_aceptados(arr_aceptados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_aceptados) RETURNING arr_aceptados, v_contador_aceptados
                  END IF 
                  IF v_radio = 2 THEN
                     CALL fn_busca_transferencia_rechazados(arr_rechazados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_rechazados) RETURNING arr_rechazados, v_contador_rechazados
                  END IF
                  IF v_radio = 3 THEN
                     CALL fn_busca_transferencia_aceptados(arr_aceptados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_aceptados) RETURNING arr_aceptados, v_contador_aceptados
                     CALL fn_busca_transferencia_rechazados(arr_rechazados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_rechazados) RETURNING arr_rechazados, v_contador_rechazados
                  END IF 
               WHEN 8 -- PMG
                  IF v_radio = 1 THEN 
                     CALL fn_busca_pmg_aceptados(arr_aceptados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_aceptados) RETURNING arr_aceptados, v_contador_aceptados
                  END IF 
                  IF v_radio = 2 THEN
                     CALL fn_busca_pmg_rechazados(arr_rechazados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_rechazados) RETURNING arr_rechazados, v_contador_rechazados
                  END IF
                  IF v_radio = 3 THEN
                     CALL fn_busca_pmg_aceptados(arr_aceptados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_aceptados) RETURNING arr_aceptados, v_contador_aceptados
                     CALL fn_busca_pmg_rechazados(arr_rechazados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_rechazados) RETURNING arr_rechazados, v_contador_rechazados
                  END IF 
               OTHERWISE 
               DISPLAY "Consultara todas las modalidades"
                  IF v_radio = 1 THEN 
                     CALL fn_busca_disposicion_aceptados(arr_aceptados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_aceptados) RETURNING arr_aceptados, v_contador_aceptados
                     CALL fn_busca_transferencia_aceptados(arr_aceptados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_aceptados) RETURNING arr_aceptados, v_contador_aceptados
                     CALL fn_busca_pmg_aceptados(arr_aceptados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_aceptados) RETURNING arr_aceptados, v_contador_aceptados
                  END IF 
                  IF v_radio = 2 THEN 
                     CALL fn_busca_disposicion_rechazados(arr_rechazados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_rechazados) RETURNING arr_rechazados, v_contador_rechazados
                     CALL fn_busca_transferencia_rechazados(arr_rechazados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_rechazados) RETURNING arr_rechazados, v_contador_rechazados
                     CALL fn_busca_pmg_rechazados(arr_rechazados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_rechazados) RETURNING arr_rechazados, v_contador_rechazados
                  END IF 
                  IF v_radio = 3 THEN 
                     CALL fn_busca_disposicion_aceptados(arr_aceptados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_aceptados) RETURNING arr_aceptados, v_contador_aceptados
                     CALL fn_busca_transferencia_aceptados(arr_aceptados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_aceptados) RETURNING arr_aceptados, v_contador_aceptados
                     CALL fn_busca_pmg_aceptados(arr_aceptados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_aceptados) RETURNING arr_aceptados, v_contador_aceptados
                     CALL fn_busca_disposicion_rechazados(arr_rechazados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_rechazados) RETURNING arr_rechazados, v_contador_rechazados
                     CALL fn_busca_transferencia_rechazados(arr_rechazados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_rechazados) RETURNING arr_rechazados, v_contador_rechazados
                     CALL fn_busca_pmg_rechazados(arr_rechazados, v_f_inicial, v_f_final, v_nss, v_tpo_retiro, v_contador_rechazados) RETURNING arr_rechazados, v_contador_rechazados
                  END IF 
            END CASE 
         ELSE
            CALL fn_mensaje("Consulta","Debe ingresar un rango de fechas o el NSS para realizar la búsqueda","about")
            CONTINUE INPUT 
         END IF 
         

         
         -- se abre la ventana de resultados
         OPEN WINDOW w_detalle_consulta WITH FORM "RETC4232"
         
         -- se abre un dialog para realizar el despliegue de los resultados
         DIALOG
         ATTRIBUTES ( UNBUFFERED )
         
            DISPLAY ARRAY arr_aceptados TO tbl_aceptados.*
                 
            END DISPLAY
            
            DISPLAY ARRAY arr_rechazados TO tbl_rechazados.*
            END DISPLAY
            
            ON ACTION regresar
               EXIT DIALOG
            ON ACTION exportar 
--               OPEN WINDOW w_exporta_consulta WITH FORM "RETC4233"
--                  INPUT v_radio_exporta
--                  FROM radiogroupExp 
--                  ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)
--                  ON ACTION ACCEPT 
                     CALL fn_exporta_archivo(arr_aceptados, arr_rechazados, v_radio) RETURNING v_resultado
--                  END INPUT 
--               CLOSE WINDOW w_exporta_consulta
         END DIALOG
         
         CLOSE WINDOW w_detalle_consulta
         
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta

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
           v_afore                  STRING,
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
   
      PRINTX v_r_despliegue.v_afore,
             v_r_despliegue.v_folio,
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
      LET v_total_aivsvol_sdo             = v_total_aivsvol_sdo           + v_r_despliegue.v_aivsvol_sdo      
      LET v_total_aivssum97vol_sdo       = v_total_aivssum97vol_sdo       + v_r_despliegue.v_aivssum97vol_sdo      
      LET v_total_aivs92_sdo_pesos       = v_total_aivs92_sdo_pesos       + v_r_despliegue.v_aivs92_sdo_pesos
      LET v_total_aivs97_sdo_pesos       = v_total_aivs97_sdo_pesos       + v_r_despliegue.v_aivs97_sdo_pesos
      LET v_total_aivsvol_sdo_pesos       = v_total_aivsvol_sdo_pesos     + v_r_despliegue.v_aivsvol_sdo_pesos
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

FUNCTION fn_busca_disposicion_aceptados(arr_aceptados, v_f_inicial, v_f_final, v_nss, v_tipo_retiro, v_indice)
DEFINE arr_aceptados        DYNAMIC ARRAY OF RECORD
         nss                     CHAR(11),
         nombre                  CHAR(50),
         tpo_retiro              CHAR(1),
         tpo_seguro              CHAR(2),
         aivs_97                 DECIMAL(15,6),
         pesos_97                DECIMAL(12,2),
         aivs_92                 DECIMAL(15,6),
         pesos_92                DECIMAL(12,2),
         aceptado                CHAR(1),
         afore                   CHAR(30)
       END RECORD

DEFINE arr_acep_paso        RECORD 
         nss                     CHAR(11),
         nombre                  CHAR(50),
         tpo_retiro              CHAR(1),
         tpo_seguro              CHAR(2),
         aivs_97                 DECIMAL(15,6),
         pesos_97                DECIMAL(12,2),
         aivs_92                 DECIMAL(15,6),
         pesos_92                DECIMAL(12,2),
         aceptado                CHAR(1),
         afore                   CHAR(30)
       END RECORD

DEFINE v_f_inicial          DATE
DEFINE v_f_final            DATE 
DEFINE v_nss                CHAR(11)
DEFINE v_tipo_retiro        CHAR(1)
DEFINE v_tipo_seguro        CHAR(2)
DEFINE v_indice             INTEGER 
DEFINE v_query              STRING 
DEFINE v_contador           INTEGER



   LET v_query = "\n SELECT g.nss, g.nombre_imss, h.tpo_retiro, h.tpo_seguro,       ", 
                 "\n        a.aivs_viv97, round((a.aivs_viv97 * i.precio_fondo),2), ",
                 "\n        a.aivs_viv92, round((a.aivs_viv92 * i.precio_fondo),2), ",
                 "\n        '1', a.cve_afore || ' ' || f.afore_desc     ",
                 "\n FROM   ret_disposicion a,                          ",              
                 "\n        glo_ctr_archivo b,                          ",
                 "\n        cat_proceso c,                              ",
                 "\n        ret_tipo_retiro d,                          ",
                 "\n        ret_rechazo e,                              ",
                 "\n        cat_afore f,                                ",
                 "\n        afi_derechohabiente g,                      ",
                 "\n        ret_matriz_derecho h,                       ",
                 "\n        ret_cza_disposicion i                       ",
                 "\n WHERE  b.proceso_cod = 1502                        ",
                 "\n AND    b.folio = a.folio                           ",
                 "\n AND    c.proceso_cod = b.proceso_cod               ",
                 "\n AND    a.id_ret_matriz_derecho = h.id_ret_matriz_derecho ",
                 "\n AND    h.tpo_retiro = d.tpo_retiro                 ",
                 "\n AND    h.modalidad_retiro = 5                      ",
                 "\n AND    a.cod_rechazo = e.cod_rechazo               ",
                 "\n AND    a.cve_afore = f.afore_cod                   ",
                 "\n AND    a.id_derechohabiente = g.id_derechohabiente ",
                 "\n AND    a.estado_solicitud IN (30, 50, 60, 70)      ",
                 "\n AND    i.folio = a.folio                           "
                

   -- si se recibieron la fecha inicial y la fecha final como parametro
   IF ( v_f_inicial IS NOT NULL) AND (v_f_final IS NOT NULL ) THEN   
      -- agrupador
      LET v_query = v_query, "\n AND    b.f_actualiza BETWEEN '", v_f_inicial, "' AND '", v_f_final, "'"
   END IF

   IF ( v_nss IS NOT NULL ) THEN 
      LET v_query = v_query, "\n AND    g.nss = '", v_nss, "'"
   END IF 

   IF ( v_tipo_retiro IS NOT NULL ) THEN 
      LET v_query = v_query, "\n AND    h.tpo_retiro = '", v_tipo_retiro CLIPPED , "'"
   END IF 

   -- se ordenan los datos
   LET v_query = v_query, "\n ORDER BY h.tpo_retiro, g.nss"
   DISPLAY "El query \n", v_query
   -- se obtienen los registros para el reporte
   PREPARE sid_disp_acept FROM v_query
   DECLARE cur_disp_acept CURSOR FOR sid_disp_acept

   -- llena el arreglo

   FOREACH cur_disp_acept INTO arr_acep_paso.*
      LET arr_aceptados[v_indice].* = arr_acep_paso.*
      LET v_indice = v_indice + 1
   END FOREACH 
   RETURN arr_aceptados, v_indice
   
END FUNCTION 

FUNCTION fn_busca_transferencia_aceptados(arr_aceptados, v_f_inicial, v_f_final, v_nss, v_tipo_retiro, v_indice)
DEFINE arr_aceptados        DYNAMIC ARRAY OF RECORD
         nss                     CHAR(11),
         nombre                  CHAR(50),
         tpo_retiro              CHAR(1),
         tpo_seguro              CHAR(2),
         aivs_97                 DECIMAL(15,6),
         pesos_97                DECIMAL(12,2),
         aivs_92                 DECIMAL(15,6),
         pesos_92                DECIMAL(12,2),
         aceptado                CHAR(1),
         afore                   CHAR(30)
       END RECORD

DEFINE arr_acep_paso        RECORD 
         nss                     CHAR(11),
         nombre                  CHAR(50),
         tpo_retiro              CHAR(1),
         tpo_seguro              CHAR(2),
         aivs_97                 DECIMAL(15,6),
         pesos_97                DECIMAL(12,2),
         aivs_92                 DECIMAL(15,6),
         pesos_92                DECIMAL(12,2),
         aceptado                CHAR(1),
         afore                   CHAR(30)
       END RECORD

DEFINE v_f_inicial          DATE
DEFINE v_f_final            DATE 
DEFINE v_nss                CHAR(11)
DEFINE v_tipo_retiro        CHAR(1)
DEFINE v_tipo_seguro        CHAR(2)
DEFINE v_indice             INTEGER 
DEFINE v_query              STRING 
DEFINE v_contador           INTEGER



   LET v_query = "\n SELECT g.nss, g.nombre_imss, h.tpo_retiro, h.tpo_seguro,       ", 
                 "\n        a.aivs_viv97, round((a.aivs_viv97 * i.precio_fondo),2), ",
                 "\n        0, 0,                                       ",
                 "\n        '1', a.cve_afore || ' ' || f.afore_desc     ",
                 "\n FROM   ret_transferencia a,                        ",              
                 "\n        glo_ctr_archivo b,                          ",
                 "\n        cat_proceso c,                              ",
                 "\n        ret_tipo_retiro d,                          ",
                 "\n        ret_rechazo e,                              ",
                 "\n        cat_afore f,                                ",
                 "\n        afi_derechohabiente g,                      ",
                 "\n        ret_matriz_derecho h,                       ",
                 "\n        ret_cza_transferencia i                     ",
                 "\n WHERE  b.proceso_cod = 1504                        ",
                 "\n AND    b.folio = a.folio                           ",
                 "\n AND    c.proceso_cod = b.proceso_cod               ",
                 "\n AND    a.id_ret_matriz_derecho = h.id_ret_matriz_derecho ",
                 "\n AND    h.tpo_retiro = d.tpo_retiro                 ",
                 "\n AND    h.modalidad_retiro = 6                      ",
                 "\n AND    a.cod_rechazo = e.cod_rechazo               ",
                 "\n AND    a.cve_afore = f.afore_cod                   ",
                 "\n AND    a.id_derechohabiente = g.id_derechohabiente ",
                 "\n AND    a.estado_solicitud IN (30, 50, 60, 70)      ",
                 "\n AND    i.folio = a.folio                           "
                

   -- si se recibieron la fecha inicial y la fecha final como parametro
   IF ( v_f_inicial IS NOT NULL) AND (v_f_final IS NOT NULL ) THEN   
      -- agrupador
      LET v_query = v_query, "\n AND    b.f_actualiza BETWEEN '", v_f_inicial, "' AND '", v_f_final, "'"
   END IF

   IF ( v_nss IS NOT NULL ) THEN 
      LET v_query = v_query, "\n AND    g.nss = '", v_nss, "'"
   END IF 

   IF ( v_tipo_retiro IS NOT NULL ) THEN 
      LET v_query = v_query, "\n AND    h.tpo_retiro = '", v_tipo_retiro CLIPPED , "'"
   END IF 

   -- se ordenan los datos
   LET v_query = v_query, "\n ORDER BY h.tpo_retiro, g.nss"
   DISPLAY "El query \n", v_query
   -- se obtienen los registros para el reporte
   PREPARE sid_trans_acept FROM v_query
   DECLARE cur_trans_acept CURSOR FOR sid_trans_acept

   -- llena el arreglo
   FOREACH cur_trans_acept INTO arr_acep_paso.*
      LET arr_aceptados[v_indice].* = arr_acep_paso.*
      LET v_indice = v_indice + 1
   END FOREACH 
   RETURN arr_aceptados, v_indice
   
END FUNCTION 

FUNCTION fn_busca_pmg_aceptados(arr_aceptados, v_f_inicial, v_f_final, v_nss, v_tipo_retiro, v_indice)
DEFINE arr_aceptados        DYNAMIC ARRAY OF RECORD
         nss                     CHAR(11),
         nombre                  CHAR(50),
         tpo_retiro              CHAR(1),
         tpo_seguro              CHAR(2),
         aivs_97                 DECIMAL(15,6),
         pesos_97                DECIMAL(12,2),
         aivs_92                 DECIMAL(15,6),
         pesos_92                DECIMAL(12,2),
         aceptado                CHAR(1),
         afore                   CHAR(30)
       END RECORD

DEFINE arr_acep_paso        RECORD 
         nss                     CHAR(11),
         nombre                  CHAR(50),
         tpo_retiro              CHAR(1),
         tpo_seguro              CHAR(2),
         aivs_97                 DECIMAL(15,6),
         pesos_97                DECIMAL(12,2),
         aivs_92                 DECIMAL(15,6),
         pesos_92                DECIMAL(12,2),
         aceptado                CHAR(1),
         afore                   CHAR(30)
       END RECORD

DEFINE v_f_inicial          DATE
DEFINE v_f_final            DATE 
DEFINE v_nss                CHAR(11)
DEFINE v_tipo_retiro        CHAR(1)
DEFINE v_tipo_seguro        char(2)
DEFINE v_indice             INTEGER 
DEFINE v_query              STRING 
DEFINE v_contador           INTEGER



   LET v_query = "\n SELECT g.nss, g.nombre_imss, h.tpo_retiro, h.tpo_seguro,       ", 
                 "\n        a.aivs_viv97, round((a.aivs_viv97 * i.precio_fondo),2), ",
                 "\n        a.aivs_viv92, round((a.aivs_viv92 * i.precio_fondo),2), ",
                 "\n        '1', a.cve_afore || ' ' || f.afore_desc     ",
                 "\n FROM   ret_disposicion a,                          ",              
                 "\n        glo_ctr_archivo b,                          ",
                 "\n        cat_proceso c,                              ",
                 "\n        ret_tipo_retiro d,                          ",
                 "\n        ret_rechazo e,                              ",
                 "\n        cat_afore f,                                ",
                 "\n        afi_derechohabiente g,                      ",
                 "\n        ret_matriz_derecho h,                       ",
                 "\n        ret_cza_disposicion i                       ",
                 "\n WHERE  b.proceso_cod = 1519                        ",
                 "\n AND    b.folio = a.folio                           ",
                 "\n AND    c.proceso_cod = b.proceso_cod               ",
                 "\n AND    a.id_ret_matriz_derecho = h.id_ret_matriz_derecho ",
                 "\n AND    h.tpo_retiro = d.tpo_retiro                 ",
                 "\n AND    h.modalidad_retiro = 5                      ",
                 "\n AND    a.cod_rechazo = e.cod_rechazo               ",
                 "\n AND    a.cve_afore = f.afore_cod                   ",
                 "\n AND    a.id_derechohabiente = g.id_derechohabiente ",
                 "\n AND    a.estado_solicitud IN (30, 50, 60, 70)      ",
                 "\n AND    i.folio = a.folio                           "
                

   -- si se recibieron la fecha inicial y la fecha final como parametro
   IF ( v_f_inicial IS NOT NULL) AND (v_f_final IS NOT NULL ) THEN   
      -- agrupador
      LET v_query = v_query, "\n AND    b.f_actualiza BETWEEN '", v_f_inicial, "' AND '", v_f_final, "'"
   END IF

   IF ( v_nss IS NOT NULL ) THEN 
      LET v_query = v_query, "\n AND    g.nss = '", v_nss, "'"
   END IF 

   IF ( v_tipo_retiro IS NOT NULL ) THEN 
      LET v_query = v_query, "\n AND    h.tpo_retiro = '", v_tipo_retiro CLIPPED , "'"
   END IF 

   -- se ordenan los datos
   LET v_query = v_query, "\n ORDER BY h.tpo_retiro, g.nss"
   DISPLAY "El query \n", v_query
   -- se obtienen los registros para el reporte
   PREPARE sid_pmg_acept FROM v_query
   DECLARE cur_pmg_acept CURSOR FOR sid_pmg_acept

   -- llena el arreglo
   FOREACH cur_pmg_acept INTO arr_acep_paso.*
      LET arr_aceptados[v_indice].* = arr_acep_paso.*
      LET v_indice = v_indice + 1
   END FOREACH 
   RETURN arr_aceptados, v_indice
   
END FUNCTION 

FUNCTION fn_busca_disposicion_rechazados(arr_rechazados, v_f_inicial, v_f_final, v_nss, v_tipo_retiro, v_indice)
DEFINE arr_rechazados        DYNAMIC ARRAY OF RECORD
         fch_rechazo             DATE,
         modalidad               CHAR(40),
         tpo_retiro              CHAR(40),
         tpo_seguro              CHAR(30),
         mascara                 CHAR(40),
         cod_rechazo             SMALLINT,
         desc_rechazo            CHAR(40),
         cve_afore               CHAR(3),
         desc_afore              CHAR(40),
         nss                     CHAR(11),
         nombre                  CHAR(50),
         aivs_92_sol             DECIMAL(15,6),
         aivs_97_sol             DECIMAL(15,6),
         aivs_92_saldo           DECIMAL(15,6),
         aivs_97_saldo           DECIMAL(15,6),
         aivs_92_dif             DECIMAL(15,6),
         aivs_97_dif             DECIMAL(15,6),
         pesos_92_sol            DECIMAL(12,2),
         pesos_97_sol            DECIMAL(12,2),
         pesos_92_saldo          DECIMAL(12,2),
         pesos_97_saldo          DECIMAL(12,2),
         pesos_92_dif            DECIMAL(12,2),
         pesos_97_dif            DECIMAL(12,2)
       END RECORD

DEFINE arr_rech_paso        RECORD 
         fch_rechazo             DATE,
         modalidad               CHAR(40),
         tpo_retiro              CHAR(40),
         tpo_seguro              CHAR(30),
         mascara                 CHAR(40),
         cod_rechazo             SMALLINT,
         desc_rechazo            CHAR(40),
         cve_afore               CHAR(3),
         desc_afore              CHAR(40),
         nss                     CHAR(11),
         nombre                  CHAR(50),
         aivs_92_sol             DECIMAL(15,6),
         aivs_97_sol             DECIMAL(15,6),
         aivs_92_saldo           DECIMAL(15,6),
         aivs_97_saldo           DECIMAL(15,6),
         aivs_92_dif             DECIMAL(15,6),
         aivs_97_dif             DECIMAL(15,6),
         pesos_92_sol            DECIMAL(12,2),
         pesos_97_sol            DECIMAL(12,2),
         pesos_92_saldo          DECIMAL(12,2),
         pesos_97_saldo          DECIMAL(12,2),
         pesos_92_dif            DECIMAL(12,2),
         pesos_97_dif            DECIMAL(12,2)
       END RECORD
DEFINE  v_precio_valuacion        DECIMAL(15,5)

DEFINE arr_sol_saldo_dif    RECORD 
         sol_aivs             DECIMAL(15,6),
         saldo_aivs           DECIMAL(15,6),
         dif_aivs             DECIMAL(15,6),
         sol_pesos            DECIMAL(12,2),
         saldo_pesos          DECIMAL(12,2),
         dif_pesos            DECIMAL(12,2),
         f_valuacion          DATE 
      END RECORD 
         

DEFINE v_f_inicial          DATE
DEFINE v_f_final            DATE 
DEFINE v_nss                CHAR(11)
DEFINE v_tipo_retiro        CHAR(1)
DEFINE v_indice             INTEGER 
DEFINE v_query              STRING 
DEFINE v_contador           INTEGER
DEFINE v_valor_accion       DECIMAL(9,6)

   -- Obtiene los registros rechazados de la tabla ret_disposicion
   
   LET v_query = "\n SELECT b.f_actualiza, 'Disposición', d.des_corta, j.des_corta, b.nombre_archivo, a.cod_rechazo, e.des_corta,    ",
                 "\n        a.cve_afore, f.afore_desc, g.nss, TRIM(a.paterno_afore) || '$' || TRIM(a.materno_afore) || '$' || TRIM(a.nombre_afore) as nombre, a.aivs_viv92, a.aivs_viv97,        ",
                 "\n        0,0,0,0,                                                                            ",
                 "\n        round((a.aivs_viv92 * i.precio_fondo),2), round((a.aivs_viv97 * i.precio_fondo),2), ",
                 "\n        0,0,0,0,i.precio_fondo                                                                           ",
                 "\n FROM   ret_disposicion a,                                    ",
                 "\n        glo_ctr_archivo b,                                    ",
                 "\n        cat_proceso c,                                        ",
                 "\n        ret_tipo_retiro d,                                    ",
                 "\n        ret_rechazo e,                                        ",
                 "\n        cat_afore f,                                          ",
                 "\n        afi_derechohabiente g,                                ",
                 "\n        ret_matriz_derecho h,                                 ",
                 "\n        ret_cza_disposicion i,                                ",
                 "\n        ret_tipo_seguro j                                     ",
                 "\n WHERE  b.proceso_cod = 1502                                  ",
                 "\n AND    b.folio = a.folio                                     ",
                 "\n AND    c.proceso_cod = b.proceso_cod                         ",
                 "\n AND    a.id_ret_matriz_derecho = h.id_ret_matriz_derecho     ",
                 "\n AND    h.tpo_retiro = d.tpo_retiro                           ",
                 "\n AND    h.modalidad_retiro = 5                                ",
                 "\n AND    a.cod_rechazo = e.cod_rechazo                         ",
                 "\n AND    a.cve_afore = f.afore_cod                             ",
                 "\n AND    a.id_derechohabiente = g.id_derechohabiente           ",
                 "\n AND    a.estado_solicitud IN (100, 101)                      ",
                 "\n AND    i.folio = a.folio                                     ",
                 "\n AND    h.tpo_seguro = j.tpo_seguro                           "

   -- si se recibieron la fecha inicial y la fecha final como parametro
   IF ( v_f_inicial IS NOT NULL) AND (v_f_final IS NOT NULL ) THEN   
      -- agrupador
      LET v_query = v_query, "\n AND    b.f_actualiza BETWEEN '", v_f_inicial, "' AND '", v_f_final, "'"
   END IF

   IF ( v_nss IS NOT NULL ) THEN 
      LET v_query = v_query, "\n AND    g.nss = '", v_nss, "'"
   END IF 

   IF ( v_tipo_retiro IS NOT NULL ) THEN 
      LET v_query = v_query, "\n AND    h.tpo_retiro = '", v_tipo_retiro CLIPPED , "'"
   END IF 

   -- se ordenan los datos
   LET v_query = v_query, "\n ORDER BY h.tpo_retiro, g.nss"
   DISPLAY "El query \n", v_query
   -- se obtienen los registros para el reporte
   PREPARE sid_disp_rech FROM v_query
   DECLARE cur_disp_rech CURSOR FOR sid_disp_rech

   -- llena el arreglo
   FOREACH cur_disp_rech INTO arr_rech_paso.*, v_precio_valuacion
      LET arr_sol_saldo_dif.sol_aivs = arr_rech_paso.aivs_97_sol
      LET arr_sol_saldo_dif.f_valuacion = arr_rech_paso.fch_rechazo
      CALL fn_saldo_al_dia(arr_rech_paso.nss, 4, v_precio_valuacion, arr_sol_saldo_dif.*) RETURNING arr_sol_saldo_dif.*
      LET arr_rech_paso.aivs_97_saldo = arr_sol_saldo_dif.saldo_aivs
      LET arr_rech_paso.aivs_97_dif = arr_sol_saldo_dif.dif_aivs
      LET arr_rech_paso.pesos_97_sol = arr_sol_saldo_dif.sol_pesos
      LET arr_rech_paso.pesos_97_saldo = arr_sol_saldo_dif.saldo_pesos
      LET arr_rech_paso.pesos_97_dif = arr_sol_saldo_dif.dif_pesos

      LET arr_sol_saldo_dif.sol_aivs = arr_rech_paso.aivs_92_sol
      LET arr_sol_saldo_dif.f_valuacion = arr_rech_paso.fch_rechazo
      CALL fn_saldo_al_dia(arr_rech_paso.nss, 8, v_precio_valuacion, arr_sol_saldo_dif.*) RETURNING arr_sol_saldo_dif.*
      LET arr_rech_paso.aivs_92_saldo = arr_sol_saldo_dif.saldo_aivs
      LET arr_rech_paso.aivs_92_dif = arr_sol_saldo_dif.dif_aivs
      LET arr_rech_paso.pesos_92_sol = arr_sol_saldo_dif.sol_pesos
      LET arr_rech_paso.pesos_92_saldo = arr_sol_saldo_dif.saldo_pesos
      LET arr_rech_paso.pesos_92_dif = arr_sol_saldo_dif.dif_pesos
      
      LET arr_rechazados[v_indice].* = arr_rech_paso.*
      LET v_indice = v_indice + 1
   END FOREACH 

   -- Obtiene los registros rechazados de la tabla ret_disposicion_rch
   
   LET v_query = "\n SELECT b.f_actualiza, 'Disposición', d.des_corta, j.des_corta,  b.nombre_archivo, a.cod_rechazo_1, e.des_corta,  ",
                 "\n        a.cve_afore, f.afore_desc, a.nss, TRIM(a.paterno_afore) || '$' || TRIM(a.materno_afore) || '$' || TRIM(a.nombre_afore) as nombre, ",
                 "\n        a.aivs_viv92, a.aivs_viv97,        ",
                 "\n        0,0,0,0,                                                                            ",
                 "\n        round((a.aivs_viv92 * i.precio_fondo),2), round((a.aivs_viv97 * i.precio_fondo),2), ",
                 "\n        0,0,0,0, i.precio_fondo                                                                             ",
                 "\n FROM   ret_disposicion_rch a,                                ",
                 "\n        glo_ctr_archivo b,                                    ",
                 "\n        cat_proceso c,                                        ",
                 "\n        ret_tipo_retiro d,                                    ",
                 "\n        ret_rechazo e,                                        ",
                 "\n        cat_afore f,                                          ",
                 "\n        ret_matriz_derecho h,                                 ",
                 "\n        ret_cza_disposicion i,                                ",
                 "\n        ret_tipo_seguro j                                     ",
                 "\n WHERE  b.proceso_cod = 1502                                  ",
                 "\n AND    b.folio = a.folio                                     ",
                 "\n AND    c.proceso_cod = b.proceso_cod                         ",
                 "\n AND    a.tipo_retiro = h.tpo_retiro                          ",
                 "\n AND    a.regimen  = h.regimen                                ",
                 "\n AND    a.tpo_seguro = h.tpo_seguro                           ",
                 "\n AND    a.tpo_pension = h.tpo_pension                         ",
                 "\n AND    a.tpo_prestacion = h.tpo_prestacion                   ",
                 "\n AND    h.tpo_retiro = d.tpo_retiro                           ",
                 "\n AND    h.modalidad_retiro = 5                                ",
                 "\n AND    a.cod_rechazo_1 = e.cod_rechazo                       ",
                 "\n AND    a.cve_afore = f.afore_cod                             ",
                 "\n AND    a.folio = i.folio                                     ",
                 "\n AND    h.tpo_seguro = j.tpo_seguro                           "

   -- si se recibieron la fecha inicial y la fecha final como parametro
   IF ( v_f_inicial IS NOT NULL) AND (v_f_final IS NOT NULL ) THEN   
      -- agrupador
      LET v_query = v_query, "\n AND    b.f_actualiza BETWEEN '", v_f_inicial, "' AND '", v_f_final, "'"
   END IF

   IF ( v_nss IS NOT NULL ) THEN 
      LET v_query = v_query, "\n AND    a.nss = '", v_nss, "'"
   END IF 

   IF ( v_tipo_retiro IS NOT NULL ) THEN 
      LET v_query = v_query, "\n AND    h.tpo_retiro = '", v_tipo_retiro CLIPPED , "'"
   END IF 

   -- se ordenan los datos
   LET v_query = v_query, "\n ORDER BY h.tpo_retiro, a.nss"
   DISPLAY "El query \n", v_query
   -- se obtienen los registros para el reporte
   PREPARE sid_disp_rech_rch FROM v_query
   DECLARE cur_disp_rech_rch CURSOR FOR sid_disp_rech_rch

   -- llena el arreglo
   FOREACH cur_disp_rech_rch INTO arr_rech_paso.*, v_precio_valuacion
      LET arr_sol_saldo_dif.sol_aivs = arr_rech_paso.aivs_97_sol
      LET arr_sol_saldo_dif.f_valuacion = arr_rech_paso.fch_rechazo
      CALL fn_saldo_al_dia(arr_rech_paso.nss, 4, v_precio_valuacion, arr_sol_saldo_dif.*) RETURNING arr_sol_saldo_dif.*
      LET arr_rech_paso.aivs_97_saldo = arr_sol_saldo_dif.saldo_aivs
      LET arr_rech_paso.aivs_97_dif = arr_sol_saldo_dif.dif_aivs
      LET arr_rech_paso.pesos_97_sol = arr_sol_saldo_dif.sol_pesos
      LET arr_rech_paso.pesos_97_saldo = arr_sol_saldo_dif.saldo_pesos
      LET arr_rech_paso.pesos_97_dif = arr_sol_saldo_dif.dif_pesos

      LET arr_sol_saldo_dif.sol_aivs = arr_rech_paso.aivs_92_sol
      LET arr_sol_saldo_dif.f_valuacion = arr_rech_paso.fch_rechazo
      CALL fn_saldo_al_dia(arr_rech_paso.nss, 8, v_precio_valuacion, arr_sol_saldo_dif.*) RETURNING arr_sol_saldo_dif.*
      LET arr_rech_paso.aivs_92_saldo = arr_sol_saldo_dif.saldo_aivs
      LET arr_rech_paso.aivs_92_dif = arr_sol_saldo_dif.dif_aivs
      LET arr_rech_paso.pesos_92_sol = arr_sol_saldo_dif.sol_pesos
      LET arr_rech_paso.pesos_92_saldo = arr_sol_saldo_dif.saldo_pesos
      LET arr_rech_paso.pesos_92_dif = arr_sol_saldo_dif.dif_pesos
      
      LET arr_rechazados[v_indice].* = arr_rech_paso.*
      LET v_indice = v_indice + 1
   END FOREACH 

   RETURN arr_rechazados, v_indice
   
END FUNCTION 

FUNCTION fn_busca_transferencia_rechazados(arr_rechazados, v_f_inicial, v_f_final, v_nss, v_tipo_retiro, v_indice)
DEFINE arr_rechazados        DYNAMIC ARRAY OF RECORD
         fch_rechazo             DATE,
         modalidad               CHAR(40),
         tpo_retiro              CHAR(40),
         tpo_seguro              CHAR(30),
         mascara                 CHAR(40),
         cod_rechazo             SMALLINT,
         desc_rechazo            CHAR(40),
         cve_afore               CHAR(3),
         desc_afore              CHAR(40),
         nss                     CHAR(11),
         nombre                  CHAR(50),
         aivs_92_sol             DECIMAL(15,6),
         aivs_97_sol             DECIMAL(15,6),
         aivs_92_saldo           DECIMAL(15,6),
         aivs_97_saldo           DECIMAL(15,6),
         aivs_92_dif             DECIMAL(15,6),
         aivs_97_dif             DECIMAL(15,6),
         pesos_92_sol            DECIMAL(12,2),
         pesos_97_sol            DECIMAL(12,2),
         pesos_92_saldo          DECIMAL(12,2),
         pesos_97_saldo          DECIMAL(12,2),
         pesos_92_dif            DECIMAL(12,2),
         pesos_97_dif            DECIMAL(12,2)
       END RECORD

DEFINE arr_rech_paso        RECORD 
         fch_rechazo             DATE,
         modalidad               CHAR(40),
         tpo_retiro              CHAR(40),
         tpo_seguro              CHAR(30),
         mascara                 CHAR(40),
         cod_rechazo             SMALLINT,
         desc_rechazo            CHAR(40),
         cve_afore               CHAR(3),
         desc_afore              CHAR(40),
         nss                     CHAR(11),
         nombre                  CHAR(50),
         aivs_92_sol             DECIMAL(15,6),
         aivs_97_sol             DECIMAL(15,6),
         aivs_92_saldo           DECIMAL(15,6),
         aivs_97_saldo           DECIMAL(15,6),
         aivs_92_dif             DECIMAL(15,6),
         aivs_97_dif             DECIMAL(15,6),
         pesos_92_sol            DECIMAL(12,2),
         pesos_97_sol            DECIMAL(12,2),
         pesos_92_saldo          DECIMAL(12,2),
         pesos_97_saldo          DECIMAL(12,2),
         pesos_92_dif            DECIMAL(12,2),
         pesos_97_dif            DECIMAL(12,2)
       END RECORD
DEFINE  v_precio_valuacion       DECIMAL(15,5)
DEFINE arr_sol_saldo_dif    RECORD 
         sol_aivs             DECIMAL(15,6),
         saldo_aivs           DECIMAL(15,6),
         dif_aivs             DECIMAL(15,6),
         sol_pesos            DECIMAL(12,2),
         saldo_pesos          DECIMAL(12,2),
         dif_pesos            DECIMAL(12,2),
         f_valuacion          DATE 
      END RECORD 
         
DEFINE v_f_inicial          DATE
DEFINE v_f_final            DATE 
DEFINE v_nss                CHAR(11)
DEFINE v_tipo_retiro        CHAR(1)
DEFINE v_indice             INTEGER 
DEFINE v_query              STRING 
DEFINE v_contador           INTEGER
DEFINE v_valor_accion       DECIMAL(9,6)

   -- Obtiene los registros rechazados de la tabla ret_disposicion
   
   LET v_query = "\n SELECT b.f_actualiza, 'Transferencia', d.des_corta, j.des_corta, b.nombre_archivo, a.cod_rechazo, e.des_corta,    ",
                 "\n        a.cve_afore, f.afore_desc, g.nss, TRIM(a.paterno_afore) || '$' || TRIM(a.materno_afore) || '$' || TRIM(a.nombre_afore) as nombre, 0, a.aivs_viv97,                   ",
                 "\n        0,0,0,0,                                                                            ",
                 "\n        0, round((a.aivs_viv97 * i.precio_fondo),2), ",
                 "\n        0,0,0,0,i.precio_fondo                                                                             ",
                 "\n FROM   ret_transferencia a,                                    ",
                 "\n        glo_ctr_archivo b,                                    ",
                 "\n        cat_proceso c,                                        ",
                 "\n        ret_tipo_retiro d,                                    ",
                 "\n        ret_rechazo e,                                        ",
                 "\n        cat_afore f,                                          ",
                 "\n        afi_derechohabiente g,                                ",
                 "\n        ret_matriz_derecho h,                                 ",
                 "\n        ret_cza_transferencia i,                              ",
                 "\n        ret_tipo_seguro j                                     ",
                 "\n WHERE  b.proceso_cod = 1504                                  ",
                 "\n AND    b.folio = a.folio                                     ",
                 "\n AND    c.proceso_cod = b.proceso_cod                         ",
                 "\n AND    a.id_ret_matriz_derecho = h.id_ret_matriz_derecho     ",
                 "\n AND    h.tpo_retiro = d.tpo_retiro                           ",
                 "\n AND    h.modalidad_retiro = 6                                ",
                 "\n AND    a.cod_rechazo = e.cod_rechazo                         ",
                 "\n AND    a.cve_afore = f.afore_cod                             ",
                 "\n AND    a.id_derechohabiente = g.id_derechohabiente           ",
                 "\n AND    a.estado_solicitud IN (100, 101)                      ",
                 "\n AND    a.folio = i.folio                                     ",
                 "\n AND    h.tpo_seguro = j.tpo_seguro                           "

   -- si se recibieron la fecha inicial y la fecha final como parametro
   IF ( v_f_inicial IS NOT NULL) AND (v_f_final IS NOT NULL ) THEN   
      -- agrupador
      LET v_query = v_query, "\n AND    b.f_actualiza BETWEEN '", v_f_inicial, "' AND '", v_f_final, "'"
   END IF

   IF ( v_nss IS NOT NULL ) THEN 
      LET v_query = v_query, "\n AND    g.nss = '", v_nss, "'"
   END IF 

   IF ( v_tipo_retiro IS NOT NULL ) THEN 
      LET v_query = v_query, "\n AND    h.tpo_retiro = '", v_tipo_retiro CLIPPED , "'"
   END IF 

   -- se ordenan los datos
   LET v_query = v_query, "\n ORDER BY h.tpo_retiro, g.nss"
   DISPLAY "El query \n", v_query
   -- se obtienen los registros para el reporte
   PREPARE sid_trans_rech FROM v_query
   DECLARE cur_trans_rech CURSOR FOR sid_trans_rech

   -- llena el arreglo
   FOREACH cur_trans_rech INTO arr_rech_paso.*, v_precio_valuacion
      LET arr_sol_saldo_dif.sol_aivs = arr_rech_paso.aivs_97_sol
      LET arr_sol_saldo_dif.f_valuacion = arr_rech_paso.fch_rechazo
      CALL fn_saldo_al_dia(arr_rech_paso.nss, 4, v_precio_valuacion, arr_sol_saldo_dif.*) RETURNING arr_sol_saldo_dif.*
      LET arr_rech_paso.aivs_97_saldo = arr_sol_saldo_dif.saldo_aivs
      LET arr_rech_paso.aivs_97_dif = arr_sol_saldo_dif.dif_aivs
      LET arr_rech_paso.pesos_97_sol = arr_sol_saldo_dif.sol_pesos
      LET arr_rech_paso.pesos_97_saldo = arr_sol_saldo_dif.saldo_pesos
      LET arr_rech_paso.pesos_97_dif = arr_sol_saldo_dif.dif_pesos
      LET arr_rechazados[v_indice].* = arr_rech_paso.*
      LET v_indice = v_indice + 1
   END FOREACH 

   -- Obtiene los registros rechazados de la tabla ret_disposicion_rch
   
   LET v_query = "\n SELECT b.f_actualiza, 'Transferencia', d.des_corta, j.des_corta, b.nombre_archivo, a.cod_rechazo_1, e.des_corta,  ",
                 "\n        a.cve_afore, f.afore_desc, a.nss, a.nombre_datamart, 0, a.aivs_viv97,        ",
                 "\n        0,0,0,0,                                                                            ",
                 "\n        0, round((a.aivs_viv97 * i.precio_fondo),2), ",
                 "\n        0,0,0,0,i.precio_fondo                                                                             ",
                 "\n FROM   ret_transferencia_rch a,                              ",
                 "\n        glo_ctr_archivo b,                                    ",
                 "\n        cat_proceso c,                                        ",
                 "\n        ret_tipo_retiro d,                                    ",
                 "\n        ret_rechazo e,                                        ",
                 "\n        cat_afore f,                                          ",
                 "\n        ret_matriz_derecho h,                                 ",
                 "\n        ret_cza_transferencia i,                              ",
                 "\n        ret_tipo_seguro j                                     ",
                 "\n WHERE  b.proceso_cod = 1504                                  ",
                 "\n AND    b.folio = a.folio                                     ",
                 "\n AND    c.proceso_cod = b.proceso_cod                         ",
                 "\n AND    a.tpo_retiro = h.tpo_retiro                           ",
                 "\n AND    a.regimen  = h.regimen                                ",
                 "\n AND    a.tpo_seguro = h.tpo_seguro                           ",
                 "\n AND    a.tpo_pension = h.tpo_pension                         ",
                 "\n AND    a.tpo_prestacion = h.tpo_prestacion                   ",
                 "\n AND    h.tpo_retiro = d.tpo_retiro                           ",
                 "\n AND    h.modalidad_retiro = 6                                ",
                 "\n AND    a.cod_rechazo_1 = e.cod_rechazo                       ",
                 "\n AND    a.cve_afore = f.afore_cod                             ",
                 "\n AND    a.folio = i.folio                                     ",
                 "\n AND    h.tpo_seguro = j.tpo_seguro                           "

   -- si se recibieron la fecha inicial y la fecha final como parametro
   IF ( v_f_inicial IS NOT NULL) AND (v_f_final IS NOT NULL ) THEN   
      -- agrupador
      LET v_query = v_query, "\n AND    b.f_actualiza BETWEEN '", v_f_inicial, "' AND '", v_f_final, "'"
   END IF

   IF ( v_nss IS NOT NULL ) THEN 
      LET v_query = v_query, "\n AND    a.nss = '", v_nss, "'"
   END IF 

   IF ( v_tipo_retiro IS NOT NULL ) THEN 
      LET v_query = v_query, "\n AND    h.tpo_retiro = '", v_tipo_retiro CLIPPED , "'"
   END IF 

   -- se ordenan los datos
   LET v_query = v_query, "\n ORDER BY h.tpo_retiro, a.nss"
   DISPLAY "El query \n", v_query
   -- se obtienen los registros para el reporte
   PREPARE sid_trans_rech_rch FROM v_query
   DECLARE cur_trans_rech_rch CURSOR FOR sid_trans_rech_rch

   -- llena el arreglo
   FOREACH cur_trans_rech_rch INTO arr_rech_paso.*, v_precio_valuacion
      LET arr_sol_saldo_dif.sol_aivs = arr_rech_paso.aivs_97_sol
      LET arr_sol_saldo_dif.f_valuacion = arr_rech_paso.fch_rechazo
      CALL fn_saldo_al_dia(arr_rech_paso.nss, 4, v_precio_valuacion, arr_sol_saldo_dif.*) RETURNING arr_sol_saldo_dif.*
      LET arr_rech_paso.aivs_97_saldo = arr_sol_saldo_dif.saldo_aivs
      LET arr_rech_paso.aivs_97_dif = arr_sol_saldo_dif.dif_aivs
      LET arr_rech_paso.pesos_97_sol = arr_sol_saldo_dif.sol_pesos
      LET arr_rech_paso.pesos_97_saldo = arr_sol_saldo_dif.saldo_pesos
      LET arr_rech_paso.pesos_97_dif = arr_sol_saldo_dif.dif_pesos
      LET arr_rechazados[v_indice].* = arr_rech_paso.*
      LET v_indice = v_indice + 1
   END FOREACH 

   RETURN arr_rechazados, v_indice
   
END FUNCTION 

FUNCTION fn_busca_pmg_rechazados(arr_rechazados, v_f_inicial, v_f_final, v_nss, v_tipo_retiro, v_indice)
DEFINE arr_rechazados        DYNAMIC ARRAY OF RECORD
         fch_rechazo             DATE,
         modalidad               CHAR(40),
         tpo_retiro              CHAR(40),
         tpo_seguro              CHAR(30),
         mascara                 CHAR(40),
         cod_rechazo             SMALLINT,
         desc_rechazo            CHAR(40),
         cve_afore               CHAR(3),
         desc_afore              CHAR(40),
         nss                     CHAR(11),
         nombre                  CHAR(50),
         aivs_92_sol             DECIMAL(15,6),
         aivs_97_sol             DECIMAL(15,6),
         aivs_92_saldo           DECIMAL(15,6),
         aivs_97_saldo           DECIMAL(15,6),
         aivs_92_dif             DECIMAL(15,6),
         aivs_97_dif             DECIMAL(15,6),
         pesos_92_sol            DECIMAL(12,2),
         pesos_97_sol            DECIMAL(12,2),
         pesos_92_saldo          DECIMAL(12,2),
         pesos_97_saldo          DECIMAL(12,2),
         pesos_92_dif            DECIMAL(12,2),
         pesos_97_dif            DECIMAL(12,2)
       END RECORD

DEFINE arr_rech_paso        RECORD 
         fch_rechazo             DATE,
         modalidad               CHAR(40),
         tpo_retiro              CHAR(40),
         tpo_seguro              CHAR(30),
         mascara                 CHAR(40),
         cod_rechazo             SMALLINT,
         desc_rechazo            CHAR(40),
         cve_afore               CHAR(3),
         desc_afore              CHAR(40),
         nss                     CHAR(11),
         nombre                  CHAR(50),
         aivs_92_sol             DECIMAL(15,6),
         aivs_97_sol             DECIMAL(15,6),
         aivs_92_saldo           DECIMAL(15,6),
         aivs_97_saldo           DECIMAL(15,6),
         aivs_92_dif             DECIMAL(15,6),
         aivs_97_dif             DECIMAL(15,6),
         pesos_92_sol            DECIMAL(12,2),
         pesos_97_sol            DECIMAL(12,2),
         pesos_92_saldo          DECIMAL(12,2),
         pesos_97_saldo          DECIMAL(12,2),
         pesos_92_dif            DECIMAL(12,2),
         pesos_97_dif            DECIMAL(12,2)
       END RECORD
DEFINE  v_precio_valuacion       DECIMAL(15,5)
DEFINE arr_sol_saldo_dif    RECORD 
         sol_aivs             DECIMAL(15,6),
         saldo_aivs           DECIMAL(15,6),
         dif_aivs             DECIMAL(15,6),
         sol_pesos            DECIMAL(12,2),
         saldo_pesos          DECIMAL(12,2),
         dif_pesos            DECIMAL(12,2),
         f_valuacion          DATE 
      END RECORD 
         

DEFINE v_f_inicial          DATE
DEFINE v_f_final            DATE 
DEFINE v_nss                CHAR(11)
DEFINE v_tipo_retiro        CHAR(1)
DEFINE v_indice             INTEGER 
DEFINE v_query              STRING 
DEFINE v_contador           INTEGER
DEFINE v_valor_accion       DECIMAL(9,6)

   -- Obtiene los registros rechazados de la tabla ret_disposicion
   
   LET v_query = "\n SELECT b.f_actualiza, 'Disposición PMG', d.des_corta, j.des_corta, b.nombre_archivo, a.cod_rechazo, e.des_corta,    ",
                 "\n        a.cve_afore, f.afore_desc, g.nss, TRIM(a.paterno_afore) || '$' || TRIM(a.materno_afore) || '$' || TRIM(a.nombre_afore) as nombre, a.aivs_viv92, a.aivs_viv97,        ",
                 "\n        0,0,0,0,                                                                            ",
                 "\n        round((a.aivs_viv92 * i.precio_fondo),2), round((a.aivs_viv97 * i.precio_fondo),2), ",
                 "\n        0,0,0,0,i.precio_fondo                                                                             ",
                 "\n FROM   ret_disposicion a,                                    ",
                 "\n        glo_ctr_archivo b,                                    ",
                 "\n        cat_proceso c,                                        ",
                 "\n        ret_tipo_retiro d,                                    ",
                 "\n        ret_rechazo e,                                        ",
                 "\n        cat_afore f,                                          ",
                 "\n        afi_derechohabiente g,                                ",
                 "\n        ret_matriz_derecho h,                                 ",
                 "\n        ret_cza_disposicion i,                                ",
                 "\n        ret_tipo_seguro j                                     ",
                 "\n WHERE  b.proceso_cod = 1519                                  ",
                 "\n AND    b.folio = a.folio                                     ",
                 "\n AND    c.proceso_cod = b.proceso_cod                         ",
                 "\n AND    a.id_ret_matriz_derecho = h.id_ret_matriz_derecho     ",
                 "\n AND    h.tpo_retiro = d.tpo_retiro                           ",
                 "\n AND    h.modalidad_retiro = 5                                ",
                 "\n AND    a.cod_rechazo = e.cod_rechazo                         ",
                 "\n AND    a.cve_afore = f.afore_cod                             ",
                 "\n AND    a.id_derechohabiente = g.id_derechohabiente           ",
                 "\n AND    a.estado_solicitud IN (100, 101)                      ",
                 "\n AND    a.folio = i.folio                                     ",
                 "\n AND    h.tpo_seguro = j.tpo_seguro                           "

   -- si se recibieron la fecha inicial y la fecha final como parametro
   IF ( v_f_inicial IS NOT NULL) AND (v_f_final IS NOT NULL ) THEN   
      -- agrupador
      LET v_query = v_query, "\n AND    b.f_actualiza BETWEEN '", v_f_inicial, "' AND '", v_f_final, "'"
   END IF

   IF ( v_nss IS NOT NULL ) THEN 
      LET v_query = v_query, "\n AND    g.nss = '", v_nss, "'"
   END IF 

   IF ( v_tipo_retiro IS NOT NULL ) THEN 
      LET v_query = v_query, "\n AND    h.tpo_retiro = '", v_tipo_retiro CLIPPED , "'"
   END IF 

   -- se ordenan los datos
   LET v_query = v_query, "\n ORDER BY h.tpo_retiro, g.nss"
   DISPLAY "El query \n", v_query
   -- se obtienen los registros para el reporte
   PREPARE sid_pmg_rech FROM v_query
   DECLARE cur_pmg_rech CURSOR FOR sid_pmg_rech

   -- llena el arreglo
   FOREACH cur_pmg_rech INTO arr_rech_paso.*, v_precio_valuacion
      LET arr_sol_saldo_dif.sol_aivs = arr_rech_paso.aivs_97_sol
      LET arr_sol_saldo_dif.f_valuacion = arr_rech_paso.fch_rechazo
      CALL fn_saldo_al_dia(arr_rech_paso.nss, 4, v_precio_valuacion, arr_sol_saldo_dif.*) RETURNING arr_sol_saldo_dif.*
      LET arr_rech_paso.aivs_97_saldo = arr_sol_saldo_dif.saldo_aivs
      LET arr_rech_paso.aivs_97_dif = arr_sol_saldo_dif.dif_aivs
      LET arr_rech_paso.pesos_97_sol = arr_sol_saldo_dif.sol_pesos
      LET arr_rech_paso.pesos_97_saldo = arr_sol_saldo_dif.saldo_pesos
      LET arr_rech_paso.pesos_97_dif = arr_sol_saldo_dif.dif_pesos

      LET arr_sol_saldo_dif.sol_aivs = arr_rech_paso.aivs_92_sol
      LET arr_sol_saldo_dif.f_valuacion = arr_rech_paso.fch_rechazo
      CALL fn_saldo_al_dia(arr_rech_paso.nss, 8, v_precio_valuacion, arr_sol_saldo_dif.*) RETURNING arr_sol_saldo_dif.*
      LET arr_rech_paso.aivs_92_saldo = arr_sol_saldo_dif.saldo_aivs
      LET arr_rech_paso.aivs_92_dif = arr_sol_saldo_dif.dif_aivs
      LET arr_rech_paso.pesos_92_sol = arr_sol_saldo_dif.sol_pesos
      LET arr_rech_paso.pesos_92_saldo = arr_sol_saldo_dif.saldo_pesos
      LET arr_rech_paso.pesos_92_dif = arr_sol_saldo_dif.dif_pesos
      
      LET arr_rechazados[v_indice].* = arr_rech_paso.*
      LET v_indice = v_indice + 1
   END FOREACH 

   -- Obtiene los registros rechazados de la tabla ret_disposicion_rch

   LET v_query = "\n SELECT b.f_actualiza, 'Disposición PMG', d.des_corta, j.des_corta, b.nombre_archivo, a.cod_rechazo_1, e.des_corta,  ",
                 "\n        a.cve_afore, f.afore_desc, a.nss, TRIM(a.paterno_afore) || '$' || TRIM(a.materno_afore) || '$' || TRIM(a.nombre_afore) as nombre, ",
                 "\n        a.aivs_viv92, a.aivs_viv97,        ",
                 "\n        0,0,0,0,                                                                            ",
                 "\n        round((a.aivs_viv92 * i.precio_fondo),2), round((a.aivs_viv97 * i.precio_fondo),2), ",
                 "\n        0,0,0,0,i.precio_fondo                                                                             ",
                 "\n FROM   ret_disposicion_rch a,                                ",
                 "\n        glo_ctr_archivo b,                                    ",
                 "\n        cat_proceso c,                                        ",
                 "\n        ret_tipo_retiro d,                                    ",
                 "\n        ret_rechazo e,                                        ",
                 "\n        cat_afore f,                                          ",
                 "\n        ret_matriz_derecho h,                                 ",
                 "\n        ret_cza_disposicion i,                                ",
                 "\n        ret_tipo_seguro j                                     ",
                 "\n WHERE  b.proceso_cod = 1519                                  ",
                 "\n AND    b.folio = a.folio                                     ",
                 "\n AND    c.proceso_cod = b.proceso_cod                         ",
                 "\n AND    a.tipo_retiro = h.tpo_retiro                          ",
                 "\n AND    a.regimen  = h.regimen                                ",
                 "\n AND    a.tpo_seguro = h.tpo_seguro                           ",
                 "\n AND    a.tpo_pension = h.tpo_pension                         ",
                 "\n AND    a.tpo_prestacion = h.tpo_prestacion                   ",
                 "\n AND    h.tpo_retiro = d.tpo_retiro                           ",
                 "\n AND    h.modalidad_retiro = 5                                ",
                 "\n AND    a.cod_rechazo_1 = e.cod_rechazo                       ",
                 "\n AND    a.cve_afore = f.afore_cod                             ",
                 "\n AND    a.folio = i.folio                                     ",
                 "\n AND    h.tpo_seguro = j.tpo_seguro                           "

   -- si se recibieron la fecha inicial y la fecha final como parametro
   IF ( v_f_inicial IS NOT NULL) AND (v_f_final IS NOT NULL ) THEN   
      -- agrupador
      LET v_query = v_query, "\n AND    b.f_actualiza BETWEEN '", v_f_inicial, "' AND '", v_f_final, "'"
   END IF

   IF ( v_nss IS NOT NULL ) THEN 
      LET v_query = v_query, "\n AND    a.nss = '", v_nss, "'"
   END IF 

   IF ( v_tipo_retiro IS NOT NULL ) THEN 
      LET v_query = v_query, "\n AND    h.tpo_retiro = '", v_tipo_retiro CLIPPED , "'"
   END IF 

   -- se ordenan los datos
   LET v_query = v_query, "\n ORDER BY h.tpo_retiro, a.nss"
   DISPLAY "El query \n", v_query
   -- se obtienen los registros para el reporte
   PREPARE sid_pmg_rech_rch FROM v_query
   DECLARE cur_pmg_rech_rch CURSOR FOR sid_pmg_rech_rch

   -- llena el arreglo
   FOREACH cur_pmg_rech_rch INTO arr_rech_paso.*,v_precio_valuacion
      LET arr_sol_saldo_dif.sol_aivs = arr_rech_paso.aivs_97_sol
      LET arr_sol_saldo_dif.f_valuacion = arr_rech_paso.fch_rechazo
      CALL fn_saldo_al_dia(arr_rech_paso.nss, 4, v_precio_valuacion, arr_sol_saldo_dif.*) RETURNING arr_sol_saldo_dif.*
      LET arr_rech_paso.aivs_97_saldo = arr_sol_saldo_dif.saldo_aivs
      LET arr_rech_paso.aivs_97_dif = arr_sol_saldo_dif.dif_aivs
      LET arr_rech_paso.pesos_97_sol = arr_sol_saldo_dif.sol_pesos
      LET arr_rech_paso.pesos_97_saldo = arr_sol_saldo_dif.saldo_pesos
      LET arr_rech_paso.pesos_97_dif = arr_sol_saldo_dif.dif_pesos

      LET arr_sol_saldo_dif.sol_aivs = arr_rech_paso.aivs_92_sol
      LET arr_sol_saldo_dif.f_valuacion = arr_rech_paso.fch_rechazo
      CALL fn_saldo_al_dia(arr_rech_paso.nss, 8, v_precio_valuacion, arr_sol_saldo_dif.*) RETURNING arr_sol_saldo_dif.*
      LET arr_rech_paso.aivs_92_saldo = arr_sol_saldo_dif.saldo_aivs
      LET arr_rech_paso.aivs_92_dif = arr_sol_saldo_dif.dif_aivs
      LET arr_rech_paso.pesos_92_sol = arr_sol_saldo_dif.sol_pesos
      LET arr_rech_paso.pesos_92_saldo = arr_sol_saldo_dif.saldo_pesos
      LET arr_rech_paso.pesos_92_dif = arr_sol_saldo_dif.dif_pesos
      
      LET arr_rechazados[v_indice].* = arr_rech_paso.*
      LET v_indice = v_indice + 1
   END FOREACH 

   RETURN arr_rechazados, v_indice
   
END FUNCTION 


FUNCTION fn_saldo_al_dia(p_nss, p_subcuenta, p_precio_valuacion, p_arr_sol_saldo_dif)
DEFINE p_nss       CHAR(11)
DEFINE p_subcuenta SMALLINT 
DEFINE p_precio_valuacion DECIMAL(15,5)
DEFINE p_arr_sol_saldo_dif RECORD 
         aivs_sol            DECIMAL(15,6),
         aivs_saldo          DECIMAL(15,6),
         aivs_dif            DECIMAL(15,6),
         pesos_sol           DECIMAL(12,6),
         pesos_saldo         DECIMAL(12,6),
         pesos_dif           DECIMAL(12,6),
         f_valuacion         DATE 
      END RECORD 

DEFINE v_id_derechohabiente DECIMAL(9,0)
DEFINE v_query              STRING 
DEFINE v_resultado          SMALLINT
DEFINE v_pesos              DECIMAL(12,2)
DEFINE v_precio_fondo       DECIMAL(9,6)

 
   LET p_arr_sol_saldo_dif.aivs_saldo  = 0
   LET p_arr_sol_saldo_dif.aivs_dif    = 0
   LET p_arr_sol_saldo_dif.pesos_sol   = 0
   LET p_arr_sol_saldo_dif.pesos_saldo = 0
   LET p_arr_sol_saldo_dif.pesos_dif   = 0
   LET v_pesos = 0
   LET v_precio_fondo = 0
   LET v_resultado = 1
   

   SELECT precio_fondo
   INTO   v_precio_fondo
   FROM   glo_valor_fondo
   WHERE  f_valuacion = TODAY
   AND    fondo = 11

   IF p_arr_sol_saldo_dif.aivs_sol > 0 THEN 
      LET v_id_derechohabiente = NULL 
         -- se contruye el enuncionado SQL
      LET v_query = "EXECUTE FUNCTION fn_saldo_dia(?,?,?,?)"

      -- se prepara la ejecucion del stored procedure para la integracion
      PREPARE sid_cons_saldo FROM v_query

      -- se ejecuta el stored procedure
      EXECUTE sid_cons_saldo USING p_nss, v_id_derechohabiente, p_subcuenta, p_arr_sol_saldo_dif.f_valuacion
         INTO v_resultado, p_arr_sol_saldo_dif.aivs_saldo, v_pesos
      LET v_pesos = p_arr_sol_saldo_dif.aivs_saldo * v_precio_fondo
      LET p_arr_sol_saldo_dif.pesos_sol = p_arr_sol_saldo_dif.aivs_sol * p_precio_valuacion
      IF v_resultado = 0 AND p_arr_sol_saldo_dif.aivs_saldo > 0 THEN  
         LET p_arr_sol_Saldo_dif.pesos_saldo = v_pesos
         LET p_arr_sol_saldo_dif.pesos_dif = p_arr_sol_saldo_dif.pesos_saldo - p_arr_sol_saldo_dif.pesos_sol
         LET p_arr_sol_saldo_dif.aivs_dif = p_arr_sol_saldo_dif.aivs_saldo - p_arr_sol_saldo_dif.aivs_sol
      END IF 
   END IF 

RETURN p_arr_sol_saldo_dif.aivs_sol, 
       p_arr_sol_saldo_dif.aivs_saldo,
       p_arr_sol_saldo_dif.aivs_dif,
       p_arr_sol_saldo_dif.pesos_sol, 
       p_arr_sol_saldo_dif.pesos_saldo,
       p_arr_sol_saldo_dif.pesos_dif,
       p_arr_sol_saldo_dif.f_valuacion
       
END FUNCTION 


--Función que exporta datos a un archivo
FUNCTION fn_exporta_archivo(p_arr_aceptados, p_arr_rechazados, p_exporta_opcion)
DEFINE p_arr_aceptados        DYNAMIC ARRAY OF RECORD
         nss                     CHAR(11),
         nombre                  CHAR(50),
         tpo_retiro              CHAR(1),
         tpo_seguro              CHAR(2),
         aivs_97                 DECIMAL(15,6),
         pesos_97                DECIMAL(12,2),
         aivs_92                 DECIMAL(15,6),
         pesos_92                DECIMAL(12,2),
         aceptado                CHAR(1),
         afore                   CHAR(30)
       END RECORD
DEFINE p_arr_rechazados       DYNAMIC ARRAY OF RECORD
         fch_rechazo             DATE,
         modalidad               CHAR(40),
         tpo_retiro              CHAR(40),
         tpo_seguro              CHAR(30),
         mascara                 CHAR(40),
         cod_rechazo             SMALLINT,
         desc_rechazo            CHAR(40),
         cve_afore               CHAR(3),
         desc_afore              CHAR(40),
         nss                     CHAR(11),
         nombre                  CHAR(50),
         aivs_92_sol             DECIMAL(15,6),
         aivs_97_sol             DECIMAL(15,6),
         aivs_92_saldo           DECIMAL(15,6),
         aivs_97_saldo           DECIMAL(15,6),
         aivs_92_dif             DECIMAL(15,6),
         aivs_97_dif             DECIMAL(15,6),
         pesos_92_sol            DECIMAL(12,2),
         pesos_97_sol            DECIMAL(12,2),
         pesos_92_saldo          DECIMAL(12,2),
         pesos_97_saldo          DECIMAL(12,2),
         pesos_92_dif            DECIMAL(12,2),
         pesos_97_dif            DECIMAL(12,2)
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
      v_contador              INTEGER

   LET v_regresa = FALSE
   
   -- se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio
   INTO   v_c_ruta_env 
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- las extensiones del archivo son csv para el detalle
   LET v_extension_txt = ".csv"
   LET v_hora = CURRENT HOUR TO SECOND

   -- Se genera el nombre del archivo de Rechazos
   LET v_nom_archivo = "Consulta_Rechazos_Retiros", TODAY USING "yyyymmdd", "_", v_hora[1,2], v_hora[4,5], v_hora[7,8]
   LET v_archivo_txt = v_nom_archivo, v_extension_txt

   -- El archivo con ruta destino que contiene el detalle de Rechazados
   LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se genera el nombre del archivo de Aceptados
   LET v_nom_archivo = "Consulta_Aceptados_Retiros", TODAY USING "yyyymmdd", "_", v_hora[1,2], v_hora[4,5], v_hora[7,8]
   LET v_archivo_txt = v_nom_archivo, v_extension_txt

   -- El archivo con ruta destino que contiene el detalle de Aceptados
   LET v_v_ruta_nomarch2 = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se muestra mensaje
   IF p_exporta_opcion = 2 THEN 
      LET v_mensaje_archivo = "Se generará el archivo:\n\n\t", v_v_ruta_nomarch
   ELSE 
      IF p_exporta_opcion = 1 THEN 
         LET v_mensaje_archivo = "Se generará el archivos:\n\n\t", v_v_ruta_nomarch2
      ELSE 
         LET v_mensaje_archivo = "Se generaran los siguientes archivos:\n\n\t", v_v_ruta_nomarch, "\n\t", v_v_ruta_nomarch2
      END IF 
   END IF 
   
   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
   -- nombre de archivo generado
   DISPLAY "~~~~~~~~~~~"
   DISPLAY "Archivo de rechazados generado: ", v_v_ruta_nomarch

   IF p_exporta_opcion = 2 OR p_exporta_opcion = 3 THEN 
      -- Se crea el manejador de archivo
      LET v_ch_arch_ret_generico = base.Channel.create()
      CALL v_ch_arch_ret_generico.setDelimiter(NULL)

      -- Se crea archivo y se indica que se escribira en el mismo
      CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )
      -- Escribe el encabezado del archivo de Rechazados
      LET v_s_detalle = "FECHA RECHAZO|TIPO ARCHIVO|TIPO RETIRO / TRANSFERENCIA|TIPO SEGURO|MASCARA ARCHIVO|CVE RECHAZO|",
                        "DESCRIPCION RECHAZO|CVE AFORE|AFORE|NSS|NOMBRE DEL TRABAJADOR|",
                        "IMPORTE SOLICITADO 92 AIVS|IMPORTE SOLICITADO 97 AIVS|SALDO 92 AIVS|SALDO 97 AIVS|",
                        "DIFERENCIA 92 AIVS|DIFERENCIA 97 AVIS|IMPORTE SOLICITADO 92 MXN|IMPORTE SOLICITADO 97 MXN|",
                        "SALDO 92 MXN|SALDO 97 MXN|DIFERENCIA 92 MXN|DIFERENCIA 97 MXN"

      CALL v_ch_arch_ret_generico.write(v_s_detalle)

      -- Ahora imprimo todos los registros Rechazados


      FOR v_contador = 1 TO p_arr_rechazados.getLength()
         LET v_s_detalle = p_arr_rechazados[v_contador].fch_rechazo USING "dd/mm/yyyy","|",
                           p_arr_rechazados[v_contador].modalidad CLIPPED, "|",
                           p_arr_rechazados[v_contador].tpo_retiro, "|",
                           p_arr_rechazados[v_contador].tpo_seguro, "|",
                           p_arr_rechazados[v_contador].mascara CLIPPED, "|",
                           p_arr_rechazados[v_contador].cod_rechazo, "|",
                           p_arr_rechazados[v_contador].desc_rechazo CLIPPED, "|",
                           p_arr_rechazados[v_contador].cve_afore, "|",
                           p_arr_rechazados[v_contador].desc_afore CLIPPED, "|",
                           p_arr_rechazados[v_contador].nss, "|",
                           p_arr_rechazados[v_contador].nombre CLIPPED, "|",
                           p_arr_rechazados[v_contador].aivs_92_sol USING "<<<<<<<<<<<<<&.&&&&&&","|",
                           p_arr_rechazados[v_contador].aivs_97_sol USING "<<<<<<<<<<<<<&.&&&&&&","|",
                           p_arr_rechazados[v_contador].aivs_92_saldo USING "<<<<<<<<<<<<<&.&&&&&&","|",
                           p_arr_rechazados[v_contador].aivs_97_saldo USING "<<<<<<<<<<<<<&.&&&&&&","|",
                           p_arr_rechazados[v_contador].aivs_92_dif USING "<<<<<<<<<<<<<&.&&&&&&","|",
                           p_arr_rechazados[v_contador].aivs_97_dif USING "<<<<<<<<<<<<<&.&&&&&&","|",
                           p_arr_rechazados[v_contador].pesos_92_sol USING "<<<<<<<<<<<<<&.&&","|",
                           p_arr_rechazados[v_contador].pesos_97_sol USING "<<<<<<<<<<<<<&.&&","|",
                           p_arr_rechazados[v_contador].pesos_92_saldo USING "<<<<<<<<<<<<<&.&&","|",
                           p_arr_rechazados[v_contador].pesos_97_saldo USING "<<<<<<<<<<<<<&.&&","|",
                           p_arr_rechazados[v_contador].pesos_92_dif USING "<<<<<<<<<<<<<&.&&","|",
                           p_arr_rechazados[v_contador].pesos_97_dif USING "<<<<<<<<<<<<<&.&&","|"
                           
         CALL v_ch_arch_ret_generico.write(v_s_detalle)
         --LET v_contador = v_contador + 1
      END FOR 

      -- Se cierra el archivo de Rechazados
      CALL v_ch_arch_ret_generico.close()
   END IF 

   IF p_exporta_opcion = 1 OR p_exporta_opcion = 3 THEN 

      -- Se crea el manejador de archivo
      LET v_ch_arch_ret_generico = base.Channel.create()
      CALL v_ch_arch_ret_generico.setDelimiter(NULL)

      -- Se crea archivo y se indica que se escribira en el mismo
      CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch2, "w" )
      -- Escribe el encabezado del archivo de Aceptados
      LET v_s_detalle = "NSS|NOMBRE|TIPO RETIRO|TIPO SEGURO|AIVS 97|PESOS 97|AIVS 92|PESOS 92|ACEPTADO|AFORE"

      CALL v_ch_arch_ret_generico.write(v_s_detalle)

      -- Ahora imprimo todos los registros Aceptados

      FOR v_contador = 1 TO p_arr_aceptados.getLength()
         LET v_s_detalle = p_arr_aceptados[v_contador].nss, "|",
                           p_arr_aceptados[v_contador].nombre CLIPPED, "|",
                           p_arr_aceptados[v_contador].tpo_retiro, "|",
                           p_arr_aceptados[v_contador].tpo_seguro, "|",
                           p_arr_aceptados[v_contador].aivs_97 USING "<<<<<<<<<<<<<&.&&&&&&","|",
                           p_arr_aceptados[v_contador].pesos_97 USING "<<<<<<<<<<<<<&.&&","|",
                           p_arr_aceptados[v_contador].aivs_92 USING "<<<<<<<<<<<<<&.&&&&&&","|",
                           p_arr_aceptados[v_contador].pesos_92 USING "<<<<<<<<<<<<<&.&&","|",
                           "1|",
                           p_arr_aceptados[v_contador].afore CLIPPED, "|"
         CALL v_ch_arch_ret_generico.write(v_s_detalle)
      END FOR 
   END IF 

   -- Se cierra el archivo de Aceptados
   CALL v_ch_arch_ret_generico.close()
   IF p_exporta_opcion = 2 THEN 
      LET v_mensaje_archivo = "Archivos generado exitosamente:\n\n\t", v_v_ruta_nomarch
   ELSE 
      IF p_exporta_opcion = 1 THEN 
         LET v_mensaje_archivo = "Archivos generado exitosamente:\n\n\t", v_v_ruta_nomarch2
      ELSE 
         LET v_mensaje_archivo = "Los archivos fueron generados exitosamente:\n\n\t", v_v_ruta_nomarch, "\n\t", v_v_ruta_nomarch2
      END IF 
   END IF 

   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")

   RETURN v_regresa
   
END FUNCTION
