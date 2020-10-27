--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 04/10/2012
--==============================================================================

################################################################################
#Proyecto     => SAFRE VIVIENDA                                                #
#Propietario  => E.F.P.                                                        #
#Modulo       => SEP                                                           #
#Programa     => SEPF60                                                        #
#Objetivo     => Captura de montos a separar para expedientes de               #
#                solo infonavit                                                #
#Autor        => Hugo César Ramírez García                                     #
#Fecha Inicio => 04 de Octubre de 2012                                         #
################################################################################
DATABASE safre_viv
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   SMALLINT,
       p_titulo_ventana  STRING,
       v_ventana         ui.Window,
       v_forma           ui.Form,
       v_expediente DYNAMIC ARRAY OF RECORD
          v_id_expediente LIKE sep_expediente.id_expediente,
          v_caso_adai     LIKE sep_expediente.caso_adai,
          v_flujo_desc    LIKE sep_cat_tipo_flujo.flujo_desc,
          v_f_captura     LIKE sep_expediente.f_captura,
          v_canal_desc    LIKE sep_cat_canal_recepcion_exp.canal_desc,
          v_acreditado    LIKE sep_nss_expediente.nss,
          v_trabajador    LIKE sep_nss_expediente.nss
       END RECORD,
       v_expediente_sep_inf DYNAMIC ARRAY OF RECORD
          v_num           INTEGER,
          v_id_expediente LIKE sep_expediente.id_expediente,
          v_invadido      LIKE sep_nss_expediente.nss,
          v_asociado      LIKE sep_nss_expediente.nss,
          --v_estado        LIKE sep_expediente.estado
          v_estado        LIKE sep_estado_expediente.descripcion
       END RECORD,
       v_datos_expediente RECORD
          v_caso_adai     LIKE sep_expediente.caso_adai,
          v_id_expediente LIKE sep_expediente.id_expediente,
          v_invadido      LIKE sep_nss_expediente.nss,
          v_asociado      LIKE sep_nss_expediente.nss,
          v_estado        LIKE sep_estado_expediente.descripcion
       END RECORD,
       v_montos DYNAMIC ARRAY OF RECORD
          v_num INTEGER,
          v_id_expediente    LIKE sep_expediente.id_expediente,
          v_ind_mov_asociado SMALLINT,
          v_subcuenta_cod    LIKE cta_movimiento.subcuenta,
          v_subcuenta        LIKE cat_subcuenta.subcuenta_corta,
          v_movimiento_cod   LIKE cta_movimiento.movimiento,
          v_movimiento       LIKE cat_movimiento.movimiento_desc,
          v_nrp              LIKE cta_his_pagos.nrp,
          v_periodo_pago     LIKE cta_his_pagos.periodo_pago,
          v_bimestre         CHAR(6),
          v_pesos            LIKE cta_movimiento.monto_pesos,
          v_aivs             LIKE cta_movimiento.monto_acciones,
          v_aivs_separar     LIKE cta_movimiento.monto_acciones,
          v_nss_asociado     LIKE sep_nss_expediente.nss,
          v_id_sep_mov_inv   DECIMAL(9,0),
          v_f_valor          LIKE sep_movimiento_invadido.f_valor
       END RECORD,
       v_nom_reporte     STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin
       
MAIN

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tpo_ejecucion  = ARG_VAL(2)
   LET p_titulo_ventana = ARG_VAL(3)

   CALL fn_consulta_expedientes_infonavit()

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF60                                                   #
#Descripcion       => separación de montos de expedientes de solo infonavit    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 04 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_consulta_expedientes_infonavit()
DEFINE v_filtro        STRING,
       v_continua      BOOLEAN,
       v_consulta      BOOLEAN,
       v_id_expediente LIKE sep_expediente.id_expediente

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "sep"

   OPEN WINDOW vtna_consulta_expedientes WITH FORM v_ruta_ejecutable CLIPPED||"/SEPF601"
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_titulo_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo_ventana)         
         CALL v_ventana.setText(p_titulo_ventana)
      END IF
      LET v_continua = TRUE
      WHILE v_continua
         MENU #Pantalla principal
            ON ACTION Nuevo
               # captura criterios de busqueda
               CALL fn_recupera_filtro_expedientes()RETURNING v_filtro,v_consulta
               IF(v_consulta)THEN
                  # recupera la información relaciónada al filtro
                  CALL fn_recupera_expedientes(v_filtro)
                  IF(v_expediente.getLength() > 0)THEN
                     # pantalla de seleccion de expediente para nuevo
                     CALL fn_selecciona_expediente() RETURNING v_id_expediente
                     IF(v_id_expediente IS NOT NULL)THEN
                        # mustra pantalla de seleccion de montos
                        CALL fn_separa_saldos(v_id_expediente,"N")
                     END IF
                  ELSE
                     CALL fn_mensaje("AVISO","No se encontraron registros con criterio dado","information")
                  END IF
               END IF
               CALL v_expediente.clear()
               EXIT MENU

            ON ACTION Recuperar
               # Pantalla de seleccion de exoedietes previo guardado
               CALL fn_rec_expediente_separacion("R") RETURNING v_id_expediente
               IF(v_id_expediente IS NOT NULL)THEN
                  # mustra pantalla de seleccion de montos
                  CALL fn_separa_saldos(v_id_expediente,"R")
               END IF
               EXIT MENU

            ON ACTION Consultar
               # Pantalla de seleccion de expedietes previo guardado
               CALL fn_rec_expediente_separacion("C") RETURNING v_id_expediente
               IF(v_id_expediente IS NOT NULL)THEN
                  # pantalla de consulta de separacion de montos
                  CALL fn_consulta_saldos(v_id_expediente,"C")
               END IF
               EXIT MENU

            ON ACTION CANCEL
               LET v_continua = FALSE
               EXIT MENU

         END MENU
            
      END WHILE
      

   CLOSE WINDOW vtna_consulta_expedientes

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF60                                                   #
#Descripcion       => Recupera el filtro para los expedientes                  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 04 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_recupera_filtro_expedientes()
DEFINE v_filtro    STRING,
       v_consulta  BOOLEAN,
       v_cb_estado ui.ComboBox

   # filtrado de búsqueda
   CONSTRUCT BY NAME v_filtro ON exp.caso_adai,
                                 exp.id_expediente,
                                 nss.nss,
                                 nss.tipo_nss,
                                 exp.estado ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)
      
      BEFORE CONSTRUCT
         LET v_cb_estado = ui.ComboBox.forName("formonly.estado")
         # Función para recuperar los estados del expedietne y llenar el combo
         CALL fn_llena_cb_estado(v_cb_estado)
         INITIALIZE v_filtro TO NULL
         LET v_consulta = FALSE 
         
      ON ACTION aceptar
         LET v_consulta = TRUE
         ACCEPT CONSTRUCT

      ON ACTION cancelar
         LET v_consulta = FALSE
         EXIT CONSTRUCT
         
   END CONSTRUCT
   
   RETURN v_filtro,v_consulta
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF60                                                   #
#Descripcion       => Funcion para llenar combo de estado de expediente        #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 11 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_llena_cb_estado(p_cb_estado)
DEFINE p_cb_estado   ui.ComboBox, 
       v_consulta    STRING,
       v_estado      LIKE sep_estado_expediente.estado,
       v_descripcion LIKE sep_estado_expediente.descripcion

   CALL p_cb_estado.clear()
   LET v_consulta = "\n SELECT estado, descripcion",
                    "\n   FROM sep_estado_expediente",
                    "\n  WHERE 1 = 1"
   PREPARE prp_rec_estados_expediente FROM v_consulta
   DECLARE cur_rec_estados_expediente CURSOR FOR prp_rec_estados_expediente
   FOREACH cur_rec_estados_expediente INTO v_estado,v_descripcion
      CALL p_cb_estado.addItem(v_estado,v_descripcion)
   END FOREACH

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF60                                                   #
#Descripcion       => Recupera el filtro para los expedientes                  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 04 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_recupera_expedientes(p_filtro)
DEFINE p_filtro   STRING,
       v_consulta STRING,
       v_expediente_aux RECORD
          v_id_expediente LIKE sep_expediente.id_expediente,
          v_caso_adai     LIKE sep_expediente.caso_adai,
          v_flujo_cod     LIKE sep_expediente.flujo_cod,
          v_flujo_desc    LIKE sep_cat_tipo_flujo.flujo_desc,
          v_f_captura     LIKE sep_expediente.f_captura,
          v_canal_cod     LIKE sep_expediente.canal_cod,
          v_canal_desc    LIKE sep_cat_canal_recepcion_exp.canal_desc
       END RECORD,
       v_tipo_nss LIKE sep_nss_expediente.tipo_nss,
       v_indice   INTEGER

   LET v_indice = 1
   CALL v_expediente.clear()
   # recupera los nss del expediente
   LET v_consulta = "\n SELECT nss",
                    "\n   FROM sep_nss_expediente",
                    "\n  WHERE id_expediente = ?",
                    "\n    AND tipo_nss = ?"
   PREPARE prp_rec_nss FROM v_consulta 
   
   # recupera los expedientes de solo infonavit para separación de montos
   LET v_consulta = "\n SELECT DISTINCT exp.id_expediente,",
                    "\n        exp.caso_adai,",
                    "\n        exp.flujo_cod,",
                    "\n        flu.flujo_desc,",
                    "\n        exp.f_captura,",
                    "\n        exp.canal_cod,",
                    "\n        cnl.canal_desc",
                    "\n   FROM sep_expediente exp JOIN sep_cat_tipo_flujo flu",
                    "\n     ON exp.flujo_cod = flu.flujo_cod",
                    "\n        JOIN sep_cat_canal_recepcion_exp cnl",
                    "\n     ON exp.canal_cod = cnl.canal_cod",
                    "\n        JOIN sep_nss_expediente nss",
                    "\n     ON nss.id_expediente = exp.id_expediente",
                    "\n  WHERE exp.estado = 45",   # Restitución cargada
                    "\n    AND exp.flujo_cod = 3", # Solo infonavit
                    "\n    AND ",p_filtro
   PREPARE prp_rec_expedientes FROM v_consulta
   DECLARE cur_rec_expedientes CURSOR FOR prp_rec_expedientes
   FOREACH cur_rec_expedientes INTO v_expediente_aux.*
      LET v_expediente[v_indice].v_id_expediente = v_expediente_aux.v_id_expediente
      LET v_expediente[v_indice].v_caso_adai     = v_expediente_aux.v_caso_adai
      LET v_expediente[v_indice].v_canal_desc    = v_expediente_aux.v_canal_desc
      LET v_expediente[v_indice].v_flujo_desc    = v_expediente_aux.v_flujo_desc
      LET v_expediente[v_indice].v_f_captura     = v_expediente_aux.v_f_captura

      LET v_tipo_nss = 1
      EXECUTE prp_rec_nss USING v_expediente_aux.v_id_expediente,
                                v_tipo_nss
                           INTO v_expediente[v_indice].v_acreditado

      LET v_tipo_nss = 2
      EXECUTE prp_rec_nss USING v_expediente_aux.v_id_expediente,
                                v_tipo_nss
                           INTO v_expediente[v_indice].v_trabajador

      LET v_indice = v_indice + 1
   END FOREACH 
   FREE cur_rec_expedientes
   
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF60                                                   #
#Descripcion       => Recupera expediente a separar montos                     #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 04 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_selecciona_expediente()
DEFINE v_id_expediente LIKE sep_expediente.id_expediente 

   # seleccion de expediente
   DISPLAY ARRAY v_expediente TO tbl_expediente.* 
      ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)

      BEFORE DISPLAY
         DISPLAY v_expediente.getLength() TO tot_expedientes
         INITIALIZE v_id_expediente TO NULL

      ON ACTION aceptar
         LET v_id_expediente = v_expediente[ARR_CURR()].v_id_expediente
         CALL v_expediente.clear()  
         DISPLAY " " TO tot_expedientes
         ACCEPT DISPLAY

      ON ACTION cancelar
         CALL v_expediente.clear()
         INITIALIZE v_id_expediente TO NULL
         DISPLAY " " TO tot_expedientes
         EXIT DISPLAY

   END DISPLAY

   RETURN v_id_expediente
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF60                                                   #
#Descripcion       => Recupera expediente a separar montos                     #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 04 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_rec_expediente_separacion(p_tpo_consulta)
DEFINE v_id_expediente LIKE sep_expediente.id_expediente,
       p_tpo_consulta  CHAR(1)

   # seleccion de expedientes
   OPEN WINDOW vtna_rec_expediente_sep WITH FORM v_ruta_ejecutable CLIPPED||"/SEPF602"

      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_titulo_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo_ventana)
         CALL v_ventana.setText(p_titulo_ventana)
      END IF
      # recupera los expedientes previos guardados
      CALL fn_con_expedientes_separacion_infonavit(p_tpo_consulta)
      IF( v_expediente_sep_inf.getLength() > 0 )THEN
         # selecciona el expediente
         DISPLAY ARRAY v_expediente_sep_inf TO tbl_expedientes_infonavit.* 
            ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)

            BEFORE DISPLAY
               INITIALIZE v_id_expediente TO NULL
               CALL FGL_SET_ARR_CURR( 1 )

            ON ACTION aceptar
               LET v_id_expediente = v_expediente_sep_inf[ARR_CURR()].v_id_expediente
               ACCEPT DISPLAY

            ON ACTION cancelar
               INITIALIZE v_id_expediente TO NULL
               EXIT DISPLAY

         END DISPLAY
      ELSE
         CALL fn_mensaje("AVISO","No se encontraron registros","information")
      END IF
      
   CLOSE WINDOW vtna_rec_expediente_sep

   RETURN v_id_expediente
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF60                                                   #
#Descripcion       => Recupera expediente a separar montos                     #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 04 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_con_expedientes_separacion_infonavit(p_tpo_consulta)
DEFINE v_consulta     STRING,
       p_tpo_consulta CHAR(1),
       v_expediente_sep_inf_aux RECORD
          v_id_expediente LIKE sep_expediente.id_expediente,
          --v_estado        LIKE sep_expediente.estado
          v_estado        LIKE sep_estado_expediente.descripcion
       END RECORD,
       v_tpo_nss      LIKE sep_nss_expediente.nss,
       v_indice       INTEGER,
       v_tpo_filtro   STRING

   LET v_indice = 1
   CALL v_expediente_sep_inf.clear()
   # recupera nss de expediente
   LET v_consulta = "\n SELECT nss",
                    "\n   FROM sep_nss_expediente",
                    "\n  WHERE id_expediente = ?",
                    "\n    AND tipo_nss = ?"
   PREPARE prp_rec_nss_sep_inf FROM v_consulta 
   # contruye consulta dependiendo de quien lo llame
   CASE p_tpo_consulta   
      WHEN "R"
         LET v_tpo_filtro = "\n  WHERE exp.estado = 70" # Previo Registrado

      WHEN "C"
         LET v_tpo_filtro = "\n  WHERE exp.estado IN (75,80,85)" # Previo confimrado, Previo Preliquidado, Previo Liquidado

   END CASE
   # recupera los expedientes en separación de montos
   LET v_consulta = "\n SELECT exp.id_expediente,",
                    "\n        edo.descripcion",
                    "\n   FROM sep_expediente exp LEFT OUTER JOIN sep_estado_expediente edo",
                    "\n     ON exp.estado = edo.estado",
                    v_tpo_filtro,
                    "\n    AND exp.flujo_cod = 3"
   PREPARE prp_rec_expedientes_sep_inf FROM v_consulta
   DECLARE cur_rec_expedientes_sep_inf CURSOR FOR prp_rec_expedientes_sep_inf
   FOREACH cur_rec_expedientes_sep_inf INTO v_expediente_sep_inf_aux.*
      LET v_expediente_sep_inf[v_indice].v_num           = v_indice
      LET v_expediente_sep_inf[v_indice].v_id_expediente = v_expediente_sep_inf_aux.v_id_expediente
      LET v_expediente_sep_inf[v_indice].v_estado        = v_expediente_sep_inf_aux.v_estado

      LET v_tpo_nss = 1
      EXECUTE prp_rec_nss_sep_inf USING v_expediente_sep_inf_aux.v_id_expediente,
                                        v_tpo_nss
                                   INTO v_expediente_sep_inf[v_indice].v_invadido

      LET v_tpo_nss = 2
      EXECUTE prp_rec_nss_sep_inf USING v_expediente_sep_inf_aux.v_id_expediente,
                                        v_tpo_nss
                                   INTO v_expediente_sep_inf[v_indice].v_asociado
                                        
      LET v_indice = v_indice + 1
   END FOREACH 
   FREE cur_rec_expedientes_sep_inf

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF60                                                   #
#Descripcion       => Pantalla para la separación de saldos                    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 05 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_separa_saldos(p_id_expediente,p_tpo_accion)
DEFINE p_id_expediente        LIKE sep_expediente.id_expediente,
       p_tpo_accion           CHAR(1),
       r_existen_registros    BOOLEAN,
       v_monto_total_acciones DECIMAL(16,6),
       v_seleccionado         BOOLEAN,
       v_senial               SMALLINT,
       v_genero_rpt           BOOLEAN,
       r_exp_avanzado         BOOLEAN,
       r_confirma             BOOLEAN,
       v_edo_expediente       LIKE sep_expediente.estado--,
       --v_nom_aux_pdf          STRING

   # Separación de montos
   OPEN WINDOW vtna_separa_saldos WITH FORM v_ruta_ejecutable CLIPPED||"/SEPF603"

      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_titulo_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo_ventana)
         CALL v_ventana.setText(p_titulo_ventana)
      END IF

      INPUT ARRAY v_montos WITHOUT DEFAULTS FROM tbl_movimientos.*
         ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE, AUTO APPEND = FALSE, UNBUFFERED)

         BEFORE INPUT
            # funcion que recupera los registros de movimientos del invadido de cta_movimiento
            CALL fn_recupera_montos(p_id_expediente,p_tpo_accion)RETURNING r_existen_registros
            IF NOT(r_existen_registros)THEN
               CALL fn_mensaje("AVISO","No se encontraron registros","information")
               EXIT INPUT
            END IF
            # imprime datos generales expediente
            DISPLAY v_datos_expediente.v_caso_adai     TO caso_adai
            DISPLAY v_datos_expediente.v_id_expediente TO id_expediente 
            DISPLAY v_datos_expediente.v_invadido      TO invadido
            DISPLAY v_datos_expediente.v_asociado      TO asociado
            DISPLAY v_datos_expediente.v_estado       TO desc_estado
            
            LET v_monto_total_acciones = 0
            DISPLAY v_monto_total_acciones TO mto_total_sep
            DISPLAY " " TO reporte

         ON CHANGE aivs_separar
            CALL GET_FLDBUF(aivs_separar) RETURNING v_montos[ARR_CURR()].v_aivs_separar
            IF(v_montos[ARR_CURR()].v_aivs_separar > v_montos[ARR_CURR()].v_aivs)THEN
               CALL fn_mensaje("AVISO","'Aivs separar' excede monto aivs","information")
               LET v_montos[ARR_CURR()].v_aivs_separar = 0
               NEXT FIELD aivs_separar 
            END IF

         BEFORE FIELD aivs_separar
            CALL GET_FLDBUF(ind_movi_asociado) RETURNING v_montos[ARR_CURR()].v_ind_mov_asociado
            IF(v_montos[ARR_CURR()].v_movimiento_cod <> 999 OR # 999 saldo inicial
               v_montos[ARR_CURR()].v_ind_mov_asociado <> 1)THEN
               NEXT FIELD ind_movi_asociado
            END IF
         
         ON CHANGE ind_movi_asociado
            CALL GET_FLDBUF(ind_movi_asociado) RETURNING v_montos[ARR_CURR()].v_ind_mov_asociado
            
            IF(v_montos[ARR_CURR()].v_ind_mov_asociado = 1 AND 
               v_montos[ARR_CURR()].v_movimiento_cod <> 999)THEN # 999 saldo inicial
               LET v_montos[ARR_CURR()].v_aivs_separar = v_montos[ARR_CURR()].v_aivs
               
               CONTINUE INPUT  
            END IF
            IF(v_montos[ARR_CURR()].v_ind_mov_asociado = 0)THEN
               LET v_montos[ARR_CURR()].v_aivs_separar = 0
            END IF

         ON ACTION guardar
            CALL GET_FLDBUF(aivs_separar) RETURNING v_montos[ARR_CURR()].v_aivs_separar
            IF(v_montos[ARR_CURR()].v_aivs_separar > v_montos[ARR_CURR()].v_aivs)THEN
               CALL fn_mensaje("AVISO","'Aivs separar' excede monto aivs","information")
               NEXT FIELD aivs_separar 
            END IF
            # funcion que verifica si se ha seleccionado algun check
            CALL fn_verifica_seleccion() RETURNING v_seleccionado
            IF(v_seleccionado)THEN
               CALL fn_ventana_confirma(p_titulo_ventana,"Guardar previo?","question") RETURNING r_confirma
               IF(r_confirma)THEN
                  --DISPLAY "aivs:",v_montos[ARR_CURR()].v_aivs_separar
                  # llama funcion que actualiza la información            
                  CALL fn_actualiza_registros_movimientos()
                  SELECT estado
                    INTO v_edo_expediente
                    FROM sep_expediente
                   WHERE id_expediente = p_id_expediente
                  IF(v_edo_expediente <> 70)THEN # no ejecuta maquinaria si el expediente ya esta en guardado previo
                     # señal guardar previo
                     LET v_senial = 70
                     CALL fn_avanza_edo_expediente(p_id_expediente,v_senial) RETURNING r_exp_avanzado
                  END IF
                  IF(r_exp_avanzado)THEN
                     CALL fn_mensaje("AVISO","Guardado realizado correctamente","information")
                     ACCEPT INPUT
                  ELSE
                     CONTINUE INPUT
                  END IF
               ELSE
                  CONTINUE INPUT
               END IF
            ELSE
               CALL fn_mensaje("AVISO","No se ha seleccionado ningún registro","information")
               CONTINUE INPUT
            END IF
            
            
         ON ACTION aceptar
            CALL GET_FLDBUF(aivs_separar) RETURNING v_montos[ARR_CURR()].v_aivs_separar
            IF(v_montos[ARR_CURR()].v_aivs_separar > v_montos[ARR_CURR()].v_aivs)THEN
               CALL fn_mensaje("AVISO","'Aivs separar' excede monto aivs","information")
               NEXT FIELD aivs_separar 
            END IF
            # funcion que verifica si se ha seleccionado algun check
            CALL fn_verifica_seleccion() RETURNING v_seleccionado
            IF(v_seleccionado)THEN
               CALL fn_ventana_confirma(p_titulo_ventana,"Confirmar previo?","question") RETURNING r_confirma
               IF(r_confirma)THEN
                  # llama funcion que actualiza la información
                  CALL fn_actualiza_registros_movimientos()
                  # señal confirmar previo
                  LET v_senial = 75
                  CALL fn_avanza_edo_expediente(p_id_expediente,v_senial) RETURNING r_exp_avanzado
                  IF(r_exp_avanzado)THEN
                     CALL fn_mensaje("AVISO","Confirmación realizada correctamente","information")
                     ACCEPT INPUT
                  ELSE
                     CONTINUE INPUT
                  END IF
               ELSE
                  CONTINUE INPUT
               END IF
            ELSE
               CALL fn_mensaje("AVISO","No se ha seleccionado ningún registro","information")
               CONTINUE INPUT
            END IF

         ON ACTION calcular
            CALL GET_FLDBUF(aivs_separar) RETURNING v_montos[ARR_CURR()].v_aivs_separar
            IF(v_montos[ARR_CURR()].v_aivs_separar > v_montos[ARR_CURR()].v_aivs)THEN
               CALL fn_mensaje("AVISO","'Aivs separar' excede monto aivs","information")
               NEXT FIELD aivs_separar 
            END IF
            # raliza sumatoria de aivs para recuperar total
            CALL fn_calculo_monto_separacion()RETURNING v_monto_total_acciones
            # genera reporte de movimientos actuales
            CALL fn_genera_reporte(p_id_expediente) RETURNING v_genero_rpt
            IF(v_genero_rpt)THEN
               DISPLAY v_monto_total_acciones TO mto_total_sep
               --LET v_nom_aux_pdf = v_nom_reporte,".pdf"
               DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte||"','sep')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO reporte
            ELSE
               LET v_monto_total_acciones = 0
               DISPLAY v_monto_total_acciones TO mto_total_sep
            END IF

         ON ACTION eliminar
            CALL fn_ventana_confirma(p_titulo_ventana,"Eliminar previo?","question") RETURNING r_confirma
            IF(r_confirma)THEN
               # felimina los registros de sep_movimiento_invadido y sep_saldo_inicial para el expediente
               CALL fn_elimina_reg_sep_mov(p_id_expediente)
               # señal REVERSAR PREVIO GUARDADO
               LET v_senial = 90
               CALL fn_avanza_edo_expediente(p_id_expediente,v_senial) RETURNING r_exp_avanzado
               IF(r_exp_avanzado)THEN
                  CALL fn_mensaje("AVISO","Previo eliminado correctamente","information")
                  ACCEPT INPUT
               ELSE
                  CONTINUE INPUT
               END IF
            END IF

         ON ACTION cancelar
            # solo para el caso en que la accion es nuevo y se cancela la separación, se eliminan los registros que se 
            # depositaron en sep_movimiento_invadido para que no haya datos basura (para la accion recuperar, no importa, ya que solo se actualizó los movimientos)
            IF(p_tpo_accion = 'N')THEN
               CALL fn_elimina_reg_sep_mov(p_id_expediente)
            END IF
            EXIT INPUT

      END INPUT

   CLOSE WINDOW vtna_separa_saldos

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF60                                                   #
#Descripcion       => Pantalla para la consulta de separacion de saldos        #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 08 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_consulta_saldos(p_id_expediente,p_tpo_accion)
DEFINE p_id_expediente        LIKE sep_expediente.id_expediente,
       p_tpo_accion           CHAR(1),
       r_existen_registros    BOOLEAN,
       v_genero_rpt           BOOLEAN,
       v_monto_total_acciones DECIMAL(16,6)

   # Pantalla de separación de montos en modo consulta
   OPEN WINDOW vtna_separa_saldos WITH FORM v_ruta_ejecutable CLIPPED||"/SEPF603"

      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_titulo_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo_ventana)
         CALL v_ventana.setText(p_titulo_ventana)
      END IF

      DISPLAY ARRAY v_montos TO tbl_movimientos.*
         ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

         BEFORE DISPLAY
            # funcion que recupera los registros de movimientos del invadido de cta_movimiento
            CALL fn_recupera_montos(p_id_expediente,p_tpo_accion)RETURNING r_existen_registros
            IF NOT(r_existen_registros)THEN
               CALL fn_mensaje("AVISO","No se encontraron registros","information")
               EXIT DISPLAY
            END IF
            DISPLAY v_datos_expediente.v_caso_adai     TO caso_adai
            DISPLAY v_datos_expediente.v_id_expediente TO id_expediente
            DISPLAY v_datos_expediente.v_invadido      TO invadido
            DISPLAY v_datos_expediente.v_asociado      TO asociado
            DISPLAY v_datos_expediente.v_estado        TO desc_estado
            DISPLAY " " TO reporte
            DISPLAY 0 TO mto_total_sep

         ON ACTION calcular
            # raliza sumatoria de aivs para recuperar total
            CALL fn_calculo_monto_separacion()RETURNING v_monto_total_acciones
            # genera reporte de movimientos actuales
            CALL fn_genera_reporte(p_id_expediente) RETURNING v_genero_rpt
            IF(v_genero_rpt)THEN
               DISPLAY v_monto_total_acciones TO mto_total_sep
               DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte||"','sep')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO reporte
            ELSE
               LET v_monto_total_acciones = 0
               DISPLAY v_monto_total_acciones TO mto_total_sep
            END IF

         ON ACTION cancelar
            EXIT DISPLAY

      END DISPLAY

   CLOSE WINDOW vtna_separa_saldos

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF60                                                   #
#Descripcion       => Funcion para actualizar el estado del expediente         #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 08 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_avanza_edo_expediente(p_id_expediente,p_senial)
DEFINE p_id_expediente LIKE sep_expediente.id_expediente,
       v_consulta      STRING,
       p_senial        SMALLINT,
       r_estado        SMALLINT,
       r_diag          CHAR(3),
       r_edo_destino   SMALLINT

   # Sp para avanzar maquinaria de estados del expediente
   LET v_consulta = "EXECUTE FUNCTION safre_viv:fn_maquinaria_individual(?,?,?,?,?)"
   PREPARE prp_act_edo_expediente FROM v_consulta
   EXECUTE prp_act_edo_expediente USING 'maq_sep_expediente',
                                        p_id_expediente,
                                        "id_expediente",
                                        p_senial, 
                                        p_usuario_cod
                                   INTO r_estado, 
                                        r_diag, 
                                        r_edo_destino
   IF(r_estado <> 0)THEN
      CALL fn_mensaje("AVISO","Ocurrió un error al avanzar maquinaria, código de error:"||r_estado,"information")
      RETURN FALSE
   ELSE
      RETURN TRUE
   END IF
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF60                                                   #
#Descripcion       => funcion para actualizar los registros de la tabla        #
#                     sep_movimiento_invadido                                  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 08 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_verifica_seleccion()
DEFINE v_indice       INTEGER,
       v_seleccionado BOOLEAN

   LET v_seleccionado = FALSE
   FOR v_indice = 1 TO v_montos.getLength()
      IF(v_montos[v_indice].v_ind_mov_asociado = 1)THEN
         LET v_seleccionado = TRUE
         EXIT FOR
      END IF
   END FOR
   RETURN v_seleccionado
END FUNCTION
################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF60                                                   #
#Descripcion       => funcion para actualizar los registros de la tabla        #
#                     sep_movimiento_invadido                                  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 08 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_actualiza_registros_movimientos()
DEFINE v_consulta STRING,
       v_indice   INTEGER

   # Actualiza los movimientos de separación
   LET v_consulta = "\n UPDATE sep_movimiento_invadido",
                    "\n    SET ind_mov_asociado = ?",
                    "\n  WHERE id_expediente = ?",
                    "\n    AND id_sep_movimiento_invadido = ?"
   PREPARE prp_act_registros_mov_sep FROM v_consulta
   
   LET v_consulta = "\n UPDATE sep_saldo_inicial",
                    "\n    SET mto_aivs_separado = ?",
                    "\n  WHERE id_sep_movimiento_invadido = ?"
   PREPARE prp_act_registros_sdo_inicial FROM v_consulta
   
   FOR v_indice = 1 TO v_montos.getLength()
      # actualiza los registros de sep_movimiento_invadido, para el caso de selecion de montos
      EXECUTE prp_act_registros_mov_sep USING v_montos[v_indice].v_ind_mov_asociado,
                                              v_montos[v_indice].v_id_expediente,
                                              v_montos[v_indice].v_id_sep_mov_inv
                                              
      IF(v_montos[v_indice].v_movimiento_cod = 999)THEN # 999 -> saldo inicial
         # actualiza la tabla sep_saldo_inicial solo para saldos iniciales, en el caso de que se introduzca una cantidad
         EXECUTE prp_act_registros_sdo_inicial USING v_montos[v_indice].v_aivs_separar,
                                                     v_montos[v_indice].v_id_sep_mov_inv
      END IF
   END FOR
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF60                                                   #
#Descripcion       => Funcion para eliminar los registros de la tabla          #
#                     sep_movimiento_invadido                                  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 09 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_elimina_reg_sep_mov(p_id_expediente)
DEFINE p_id_expediente LIKE sep_expediente.id_expediente
    
   DELETE
     FROM sep_saldo_inicial
    WHERE id_sep_movimiento_invadido IN (SELECT id_sep_movimiento_invadido
                                           FROM sep_movimiento_invadido
                                          WHERE id_expediente = p_id_expediente)    
   DELETE
     FROM sep_movimiento_invadido
    WHERE id_expediente = p_id_expediente

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF60                                                   #
#Descripcion       => Función para calcular el monto a separar                 #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 08 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_calculo_monto_separacion()
DEFINE v_monto_total_separacion DECIMAL(16,6),
       v_indice                 INTEGER

   LET v_monto_total_separacion = 0
   # realiza sumatoria para obtener total de aivs
   FOR v_indice = 1 TO v_montos.getLength()
      LET v_monto_total_separacion = v_monto_total_separacion + v_montos[v_indice].v_aivs_separar 
   END FOR
   RETURN v_monto_total_separacion
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF60                                                   #
#Descripcion       => Pantalla para la separación de saldos                    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 05 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_recupera_montos(p_id_expediente,p_tpo_accion)
DEFINE p_id_expediente      LIKE sep_expediente.id_expediente,
       p_tpo_accion         CHAR(1),
       v_consulta           STRING,
       v_id_derech_inv      LIKE sep_nss_expediente.id_expediente,
       v_id_derech_aso      LIKE sep_nss_expediente.id_expediente,
       v_nss_inv            LIKE sep_nss_expediente.nss,
       v_nss_aso            LIKE sep_nss_expediente.nss,
       v_montos_cta_mov     RECORD
          v_subcuenta_corta LIKE cat_subcuenta.subcuenta_corta,
          v_movimiento_desc LIKE cat_movimiento.movimiento_desc,
          v_nrp             LIKE cta_his_pagos.nrp,
          v_periodo_pago    LIKE cta_his_pagos.periodo_pago,
          v_bimestre        CHAR(6),
          v_monto_pesos     LIKE cta_movimiento.monto_pesos,
          v_monto_acciones  LIKE cta_movimiento.monto_acciones,
          v_f_liquida       LIKE cta_movimiento.f_liquida,
          v_fondo_inversion LIKE cta_movimiento.fondo_inversion,
          v_movimiento      LIKE cta_movimiento.movimiento,
          v_folio_liquida   LIKE cta_movimiento.folio_liquida,
          v_id_referencia   LIKE cta_movimiento.id_referencia,
          v_f_valor         LIKE cta_movimiento.f_valor,
          v_f_registro      LIKE cta_movimiento.f_registro,
          v_h_registro      LIKE cta_movimiento.h_registro,
          v_origen          LIKE cta_movimiento.origen,
          v_subcuenta       LIKE cta_movimiento.subcuenta
       END RECORD,
       v_ind_mov_asociado_aux   SMALLINT,
       v_montos_sep RECORD
          v_id_expediente    LIKE sep_movimiento_invadido.id_expediente,
          v_ind_mov_asociado LIKE sep_movimiento_invadido.ind_mov_asociado,
          v_subcuenta_cod    LIKE sep_movimiento_invadido.subcuenta,
          v_subcuenta        LIKE cat_subcuenta.subcuenta_corta,
          v_movimiento_cod   LIKE sep_movimiento_invadido.movimiento,
          v_movimiento       LIKE cat_movimiento.movimiento_desc,
          v_nrp              LIKE cta_his_pagos.nrp,
          v_periodo_pago     LIKE cta_his_pagos.periodo_pago,
          v_bimestre         CHAR(6),
          v_monto_pesos      LIKE sep_movimiento_invadido.monto_pesos,
          v_monto_acciones   LIKE sep_movimiento_invadido.monto_acciones,
          v_asociado         LIKE sep_movimiento_invadido.asociado,
          v_id_sep_mov_inv   LIKE sep_movimiento_invadido.id_sep_movimiento_invadido,
          v_f_valor          LIKE sep_movimiento_invadido.f_valor
       END RECORD,
       v_indice               INTEGER,
       v_max_f_liquida        DATE,--LIKE sep_movimiento_invadido.f_liquida
       v_existen_registros    BOOLEAN
       
CONSTANT v_aivs_separado SMALLINT = 0

   LET v_existen_registros = FALSE
   INITIALIZE v_montos_cta_mov.*,v_montos_sep.*,v_max_f_liquida TO NULL
   CALL v_montos.clear()
   #recupera nss para datos generales
   SELECT id_derechohabiente,nss
     INTO v_id_derech_inv,v_nss_inv
     FROM sep_nss_expediente
    WHERE id_expediente = p_id_expediente
      AND tipo_nss = 1

   SELECT id_derechohabiente,nss
     INTO v_id_derech_aso,v_nss_aso
     FROM sep_nss_expediente
    WHERE id_expediente = p_id_expediente
      AND tipo_nss = 2

   # Datos generales del expediente
   LET v_consulta = "\n SELECT exp.caso_adai,",
                    "\n        edo.descripcion",
                    "\n   FROM sep_expediente exp LEFT OUTER JOIN sep_estado_expediente edo",
                    "\n     ON exp.estado = edo.estado",
                    "\n  WHERE id_expediente = ?"
   PREPARE prp_rec_datos_expediente FROM v_consulta
   EXECUTE prp_rec_datos_expediente USING p_id_expediente
                                     INTO v_datos_expediente.v_caso_adai,
                                          v_datos_expediente.v_estado

   # se recupera los datos del expediente
   LET v_datos_expediente.v_id_expediente = p_id_expediente
   LET v_datos_expediente.v_invadido = v_nss_inv
   LET v_datos_expediente.v_asociado = v_nss_aso
   # se hace una copia de los montos de la cuenta a las tablas de separación
   LET v_consulta = "\n INSERT INTO sep_movimiento_invadido",
                    "\n (id_sep_movimiento_invadido,",
                    "\n  f_liquida,",
                    "\n  invadido,",
                    "\n  id_derechohabiente_invadido,",
                    "\n  asociado,",
                    "\n  id_derechohabiente_asociado,",
                    "\n  subcuenta,",
                    "\n  fondo_inversion,",
                    "\n  movimiento,",
                    "\n  folio_liquida,",
                    "\n  id_referencia,",
                    "\n  monto_acciones,",
                    "\n  monto_pesos,",
                    "\n  f_valor,",
                    "\n  f_registro,",
                    "\n  h_registro,",
                    "\n  origen,",
                    "\n  id_expediente,",
                    "\n  ind_mov_asociado)",
                    "\n VALUES(seq_sep_movimiento_invadido.NEXTVAL,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE prp_ins_montos FROM v_consulta
   # inserta los saldos con movimientos de tipo saldo inicial(999) y los separados
   LET v_consulta = "\n INSERT INTO sep_saldo_inicial",
                    "\n   (id_sep_movimiento_invadido,",
                    "\n    mto_aivs_origen,",
                    "\n    mto_aivs_separado)",
                    "\n VALUES(seq_sep_movimiento_invadido.CURRVAL,?,?)"
   PREPARE prp_ins_sdo_separado FROM v_consulta 

   # consulta registros para mostrar en pantalla de separación
   LET v_consulta = "\n SELECT DISTINCT a.id_expediente,",
                    "\n        a.ind_mov_asociado,",
                    "\n        a.subcuenta,",
                    "\n        c.subcuenta_corta,",
                    "\n        a.movimiento,",
                    "\n        d.movimiento_desc,",
                    "\n        b.nrp           ,",
                    "\n        b.periodo_pago  ,",
                    "\n        fn_bimestre_pago(b.periodo_pago),",--*****
                    "\n        a.monto_pesos   ,",
                    "\n        a.monto_acciones,",
                    "\n        a.asociado,",
                    "\n        a.id_sep_movimiento_invadido,",
                    "\n        a.f_valor",                    
                    "\n FROM sep_movimiento_invadido a ,",
                    "\n      OUTER cta_his_pagos  b ,",
                    "\n      cat_subcuenta  c ,",
                    "\n      cat_movimiento d",
                    "\n WHERE a.folio_liquida      = b.folio",
                    "\n   AND a.id_derechohabiente_invadido = b.id_derechohabiente",
                    "\n   AND a.id_referencia      = b.id_referencia",
                    "\n   AND a.origen             = b.nrp",
                    "\n   AND a.subcuenta  = 44",
                    "\n   AND a.subcuenta  = c.subcuenta",
                    "\n   AND a.movimiento = d.movimiento",
                    "\n   AND a.id_expediente = ?",
                    "\n ORDER BY 5,4"
   PREPARE prp_rec_montos_sep FROM v_consulta
   DECLARE cur_rec_montos_sep CURSOR FOR prp_rec_montos_sep

   LET v_consulta = "\n SELECT mto_aivs_separado",
                    "\n   FROM sep_saldo_inicial",
                    "\n  WHERE id_sep_movimiento_invadido = ?"
   PREPARE prp_rec_sdo_ini FROM v_consulta
         
   CASE p_tpo_accion

      WHEN "N" # para la opcion nuevo (separación para nuevo expediente)
         LET v_ind_mov_asociado_aux = 0
         # recupera los montos de cta_movimiento y los pasa a la tabla sep_movimiento_invadido
         LET v_consulta = "\n SELECT c.subcuenta_corta,",
                          "\n        d.movimiento_desc,",
                          "\n        b.nrp           ,",
                          "\n        b.periodo_pago  ,",--*****
                          "\n        fn_bimestre_pago(b.periodo_pago),",--*****
                          "\n        a.monto_pesos   ,",
                          "\n        a.monto_acciones,",
                          "\n        a.f_liquida,",
                          "\n        a.fondo_inversion,",
                          "\n        a.movimiento,",
                          "\n        a.folio_liquida,",
                          "\n        a.id_referencia,",
                          "\n        a.f_valor,",
                          "\n        a.f_registro,",
                          "\n        a.h_registro,",
                          "\n        a.origen,",
                          "\n        a.subcuenta",
                          "\n FROM cta_movimiento a ,",
                          "\n      OUTER cta_his_pagos  b ,",
                          "\n      cat_subcuenta  c ,",
                          "\n      cat_movimiento d",
                          "\n WHERE a.folio_liquida      = b.folio",
                          "\n   AND a.id_derechohabiente = b.id_derechohabiente",
                          "\n   AND a.id_referencia      = b.id_referencia",
                          "\n   AND a.origen             = b.nrp",
                          "\n   AND a.subcuenta  = 44",
                          "\n   AND a.subcuenta  = c.subcuenta",
                          "\n   AND a.movimiento = d.movimiento",
                          "\n   AND a.id_derechohabiente = ?",
                          "\n ORDER BY 5,4"
         PREPARE prp_rec_montos_invadido FROM v_consulta
         DECLARE cur_rec_montos_invadido CURSOR FOR prp_rec_montos_invadido
         FOREACH cur_rec_montos_invadido USING v_id_derech_inv
                                          INTO v_montos_cta_mov.*

            # realiza copia de los montos del invadido
            EXECUTE prp_ins_montos USING v_montos_cta_mov.v_f_liquida,
                                         v_nss_inv,
                                         v_id_derech_inv,
                                         v_nss_aso,
                                         v_id_derech_aso,
                                         v_montos_cta_mov.v_subcuenta,
                                         v_montos_cta_mov.v_fondo_inversion,
                                         v_montos_cta_mov.v_movimiento,
                                         v_montos_cta_mov.v_folio_liquida,
                                         v_montos_cta_mov.v_id_referencia,
                                         v_montos_cta_mov.v_monto_acciones,
                                         v_montos_cta_mov.v_monto_pesos,
                                         v_montos_cta_mov.v_f_valor,
                                         v_montos_cta_mov.v_f_registro,
                                         v_montos_cta_mov.v_h_registro,
                                         v_montos_cta_mov.v_origen,
                                         p_id_expediente,
                                         v_ind_mov_asociado_aux
                                         
            #inserta registros en la tabla sep_saldo_inicial solo para los avis con movimiento saldo inicial(999) 
            IF(v_montos_cta_mov.v_movimiento = 999)THEN # 999 saldo inicial                                         
               EXECUTE prp_ins_sdo_separado USING v_montos_cta_mov.v_monto_acciones,
                                                  v_aivs_separado
            END IF

         END FOREACH
         FREE cur_rec_montos_invadido

         # recupera los montos para mostrar en la separación
         LET v_indice = 1
         FOREACH cur_rec_montos_sep USING p_id_expediente
                                     INTO v_montos_sep.*
            LET v_montos[v_indice].v_num              = v_indice
            LET v_montos[v_indice].v_id_expediente    = v_montos_sep.v_id_expediente
            LET v_montos[v_indice].v_ind_mov_asociado = v_montos_sep.v_ind_mov_asociado
            LET v_montos[v_indice].v_subcuenta_cod    = v_montos_sep.v_subcuenta_cod
            LET v_montos[v_indice].v_subcuenta        = v_montos_sep.v_subcuenta
            LET v_montos[v_indice].v_movimiento_cod   = v_montos_sep.v_movimiento_cod
            LET v_montos[v_indice].v_movimiento       = v_montos_sep.v_movimiento
            LET v_montos[v_indice].v_nrp              = v_montos_sep.v_nrp
            LET v_montos[v_indice].v_periodo_pago     = v_montos_sep.v_periodo_pago
            LET v_montos[v_indice].v_bimestre         = v_montos_sep.v_bimestre            
            LET v_montos[v_indice].v_pesos            = v_montos_sep.v_monto_pesos
            LET v_montos[v_indice].v_aivs             = v_montos_sep.v_monto_acciones
            LET v_montos[v_indice].v_nss_asociado     = v_montos_sep.v_asociado
            LET v_montos[v_indice].v_id_sep_mov_inv   = v_montos_sep.v_id_sep_mov_inv
            LET v_montos[v_indice].v_f_valor          = v_montos_sep.v_f_valor
            
            IF(v_montos[v_indice].v_ind_mov_asociado = 1)THEN
               IF(v_montos[v_indice].v_movimiento_cod = 999)THEN
                  # para el caso de saldo inicial(999) se recupera el monto de la tabla sep_saldo_inicial
                  EXECUTE prp_rec_sdo_ini USING v_montos[v_indice].v_id_sep_mov_inv
                                           INTO v_montos[v_indice].v_aivs_separar
                  
               ELSE
                  LET v_montos[v_indice].v_aivs_separar  = v_montos[v_indice].v_aivs
               END IF
            ELSE
               LET v_montos[v_indice].v_aivs_separar  = 0 # inicializa en 0 el valor de separación
            END IF
            
            LET v_indice = v_indice + 1
            LET v_existen_registros = TRUE
         END FOREACH
         FREE cur_rec_montos_sep
         
      WHEN "R" # recuperar y separar
         LET v_ind_mov_asociado_aux = 0
         # recupera la maxima fecha insertada en la tabla sep_movimiento_invadido para poder recuperar
         # los registros más actuales de cta_movimiento a partir de la fecha
         SELECT MAX(f_liquida)
           INTO v_max_f_liquida
           FROM sep_movimiento_invadido
          WHERE id_expediente = p_id_expediente
          
         LET v_consulta = "\n SELECT c.subcuenta_corta,",
                          "\n        d.movimiento_desc,",
                          "\n        b.nrp           ,",
                          "\n        b.periodo_pago  ,",--*****
                          "\n        fn_bimestre_pago(b.periodo_pago),",--*****
                          "\n        a.monto_pesos   ,",
                          "\n        a.monto_acciones,",
                          "\n        a.f_liquida,",
                          "\n        a.fondo_inversion,",
                          "\n        a.movimiento,",
                          "\n        a.folio_liquida,",
                          "\n        a.id_referencia,",
                          "\n        a.f_valor,",
                          "\n        a.f_registro,",
                          "\n        a.h_registro,",
                          "\n        a.origen,",
                          "\n        a.subcuenta",
                          "\n FROM cta_movimiento a ,",
                          "\n      OUTER cta_his_pagos  b ,",
                          "\n      cat_subcuenta  c ,",
                          "\n      cat_movimiento d",
                          "\n WHERE a.folio_liquida      = b.folio",
                          "\n   AND a.id_derechohabiente = b.id_derechohabiente",
                          "\n   AND a.id_referencia      = b.id_referencia",
                          "\n   AND a.origen             = b.nrp",
                          "\n   AND a.subcuenta  = 44",
                          "\n   AND a.subcuenta  = c.subcuenta",
                          "\n   AND a.movimiento = d.movimiento",
                          "\n   AND a.id_derechohabiente = ?",
                          "\n   AND a.f_liquida > ?",
                          "\n ORDER BY 5,4"
         PREPARE prp_rec_nuevos_montos FROM v_consulta
         DECLARE cur_rec_nuevos_montos CURSOR FOR prp_rec_nuevos_montos
         FOREACH cur_rec_nuevos_montos USING v_id_derech_inv,
                                             v_max_f_liquida
                                        INTO v_montos_cta_mov.*
            # realiza copia de los montos mas actuales
            EXECUTE prp_ins_montos USING v_montos_cta_mov.v_f_liquida,
                                         v_nss_inv,
                                         v_id_derech_inv,
                                         v_nss_aso,
                                         v_id_derech_aso,
                                         v_montos_cta_mov.v_subcuenta,
                                         v_montos_cta_mov.v_fondo_inversion,
                                         v_montos_cta_mov.v_movimiento,
                                         v_montos_cta_mov.v_folio_liquida,
                                         v_montos_cta_mov.v_id_referencia,
                                         v_montos_cta_mov.v_monto_acciones,
                                         v_montos_cta_mov.v_monto_pesos,
                                         v_montos_cta_mov.v_f_valor,
                                         v_montos_cta_mov.v_f_registro,
                                         v_montos_cta_mov.v_h_registro,
                                         v_montos_cta_mov.v_origen,
                                         p_id_expediente,
                                         v_ind_mov_asociado_aux
            #inserta registros en la tabla sep_saldo_inicial solo para los avis con movimiento saldo inicial(999) 
            IF(v_montos_cta_mov.v_movimiento = 999)THEN # 999 --> saldo inicial                                         
               EXECUTE prp_ins_sdo_separado USING v_montos_cta_mov.v_monto_acciones,
                                                  v_aivs_separado
            END IF

         END FOREACH
         FREE cur_rec_nuevos_montos
         # recupera los montos para mostrar en la separación
         LET v_indice = 1
         FOREACH cur_rec_montos_sep USING p_id_expediente
                                     INTO v_montos_sep.*
            LET v_montos[v_indice].v_num              = v_indice
            LET v_montos[v_indice].v_id_expediente    = v_montos_sep.v_id_expediente
            LET v_montos[v_indice].v_ind_mov_asociado = v_montos_sep.v_ind_mov_asociado
            LET v_montos[v_indice].v_subcuenta_cod    = v_montos_sep.v_subcuenta_cod
            LET v_montos[v_indice].v_subcuenta        = v_montos_sep.v_subcuenta
            LET v_montos[v_indice].v_movimiento_cod   = v_montos_sep.v_movimiento_cod
            LET v_montos[v_indice].v_movimiento       = v_montos_sep.v_movimiento
            LET v_montos[v_indice].v_nrp              = v_montos_sep.v_nrp
            LET v_montos[v_indice].v_periodo_pago     = v_montos_sep.v_periodo_pago
            LET v_montos[v_indice].v_bimestre         = v_montos_sep.v_bimestre            
            LET v_montos[v_indice].v_pesos            = v_montos_sep.v_monto_pesos
            LET v_montos[v_indice].v_aivs             = v_montos_sep.v_monto_acciones
            LET v_montos[v_indice].v_nss_asociado     = v_montos_sep.v_asociado
            LET v_montos[v_indice].v_id_sep_mov_inv   = v_montos_sep.v_id_sep_mov_inv
            LET v_montos[v_indice].v_f_valor          = v_montos_sep.v_f_valor
            
            IF(v_montos[v_indice].v_ind_mov_asociado = 1)THEN
               IF(v_montos[v_indice].v_movimiento_cod = 999)THEN
                  # para el caso de saldo inicial(999) se recupera el monto de la tabla sep_saldo_inicial
                  EXECUTE prp_rec_sdo_ini USING v_montos[v_indice].v_id_sep_mov_inv
                                           INTO v_montos[v_indice].v_aivs_separar
                  
               ELSE
                  LET v_montos[v_indice].v_aivs_separar  = v_montos[v_indice].v_aivs
               END IF
            ELSE
               LET v_montos[v_indice].v_aivs_separar  = 0 # inicializa en 0 el valor de separación
            END IF
            LET v_indice = v_indice + 1
            LET v_existen_registros = TRUE
         END FOREACH
         FREE cur_rec_montos_sep
         

      WHEN "C" # consulta separación
         # recupera los montos para mostrar en la separación
         LET v_indice = 1
         FOREACH cur_rec_montos_sep USING p_id_expediente
                                     INTO v_montos_sep.*
            LET v_montos[v_indice].v_num              = v_indice
            LET v_montos[v_indice].v_id_expediente    = v_montos_sep.v_id_expediente
            LET v_montos[v_indice].v_ind_mov_asociado = v_montos_sep.v_ind_mov_asociado
            LET v_montos[v_indice].v_subcuenta_cod    = v_montos_sep.v_subcuenta_cod
            LET v_montos[v_indice].v_subcuenta        = v_montos_sep.v_subcuenta
            LET v_montos[v_indice].v_movimiento_cod   = v_montos_sep.v_movimiento_cod
            LET v_montos[v_indice].v_movimiento       = v_montos_sep.v_movimiento
            LET v_montos[v_indice].v_nrp              = v_montos_sep.v_nrp
            LET v_montos[v_indice].v_periodo_pago     = v_montos_sep.v_periodo_pago
            LET v_montos[v_indice].v_bimestre         = v_montos_sep.v_bimestre            
            LET v_montos[v_indice].v_pesos            = v_montos_sep.v_monto_pesos
            LET v_montos[v_indice].v_aivs             = v_montos_sep.v_monto_acciones
            LET v_montos[v_indice].v_nss_asociado     = v_montos_sep.v_asociado
            LET v_montos[v_indice].v_id_sep_mov_inv   = v_montos_sep.v_id_sep_mov_inv
            LET v_montos[v_indice].v_f_valor          = v_montos_sep.v_f_valor
            
            IF(v_montos[v_indice].v_ind_mov_asociado = 1)THEN
               IF(v_montos[v_indice].v_movimiento_cod = 999)THEN
                  # para el caso de saldo inicial(999) se recupera el monto de la tabla sep_saldo_inicial
                  EXECUTE prp_rec_sdo_ini USING v_montos[v_indice].v_id_sep_mov_inv
                                           INTO v_montos[v_indice].v_aivs_separar
                  
               ELSE
                  LET v_montos[v_indice].v_aivs_separar  = v_montos[v_indice].v_aivs
               END IF
            ELSE
               LET v_montos[v_indice].v_aivs_separar  = 0 # inicializa en 0 el valor de separación
            END IF
            
            LET v_indice = v_indice + 1
            LET v_existen_registros = TRUE
         END FOREACH
         FREE cur_rec_montos_sep
         
   END CASE

   RETURN v_existen_registros
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF60                                                   #
#Descripcion       => Funcion para generar reporte                             #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 09 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_genera_reporte(p_id_expediente)
DEFINE p_id_expediente LIKE sep_expediente.id_expediente,
       v_indice        INTEGER,
       v_consulta      STRING,
       v_encabezado    RECORD
          v_id_expediente  LIKE sep_expediente.id_expediente,
          v_nss_inv        LIKE sep_nss_expediente.nss,
          v_nombre_inv     LIKE afi_derechohabiente.nombre_af,
          v_ap_paterno_inv LIKE afi_derechohabiente.ap_paterno_af,
          v_ap_materno_inv LIKE afi_derechohabiente.ap_materno_af,
          v_nss_aso        LIKE sep_nss_expediente.nss,
          v_nombre_aso     LIKE afi_derechohabiente.nombre_af,
          v_ap_paterno_aso LIKE afi_derechohabiente.ap_paterno_af,
          v_ap_materno_aso LIKE afi_derechohabiente.ap_materno_af
       END RECORD,
       v_tpo_nss  LIKE sep_nss_expediente.tipo_nss,
       r_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_ruta_lst        LIKE seg_modulo.ruta_listados,
       v_id_nom_rpt      CHAR(5),
       v_genero_rpt      BOOLEAN,
       v_manejador_rpt   OM.SaxDocumentHandler

   LET v_genero_rpt = FALSE
   CALL fn_rutas("sep") RETURNING r_ruta_ejecutable, r_ruta_lst
   IF(fgl_report_loadCurrentSettings(r_ruta_ejecutable CLIPPED||"/SEPF601.4rp"))THEN
      # Salida del reporte
      CALL fgl_report_selectDevice("PDF")
      LET v_id_nom_rpt = p_id_expediente
      LET v_nom_reporte = p_usuario_cod CLIPPED, "-SEPF60-", 
                          v_id_nom_rpt USING "&&&&&", "-", 
                          "00000" USING "&&&&&", "-", 
                          "00000" USING "&&&&&",
                          ".pdf"
      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(r_ruta_lst CLIPPED||"/"||v_nom_reporte)
         
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      START REPORT fn_genera_rpt_sep_montos TO XML HANDLER v_manejador_rpt
         # datos de encabezado
         LET v_consulta = "\n SELECT exp.nss,",
                          "\n        nombre_af,",
                          "\n        afi.ap_paterno_af,",
                          "\n        afi.ap_materno_af",
                          "\n   FROM sep_nss_expediente exp LEFT OUTER JOIN afi_derechohabiente afi",
                          "\n     ON afi.id_derechohabiente = exp.id_derechohabiente",
                          "\n  WHERE exp.id_expediente = ?",
                          "\n    AND exp.tipo_nss = ?"
         PREPARE prp_rec_datos_enc FROM v_consulta
         LET v_tpo_nss = 1
         LET v_encabezado.v_id_expediente = p_id_expediente
         # recupera datos de invadido
         EXECUTE prp_rec_datos_enc USING p_id_expediente,
                                         v_tpo_nss
                                    INTO v_encabezado.v_nss_inv,
                                         v_encabezado.v_nombre_inv,
                                         v_encabezado.v_ap_paterno_inv,
                                         v_encabezado.v_ap_materno_inv
         LET v_tpo_nss = 2
         # recupera datos de asociado
         EXECUTE prp_rec_datos_enc USING p_id_expediente,
                                         v_tpo_nss
                                    INTO v_encabezado.v_nss_aso,
                                         v_encabezado.v_nombre_aso,
                                         v_encabezado.v_ap_paterno_aso,
                                         v_encabezado.v_ap_materno_aso
      
         FOR v_indice = 1 TO v_montos.getLength()
            IF(v_montos[v_indice].v_ind_mov_asociado = 1)THEN
               OUTPUT TO REPORT fn_genera_rpt_sep_montos(v_encabezado.*,v_montos[v_indice].*)
               LET v_genero_rpt = TRUE
            END IF
         END FOR
      FINISH REPORT fn_genera_rpt_sep_montos
   ELSE
      CALL fn_mensaje("AVISO","Ocurrió un error al cargar plantilla de reporte","information")
   END IF
   RETURN v_genero_rpt
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF60                                                   #
#Descripcion       => Funcion para generar reporte                             #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 09 de Octubre de 2012                                    #
################################################################################
REPORT fn_genera_rpt_sep_montos(p_encabezado,p_montos)
DEFINE p_encabezado    RECORD
          v_id_expediente  LIKE sep_expediente.id_expediente,
          v_nss_inv        LIKE sep_nss_expediente.nss,
          v_nombre_inv     LIKE afi_derechohabiente.nombre_af,
          v_ap_paterno_inv LIKE afi_derechohabiente.ap_paterno_af,
          v_ap_materno_inv LIKE afi_derechohabiente.ap_materno_af,
          v_nss_aso        LIKE sep_nss_expediente.nss,
          v_nombre_aso     LIKE afi_derechohabiente.nombre_af,
          v_ap_paterno_aso LIKE afi_derechohabiente.ap_paterno_af,
          v_ap_materno_aso LIKE afi_derechohabiente.ap_materno_af
       END RECORD,
       p_montos RECORD
          v_num              INTEGER,
          v_id_expediente    LIKE sep_expediente.id_expediente,
          v_ind_mov_asociado LIKE sep_movimiento_invadido.ind_mov_asociado,
          v_subcuenta_cod    LIKE sep_movimiento_invadido.subcuenta,
          v_subcuenta        LIKE cat_subcuenta.subcuenta_corta,
          v_movimiento_cod   LIKE sep_movimiento_invadido.movimiento,
          v_movimiento       LIKE cat_movimiento.movimiento_desc,
          v_nrp              LIKE cta_his_pagos.nrp,
          v_periodo_pago     LIKE cta_his_pagos.periodo_pago,
          v_bimestre         CHAR(6),
          v_pesos            LIKE cta_movimiento.monto_pesos,
          v_aivs             LIKE cta_movimiento.monto_acciones,
          v_aivs_separar     LIKE cta_movimiento.monto_acciones,
          v_nss_asociado     LIKE sep_nss_expediente.nss,
          v_id_sep_mov_inv   LIKE sep_movimiento_invadido.id_sep_movimiento_invadido,
          v_f_valor          LIKE sep_movimiento_invadido.f_valor
       END RECORD,
       v_consulta     STRING,
       v_valor_accion LIKE glo_valor_fondo.precio_fondo,
       v_fecha_actual DATE,
       v_hora_actual  DATETIME HOUR TO SECOND,
       v_total_pesos  LIKE cta_movimiento.monto_pesos,
       v_total_aivs   LIKE cta_movimiento.monto_acciones,
       v_pagina       SMALLINT

   FORMAT

      FIRST PAGE HEADER
         LET v_consulta = "\n SELECT precio_fondo",           
                          "\n   FROM glo_valor_fondo",
                          "\n  WHERE fondo = 11",
                          "\n    AND f_valuacion = ?"
         PREPARE prp_rec_precio_aivs FROM v_consulta 
         
         LET v_fecha_actual = TODAY
         LET v_hora_actual  = CURRENT HOUR TO SECOND
         PRINTX v_fecha_actual,v_hora_actual,p_encabezado.*

      ON EVERY ROW 
         IF(p_montos.v_movimiento_cod = 999)THEN # 999 --> saldo inicial
            # recalcula pesos para el caso de saldo inicial, para el caso en el que se ingresó un valor distintio de aivs
            EXECUTE prp_rec_precio_aivs USING p_montos.v_f_valor
                                         INTO v_valor_accion
            LET p_montos.v_pesos = p_montos.v_aivs_separar * v_valor_accion
         END IF
         PRINTX p_montos.*

      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina

      ON LAST ROW
         LET v_total_pesos = SUM(p_montos.v_pesos)
         LET v_total_aivs  = SUM(p_montos.v_aivs) 
         PRINTX v_total_pesos, v_total_aivs

      
END REPORT
