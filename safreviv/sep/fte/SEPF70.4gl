--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 04/10/2012
--==============================================================================

################################################################################
#Proyecto     => SAFRE VIVIENDA                                                #
#Propietario  => E.F.P.                                                        #
#Modulo       => SEP                                                           #
#Programa     => SEPF70                                                        #
#Objetivo     => Captura de montos a separar para expedientes de               #
#                solo infonavit                                                #
#Autor        => Hugo César Ramírez García                                     #
#Fecha Inicio => 04 de Octubre de 2012                                         #
################################################################################
DATABASE safre_viv
DEFINE v_exito BOOLEAN
DEFINE g_enter SMALLINT
DEFINE v_pid   DECIMAL(9,0)
DEFINE v_verifica_saldo BOOLEAN
DEFINE reg_expediente RECORD 
       nss_invadido   char(11),
       rfc_invadido   char(13),
       nombre_invadido char(40),
       nss_asociado   char(11),
       rfc_asociado  char(13),
       nombre_asociado char(40)
END RECORD      
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
          v_id_expediente_fondo72 DECIMAL(9,0),
          v_rfc_invadido          CHAR(13),
          v_rfc_asociado          CHAR(13),
          --v_estado        LIKE sep_expediente.estado
          v_estado        LIKE sep_estado_expediente.descripcion
       END RECORD,
       v_datos_expediente RECORD
          v_id_expediente LIKE sep_expediente.id_expediente,
          v_estado        LIKE sep_estado_expediente.descripcion
       END RECORD,
       v_montos DYNAMIC ARRAY OF RECORD
          v_num INTEGER,
          v_id_expediente    LIKE sep_expediente.id_expediente,
          v_ind_mov_asociado SMALLINT,
          v_nombre           LIKE cta_his_fondo72.nombre,
          v_folio            LIKE cta_his_fondo72.folio,
          v_ejercicio        LIKE cta_his_fondo72.ejercicio,
          v_clave_mov        CHAR(03),
          v_empresa          LIKE cta_his_fondo72.empresa,
          v_bimestres        LIKE cta_his_fondo72.bimestres,
          v_importe          LIKE cta_his_fondo72.importe,
          v_importe_separar  LIKE cta_his_fondo72.importe,
          v_rfc_asociado1    CHAR(013) ,
          v_id_sep_mov_inv   DECIMAL(9,0),
          v_id_cta_his_fondo72   DECIMAL(9,0)
       END RECORD       ,   

       v_nom_reporte     STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin ,
       v_id_afi_fondo72  DECIMAL(9,0)
       
       

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF70                                                   #
#Descripcion       => separación de montos de expedientes de solo infonavit    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 04 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_consulta_expedientes_infonavit(p_usuario,sep_id_afi_fondo72)

DEFINE sep_id_afi_fondo72 DECIMAL(9,0)
DEFINE p_usuario          CHAR(20)

DEFINE v_continua      BOOLEAN,
       v_consulta      BOOLEAN,
       v_id_expediente LIKE sep_expediente.id_expediente

   LET p_usuario_cod    = p_usuario
   
   LET p_tpo_ejecucion  = ARG_VAL(2)
   
   LET p_titulo_ventana = "Separacion Fondo72"
   LET v_id_afi_fondo72 = sep_id_afi_fondo72   
       
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "sep"

   OPEN WINDOW vtna_consulta_expedientes WITH FORM v_ruta_ejecutable CLIPPED||"/SEPF701"
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_titulo_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo_ventana)         
         CALL v_ventana.setText(p_titulo_ventana)
      END IF
      

      LET v_continua = TRUE
 
      #recupera los datos iniciales
      CALL fn_recupera_datos_iniciales(sep_id_afi_fondo72)      

      WHILE v_continua
         MENU #Pantalla principal
         
            BEFORE MENU
               LET v_ventana = ui.Window.getCurrent()
               LET v_forma = v_ventana.getForm() 
               CALL v_forma.setElementHidden("seccion_asociado",1)   
               
            ON ACTION Nuevo
               #verifica si existe aclaracion sep en proceso
               CALL fn_valida_existe(sep_id_afi_fondo72)RETURNING v_consulta
               IF (v_consulta) THEN 
                   CALL v_forma.setElementHidden("seccion_asociado",0)   
                   # se calcula secuencia del expediente  

                  SELECT seq_sep_expediente_fondo72.NEXTVAL 
                  INTO v_id_expediente
                  FROM cat_fondo_local WHERE fondo = 11
                   # muestra pantalla de seleccion de montos
                   CALL fn_separa_saldos(v_id_expediente,"N")
               ELSE 
                   CALL fn_mensaje("AVISO","Cuenta con trámite en proceso, utilizar opción RECUPERAR","information")
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
               
            ON ACTION Aplicar
               # Pantalla de seleccion de exoedietes previo guardado
               CALL fn_rec_expediente_separacion("P") RETURNING v_id_expediente
               IF(v_id_expediente IS NOT NULL)THEN
                  # mustra pantalla de seleccion de montos
                  CALL fn_consulta_saldos(v_id_expediente,"P")
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
#Programa          => SEPF70                                                   #
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
#Programa          => SEPF70                                                   #
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
#Programa          => SEPF70                                                   #
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
#Programa          => SEPF70                                                   #
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
#Programa          => SEPF70                                                   #
#Descripcion       => Recupera expediente a separar montos                     #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 04 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_rec_expediente_separacion(p_tpo_consulta)
DEFINE v_id_expediente LIKE sep_expediente.id_expediente,
       p_tpo_consulta  CHAR(1)

   # seleccion de expedientes
   OPEN WINDOW vtna_rec_expediente_sep WITH FORM v_ruta_ejecutable CLIPPED||"/SEPF702"

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
               LET v_id_expediente = v_expediente_sep_inf[ARR_CURR()].v_id_expediente_fondo72
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
#Programa          => SEPF70                                                   #
#Descripcion       => Recupera expediente a separar montos                     #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 04 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_con_expedientes_separacion_infonavit(p_tpo_consulta)
DEFINE v_consulta     STRING,
       p_tpo_consulta CHAR(1),
       v_expediente_sep_inf_aux RECORD
          v_id_expediente DECIMAL(9,0),
          v_rfc_invadido  CHAR(13),          
          v_rfc_asociado  CHAR(13),          
          --v_estado        LIKE sep_expediente.estado
          v_estado        LIKE sep_estado_expediente.descripcion
       END RECORD,
       v_indice       INTEGER,
       v_tpo_filtro   STRING

   LET v_indice = 1
   
   CALL v_expediente_sep_inf.clear()
   
   # contruye consulta dependiendo de quien lo llame
   CASE p_tpo_consulta   
      WHEN "R"
         LET v_tpo_filtro = "\n  WHERE exp.estado = 70 AND exp.id_afi_fondo72_invadido = ? " # Previo Registrado

      WHEN "C"
         LET v_tpo_filtro = "\n  WHERE exp.estado IN (75,80,85) AND exp.id_afi_fondo72_invadido = ? " # Previo confimrado, Previo Preliquidado, Previo Liquidado
      WHEN "P"
         LET v_tpo_filtro = "\n  WHERE exp.estado IN (75) AND exp.id_afi_fondo72_invadido = ? " # Previo confimrado, Previo Preliquidado, Previo Liquidado

   END CASE
   # recupera los expedientes en separación de montos
   LET v_consulta = "\n SELECT exp.id_expediente_fondo72,",
                    "\n        exp.rfc_invadido ,",
                    "\n        exp.rfc_asociado ,",
                    "\n        edo.descripcion",
                    "\n   FROM sep_expediente_fondo72 exp LEFT OUTER JOIN sep_estado_expediente edo",
                    "\n     ON exp.estado = edo.estado",
                    v_tpo_filtro
   PREPARE prp_rec_expedientes_sep_inf FROM v_consulta
   DECLARE cur_rec_expedientes_sep_inf CURSOR FOR prp_rec_expedientes_sep_inf
   FOREACH cur_rec_expedientes_sep_inf USING v_id_afi_fondo72 
                                       INTO v_expediente_sep_inf_aux.*
   
      LET v_expediente_sep_inf[v_indice].v_num           = v_indice
      LET v_expediente_sep_inf[v_indice].v_id_expediente_fondo72 = v_expediente_sep_inf_aux.v_id_expediente
      LET v_expediente_sep_inf[v_indice].v_rfc_invadido = v_expediente_sep_inf_aux.v_rfc_invadido
      LET v_expediente_sep_inf[v_indice].v_rfc_asociado = v_expediente_sep_inf_aux.v_rfc_asociado
      LET v_expediente_sep_inf[v_indice].v_estado       = v_expediente_sep_inf_aux.v_estado

      LET v_indice = v_indice + 1
   END FOREACH 
   FREE cur_rec_expedientes_sep_inf

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF70                                                   #
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
       v_edo_expediente       LIKE sep_expediente.estado,
       v_estado_preliquidar   SMALLINT, -- evalua si muestra accion preliquidar
       v_filtro               STRING  ,
       v_valida_historico     BOOLEAN    

   # Separación de montos
   OPEN WINDOW vtna_separa_saldos WITH FORM v_ruta_ejecutable CLIPPED||"/SEPF703"

      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_titulo_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo_ventana)
         CALL v_ventana.setText(p_titulo_ventana)
      END IF
      
      DIALOG ATTRIBUTES(UNBUFFERED)

         # Captura asociado
         {INPUT reg_expediente.nss_asociado,
               reg_expediente.rfc_asociado,
               reg_expediente.nombre_asociado
          FROM nss_asociado,
               rfc_asociado,
               nombre_asociado
               ATTRIBUTES(WITHOUT DEFAULTS)}
         CONSTRUCT v_filtro ON nss, rfc FROM nss_asociado,rfc_asociado

            BEFORE CONSTRUCT 
               CASE p_tpo_accion
                 WHEN "N"

                 WHEN "R"
                    # Recupera los datos asociados
                    CALL fn_recupera_datos_asociado(v_id_afi_fondo72,p_id_expediente)
                    --DISPLAY reg_expediente.nss_asociado
                    --DISPLAY reg_expediente.rfc_asociado
                    --DISPLAY reg_expediente.nombre_asociado

               END CASE

            ON ACTION cuenta_nueva        
            
               INITIALIZE reg_expediente.nombre_asociado TO NULL
               CALL fn_muestra_nueva_cuenta() RETURNING v_exito
               
               IF NOT v_exito THEN 
                  NEXT FIELD rfc_asociado
               END IF

            ON ACTION limpiar_campos
               INITIALIZE reg_expediente.nss_asociado,
                          reg_expediente.rfc_asociado,
                          reg_expediente.nombre_asociado TO NULL
               DISPLAY reg_expediente.nss_asociado TO nss_asociado
               DISPLAY reg_expediente.rfc_asociado TO rfc_asociado
               DISPLAY reg_expediente.nombre_asociado TO nombre_asociado
            
            ON ACTION existe_cuenta
               IF(GET_FLDBUF(nss_asociado) IS NULL AND GET_FLDBUF(rfc_asociado) IS NULL)THEN
                  CALL fn_mensaje("AVISO","Capture algún filtro para la búsqueda","information")
                  NEXT FIELD nss_asociado
               END IF
               
               # busca la cuentas con los datos capturados
               CALL fn_busca_cuenta(v_filtro) 
                    RETURNING reg_expediente.nss_asociado,
                              reg_expediente.rfc_asociado,
                              reg_expediente.nombre_asociado -- busca la existencia de la cuenta
               DISPLAY reg_expediente.nss_asociado TO nss_asociado
               DISPLAY reg_expediente.rfc_asociado TO rfc_asociado
               DISPLAY reg_expediente.nombre_asociado TO nombre_asociado
               
         END CONSTRUCT

         INPUT ARRAY v_montos FROM tbl_movimientos.*
            ATTRIBUTES(APPEND ROW = FALSE, 
                       INSERT ROW = FALSE, 
                       DELETE ROW = FALSE, 
                       AUTO APPEND = FALSE,
                       WITHOUT DEFAULTS)

            ON CHANGE v_importe_separar
               CALL GET_FLDBUF(v_importe_separar) RETURNING v_montos[ARR_CURR()].v_importe_separar

               IF( v_montos[ARR_CURR()].v_importe_separar > v_montos[ARR_CURR()].v_importe )THEN
                  CALL fn_mensaje("AVISO","'importe separar' excede monto importe","information")
                  LET v_montos[ARR_CURR()].v_importe_separar = 0
                  NEXT FIELD v_importe_separar 
               END IF

            BEFORE FIELD v_importe_separar
               CALL GET_FLDBUF(ind_movi_asociado) RETURNING v_montos[ARR_CURR()].v_ind_mov_asociado
               IF( v_montos[ARR_CURR()].v_clave_mov <> "AA" OR # 999 saldo inicial
                   v_montos[ARR_CURR()].v_ind_mov_asociado <> 1 )THEN
                  NEXT FIELD ind_movi_asociado
               END IF
            
            ON CHANGE ind_movi_asociado
               CALL GET_FLDBUF(ind_movi_asociado) RETURNING v_montos[ARR_CURR()].v_ind_mov_asociado

               

               
               LET v_valida_historico = FALSE 
               
               SELECT 1 
               INTO v_valida_historico               
               FROM   sep_movimiento_invadido_fondo72
               WHERE  id_cta_his_fondo72 = v_montos[ARR_CURR()].v_id_cta_his_fondo72
               AND    clave_mov = "SEP"
               AND    ind_mov_asociado = 1 
               AND    id_expediente_fondo72 <> p_id_expediente
               GROUP BY 1               

               IF((v_montos[ARR_CURR()].v_clave_mov = "AP" OR 
                  v_montos[ARR_CURR()].v_clave_mov = "SEP") AND
                  v_montos[ARR_CURR()].v_importe >= 0 AND
                  v_valida_historico = 0 )THEN # 999 saldo inicial 
                  CALL fn_verifica_saldo_movimientos() RETURNING v_verifica_saldo      
                  IF NOT v_verifica_saldo THEN
                     CALL fn_mensaje("AVISO","Saldo insuficiente en cuenta ","information")
                     LET v_montos[ARR_CURR()].v_ind_mov_asociado = 0               
                     NEXT FIELD ind_movi_asociado
                  END IF
          
                  IF(v_montos[ARR_CURR()].v_ind_mov_asociado = 1 )THEN # 999 saldo inicial
                     IF reg_expediente.rfc_asociado IS NULL THEN
                        LET v_montos[ARR_CURR()].v_importe_separar = 0
                        LET v_montos[ARR_CURR()].v_rfc_asociado1   = ""
                        LET v_montos[ARR_CURR()].v_ind_mov_asociado = 0               
                        CALL fn_mensaje("AVISO","Primero Capture cuenta asociada ","information")
                        NEXT FIELD rfc_asociado 
                     ELSE 
                        LET v_montos[ARR_CURR()].v_importe_separar = v_montos[ARR_CURR()].v_importe
                        LET v_montos[ARR_CURR()].v_rfc_asociado1   = reg_expediente.rfc_asociado
                     END IF                     
                  END IF               
          
                  IF( v_montos[ARR_CURR()].v_ind_mov_asociado = 0 )THEN # 999 saldo inicial
                     LET v_montos[ARR_CURR()].v_importe_separar = 0
                     LET v_montos[ARR_CURR()].v_rfc_asociado1   = ""
                  END IF
                  CONTINUE DIALOG    
               ELSE
                  CALL fn_mensaje("AVISO","Movimiento no elegible ","information")
                  LET v_montos[ARR_CURR()].v_ind_mov_asociado = 0               
                  NEXT FIELD ind_movi_asociado
               END IF
               IF(v_montos[ARR_CURR()].v_ind_mov_asociado = 0)THEN
                  LET v_montos[ARR_CURR()].v_importe_separar = 0
               END IF
        
         END INPUT

         BEFORE DIALOG
            # funcion que recupera los registros de movimientos del invadido de cta_movimiento

            DISPLAY BY NAME reg_expediente.nss_invadido,
                            reg_expediente.rfc_invadido,
                            reg_expediente.nombre_invadido

            # verifica el estado para ocultar acciones
            LET v_estado_preliquidar = ""
            LET v_datos_expediente.v_estado   = 0
            LET v_datos_expediente.v_id_expediente   = ""
         
            SELECT a.id_expediente_fondo72 ,
                   a.estado                ,
                   b.descripcion
            INTO   v_datos_expediente.v_id_expediente ,
                   v_estado_preliquidar               ,
                   v_datos_expediente.v_estado
            FROM   sep_expediente_fondo72 a ,
                   sep_estado_expediente b
            WHERE  a.id_expediente_fondo72 = p_id_expediente
            AND    a.estado = b.estado           
 
            IF v_estado_preliquidar = 75 THEN 
               CALL DIALOG.setActionHidden("guardar",1)
            END IF 
            CALL fn_recupera_montos(p_id_expediente,p_tpo_accion)RETURNING r_existen_registros
            IF NOT(r_existen_registros)THEN
               CALL fn_mensaje("AVISO","No se encontraron registros","information")
               EXIT DIALOG
            END IF
            
            
            # imprime datos generales expediente
            {
            --DISPLAY v_datos_expediente.v_caso_adai     TO caso_adai
            --DISPLAY v_datos_expediente.v_id_expediente TO id_expediente 
            DISPLAY v_datos_expediente.v_invadido      TO invadido
            DISPLAY v_datos_expediente.v_asociado      TO asociado
            DISPLAY v_datos_expediente.v_estado       TO desc_estado
            }            
            DISPLAY v_datos_expediente.v_estado       TO desc_estado
            LET v_monto_total_acciones = 0
            DISPLAY v_monto_total_acciones TO mto_total_sep
            DISPLAY " " TO reporte

         ON ACTION Guardar
            IF(reg_expediente.nss_asociado IS NULL OR reg_expediente.rfc_asociado IS NULL)THEN
               CALL fn_mensaje("AVISO","Capture asociado","information")
               CONTINUE DIALOG
            END IF
            CALL GET_FLDBUF(v_importe_separar) RETURNING v_montos[ARR_CURR()].v_importe_separar
            {            
            IF( v_montos[ARR_CURR()].v_importe_separar > v_montos[ARR_CURR()].v_importe )THEN
               CALL fn_mensaje("AVISO","'Aivs separar' excede monto importe","information")
               NEXT FIELD v_importe_separar 
            END IF
            }            
            # funcion que verifica si se ha seleccionado algun check
            CALL fn_verifica_seleccion() RETURNING v_seleccionado
            IF( v_seleccionado )THEN
               CALL fn_ventana_confirma(p_titulo_ventana,"¿Guardar previo?","question") RETURNING r_confirma
               IF( r_confirma )THEN
                  --DISPLAY "importe:",v_montos[ARR_CURR()].v_importe_separar
                  # llama funcion que actualiza la información            

                  UPDATE afi_fondo72 
                     SET ind_estado_cuenta = 1 
                   WHERE id_afi_fondo72 = (SELECT b.id_afi_fondo72_invadido 
                                             FROM sep_expediente_fondo72 b
                                            WHERE b.id_expediente_fondo72 = p_id_expediente) 

                  CALL fn_actualiza_registros_movimientos()
                 
                  SELECT estado
                    INTO v_edo_expediente
                    FROM sep_expediente
                   WHERE id_expediente = p_id_expediente
                  IF( v_edo_expediente <> 70 )THEN # no ejecuta maquinaria si el expediente ya esta en guardado previo
                     # señal guardar previo
                     LET v_senial = 70
                     # provisional en lo que se da de alta maquinaria                     
                     UPDATE sep_expediente_fondo72 
                     SET estado = 70                          
                     WHERE id_expediente_fondo72 = p_id_expediente                     
                     CALL fn_avanza_edo_expediente(p_id_expediente,v_senial) RETURNING r_exp_avanzado
                  END IF
                  IF(r_exp_avanzado)THEN
                     CALL fn_mensaje("AVISO","Guardado realizado correctamente","information")
                     ACCEPT DIALOG
                  ELSE
                     CONTINUE DIALOG
                  END IF
               ELSE
                  CONTINUE DIALOG
               END IF
            ELSE
               CALL fn_mensaje("AVISO","No se ha seleccionado ningún registro","information")
               CONTINUE DIALOG
            END IF
            
         ON ACTION Aceptar
            IF(reg_expediente.nss_asociado IS NULL OR reg_expediente.rfc_asociado IS NULL)THEN
               CALL fn_mensaje("AVISO","Capture asociado","information")
               CONTINUE DIALOG
            END IF
            CALL GET_FLDBUF(v_importe_separar) RETURNING v_montos[ARR_CURR()].v_importe_separar
            {            
            IF( v_montos[ARR_CURR()].v_importe_separar > v_montos[ARR_CURR()].v_importe )THEN
               CALL fn_mensaje("AVISO","'Aivs separar' excede monto importe","information")
               NEXT FIELD v_importe_separar 
            END IF
            }            
            # funcion que verifica si se ha seleccionado algun check
            CALL fn_verifica_seleccion() RETURNING v_seleccionado
            IF(v_seleccionado)THEN
               CALL fn_ventana_confirma(p_titulo_ventana,"¿Confirmar Operación?","question") RETURNING r_confirma
               IF(r_confirma)THEN
                  # llama funcion que actualiza la información

                  UPDATE afi_fondo72 
                     SET ind_estado_cuenta = 1 
                   WHERE id_afi_fondo72 = (SELECT b.id_afi_fondo72_invadido 
                                             FROM sep_expediente_fondo72 b
                                            WHERE b.id_expediente_fondo72 = p_id_expediente) 

                  CALL fn_actualiza_registros_movimientos()
                  # señal confirmar previo
                  SELECT a.estado 
                  INTO v_estado_preliquidar  
                  FROM sep_expediente_fondo72 a
                  WHERE id_expediente_fondo72 = p_id_expediente

                  CASE v_estado_preliquidar
                     WHEN 0
                        LET v_senial = 75
                        EXIT CASE                  
                     WHEN 70                  
                        LET v_senial = 75
                        EXIT CASE                  
                     WHEN 75
                        LET v_senial = 85
                        EXIT CASE                  
                  END CASE                  
                  
                  #provisional mientras se habilita maquinaria                  
                  UPDATE sep_expediente_fondo72 
                  SET estado = v_senial
                  WHERE id_expediente_fondo72 = p_id_expediente
                 
                  CALL fn_avanza_edo_expediente(p_id_expediente,v_senial) RETURNING r_exp_avanzado
                  IF(r_exp_avanzado)THEN
                     CALL fn_mensaje("AVISO","Confirmación realizada correctamente","information")
                     ACCEPT DIALOG
                  ELSE
                     CONTINUE DIALOG
                  END IF
               ELSE
                  CONTINUE DIALOG
               END IF
            ELSE
               CALL fn_mensaje("AVISO","No se ha seleccionado ningún registro","information")
               CONTINUE DIALOG
            END IF

         ON ACTION Calcular
            CALL GET_FLDBUF(v_importe_separar) RETURNING v_montos[ARR_CURR()].v_importe_separar
            {            
            IF(v_montos[ARR_CURR()].v_importe_separar > v_montos[ARR_CURR()].v_importe)THEN
               CALL fn_mensaje("AVISO","'Aivs separar' excede monto importe","information")
               NEXT FIELD v_importe_separar 
            END IF
            }            
            # raliza sumatoria de importe para recuperar total
            CALL fn_calculo_monto_separacion()RETURNING v_monto_total_acciones
            # genera reporte de movimientos actuales
            CALL fn_genera_reporte(p_id_expediente,"00000","00000","00000","REPORTE PREVIO") RETURNING v_genero_rpt
            IF(v_genero_rpt)THEN
               DISPLAY v_monto_total_acciones TO mto_total_sep
               --LET v_nom_aux_pdf = v_nom_reporte,".pdf"
               DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte||"','sep')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO reporte
            ELSE
               LET v_monto_total_acciones = 0
               DISPLAY v_monto_total_acciones TO mto_total_sep
            END IF

         ON ACTION Eliminar
            CALL fn_ventana_confirma(p_titulo_ventana,"¿Eliminar previo?","question") RETURNING r_confirma
            IF(r_confirma)THEN
               # felimina los registros de sep_movimiento_invadido y sep_saldo_inicial para el expediente
               CALL fn_elimina_reg_sep_mov(p_id_expediente)
               # señal REVERSAR PREVIO GUARDADO
               LET v_senial = 90
               CALL fn_avanza_edo_expediente(p_id_expediente,v_senial) RETURNING r_exp_avanzado
               IF(r_exp_avanzado)THEN
                  CALL fn_mensaje("AVISO","Previo eliminado correctamente","information")
                  ACCEPT DIALOG
               ELSE
                  CONTINUE DIALOG
               END IF
            END IF

         ON ACTION Cancelar
            # solo para el caso en que la accion es nuevo y se cancela la separación, se eliminan los registros que se 
            # depositaron en sep_movimiento_invadido para que no haya datos basura (para la accion recuperar, no importa, ya que solo se actualizó los movimientos)
            IF(p_tpo_accion = 'N')THEN
               CALL fn_elimina_reg_sep_mov(p_id_expediente)
            END IF
            EXIT DIALOG            

      END DIALOG

   CLOSE WINDOW vtna_separa_saldos

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF70                                                   #
#Descripcion       => Pantalla para la consulta de separacion de saldos        #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 08 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_consulta_saldos(p_id_expediente,p_tpo_accion)
DEFINE p_id_expediente        LIKE sep_expediente.id_expediente,
       p_tpo_accion           CHAR(1),
       r_existen_registros    BOOLEAN,
       v_genero_rpt           BOOLEAN,
       v_monto_total_acciones DECIMAL(16,6),
       
       # variables nuevas
       
       v_seleccionado         BOOLEAN,
       v_senial               SMALLINT,
       r_exp_avanzado         BOOLEAN,
       r_confirma             BOOLEAN,
       v_estado_preliquidar   SMALLINT,
       v_leyenda_rpt          CHAR(40)

   # Pantalla de separación de montos en modo consulta
   OPEN WINDOW vtna_separa_saldos WITH FORM v_ruta_ejecutable CLIPPED||"/SEPF703"

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

            LET v_estado_preliquidar        = 0
            LET v_datos_expediente.v_estado = ""

            SELECT  a.estado                ,
                    b.descripcion
             INTO   v_estado_preliquidar    ,
                    v_datos_expediente.v_estado
             FROM   sep_expediente_fondo72 a ,
                    sep_estado_expediente b
             WHERE  a.id_expediente_fondo72 = p_id_expediente
             AND    a.estado = b.estado           
             
            DISPLAY v_datos_expediente.v_estado TO desc_estado
            
            IF v_estado_preliquidar <> 75 OR p_tpo_accion <> "P"  THEN 
              CALL DIALOG.setActionHidden("aceptar",1)
            END IF              
            
            DISPLAY BY NAME reg_expediente.nss_invadido
            DISPLAY BY NAME reg_expediente.rfc_invadido
            DISPLAY BY NAME reg_expediente.nombre_invadido
            
            SELECT a.rfc_asociado,
                   a.nss_asociado,
                   a.nombre_asociado
            INTO   reg_expediente.rfc_asociado,
                   reg_expediente.nss_asociado,
                   reg_expediente.nombre_asociado
            FROM   sep_expediente_fondo72 a
            WHERE  a.id_expediente_fondo72 = p_id_expediente
            
            DISPLAY BY NAME reg_expediente.rfc_asociado,
                            reg_expediente.nss_asociado,
                            reg_expediente.nombre_asociado
            --DISPLAY reg_expediente.nss_invadido TO nss_asociado            
            --DISPLAY reg_expediente.nombre_invadido TO nombre_asociado            
            DISPLAY " " TO reporte
            DISPLAY 0 TO mto_total_sep

         ON ACTION calcular
            # raliza sumatoria de importe para recuperar total
            CALL fn_calculo_monto_separacion()RETURNING v_monto_total_acciones
            # genera reporte de movimientos actuales
            
            SELECT a.estado 
            INTO v_estado_preliquidar  
            FROM sep_expediente_fondo72 a
            WHERE id_expediente_fondo72 = p_id_expediente
            IF v_estado_preliquidar = 75 THEN 
               LET v_leyenda_rpt = "PREVIO LIQUIDACION"
            ELSE 
               LET v_leyenda_rpt = "CONSULTA"
            END IF            
            
            CALL fn_genera_reporte(p_id_expediente,"00000","00000","00000",v_leyenda_rpt) RETURNING v_genero_rpt
            IF(v_genero_rpt)THEN
               DISPLAY v_monto_total_acciones TO mto_total_sep
               DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte||"','sep')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO reporte
            ELSE
               LET v_monto_total_acciones = 0
               DISPLAY v_monto_total_acciones TO mto_total_sep
            END IF
            
        ON ACTION Aceptar
--            CALL GET_FLDBUF(v_importe_separar) RETURNING v_montos[ARR_CURR()].v_importe_separar
           -- IF(v_montos[ARR_CURR()].v_importe_separar > v_montos[ARR_CURR()].v_importe)THEN
               --CALL fn_mensaje("AVISO","'Aivs separar' excede monto importe","information")
               --NEXT FIELD v_importe_separar 
            --END IF
            # funcion que verifica si se ha seleccionado algun check
            CALL fn_verifica_seleccion() RETURNING v_seleccionado
            IF(v_seleccionado)THEN
               CALL fn_ventana_confirma(p_titulo_ventana,"Confirmar Operación?","question") RETURNING r_confirma
               IF(r_confirma)THEN
                  # llama funcion que actualiza la información

                 UPDATE afi_fondo72 
                 SET ind_estado_cuenta = 1 
                 WHERE id_afi_fondo72 = (SELECT b.id_afi_fondo72_invadido 
                                         FROM   sep_expediente_fondo72 b
                                         WHERE  b.id_expediente_fondo72 = p_id_expediente)  
                  
                  CALL fn_actualiza_registros_movimientos()
                  
                  # señal confirmar previo
                  SELECT a.estado 
                  INTO v_estado_preliquidar  
                  FROM sep_expediente_fondo72 a
                  WHERE id_expediente_fondo72 = p_id_expediente

                  CASE v_estado_preliquidar
                  WHEN 75
                     LET v_senial = 85
                    EXIT CASE                     
                  WHEN 80
                     LET v_senial = 85
                    EXIT CASE                  
                  END CASE                  
                  
                  #provisional mientras se habilita maquinaria                  

                  CALL fn_ejecuta_preliquidacion_fondo72(2235,1,p_id_expediente) RETURNING r_exp_avanzado
                  IF NOT(r_exp_avanzado)THEN
                  ELSE
                     CONTINUE DISPLAY 
                  END IF
                  
                  CALL fn_ejecuta_liquidacion_fondo72(2235,2,p_id_expediente) RETURNING r_exp_avanzado
                  IF NOT(r_exp_avanzado)THEN
                     CALL fn_mensaje("AVISO","Separacion Aplicada correctamente","information")
                     
                     UPDATE afi_fondo72 
                     SET ind_estado_cuenta = 0 
                     WHERE id_afi_fondo72 = (SELECT b.id_afi_fondo72_invadido 
                                             FROM   sep_expediente_fondo72 b
                                             WHERE  b.id_expediente_fondo72 = p_id_expediente)                       
                     EXIT DISPLAY                                             
                  ELSE
                     CONTINUE DISPLAY 
                  END IF
               ELSE
                  CONTINUE DISPLAY 
               END IF
            ELSE
               CALL fn_mensaje("AVISO","No se ha seleccionado ningún registro","information")
               CONTINUE DISPLAY 
            END IF
         ON ACTION cancelar
            EXIT DISPLAY

      END DISPLAY

   CLOSE WINDOW vtna_separa_saldos

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF70                                                   #
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
#Programa          => SEPF70                                                   #
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
#Programa          => SEPF70                                                   #
#Descripcion       => funcion para actualizar los registros de la tabla        #
#                     sep_movimiento_invadido                                  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 08 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_actualiza_registros_movimientos()
DEFINE v_consulta STRING,
       v_indice   INTEGER,
       v_id_afi_fondo72_asociado LIKE afi_fondo72.id_afi_fondo72


DEFINE vf_id_expediente DECIMAL(9,0)       

   # Actualiza los movimientos de separación
   LET v_consulta = "\n UPDATE sep_movimiento_invadido_fondo72",
                    "\n    SET ind_mov_asociado = ? ,",
                    "\n        rfc_asociado     = ?  ",
                    "\n  WHERE id_expediente_fondo72 = ?",
                    "\n    AND id_sep_movimiento_invadido_fondo72 = ?"
   PREPARE prp_act_registros_mov_sep FROM v_consulta
   
   LET v_consulta = "\n UPDATE cta_his_fondo72  ",
                    "\n    SET ind_verifica = ? ",
                    "\n  WHERE id_cta_his_fondo72 = ?"
   PREPARE prp_act_registros_his FROM v_consulta
   
   LET v_consulta = "\n UPDATE sep_saldo_inicial",
                    "\n    SET mto_aivs_separado = ?",
                    "\n  WHERE id_sep_movimiento_invadido = ?"
   PREPARE prp_act_registros_sdo_inicial FROM v_consulta
   
   FOR v_indice = 1 TO v_montos.getLength()
      # actualiza los registros de sep_movimiento_invadido, para el caso de selecion de montos
      EXECUTE prp_act_registros_mov_sep USING v_montos[v_indice].v_ind_mov_asociado,
                                              v_montos[v_indice].v_rfc_asociado1,
                                              v_montos[v_indice].v_id_expediente,
                                              v_montos[v_indice].v_id_sep_mov_inv

             LET vf_id_expediente = v_montos[v_indice].v_id_expediente

      #por utilizacion de rowid, evita actualizar un registro que no se debe
      IF v_montos[v_indice].v_clave_mov <> "SEP"  THEN --.v_empresa <> "SACI" THEN      
      # actualiza los registros de cta_his_fondo72, para el caso de selecion de montos
      EXECUTE prp_act_registros_his USING     v_montos[v_indice].v_ind_mov_asociado,
                                              v_montos[v_indice].v_id_cta_his_fondo72
      END IF                                              
                                              
      IF(v_montos[v_indice].v_clave_mov = "AA") THEN # 999 -> saldo inicial
         # actualiza la tabla sep_saldo_inicial solo para saldos iniciales, en el caso de que se introduzca una cantidad
         --EXECUTE prp_act_registros_sdo_inicial USING v_montos[v_indice].v_importe_separar,
         --                                            v_montos[v_indice].v_id_sep_mov_inv
      END IF
   END FOR


   SELECT id_afi_fondo72
     INTO v_id_afi_fondo72_asociado
     FROM afi_fondo72
    WHERE nss = reg_expediente.nss_asociado
      AND rfc = reg_expediente.rfc_asociado
      
   -- actualiza los datos del asociado   
   UPDATE sep_expediente_fondo72
   SET    nss_asociado = reg_expediente.nss_asociado,
          rfc_asociado = reg_expediente.rfc_asociado,
          nombre_asociado = reg_expediente.nombre_asociado,
          id_afi_fondo72_asociado = v_id_afi_fondo72_asociado
   WHERE  id_expediente_fondo72 = vf_id_expediente          
   
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF70                                                   #
#Descripcion       => Funcion para verificar el saldo vs mov elegidos          #
#                     sep_movimiento_invadido_fondo72                          #
#Autor             => Jesus David Yañez Moreno                                 #
#Fecha inicio      => 23 de Agosto de 2013                                     #
################################################################################
FUNCTION fn_verifica_saldo_movimientos()
DEFINE v_indice   INTEGER
DEFINE v_saldo_mov_separar DECIMAL(16,2),
       v_saldo_cta         DECIMAL(16,2),
       v_verifica          SMALLINT
       
   LET v_verifica  = 1
   LET v_saldo_cta = 0
   
   SELECT sum(a.importe) 
     INTO v_saldo_cta 
     FROM cta_fondo72 a
    WHERE a.id_afi_fondo72 = v_id_afi_fondo72   

   LET v_saldo_mov_separar = 0
 
   FOR v_indice = 1 TO v_montos.getLength()
      IF v_montos[v_indice].v_ind_mov_asociado = 1 THEN
       LET v_saldo_mov_separar = v_saldo_mov_separar + v_montos[v_indice].v_importe
      END IF       
   END FOR

   IF v_saldo_mov_separar > v_saldo_cta THEN 
      LET v_verifica = 0   
   END IF
   
RETURN v_verifica
 
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF70                                                   #
#Descripcion       => Funcion para eliminar los registros de la tabla          #
#                     sep_movimiento_invadido                                  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 09 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_elimina_reg_sep_mov(p_id_expediente)
DEFINE p_id_expediente LIKE sep_expediente.id_expediente
    
--   DELETE
     --FROM sep_saldo_inicial
    --WHERE id_sep_movimiento_invadido IN (SELECT id_sep_movimiento_invadido
                                           --FROM sep_movimiento_invadido
                                          --WHERE id_expediente = p_id_expediente)    
                                          
   UPDATE cta_his_fondo72 
   SET    ind_verifica = 0 
   WHERE  id_cta_his_fondo72 IN (SELECT b.id_cta_his_fondo72  
                                 FROM   sep_movimiento_invadido_fondo72 b   
                                 WHERE  b.id_expediente_fondo72 = p_id_expediente
                                   AND  b.ind_mov_asociado = 1)

   DELETE
     FROM sep_movimiento_invadido_fondo72
    WHERE id_expediente_fondo72 = p_id_expediente

   UPDATE afi_fondo72 
      SET ind_estado_cuenta = 0 
    WHERE id_afi_fondo72 = (SELECT b.id_afi_fondo72_invadido 
                            FROM   sep_expediente_fondo72 b
                            WHERE  b.id_expediente_fondo72 = p_id_expediente) 
    
   DELETE 
     FROM sep_expediente_fondo72
    WHERE id_expediente_fondo72 = p_id_expediente

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF70                                                   #
#Descripcion       => Función para calcular el monto a separar                 #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 08 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_calculo_monto_separacion()
DEFINE v_monto_total_separacion DECIMAL(16,6),
       v_indice                 INTEGER

   LET v_monto_total_separacion = 0
   # realiza sumatoria para obtener total de importe 
   FOR v_indice = 1 TO v_montos.getLength()
      LET v_monto_total_separacion = v_monto_total_separacion + v_montos[v_indice].v_importe_separar 
   END FOR
   RETURN v_monto_total_separacion
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF70                                                   #
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
          v_id_cta_his_fondo72 DECIMAL(9,0),       
          v_nombre          CHAR(40),
          v_folio           DECIMAL(9,0),
          v_ejercicio       char(4),
          v_clave_mov       CHAR(3),
          v_empresa         CHAR(40),
          v_bimestres       SMALLINT,
          v_importe         DECIMAL(10,2),
          v_ind_verifica    DECIMAL(9,0),   
          v_rfc_asociado    CHAR(13)          
       END RECORD,
       v_ind_mov_asociado_aux   SMALLINT,
       v_montos_sep RECORD
          v_id_expediente    DECIMAL(9,0)  ,
          v_ind_mov_asociado DECIMAL(9,0)  ,
          v_nombre           CHAR(40)      ,
          v_folio            DECIMAL(9,0)  ,
          v_ejercicio        CHAR(04)      ,
          v_clave_mov        CHAR(03)      ,
          v_empresa          CHAR(40)      ,
          v_bimestres        SMALLINT      ,
          v_importe          DECIMAL(10,2) ,
          v_rfc_asociado1    char(13)      ,
          v_id_sep_mov_inv   DECIMAL(9,0)  ,
          v_id_cta_his_fondo72 DECIMAL(9,0)
       END RECORD,
       v_indice               INTEGER,
       v_max_f_liquida        DATE,--LIKE sep_movimiento_invadido.f_liquida
       v_existen_registros    BOOLEAN,
       v_id_afi_exp           DECIMAL(9,0) -- id de afi o id de expediente
       
       
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

   # se hace una copia de los montos de la cuenta a las tablas de separación

   
   LET v_consulta = "\n INSERT INTO sep_movimiento_invadido_fondo72 ",
                    "\n (id_sep_movimiento_invadido_fondo72, ",
                    "\n  id_expediente_fondo72, ",
                    "\n  id_cta_his_fondo72,    ",
                    "\n  nombre          , ",
                    "\n  folio           , ",
                    "\n  ejercicio       , ",
                    "\n  clave_mov       , ",
                    "\n  empresa         , ",
                    "\n  bimestres       , ",
                    "\n  importe         , ",
                    "\n  ind_mov_asociado, ",
                    "\n  rfc_asociado)     ",
                    "\n VALUES(seq_sep_movimiento_invadido_fondo72.NEXTVAL,?,?,?,?,?,?,?,?,?,?,?) "

   PREPARE prp_ins_montos FROM v_consulta

   # consulta registros para mostrar en pantalla de separación
   LET v_consulta = "\n SELECT DISTINCT a.id_expediente_fondo72,",
                    "\n        a.ind_mov_asociado  ,",
                    "\n        a.nombre            ,",
                    "\n        a.folio             ,",
                    "\n        a.ejercicio         ,",
                    "\n        a.clave_mov         ,",
                    "\n        a.empresa           ,",
                    "\n        a.bimestres         ,",
                    "\n        a.importe           ,",
                    "\n        a.rfc_asociado      ,",
                    "\n        a.id_sep_movimiento_invadido_fondo72, ", 
                    "\n        a.id_cta_his_fondo72 ", 
                    "\n FROM sep_movimiento_invadido_fondo72 a ",
                    "\n WHERE a.id_expediente_fondo72 = ?      ",
                    "\n ORDER BY a.ejercicio "
   PREPARE prp_rec_montos_sep FROM v_consulta
   DECLARE cur_rec_montos_sep CURSOR FOR prp_rec_montos_sep

   --LET v_consulta = "\n SELECT mto_aivs_separado",
                    --"\n   FROM sep_saldo_inicial",
                    --"\n  WHERE id_sep_movimiento_invadido = ?"
   --PREPARE prp_rec_sdo_ini FROM v_consulta
         
   CASE p_tpo_accion

      WHEN "N" # para la opcion nuevo (separación para nuevo expediente)
         LET v_ind_mov_asociado_aux = 0
         
         #inserta el registro de expediente nuevo
         INSERT INTO sep_expediente_fondo72
         (id_expediente_fondo72,
          id_afi_fondo72_invadido,
          rfc_invadido,
          id_afi_fondo72_asociado,
          nss_asociado,
          rfc_asociado,
          nombre_asociado,
          estado) 
         VALUES (p_id_expediente,
                 v_id_afi_fondo72,
                 reg_expediente.rfc_invadido,
                 "",
                 reg_expediente.nss_asociado,
                 reg_expediente.rfc_asociado,
                 reg_expediente.nombre_asociado,
                 "0")

            # recupera los montos de cta_his_fondo72 y los pasa a la tabla sep_movimiento_invadido_fondo72
            
            LET v_consulta = " SELECT  a.id_cta_his_fondo72 , ",
                             " a.nombre    , ",
                             " a.folio     , ",
                             " a.ejercicio , ",
                             " a.clave_mov , ",
                             " a.empresa   , ",
                             " a.bimestres , ",
                             " a.importe   , ",
                             " '0'         , ",
                             " '' ",
                      " FROM cta_his_fondo72 a, ",
                           " afi_fondo72     b ",
                      " WHERE a.nss = b.nss ",
                        " AND a.rfc = b.rfc ",
                        " AND b.id_afi_fondo72 =  ? ",
                        " AND a.ind_verifica = 0 ",
                      " UNION ALL ",
                      --" SELECT a.rowid , ",
                      " SELECT a.id_referencia , ",
                             " b.nombre , ",
                             " a.folio_liquida , ",
                             " YEAR(a.f_liquida) , ",
                             " UPPER(c.modulo_cod) , ",
                             " a.origen , ",                             
                             --" 'SACI' , ",
                             " 0            , ",
                             " a.importe    , ",
                             " '0'          , ",
                             " '' ",
                      " FROM cta_fondo72 a, ",
                           " afi_fondo72 b , ",
                           " cat_movimiento c ",
                      " WHERE b.id_afi_fondo72 = ? ",
                        " AND a.id_afi_fondo72 = b.id_afi_fondo72 ",
                        " AND a.movimiento = c.movimiento ",
                        " AND a.movimiento <> 999 ", -- no se incluye saldo inicial
                        " ORDER BY 3 "

                    LET v_id_afi_exp = v_id_afi_fondo72                             
   
         PREPARE prp_rec_montos_invadido FROM v_consulta
         DECLARE cur_rec_montos_invadido CURSOR FOR prp_rec_montos_invadido
         FOREACH cur_rec_montos_invadido USING v_id_afi_exp,v_id_afi_exp
                                          INTO v_montos_cta_mov.*
            # realiza copia de los montos del invadido
            EXECUTE prp_ins_montos USING p_id_expediente                 ,
                                         v_montos_cta_mov.v_id_cta_his_fondo72 ,            
                                         v_montos_cta_mov.v_nombre       ,
                                         v_montos_cta_mov.v_folio        ,
                                         v_montos_cta_mov.v_ejercicio    ,
                                         v_montos_cta_mov.v_clave_mov    ,
                                         v_montos_cta_mov.v_empresa      ,
                                         v_montos_cta_mov.v_bimestres    ,
                                         v_montos_cta_mov.v_importe      ,
                                         v_montos_cta_mov.v_ind_verifica ,
                                         v_montos_cta_mov.v_rfc_asociado
                                         
         END FOREACH
         FREE cur_rec_montos_invadido

         # recupera los montos para mostrar en la separación
         LET v_indice = 1
         FOREACH cur_rec_montos_sep USING p_id_expediente
                                     INTO v_montos_sep.*
            LET v_montos[v_indice].v_num              = v_indice
            LET v_montos[v_indice].v_id_expediente    = v_montos_sep.v_id_expediente
            LET v_montos[v_indice].v_ind_mov_asociado = v_montos_sep.v_ind_mov_asociado
            LET v_montos[v_indice].v_nombre           = v_montos_sep.v_nombre
            LET v_montos[v_indice].v_folio            = v_montos_sep.v_folio
            LET v_montos[v_indice].v_ejercicio        = v_montos_sep.v_ejercicio
            LET v_montos[v_indice].v_clave_mov        = v_montos_sep.v_clave_mov
            LET v_montos[v_indice].v_empresa          = v_montos_sep.v_empresa
            LET v_montos[v_indice].v_bimestres        = v_montos_sep.v_bimestres
            LET v_montos[v_indice].v_importe          = v_montos_sep.v_importe
            LET v_montos[v_indice].v_rfc_asociado1    = v_montos_sep.v_rfc_asociado1
            LET v_montos[v_indice].v_id_sep_mov_inv   = v_montos_sep.v_id_sep_mov_inv
            LET v_montos[v_indice].v_id_cta_his_fondo72  = v_montos_sep.v_id_cta_his_fondo72
            
            IF(v_montos[v_indice].v_ind_mov_asociado = 1)THEN
               IF(v_montos[v_indice].v_clave_mov = "AA")THEN
                  # para el caso de saldo inicial(999) se recupera el monto de la tabla sep_saldo_inicial
                  --EXECUTE prp_rec_sdo_ini USING v_montos[v_indice].v_id_sep_mov_inv
                  --                         INTO v_montos[v_indice].v_importe_separar
                  
               ELSE
                  LET v_montos[v_indice].v_importe_separar  = v_montos[v_indice].v_importe
               END IF
            ELSE
               LET v_montos[v_indice].v_importe_separar  = 0 # inicializa en 0 el valor de separación
            END IF
            
            LET v_indice = v_indice + 1
            LET v_existen_registros = TRUE
         END FOREACH
         FREE cur_rec_montos_sep
         
      WHEN "R" # recuperar y separar

         LET v_ind_mov_asociado_aux = 0

         # recupera los montos para mostrar en la separación
         LET v_indice = 1

         FOREACH cur_rec_montos_sep USING p_id_expediente
                                     INTO v_montos_sep.*
            LET v_montos[v_indice].v_num              = v_indice
            LET v_montos[v_indice].v_id_expediente    = v_montos_sep.v_id_expediente
            LET v_montos[v_indice].v_ind_mov_asociado = v_montos_sep.v_ind_mov_asociado
            LET v_montos[v_indice].v_nombre           = v_montos_sep.v_nombre
            LET v_montos[v_indice].v_folio            = v_montos_sep.v_folio
            LET v_montos[v_indice].v_ejercicio              = v_montos_sep.v_ejercicio
            LET v_montos[v_indice].v_clave_mov        = v_montos_sep.v_clave_mov
            LET v_montos[v_indice].v_empresa          = v_montos_sep.v_empresa
            LET v_montos[v_indice].v_bimestres        = v_montos_sep.v_bimestres
            LET v_montos[v_indice].v_importe          = v_montos_sep.v_importe
            LET v_montos[v_indice].v_rfc_asociado1     = v_montos_sep.v_rfc_asociado1
            LET v_montos[v_indice].v_id_sep_mov_inv   = v_montos_sep.v_id_sep_mov_inv
            LET v_montos[v_indice].v_id_cta_his_fondo72  = v_montos_sep.v_id_cta_his_fondo72
            
            IF(v_montos[v_indice].v_ind_mov_asociado = 1)THEN
               IF(v_montos[v_indice].v_clave_mov = "AA") THEN
                  # para el caso de saldo inicial(999) se recupera el monto de la tabla sep_saldo_inicial
                  --EXECUTE prp_rec_sdo_ini USING v_montos[v_indice].v_id_sep_mov_inv
                  --                         INTO v_montos[v_indice].v_importe_separar
                  
               ELSE
                  LET v_montos[v_indice].v_importe_separar  = v_montos[v_indice].v_importe
               END IF
            ELSE
               LET v_montos[v_indice].v_importe_separar  = 0 # inicializa en 0 el valor de separación
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
            LET v_montos[v_indice].v_nombre           = v_montos_sep.v_nombre
            LET v_montos[v_indice].v_folio            = v_montos_sep.v_folio
            LET v_montos[v_indice].v_ejercicio              = v_montos_sep.v_ejercicio
            LET v_montos[v_indice].v_clave_mov        = v_montos_sep.v_clave_mov
            LET v_montos[v_indice].v_empresa          = v_montos_sep.v_empresa
            LET v_montos[v_indice].v_bimestres        = v_montos_sep.v_bimestres
            LET v_montos[v_indice].v_importe          = v_montos_sep.v_importe
            LET v_montos[v_indice].v_rfc_asociado1    = v_montos_sep.v_rfc_asociado1
            LET v_montos[v_indice].v_id_sep_mov_inv   = v_montos_sep.v_id_sep_mov_inv
            LET v_montos[v_indice].v_id_cta_his_fondo72  = v_montos_sep.v_id_cta_his_fondo72
            
            IF(v_montos[v_indice].v_ind_mov_asociado = 1)THEN
               IF(v_montos[v_indice].v_clave_mov = "AA") THEN
                  # para el caso de saldo inicial(999) se recupera el monto de la tabla sep_saldo_inicial
                  --EXECUTE prp_rec_sdo_ini USING v_montos[v_indice].v_id_sep_mov_inv
                                           --INTO v_montos[v_indice].v_importe_separar
               ELSE
                  LET v_montos[v_indice].v_importe_separar  = v_montos[v_indice].v_importe
               END IF
            ELSE
               LET v_montos[v_indice].v_importe_separar  = 0 # inicializa en 0 el valor de separación
            END IF
            
            LET v_indice = v_indice + 1
            LET v_existen_registros = TRUE
         END FOREACH
         FREE cur_rec_montos_sep
       WHEN "P" # consulta separación
         # recupera los montos para mostrar en la separación
         LET v_indice = 1
         FOREACH cur_rec_montos_sep USING p_id_expediente
                                     INTO v_montos_sep.*
            LET v_montos[v_indice].v_num              = v_indice
            LET v_montos[v_indice].v_id_expediente    = v_montos_sep.v_id_expediente
            LET v_montos[v_indice].v_ind_mov_asociado = v_montos_sep.v_ind_mov_asociado
            LET v_montos[v_indice].v_nombre           = v_montos_sep.v_nombre
            LET v_montos[v_indice].v_folio            = v_montos_sep.v_folio
            LET v_montos[v_indice].v_ejercicio              = v_montos_sep.v_ejercicio
            LET v_montos[v_indice].v_clave_mov        = v_montos_sep.v_clave_mov
            LET v_montos[v_indice].v_empresa          = v_montos_sep.v_empresa
            LET v_montos[v_indice].v_bimestres        = v_montos_sep.v_bimestres
            LET v_montos[v_indice].v_importe          = v_montos_sep.v_importe
            LET v_montos[v_indice].v_rfc_asociado1    = v_montos_sep.v_rfc_asociado1
            LET v_montos[v_indice].v_id_sep_mov_inv   = v_montos_sep.v_id_sep_mov_inv
            LET v_montos[v_indice].v_id_cta_his_fondo72  = v_montos_sep.v_id_cta_his_fondo72
            
            IF(v_montos[v_indice].v_ind_mov_asociado = 1)THEN
               IF(v_montos[v_indice].v_clave_mov = "AA") THEN
                  # para el caso de saldo inicial(999) se recupera el monto de la tabla sep_saldo_inicial
                  --EXECUTE prp_rec_sdo_ini USING v_montos[v_indice].v_id_sep_mov_inv
                                           --INTO v_montos[v_indice].v_importe_separar
               ELSE
                  LET v_montos[v_indice].v_importe_separar  = v_montos[v_indice].v_importe
               END IF
            ELSE
               LET v_montos[v_indice].v_importe_separar  = 0 # inicializa en 0 el valor de separación
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
#Programa          => SEPF70                                                   #
#Descripcion       => Funcion para generar reporte                             #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 09 de Octubre de 2012                                    #
################################################################################
FUNCTION fn_genera_reporte(p_id_expediente,v_rpt_pid,v_rpt_proceso_cod,v_rpt_opera_cod,v_tipo_rpt)
DEFINE p_id_expediente LIKE sep_expediente.id_expediente,
       v_indice        INTEGER,
       v_consulta      STRING,
       v_encabezado    RECORD
          v_id_expediente  LIKE sep_expediente.id_expediente,
          v_rfc_inv        CHAR(13),
          v_nombre_inv     CHAR(40),
          v_rfc_aso        CHAR(13),
          v_tipo_reporte   CHAR(40)
       END RECORD,
       v_tipo_rpt      CHAR(40),
       v_rpt_pid       INTEGER ,
       v_rpt_proceso_cod INTEGER,
       v_rpt_opera_cod   SMALLINT,       
       v_tpo_nss  LIKE sep_nss_expediente.tipo_nss,
       r_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_ruta_lst        LIKE seg_modulo.ruta_listados,
       v_id_nom_rpt      CHAR(5),
       v_genero_rpt      BOOLEAN,
       v_manejador_rpt   OM.SaxDocumentHandler

   LET v_encabezado.v_tipo_reporte = v_tipo_rpt
       
   LET v_genero_rpt = FALSE
   CALL fn_rutas("sep") RETURNING r_ruta_ejecutable, r_ruta_lst
   IF(fgl_report_loadCurrentSettings(r_ruta_ejecutable CLIPPED||"/SEPF701.4rp"))THEN
      # Salida del reporte
      CALL fgl_report_selectDevice("PDF")
      IF v_rpt_pid = "00000" THEN
         LET v_id_nom_rpt = p_id_expediente
      ELSE 
         LET v_id_nom_rpt = v_rpt_pid
      END IF       
      LET v_nom_reporte = p_usuario_cod CLIPPED, "-SEPP71-", 
                          v_id_nom_rpt USING "&&&&&", "-", 
                          v_rpt_proceso_cod USING "&&&&&", "-", 
                          v_rpt_opera_cod USING "&&&&&",
                          ".pdf"
      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(r_ruta_lst CLIPPED||"/"||v_nom_reporte)
         
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      START REPORT fn_genera_rpt_sep_montos TO XML HANDLER v_manejador_rpt
         # datos de encabezado
         LET v_consulta = "\n SELECT exp.rfc_invadido,",
                          "\n        exp.rfc_asociado,",
                          "\n        afi.nombre",
                          "\n   FROM sep_expediente_fondo72 exp LEFT OUTER JOIN afi_fondo72 afi",
                          "\n     ON afi.id_afi_fondo72 = exp.id_afi_fondo72_invadido",
                          "\n  WHERE exp.id_expediente_fondo72 = ?"
         PREPARE prp_rec_datos_enc FROM v_consulta
         LET v_tpo_nss = 1
         LET v_encabezado.v_id_expediente = p_id_expediente
         # recupera datos de invadido
         EXECUTE prp_rec_datos_enc USING p_id_expediente
                                    INTO v_encabezado.v_rfc_inv,
                                         v_encabezado.v_rfc_aso,
                                         v_encabezado.v_nombre_inv
      
          LET v_encabezado.v_id_expediente = p_id_expediente

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

FUNCTION fn_recupera_datos_iniciales(f_id_afi)
#--------------------------------------------

DEFINE f_id_afi DECIMAL(9,0)
SELECT a.nss , 
       a.rfc , 
       a.nombre
INTO   reg_expediente.nss_invadido ,
       reg_expediente.rfc_invadido ,
       reg_expediente.nombre_invadido
FROM   afi_fondo72 a
WHERE  id_afi_fondo72 = f_id_afi

DISPLAY BY NAME reg_expediente.nss_invadido,
                reg_expediente.rfc_invadido,
                reg_expediente.nombre_invadido
                
END FUNCTION


FUNCTION fn_recupera_datos_asociado(p_id_afi_fondo72,p_id_expediente)
DEFINE p_id_afi_fondo72 LIKE afi_fondo72.id_afi_fondo72
DEFINE p_id_expediente DECIMAL(9,0)

   INITIALIZE reg_expediente.nss_asociado,
              reg_expediente.rfc_asociado,
              reg_expediente.nombre_asociado TO NULL
              
   SELECT nss_asociado, 
          rfc_asociado, 
          nombre_asociado
     INTO reg_expediente.nss_asociado,
          reg_expediente.rfc_asociado,
          reg_expediente.nombre_asociado
     FROM sep_expediente_fondo72
    WHERE id_expediente_fondo72 = p_id_expediente 
    AND   id_afi_fondo72_invadido = p_id_afi_fondo72

   
   DISPLAY reg_expediente.nss_asociado TO nss_asociado
   DISPLAY reg_expediente.rfc_asociado TO rfc_asociado
   DISPLAY reg_expediente.nombre_asociado TO nombre_asociado
                
END FUNCTION

FUNCTION fn_muestra_nueva_cuenta()
#----------------------------------

DEFINE f_anio_rfc smallint
DEFINE rfc_modificado CHAR(013)
DEFINE v_ind               SMALLINT   ,
       v_desc              CHAR(45)   ,
       v_caracter_invalido CHAR(010)  ,
       v_rfc_original      CHAR(010)  ,
       v_rfc_homoclave     CHAR(013)

--LET reg_expediente.nss_asociado = reg_expediente.nss_invadido

-- se asigna ceros al nss asociado por requerimiento PRODINF-97
LET reg_expediente.nss_asociado = "00000000000"
LET f_anio_rfc = reg_expediente.rfc_invadido[5,6]

WHILE TRUE

   LET f_anio_rfc = f_anio_rfc + 1 

   LET rfc_modificado[1,4] = reg_expediente.rfc_invadido[1,4]
   LET rfc_modificado[5,6] = f_anio_rfc USING "&&"
   LET rfc_modificado[7,10] = reg_expediente.rfc_invadido[7,10]

   PREPARE prp_calcula_homoclave 
   FROM "EXECUTE PROCEDURE sp_calcula_homoclave_rfc(?,?)"  
   
   EXECUTE prp_calcula_homoclave USING reg_expediente.nombre_invadido ,
                                       rfc_modificado
                                 INTO  v_ind               ,
                                       v_desc              ,
                                       v_caracter_invalido ,
                                       v_rfc_original      ,
                                       v_rfc_homoclave
   IF v_ind = 1 THEN 
      CALL fn_mensaje("SEP FONDO 72",v_desc||":  "||v_caracter_invalido,"about")
      RETURN 0     
   END IF
   
   SELECT "OK" 
   FROM   afi_fondo72 a 
   WHERE  a.nss = reg_expediente.nss_asociado 
   AND    a.rfc = v_rfc_homoclave
   GROUP BY 1   

   IF STATUS = NOTFOUND  THEN 
      EXIT WHILE
   ELSE    
      CONTINUE WHILE
   END IF   
 
END WHILE

LET reg_expediente.rfc_asociado = v_rfc_homoclave
LET reg_expediente.nombre_asociado = reg_expediente.nombre_invadido

DISPLAY BY NAME reg_expediente.nss_asociado,
                reg_expediente.rfc_asociado,
                reg_expediente.nombre_asociado
      RETURN 1      
END FUNCTION

FUNCTION fn_valida_existe(f_id_afi)
#----------------------------------

DEFINE f_id_afi DECIMAL(9,0) 
DEFINE f_valida SMALLINT


       LET f_valida = 0       
       SELECT "OK"
       FROM   sep_expediente_fondo72 a
       WHERE  id_afi_fondo72_invadido = f_id_afi
       AND    a.estado IN (70,75,80)  -- liquidado       
       GROUP BY 1       
          
       IF STATUS <> NOTFOUND THEN
          LET f_valida = 0
       ELSE 
          LET f_valida = 1       
       END IF 
RETURN f_valida

END FUNCTION



################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF70                                                   #
#Descripcion       => Funcion para generar reporte                             #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 09 de Octubre de 2012                                    #
################################################################################
REPORT fn_genera_rpt_sep_montos(p_encabezado,p_montos)
DEFINE p_encabezado    RECORD
          v_id_expediente  LIKE sep_expediente.id_expediente,
          v_rfc_inv        CHAR(13),
          v_nombre_inv     CHAR(40),
          v_rfc_aso        CHAR(13),
          v_tipo_reporte   CHAR(40)
       END RECORD,
       p_montos RECORD
          v_num              INTEGER,
          v_id_expediente    DECIMAL(9,0),
          v_ind_mov_asociado DECIMAL(9,0),
          v_nombre           CHAR(30),
          v_folio            DECIMAL(9,0),
          v_ejercicio        CHAR(4),
          v_clave_mov        CHAR(3),
          v_empresa          CHAR(30),
          v_bimestres        SMALLINT,
          v_importe          DECIMAL(10,2),
          v_importe_separar  DECIMAL(6,2),
          v_rfc_asociado1     CHAR(13),
          v_id_sep_mov_inv   DECIMAL(9,0),
          v_cta_his_fondo72   DECIMAL(9,0)
       END RECORD,
       v_fecha_actual DATE,
       v_hora_actual  DATETIME HOUR TO SECOND,
       v_total_pesos  LIKE cta_movimiento.monto_pesos,
       v_total_aivs   LIKE cta_movimiento.monto_acciones,
       v_pagina       SMALLINT

   FORMAT

      FIRST PAGE HEADER
         
         LET v_fecha_actual = TODAY
         LET v_hora_actual  = CURRENT HOUR TO SECOND
         PRINTX v_fecha_actual,v_hora_actual,p_encabezado.*

      ON EVERY ROW 
         PRINTX p_montos.*
      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina
      ON LAST ROW
         LET v_total_pesos = SUM(p_montos.v_importe)
         LET v_total_aivs  = SUM(p_montos.v_importe) 
         PRINTX v_total_pesos, v_total_aivs
END REPORT


################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL61                                                   #
#Descripcion       => funcion que ejecuta lanzado de iquidación                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 15 Octubre 2012                                          #
################################################################################
FUNCTION fn_ejecuta_preliquidacion_fondo72(v_proceso_cod,v_opera_cod,p_id_expediente)
DEFINE r_resultado_opera SMALLINT,
       v_continuar       BOOLEAN,
       v_folio           LIKE glo_folio.folio,
       v_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo,
       r_pid             LIKE glo_pid.pid,
       v_ruta_bin_aux    LIKE seg_modulo.ruta_bin,
       v_comando         STRING ,
       v_proceso_cod     INTEGER,
       v_opera_cod       SMALLINT,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       p_id_expediente   DECIMAL(9,0),
       v_genero_rpt      BOOLEAN       

   CALL fn_valida_operacion(0,v_proceso_cod,v_opera_cod) RETURNING r_resultado_opera
   # Se verifica si la operacion es valida
   IF(r_resultado_opera <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      # indica que continua en pantalla de detalle
      LET v_continuar = TRUE 
      RETURN v_continuar      
   END IF

   LET v_folio = 0
   LET v_nom_archivo = "NA"
   CALL fn_genera_pid(v_proceso_cod,v_opera_cod,p_usuario_cod) 
          RETURNING r_pid 
   LET v_pid = r_pid          
   CALL fn_inicializa_proceso(r_pid,         # pid
                              v_proceso_cod, # proceso
                              v_opera_cod,   # operacion
                              0,             # folio
                              "SEPF70",      # programa
                              v_nom_archivo, # archivo
                              p_usuario_cod) # usuario
              RETURNING r_resultado_opera
   IF( r_resultado_opera <> 0 ) THEN
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      LET v_continuar = TRUE 
      RETURN v_continuar      
   END IF
   CALL fn_actualiza_opera_ini(r_pid,
                               v_proceso_cod,
                               v_opera_cod,
                               v_folio,
                               "SEPF70",
                               v_nom_archivo,
                               p_usuario_cod) RETURNING r_resultado_opera
   IF(r_resultado_opera <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      # indica que continua en pantalla de detalle
      LET v_continuar = TRUE 
      RETURN v_continuar      
   END IF
   CALL fn_rutas("bat") RETURNING v_ruta_bin_aux, v_ruta_listados
   LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/SEPP71.42r ",
                                   p_usuario_cod CLIPPED, " ",
                                   r_pid, " ",
                                   v_proceso_cod," ",
                                   v_opera_cod," ",
                                   v_folio, " ",
                                   p_id_expediente,
                            " 1> ",v_ruta_listados CLIPPED,
                         "/nohup:",r_pid USING "&&&&&",":",
                                   v_proceso_cod USING "&&&&&",":",
                                   v_opera_cod USING "&&&&&",
                          " 2>&1 &"
   DISPLAY v_comando
   RUN v_comando
   IF(STATUS)THEN
      LET v_continuar = TRUE
      CALL fn_mensaje("SEP FONDO 72","Ocurrio un error al ejecutar la operación","about")
   ELSE
      CALL fn_genera_reporte(p_id_expediente,r_pid,v_proceso_cod,v_opera_cod,"LIQUIDACIÓN") RETURNING v_genero_rpt
      CALL fn_mensaje("SEP FONDO 72","Se ha enviado la operacion.\nPodrá revisar el detalle en el monitoreo de procesos para el pid "||r_pid,"about") LET v_continuar = FALSE
   END IF

   RETURN v_continuar
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL61                                                   #
#Descripcion       => funcion que ejecuta lanzado de iquidación                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 15 Octubre 2012                                          #
################################################################################
FUNCTION fn_ejecuta_liquidacion_fondo72(v_proceso_cod,v_opera_cod,p_id_expediente)

DEFINE r_resultado_opera SMALLINT,
       v_continuar       BOOLEAN,
       v_folio           LIKE glo_folio.folio,
       v_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo,
       r_pid             LIKE glo_pid.pid,
       v_ruta_bin_aux    LIKE seg_modulo.ruta_bin,
       v_ruta_bin_glo    LIKE seg_modulo.ruta_bin,
       v_comando         STRING ,
       v_proceso_cod     INTEGER,
       v_opera_cod       SMALLINT,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_ruta_listados_glo   LIKE seg_modulo.ruta_listados,
       p_id_expediente   DECIMAL(9,0),
       v_salir           BOOLEAN ,
       v_estado_cod      SMALLINT
       
   LET r_pid = v_pid  
   LET v_salir = FALSE 
   
   WHILE TRUE
   
         SELECT a.estado_cod 
         INTO v_estado_cod         
         FROM   bat_ctr_operacion a
         WHERE a.pid       = r_pid 
         AND   a.opera_cod = 1
         
         IF v_estado_cod = 4  THEN
            EXIT WHILE
         END IF 
   END WHILE

   CALL fn_valida_operacion(r_pid,v_proceso_cod,v_opera_cod) RETURNING r_resultado_opera
   # Se verifica si la operacion es valida
   IF(r_resultado_opera <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      # indica que continua en pantalla de detalle
      LET v_continuar = TRUE 
      RETURN v_continuar      
   END IF
   LET v_folio = 0
   LET v_nom_archivo = "NA"
   
   CALL fn_actualiza_opera_ini(r_pid,
                               v_proceso_cod,
                               v_opera_cod,
                               v_folio,
                               "SEPF70",
                               v_nom_archivo,
                               p_usuario_cod) RETURNING r_resultado_opera
   IF(r_resultado_opera <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      # indica que continua en pantalla de detalle
      LET v_continuar = TRUE 
      RETURN v_continuar      
   END IF
   
   SELECT a.folio 
   INTO   v_folio
   FROM bat_ctr_operacion a
   WHERE a.pid = r_pid 
   AND   a.opera_cod = 1   
   
   CALL fn_rutas("glo") RETURNING v_ruta_bin_glo, v_ruta_listados_glo
   CALL fn_rutas("bat") RETURNING v_ruta_bin_aux, v_ruta_listados
   
   LET v_comando = "nohup fglrun ",v_ruta_bin_glo CLIPPED,"/GLOG03.42r ", p_usuario_cod CLIPPED, " ", r_pid, " ", v_proceso_cod," ",
                                   v_opera_cod," ",
                                   v_folio, " ",
                                   v_nom_archivo,
                            " 1> ",v_ruta_listados CLIPPED,
                         "/nohup:",r_pid USING "&&&&&",":",
                                   v_proceso_cod USING "&&&&&",":",
                                   v_opera_cod USING "&&&&&",
                          " 2>&1 &"
   DISPLAY v_comando
   RUN v_comando
   IF(STATUS)THEN
      LET v_continuar = TRUE
      CALL fn_mensaje("SEP FONDO 72","Ocurrio un error al ejecutar la operación","about")
   ELSE
      CALL fn_mensaje("SEP FONDO 72","Se ha enviado la operacion.\nPodrá revisar el detalle en el monitoreo de procesos para el pid "||r_pid,"about") LET v_continuar = FALSE
   END IF

   RETURN v_continuar
END FUNCTION

FUNCTION fn_busca_cuenta(p_filtro)
DEFINE --p_nss     LIKE afi_fondo72.nss,
       --p_rfc     LIKE afi_fondo72.rfc,
       p_filtro  STRING,
       v_asociado RECORD
         v_nss    LIKE afi_fondo72.nss,
         v_rfc    LIKE afi_fondo72.rfc,
         v_nombre LIKE afi_fondo72.nombre
       END RECORD,
       v_contador SMALLINT,
       v_cadena   STRING


   LET v_contador = 0
   LET v_cadena = "\n SELECT COUNT(*)",
                  "\n   FROM afi_fondo72",
                  "\n  WHERE ",p_filtro
   PREPARE prp_obtiene_conteo FROM v_cadena
   EXECUTE prp_obtiene_conteo INTO v_contador
   
   CASE 
      # si no encontró algun registro
      WHEN v_contador = 0 OR v_contador IS NULL
         CALL fn_mensaje("AVISO","No se encontraron registros con criterio dado","information")
         INITIALIZE v_asociado TO NULL

      # si solo encontro un registro
      WHEN v_contador = 1
         LET v_cadena = "\n SELECT nss,",
                        "\n        rfc,",
                        "\n        nombre",
                        "\n   FROM afi_fondo72",
                        "\n  WHERE ",p_filtro
         PREPARE prp_rec_asociado FROM v_cadena
         EXECUTE prp_rec_asociado INTO v_asociado.v_nss,
                                       v_asociado.v_rfc,
                                       v_asociado.v_nombre

      # si encontro mas de una coincidencia
      WHEN v_contador > 1
         CALL fn_selecciona_cuenta(p_filtro) RETURNING v_asociado.*

   END CASE

   RETURN v_asociado.*
END FUNCTION

FUNCTION fn_selecciona_cuenta(p_filtro)
DEFINE p_filtro    STRING,
       v_asociado RECORD
         v_nss        LIKE afi_fondo72.nss,
         v_rfc        LIKE afi_fondo72.rfc,
         v_nombre     LIKE afi_fondo72.nombre,
         v_f_apertura LIKE afi_fondo72.f_apertura
       END RECORD,
       v_asociados DYNAMIC ARRAY OF RECORD
         v_nss        LIKE afi_fondo72.nss,
         v_rfc        LIKE afi_fondo72.rfc,
         v_nombre     LIKE afi_fondo72.nombre,
         v_f_apertura LIKE afi_fondo72.f_apertura
       END RECORD,
       v_indice   SMALLINT,
       v_cadena   STRING

   LET v_cadena = "\n SELECT nss,rfc,nombre,f_apertura",
                  "\n   FROM afi_fondo72",
                  "\n  WHERE ",p_filtro
   
   LET v_indice = 1
   PREPARE prp_seleciona_cuenta FROM v_cadena
   DECLARE cur_seleciona_cuenta CURSOR FOR prp_seleciona_cuenta
   FOREACH cur_seleciona_cuenta INTO v_asociado.*
      LET v_asociados[v_indice].* = v_asociado.*
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_seleciona_cuenta
   # abre ventana para seleccionar un solo registro
   OPEN WINDOW vtna_selecciona_cuenta WITH FORM v_ruta_ejecutable CLIPPED||"/SEPF704" ATTRIBUTES(STYLE="dialog")
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_titulo_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo_ventana)         
         CALL v_ventana.setText(p_titulo_ventana)
      END IF
      DISPLAY ARRAY v_asociados TO sr_asociados.* ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)

         BEFORE DISPLAY
            INITIALIZE v_asociado TO NULL

         ON ACTION aceptar
            LET v_asociado.* = v_asociados[ARR_CURR()].*
            ACCEPT DISPLAY

         ON ACTION cancelar
            EXIT DISPLAY

      END DISPLAY

   CLOSE WINDOW vtna_selecciona_cuenta

   RETURN v_asociado.v_nss,
          v_asociado.v_rfc,
          v_asociado.v_nombre

END FUNCTION
