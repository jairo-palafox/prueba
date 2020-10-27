################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => LEY 72                                                   #
#Programa          => L72C01                                                   #
#Objetivo          => Consultar información de Ley 72 para que el usuario      #
#                     seleccione la operación a ejecutar                       #
#Fecha Inicio      => 23/05/2012                                               #
################################################################################
DATABASE safre_viv

--GLOBALS "L72C01.inc"

GLOBALS

DEFINE g_proceso_cod SMALLINT,
       g_opera_cod SMALLINT,
       g_pid       DECIMAL(9,0)

DEFINE g_reg_modulo   RECORD
        ruta_bin         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD

DEFINE g_reg_modulo_bat   RECORD
        ruta_bin         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD

--Arreglo todos los clientes
DEFINE v_lista_clientes         DYNAMIC ARRAY OF RECORD
          id_afi_fondo72               DECIMAL(9,0),
          nss                          CHAR(11),
          rfc                          CHAR(13),
          nombre_completo              VARCHAR(60),
          elige_registro               SMALLINT
END RECORD   

DEFINE arr_datos_unificar DYNAMIC ARRAY OF RECORD
       v_uni_id_afi_fondo72  DECIMAL(9,0),
       v_uni_nss             CHAR(11),
       v_uni_rfc             CHAR(13),
       v_uni_nombre          CHAR(30),
       v_uni_saldo     DECIMAL(22,2),
       v_uni_tipo_nss        SMALLINT
END RECORD

DEFINE arr_hist_detalles DYNAMIC ARRAY OF RECORD 
          v_nss          CHAR(11),
          v_rfc          CHAR(13),
          v_nombre       CHAR(40),
          v_folio        DECIMAL(9,0),
          v_ejercicio    SMALLINT,
          v_clave_mov    CHAR(2),
          v_empresa      CHAR(40),
          v_bimestres    SMALLINT,
          v_importe      DECIMAL(16,6),
          v_ind_verifica SMALLINT,
          v_monto_proyectado DECIMAL(16,6)
END RECORD

DEFINE arr_hist_det_dor DYNAMIC ARRAY OF RECORD 
          v_nss          CHAR(11),
          v_rfc          CHAR(13),
          v_ejercicio    SMALLINT,
          v_empresa      CHAR(40),
          v_importe      DECIMAL(16,6)
END RECORD

DEFINE arr_hist_det_ado DYNAMIC ARRAY OF RECORD 
          v_nss          CHAR(11),
          v_rfc          CHAR(13),
          v_ejercicio    SMALLINT,
          v_empresa      CHAR(40),
          v_importe      DECIMAL(16,6)
END RECORD

DEFINE arr_seleccionado DYNAMIC ARRAY OF RECORD
       v_id_seleccion  VARCHAR(20)
END RECORD        

DEFINE i_det         INTEGER, 
       v_ind_datos   INTEGER,
       v_i_dm        INTEGER,
       v_i_dethist   INTEGER,
       v_i_dethist_dor INTEGER,
       v_i_dethist_ado INTEGER,
       v_confirma     SMALLINT,
       i INTEGER,
       j INTEGER,
       k INTEGER,
       p_usuario_cod            CHAR(10)

DEFINE v_total_proyectado_ado  DECIMAL(10,2),
       v_total_proyectado_dor  DECIMAL(10,2)
       
END GLOBALS

#Variables para capturar los parametros que recibe la consulta
PRIVATE DEFINE p_usuario            CHAR(10)
PRIVATE DEFINE p_tipo_proc          CHAR(1)
PRIVATE DEFINE p_nombre_menu        CHAR(50)
PRIVATE DEFINE p_id_fondo           DECIMAL(9,0)

PRIVATE DEFINE v_saldo_total        DECIMAL(22,2)

#Variables para el manejo de la pantalla
PRIVATE DEFINE ventana                       ui.Window
PRIVATE DEFINE forma                         ui.Form

FUNCTION fn_unifica_ley72(p_condicion, p_usuario_cod)
DEFINE p_condicion      STRING,
       v_ind_seleccion  INTEGER,
       v_total_pesos    DECIMAL(16,6),
       v_tipo_nss       SMALLINT,
       v_ids_seleccion  VARCHAR(100),
       v_nss_seleccion  VARCHAR(100),
       p_usuario_cod    CHAR(10)

DEFINE v_id_seleccionado        STRING,
       v_total_monto_pesos_ado  DECIMAL(16,6),
       v_monto_pesos_ado        DECIMAL(16,6),
       v_total_monto_pesos_dor  DECIMAL(16,6),
       v_monto_pesos_dor        DECIMAL(16,6),
       v_num_unificador         SMALLINT,
       r_ind_datos              INTEGER,
       v_folio_preunifica       DECIMAL(9,0),
       v_preu_ejercicio         SMALLINT,
       v_preu_empresa           CHAR(40),
       v_imp_movimiento         DECIMAL(10,2),
       a INTEGER,
       v_ruta_forma CHAR(40)
DEFINE buf base.StringBuffer

DEFINE v_nss_unificador CHAR(11),
       v_rfc_unificador CHAR(13),
       v_nss_unificados CHAR(11),
       v_rfc_unificados CHAR(13),
       v_num_unificados SMALLINT
   
   LET p_usuario            = ARG_VAL(1)
   LET p_tipo_proc          = ARG_VAL(2) -- Recibe el tipo de proceso
   LET p_nombre_menu        = ARG_VAL(3) -- Recibe el nombre del programa
   LET p_id_fondo           = ARG_VAL(4)

   LET g_proceso_cod = 2308
   LET g_opera_cod   = 1

   CALL STARTLOG(p_usuario CLIPPED ||".UNIP14.log")

   -- se asigna el titulo del programa
   LET p_nombre_menu = "Unificación Fondo 72"
   CALL ui.Interface.setText(p_nombre_menu)
   LET buf = base.StringBuffer.create()

   SELECT s.ruta_bin
   INTO   g_reg_modulo.ruta_bin
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'uni'   

   LET v_ruta_forma = g_reg_modulo.ruta_bin CLIPPED,"/UNIP140"
   
OPEN WINDOW vtn_L72P011 WITH FORM v_ruta_forma CLIPPED

   LET ventana = ui.Window.getCurrent()
   LET forma   = ventana.getForm()
   
   CALL forma.setElementHidden("gr_parametros",1)
   CALL forma.setElementHidden("gr_det_unificacion",1)
   CALL forma.setElementHidden("gr_busqueda_sel",0)
   CALL forma.setElementHidden("gr_det_historico", 0)
--
   CALL forma.setElementHidden("historicos_unificador", 1)
   CALL forma.setElementHidden("historicos_unificados", 1)

   --Llama función con datos iniciales 
   CALL fn_consulta_detalles(p_condicion)

   --Inicia proceso de unificación
   DIALOG ATTRIBUTES (UNBUFFERED)
      INPUT ARRAY v_lista_clientes 
      FROM scr_busqueda_sel.*
      ATTRIBUTES  (INSERT ROW = FALSE,
                   APPEND ROW = FALSE, 
                   DELETE ROW = FALSE)

         BEFORE INPUT
            LET v_ind_datos = 1
            
            IF i_det= 1 THEN
               CALL fn_mensaje ("Atención", 
                                "No se encontraron registros con los críterios seleccionados", 
                                "stop")
               EXIT DIALOG
            END IF 

         BEFORE ROW
            CALL arr_hist_detalles.clear()
            CALL fn_consulta_det_historico(v_lista_clientes[ARR_CURR()].nss,
                                           v_lista_clientes[ARR_CURR()].rfc)

         ON CHANGE v_check_box
            IF v_lista_clientes[ARR_CURR()].elige_registro = 1 THEN
               LET v_ind_seleccion = v_ind_seleccion + 1
            END IF 
            
            IF v_lista_clientes[ARR_CURR()].elige_registro = 0 THEN
               LET v_ind_seleccion = v_ind_seleccion - 1
            END IF                                                                                                                       
      
            ON ACTION ACCEPT
               LET j = 1
               FOR i = 1 TO v_lista_clientes.getLength() 
                  IF v_lista_clientes[i].elige_registro = 1 THEN
                     LET v_id_seleccionado = v_lista_clientes[i].id_afi_fondo72
                     CALL buf.append(v_id_seleccionado)
                     CALL buf.trimleft()

                     LET v_ids_seleccion = v_ids_seleccion CLIPPED, v_id_seleccionado CLIPPED, ","
                     --DISPLAY v_ids_seleccion
                     CALL buf.append(v_ids_seleccion)
                     CALL buf.trimleft()
                     LET j = j + 1 
                  END IF
               END FOR

         	   IF LENGTH(v_ids_seleccion CLIPPED) <> 0 THEN
         	      LET v_ids_seleccion = v_ids_seleccion[1,LENGTH(v_ids_seleccion)-1]
         	   ELSE
         	  	  LET  v_ids_seleccion = 0  
         	   END IF

               IF v_ind_seleccion > = 2 THEN
                  CALL fn_consulta_det_unificar(v_ids_seleccion)
                  CALL forma.setElementHidden("gr_busqueda_sel", 1)
                  CALL forma.setElementHidden("gr_det_unificacion", 0)

                  --Identifica el UNIFICADO Y UNIFICADOR
                  INPUT ARRAY arr_datos_unificar 
                  WITHOUT DEFAULTS 
                  FROM scr_det_unificacion.*
                  ATTRIBUTES  (UNBUFFERED, 
                               INSERT ROW = FALSE,
                               APPEND ROW = FALSE, 
                               DELETE ROW = FALSE)

                     BEFORE INPUT
                        CALL forma.setElementHidden("gr_det_historico", 1)

                     BEFORE ROW
                        CALL arr_hist_detalles.clear()                        
                        CALL fn_consulta_det_historico(arr_datos_unificar[ARR_CURR()].v_uni_nss,
                                                       arr_datos_unificar[ARR_CURR()].v_uni_rfc)

                   LET j = 1
                   LET k = 2

                   FOR i = 1 TO arr_datos_unificar.getLength()
                        --Valida que las primeras 4 posiciones del RFC sean iguales
                        IF arr_datos_unificar[j].v_uni_rfc[1,4] <> arr_datos_unificar[k].v_uni_rfc[1,4] then 
                           CALL fn_mensaje("Consulta Fondo 72",
                                           "No se puede proceder con la unificación \n El RFC es diferente",
                                           "stop")
                           CALL arr_datos_unificar.clear()
                           CALL forma.setElementHidden("gr_parametros",1)
                           CALL forma.setElementHidden("gr_det_unificacion",1)
                           CALL forma.setElementHidden("gr_hist_unificador", 1)
                           CALL forma.setElementHidden("gr_hist_unificado", 1)
                           CALL forma.setElementHidden("gr_busqueda_sel",0)
                           CALL forma.setElementHidden("gr_det_historico", 0)

                           EXIT INPUT
                        ELSE 
                           --Valida que el nombre completo sea igual 
                           IF arr_datos_unificar[j].v_uni_nombre <> arr_datos_unificar[k].v_uni_nombre THEN 
                              CALL fn_mensaje("Consulta Fondo 72",
                                              "No se puede proceder con la unificación \n El Nombre es diferente",
                                              "stop")
                           CALL arr_datos_unificar.clear()
                           CALL forma.setElementHidden("gr_parametros",1)
                           CALL forma.setElementHidden("gr_det_unificacion",1)
                           CALL forma.setElementHidden("gr_hist_unificador", 1)
                           CALL forma.setElementHidden("gr_hist_unificado", 1)
                           CALL forma.setElementHidden("gr_busqueda_sel",0)
                           CALL forma.setElementHidden("gr_det_historico", 0)

                              EXIT INPUT
                           END IF
                        END IF

                     LET j = j + 1
                     LET k = k + 1

                     IF j = arr_datos_unificar.getLength() THEN 
                        EXIT FOR
                     END IF
                  END FOR

                     ON CHANGE v_u_tipo_nss
                        LET v_total_monto_pesos_ado = 0  
                        LET v_monto_pesos_ado       = 0
                        LET v_total_monto_pesos_dor = 0
                        LET v_monto_pesos_dor       = 0
                        LET v_num_unificador        = 0
                        LET v_nss_unificador = 0
                        LET v_rfc_unificador = 0
                        LET v_nss_unificados = 0
                        LET v_rfc_unificados = 0

                        LET j = 1
                        LET a = 1

                        --Sección de Cálculo de Totales 
                        FOR i = 1 TO arr_datos_unificar.getLength()
                           --Calcula total pesos UNIFICADOS   
                           IF arr_datos_unificar[i].v_uni_tipo_nss = 2 THEN
                              LET v_monto_pesos_ado = arr_datos_unificar[j].v_uni_saldo

                              LET v_nss_unificados = arr_datos_unificar[j].v_uni_nss
                              LET v_rfc_unificados = arr_datos_unificar[j].v_uni_rfc
                              
                              IF v_monto_pesos_ado IS NULL THEN
                                 LET v_monto_pesos_ado = 0
                              END IF

                              CALL buf.append(v_monto_pesos_ado)
                              CALL buf.trimleft()

                              LET v_total_monto_pesos_ado = v_total_monto_pesos_ado + v_monto_pesos_ado
                              CALL buf.append(v_total_monto_pesos_ado)
                              CALL buf.trimleft()

                              LET v_num_unificados = v_num_unificados + 1

                              LET j = j + 1
                           ELSE
                              LET v_monto_pesos_dor = arr_datos_unificar[j].v_uni_saldo
                              LET v_nss_unificador = arr_datos_unificar[j].v_uni_nss
                              LET v_rfc_unificador = arr_datos_unificar[j].v_uni_rfc

                              IF v_monto_pesos_dor IS NULL THEN
                                 LET v_monto_pesos_dor = 0
                              END IF

                              CALL buf.append(v_monto_pesos_dor)
                              CALL buf.append(v_nss_unificador)
                              CALL buf.append(v_rfc_unificador)

                              CALL buf.trimleft()

                              LET v_total_monto_pesos_dor = v_total_monto_pesos_dor + v_monto_pesos_dor
                              CALL buf.append(v_total_monto_pesos_dor)
                              CALL buf.trimleft()

                              LET v_num_unificador = v_num_unificador + 1
                              LET j = j + 1
                           END IF 

                        END FOR

                        LET v_total_monto_pesos_dor = v_total_monto_pesos_dor + v_total_monto_pesos_ado
                        LET v_total_monto_pesos_ado = ( v_total_monto_pesos_ado * -1 )
                        
                        IF v_num_unificados > 2 AND v_num_unificador = 0 THEN 
                           LET v_total_monto_pesos_dor = 0 
                        END IF 

                        DISPLAY v_total_monto_pesos_dor TO ed_tot_dor
                        DISPLAY v_total_monto_pesos_ado TO ed_tot_ado

                     ---Sección de Cálculo de Totales 
                     ON ACTION ACCEPT
                     DISPLAY v_total_monto_pesos_ado * -1 
                     IF v_num_unificador > 1 THEN
                        CALL fn_mensaje ("Atención", "Solamente puede existir un UNIFICADOR", "stop") 
                        INITIALIZE v_total_monto_pesos_dor TO NULL 
                        INITIALIZE v_total_monto_pesos_ado TO NULL 
                     ELSE
                        IF v_num_unificador = 0 THEN
                           CALL fn_mensaje ("Atención", "No se cuenta con ningún UNIFICADOR", "stop") 
                        ELSE  

                        LET j = 1
                        FOR i = 1 TO arr_datos_unificar.getLength() 
                           IF arr_datos_unificar[i].v_uni_tipo_nss = 2 THEN
                              LET v_nss_unificados = arr_datos_unificar[i].v_uni_nss
                              CALL buf.append(v_nss_unificados)
                              CALL buf.trimleft()

                              LET v_nss_seleccion = v_nss_seleccion CLIPPED, "'", v_nss_unificados CLIPPED, "'", ","
                              DISPLAY "selección", v_nss_seleccion
                              CALL buf.append(v_nss_seleccion)
                              CALL buf.trimleft()
                              LET j = j + 1 
                           END IF
                        END FOR

                        IF LENGTH(v_nss_seleccion CLIPPED) <> 0 THEN
                           LET v_nss_seleccion = v_nss_seleccion[1,LENGTH(v_nss_seleccion)-1]
                        ELSE
                           LET  v_nss_seleccion = 0  
                        END IF
DISPLAY "v_nss_seleccion antes de envio ", v_nss_seleccion
                        --Obtiene totales del unificador
                        CALL fn_consulta_unificador(v_nss_unificador,
                                                    v_rfc_unificador,
                                                    v_nss_unificados, 
                                                    v_rfc_unificados, 
                                                    v_nss_seleccion)
                        --Muestra display de historicos por unificado y unificador
                        CALL forma.setElementHidden("historicos_unificador", 0)
                        CALL forma.setElementHidden("historicos_unificados", 0)

                        DIALOG ATTRIBUTES (UNBUFFERED)
                           DISPLAY ARRAY arr_hist_det_dor TO scr_unificadores.*
                           END DISPLAY

                           DISPLAY ARRAY arr_hist_det_ado TO scr_unificados.*
                           END DISPLAY
                           
                           ON ACTION unificacion
                              CALL fn_ventana_confirma ("Atención","Se ejecutara la unificación, ¿Desea continuar?", "question")
                              RETURNING v_confirma

                              IF v_confirma = 1 THEN 
                                 --Ejecuta liquidación
                                 CALL fn_ejecuta_unifica_ley72(v_ids_seleccion, p_usuario_cod, v_total_monto_pesos_ado)

                                 EXIT PROGRAM
                              ELSE    
                                 EXIT DIALOG
                                 CALL arr_hist_det_dor.clear()
                                 CALL arr_hist_det_ado.clear()
                                 CALL forma.setElementHidden("historicos_unificador", 1)
                                 CALL forma.setElementHidden("historicos_unificados", 1)
                              END IF

                           ON ACTION cancelar
                              EXIT DIALOG
                              CALL arr_hist_det_dor.clear()
                              CALL arr_hist_det_ado.clear()
                              CALL forma.setElementHidden("historicos_unificador", 1)
                              CALL forma.setElementHidden("historicos_unificados", 1)
                           END DIALOG 
                        END IF
                     END IF                 
                     ON ACTION CANCEL
                        CALL arr_datos_unificar.clear()

                        CALL forma.setElementHidden("gr_det_unificacion", 1)
                        CALL forma.setElementHidden("gr_busqueda_sel", 0)
                        EXIT INPUT  
                  END INPUT 
               ELSE
                  CALL fn_mensaje("Consulta Fondo 72",
                                  "Debe seleccionar dos registros como mínimo",
                                  "about")
                  EXIT DIALOG                
               END IF

            ON ACTION cancelar 
               CALL v_lista_clientes.clear() 
               CALL forma.setElementHidden("gr_busqueda_sel", 1)
               EXIT DIALOG 
         END INPUT 

         --Muestra detalles antes de elegir los 2 registros a unificar
         DISPLAY ARRAY arr_hist_detalles TO scr_det_historicos.* 
         END DISPLAY

   END DIALOG 
CLOSE WINDOW vtn_L72P011
END FUNCTION 

#OBJETIVO: Obtener los registros que cumplan con los parámetro solo para consulta
FUNCTION fn_consulta_detalles(p_condicion)
DEFINE v_QryTxt    STRING,
       p_condicion STRING 
DISPLAY p_condicion
   --Consulta registros a unificar en base al parámetro capturado
   LET v_QryTxt = "\n SELECT FIRST 51 ",
                  "\n        id_afi_fondo72, ",
                  "\n        nss, ",
                  "\n        rfc, ",
                  "\n        nombre ",
                  "\n FROM   afi_fondo72 ", 
                  "\n WHERE ", p_condicion,
                  "\n AND    ind_estado_cuenta = 0 " --Sin unificar
--DISPLAY v_QryTxt
--DISPLAY "Consulta registros a unificar en base al parámetro capturado"
   PREPARE exe_consulta_rfc FROM v_QryTxt
   DECLARE cur_consulta_rfc CURSOR FOR exe_consulta_rfc
   
   LET i_det = 1
   
   FOREACH cur_consulta_rfc INTO v_lista_clientes[i_det].*
      LET i_det = i_det + 1
   END FOREACH

   CALL v_lista_clientes.deleteElement(i_det)

END FUNCTION 

#OBJETIVO: Consultar los detalles de movimiento de los registros a unificar
FUNCTION fn_consulta_det_unificar(p_ids_seleccion)
DEFINE v_QryTxt         STRING,
       v_QryTxt1        STRING,
       p_ids_seleccion  VARCHAR(100)

   LET v_QryTxt = "\n SELECT a.id_afi_fondo72, ",
                  "\n        a.nss,            ",
                  "\n        a.rfc,            ",
                  "\n        a.nombre,         ",
                  "\n        SUM(mov.importe)  ",
                  "\n FROM   afi_fondo72 a     ",
                  "\n LEFT   OUTER JOIN cta_fondo72 mov",
                  "\n ON     a.id_afi_fondo72 = mov.id_afi_fondo72",
                  "\n WHERE  a.id_afi_fondo72 IN ( ",p_ids_seleccion, " ) ",
                  "\n GROUP  BY 1,2,3,4",
                  "\n ORDER  BY 3"

--DISPLAY v_QryTxt
                  
   PREPARE prp_det_movimientos FROM v_QryTxt
   DECLARE cur_det_movimientos CURSOR FOR prp_det_movimientos
    
   LET v_i_dm = 1
   
   FOREACH cur_det_movimientos INTO arr_datos_unificar[v_i_dm].v_uni_id_afi_fondo72,
                                    arr_datos_unificar[v_i_dm].v_uni_nss,
                                    arr_datos_unificar[v_i_dm].v_uni_rfc,
                                    arr_datos_unificar[v_i_dm].v_uni_nombre,
                                    arr_datos_unificar[v_i_dm].v_uni_saldo       
      LET v_i_dm = v_i_dm + 1
   END FOREACH

   
   CALL arr_datos_unificar.deleteElement(arr_datos_unificar.getLength())

END FUNCTION

#OBJETIVO: Consultar los movimientos históricos del NSS seleccionado
FUNCTION fn_consulta_det_historico(p_nss, p_rfc)
DEFINE p_nss    CHAR(11), 
       p_rfc    CHAR(13),
       v_QryTxt STRING
      
   LET v_QryTxt = "\n SELECT nss,",
                  "\n        rfc,",
                  "\n        nombre,",
                  "\n        folio,",
                  "\n        ejercicio,",
                  "\n        clave_mov,",
                  "\n        empresa,",
                  "\n        bimestres,",
                  "\n        importe,",
                  "\n        ind_verifica,",
                  "\n        0",
                  "\n FROM   cta_his_fondo72",
                  "\n WHERE  nss = ", "'", p_nss ,"'",
                  "\n AND    rfc = ", "'", p_rfc ,"'"
   --DISPLAY v_QryTxt

   LET v_i_dethist = 1

   PREPARE prp_cons_historico FROM v_QryTxt
   DECLARE cur_cons_historico CURSOR FOR prp_cons_historico

   FOREACH cur_cons_historico INTO arr_hist_detalles[v_i_dethist].*  
      LET v_i_dethist = v_i_dethist + 1
   END FOREACH

   CALL arr_hist_detalles.deleteElement(v_i_dethist)

END FUNCTION

FUNCTION fn_valida_saldo(p_id_afi_fondo72)
DEFINE v_total_fondo    DECIMAL(16,6),
       p_id_afi_fondo72 DECIMAL(9,0)

   SELECT importe
   INTO   v_total_fondo 
   FROM   cta_fondo72
   WHERE  id_afi_fondo72 = p_id_afi_fondo72

   RETURN v_total_fondo

END FUNCTION 

FUNCTION fn_ejecuta_unifica_ley72(p_ids_seleccion, p_usuario_cod, p_pesos_dor)
DEFINE p_ids_seleccion    CHAR(40),
       v_rest_valida      SMALLINT
DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       v_s_comando        STRING, -- cadena con una instruccion de consola
       v_nombre_archivo   LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       r_bnd_opera_ini    SMALLINT,
       r_bnd_opera_fin    SMALLINT,
       v_mensaje          STRING,
       v_inicia_proceso   SMALLINT,
       v_valida_operacion SMALLINT,
       r_bnd_fin_oper     SMALLINT,
       v_folio            DECIMAL(9,0);
DEFINE v_i_resultado      SMALLINT, 
       v_total_regs       INTEGER, 
       v_folio_liquida    DECIMAL, 
       v_error_isam       INTEGER, 
       v_mensaje_error    VARCHAR(255),
       v_QryTxt           STRING

DEFINE v_preu_id_afi_fondo72    DECIMAL(9,0),
       v_preu_nss               CHAR(11),
       v_preu_rfc               CHAR(13),
       v_preu_nombre            CHAR(40),
       v_preu_fecha_liquidacion DATE,
       v_preu_folio_liquidacion DECIMAL(9,0),
       v_preu_movimiento        SMALLINT,
       v_preu_origen            CHAR(20),
       v_preu_monto_pesos       DECIMAL(16,6),
       v_preu_tipo_nss          SMALLINT,
       v_preu_diagnostico       SMALLINT,
       p_pesos_dor              DECIMAL(16,6)
       
	 -- se obtiene el ID del proceso
   CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod)
   RETURNING g_pid

   --Inicia proceso
   CALL fn_inicializa_proceso(g_pid,g_proceso_cod,g_opera_cod,0,"CTAC04","",p_usuario_cod)
   RETURNING v_inicia_proceso

   IF ( v_inicia_proceso = 0)THEN
      -- se verifica si se puede continuar con la operacion
      CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
      RETURNING v_valida_operacion

      --DISPLAY v_valida_operacion
      LET v_valida_operacion = 0
      
      IF ( v_valida_operacion = 0 ) THEN
         -- Inicio operacion.
         CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,v_folio,"CTAC04","",p_usuario_cod)
         RETURNING r_bnd_opera_ini
         
            CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod)
            RETURNING v_folio

            ---Ingresa datos para la preliquidacion de la unificación
            LET j = 1
            FOR i = 1 TO arr_datos_unificar.getLength()
            --DISPLAY "DEBE INSERTAR COSAS"
               LET v_preu_id_afi_fondo72    = arr_datos_unificar[j].v_uni_id_afi_fondo72
               LET v_preu_nss               = arr_datos_unificar[j].v_uni_nss
               LET v_preu_rfc               = arr_datos_unificar[j].v_uni_rfc
               LET v_preu_nombre            = arr_datos_unificar[j].v_uni_nombre
               LET v_preu_fecha_liquidacion = TODAY --arr_datos_unificar[j].v_uni_fecha_liquida
               LET v_preu_folio_liquidacion = v_folio
               LET v_preu_movimiento        = 0 --arr_datos_unificar[j].v_uni_movimiento
               LET v_preu_origen            = "UNIFICACION FONDO 72"
               LET v_preu_monto_pesos       = arr_datos_unificar[j].v_uni_saldo
               LET v_preu_tipo_nss          = arr_datos_unificar[j].v_uni_tipo_nss
               LET v_preu_diagnostico       = 2

               INSERT INTO uni_preunifica_fondo72
                    VALUES (v_folio,
                            v_preu_id_afi_fondo72,
                            v_preu_nss,
                            v_preu_rfc,
                            v_preu_nombre,
                            v_preu_fecha_liquidacion, 
                            v_preu_folio_liquidacion, 
                            v_preu_movimiento,
                            v_preu_origen,
                            v_preu_monto_pesos,
                            v_preu_tipo_nss,
                            v_preu_diagnostico)

               LET j = j + 1             
            END FOR

            LET v_QryTxt = "EXECUTE FUNCTION fn_uni_preliq_fondo72(?,?,?,?)"
            -- se prepara la ejecucion del stored procedure para la preliquidacion
            PREPARE prp_preliquida FROM v_QryTxt 
            -- se ejecuta el stored procedure
            EXECUTE prp_preliquida  USING v_folio, 
                                          p_usuario_cod, 
                                          g_pid, 
                                          g_proceso_cod
                                     INTO v_i_resultado, 
                                          v_total_regs, 
                                          v_folio_liquida, 
                                          v_error_isam, 
                                          v_mensaje_error

            DISPLAY "RESULTADO DE PRELIQUIDA : ",v_i_resultado
            DISPLAY "ISAM    : ", v_error_isam
            DISPLAY "MENSAJE : ", v_mensaje_error

            IF v_i_resultado = 0 THEN                
               SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
               INTO   g_reg_modulo.*
               FROM   seg_modulo s
               WHERE  s.modulo_cod = 'glo'

               SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
               INTO   g_reg_modulo_bat.*
               FROM   seg_modulo s
               WHERE  s.modulo_cod = 'bat'

               LET v_nombre_archivo = "NA" 
               
               LET v_s_comando = "nohup fglrun ",g_reg_modulo.ruta_bin CLIPPED,"/GLOG03.42r ", 
                                 p_usuario_cod CLIPPED, " ", 
                                 g_pid, " ", 
                                 g_proceso_cod," ",
                                 g_opera_cod," ",
                                 v_folio, " ",
                                 v_nombre_archivo CLIPPED,
                                 " 1> ",g_reg_modulo_bat.ruta_listados  CLIPPED,
                                 "/nohup:",g_pid USING "&&&&&",":",
                                 g_proceso_cod USING "&&&&&",":",
                                 g_opera_cod USING "&&&&&",
                                 " 2>&1 &"
               DISPLAY v_s_comando
               RUN v_s_comando

               CALL fn_mensaje("Atención", 
                               " Se ha enviado la Unificación \n puede revisar el avance en el monitor de procesos", 
                               "about")
            ELSE
               CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
               RETURNING r_bnd_fin_oper

               DISPLAY "ERROR OPERA : ", r_bnd_fin_oper
               DISPLAY "# Ha ocurrido un error en la ejecución de la preliquidación "
               DISPLAY "# Resultado : ",  v_i_resultado 
               DISPLAY "# ISAM      : ",  v_error_isam
               DISPLAY "# Mensaje   : ",  v_mensaje_error
            END IF
         ELSE
            CALL fn_recupera_inconsis_opera(r_bnd_opera_ini) RETURNING v_mensaje
            CALL fn_mensaje ("Atención", v_mensaje, "stop")
         END IF
      CALL fn_recupera_inconsis_opera(v_valida_operacion) RETURNING v_mensaje
      CALL fn_mensaje ("Atención", v_mensaje, "stop")
   END IF

END FUNCTION 

#OBJETIVO: Ejecutar la unificación de movimientos históricos del UNIFICADOR
FUNCTION fn_consulta_unificador(p_nss,
                                p_rfc, 
                                p_nss_ado, 
                                p_rfc_ado,
                                p_nss_seleccion)
DEFINE p_nss           CHAR(11), 
       p_rfc           CHAR(13),
       v_QryTxt        STRING,
       p_nss_ado       CHAR(11), 
       p_rfc_ado       CHAR(13),
       p_nss_seleccion VARCHAR(1000)

   LET v_QryTxt = "\n SELECT nss,",
                  "\n        rfc,",
                  "\n        ejercicio,",
                  "\n        empresa,",
                  "\n        importe,",
                  "\n        ''",
                  "\n FROM   cta_his_fondo72",
                  "\n WHERE  nss = ", "'", p_nss ,"'",
                  "\n AND    rfc = ", "'", p_rfc ,"'",
                  "\n ORDER BY 3,4,5"

DISPLAY "HIS DOR", v_QryTxt
                  
   LET v_i_dethist_dor = 1

   LET v_total_proyectado_dor = 0  
   PREPARE prp_cons_hist_dor FROM v_QryTxt
   DECLARE cur_cons_hist_dor CURSOR FOR prp_cons_hist_dor

   FOREACH cur_cons_hist_dor INTO arr_hist_det_dor[v_i_dethist_dor].* 
      LET v_i_dethist_dor = v_i_dethist_dor + 1
   END FOREACH

   DISPLAY p_nss_seleccion
   
   LET v_QryTxt = "\n SELECT nss,",
                  "\n        rfc,",
                  "\n        ejercicio,",
                  "\n        empresa,",
                  "\n        importe",
                  "\n FROM   cta_his_fondo72",
                  "\n WHERE  nss IN ", "(", p_nss_seleccion ,")",  
                  "\n AND    rfc = ", "'", p_rfc_ado ,"'",
                  "\n ORDER BY 3,4,5"  

DISPLAY "HIS ADO", v_QryTxt 

   LET v_i_dethist_ado = 1

   PREPARE prp_cons_hist_ado FROM v_QryTxt
   DECLARE cur_cons_hist_ado CURSOR FOR prp_cons_hist_ado

   LET v_total_proyectado_ado = 0

   FOREACH cur_cons_hist_ado INTO arr_hist_det_ado[v_i_dethist_ado].v_nss,
                                  arr_hist_det_ado[v_i_dethist_ado].v_rfc,
                                  arr_hist_det_ado[v_i_dethist_ado].v_ejercicio,
                                  arr_hist_det_ado[v_i_dethist_ado].v_empresa,
                                  arr_hist_det_ado[v_i_dethist_ado].v_importe

      LET v_i_dethist_ado = v_i_dethist_ado + 1
   END FOREACH
   
   CALL arr_hist_det_dor.deleteElement(v_i_dethist_dor)
   CALL arr_hist_det_dor.deleteElement(v_i_dethist_ado)
END FUNCTION
