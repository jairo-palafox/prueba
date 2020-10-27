################################################################################
#Modulo        => RET                                                          #
#Programa      => RETF414                                                      #
#Ojetivo       => Marca y Desmarca Manual mediante NSS con consulta a PROCESAR #
#Fecha inicio  => Noviembre, 2015.                                             #
#Requerimiento => PRODINF-845                                                  #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
DATABASE safre_viv

GLOBALS "RETG01.4gl"

MAIN

    -- Variables usadas para realizar la busqueda
    DEFINE v_nss                STRING 
    DEFINE v_modalidad          SMALLINT

    -- Combobox para la busqueda
    DEFINE cbx_modalidad        ui.ComboBox

    DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod -- clave del usuario firmado
    DEFINE p_tipo_ejecucion    SMALLINT -- forma como ejecutara el programa
    DEFINE p_s_titulo          STRING   -- titulo de la ventana

    DEFINE v_estado     SMALLINT
    DEFINE v_mensaje    STRING

    --Se obtienen los valores de la ejecucion
    -- se obtienen los parametros de ejecucion
    LET p_usuario_cod    = ARG_VAL(1)
    LET p_tipo_ejecucion = ARG_VAL(2)
    LET p_s_titulo       = ARG_VAL(3)

    -- si se obtuvo el titulo, se pone como titulo de programa
    IF ( p_s_titulo IS NOT NULL ) THEN
       CALL ui.Interface.setText(p_s_titulo)
    END IF

    CLOSE WINDOW SCREEN

    --Se llama a la funcion que desplegara la ventana para introducir los 
    --parametros de la consulta
    OPEN WINDOW busqueda_marcas WITH FORM "RETF4141"

        -- Se establece el combobox de modalidad: marcar o desmarcar.
        LET  cbx_modalidad = ui.ComboBox.forName("v_modalidad")

        INPUT BY NAME  v_nss,v_modalidad
                       ATTRIBUTES(UNBUFFERED,CANCEL = 0)

            BEFORE INPUT

                -- Crear el combobox
                CALL cbx_modalidad.clear()
                CALL cbx_modalidad.addItem(44,"MARCA"   )
                CALL cbx_modalidad.addItem(60,"DESMARCA")

                -- Inicializar variables
                LET v_modalidad = 44
                LET v_nss       = NULL

            ON ACTION ACCEPT

                --Se valida que el NSS sea un numero de 11 digitos
                IF v_nss IS NULL  THEN
                    CALL fn_mensaje("Atención",
                                    "El NSS debe ser un número válido.",
                                    "stop")
                    NEXT FIELD v_nss
                END IF

                --Se debe escoger una modalidad
                IF v_modalidad IS NULL THEN
                    CALL fn_mensaje("Atención",
                                    "Debe escoger una modalidad: Marca o Desmarca",
                                    "stop")
                    NEXT FIELD v_modalidad
                END IF

                -- Llamada a la funcion que realiza la marca o desmarca
                CALL fn_marca_desmarca_procesar(v_nss,v_modalidad,p_usuario_cod)
                                      RETURNING v_estado,v_mensaje

                CALL fn_mensaje("Atención",
                                "Proceso concluido",
                                "information")

            ON ACTION Salir
                IF fn_ventana_confirma("Atención","¿Desea salir?","quest") = 1 THEN
                    EXIT INPUT
                END IF

        END INPUT

    CLOSE WINDOW busqueda_marcas

END MAIN

#Funcion que valida si la cadena que se le pasa es un numero
FUNCTION fn_numero_valido(p_cadena)

    DEFINE p_cadena     STRING
    DEFINE v_entero     BIGINT

    TRY
        LET v_entero = p_cadena
        RETURN TRUE
    CATCH
        RETURN FALSE
    END TRY

END FUNCTION

#p_modalidad: 1 - Marca
#             2 - Desmarca
#Funcion que realiza la marca o desmarca a PROCESAR
FUNCTION fn_marca_desmarca_procesar(p_nss,p_modalidad,p_usuario_cod)

   -- Parametros recibidos
   DEFINE p_nss            STRING
   DEFINE p_modalidad      SMALLINT
   DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod

   -- Variables para el procesamiento
   DEFINE v_indicador_marca    SMALLINT
   DEFINE v_estado_indicador   SMALLINT
   DEFINE v_estado_cuenta      SMALLINT
   DEFINE v_id_solicitud       DECIMAL(9,0)
   DEFINE v_caso_crm           CHAR(10)
   DEFINE v_origen             SMALLINT
   DEFINE v_nss_paso           CHAR(11)

   -- Variables de la consulta a PROCESAR
   DEFINE v_aivs_viv92     DECIMAL(24,6)
   DEFINE v_aivs_viv97     DECIMAL(24,6)
   DEFINE v_pesos_viv92    DECIMAL(22,2)
   DEFINE v_pesos_viv97    DECIMAL(22,2)
   DEFINE v_diagnostico    SMALLINT

   -- Variables de control
   DEFINE v_estado SMALLINT

   -- Variables auxiliares
   DEFINE v_proceso_exitoso    SMALLINT
   DEFINE v_proceso_no_exitoso SMALLINT
   DEFINE v_s_proceso          STRING
   DEFINE v_query              STRING
   DEFINE v_registros          INTEGER 

   #Valores de retorno
   DEFINE r_mensaje            STRING
   DEFINE r_estado             SMALLINT
   DEFINE v_cod_rechazo        SMALLINT 
   DEFINE v_resultado          SMALLINT 
   DEFINE v_arr_detalle        DYNAMIC ARRAY OF RECORD
      nss                      CHAR(11),
      diagnositico             CHAR(3), 
      estado_cuenta            CHAR(3),
      descripcion              CHAR(80),  
      id_solicitud             DECIMAL(9,0), 
      caso_crm                 CHAR(10),
      modalidad                CHAR(8)
   END RECORD 
   DEFINE v_indice             INTEGER 

   -- Inicializar
   LET v_origen        = 3 -- Manual
   LET v_aivs_viv92    = 0
   LET v_aivs_viv97    = 0
   LET v_pesos_viv92   = 0
   LET v_pesos_viv97   = 0
   LET v_diagnostico   = 0
   LET v_estado_cuenta = 0
   LET v_estado_indicador = 0
   LET v_registros     = 0

   IF p_modalidad = 44 THEN
      LET v_s_proceso          = "Marca"
      LET v_indicador_marca    = 1 -- Marca
      LET v_proceso_exitoso    = 2 -- Marcado
      LET v_proceso_no_exitoso = 1 -- No marcado
   ELSE
      LET v_s_proceso          = "Desmarca"
      LET v_indicador_marca    = 2 -- Desmarca
      LET v_proceso_exitoso    = 5 -- Desmarcado
      LET v_proceso_no_exitoso = 4 -- No desmarcado
   END IF

   IF p_nss IS NOT NULL THEN 
      DROP TABLE IF EXISTS tmp_nss;
      CREATE TEMP TABLE tmp_nss (dato CHAR(11), diagnositico CHAR(3), estado_cuenta CHAR(3), id_solicitud DECIMAL(9,0), caso_crm CHAR(10));
      CALL fn_llena_tabla(p_nss, "tmp_nss") RETURNING v_resultado
   END IF 

   CALL v_arr_detalle.clear()
   LET v_indice = 1
   
   SELECT COUNT(*) 
   INTO   v_registros 
   FROM   tmp_nss;
   IF v_registros > 0 THEN 
      IF p_modalidad = 44 THEN 
         DISPLAY "Cantidad de registros a marcar :", v_registros
      ELSE 
         DISPLAY "Cantidad de registros a desmarcar :", v_registros
      END IF
      LET v_query = "SELECT * FROM tmp_nss"
      PREPARE prp_detalle FROM v_query
      DECLARE cur_detalle CURSOR FOR prp_detalle
      --DISPLAY v_s_qryTxt
      FOREACH cur_detalle INTO v_nss_paso
         LET v_id_solicitud = 0
         LET v_caso_crm = 0
         DISPLAY "----------------------------------------"
         DISPLAY "Se envía solicitud a Procesar del NSS:", v_nss_paso
         LET v_arr_detalle[v_indice].nss = v_nss_paso
         LET v_arr_detalle[v_indice].modalidad = v_s_proceso
         
         CALL fn_consulta_saldo_vivienda_afore(p_nss, p_modalidad)
         RETURNING v_diagnostico, v_estado_cuenta, v_aivs_viv92,
                  v_pesos_viv92, v_aivs_viv97, v_pesos_viv97,v_cod_rechazo

         LET v_arr_detalle[v_indice].diagnositico  = v_diagnostico
         LET v_arr_detalle[v_indice].estado_cuenta = v_estado_cuenta 
         CALL fn_diagnostico_desc(v_diagnostico,v_estado_cuenta) RETURNING v_arr_detalle[v_indice].descripcion
         DISPLAY "Responde el servicio de Procesar :"
         DISPLAY "v_diagnostico   :	",v_diagnostico
         DISPLAY "v_estado_cuenta :	",v_estado_cuenta
         
--         CALL recuperaDatos(v_nss_paso,v_indicador_marca) RETURNING v_estado,
--                                                                    v_id_solicitud,
--                                                                    v_caso_crm
         CALL fn_busca_id_solicitud_caso_crm(v_nss_paso) RETURNING v_estado,
                                                                   v_id_solicitud,
                                                                   v_caso_crm;

         DISPLAY ""
         DISPLAY ""
         -- Ejecucion correcta
         IF v_id_solicitud <> 0 AND v_id_solicitud IS NOT NULL THEN
            LET v_arr_detalle[v_indice].id_solicitud = v_id_solicitud
            LET v_arr_detalle[v_indice].caso_crm = v_caso_crm

            -- Se determina v_estado_indicador
            -- Si el diagnostico es correcto
            IF v_diagnostico = "101" THEN
               -- Si no hay motivo de rechazo
               IF v_estado_cuenta = "101" THEN
                  LET v_estado_indicador = v_proceso_exitoso
               ELSE
                  LET v_estado_indicador = v_proceso_no_exitoso
               END IF
            ELSE
               LET v_estado_indicador = v_proceso_no_exitoso
            END IF

            DISPLAY "GUARDANDO EN BITACORA"
            DISPLAY "----------------------------------------"
--            DISPLAY "v_estado_indicador :	",v_estado_indicador
--            DISPLAY "p_usuario_cod      :	",p_usuario_cod
--            DISPLAY "Id solicitud       :  ", v_id_solicitud
            DISPLAY "La respuesta del llamado :",r_mensaje 


            -- Se guardan los datos en la bitacora
            CALL fn_guarda_consulta_ws_vent_afore(v_nss_paso,
                                                    v_indicador_marca,
                                                    v_estado_indicador,
                                                    TODAY,
                                                    CURRENT HOUR TO SECOND,
                                                    v_diagnostico,
                                                    v_estado_cuenta,
                                                    0, 
                                                    0,
                                                    p_usuario_cod,
                                                    v_id_solicitud,
                                                    v_caso_crm,
                                                    v_origen)
         END IF -- v_estado
         LET v_indice = v_indice + 1
      END FOREACH 


      OPEN WINDOW respuesta_proceso WITH FORM "RETF4142"

         DISPLAY ARRAY v_arr_detalle
                    TO tbl_resultado.* ATTRIBUTES(ACCEPT = FALSE,CANCEL = FALSE)

            ON ACTION exportar
               CALL fn_genera_archivo(v_arr_detalle)

            ON ACTION salir
               EXIT DISPLAY

         END DISPLAY

      CLOSE WINDOW solicitudes_detalle






      
   ELSE 
      DISPLAY "No existen registros para marcar/desmarcar" 
   END IF 

   RETURN r_estado,r_mensaje

END FUNCTION

#Funcion que recupera y valida si el nss con la modalidad dada,
#tiene un proceso pendiente a ser invocado
#Regresa un 1 si es correcto el proceso, 2 si es proceso de desmarca para
#un NSS aun no marcado y 0 si no tiene proceso pendiente
FUNCTION recuperaDatos(p_nss,p_indicador_marca)

    -- Parametros recibidos
    DEFINE p_nss                CHAR(11)
    DEFINE p_indicador_marca    SMALLINT

    -- Datos a devolver
    DEFINE r_estado             SMALLINT
    DEFINE r_id_solicitud       DECIMAL(9,0)
    DEFINE r_caso_crm           CHAR(10)

    -- Variables auxiliares
    DEFINE v_sql                STRING
    DEFINE v_conteo             SMALLINT
    DEFINE v_estado_indicador   STRING
    DEFINE v_f_intento          DATE

    LET r_estado = 0 -- Se asume que no hay solicitud pendiente de marca o desmarca

    IF p_indicador_marca = 1 THEN
        LET v_estado_indicador = " = 1" -- No Marcado
    ELSE
        -- Se valida el caso que se quiera desmarcar sin aun haber marcado en
        -- PROCESAR
        LET v_estado_indicador = "IN (1,2,4)" -- 1 No Marcado, 2 Marcado, 4 No Desmarcado
    END IF

    -- OBTENER LOS DATOS

    -- Se obtiene el ultimo registro (maximo id solicitud)
    LET v_sql = "SELECT MAX(id_solicitud)              "||
                "FROM   ret_ctr_marca_procesar_maestra "||
                "WHERE  nss = '",p_nss,"'"

    PREPARE prp_id_solicitud FROM v_sql
    EXECUTE prp_id_solicitud INTO r_id_solicitud

    IF r_id_solicitud IS NULL OR r_id_solicitud = 0 THEN  
      LET r_estado = 1  -- Se indica que es correcto ya que puede ser una marca que no se tenga en SACI (TRM)
      LET r_id_solicitud = 0
      LET r_caso_crm = ''
    ELSE 
       -- Se prepara la consulta para obtener los datos
       LET v_sql = "SELECT MAX(f_intento),caso_crm ",
                   "FROM   ret_ctr_marca_procesar  ",
                   "WHERE  id_solicitud     =  ",r_id_solicitud,
                   "  AND  nss              = '",p_nss,"'",
                   "GROUP  BY caso_crm"

       PREPARE prp_datos_proceso FROM v_sql
       EXECUTE prp_datos_proceso INTO v_f_intento,r_caso_crm

       -- VALIDAR LA OPERACION

       -- Se valida que este pendiente de marca o desmarca
       -- (segun sea la modalidad invocada)
       LET v_sql = "SELECT COUNT(*)                       ",
                   "FROM   ret_ctr_marca_procesar_maestra ",
                   "WHERE  id_solicitud     =  ",r_id_solicitud,
                   "  AND  nss              = '",p_nss,"'",
                   "  AND  estado_indicador ",v_estado_indicador

       PREPARE prp_conteo FROM v_sql
       EXECUTE prp_conteo INTO v_conteo

       IF p_indicador_marca = 1 THEN

           -- Si la modalidad es marca solo debe haber un registro
           -- en la tabla maestra
           IF v_conteo = 1 THEN

               LET r_estado = 1 -- Ejecucion correcta

           END IF
       ELSE
           -- Si la modalidad es desmarca debe haber uno o dos registros
           -- en la tabla maestra, uno por la marca, y posiblemente un segundo
           -- por la desmarca (si ya se ha intentado desmarcar)
           IF v_conteo = 2 OR v_conteo = 1 THEN

               LET r_estado = 1 -- Ejecucion correcta

               -- Se valida si existe un registro de No marcado
               LET v_sql = "SELECT COUNT(*)                       ",
                           "FROM   ret_ctr_marca_procesar_maestra ",
                           "WHERE  id_solicitud     =  ",r_id_solicitud,
                           "  AND  nss              = '",p_nss,"'",
                           "  AND  estado_indicador = 1"

               PREPARE prp_valida_estado FROM v_sql
               EXECUTE prp_valida_estado INTO v_conteo

               IF v_conteo = 1 THEN
                   LET r_estado = 2
               END IF

           END IF
       END IF
    END IF 

    RETURN r_estado,r_id_solicitud,r_caso_crm

END FUNCTION

#Funcion que regresa la descripcion del diagnostico
FUNCTION fn_diagnostico_desc(p_diagnostico, p_estado_cuenta)

    DEFINE p_diagnostico   SMALLINT 
    DEFINE p_estado_cuenta SMALLINT

    IF p_diagnostico  = 101 THEN 
       CASE p_estado_cuenta
           WHEN 101
               RETURN "101-101 Aceptada"
           WHEN 102
               RETURN "101-102 NSS no se encuentra registrado en la BDNSAR"
           WHEN 201
               RETURN "101-201 La cuenta se encuentra en proceso de 43 BIS"
           WHEN 202
               RETURN "101-202 En proceso de Traspaso A-A"
           WHEN 203
               RETURN "101-203 En proceso de Unificacion de cuentas"
           WHEN 204
               RETURN "101-204 En proceso de Fusion de Afore"
           WHEN 205
               RETURN "101-205 En proceso de separacion de cuentas"
           WHEN 207
               RETURN "101-207 En proceso de transferencias de recursos"
           WHEN 208
               RETURN "101-208 En proceso de disposicion de recursos"
           WHEN 211
               RETURN "101-211 En proceso de Devolucion de pagos efectuados sin Justificacion Legal"
           WHEN 212
               RETURN "101-212 En proceso de retiros parciales"
           WHEN 213
               RETURN "101-213 En proceso de tramite judicial iniciado por Afore"
           WHEN 216
               RETURN "101-216 Cuenta en proceso de aclaracion por conciliacion"
           WHEN 217
               RETURN "101-217 Cuenta en proceso de seleccion SIEFORE"
           WHEN 219
               RETURN "101-219 Cuenta en proceso de modificación"
           WHEN 220
               RETURN "101-220 Cuenta en proceso de crédito de vivienda"
           WHEN 221
               RETURN "101-221 Cuenta en proceso de crédito de vivienda"
           WHEN 223
               RETURN "101-223 Cuenta en proceso de saldo previo"
           WHEN 224
               RETURN "101-224 No se encuentra marcado"
           WHEN 225
               RETURN "101-225 Existe alguna notificación de pago por Ventanilla INFONAVIT"
           WHEN 226
               RETURN "101-226 Existe alguna notificación de pago por Ventanilla INFONAVIT"
           OTHERWISE
               RETURN "Error no reconocido"
       END CASE
   ELSE
      IF p_diagnostico = 127 THEN 
         RETURN "127 No se pudo invocar al servicio de PROCESAR"
      ELSE 
         RETURN p_diagnostico CLIPPED || "-" || p_estado_cuenta CLIPPED || " Error no reconocido"
      END IF 
   END IF 
   

END FUNCTION

FUNCTION fn_llena_tabla(p_dato,p_tabla)
DEFINE  p_dato      STRING
DEFINE  p_tabla     CHAR(25)
DEFINE  v_respuesta SMALLINT 
DEFINE  i           INTEGER 
DEFINE  v_dato       CHAR(25)
DEFINE  v_num_reg   INTEGER 
DEFINE  v_query     STRING 


   LET v_dato = ""
   LET v_num_reg = 0
   LET v_query = "INSERT INTO ", p_tabla CLIPPED, " (dato) VALUES (?)"
   PREPARE prp_inserta_datos FROM v_query

   LET v_query = "SELECT COUNT(*)",
                  " FROM ", p_tabla
                  
   PREPARE prp_cuenta_insertados FROM v_query

   FOR i = 1 TO p_dato.getLength()
      IF i = 1 THEN
         LET v_dato = v_dato CLIPPED, p_dato.subString(i,i)
      ELSE
         IF p_dato.subString(i,i) = "\n" THEN 
            --DISPLAY "EL DATO A INSERTAR >",v_dato, "<"
            EXECUTE prp_inserta_datos USING  v_dato
            LET v_dato = ""
         ELSE 
            LET v_dato = v_dato CLIPPED, p_dato.subString(i,i)
         END IF 
      END IF 
   END FOR 
   IF i > 1 AND p_dato.subString(i,i) <> "\n" THEN 
      EXECUTE prp_inserta_datos USING  v_dato
   END IF 

   EXECUTE prp_cuenta_insertados INTO v_num_reg 

   DISPLAY "Se insertaron en la temporal ", p_tabla, " : ", v_num_reg
      

RETURN v_respuesta
END FUNCTION 
#Funcion que recupera y valida si el nss con la modalidad dada,
#tiene un proceso pendiente a ser invocado
#Regresa un 1 si es correcto el proceso, 2 si es proceso de desmarca para
#un NSS aun no marcado y 0 si no tiene proceso pendiente
FUNCTION fn_busca_id_solicitud_caso_crm(p_nss)

   -- Parametros recibidos
   DEFINE p_nss                CHAR(11)

   -- Datos a devolver
   DEFINE r_estado             SMALLINT
   DEFINE r_id_solicitud       DECIMAL(9,0)
   DEFINE r_caso_crm           CHAR(10)

   -- Variables auxiliares
   DEFINE v_sql                STRING
   DEFINE v_conteo             SMALLINT
   DEFINE v_estado_indicador   STRING
   DEFINE v_f_intento          DATE

   -- OBTENER LOS DATOS
   LET r_estado = 0
   LET r_id_solicitud = 0
   LET r_caso_crm = ''
   
   -- Se obtiene el ultimo registro (maximo id solicitud)
   LET v_sql = "SELECT MAX(a.id_solicitud)              "||
               "FROM   ret_solicitud_generico a,        "||
               "       ret_ley73_generico b             "||
               "WHERE  a.id_solicitud = b.id_solicitud  "||
               "AND    a.nss = '",p_nss,"'              "||
               "AND    a.modalidad_retiro = 3           "||
               "AND    b.gpo_ley73 = 1                  "
                

   PREPARE prp_id_sol FROM v_sql
   EXECUTE prp_id_sol INTO r_id_solicitud

   IF r_id_solicitud IS NULL OR r_id_solicitud = 0 THEN  
      LET r_estado = 1  -- Se indica que es correcto ya que puede ser una marca que no se tenga en SACI (TRM)
      LET r_id_solicitud = 0
      LET r_caso_crm = ''
   ELSE 
      -- Se prepara la consulta para obtener los datos
      LET v_sql = "SELECT caso_adai               ",
                  "FROM   ret_solicitud_generico  ",
                  "WHERE  id_solicitud     =  ",r_id_solicitud,
                  "AND    nss              = '",p_nss,"'"

      PREPARE prp_datos FROM v_sql
      EXECUTE prp_datos INTO r_caso_crm
      IF r_caso_crm IS NULL OR r_caso_crm = '0' THEN  
         LET r_estado = 1  -- Se indica que es correcto ya que puede ser una marca que no se tenga en SACI (TRM)
         LET r_caso_crm = ''

      END IF
   END IF            -- Si la modalidad es desmarca debe haber uno o dos registros

   RETURN r_estado,r_id_solicitud,r_caso_crm

END FUNCTION

#Funcion que genera un archivo plano con el resultado de la consulta,
#separados por pipes.
FUNCTION fn_genera_archivo(p_arr_detalle)

   -- Valores a insertar en el archivo de salida
   DEFINE p_arr_detalle DYNAMIC ARRAY OF RECORD
      nss                      CHAR(11),
      diagnositico             CHAR(3), 
      estado_cuenta            CHAR(3),
      descripcion              CHAR(80), 
      id_solicitud             DECIMAL(9,0), 
      caso_crm                 CHAR(10),
      modalidad                CHAR(8)
   END RECORD

   -- Variables auxiliares
   DEFINE v_channel_sal    base.Channel -- Creacion del archivo de salida
   DEFINE v_texto          STRING
   DEFINE v_archivo        STRING
   DEFINE v_ruta_env_arch  LIKE seg_modulo.ruta_envio -- Ruta de envio
   DEFINE v_ruta_arch      STRING -- Ruta de archivo
   DEFINE v_contador       INTEGER
   DEFINE v_string         base.StringBuffer

   --Se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio, ruta_bin
   INTO   v_ruta_env_arch
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- Se prepara el archivo de salida
   LET v_channel_sal = base.Channel.create()

   LET v_string = base.StringBuffer.create()
   LET v_archivo = TIME(CURRENT)
   CALL v_string.append(v_archivo)
   CALL v_string.replace(":","",2)

   -- Establecemos el nombre del archivo a partir de la fecha
   LET v_archivo = "marca_desmarca_"||(TODAY USING "ddmmyyyy_")||v_string.toString()||".mdprocesar"

   LET v_ruta_arch = v_ruta_env_arch CLIPPED,"/",v_archivo
   CALL v_channel_sal.openFile(v_ruta_arch,"w")

   -- Se escribe el encabezado
   LET v_texto =  "NSS|"           ||
                  "DIAGNOSTICO|"   ||
                  "ESTADO|"        ||
                  "DESCRIPCION|"   ||
                  "MODALIDAD|"                            
   CALL v_channel_sal.writeLine(v_texto)

   -- Se escribe el detalle
   FOR v_contador = 1 TO p_arr_detalle.getLength()
      LET v_texto =  p_arr_detalle[v_contador].nss           ,"|",
                     p_arr_detalle[v_contador].diagnositico  ,"|",
                     p_arr_detalle[v_contador].estado_cuenta ,"|",
                     p_arr_detalle[v_contador].descripcion   ,"|",
                     p_arr_detalle[v_contador].modalidad     ,"|"  
      CALL v_channel_sal.writeLine(v_texto)
   END FOR

   CALL v_channel_sal.close()

   CALL v_string.clear()
   CALL v_string.append(v_ruta_env_arch)
   CALL v_string.replace(" ","",0)

   LET v_texto = v_string.toString()

   CALL fn_mensaje("Atención","Ruta de archivo de salida: "||v_texto||"/"||v_archivo,"stop")

END FUNCTION
