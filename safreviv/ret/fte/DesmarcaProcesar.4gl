################################################################################
#Modulo        => RET                                                          #
#Programa      => DesmarcaProcesar                                             #
#Ojetivo       => Desmarca Manual mediante NSS con consulta a PROCESAR         #
#Fecha inicio  => Mayo, 2019.                                                  #
#Requerimiento =>                                                              #
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
   DEFINE v_estado             SMALLINT
   DEFINE v_mensaje            STRING

   --Se obtienen los valores de la ejecucion
   -- se obtienen los parametros de ejecucion
   LET v_nss    = ARG_VAL(1)
   --Se valida que el NSS sea un numero de 11 digitos
   IF v_nss IS NULL  THEN
      DISPLAY "El NSS debe ser un número válido."
   END IF


   -- Llamada a la funcion que realiza la marca o desmarca
   CALL fn_marca_desmarca_procesar(v_nss,60,"")
                       RETURNING v_estado,v_mensaje

END MAIN

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
      CREATE TEMP TABLE tmp_nss (dato CHAR(11));
      CALL fn_llena_tabla (p_nss, "tmp_nss") RETURNING v_resultado
   END IF 
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
         CALL recuperaDatos(v_nss_paso,v_indicador_marca) RETURNING v_estado,
                                                                    v_id_solicitud,
                                                                    v_caso_crm

         DISPLAY "Datos del NSS"
         DISPLAY "----------------------------------------"
         DISPLAY "p_nss             :	",v_nss_paso
--         DISPLAY "p_modalidad       :	",p_modalidad
--         DISPLAY "v_indicador_marca :	",v_indicador_marca
--         DISPLAY "v_estado          :	",v_estado
--         DISPLAY "v_id_solicitud    :	",v_id_solicitud
--         DISPLAY "v_caso_crm        :	",v_caso_crm
         DISPLAY ""
         DISPLAY ""

         -- Ejecucion correcta
         IF v_estado = 1 THEN

            -- Se realiza la consulta a PROCESAR
            CALL fn_consulta_saldo_vivienda_afore(p_nss, p_modalidad)
            RETURNING v_diagnostico, v_estado_cuenta, v_aivs_viv92,
                     v_pesos_viv92, v_aivs_viv97, v_pesos_viv97,v_cod_rechazo
           -- LET v_diagnostico = '101'
           -- LET v_estado_cuenta = '101'
            DISPLAY "CONSULTA A PROCESAR"
            DISPLAY "----------------------------------------"
            DISPLAY "v_diagnostico   :	",v_diagnostico
            DISPLAY "v_estado_cuenta :	",v_estado_cuenta
--            DISPLAY "v_aivs_viv92    :	",v_aivs_viv92
--            DISPLAY "v_pesos_viv92   :	",v_pesos_viv92
--            DISPLAY "v_aivs_viv97    :	",v_aivs_viv97
--            DISPLAY "v_pesos_viv97   :	",v_pesos_viv97
--            DISPLAY ""
            DISPLAY ""

            -- Se determina v_estado_indicador
            -- Si el diagnostico es correcto
            IF v_diagnostico = "101" THEN
               -- Si no hay motivo de rechazo
               IF v_estado_cuenta = "101" THEN
                  LET v_estado_indicador = v_proceso_exitoso
                  LET r_mensaje = v_s_proceso," del NSS: '",v_nss_paso,"' exitoso"
               ELSE
                  LET v_estado_indicador = v_proceso_no_exitoso
                  LET r_mensaje = "El proceso de ",v_s_proceso," del NSS: '",
                                v_nss_paso,"' no fue exitoso.\nRechazo: ",
                                fn_diagnostico_desc(v_estado_cuenta)
               END IF
            ELSE
               LET v_estado_indicador = v_proceso_no_exitoso
               LET r_mensaje = "El proceso de ",v_s_proceso," del NSS: '",
                                v_nss_paso,"' no fue exitoso.\nNo hay respuesta de PROCESAR"
            END IF

            DISPLAY "GUARDANDO EN BITACORA"
            DISPLAY "----------------------------------------"
--            DISPLAY "v_estado_indicador :	",v_estado_indicador
--            DISPLAY "p_usuario_cod      :	",p_usuario_cod
--            DISPLAY "Id solicitud       :  ", v_id_solicitud
            DISPLAY "La respuesta del llamado :",r_mensaje 

            IF v_id_solicitud IS NOT NULL AND v_id_solicitud <> 0 THEN 

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
            END IF 
         ELSE
            -- Si se intento desmarcar, pero no estaba marcado
            IF v_estado = 2 THEN
               -- Se guardan los datos en la bitacora
               CALL fn_guarda_consulta_ws_vent_afore(v_nss_paso,
                                                        v_indicador_marca,
                                                        6,
                                                        TODAY,
                                                        CURRENT HOUR TO SECOND,
                                                        '',
                                                        '',
                                                        v_aivs_viv92, 
                                                        v_aivs_viv97,
                                                        p_usuario_cod,
                                                        v_id_solicitud,
                                                        v_caso_crm,
                                                        v_origen)
               LET r_mensaje = "Se intento desmarcar el NSS: '",v_nss_paso,"'."||
                               "\nPero aun no se habia podido marcar."
            ELSE
               LET r_mensaje = "No existe un proceso pendiente de ",v_s_proceso,
                               " para el NSS: '",v_nss_paso,"'"
            END IF
         END IF -- v_estado
      END FOREACH 
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
FUNCTION fn_diagnostico_desc(p_estado_cuenta)

    DEFINE p_estado_cuenta SMALLINT

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
   LET v_query = "INSERT INTO ", p_tabla CLIPPED, " VALUES (?)"
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