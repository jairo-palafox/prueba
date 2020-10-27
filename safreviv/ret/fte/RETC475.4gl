################################################################################
#Modulo        => RET                                                          #
#Programa      => RETC475                                                      #
#Ojetivo       => Consulta las notificaciones de Procesar hacia Infonavit de   #
#                 la cuenta CLABE                                              #
#Fecha inicio  => Febrero, 2019.                                               #
#Requerimiento => SACI2018-156                                                 #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
DATABASE safre_viv

GLOBALS "RETG01.4gl"

MAIN

    -- Parametros pasados al mandar llamar el programa
    DEFINE p_usuario            CHAR(20)    

    -- Variables usadas para realizar la busqueda
    DEFINE v_nss                STRING
    DEFINE v_chk_aceptadas      BOOLEAN
    DEFINE v_chk_rechazadas     BOOLEAN
    DEFINE v_fecha_marca_inicio DATE
    DEFINE v_fecha_marca_fin    DATE

    -- Variables auxiliares
    DEFINE v_sql                  STRING
    DEFINE v_contador             INTEGER


    DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod -- clave del usuario firmado
    DEFINE p_tipo_ejecucion    SMALLINT -- forma como ejecutara el programa
    DEFINE p_s_titulo          STRING   -- titulo de la ventana 

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
    OPEN WINDOW busqueda_marcas WITH FORM "RETC4751"

        INPUT BY NAME  v_nss,v_chk_aceptadas,v_chk_rechazadas,v_fecha_marca_inicio,
                       v_fecha_marca_fin
                       ATTRIBUTES(UNBUFFERED,CANCEL = 0)

            ON ACTION ACCEPT
                --Se valida que el NSS sea un numero de 11 digitos
--                IF NOT  fn_numero_valido(v_nss) OR (length(v_nss) <> 11 AND length(v_nss) > 0) THEN
--                    CALL fn_mensaje("Atención",
--                                    "El NSS debe ser un numero de 11 digitos.",
--                                    "stop")
--                    NEXT FIELD v_nss
--                END IF
                CALL fn_busca_solicitudes(v_nss,v_chk_aceptadas,v_chk_rechazadas,
                                          v_fecha_marca_inicio,v_fecha_marca_fin)
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

#Funcion que realiza la busqueda de las solicitudes y las imprime
FUNCTION fn_busca_solicitudes(p_nss,p_chk_aceptadas,p_chk_rechazadas,
                              p_fecha_marca_inicio,p_fecha_marca_fin)

-- Parametros para realizar la busqueda
DEFINE p_nss                STRING 
DEFINE p_chk_aceptadas      BOOLEAN
DEFINE p_chk_rechazadas     BOOLEAN
DEFINE p_fecha_marca_inicio DATE
DEFINE p_fecha_marca_fin    DATE

-- Variables auxiliares
DEFINE v_contador       INTEGER
DEFINE v_sql            STRING
DEFINE v_resultado      SMALLINT

-- Resultado de la busqueda
DEFINE v_detalle_solicitud DYNAMIC ARRAY OF RECORD
          nss                     CHAR(11),
          codigo                  CHAR(4),
          mensaje             CHAR(100),
          f_marca                 DATE,
          f_notificacion          DATE
END RECORD
-- Resultado de la busqueda
DEFINE v_detalle_solicitud_comp DYNAMIC ARRAY OF RECORD
          id_solicitud            DECIMAL(10,0),
          id_derechohabiente      DECIMAL(10,0)
END RECORD

DEFINE v_string         base.StringBuffer
   LET v_string = base.StringBuffer.create()

   -- Se hace le cruce entre ret_ley73_generico y la tabla ret_ws_sol_retiro_vent_afore,
   -- se obtiene el grupo de ventanilla de ret_solicitud_generico y 
   LET v_sql = "SELECT rw.nss, rwd.codigo, rwd.mensaje, rsg.f_solicitud, DATE(rwd.f_notifica)\n"||
             "FROM   ret_ws_notifica_cta_clabe_resp rwd,                                         \n"||
             "       ret_ws_sol_retiro_vent_afore rw                                             \n"||
             "       LEFT OUTER JOIN ret_solicitud_generico rsg                                  \n"||
             "                    ON rw.id_solicitud = rsg.id_solicitud                          \n"||
             "                   AND rsg.modalidad_retiro = 3                                    \n"||
             "WHERE  rw.id_peticion = rwd.id_peticion                                            \n"
          
   IF p_nss IS NOT NULL THEN 
      DROP TABLE IF EXISTS tmp_nss;
      CREATE TEMP TABLE tmp_nss (dato CHAR(11));
      LET v_sql = v_sql, " AND    rw.nss IN (SELECT dato FROM tmp_nss) \n"
      CALL fn_llena_tabla (p_nss, "tmp_nss") RETURNING v_resultado
   END IF 

   IF p_chk_aceptadas AND NOT p_chk_rechazadas THEN
      LET v_sql = v_sql||"AND    rwd.codigo = 101 \n"
   END IF 
   IF NOT p_chk_aceptadas AND p_chk_rechazadas THEN
      LET v_sql = v_sql||"AND    rwd.codigo <> 101 \n"
   END IF 

   IF p_fecha_marca_inicio IS NOT NULL AND p_fecha_marca_fin IS NOT NULL THEN 
      LET v_sql = v_sql||"AND    rsg.f_solicitud between '", p_fecha_marca_inicio, "' AND '", p_fecha_marca_fin, "'\n"
   END IF 

   DISPLAY v_sql
   CALL v_detalle_solicitud.clear()
   CALL v_detalle_solicitud_comp.clear()

   PREPARE prp_detalle_solicitudes FROM v_sql
   DECLARE cur_detalle_solicitudes CURSOR FOR prp_detalle_solicitudes

   LET v_contador = 1

   FOREACH cur_detalle_solicitudes INTO v_detalle_solicitud[v_contador].*
      LET v_contador = v_contador + 1
   END FOREACH

   -- Se elimina el elemento nulo
   IF v_contador > 1 THEN
      CALL v_detalle_solicitud.deleteElement(v_contador)

     OPEN WINDOW solicitudes_detalle WITH FORM "RETC4752"

         DISPLAY ARRAY v_detalle_solicitud
                    TO solicitud_detalle.* ATTRIBUTES(ACCEPT = FALSE,CANCEL = FALSE)

             ON ACTION exportar
                 CALL fn_genera_archivo(v_detalle_solicitud)

             ON ACTION salir
                 EXIT DISPLAY

         END DISPLAY

     CLOSE WINDOW solicitudes_detalle
 ELSE
     CALL fn_mensaje("Atención","No se encontro ningún resultado.","stop")
 END IF

END FUNCTION

#Funcion que genera un archivo plano con el resultado de la consulta,
#separados por pipes.
FUNCTION fn_genera_archivo(p_detalle_solicitud)

-- Valores a insertar en el archivo de salida
DEFINE p_detalle_solicitud DYNAMIC ARRAY OF RECORD
       nss                     CHAR(11),
       codigo                  CHAR(4),
       mensaje             CHAR(100),
       f_marca                 DATE,
       f_notificacion          DATE
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
   LET v_archivo = "consulta_notifica_procesar_infonavit_"||(TODAY USING "ddmmyyyy_")||v_string.toString()||".rmdnp"

   LET v_ruta_arch = v_ruta_env_arch CLIPPED,"/",v_archivo
   CALL v_channel_sal.openFile(v_ruta_arch,"w")

   -- Se escribe el encabezado
   LET v_texto = "NSS|"                                ||
               "Código|mensaje|"                   ||
               "Fecha de Marca|"                       ||
               "Fecha de notificacion"
   CALL v_channel_sal.writeLine(v_texto)

   -- Se escribe el detalle
   FOR v_contador = 1 TO p_detalle_solicitud.getLength()
     LET v_texto =  p_detalle_solicitud[v_contador].nss                                     ,"|",
                    p_detalle_solicitud[v_contador].codigo                                  ,"|",
                    p_detalle_solicitud[v_contador].mensaje                             ,"|",
                    (p_detalle_solicitud[v_contador].f_marca        USING "dd/mm/yyyy"     ),"|",
                    (p_detalle_solicitud[v_contador].f_notificacion USING "dd/mm/yyyy"     )
     CALL v_channel_sal.writeLine(v_texto)
   END FOR

   CALL v_channel_sal.close()

   CALL v_string.clear()
   CALL v_string.append(v_ruta_env_arch)
   CALL v_string.replace(" ","",0)

   LET v_texto = v_string.toString()

   CALL fn_mensaje("Atención","Ruta de archivo de salida: "||v_texto||"/"||v_archivo,"stop")

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
            DISPLAY "EL DATO A INSERTAR >",v_dato, "<"
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