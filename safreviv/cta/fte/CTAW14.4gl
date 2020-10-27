####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAW14                                        #
#Objetivo          =>Programa que ejecuta le llamado al WS de      #
#                    marcas de procesar SIN EJECUTAR las reglas    #
#                    de negocio del modulo de creditos             #
#Fecha inicio      =>09 JUNIO 2012                                 #
####################################################################

GLOBALS "CTAW14.inc"    #Archivo de variables globales del lanzador
GLOBALS "CTAW12.inc"    #Archivo de variables globales del WS de marcas



#Parametros de conexion
PRIVATE DEFINE v_url_servidor       VARCHAR(100) 
PRIVATE DEFINE v_usuario            VARCHAR(40) 
PRIVATE DEFINE v_password           VARCHAR(40) 
PRIVATE DEFINE v_intentos           DECIMAL(9,0)

FUNCTION fn_solicita_marca_procesar(p_solicitud_marca)
   DEFINE p_solicitud_marca         solicita_marca
   DEFINE v_respuesta_marca         respuesta_marca
   DEFINE v_solicitud               tSolicMarcaVO
   DEFINE v_respuesta               tSolicMarcaRespVO

   CALL fn_configura_ws(WS_MARCA) --1 corresponde a la clave del WS de marca

   #Se llenan los parametros que se enviaran al WS
   LET v_solicitud.* = p_solicitud_marca.*

   #Se manda llamar la funcion que invoca el cliente del WS
   CALL fn_solicita_marca( v_url_servidor CLIPPED, 
   #CALL fn_marca_procesar( v_url_servidor CLIPPED,
                           v_usuario      CLIPPED, 
                           v_password     CLIPPED, 
                           v_intentos, 
                           v_solicitud.*) 
   RETURNING v_respuesta.*

    # se asignan los valores de respuesta
    LET v_respuesta_marca.* = v_respuesta.*

   RETURN v_respuesta_marca.*
END FUNCTION

FUNCTION fn_solicita_desmarca_procesar(p_solicitud_desmarca)
   DEFINE p_solicitud_desmarca         solicita_desmarca
   DEFINE v_respuesta_desmarca         respuesta_desmarca
   DEFINE v_solicitud                  tSolicDesmarcaVO
   DEFINE v_respuesta                  tSolicDesmarcaRespVO

   CALL fn_configura_ws(WS_DESMARCA) --3 corresponde a la clave del WS de desmarca

   #Se llenan los parametros que se enviaran al WS
   LET v_solicitud.* = p_solicitud_desmarca.*

   #Se manda llamar la funcion que invoca el cliente del WS
   CALL fn_solicita_desmarca( v_url_servidor CLIPPED,
   #CALL fn_desmarca_procesar( v_url_servidor CLIPPED,
                              v_usuario      CLIPPED, 
                              v_password     CLIPPED, 
                              v_intentos, 
                              v_solicitud.*) 
   RETURNING v_respuesta.*

    # se asignan los valores de respuesta
    LET v_respuesta_desmarca.* = v_respuesta.*

   RETURN v_respuesta_desmarca.*
END FUNCTION

PRIVATE FUNCTION fn_configura_ws(p_tipo_ws)
   DEFINE p_tipo_ws     CHAR(5)
   DEFINE v_consulta    STRING

   DATABASE safre_viv
   
   LET v_consulta = "SELECT   ruta_servidor, 
                              usuario, 
                              password, 
                              num_reintento 
                     FROM     wsv_cliente 
                     WHERE    cve_cliente = ?"
   PREPARE exe_consulta FROM v_consulta

   EXECUTE exe_consulta USING p_tipo_ws INTO  v_url_servidor,
                                              v_usuario,
                                              v_password,
                                              v_intentos

   DISPLAY "Conectando a la siguiente URL: ", v_url_servidor
   DISPLAY "Usuario: ", v_usuario
   DISPLAY "Psw: " , v_password
   DISPLAY "Intentos a ejecutar: ",v_intentos

END FUNCTION