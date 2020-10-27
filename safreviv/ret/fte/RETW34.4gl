--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETW34                                                  #
#OBJETIVO          => WS CONSULTA DE PERFILAMIENTO                            #
#FECHA INICIO      => 16-jun-2014                                             #
###############################################################################

IMPORT FGL WSHelper
IMPORT com

DATABASE safre_viv   
# 
# USER GLOBALS VARIABLES
#
GLOBALS
-- registro de entrada para la consulta
DEFINE ws_ret_cons_perfilamiento_in RECORD
         nss                    CHAR(11), -- nss del trabajador
         rfc                    CHAR(13), -- rfc del trabajador
         l73_titular                CHAR(02), -- Es titular
         l73_pensionado             CHAR(02), -- Es Pensionado
         l73_imss                   CHAR(02), -- Pension IMSS
         l73_p_p_p                  CHAR(02), -- Plan Privado de Pension
         l73_f_defun                CHAR(02), -- Tiene fecha de Defuncion
         l73_f_emision              CHAR(02), -- Fecha de Emision de Resolucion Posterior al 12/01/2012
         l73_tiene_demanda          CHAR(02), -- Tiene Demanda
         l73_tramite_desistimiento  CHAR(02), -- Esta en Tramite o Desistimiento
         l73_laudo_firme            CHAR(02), -- Tiene Laudo o esta en Firme
         l73_a_favor                CHAR(02), -- A Favor
         l73_en_contra              CHAR(02), -- En Contra
         fa_titular                 CHAR(02), -- Es titular para Fondo de Ahorro
         fa_pensionado              CHAR(02), -- Es Pensionado para Fondo de Ahorro
         fa_imss                    CHAR(02), -- Pension IMSS para Fondo de Ahorro
         fa_p_p_p                   CHAR(02), -- Plan Privado de Pension para Fondo de Ahorro
         fa_f_defun                 CHAR(02)  -- Tiene fecha de Defuncion para Fondo de Ahorro
       END RECORD,
       -- registro de respuesta
       ws_ret_cons_perfilamiento_out  RECORD
         nss                    CHAR(11), -- Número de seguridad social del trabajador
         rfc                    CHAR(13), -- rfc del trabajador
         grupo                  SMALLINT, -- Grupo Segun Perfilamiento
         causal                 SMALLINT  -- Causal Segun Perfilamiento
       END RECORD
         
DEFINE g_indice_retiro      SMALLINT, -- indice del tipo de retiro consultado
       g_id_derechohabiente DECIMAL(9,0) ,
       g_id_fondo72         DECIMAL(9,0) ,
       g_causal_ref         SMALLINT     ,
       g_nss                CHAR(11)     ,
       g_rfc                CHAR(13)     , -- rfc del trabajador
       g_acc_acciones       DECIMAL(14,6),
       g_acc_pesos          DECIMAL(14,6),
       g_tanto_adicional    DECIMAL(14,6),
       g_id_solicitud       DECIMAL(9,0) ,
       g_refer              CHAR(18)     ,
       g_id_beneficiario              SMALLINT     , -- Identificador de beneficiario (si aplica)
       g_nombre             CHAR(18)     , -- Nombre del beneficiario 
       g_ape_pat            CHAR(18)     , -- Apellido paterno 
       g_ape_mat            CHAR(18)     , -- Apellido materno           
       g_causal_adai        SMALLINT     , -- Clave de Adai 
       g_entidad            SMALLINT     , -- Entidad federativa
       g_id_datamart        DECIMAL(9,0) , -- Identificador datamart
       g_causal_retiro      SMALLINT     ,
       g_bnd_uso_seq        SMALLINT     ,
       g_sq_ret_solicitud   DECIMAL(9,0) -- id de solicitud nueva 

DEFINE g_r_tmp_id_fondo72   RECORD
        nss                  CHAR(11)     ,
        id_derechohabiente   DECIMAL(9,0) ,
        id_afi_fondo72       DECIMAL(9,0) ,
        importe              DECIMAL(12,2),
        rfc                  CHAR(13)     ,
        estatus              SMALLINT     ,
        rechazo_cod          SMALLINT
       END RECORD

-- =======================================================
-- constantes para la evaluacion del resultado de la ejecucion del webservice
CONSTANT  g_res_procesada                    SMALLINT = 0  ,
          g_res_sin_solicitud                SMALLINT = -1 ,
          g_res_desconectado_del_servidor    SMALLINT = -2 ,
          g_res_conexion_con_cliente_perdida SMALLINT = -3 ,
          g_res_servidor_interrumpido_ctrl_c SMALLINT = -4 ,
          g_res_error_interno                SMALLINT = -10,
          g_msg_procesada                    STRING = "Solicitud procesada"                  ,
          g_msg_sin_solicitud                STRING = "Sin solicitud"                        ,
          g_msg_desconectado_del_servidor    STRING = "Desconectado del servidor"            ,
          g_msg_conexion_con_cliente_perdida STRING = "Se perdió la conexión con el cliente" ,
          g_msg_servidor_interrumpido_ctrl_c STRING = "Se interrumpió el servidor con CTRL-C",
          g_msg_error_interno                STRING = "Ocurrió un error interno"
        
DEFINE serverURL STRING -- URL del servidor
DEFINE v_pantalla    SMALLINT

END GLOBALS

#
# MAIN
#
MAIN
DEFINE v_resultado       INTEGER, -- recibe el resultado de la ejecucion del servicio 
       v_ruta_log        STRING,
       v_cadena          STRING,
       v_ruta_ejecutable VARCHAR(40)
      
  DEFER INTERRUPT

  -- se obtiene la ruta ejecutable
  SELECT ruta_bin
  INTO   v_ruta_ejecutable
  FROM   seg_modulo
  WHERE  modulo_cod = "ret"
  
  -- se define la ruta del log
  LET v_ruta_log = v_ruta_ejecutable CLIPPED, "/RETW34."
  LET v_cadena   = TODAY USING "yyyymmdd"
  LET v_ruta_log = v_ruta_log || v_cadena
  LET v_cadena   = CURRENT HOUR TO HOUR
  LET v_ruta_log = v_ruta_log || v_cadena
  LET v_cadena   = CURRENT MINUTE TO MINUTE
  LET v_ruta_log = v_ruta_log || v_cadena
  LET v_cadena   = CURRENT SECOND TO SECOND
  LET v_ruta_log = v_ruta_log || v_cadena || ".log"
  
  
  DISPLAY "Ruta del log creada del servidor: ", v_ruta_log
  
  -- se inicia el log del programa
  CALL STARTLOG(v_ruta_log)

  LET v_pantalla = FALSE
  #
  # Check arguments
  #
  IF num_args() = 2 AND arg_val(1) = "-W" THEN
      LET serverURL = arg_val(2)
      CALL fn_crea_servicio_consulta_perfilamiento(TRUE)
      EXIT PROGRAM
  ELSE 
    IF num_args() = 2 AND arg_val(1) = "-S" THEN
      LET v_pantalla = TRUE
      CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
      CLOSE WINDOW SCREEN
      
      -- se abre la ventana monitor del servidor (en consola)
      OPEN WINDOW w WITH FORM "RETWE340" ATTRIBUTES(TEXT = "Consulta Perfilamiento")
      --display_status("Retiro Service Startup")
    ELSE
      IF num_args() <> 0 THEN
        CALL exitHelp()
        EXIT PROGRAM
      END IF
    END IF
  END IF
  
  -- se crea el servicio
  CALL ERRORLOG("invoca creacion de servicio Perfilamiento")
  CALL fn_crea_servicio_consulta_perfilamiento(FALSE)

  -- se inicia el servidor
  CALL ERRORLOG("Iniciando servidor Perfilamiento version 1.0 ...")

  -- se inicia el motor de WS
  CALL com.WebServiceEngine.Start()
  CALL ERRORLOG("Servidor de Perfilamiento en escucha")

  -- si se tiene pantalla
  
    WHILE ( TRUE )
       LET v_resultado = com.WebServiceEngine.ProcessServices(-1) -- [sin timeout -1]
       CALL ERRORLOG("Regresa de procesar el servicio: ")
       CALL ERRORLOG(v_resultado)

       -- se verifica el resultado
       CASE v_resultado
         WHEN g_res_procesada
            DISPLAY g_msg_procesada
            CALL ERRORLOG(g_msg_procesada)
           
         WHEN g_res_sin_solicitud
            DISPLAY g_msg_sin_solicitud
            CALL ERRORLOG(g_msg_procesada)
           
         WHEN g_res_desconectado_del_servidor
            DISPLAY g_msg_desconectado_del_servidor
            CALL ERRORLOG(g_msg_procesada)
            EXIT PROGRAM   # The Application server has closed the connection
           
         WHEN g_res_conexion_con_cliente_perdida
            CALL ERRORLOG(g_msg_procesada)
            DISPLAY g_msg_conexion_con_cliente_perdida
           
         WHEN g_res_servidor_interrumpido_ctrl_c
            CALL ERRORLOG(g_msg_procesada)
            DISPLAY g_msg_servidor_interrumpido_ctrl_c
           
         WHEN g_msg_error_interno
            CALL ERRORLOG(g_msg_procesada)
            DISPLAY g_msg_error_interno
           
         OTHERWISE 
            -- se recibio algun otro codigo de retorno
            DISPLAY "Se recibio otro codigo de retorno"
            CALL ERRORLOG(g_msg_procesada)
       END CASE

       IF ( INT_FLAG <> 0 ) THEN
          LET INT_FLAG = 0
          EXIT WHILE
       END IF
       
    END WHILE
    
    CALL ERRORLOG("El servidor se detuvo")
END MAIN

{
======================================================================
Clave: 
Nombre: fn_crea_servicio_consulta_perfilamiento
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera el servicio web de retiro generico que consulta el perfilamiento del derechohabiente

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio_consulta_perfilamiento(p_generar_WSDL)
  DEFINE v_webservice         com.WebService       # WebService
  DEFINE op                   com.WebOperation     # Operation of a WebService
  DEFINE v_service_NameSpace  STRING -- namespace del servicio
  DEFINE p_generar_WSDL       SMALLINT -- booleana que indica si se solicito enviar el WSDL
  DEFINE v_resultado          INTEGER

  -- se declara el namespace del servicio
  LET v_service_NameSpace = "http://localhost/"
  LET v_service_NameSpace = "http://www.infonavit.gob.mx/"

  TRY
    -- =============================
    -- se crea el servicio
    LET v_webservice = com.WebService.CreateWebService("consultaPerfilamiento", v_service_NameSpace)
  
    -- =============================
    -- Publicacion de las funciones
    
    --LET op = com.WebOperation.CreateRPCStyle("fn_ret_perfilamiento","fn_ret_perfilamiento",ws_ret_cons_perfilamiento_in,ws_ret_cons_perfilamiento_out)
    LET op = com.WebOperation.CreateDOCStyle("fn_ret_perfilamiento","fn_ret_perfilamiento",ws_ret_cons_perfilamiento_in,ws_ret_cons_perfilamiento_out)
    --CALL v_webservice.publishOperation(op, "urn:http://10.90.8.199:7780/consultaPerfilamiento/fn_ret_perfilamiento") -- se crea con el nombre de operacion fn_ret_perfilamiento
    CALL v_webservice.publishOperation(op, "fn_ret_perfilamiento") -- se crea con el nombre de operacion fn_ret_perfilamiento

    -- si se hace generacion del WSDL
    IF ( p_generar_WSDL ) THEN
       -- Generar el WSDL
       LET v_resultado = v_webservice.saveWSDL(serverURL)
       
       -- si se genero el WSDL sin errores
       IF ( v_resultado = 0 ) THEN
          DISPLAY "WSDL Perfilamiento creado exitosamente"
       ELSE
          DISPLAY "ERROR: No se pudo crear el WSDL Perfilamiento"
       END IF
    ELSE
       -- =========================
       -- REgistro del servicio
       CALL com.WebServiceEngine.RegisterService(v_webservice)  
       --display_status("Retiro 72-92 Service registrado")
       CALL ERRORLOG("Se registro el servicio consulta perfilamiento")
    END IF
    
  CATCH -- en caso de error
    DISPLAY("No se pudo crear el servicio 'Consulta Perfilamiento': " || STATUS)
    EXIT PROGRAM
  END TRY

END FUNCTION

FUNCTION exitHelp()
  DISPLAY "Usage: "
  DISPLAY "  ", arg_val(0)
  DISPLAY "    Start the server on port defined by FGLAPPSERVER"
  DISPLAY "  ", arg_val(0), " -W serverurl"
  DISPLAY "    Generate the WSDL file for the given url"
  DISPLAY "  ", arg_val(0), " -S port"
  DISPLAY "    Start service in graphical mode and on given port"
  EXIT PROGRAM
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_ret_perfilamiento
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Funcion del WS que obtiene el perfilamiento del derechohabiente 

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_ret_perfilamiento()
DEFINE v_indice     SMALLINT, -- indice de la subcuenta
       v_nss                    LIKE afi_derechohabiente.nss,
       v_rfc                    LIKE afi_derechohabiente.rfc,
       v_l73_titular                CHAR(02), -- Es titular  
       v_l73_pensionado             CHAR(02), -- Es Pensionado
       v_l73_imss                   CHAR(02), -- Pension IMSS
       v_l73_p_p_p                  CHAR(02), -- Plan Privado de Pension
       v_l73_f_defun                CHAR(02), -- Tiene fecha de Defuncion
       v_l73_f_emision              CHAR(02), -- Fecha de Emision de Resolucion Posterior al 12/01/2012
       v_l73_tiene_demanda          CHAR(02), -- Tiene Demanda
       v_l73_tramite_desistimiento  CHAR(02), -- Esta en Tramite o Desistimiento
       v_l73_laudo_firme            CHAR(02), -- Tiene Laudo o esta en Firme
       v_l73_a_favor                CHAR(02), -- A Favor
       v_l73_en_contra              CHAR(02), -- En Contra
       v_fa_titular                 CHAR(02), -- Es titular Fondo de Ahorro  
       v_fa_pensionado              CHAR(02), -- Es Pensionado Fondo de Ahorro
       v_fa_imss                    CHAR(02), -- Pension IMSS Fondo de Ahorro
       v_fa_p_p_p                   CHAR(02), -- Plan Privado de Pension Fondo de Ahorro
       v_fa_f_defun                 CHAR(02), -- Tiene fecha de Defuncion Fondo de Ahorro
       v_grupo                      SMALLINT, -- Grupo para Ley 73
       v_causal                     SMALLINT, -- Causal para Fondo de Ahorro
       v_sql                        STRING    -- cadena con una instruccion SQL


   -- se crea la respuesta del servicio
   LET ws_ret_cons_perfilamiento_out.nss = ws_ret_cons_perfilamiento_in.nss
   LET ws_ret_cons_perfilamiento_out.rfc = ws_ret_cons_perfilamiento_in.rfc

   -- se obtiene los datos recibidos para realizar la consulta
   LET v_nss                        = ws_ret_cons_perfilamiento_in.nss
   LET v_rfc                        = ws_ret_cons_perfilamiento_in.rfc
   LET v_l73_titular                = ws_ret_cons_perfilamiento_in.l73_titular
   LET v_l73_pensionado             = ws_ret_cons_perfilamiento_in.l73_pensionado
   LET v_l73_imss                   = ws_ret_cons_perfilamiento_in.l73_imss
   LET v_l73_p_p_p                  = ws_ret_cons_perfilamiento_in.l73_p_p_p
   LET v_l73_f_defun                = ws_ret_cons_perfilamiento_in.l73_f_defun
   LET v_l73_f_emision              = ws_ret_cons_perfilamiento_in.l73_f_emision
   LET v_l73_tiene_demanda          = ws_ret_cons_perfilamiento_in.l73_tiene_demanda
   LET v_l73_tramite_desistimiento  = ws_ret_cons_perfilamiento_in.l73_tramite_desistimiento
   LET v_l73_laudo_firme            = ws_ret_cons_perfilamiento_in.l73_laudo_firme
   LET v_l73_a_favor                = ws_ret_cons_perfilamiento_in.l73_a_favor
   LET v_l73_en_contra              = ws_ret_cons_perfilamiento_in.l73_en_contra
   LET v_fa_titular                 = ws_ret_cons_perfilamiento_in.fa_titular
   LET v_fa_pensionado              = ws_ret_cons_perfilamiento_in.fa_pensionado
   LET v_fa_imss                    = ws_ret_cons_perfilamiento_in.fa_imss
   LET v_fa_p_p_p                   = ws_ret_cons_perfilamiento_in.fa_p_p_p
   LET v_fa_f_defun                 = ws_ret_cons_perfilamiento_in.fa_f_defun

   LET v_grupo  = 0
   LET v_causal = 0

   
   -- se obtiene el grupo por si la consulta es para ley 73
   IF (v_l73_titular)                = "" AND 
      (v_l73_pensionado)             = "" AND
      (v_l73_imss)                   = "" AND
      (v_l73_p_p_p)                  = "" AND
      (v_l73_f_defun)                = "" AND
      (v_l73_f_emision)              = "" AND
      (v_l73_tiene_demanda)          = "" AND
      (v_l73_tramite_desistimiento)  = "" AND
      (v_l73_laudo_firme)            = "" AND
      (v_l73_a_favor)                = "" AND
      (v_l73_en_contra)              = "" THEN
      LET v_grupo = 0;
   ELSE
       LET v_sql = "\nSELECT grupo_trabajador",
                      "\nFROM   ret_matriz_perfilamiento",
                      "\nWHERE  1 = 1 " 
       IF v_l73_titular IS NULL OR v_l73_titular = " " OR v_l73_titular = "  " THEN 
           LET v_sql = v_sql.trim() || "\n AND titular = '  '"
       ELSE
           LET v_sql = v_sql.trim() || "\n AND titular = '" || v_l73_titular || "'"
       END IF

       IF v_l73_pensionado IS NULL OR v_l73_pensionado = " " OR v_l73_pensionado = "  " THEN 
           LET v_sql = v_sql.trim() || "\n AND pensionado = '  '"
       ELSE
           LET v_sql = v_sql.trim() || "\n AND pensionado = '" || v_l73_pensionado || "'"
       END IF

       IF v_l73_imss IS NULL OR v_l73_imss = " " OR v_l73_imss = "  " THEN 
           LET v_sql = v_sql.trim() || "\n AND pensionado_imss = '  '"
       ELSE
           LET v_sql = v_sql.trim() || "\n AND pensionado_imss = '" || v_l73_imss || "'"
       END IF

       IF v_l73_p_p_p IS NULL OR v_l73_p_p_p = " " OR v_l73_p_p_p = "  " THEN 
           LET v_sql = v_sql.trim() || "\n AND plan_privado = '  '"
       ELSE
           LET v_sql = v_sql.trim() || "\n AND plan_privado = '" || v_l73_p_p_p || "'"
       END IF

       IF v_l73_f_defun IS NULL OR v_l73_f_defun = " " OR v_l73_f_defun = "  " THEN 
           LET v_sql = v_sql.trim() || "\n AND envia_f_defuncion = '  '"
       ELSE
           LET v_sql = v_sql.trim() || "\n AND envia_f_defuncion = '" || v_l73_f_defun || "'"
       END IF
       
       IF v_l73_f_emision IS NULL OR v_l73_f_emision = " " OR v_l73_f_emision = "  " THEN 
           LET v_sql = v_sql.trim() || "\n AND id_f_resol_pension = '  '"
       ELSE
           LET v_sql = v_sql.trim() || "\n AND id_f_resol_pension = '" || v_l73_f_emision || "'"
       END IF

       IF v_l73_tiene_demanda IS NULL OR v_l73_tiene_demanda = " " OR v_l73_tiene_demanda = "  " THEN 
           LET v_sql = v_sql.trim() || "\n AND con_demanda = '  '"
       ELSE
           LET v_sql = v_sql.trim() || "\n AND con_demanda = '" || v_l73_tiene_demanda || "'"
       END IF

       IF v_l73_tramite_desistimiento IS NULL OR v_l73_tramite_desistimiento = " " OR v_l73_tramite_desistimiento = "  " THEN 
           LET v_sql = v_sql.trim() || "\n AND en_tramite_desistimiento = '  '"
       ELSE
           LET v_sql = v_sql.trim() || "\n AND en_tramite_desistimiento = '" || v_l73_tramite_desistimiento || "'"
       END IF
       
       IF v_l73_laudo_firme IS NULL OR v_l73_laudo_firme = " " OR v_l73_laudo_firme = "  " THEN 
           LET v_sql = v_sql.trim() || "\n AND tiene_laudo_o_en_firme = '  '"
       ELSE
           LET v_sql = v_sql.trim() || "\n AND tiene_laudo_o_en_firme = '" || v_l73_laudo_firme || "'"
       END IF
       IF v_l73_a_favor IS NULL OR v_l73_a_favor = " " OR v_l73_a_favor = "  " THEN 
           LET v_sql = v_sql.trim() || "\n AND a_favor = '  '"
       ELSE
           LET v_sql = v_sql.trim() || "\n AND a_favor = '" || v_l73_a_favor || "'"
       END IF

       IF v_l73_en_contra IS NULL OR v_l73_en_contra = " " OR v_l73_en_contra = "  " THEN 
           LET v_sql = v_sql.trim() || "\n AND encontra = '  '"
       ELSE
           LET v_sql = v_sql.trim() || "\n AND encontra = '" || v_l73_en_contra || "'"
       END IF

       PREPARE sid_l73 FROM v_sql
       EXECUTE sid_l73 INTO v_grupo
   END IF

   IF (v_fa_titular)                = "" AND 
      (v_fa_pensionado)             = "" AND
      (v_fa_imss)                   = "" AND
      (v_fa_p_p_p)                  = "" AND
      (v_fa_f_defun)                = "" THEN
      LET v_causal = 0;
   ELSE
       LET v_sql = "\nSELECT causal_retiro",
                      "\nFROM   ret_matriz_perfilamiento",
                      "\nWHERE  1 = 1 " 
       IF v_fa_titular IS NULL OR v_fa_titular = " " OR v_fa_titular = "  " THEN 
           LET v_sql = v_sql.trim() || "\n AND titular = '  '"
       ELSE
           LET v_sql = v_sql.trim() || "\n AND titular = '" || v_fa_titular || "'"
       END IF

       IF v_fa_pensionado IS NULL OR v_fa_pensionado = " " OR v_fa_pensionado = "  " THEN 
           LET v_sql = v_sql.trim() || "\n AND pensionado = '  '"
       ELSE
           LET v_sql = v_sql.trim() || "\n AND pensionado = '" || v_fa_pensionado || "'"
       END IF

       IF v_fa_imss IS NULL OR v_fa_imss = " " OR v_fa_imss = "  " THEN 
           LET v_sql = v_sql.trim() || "\n AND pensionado_imss = '  '"
       ELSE
           LET v_sql = v_sql.trim() || "\n AND pensionado_imss = '" || v_fa_imss || "'"
       END IF

       IF v_fa_p_p_p IS NULL OR v_fa_p_p_p = " " OR v_fa_p_p_p = "  " THEN 
           LET v_sql = v_sql.trim() || "\n AND plan_privado = '  '"
       ELSE
           LET v_sql = v_sql.trim() || "\n AND plan_privado = '" || v_fa_p_p_p || "'"
       END IF

       IF v_fa_f_defun IS NULL OR v_fa_f_defun = " " OR v_fa_f_defun = "  " THEN 
           LET v_sql = v_sql.trim() || "\n AND envia_f_defuncion = '  '"
       ELSE
           LET v_sql = v_sql.trim() || "\n AND envia_f_defuncion = '" || v_fa_f_defun || "'"
       END IF

       LET v_sql = v_sql.trim() || "\n AND id_f_resol_pension = 'NA'"
       LET v_sql = v_sql.trim() || "\n AND con_demanda = 'NA'"
       LET v_sql = v_sql.trim() || "\n AND en_tramite_desistimiento = 'NA'"
       LET v_sql = v_sql.trim() || "\n AND tiene_laudo_o_en_firme = 'NA'"
       LET v_sql = v_sql.trim() || "\n AND a_favor = 'NA'"
       LET v_sql = v_sql.trim() || "\n AND encontra = 'NA'"

       PREPARE sid_fa FROM v_sql
       EXECUTE sid_fa INTO v_causal
   END IF
   LET ws_ret_cons_perfilamiento_out.grupo = v_grupo
   LET ws_ret_cons_perfilamiento_out.causal = v_causal

         
            
    DISPLAY "NSS Regreso: ", v_nss
    DISPLAY "RFC Regreso: ", v_rfc
    DISPLAY "Grupo Regreso: ", v_grupo
    DISPLAY "Causal Regreso: ", v_causal
          
   
END FUNCTION
