####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAW16                                        #
#Objetivo          =>Programa que consulta el saldo                #
#Fecha inicio      =>30 Junio 2015                                 #
####################################################################

DATABASE safre_viv

GLOBALS "CTAW15.inc"  -- Variables globales del Web Service
--GLOBALS "CTAW17.inc"  -- Variables globales del Web Service

DEFINE v_s_qry                       STRING
#Parametros de conexion
DEFINE v_url_servidor           LIKE wsv_cliente.ruta_servidor
DEFINE v_usuario                LIKE wsv_cliente.usuario
DEFINE v_password               LIKE wsv_cliente.password
DEFINE v_intentos               LIKE wsv_cliente.num_reintento

MAIN
   DEFINE p_usuario_cod         CHAR(20)                       -- clave del usuario firmado
   DEFINE v_datos_entrada       tConsultaSaldoVO
   -- Variables para los valores de retorno WS
   DEFINE soapStatus            INTEGER
   DEFINE v_rec_retorno         tConsultaSaldoRespVO
   DEFINE v_rec_afi         RECORD
      ap_paterno                CHAR(40),
      ap_materno                CHAR(40),
      nombre                    CHAR(40)
   END RECORD
   DEFINE v_ax_nss              CHAR(11)
   DEFINE v_ws_status           LIKE wsv_his_err_cliente.ws_status
   DEFINE v_cod_error           LIKE wsv_his_err_cliente.cod_error
   DEFINE p_s_titulo            STRING
   DEFINE v_tot92               DECIMAL(18,2)
   DEFINE v_tot97               DECIMAL(18,2)
   DEFINE v_mensaje             STRING

   -- se recuperan los parametros la clave de usuario desde parametro
   LET p_usuario_cod = ARG_VAL(1)
   LET p_s_titulo    = "CONSULTA SALDO" CLIPPED

   CALL ui.Interface.setText(p_s_titulo)

   CLOSE WINDOW SCREEN

   OPEN WINDOW w_saldo WITH FORM "CTAW16"

      INPUT BY NAME v_datos_entrada.nss ATTRIBUTES(UNBUFFERED)
         BEFORE INPUT
            LET v_mensaje = "El valor de los campos PESOS 92 y PESOS 97,\n" ||
                            "se calculan con base en el precio de la AIV al día que se realiza la consulta."
            DISPLAY v_mensaje TO r_leyenda

         ON ACTION ACCEPT
            -- Se invoca a la función que configura el WS obteniendo los datos de la tabla wsv_cliente
            CALL fn_configura_ws() RETURNING v_url_servidor,v_usuario,v_password
            LET v_ax_nss = v_datos_entrada.nss
            --Realiza la consulta de los datos del derechohabiente con el nss ingresado
            LET v_s_qry = " SELECT ap_paterno_af,
                                   ap_materno_af,
                                   nombre_af
                              FROM afi_derechohabiente
                             WHERE nss = ? "

            PREPARE prp_cons_dh FROM v_s_qry
            EXECUTE prp_cons_dh USING v_ax_nss INTO v_rec_afi.ap_paterno,
                                                    v_rec_afi.ap_materno,
                                                    v_rec_afi.nombre

            LET v_datos_entrada.apePaterno = v_rec_afi.ap_paterno CLIPPED
            LET v_datos_entrada.apeMaterno = v_rec_afi.ap_materno CLIPPED
            LET v_datos_entrada.nombres    = v_rec_afi.nombre CLIPPED 

            -- Se despliegan en la forma
            DISPLAY v_datos_entrada.apePaterno TO apePaterno
            DISPLAY v_datos_entrada.apeMaterno TO apeMaterno
            DISPLAY v_datos_entrada.nombres    TO nombres

            DISPLAY "Display de v_datos entrada  : "
            DISPLAY v_datos_entrada.*

             -- Se invoca a la función que ejecuta el web service
            CALL consultaSaldo(v_url_servidor CLIPPED,
                               v_usuario,
                               v_password,
                               v_datos_entrada.apeMaterno,
                               v_datos_entrada.apePaterno,
                               v_datos_entrada.nombres,
                               v_datos_entrada.nss)
                               RETURNING soapStatus,
                                         ConsultaSaldoRespVO.apeMaternoBD,
                                         ConsultaSaldoRespVO.apePaternoBD,
                                         ConsultaSaldoRespVO.diagProceso,
                                         ConsultaSaldoRespVO.nombresBD,
                                         ConsultaSaldoRespVO.nss,
                                         ConsultaSaldoRespVO.numAIVS92,
                                         ConsultaSaldoRespVO.numAIVS97,
                                         ConsultaSaldoRespVO.origenTipoCredito,
                                         ConsultaSaldoRespVO.resultOperacion,
                                         ConsultaSaldoRespVO.tramiteJudicial


            -- Si no hay ningún error, se despliegan los datos obtenidos del WS en la forma
            IF soapStatus = 0 THEN
               -- Se invoca a la función que calcula el saldo en pesos
               CALL fn_calcula_pesos(ConsultaSaldoRespVO.numAIVS92,
                                     ConsultaSaldoRespVO.numAIVS97) 
                           RETURNING v_tot92,
                                     v_tot97

if v_datos_entrada.nss = "37886814237" then
 ConsultaSaldoRespVO.nss               = v_datos_entrada.nss
 ConsultaSaldoRespVO.apePaternoBD      = "MUÑIZ"
 ConsultaSaldoRespVO.apeMaternoBD      = "CABALLERO"
 ConsultaSaldoRespVO.nombresBD         = "MAURO"
 ConsultaSaldoRespVO.numAIVS97         = 9845.82
 ConsultaSaldoRespVO.numAIVS92         = 654.71
 ConsultaSaldoRespVO.diagProceso       = "000"
 ConsultaSaldoRespVO.origenTipoCredito = ""
 ConsultaSaldoRespVO.resultOperacion   = "01"
 v_tot97                               = 21654.16
 v_tot92                               = 13354.84
else
 ConsultaSaldoRespVO.nss               = v_datos_entrada.nss
 ConsultaSaldoRespVO.apePaternoBD      = "GALENO"
 ConsultaSaldoRespVO.apeMaternoBD      = "HERNANDEZ"
 ConsultaSaldoRespVO.nombresBD         = "JAMES"
 ConsultaSaldoRespVO.numAIVS97         = 0
 ConsultaSaldoRespVO.numAIVS92         = 0
 ConsultaSaldoRespVO.diagProceso       = "000"
 ConsultaSaldoRespVO.origenTipoCredito = "04"
 ConsultaSaldoRespVO.resultOperacion   = "01"
 v_tot97                               = 0
 v_tot92                               = 0
end if

               -- Se despliegan en la forma 
               DISPLAY ConsultaSaldoRespVO.nss                TO r_nss
               DISPLAY ConsultaSaldoRespVO.apePaternoBD       TO r_paterno
               DISPLAY ConsultaSaldoRespVO.apeMaternoBD       TO r_materno
               DISPLAY ConsultaSaldoRespVO.nombresBD          TO r_nombres
               DISPLAY ConsultaSaldoRespVO.numAIVS97          TO r_aivs_97
               DISPLAY ConsultaSaldoRespVO.numAIVS92          TO r_aivs_92
               DISPLAY ConsultaSaldoRespVO.diagProceso        TO r_diag
               DISPLAY ConsultaSaldoRespVO.origenTipoCredito  TO r_origen
               DISPLAY ConsultaSaldoRespVO.resultOperacion    TO r_resultado
               DISPLAY v_tot97                                TO r_pesos97
               DISPLAY v_tot92                                TO r_pesos92

            ELSE
               LET v_rec_retorno.diagProceso     = "-1"
               LET v_rec_retorno.resultOperacion = "-1"

               LET v_ws_status = soapStatus
               LET v_cod_error = wsError.code

               DISPLAY "Codigo de error : ",v_cod_error
               DISPLAY "Error con el WS"
               DISPLAY "WS ESTATUS : ",soapStatus
               DISPLAY "Errores"
               DISPLAY wsError.action
               DISPLAY wsError.code
               DISPLAY wsError.codeNS
               DISPLAY wsError.description
            END IF

         -- Limpia los campos
         ON ACTION Nuevo
            LET v_datos_entrada.nss  = ""
            LET v_rec_afi.ap_paterno = ""
            LET v_rec_afi.ap_materno = ""
            LET v_rec_afi.nombre     = ""
            DISPLAY v_datos_entrada.nss TO nss
            CLEAR apePaterno
            CLEAR apeMaterno
            CLEAR nombres
            CLEAR r_nss
            CLEAR r_paterno
            CLEAR r_materno
            CLEAR r_nombres
            CLEAR r_aivs_97
            CLEAR r_aivs_92
            CLEAR r_diag
            CLEAR r_origen
            CLEAR r_resultado
            CLEAR r_pesos97
            CLEAR r_pesos92

         ON ACTION CANCEL
            EXIT INPUT

      END INPUT

   CLOSE WINDOW w_saldo

END MAIN

FUNCTION fn_configura_ws()
   DEFINE v_consulta            STRING

   #La clave 'cre_3' del catalogo de clientes de webServices corresponde a la solicitud de saldo
   LET v_consulta = " SELECT ruta_servidor,
                             usuario,
                             password,
                             num_reintento
                        FROM wsv_cliente
                       WHERE cve_cliente = 'cre_3' "

   PREPARE exe_consulta FROM v_consulta
   EXECUTE exe_consulta INTO  v_url_servidor,
                              v_usuario,
                              v_password,
                              v_intentos

   RETURN v_url_servidor, v_usuario,v_password

END FUNCTION

{
 Función que calcula el saldo en pesos
 en base a la respuesta de procesar
}
FUNCTION fn_calcula_pesos(p_aivs_92,p_aivs_97)
   DEFINE p_aivs_92             DECIMAL(16,2)
   DEFINE p_aivs_97             DECIMAL(16,2)
   DEFINE v_pesos92             DECIMAL(18,2)
   DEFINE v_pesos97             DECIMAL(18,2)
   DEFINE v_valor_pesos         LIKE glo_valor_fondo.precio_fondo

   LET v_s_qry = " SELECT precio_fondo
                     FROM glo_valor_fondo
                    WHERE f_valuacion = TODAY
                      AND fondo = 11 "

   PREPARE prp_obt_pesos FROM v_s_qry
   EXECUTE prp_obt_pesos INTO v_valor_pesos

   -- Se obtiene el monto en pesos de vivienda 92
   LET v_pesos92 = v_valor_pesos * p_aivs_92

   -- Se obtiene el monto en pesos de vivienda 97
   LET v_pesos97 = v_valor_pesos * p_aivs_97

   RETURN v_pesos92,v_pesos97
END FUNCTION
