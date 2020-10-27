--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETF30                                                                 #
#Objetivo     => Programa para capturar los montos notificados de retiros por           #
#                transferencia                                                          #
#Fecha inicio => Marzo 07, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo   RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD

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
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = g_proceso_cod_ret_transferencia -- retiro por transferencia

   -- se invoca la captura de saldos para devolucion por errores de operacion
   CALL fn_captura_saldos_notificados_ret_transferencia(p_usuario_cod)

END MAIN

{ ======================================================================
Clave: 
Nombre: fn_captura_saldos_notificados_ret_transferencia
Fecha creacion: Marzo 07, 2012, 2012
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Presentan en pantalla un formulario que permite al usuario capturar una fecha
de operacion procesar y un monto notificado de deposito para retiros
por transferencia

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_captura_saldos_notificados_ret_transferencia(p_usuario_cod)
DEFINE p_usuario_cod                  LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       v_b_continuar                  SMALLINT, -- booleana para continuar ejecucion
       v_cbx_folio                    ui.ComboBox, -- combo con los folios
       v_si_indice                    SMALLINT, -- indice de arreglo
       v_folio_aux                    LIKE glo_folio.folio, -- folio para cargar combo
       v_continuar                    SMALLINT, -- booleana para continuar la captura
       r_ret_mto_notificado           RECORD -- registro de monto notificado
         folio                          decimal(9,0),
	     f_operacion_procesar           DATE  ,
         total_importe                  decimal(18,6)  ,
	     f_captura                      date  ,
	     h_captura                      datetime hour to second  ,
	     usuario                        char(20)  ,
	     estado_captura                 SMALLINT  
       END RECORD,       
       v_total_importe_archivo        DECIMAL(18,6), -- importe que viene en archivo
       v_f_operacion_procesar_archivo LIKE ret_cza_disposicion.f_operacion_procesar,
       v_s_sql                        STRING,
       p_titulo                       STRING, -- titulo del mensaje enviado en el correo
       p_mensaje                      STRING,  -- cuerpo del mensaje enviado
       v_diferencia                   DECIMAL(22,6)

   -- se asume que se continuara el proceso
   LET v_b_continuar = TRUE

   -- se abre la ventana de captura
   OPEN WINDOW w_captura_montos WITH FORM "RETF301"

   -- se prepara y ejecuta la consulta
   LET v_s_sql =            "\nSELECT folio"
   LET v_s_sql = v_s_sql || "\nFROM glo_folio"
   LET v_s_sql = v_s_sql || "\nWHERE"
   LET v_s_sql = v_s_sql || "\n  proceso_cod = " || g_proceso_cod
   LET v_s_sql = v_s_sql || "\nAND"
   LET v_s_sql = v_s_sql || "\n   STATUS = 0"
   LET v_s_sql = v_s_sql || "\nORDER BY"
   LET v_s_sql = v_s_sql || "\n folio DESC"

   PREPARE sid_folios FROM v_s_sql
   DECLARE cur_folios CURSOR FOR sid_folios
      
   -- se le asigna el apuntado der combo a la variable
   LET v_cbx_folio = ui.ComboBox.forName("formonly.folio")
   
   -- se inicia el combobox en blanco
   CALL v_cbx_folio.clear()
      
   -- se llena el combo con los datos del catalogo de afores
   FOREACH cur_folios INTO v_folio_aux
      -- se agrega la afore al combo
      CALL v_cbx_folio.addItem(v_folio_aux, v_folio_aux)
   END FOREACH
   
   -- se libera el cursor
   FREE cur_folios
   
   -- se inicia un DIALOG para capturar los datos y presentar en pantalla
   -- el indice del arreglo es 1
   LET v_si_indice = 1

   -- se asume que la captura no se cancela
   LET v_continuar = TRUE
   
     -- input para la captura de los saldos por AFORE
     INPUT BY NAME
       r_ret_mto_notificado.folio,
       r_ret_mto_notificado.f_operacion_procesar,
       r_ret_mto_notificado.total_importe
     WITHOUT DEFAULTS     
     ATTRIBUTES ( UNBUFFERED )
   
        -- antes de comenzar la captura
        BEFORE INPUT
           LET r_ret_mto_notificado.estado_captura       = 0
           LET r_ret_mto_notificado.f_captura            = TODAY
           LET r_ret_mto_notificado.f_operacion_procesar = TODAY
           LET r_ret_mto_notificado.folio                = NULL
           LET r_ret_mto_notificado.h_captura            = CURRENT HOUR TO SECOND
           LET r_ret_mto_notificado.total_importe        = 0
           LET r_ret_mto_notificado.usuario              = p_usuario_cod

        -- se valida la fecha capturada
        ON CHANGE f_operacion_procesar
           -- la fecha no puede ser posterior a la actual
           IF ( r_ret_mto_notificado.f_operacion_procesar > TODAY ) THEN
              CALL fn_mensaje("Atención", "La fecha de operación de PROCESAR no puede ser posterior a la fecha actual","stop")
              NEXT FIELD f_operacion_procesar
           END IF

        -- se valida el importe capturado
        ON CHANGE total_importe
           -- la fecha no puede ser posterior a la actual
           IF ( r_ret_mto_notificado.total_importe <= 0 ) THEN
              CALL fn_mensaje("Atención", "El importe total debe ser mayor a cero","stop")
              NEXT FIELD total_importe
           END IF

        AFTER FIELD total_importe
           NEXT FIELD folio 
           
        ON ACTION ACCEPT
           -- se verifica que se haya elegido folio
           IF ( r_ret_mto_notificado.folio IS NULL ) THEN
              CALL fn_mensaje("Atención", "Debe elegir un folio","stop")
              NEXT FIELD folio
           END IF
        
           -- la fecha no puede ser posterior a la actual
           IF ( r_ret_mto_notificado.f_operacion_procesar > TODAY ) THEN
              CALL fn_mensaje("Atención", "La fecha de operación de PROCESAR no puede ser posterior a la fecha actual","stop")
              NEXT FIELD f_operacion_procesar
           END IF

           -- la fecha no puede ser posterior a la actual
           IF ( r_ret_mto_notificado.total_importe <= 0 ) THEN
              CALL fn_mensaje("Atención", "El importe total debe ser mayor a cero","stop")
              NEXT FIELD total_importe
           END IF


           -- se valida que el monto capturado coincida con el monto del archivo
           -- se obtiene el importe total segun el folio
           SELECT SUM(total_importe)
           INTO v_total_importe_archivo
           FROM
              ret_cza_transferencia
           WHERE
              folio = r_ret_mto_notificado.folio

           IF ( v_total_importe_archivo IS NULL ) THEN
              LET v_total_importe_archivo = 0 
           END IF

           -- se obtiene la diferncia entre monto capturado y notificado
           LET v_diferencia = v_total_importe_archivo - r_ret_mto_notificado.total_importe

           -- si la diferencia fue negativa, la pasamos a positiva
           IF ( v_diferencia < 0 ) THEN
              LET v_diferencia = v_diferencia * (-1)
              DISPLAY " Diferencia: ", v_diferencia
           END IF

           -- se verifica si la diferencia entre los montos es mayor que la tolerancia aceptada
           IF ( v_diferencia > g_tolerancia_max_transferencia ) THEN           
              CALL fn_mensaje("Atención", "El monto capturado no coincide con el monto recibido en archivo","stop")
              CONTINUE INPUT
           END IF

           -- se obtiene la fecha de operacion de procesar del archivo
           SELECT f_operacion_procesar
           INTO
              v_f_operacion_procesar_archivo
           FROM
              ret_cza_transferencia
           WHERE
              folio = r_ret_mto_notificado.folio
              
           -- si las fechas no coinciden
           IF ( v_f_operacion_procesar_archivo <> r_ret_mto_notificado.f_operacion_procesar ) THEN
              CALL fn_mensaje("Atención", "La fecha de operación PROCESAR capturada\nno coincide con la recibida en archivo","stop")
              CONTINUE INPUT
           END IF
        
           WHENEVER SQLERROR CONTINUE
              
           -- se inserta el monto capturado
           INSERT INTO ret_mto_notificado VALUES (r_ret_mto_notificado.*)

           IF ( SQLCA.sqlcode = -239 ) THEN
              CALL fn_mensaje("Atención","Ya se ha capturado un monto para ese folio con anterioridad","stop")
              CONTINUE INPUT
           END IF
                 
           CALL fn_mensaje("Atención","El monto capturado ha sido almacenado con éxito","information")
           -- se termina la captura
           LET v_continuar = FALSE

           WHENEVER SQLERROR STOP
                 
           IF ( NOT v_continuar ) THEN
              EXIT INPUT
           END IF

        ON ACTION CANCEL
           EXIT INPUT

        ON ACTION notifica_error

           LET p_mensaje = "Se enviará un reporte de inconsistencia entre los datos encontrados en archivo\n",
                           "y los montos notificados por correo.\n",
                           "Esta operación causará el reverso de las operaciones ya ejecutadas y será necesario\n",
                           "solicitar el reenvío del archivo a PROCESAR.\n\n¿Desea continuar?"
        
           MENU "Error de montos notificados"
           ATTRIBUTES ( STYLE="dialog", COMMENT = p_mensaje )
              COMMAND "Aceptar"


                 -- se valida que el monto capturado coincida con el monto del archivo
                 -- se obtiene el importe total segun el folio
                 SELECT SUM(total_importe)
                 INTO v_total_importe_archivo
                 FROM
                    ret_cza_transferencia
                 WHERE
                    folio = r_ret_mto_notificado.folio
                 
                 IF ( v_total_importe_archivo IS NULL ) THEN
                    LET v_total_importe_archivo = 0 
                 END IF
                 
                 -- se obtiene la fecha de operacion de procesar del archivo
                 SELECT f_operacion_procesar
                 INTO
                    v_f_operacion_procesar_archivo
                 FROM
                    ret_cza_transferencia
                 WHERE
                    folio = r_ret_mto_notificado.folio
                 
                    
                 WHENEVER SQLERROR CONTINUE
                 
                 -- se cambia el estado de la captura a error
                 LET r_ret_mto_notificado.estado_captura = 110 -- INCONSISTENCIA MONTO ARCHIVO V/S MONTO CAPTURADO
                 
                 -- se inserta el monto capturado
                 INSERT INTO ret_mto_notificado VALUES (r_ret_mto_notificado.*)
                 
                 IF ( SQLCA.sqlcode = -239 ) THEN
                    CALL fn_mensaje("Atención","Ya se ha notificado el error con anterioridad","stop")
                    CONTINUE INPUT
                 END IF
                 
                 -- se envia la ejecucion de los reversos
                 CALL fn_reverso_inconsistencia_montos(r_ret_mto_notificado.folio, p_usuario_cod)
                    
                 -- se construye el cuerpo del mensaje
                 LET p_mensaje = "Informe de inconsistencia entre montos notificados vs montos encontrados en archivo.\n\n",
                                 "Se le informa que los montos encontrados en el archivo NOMBRE_DE_ARCHIVO_AQUI no coinciden\n",
                                 "con los notificados por correo electronico, siendo estos los siguientes:\n\n",
                                 "---- INFORMACION NOTIFICADA ----\n",
                                 "Fecha Procesar  : ", r_ret_mto_notificado.f_operacion_procesar, "\n",
                                 "Monto notificado: ", r_ret_mto_notificado.total_importe, "\n\n",
                                 "---- INFORMACION EN ARCHIVO ----\n",
                                 "Fecha Procesar        : ", v_f_operacion_procesar_archivo, "\n",
                                 "Monto total de archivo: ", v_total_importe_archivo, "\n\n",
                                 "Por tal motivo se considera erróneo el archivo.\n"
                                 
                 DISPLAY p_mensaje
                      
                 -- se crea el titulo del mensaje
                 LET p_titulo = "Informe de inconsistencia en montos notificados RETIROS POR TRANSFERENCIA"
                 
                 -- se obtiene el pid
                 -- se obtiene el PID del proceso
                 SELECT MAX(pid)
                 INTO g_pid
                 FROM
                  bat_ctr_proceso
                 WHERE
                  proceso_cod = g_proceso_cod
                 
                 -- se invoca el envio de correo electronico de notificacion
                 CALL fn_correo_proceso(g_pid, g_proceso_cod_ret_transferencia, g_opera_cod_ret_transf_integracion,
                                        "/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                                        p_titulo,
                                        p_mensaje)
                 
                    
                 CALL fn_mensaje("Atención",
                                 "Se ha enviado la notificación de error en montos notificados por correo\n\n",
                                 "information")

                 -- se termina la captura
                 LET v_continuar = FALSE
                 
                 WHENEVER SQLERROR STOP
                 EXIT MENU

              COMMAND "Cancelar"
                 EXIT MENU

           END MENU
        EXIT INPUT
           
     END INPUT
 
   -- se cierra la ventana
   CLOSE WINDOW w_captura_montos

END FUNCTION

{ ======================================================================
Clave: 
Nombre: fn_reverso_inconsistencia_montos
Fecha creacion: Febrero 23, 2012
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Presentan en pantalla un formulario que permite al usuario capturar una fecha
de operacion procesar y un monto notificado de deposito para retiros
por transferencia

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_reverso_inconsistencia_montos(p_folio, p_usuario_cod)
DEFINE  p_usuario_cod        LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_folio              LIKE glo_folio.folio
       ,v_nombre_archivo     LIKE glo_ctr_archivo.nombre_archivo
       ,v_s_comando          STRING -- cadena con comando para ser ejecutado
  
   -- se asigna proceso y operacion
   LET g_proceso_cod = g_proceso_cod_ret_transferencia -- retiros por transferencia
   LET g_opera_cod   = g_opera_cod_ret_transf_integracion -- integracion

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO g_pid
   FROM
    bat_ctr_proceso
   WHERE
    proceso_cod = g_proceso_cod

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'ret'

    SELECT b.ruta_listados
      INTO seg_modulo_bat.ruta_listados
      FROM seg_modulo b
     WHERE b.modulo_cod = 'bat'

   -- se obtiene el nombre del archivo
   SELECT nombre_archivo
   INTO   v_nombre_archivo
   FROM   glo_ctr_archivo
   WHERE  folio = p_folio

   -- se verifica si se puede continuar con la operacion
   --IF ( fn_valida_reverso(g_pid,g_proceso_cod,g_opera_cod) = 0 ) THEN
      -- se construye la cadena de ejecucion del programa lanzado de reverso
      LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETR45 ",
                          p_usuario_cod CLIPPED, " ",
                          g_pid  , " " ,
                          g_proceso_cod , " " ,
                          g_opera_cod ," ",
                          p_folio ," ",
                          v_nombre_archivo CLIPPED," ",
                          " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                          "/nohup:",g_pid        USING "&&&&&",":",
                          g_proceso_cod USING "&&&&&",":",
                          g_opera_cod   USING "&&&&&" ,
                          " 2>&1 &"

                         DISPLAY v_s_comando
                         
       RUN v_s_comando
   --END IF


END FUNCTION
