--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETF171                                                                #
#Objetivo     => Programa que permite realizar la validacion manual de solicitudes de   #
#                retiro PMG para rechazar casos especificos                             #
#Fecha inicio => Julio 31, 2013                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_usuario      LIKE seg_usuario.usuario_cod -- clave del usuario
END GLOBALS

MAIN
DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion     SMALLINT, -- forma como ejecutara el programa
       p_s_titulo           STRING, -- titulo de la ventana
       v_folio              LIKE glo_folio.folio,
       v_cadena             STRING, -- cadena de texto
       v_cbx_diagnostico    ui.ComboBox, -- combo DE DIAGNOSTICO
       v_cod_rechazo        LIKE ret_disposicion.cod_rechazo, -- codigo de rechazo
       v_nss                LIKE afi_derechohabiente.nss, -- NSS buscado
       v_nombre             STRING, -- nombre completo del trabajador
       v_nombre_afore       LIKE ret_disposicion.nombre_afore, -- nombre registrado en la solicitud
       v_paterno_afore      LIKE ret_disposicion.paterno_afore, -- apellido paterno en la solicitud
       v_materno_afore      LIKE ret_disposicion.materno_afore, -- apellido materno en la solicitud
       v_id_solicitud       LIKE ret_disposicion.id_solicitud, -- solicitud encontrada
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente, -- ID Derechoabiente
       v_proceso_desc       STRING, -- descripcion del proceso
       v_opera_desc         STRING, -- descripcion de la operacion
       v_sql                STRING, -- cadena con enunciado SQL
       v_resultado          SMALLINT -- resultado de ejecutar el SQL

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   LET g_usuario = p_usuario_cod
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = g_proceso_cod_ret_disposicion_pmg -- retiro PMG
   LET g_opera_cod   = g_opera_cod_ret_pmg_preliquidacion -- preliquidacion

   -- se obtiene el pid y el folio del proceso que este ejecutando
   SELECT a.pid,
          a.folio
   INTO   g_pid,
          v_folio
   FROM  bat_ctr_proceso a,
         bat_ctr_operacion b
   WHERE a.pid = b.pid 
   AND   a.proceso_cod = g_proceso_cod -- disposicion
   AND   b.opera_cod   = g_opera_cod_ret_pmg_preliquidacion -- preliquidacion
   AND   b.estado_cod  = 1 -- listo para ejecutar
   AND   a.estado_cod  = 2 -- proceso ejecutando

   -- si no se encontro, entonces no hay proceso corriendo de disposicion
   IF ( g_pid IS NULL ) THEN
      LET v_cadena = "No existe un proceso de Retiros PMG ejecutándose en este momento,\n",
                     "o el archivo procesado ya ha sido preliquidado."
                     
      CALL fn_mensaje("Atención", v_cadena ,"stop")
      EXIT PROGRAM
   END IF

   -- se abre la ventana para validacion manual
   OPEN WINDOW w_valida_manual WITH FORM "RETF1711"
  
   -- se obtienen las descripciones del proceso y la operacion
   CALL fn_proceso_cod_desc(g_proceso_cod) RETURNING v_proceso_desc
   CALL fn_opera_cod_desc(g_proceso_cod, g_opera_cod) RETURNING v_opera_desc

   -- se despliegan las descripciones
   DISPLAY BY NAME v_proceso_desc, v_folio
   
   -- se captura el nss de la solicitud que se desea rechazar
   INPUT BY NAME v_nss
   WITHOUT DEFAULTS
   ATTRIBUTES (UNBUFFERED)

      ON ACTION ACCEPT
         IF ( v_nss IS NULL ) THEN
            CALL fn_mensaje("Atención","Es necesario capturar un NSS","stop")
            CONTINUE INPUT
         END IF

         -- se busca el nss y su solicitud para el folio dados
         SELECT b.id_solicitud      ,
                b.id_derechohabiente,
                b.nombre_afore      ,
                b.paterno_afore     ,
                b.materno_afore
         INTO   v_id_solicitud      ,
                v_id_derechohabiente,
                v_nombre_afore      ,
                v_paterno_afore     ,
                v_materno_afore
         FROM   ret_disposicion b,
                afi_derechohabiente a
         WHERE  b.folio              = v_folio
         AND    b.id_derechohabiente = a.id_derechohabiente
         AND    a.nss                = v_nss
         AND    b.cod_rechazo        = 0 -- solo aceptadas
         
         -- Si no se encontro solicitud
         IF ( v_id_solicitud IS NULL ) THEN
            CALL fn_mensaje("Atención","No se encontró una solicitud con el NSS dado","stop")
            CONTINUE INPUT
         END IF  
         
         -- se conforma el nombre
         LET v_nombre = v_nombre_afore CLIPPED, " ", v_paterno_afore CLIPPED, " ", v_materno_afore CLIPPED
         
         -- se despliegan los datos
         DISPLAY BY NAME v_nombre
         CALL ui.interface.refresh()
         
         -- se obtiene el cambio de estatus
         LET v_cod_rechazo = NULL
         
         INPUT BY NAME v_cod_rechazo
         WITHOUT DEFAULTS
         ATTRIBUTES ( UNBUFFERED )
         
            ON ACTION ACCEPT 
               -- al aceptar el cambio
               IF ( v_cod_rechazo IS NULL ) THEN
                  CALL fn_mensaje("Atención", "Es necesario elegir un diagnóstico","stop")
                  CONTINUE INPUT
               END IF
               
               -- se confirma con el usuario que cambiara el estado de la solicitud
               LET v_cadena = "Por favor confirme el rechazo de la solicitud de:",
                              "\n", v_nombre,
                              "\n\n",
                              "Al confirmar el rechazo, esta solicitud no será considerada",
                              "\nen el proceso de liquidación y por tanto no será pagada."
               
               MENU "Confirmar diagnóstico"
               ATTRIBUTES ( STYLE = "dialog",COMMENT = v_cadena, IMAGE = "question" )
                  COMMAND "Confirmar"
                     -- se rechaza la solicitud
                     UPDATE ret_disposicion
                     SET    estado_solicitud = 100,
                            cod_rechazo  = v_cod_rechazo
                     WHERE  folio        = v_folio
                     AND    id_solicitud = v_id_solicitud

                     -- se desmarca la solicitud
                     LET v_sql = "\nEXECUTE FUNCTION fn_desmarca_cuenta(",
                                 "\n",v_id_derechohabiente, ",",
                                 "\n 808,", -- marca PMG
                                 "\n",v_id_solicitud,",", -- identificador de registro de archivo o lote
                                 "\n 40,", -- estado marca / rechazo validacion
                                 "\n 808,", -- marca de la causa / rechazo por validacion
                                 "\n'",p_usuario_cod CLIPPED, "',",
                                 "\n",g_proceso_cod, ")"

                     -- se prepara y ejecuta la desmarcar
                     PREPARE sid_desmarca FROM v_sql
                     EXECUTE sid_desmarca INTO v_resultado
                     
                     CALL fn_mensaje("Atención", "La solicitud ha sido rechazada","information")
                     EXIT MENU
               
                  COMMAND "Cancelar"
                     EXIT MENU
               END MENU
               EXIT INPUT
               
            ON ACTION CANCEL
               EXIT INPUT
         
         END INPUT
         EXIT INPUT
        
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_valida_manual
   
END MAIN
