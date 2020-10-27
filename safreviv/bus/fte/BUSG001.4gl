####################################################################
#Modulo            =>BUS                                           #
#Programa          =>BUSG001.4gl                                   #
#Objetivo          =>Programa de funciones globales                #
#Fecha inicio      =>25 NOVIEMBRE 2013                             #
####################################################################
DATABASE safre_viv

PRIVATE DEFINE v_id_cat_bus_proceso          DECIMAL(9,0)
PRIVATE DEFINE v_id_cat_bus_operacion        DECIMAL(9,0)
PRIVATE DEFINE v_desc_proceso_bus            VARCHAR(40)
PRIVATE DEFINE v_desc_opera_bus              VARCHAR(40)

PRIVATE DEFINE v_ind_correo                  SMALLINT
   
FUNCTION fn_envia_correo(p_proceso, p_operacion, p_titulo, p_mensaje, p_adjunto)
   DEFINE p_proceso                    VARCHAR(3)
   DEFINE p_operacion                  VARCHAR(4)
   DEFINE p_titulo                     STRING
   DEFINE p_mensaje                    STRING
   DEFINE p_adjunto                    STRING

   DEFINE v_comando                    STRING
   DEFINE v_cat_adjunto                STRING
   DEFINE v_cat_mail                   STRING
   DEFINE v_lista_correos              STRING

   #Se busca el proceso y la operacion
   SELECT
      proc.id_cat_bus_proceso,
      proc.desc_proceso_bus,
      opr.id_cat_bus_operacion,
      opr.desc_opera_bus
   INTO
      v_id_cat_bus_proceso,
      v_desc_proceso_bus,
      v_id_cat_bus_operacion,
      v_desc_opera_bus
   FROM cat_bus_proceso proc
   INNER JOIN cat_bus_operacion opr ON opr.id_cat_bus_proceso = proc.id_cat_bus_proceso
   WHERE proc.cod_proceso_bus = p_proceso
   AND opr.cod_opera_bus = p_operacion

   IF p_titulo IS NULL THEN
      LET p_titulo = "[SAFRE-BUS] Notificación de evento"
   END IF

   #Se establecen los archivos de paso para enviar el mensaje
   LET v_cat_adjunto = p_mensaje, ".cat" 

   LET v_cat_mail = p_mensaje, ".mail"

   IF ( p_adjunto IS NOT NULL ) THEN
       LET v_comando = "uuencode ",p_adjunto.trim(),"  ",p_adjunto.trim(),
                       " > ",v_cat_adjunto, " ; cat ", p_mensaje ," ",
                       v_cat_adjunto, " > ",v_cat_mail, " ; "
    ELSE
       LET v_comando = " cat ", p_mensaje , " > ",v_cat_mail, " ; "
    END IF

    LET v_comando = v_comando, " mailx -s '",p_titulo.trim(),"'"

    #Obtenemos la lista de destinatarios
    LET v_ind_correo = 0
    CALL fn_ontener_correos() RETURNING v_lista_correos
    LET v_comando = v_comando ," ", v_lista_correos CLIPPED

    LET v_comando = v_comando,  " < ", v_cat_mail
    IF v_ind_correo  = 1 THEN    
       RUN v_comando
    ELSE       
       DISPLAY "SIN CORREO REGISTRADO"    
    END IF

    LET v_comando = "rm ", p_mensaje
    RUN v_comando

    LET v_comando = "rm ", v_cat_mail
    RUN v_comando
END FUNCTION

PRIVATE FUNCTION fn_ontener_correos()
   DEFINE v_consulta_correo      STRING
   DEFINE v_usuario_cod          VARCHAR(20)
   DEFINE v_correo               VARCHAR(80)
   DEFINE v_lista_correos        STRING

   #LET v_consulta_correo = "SELECT ",
   #                        "glo.usuario_cod, ",
   #                        "usr.correo_e ",
   #                        "FROM glo_usuario_correo glo ",
   #                        "INNER JOIN seg_usuario usr ON usr.usuario_cod = glo.usuario_cod ",
   #                        "WHERE glo.id_cat_bus_operacion = ? ",
   #                        "AND usr.ind_activo = 1 ",
   #                        "AND glo.ind_envio = 1"
   #PREPARE exe_consulta_correo FROM v_consulta_correo
   #DECLARE cur_consulta_correo CURSOR FOR exe_consulta_correo
   #FOREACH cur_consulta_correo USING v_id_cat_bus_operacion INTO v_usuario_cod, v_correo
   #   IF v_usuario_cod IS NOT NULL AND v_correo IS NOT NULL THEN
   #      LET v_ind_correo = 1
   #      LET v_lista_correos = v_lista_correos CLIPPED, " ",  v_correo CLIPPED
   #   END IF
   #END FOREACH

   LET v_lista_correos = " "
  RETURN v_lista_correos
END FUNCTION