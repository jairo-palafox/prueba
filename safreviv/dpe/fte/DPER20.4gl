--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 27/05/2015
--===============================================================

################################################################################
#Modulo       => DPE                                                           #
#Programa     => DPER20                                                        #
#Objetivo     => Programa lanzador para reversar la integración del proceso    #
#Fecha inicio => 30/08/2012                                                    #
################################################################################

GLOBALS "DPEG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo       STRING -- titulo de la ventana
       ,p_operacion      SMALLINT
       ,p_d_folio        LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       --
       ,r_bandera        SMALLINT
       ,v_cadena         STRING
       ,v_r_reverso_id_derechohabiente DECIMAL(9,0)
       ,v_r_reverso_id_referencia      DECIMAL(9,0)
       ,v_r_reverso_marca              SMALLINT
       ,v_r_reverso_folio              DECIMAL(9,0)
       ,bn_reverso_desmarca            SMALLINT
       ,p_titulo                       STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje                      STRING -- cuerpo del mensaje enviado

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_tipo_ejecucion = ARG_VAL(7)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DPER20.log")

   LET g_proceso_cod = g_proceso_cod_dpe_credito -- DPE Créditos
   LET g_opera_cod   = g_opera_cod_dpe_integracion  -- preliquidacion
   
   -- Bandera para comprobar que se ejecute correctamente el reverso de la desmarca
   LET bn_reverso_desmarca = 0
   
   -- Prepara el la cadena para ejecutar el procedimiento de reverso desmarca
    LET v_cadena = "EXECUTE PROCEDURE safre_viv:sp_reversa_marca(?,?,?,?)"
    PREPARE prp_reversa_marca FROM v_cadena
    
    -- Recupera los registro que estan liquidados para aplicar el reverso marca
    DECLARE cur_reversa_marca CURSOR FOR
       SELECT id_derechohabiente, 
              marca, 
              n_referencia, 
              folio
         FROM safre_viv:sfr_marca_activa
        WHERE folio = p_d_folio
          AND marca = 402 -- Marca infonavit

   -- Por cada registro liquidado reversa la marca
    FOREACH cur_reversa_marca 
    	  INTO v_r_reverso_id_derechohabiente, v_r_reverso_marca,
             v_r_reverso_id_referencia, v_r_reverso_folio

    	  WHENEVER ERROR CONTINUE

    	  EXECUTE prp_reversa_marca USING v_r_reverso_id_derechohabiente,
    	                                  v_r_reverso_marca, --Marca
    	                                  v_r_reverso_id_referencia,
    	                                  v_r_reverso_folio
    	  
    	  IF SQLCA.SQLCODE < 0 THEN
    	  	LET bn_reverso_desmarca = SQLCA.SQLCODE
    	  END IF
    	  
    	  WHENEVER ERROR STOP
    
    END FOREACH
    
    IF bn_reverso_desmarca < 0 THEN
       DISPLAY "La Reversa marca no puedo concretarse: ", bn_reverso_desmarca
    ELSE
    	  DISPLAY "Reversa marca se aplico correctamente: ", bn_reverso_desmarca
    END IF
   
   DELETE FROM dpe_sol_creditos  WHERE folio = p_d_folio ;
   DELETE FROM dpe_creditos_arch WHERE folio = p_d_folio ;
   DELETE FROM dpe_rch_archivo   WHERE folio = p_d_folio ;

   SELECT COUNT (*)
   INTO r_bandera 
   FROM dpe_sol_creditos  
   WHERE folio = p_d_folio ;
   
   IF(r_bandera = 0)THEN
      DISPLAY " El reverso se realizó con éxito"
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   -- Reversa operación
   LET r_bandera = 0
     CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bandera
   IF(r_bandera = 0)THEN

      UPDATE glo_ctr_archivo
      SET    estado = 1, 
             folio = NULL -- cargado
      WHERE  folio = p_d_folio
      AND    estado = 2; -- integrado

      UPDATE bat_ctr_operacion 
      SET    folio = NULL
      WHERE  pid = g_pid
      AND    opera_cod = g_opera_cod;
         
      DISPLAY " Operación lista para volver a generarse."
   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   
   LET p_titulo = " Finalización de proceso - REVERSO INTEGRACIÓN"
   
   LET p_mensaje = " Finalización de proceso - REVERSO INTEGRACIÓN","\n",
                   "\n",
                   " Folio: "||p_d_folio,"\n",
                   "\n",
                   " Fecha de inicio: "||TODAY,"\n",
                   " Hora           : ",CURRENT HOUR TO SECOND,"\n"

   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
END MAIN
