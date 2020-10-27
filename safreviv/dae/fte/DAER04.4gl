################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 05/04/2013                                      #
################################################################################

################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DAE                                                      #
#Programa          => DAER04                                                   #
#Objetivo          => Programa para ejecutar el reverso de la liquidación de   #
#                     Devolución de Amortizaciones Excedentes                  #
#Fecha inicio      => 05/04/2013                                               #
################################################################################
DATABASE safre_viv

GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo       STRING -- titulo de la ventana
       ,p_i_folio        LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       --
       ,r_bandera                      SMALLINT
       ,v_s_qry                        STRING
       ,bn_reverso_desmarca            SMALLINT
       ,v_folio                        LIKE dis_preliquida.folio_liquida
       ,p_titulo                       STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje                      STRING -- cuerpo del mensaje enviado
       ,r_bnd_cnt                      SMALLINT--Bandera del reverso contable 
       ,v_folio_lote                   DECIMAL(9,0)
   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_i_folio        = ARG_VAL(5)  --Folio de liquidación
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_tipo_ejecucion = ARG_VAL(7)
   
   -- Bandera para comprobar que se ejecute correctamente el reverso de la desmarca
   LET bn_reverso_desmarca = 0

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DAER09.log")

   LET g_proceso_cod = 2400 -- DAE
   LET g_opera_cod   = 4    -- liquidacion

   --Invoca rutina para ejecutar reverso contable 
   --CALL fn_reverso_reg_cnt(p_i_folio)
   --RETURNING r_bnd_cnt

   --DISPLAY "Reverso registro contable: ", r_bnd_cnt 
   --
   --IF r_bnd_cnt = 0 THEN 
    --EJECUTA REVERSO LIQUIDACION
   
   LET v_s_qry = "\n SELECT folio_referencia", 
                 "\n   FROM glo_folio",
                 "\n  WHERE proceso_cod = ",g_proceso_cod,
                 "\n    AND status = 2",
                 "\n    AND folio = ",p_i_folio
                  
   --DISPLAY "v_s_qry: ", v_s_qry
   PREPARE prp_folio_lote FROM v_s_qry
   EXECUTE prp_folio_lote INTO v_folio
   DISPLAY "Folio Lote: ", v_folio

   -- Se invoca rutina para reversar la liquidación.
   CALL fn_reverso_liquidacion(p_i_folio)
      RETURNING r_bandera
      
   IF(r_bandera = 0)THEN
      DISPLAY "El reverso se realizó con éxito"
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   LET INT_FLAG = FALSE
      

   -- Reversa operación
   LET r_bandera = 0
     CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bandera
      LET v_s_qry = 
          "UPDATE glo_folio" 
          ,"\n   SET status = 1"
          ,"\n WHERE proceso_cod = ",g_proceso_cod
          ,"\n   AND status = 2"
          ,"\n   AND folio = ",p_i_folio

      --DISPLAY "UPDATE glo_folio: ", v_s_qry
      PREPARE Prpr_ActGlofolio FROM v_s_qry CLIPPED
      EXECUTE Prpr_ActGlofolio 

      LET v_s_qry = 
      "UPDATE glo_ctr_archivo" 
      ,"\n   SET estado = 3"
      ,"\n WHERE proceso_cod = ",g_proceso_cod
      ,"\n   AND estado = 4"
      ,"\n   AND folio = ",p_i_folio

      --DISPLAY "UPDATE glo_ctr_archivo: ", v_s_qry
      PREPARE Prpr_ActGgloCtrArchivo FROM v_s_qry CLIPPED
      EXECUTE Prpr_ActGgloCtrArchivo
      
      --  Se reversan los diagnosticos
      SELECT folio
      INTO   v_folio_lote
      FROM   glo_folio
      WHERE  folio_referencia = p_i_folio 

      --Actualiza de Estado 5 ArchivoSalida a 4 Liquidado
      UPDATE dae_det_solicitud
      SET    estado = 3
      WHERE  folio = v_folio_lote
      AND    estado = 4
         
   IF(r_bandera = 0)THEN      
      DISPLAY "Operación lista para volver a generarse."
   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   
   LET p_titulo = "Finalización de proceso - REVERSO LIQUIDACION"
   
   LET p_mensaje = "Finalización de proceso - REVERSO LIQUIDACION","\n",
                  "#\n",
                  "# Folio: "||p_i_folio,"\n",
                  "#\n",
                  "# Fecha de inicio: "||TODAY,"\n",
                  "# Hora           : ",CURRENT HOUR TO SECOND,"\n",
                  "# # # # # # # # # # # # # # # # # # # # # # # #"
 
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "",
                          p_titulo,
                          p_mensaje)
  
END MAIN