--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================
#########################################################################################
#Modulo       => DEO                                                                    #
#Programa     => DEOR07                                                                 #
#Objetivo     => Programa que ejecuta el rutna de reverso de liquidación                #
#                para la devolucion por errores de operacion                            #
#Fecha inicio => Febrero 18, 2012                                                       #
#########################################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_s_titulo       STRING -- titulo de la ventana
       ,p_operacion      SMALLINT
       ,p_i_folio        LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       --
       ,r_bandera        SMALLINT
       ,v_s_qry          STRING

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_i_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DEOL07.log")

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
          ,"\n WHERE proceso_cod = ", g_proceso_cod
          ,"\n   AND status = 2"
          ,"\n   AND folio = ",p_i_folio

      PREPARE Prpr_ActGlofolio FROM v_s_qry CLIPPED
      EXECUTE Prpr_ActGlofolio 

      LET v_s_qry = 
      "UPDATE glo_ctr_archivo" 
      ,"\n   SET estado = 3"
      ,"\n WHERE proceso_cod = ", g_proceso_cod
      ,"\n   AND estado = 4"
      ,"\n   AND folio = ",p_i_folio

      PREPARE Prpr_ActGgloCtrArchivo FROM v_s_qry CLIPPED
      EXECUTE Prpr_ActGgloCtrArchivo

         
   IF(r_bandera = 0)THEN      
      DISPLAY "Operación lista para volver a generarse."


         ;
      -- Retaura el status de folio para volver a usarlo

   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF

   
 
END MAIN