--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 05/12/2012
--===============================================================

################################################################################
#Modulo      => DPE                                                            #
#Programa    => DPEL45                                                         #
#Objetivo    => Genera  la validación del archivo que PROCESAR envia como acuse#
#            =>  después de que recibio el archivo que se genero en SACI       #
#Fecha inicio=> Noviembre 1, 2012                                              #
################################################################################

GLOBALS "DPEG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS
MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING, -- titulo de la ventana
       --
       r_bnd_fin_oper  SMALLINT,
       v_rest_valida   SMALLINT,
       v_QryTxt        STRING,
       v_i_resultado   SMALLINT,
       v_folio_liquida DECIMAL(9,0)  
       
-- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   LET r_bnd_fin_oper = 0
   
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- Pagos indebidos PROCESAR
   LET g_proceso_cod = 1006
   LET g_opera_cod   = 1  --Validación archivo acuse PROCESAR 

   -- se obtiene el PID del proceso
   --SELECT MAX(pid)
     --INTO g_pid
     --FROM bat_ctr_proceso
    --WHERE proceso_cod = g_proceso_cod   

    -- se obtiene el PID del proceso
    CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod)
         RETURNING g_pid
DISPLAY g_pid
   -- Valida operacion para verificar si se puede continuar.
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING v_rest_valida

   LET v_rest_valida = 0

   IF ( v_rest_valida = 0 ) THEN                         
      -- Inicio operacion.
      IF (fn_carga_archivo(g_pid,g_proceso_cod,g_opera_cod,2,"DPEL45","",
                           p_usuario_cod,FALSE) = TRUE)THEN
         DISPLAY "LA CARGA SE HA REALIZA CON ÉXITO, SE CONTINÚA CON LA INTEGRACION..."

         LET v_QryTxt = "EXECUTE FUNCTION fn_dpe_integra_pendientes_procesar(?, ?, ?)"
         PREPARE Prpr_ValidaEncabezados FROM v_QryTxt CLIPPED
         EXECUTE Prpr_ValidaEncabezados USING p_usuario_cod,    
                                              g_pid,  --PID         
                                              g_proceso_cod    

                                         INTO v_i_resultado 

         DISPLAY "ESTATUS FINALIZA PRELIQUIDACION: ", v_i_resultado
         --Inicia la liquidación        
         IF v_i_resultado = 0 THEN 
            -- se invoca la funcion para enviar la liquidacion
            CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, 1)
            
         LET v_QryTxt = "EXECUTE FUNCTION fn_dpe_posliquida_complementario(?, ?, ?)"
         PREPARE prp_posliquida_compl FROM v_QryTxt CLIPPED
         EXECUTE prp_posliquida_compl USING p_usuario_cod,    
                                            v_folio_liquida,
                                            g_proceso_cod    

                                         INTO v_i_resultado 

         END IF
          
      END IF
   ELSE
      CALL fn_mues_desc_valida(v_rest_valida)
  END IF     
END MAIN

{

 Obtiene la descripcion del error de la validacion y
  la muestra en mensaje para usuario.
}
FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT
        ,v_descripcion LIKE cat_bat_parametro_salida.descripcion
  

   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
     INTO v_descripcion
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_resultado_opera

   -- Muestra el mensaje encontrado
   CALL fn_mensaje("Atención",v_descripcion CLIPPED,"information")

END FUNCTION -- fn_mues_desc_valida