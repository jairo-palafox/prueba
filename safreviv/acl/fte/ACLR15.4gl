--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGR15                                                                 #
#Objetivo     => Programa que realiza el reverso de liquidación para SAR 92             #
#Fecha inicio => Enero 26, 2012                                                         #
#########################################################################################
DATABASE safre_viv

MAIN
DEFINE p_usuario_cod   LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion SMALLINT,
       p_cad_ventana   STRING,
       v_proceso_cod   LIKE cat_proceso.proceso_cod,
       v_opera_cod_liquidacion LIKE cat_operacion.opera_cod
       
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)
   LET v_proceso_cod = 106 # SAR92
   LET v_opera_cod_liquidacion = 4 #Liquidacion

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_cad_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_cad_ventana)
   END IF

   --Se invoca la funcion que efectua el reverso de la carga, integracion y preliquidación
   CALL fn_w_reverso_liquidacion(p_usuario_cod
                                 ,p_cad_ventana
                                 ,v_proceso_cod
                                 ,v_opera_cod_liquidacion)
END MAIN
