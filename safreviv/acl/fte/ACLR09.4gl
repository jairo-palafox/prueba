--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:25/10/2012
--===============================================================

#########################################################################################
#Modulo       => ACL                                                                    #
#Programa     => ACLR09                                                                 #
#Objetivo     => Programa que realiza el reverso de liquidación para ENACLARA           #
#Fecha inicio => Enero 26, 2012                                                         #
#Modificacion => se agrega archivo globales de aclaratorio y se sustituyen las variables#
#                correspondientes; hilda rivas                                          #
#########################################################################################
DATABASE safre_viv

GLOBALS "ACLG02.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod   LIKE seg_usuario.usuario_cod,
      -- p_tpo_ejecucion SMALLINT,
      -- p_cad_ventana   STRING,
      -- v_proceso_cod   LIKE cat_proceso.proceso_cod,
      -- v_opera_cod_liquidacion LIKE cat_operacion.opera_cod
        p_i_folio            LIKE glo_folio.folio
       ,p_nombre_archivo     LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera            SMALLINT
       ,v_s_sql              STRING -- cadena con un enunciado SQL
       ,v_error_isam         INTEGER
       ,v_mensaje            VARCHAR(255)           

       -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_i_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)    
   
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".ACLR06.log")

      -- Reversa operación
      LET r_bandera = 0

      CALL fn_reverso_reg_cnt(p_i_folio) RETURNING   r_bandera    

      -- se ejecuta el SP de reverso
      LET v_s_sql = " EXECUTE PROCEDURE sp_acl_enaclara_rev_liquidacion(?,?)"
      PREPARE sid_reverso FROM v_s_sql
      EXECUTE sid_reverso INTO r_bandera, v_error_isam, v_mensaje
      USING p_i_folio,g_proceso_cod

      DISPLAY "Resultado: ", r_bandera
      DISPLAY "Mensaje: ", v_mensaje
      
      CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
      RETURNING r_bandera

      IF ( r_bandera = 0 ) THEN
         -- se termino el reverso
         DISPLAY "El reverso se realizó con éxito"

      ELSE
         -- Muestra el error ocurrido
         DISPLAY fn_recupera_inconsis_opera(r_bandera)
      END IF
 
END MAIN       
{      
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)
   LET v_proceso_cod = g_proceso_cod_acl_registro_pagos # enaclara
   LET v_opera_cod_liquidacion = g_opera_cod_liquidacion #Liquidacion

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
}