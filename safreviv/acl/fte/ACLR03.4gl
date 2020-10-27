--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => ACLR03                                                                 #
#Objetivo     => Programa que realiza el reverso de liquidación para pagos aclaratorio  #
#                sin cambio                                                             #
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
DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_i_folio            LIKE glo_folio.folio
       ,p_nombre_archivo     LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera            SMALLINT
       ,v_s_sql              STRING, -- cadena con un enunciado SQL
       v_marca_disposicion   SMALLINT,
       v_edo_maraca          SMALLINT,
       v_caus_marac          SMALLINT,
       v_cod_rechazo         SMALLINT 
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
   
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".ACLR03.log")

    
      -- Reversa operación
      LET r_bandera = 0

      CALL fn_reverso_reg_cnt(p_i_folio)  RETURNING   r_bandera  

      -- se ejecuta el SP de reverso
      LET v_s_sql = " EXECUTE PROCEDURE sp_acl_sc_rev_liquidacion(?,?)"
      PREPARE sid_reverso FROM v_s_sql
      EXECUTE sid_reverso INTO r_bandera, v_error_isam, v_mensaje
      USING p_i_folio,g_proceso_cod

      DISPLAY "Resultado: ", r_bandera
      DISPLAY "Mensaje: ", v_mensaje

--------------

      -- Reversa operación 5 Reporte
      CALL fn_reversa_operacion(g_pid,g_proceso_cod,5) 
      RETURNING r_bandera

      -- Reversa Operación 4 Liquidación
      CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod) 
      RETURNING r_bandera

--------------


      IF ( r_bandera = 0 ) THEN
         -- se termino el reverso
         DISPLAY "El reverso se realizó con éxito"

      ELSE
         -- Muestra el error ocurrido
         DISPLAY fn_recupera_inconsis_opera(r_bandera)
      END IF
 
END MAIN

{ Sección cortada de la linea 67
         UPDATE bat_ctr_proceso
            SET fecha_fin   = NULL,
                estado_cod  = 2
          WHERE pid         = g_pid
            AND proceso_cod = g_proceso_cod;

         UPDATE bat_ctr_operacion
            SET fecha_ini   = NULL,
                fecha_fin   = NULL,
                estado_cod  = 1   , 
                ind_tipo_ejecucion = 0 -- tipo manual
          WHERE pid         = g_pid
            AND proceso_cod = g_proceso_cod
            AND opera_cod   = 4;
          LET r_bandera = 0
linea 83}
