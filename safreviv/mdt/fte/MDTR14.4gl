--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 05/06/2012
--===============================================================
#########################################################################################
#Modulo       => UNI                                                                    #
#Programa     => UNIR09                                                                 #
#Objetivo     => Programa que ejecuta la rutina de reverso de carga de archivo          #
#                para archivo recurrente de mandatos                                    #
#Fecha inicio => Junio 05, 2012                                                         #
#########################################################################################
DATABASE safre_viv

DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso REVERZAR
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion REVERZAR
       p_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso REVERSO
       p_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion  REVERSO

MAIN
DEFINE v_cad             STRING
DEFINE r_actualiza       SMALLINT
DEFINE v_continua        SMALLINT
DEFINE v_proceso_desc    CHAR(40)
DEFINE v_opera_desc      CHAR(40)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_s_titulo       STRING -- titulo de la ventana
       ,p_d_folio        LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera        SMALLINT
       ,v_pid            INTEGER  -- pid a reversar       
       

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

CALL fn_actualiza_opera_ini(g_pid,p_proceso_cod,p_opera_cod,p_d_folio,"MDTR14","",p_usuario_cod)
RETURNING v_continua

IF(v_continua  <> 0)THEN
  # Imprime el mensaje de inconsistencia en consola
  DISPLAY "Program stopped :"
  CALL fn_desplega_inc_operacion(v_continua)
  LET v_cad = "CALL fn_error_opera_ini(?,?,?)" 
  PREPARE prp_error_opera FROM v_cad 
  EXECUTE prp_error_opera USING g_pid, p_proceso_cod, p_opera_cod  
                          INTO  r_actualiza  

                          
ELSE
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".MDTR14.log")

   LET g_proceso_cod = 1303  # PROCESO A REVERSAR
   LET g_opera_cod   = 1

   SELECT a.proceso_desc a,
          b.opera_desc   b
   INTO   v_proceso_desc  ,
          v_opera_desc  
   FROM   cat_proceso a ,
          cat_operacion b
   WHERE  a.proceso_cod = g_proceso_cod 
   AND    a.proceso_cod = b.proceso_cod 
   AND    b.opera_cod = g_opera_cod

   DISPLAY "MDTR14: INICIO REVERSO" 
   DISPLAY " "   
   DISPLAY "MDTR14: PROCESO   : ",v_proceso_desc
   DISPLAY "MDTR14: OPERACION : ",v_opera_desc

   -- Restaura datos afectados por carga de archivo
   CALL fn_mdt_corrige_reg_carga_archivo(p_nombre_archivo)
   DISPLAY "MDTR14: ARCHIVO REVERSADO: ",p_nombre_archivo   
   DISPLAY "MDTR14: EL REVERSO SE REALIZÓ CON ÉXITO"

   -- Reversa operación
   LET r_bandera = 0
   
     SELECT max(a.pid) 
     INTO   v_pid     
     FROM   bat_ctr_operacion a
     WHERE  a.proceso_cod = g_proceso_cod 
     AND    a.opera_cod   = g_opera_cod
     AND    a.nom_archivo = p_nombre_archivo   
     
     CALL fn_reversa_operacion(v_pid,g_proceso_cod,g_opera_cod)
     RETURNING r_bandera
     
     IF(r_bandera <> 0)THEN
        CALL fn_desplega_inc_operacion(r_bandera)
     END IF
     CALL fn_actualiza_opera_fin(g_pid,p_proceso_cod,p_opera_cod) RETURNING v_continua
     IF(v_continua  <> 0)THEN
        # Imprime el mensaje de inconsistencia en consola
        CALL fn_desplega_inc_operacion(v_continua)
        CALL fn_error_opera(g_pid,p_proceso_cod,p_opera_cod) RETURNING v_continua
        IF(v_continua  <> 0)THEN
           # Imprime el mensaje de inconsistencia en consola
           CALL fn_desplega_inc_operacion(v_continua)
        END IF
           RETURN 0  # Termina ejecucion
     END IF

     CALL fn_correo_proceso(g_pid,p_proceso_cod,p_opera_cod,
                          "", -- no lleva archivo adjunto
                          "",
                          "OPERACION FINALIZADA")
END IF
END MAIN

{
   Funcion : fn_mdt_corrige_reg_carga_archivo
   Fecha   : Marzo 21, 2012
   Descrip : corrige datos adicionales de reverso de integracion
   Aturo   : Felipe Nava
}
FUNCTION fn_mdt_corrige_reg_carga_archivo(p_nombre_archivo)
  DEFINE 
   p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
   --    
   ,v_s_qry          STRING

   LET v_s_qry =
     "DELETE FROM glo_ctr_archivo"
    ,"\n WHERE nombre_archivo = ?"
    ,"\n   AND proceso_cod = ?"
    --,"\n   AND estado = 1"
    
   PREPARE Prpr_dpe_LimpiaCtrArchvo FROM v_s_qry 
   EXECUTE Prpr_dpe_LimpiaCtrArchvo USING  p_nombre_archivo, g_proceso_cod

END FUNCTION -- fn_mdt_corrige_reg_carga_archivo
