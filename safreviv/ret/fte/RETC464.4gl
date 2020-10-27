--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#MODULO       => RET                                                                    #
#PROGRAMA     => RETC464                                                                #
#OBJETIVO     => Consulta si hay casos que se tengan que reversar por inactividad       #
#Fecha inicio => 03 Marzo 2018                                                          #
#Modificacion =>                                                                        #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"

{
======================================================================
Clave: 
Nombre: main
Fecha creacion: Marzo 3, 2018
Autor: Ricardo Pérez
Narrativa del proceso que realiza:
Revisa los casos que deben cancelarse por inactividad en la tableta
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
MAIN 
DEFINE v_stop         CHAR(1)
DEFINE v_id_solicitud DECIMAL(10,0)
DEFINE v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente, 
       v_marca_entra        LIKE sfr_marca.marca,
       v_estado_marca       SMALLINT,
       v_marca_causa        LIKE sfr_marca.marca,
       v_usuario            LIKE seg_usuario.usuario_cod,
       v_proceso_cod        LIKE cat_proceso.proceso_cod,
       v_sql                STRING, -- cadena con instruccion sql
       v_respuesta_marcaje  SMALLINT, -- resultado de la desmarca
       v_hoy                DATETIME YEAR TO MINUTE, 
       v_f_registro         DATETIME YEAR TO MINUTE,
       v_f_mayor            DATETIME YEAR TO MINUTE,
       v_f_menor            DATETIME YEAR TO MINUTE,
       v_interval           INTERVAL MINUTE(9) TO MINUTE, 
       v_interval_10        INTERVAL MINUTE(9) TO MINUTE,
       v_indicador          CHAR(1),
       v_contador           SMALLINT 
             
   LET v_indicador    = ""   
   LET v_marca_entra  = 803
   LET v_estado_marca = 0
   LET v_marca_causa  = 803
   LET v_usuario      = "safreviv"
   LET v_proceso_cod  = g_proceso_cod_ret_ley73_ws
   LET v_respuesta_marcaje = 0
   LET v_f_mayor = '2018-03-03 12:10'
   LET v_f_menor = '2018-03-03 12:00'
   LET v_contador = 0 
   -- si se obtuvo el titulo, se pone como titulo de programa
   DISPLAY "Inicia Programa de detección de inactividades", CURRENT YEAR TO MINUTE
   SELECT CAST((v_f_mayor - v_f_menor) AS INTERVAL MINUTE(9) TO MINUTE )
   INTO   v_interval_10
   FROM   systables
   WHERE  tabid = 1

   DECLARE cur_sol_vencidas CURSOR FOR
   SELECT a.id_solicitud, b.id_derechohabiente, a.f_registro 
   FROM   ret_sol_medio_entrega a,
          ret_solicitud_generico b
   WHERE  a.id_solicitud = b.id_solicitud
   AND    a.f_registro IS NOT NULL 
   AND    b.estado_solicitud = 8
   AND    a.grupo = 1
   AND    a.medio_entrega = 1

   SELECT UPPER(indicador)
   INTO   v_indicador
   FROM   ret_indicador_actividad
   
   WHILE v_indicador = "S" 
      LET v_hoy = CURRENT YEAR TO MINUTE 
      DISPLAY "dentro del while v_hoy>", v_hoy, "<" 
      LET v_contador = v_contador + 1
      
      FOREACH cur_sol_vencidas INTO v_id_solicitud, v_id_derechohabiente, v_f_registro 
         DISPLAY " v_f_registro >", v_f_registro
         SELECT CAST((v_hoy - v_f_registro) AS INTERVAL MINUTE(9) TO MINUTE)
         INTO   v_interval
         FROM   systables 
         WHERE  tabid = 1

         DISPLAY "Interval >", v_interval, "<"
         DISPLAY "Interval 10 >", v_interval_10, "<"

         IF v_interval > v_interval_10 THEN 
            UPDATE ret_solicitud_generico
            SET    estado_solicitud = 100,
                   cod_rechazo      = 2000
            WHERE  id_solicitud = v_id_solicitud

            UPDATE ret_sol_medio_entrega
            SET    f_registro = NULL 
            WHERE  id_solicitud = v_id_solicitud

            -- se prepara la ejecucion de la desmarca
            LET v_sql = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
            
            -- se ejecuta
            PREPARE stm_desmarcaje FROM  v_sql
            EXECUTE stm_desmarcaje USING v_id_derechohabiente, 
                                         v_marca_entra       ,
                                         v_id_solicitud      ,
                                         v_estado_marca      ,
                                         v_marca_causa       ,
                                         v_usuario           ,
                                         v_proceso_cod 
               INTO v_respuesta_marcaje
         END IF 
         
      END FOREACH
      SELECT UPPER(indicador)
      INTO   v_indicador
      FROM   ret_indicador_actividad
      IF v_indicador = "S" AND v_contador > 1 THEN 
         SLEEP 600  -- Se pausa por 10 minutos para volver a revisar si hay inactividades
      END IF 
   END WHILE 
     
END MAIN
