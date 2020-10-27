------------------------------------------------------------------------------------------------
-- Proyecto:   > SAFRE VIVIENDA
-- Proceso:    > Registro de pagos Aclaratorio sin cambio de nss y con cambio nss y nombre
-- Componente: > Programa que concatena los archivos rechazados de ACL para generar uno solo
--             > y reprocesar en al aplicación 
-- Autor:      > Gerardo Alfonso Vega Paredes
-- Fecha:      > 21 de abril de 2015
------------------------------------------------------------------------------------------------

DATABASE safre_tmp

MAIN

   call concatena()

END MAIN


FUNCTION concatena()

   DEFINE v_id      INTEGER,
          v_proceso SMALLINT,
          v_archivo CHAR(40),
          query     CHAR(100),
          v_cont    INTEGER,
          comando   CHAR(200),
          hoy       DATE

   LET hoy = TODAY
          
   LET query = " SELECT id, ",
               "        proceso_cod, ",
   	           "        archivo ",
               " FROM   tmp_archivos_acl ",
               " ORDER  BY 1,2 "

   PREPARE cla_concatena FROM query
   DECLARE cur_concatena CURSOR FOR cla_concatena
   
   FOREACH cur_concatena INTO v_id, v_proceso, v_archivo
   	  LET v_cont = v_cont + 1
      DISPLAY "contador ",v_cont
      CASE v_proceso 
         WHEN 102
   	        LET comando = "cat ",v_archivo CLIPPED," >> ",hoy USING "YYYYMMDD","esp.disscnss "
         WHEN 103
   	        LET comando = "cat ",v_archivo CLIPPED," >> ",hoy USING "YYYYMMDD","esp.disccnss "         
         WHEN 107
   	        LET comando = "cat ",v_archivo CLIPPED," >> ",hoy USING "YYYYMMDD","esp.diccncno "         
      END CASE
   	  RUN comando
   END FOREACH


END FUNCTION
