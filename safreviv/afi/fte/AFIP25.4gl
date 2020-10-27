##################################################################################################
#Modulo       => AFI                                                                             #
#Programa     => AFIP25                                                                          #
#Objetivo     => Baja de NSS rojos individual o por archivo                                      #
#Autor        => Jose Eduardo Ventura Bonola                                                     #
#Fecha inicio => 12/ABRIL/2017                                                                   #
##################################################################################################


DATABASE safre_viv

DEFINE g_usuario      CHAR(20)
DEFINE v_pid          INTEGER
DEFINE g_proceso_cod  SMALLINT
DEFINE g_opera_cod    SMALLINT
DEFINE v_estado       SMALLINT

MAIN

-- parametros que vienen de lanzador
   LET g_usuario       = ARG_VAL (1)
   LET v_pid           = ARG_VAL (2)
   LET g_proceso_cod   = ARG_VAL (3)
   LET g_opera_cod     = ARG_VAL (4)
   LET v_estado = 0

   CALL STARTLOG(g_usuario CLIPPED|| ".AFIP25.log")
{
   -- se obtienen rutas
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'afi'

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'
}

   IF v_estado = 0 THEN
      IF bnd_estado = 0 THEN
         DISPLAY ""
         DISPLAY "PROCESO EJECUTADO CORRECTAMENTE..."
         DISPLAY ""
      END IF

     CALL fn_actualiza_opera_fin( v_pid,
                                  g_proceso_cod,
                                  g_opera_cod)
                        RETURNING v_estado
      --DISPLAY v_estado
   ELSE
 --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(v_pid, g_proceso_cod, g_opera_cod)  RETURNING v_estado
   END IF

END MAIN