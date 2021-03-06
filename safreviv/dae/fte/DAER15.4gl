################################################################################
#Proyecto         => SAFRE VIVIENDA                                            #
#Propietario      => E.F.P.                                                    #
#Modulo           => DAE                                                       #
#Programa         => DAER15                                                    #
#Objetivo         => Lanzado del reverso de Archivo Salida Ajuste Ind DAE      #
#Fecha inicio     => 18/Abr/2016                                               #
################################################################################
--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--==============================================================================
DATABASE safre_viv

GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tipo_ejecucion SMALLINT,
       p_s_titulo       STRING,
       p_i_folio        LIKE dis_preliquida.folio_liquida,
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo,
       r_bandera        SMALLINT,
       p_titulo         STRING,
       p_mensaje        STRING,
       v_folio_lote     DECIMAL (9,0)
       
   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_i_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_tipo_ejecucion = ARG_VAL(7)
      
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DPER15.log")

   LET g_proceso_cod = 2406 -- Ajuste Individual DAE
   LET g_opera_cod   = 5
   LET INT_FLAG = FALSE

   -- Reversa operación
   LET r_bandera = 0
     CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bandera
         
   IF(r_bandera = 0)THEN      
      DISPLAY "Operación lista para volver a generarse."
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF

   LET p_titulo = "Reverso de la operación Generación de Archivo Ajuste Ind DAE"

   LET p_mensaje = "Reverso de la operación Generación de Archivo Ajuste Ind DAE","\n",
                  "   Se he reversado la operación","\n",
                  "   Folio: "||p_i_folio,"\n",
                  "   Fecha de inicio: "||TODAY,"\n",
                  "   Hora           : ",CURRENT HOUR TO SECOND,"\n"

   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   
END MAIN