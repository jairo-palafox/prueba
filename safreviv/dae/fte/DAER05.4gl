################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 05/04/2013                                      #
################################################################################

################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DAE                                                      #
#Programa          => DAER05                                                   #
#Objetivo          => Programa lanzado que ejecuta el reverso de la generación #
#                    del archivo de Devolución de Amortizaciones Excedentes    #
#Fecha inicio      => 05/04/2013                                               #
################################################################################
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
       p_folio_lote        LIKE dis_preliquida.folio_liquida,
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo,
       r_bandera        SMALLINT,
       p_titulo         STRING,
       p_mensaje        STRING,
       v_folio_archivo     DECIMAL (9,0)
       
   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio_lote        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_tipo_ejecucion = ARG_VAL(7)
      
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DPER05.log")

   LET g_proceso_cod = 2401 -- DAE
   LET g_opera_cod   = 1    -- Generacion de archivo DAE
   LET INT_FLAG = FALSE

   -- Reversa operación
   LET r_bandera = 0
     CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bandera
         
   IF(r_bandera = 0)THEN      
      DISPLAY "Operación lista para volver a generarse."

      SELECT folio
      INTO   v_folio_archivo
      FROM   glo_folio
      WHERE  folio_referencia = p_folio_lote 

      --Actualiza de Estado 5 ArchivoSalida a 4 Liquidado
      UPDATE dae_det_solicitud
      SET    estado = 4
      WHERE  folio = p_folio_lote
      AND    estado = 5

      UPDATE glo_folio 
      SET    folio_referencia = NULL 
      WHERE  folio = v_folio_archivo
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   
   LET p_titulo = "Reverso del proceso - GENERACION DE ARCHIVO DAE"
   
   LET p_mensaje = "Reverso del proceso - GENERACION DE ARCHIVO DAE","\n",
                  "#\n",
                  "#\n Se he reversado la operación de GENERACION DE ARCHIVO DAE",
                  "#\n",
                  "# Folio: "||p_folio_lote,"\n",
                  "#\n",
                  "# Fecha de inicio: "||TODAY,"\n",
                  "# Hora           : ",CURRENT HOUR TO SECOND,"\n",
                  "# # # # # # # # # # # # # # # # # # # # # # # #"
   
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   
END MAIN