################################################################################
#Modulo       => LAV                                                           #
#Programa     => LAVL03                                                        #
#Objetivo     => Lanzador carga archivo precio dólar                           #
#Fecha inicio => 29/12/2014                                                    #
################################################################################
--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--==============================================================================
DATABASE safre_viv

GLOBALS
DEFINE g_usuario_cod    LIKE seg_usuario.usuario_cod,        -- Usuario firmado 
       g_pid            LIKE bat_ctr_proceso.pid,            -- ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod,        -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod,        -- codigo de operacion,
       v_folio_lote     LIKE glo_folio.folio,                -- Folio de proceso
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo  -- Nombre Archivo Procesado
END GLOBALS

MAIN
DEFINE p_tipo_ejecucion   SMALLINT, -- forma como ejecutara el programa
       p_titulo           STRING, -- titulo de la ventana
       r_bnd_fin_oper     SMALLINT,
       bnd_valida_opera      SMALLINT,
       r_bnd_carga        BOOLEAN,
       r_bnd_valida_carga SMALLINT

   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   LET r_bnd_fin_oper = 0
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = 2701
   LET g_opera_cod   = 1
   
   -- Valida operacion para verificar si se puede continuar.
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
        RETURNING bnd_valida_opera
   DISPLAY bnd_valida_opera
   IF ( bnd_valida_opera = 0 ) THEN
      IF (fn_carga_archivo(g_pid,g_proceso_cod,g_opera_cod,2,"LAVL01","",g_usuario_cod, TRUE)= TRUE)THEN
         --Se ejecuta la validación de los registros cargados.
         DISPLAY "La carga del archivo es correcta"
      ELSE
         DISPLAY "La carga del archivo es incorrecta" 
      END IF
   ELSE
   DISPLAY r_bnd_fin_oper
      CALL fn_mensaje("Atención", 
                      fn_recupera_inconsis_opera(r_bnd_fin_oper), 
                      "stop")
   END IF

END MAIN