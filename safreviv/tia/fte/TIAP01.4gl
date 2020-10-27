--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

--=============================================================================================
-- Modulo       => TIA
-- Programa     => TIAP01.4gl
-- Descripcion  => Lanzado Programa de registro de información histórica para el
--              => módulo de "Traspasos I-A"
-- Autor        => GERARDO ALFONSO VEGA PAREDES
-- Fecha inicio => 26 de Marzo de 2012
--=============================================================================================
-- Modificación => Cambiar valor de result_operacion de 03 a 04 (nss no existe en afi_decreto
-- Fehca        => 18 de Septiembre de 2017.
-- Autor        => GERARDO ALFONSO VEGA PAREDES.
-- Clave cambio => saci2017-37
--=============================================================================================

DATABASE safre_viv
GLOBALS
DEFINE g_pid                       LIKE bat_ctr_proceso.pid,     -- ID del proceso
       g_proceso_cod               LIKE cat_proceso.proceso_cod, -- Código del proceso
       g_opera_cod_integracion     LIKE cat_operacion.opera_cod, -- Código de operación
       g_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod  -- Código de operación
       
END GLOBALS

MAIN
DEFINE p_opera_cod_carga LIKE cat_operacion.opera_cod, -- Código de operacion
       p_usuario_cod     LIKE seg_usuario.usuario_cod, -- Clave de usuario
       p_nom_archivo     STRING, 
       v_cadena_pid      VARCHAR(5),
       v_cadena_proc     VARCHAR(5),
       v_cadena_opera    VARCHAR(5),
       v_comando         STRING,
       r_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_ruta_listados   LIKE seg_modulo.ruta_listados,
       r_folio           LIKE glo_folio.folio,
       r_resultado_opera SMALLINT,
       r_ruta_vacia      LIKE seg_modulo.ruta_bin,
       r_bnd_estado      SMALLINT,
       p_titulo          STRING, -- titulo del mensaje enviado en el correo
       p_mensaje         STRING, -- cuerpo del mensaje enviado
       v_descripcion     CHAR(150)  

   LET r_bnd_estado = 0 -- Inicializa bandera
   
   -- Primer parámetro
   LET p_usuario_cod = ARG_VAL(1)
   -- Segundo parámetro
   LET g_pid         = ARG_VAL(2)
   -- Tercer parámetro
   LET g_proceso_cod = ARG_VAL(3)
   -- Cuarto parámetro
   LET g_opera_cod_integracion = ARG_VAL(4) -- Paso de información a las tablas históricas
   -- Quinto parámetro
   LET r_folio       = ARG_VAL(5)
   -- Segundo parámetro
   LET p_nom_archivo = ARG_VAL(6)
   
   
   -- Indicamos operacion de carga
   LET p_opera_cod_carga = 1
   
   -- Archivo de bitacora
   CALL STARTLOG(p_usuario_cod CLIPPED||".TIAP01.log")
   
   CALL fn_display_proceso(0," Integración Traspasos I-A ")
   
   -- Genera folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod_integracion, p_usuario_cod)
                        RETURNING r_folio
   
   -- Recupera el nombre del archivo cargado
   CALL fn_recupera_arch_cargado(g_proceso_cod,p_opera_cod_carga)
                     RETURNING p_nom_archivo

   LET r_resultado_opera = 0
   
   -- Llamada a ejecución de procedimiento almacenado
   CALL fn_guarda_historicos_traspasos(r_folio) RETURNING r_resultado_opera
   
   -- cambia a estado erroneo si no se ejecutó correctamente el SP
   IF ( r_resultado_opera ) THEN
      -- Recupera la descripcion del error para enviarlo como mensaje por correo
      LET p_titulo = "Error de operación - Traspasos I-A - Integración"
      
      SELECT descripcion
        INTO v_descripcion 
        FROM cat_bat_parametro_salida
       WHERE cod_salida = r_resultado_opera
      
      LET p_mensaje = v_descripcion
      
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod_integracion) 
               RETURNING r_resultado_opera 
               
      -- actualiza el estado del archivo a reversado, para poder cargar el mismo
      CALL fn_act_edo_archivo(p_nom_archivo,r_folio,3,p_usuario_cod)
                              RETURNING r_bnd_estado 
   ELSE
      LET r_resultado_opera = 0
      
      CALL fn_display_proceso(1," Integración Traspasos I-A  ")
      
      -- Se finaliza la carga de registros historicos
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod_integracion) 
                         RETURNING r_resultado_opera
      
      -- se inicializan varibles para el correo en caso exitoso
      LET p_mensaje = "Integración realizada con éxito.\nYa se puede continuar con la Preliquidación."
      LET p_titulo = "Finalización de operación - Traspasos I-A - Integración"
      
      -- Se obtienen las variables para invocar el procedure 
      -- que almacena la inf. en tablas históricas
      CALL fn_rutas("tia") RETURNING r_ruta_ejecutable, r_ruta_vacia
      CALL fn_rutas("bat") RETURNING r_ruta_vacia, r_ruta_listados
      
      -- Actualiza el estado del archivo en glo_ctr_archivo a integrado
      -- 2 = integrado
      CALL fn_act_edo_archivo(p_nom_archivo,r_folio,2,p_usuario_cod)
                            RETURNING r_resultado_opera
   END IF
   
   -- se envia correo de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, 
                          g_opera_cod_integracion, 
                          NULL, p_titulo,p_mensaje)
      
END MAIN

{ ==========================================================================
Clave:  fn_guarda_historicos_traspasos
Nombre: fn_guarda_historicos_traspasos
Fecha creacion: 26 de Marzo de 2012
Autor: Ilhuitemoc Ricardo Ortiz
Narrativa del proceso que realiza:
 Esta función ejecuta el store procedure que almacena la información 
 de los registros históricos para el módulo de traspasos I-A
 Parametros de Entrada:
 -
 Parámetros de salida:
 -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     15 Ene 2013             - El SP devolvera tambien:
                                        * id_decreto
                                        * nombre
                                        * apellido paterno
                                        * apellido materno
Ivan Vega     21 Febrero 2013         - Al finalizar el SP, se verifica si
                                        en el proceso existieron NSS en el archivo
                                        que no se encuentran en afi_derechohabiente;
                                        de ser asi, se indica en pantalla lo ocurrido
============================================================================}
FUNCTION fn_guarda_historicos_traspasos(p_folio)
DEFINE p_folio         LIKE glo_folio.folio,
       v_sql_procedure STRING,
       v_sql_error     INTEGER, -- variables para la recepcion de errores
       v_isam_error    INTEGER,
       v_mensaje       VARCHAR(250),
       v_det_tia_consec_cuenta DECIMAL (11,0),
       v_id_decreto    DECIMAL(11,0),
       v_nombre        CHAR(40),
       v_apaterno      CHAR(40),
       v_amaterno      CHAR(40),
       v_conteo        INTEGER -- conteo de NSS no encontrados


   LET v_sql_procedure = "EXECUTE PROCEDURE sp_historico_tia_traspaso(?,?,?)"
   
   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE prp_historicoTraspasos_ia FROM v_sql_procedure
   EXECUTE prp_historicoTraspasos_ia USING p_folio, g_pid, g_proceso_cod
           INTO v_sql_error, v_isam_error, v_mensaje, v_det_tia_consec_cuenta, v_id_decreto, v_nombre,
           v_apaterno, v_amaterno

   -- si la integracion se realizo correctamente
   IF ( v_sql_error = 0 ) THEN
      -- se muestra mensaje de retorno del SP en pantalla
      DISPLAY v_mensaje

      -- se asume que todos los NSS se encontraron
      
      -- se cuentan los NSS no encontrados
      SELECT COUNT(*)
      INTO   v_conteo
      FROM   tia_det_traspaso
      WHERE  folio = p_folio
--      AND    result_operacion = "03"   --saci2017-37
      AND    result_operacion = "04"   --saci2017-37

      -- si existen NSS no encontrados en afi_derechohabiente
      IF ( v_conteo > 0 ) THEN
         DISPLAY "Se identificaron ", v_conteo, " NSS contenidos en el archivo procesado\n, no encontrados en la base de datos de derechohabientes."
      END IF
      
      RETURN FALSE
   ELSE
      DISPLAY "\nError en sp_historicos_traspasos_tia (Codigo SQL) : ", v_sql_error
      DISPLAY "\nError en sp_historicos_traspasos_tia (Codigo ISAM): ", v_isam_error
      DISPLAY "\nError en sp_historicos_traspasos_tia (Mensaje)    : ", v_mensaje
      DISPLAY "Consecutivo cuenta en curso: ", v_det_tia_consec_cuenta
      DISPLAY "ID Decreto      : ", v_id_decreto
      DISPLAY "Nombre          : ", v_nombre
      DISPLAY "Apellido Paterno: ", v_apaterno
      DISPLAY "Apellido Materno: ", v_amaterno
      RETURN TRUE
   END IF

END FUNCTION