--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 03/03/2015
--==============================================================================

################################################################################
#Módulo          => PRT                                                        #
#Programa        => PRTM03                                                     #
#Objetivo        => Catálogo de pocesos, operaciónes y contratos del           #
#                   bus genérico                                               #
#Fecha Inicio    => 03 Marzo 2015                                              #
################################################################################

DATABASE safre_viv

DEFINE p_usuario_cod        VARCHAR(20),
       p_tpo_ejecucion      SMALLINT,
       p_titulo_ventana     STRING,
       v_ventana            ui.Window,
       v_forma              ui.Form,
       v_ruta_ejecutables   LIKE seg_modulo.ruta_bin,
       v_ruta_listados      LIKE seg_modulo.ruta_listados
DEFINE g_enter char(1)      ,
       v_cb_orden           ui.ComboBox ,
       v_cb_tipo_dato       ui.ComboBox ,
       v_tam_arreglo        SMALLINT  ,
       g_orden              SMALLINT  ,
       g_id_cat_bus_entidad LIKE cat_bus_bloque.id_cat_bus_bloque, # indice global para control interno temporal
       i                    SMALLINT

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función principal e inicialización de variables          #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
MAIN

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tpo_ejecucion  = ARG_VAL(2)
   LET p_titulo_ventana = ARG_VAL(3)

   # Recupera las rutas del módulo BUS
   CALL fn_rutas("prt") RETURNING v_ruta_ejecutables, v_ruta_listados

   # inicializa consultas preparadas
   CALL fn_inicializa_consultas()
   
   # llama ventana para elegir modulo
   CALL fn_elige_modulo()
   
END MAIN

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para inicializar consultas preparadas            #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   LET g_id_cat_bus_entidad = 0

   LET v_consulta = "\n SELECT id_cat_bus_bloque,",
	                "\n        id_cat_bus_detalle_contrato,",
		            "\n        cve_natural,",
		            "\n        desc_bloque,",
		            "\n        ent_negocio_bloque",
                    "\n   FROM cat_bus_bloque",
	                "\n  WHERE id_cat_bus_detalle_contrato = ?"

   PREPARE prp_recupera_bloque_contrato FROM v_consulta

   LET v_consulta = " SELECT modulo_cod,",
                    "        modulo_desc",
                    "   FROM seg_modulo",
                    "  WHERE 1 = 1",
                    "  ORDER BY modulo_desc"
   PREPARE prp_rec_modulos FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 modulo_desc",
                    "   FROM seg_modulo",
                    "  WHERE modulo_cod = ?",
                    "  ORDER BY 1"
   PREPARE prp_rec_descripcion_modulo FROM v_consulta

   LET v_consulta = "\n SELECT pro.id_cat_bus_proceso,",
                    "\n        pro.desc_proceso_bus,",
                    "\n        ope.id_cat_bus_operacion,",
                    "\n        ope.desc_opera_bus,",
                    "\n        con.id_cat_bus_contrato,",
                    "\n        con.desc_contrato",
                    "\n   FROM cat_bus_proceso pro LEFT OUTER JOIN cat_bus_operacion ope",
                    "\n     ON ope.id_cat_bus_proceso = pro.id_cat_bus_proceso",
                    "\n        LEFT OUTER JOIN cat_bus_contrato con",
                    "\n     ON con.ind_vigencia = 1", # Activo
                    "\n    AND con.id_cat_bus_operacion = ope.id_cat_bus_operacion",
                    "\n  WHERE pro.modulo_cod = ?",
                    "\n  ORDER BY pro.id_cat_bus_proceso,ope.id_cat_bus_operacion,con.id_cat_bus_contrato"
   PREPARE prp_rec_arbol_modulo FROM v_consulta

   LET v_consulta = "\n SELECT con.id_cat_bus_operacion,",
                    "\n        con.id_cat_bus_contrato,",
                    "\n        con.cod_contrato,",
                    "\n        con.desc_contrato,",
                    "\n        con.ind_vigencia,",
                    "\n        con.f_ini_vigencia,",
                    "\n        con.f_fin_vigencia,",
                    "\n        con.id_cat_bus_negocio,",
                    "\n        neg.programa,",
                    "\n        con.entidad_negocio",
                    "\n   FROM cat_bus_contrato con LEFT OUTER JOIN cat_bus_negocio neg",
                    "\n     ON neg.id_cat_bus_negocio = con.id_cat_bus_negocio",
                    "\n   WHERE con.id_cat_bus_operacion = ?",
                    "\n     AND con.id_cat_bus_contrato = ?",
                    "\n     AND con.ind_vigencia = 1"
   PREPARE prp_recupera_datos_contrato FROM v_consulta

   LET v_consulta = "EXECUTE FUNCTION fn_prt_recupera_detalle_entidad(?,?,?,?)"
   PREPARE prp_recuper_detalle_entidad FROM v_consulta

   LET v_consulta = "EXECUTE FUNCTION fn_prt_recupera_detalle_contrato(?,?)"
   PREPARE prp_recuper_detalle_contrato FROM v_consulta

   LET v_consulta = "EXECUTE FUNCTION fn_prt_recupera_columnas_tabla(?)"
   PREPARE prp_recupera_atributos_entidad FROM v_consulta

   LET v_consulta = "EXECUTE FUNCTION fn_prt_recupera_columnas_lista(?,?,?)"
   PREPARE prp_recupera_atributos_lista FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 1",
                    "   FROM systables",
                    "  WHERE tabname = ?"
   PREPARE prp_verifica_entidad_negocio FROM v_consulta

   LET v_consulta = "SELECT FIRST 1 seq_cat_bus_proceso.NEXTVAL",
                    "  FROM syssequences"
   PREPARE prp_rec_seq_cat_bus_proceso FROM v_consulta

   LET v_consulta = "SELECT FIRST 1 seq_cat_bus_operacion.NEXTVAL",
                    "  FROM syssequences"
   PREPARE prp_rec_seq_cat_bus_operacion FROM v_consulta

   LET v_consulta = "\n SELECT FIRST 1 seq_cat_bus_contrato.NEXTVAL",
                    "\n   FROM syssequences"
   PREPARE prp_rec_seq_cat_bus_contrato FROM v_consulta

   LET v_consulta = "\n SELECT FIRST 1 seq_cat_bus_detalle_contrato.NEXTVAL",
                    "\n   FROM syssequences"
   PREPARE prp_rec_seq_cat_bus_detalle_contrato FROM v_consulta

   LET v_consulta = "\n SELECT FIRST 1 seq_cat_bus_bloque.NEXTVAL",
                    "\n   FROM syssequences"
   PREPARE prp_rec_seq_cat_bus_bloque FROM v_consulta
   
   LET v_consulta = "INSERT INTO cat_bus_detalle_contrato",
                    " (id_cat_bus_detalle_contrato,",
                    "  id_cat_bus_contrato,",
                    "  cve_natural,",
                    "  etiqueta,",
                    "  atributo_negocio,",
                    "  tipo_dato,",
                    "  orden,",
                    "  ind_opcional,",
                    "  ind_sentido)",
                    "VALUES(?,?,?,?,?,?,?,?,?)"
   PREPARE prp_almacena_detalle_contrato FROM v_consulta

   LET v_consulta = "INSERT INTO cat_bus_bloque",
                    " (id_cat_bus_bloque,",
                    "  id_cat_bus_detalle_contrato,",
                    "  cve_natural,",
                    "  desc_bloque,",
                    "  ent_negocio_bloque)",
                    "VALUES(?,?,?,?,?)"
   PREPARE prp_almacena_bloque_contrato FROM v_consulta

   LET v_consulta = "INSERT INTO cat_bus_detalle_bloque",
                    " (id_cat_bus_detalle_bloque,",
                    "  id_cat_bus_bloque,",
                    "  cve_natural_bloque,",
                    "  etiqueta,",
                    "  atributo_negocio,",
                    "  tipo_dato,",
                    "  orden,",
                    "  ind_opcional)",
                    "VALUES(seq_cat_bus_detalle_bloque.NEXTVAL,?,?,?,?,?,?,?)"
   PREPARE prp_almacena_detalle_bloque FROM v_consulta

   LET v_consulta = " SELECT programa_cod,",
                    "        programa_desc",
                    "   FROM seg_programa",
                    "  WHERE modulo_cod = ?"
   --LET v_consulta = "EXECUTE FUNCTION fn_bus_recupera_programas_modulo(?)"
   PREPARE prp_recupera_program_servicio FROM v_consulta

   LET v_consulta = " SELECT id_cat_bus_negocio,",
                    "        programa,",
                    "        desc_programa",
                    "   FROM cat_bus_negocio"
   PREPARE prp_recupera_programa_negocio FROM v_consulta  

   LET v_consulta = " SELECT tipo_dato,",
                    "        descripcion",
                    "   FROM cat_bus_tipo_dato",
                    "  WHERE 1 = 1"
   PREPARE prp_recupera_tipo_dato FROM v_consulta 

   LET v_consulta = " SELECT cod_rechazo,",
                    "        desc_rechazo",
                    "   FROM cat_bus_rechazo"
   PREPARE prp_recupera_rechazo FROM v_consulta
   
   LET v_consulta = "INSERT INTO cat_bus_proceso",
                    "(id_cat_bus_proceso,",
                    " cod_proceso_bus,",
                    " desc_proceso_bus,",
                    " modulo_cod,",
                    " f_actualiza,",
                    " usuario)",
                    "VALUES(?,?,?,?,?,?)"
   PREPARE prp_almacena_cat_bus_proceso FROM v_consulta

   LET v_consulta = "INSERT INTO cat_bus_operacion",
                    "(id_cat_bus_operacion,",
                    " id_cat_bus_proceso,",
                    " cod_opera_bus,",
                    " desc_opera_bus,",
                    " f_actualiza,",
                    " usuario)",
                    "VALUES(?,?,?,?,?,?)"
   PREPARE prp_almacena_cat_bus_operacion FROM v_consulta

   LET v_consulta = "INSERT INTO cat_bus_cza_operacion",
                    "(id_cat_bus_operacion,",
                    " id_sistema,",
                    " id_ebusiness,",
                    " id_portafolio,",
                    " id_servicio,",
                    " id_cliente,",
                    " id_canal,",
                    " url_servicio,",
                    " num_reintento,",
                    " intervalo,",
                    " id_tipo_contrato,",
                    " programa_servicio)",
                    "VALUES(?,?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE prp_almacena_cat_bus_cza_operacion FROM v_consulta

   LET v_consulta = "INSERT INTO cat_bus_contrato(id_cat_bus_contrato,",
                    " id_cat_bus_operacion,",
                    " cod_contrato,",
                    " desc_contrato,",
                    " ind_vigencia,",
                    " f_ini_vigencia,",
                    " f_fin_vigencia,",
                    " id_cat_bus_negocio,",
                    " entidad_negocio)",
                    "VALUES (?,?,?,?,?,?,?,?,?)"
   PREPARE prp_almacena_cat_bus_contrato FROM v_consulta

   LET v_consulta = " SELECT NVL(1,0)",
                    "   FROM cat_bus_contrato",
                    "  WHERE id_cat_bus_operacion = ?",
                    "    AND cod_contrato = ?"
   PREPARE prp_existe_cod_contrato FROM v_consulta

   {LET v_consulta = " SELECT NVL(1,0)",
                    "   FROM cat_bus_operacion",
                    "  WHERE id_cat_bus_proceso = ?",
                    "    AND cod_opera_bus = ?"}

   LET v_consulta = " SELECT FIRST 1 NVL(1,0)",
                    "   FROM cat_bus_proceso pro JOIN cat_bus_operacion ope",
                    "     ON ope.id_cat_bus_proceso = pro.id_cat_bus_proceso",
                    "  WHERE pro.cod_proceso_bus = ?",
                    "    AND cod_opera_bus = ?"
   PREPARE prp_existe_cod_operacion FROM v_consulta

   LET v_consulta = " SELECT NVL(1,0)",
                    "   FROM cat_bus_proceso",
                    "  WHERE modulo_cod = ?",
                    "    AND cod_proceso_bus = ?"
   PREPARE prp_existe_cod_proceso FROM v_consulta

   LET v_consulta = " UPDATE cat_bus_contrato",
                    "    SET ind_vigencia = 0",# Incativo
                    "  WHERE id_cat_bus_operacion = ?",
                    "    AND id_cat_bus_contrato  = ?"
   PREPARE prp_actualiza_baja_contrato FROM v_consulta

   LET v_consulta = " UPDATE cat_bus_cza_operacion",
                    "    SET id_sistema    = ?,",
                    "        id_ebusiness  = ?,",
                    "        id_portafolio = ?,",
                    "        id_servicio   = ?,",
                    "        id_cliente    = ?,",
                    "        id_canal      = ?,",
                    "        url_servicio  = ?,",
                    "        num_reintento = ?,",
                    "        intervalo     = ?,",
                    "        id_tipo_contrato  = ?,",
                    "        programa_servicio = ?",
                    "  WHERE id_cat_bus_operacion = ?"
   PREPARE prp_modifica_cza_operacion FROM v_consulta

   LET v_consulta = " UPDATE cat_bus_operacion",
                    "    SET desc_opera_bus = ?,",
                    "        f_actualiza    = ?,",
                    "        usuario        = ?",
                    "  WHERE id_cat_bus_proceso   = ?",
                    "    AND id_cat_bus_operacion = ?"
   PREPARE prp_modifica_operacion FROM v_consulta

   LET v_consulta = " UPDATE cat_bus_proceso",
                    "    SET desc_proceso_bus = ?,",
                    "        f_actualiza      = ?,",
                    "        usuario          = ?",
                    "  WHERE id_cat_bus_proceso = ?"
   PREPARE prp_modifica_proceso FROM v_consulta

   LET v_consulta = " SELECT id_cat_bus_proceso,",
                    "        cod_proceso_bus,",
                    "        desc_proceso_bus,",
                    "        modulo_cod,",
                    "        f_actualiza,",
                    "        usuario",
                    "   FROM cat_bus_proceso",
                    "  WHERE id_cat_bus_proceso = ?"
    PREPARE prp_recupera_proceso FROM v_consulta

    LET v_consulta = " SELECT id_cat_bus_proceso,",
                    "        id_cat_bus_operacion,",
                    "        cod_opera_bus,",
                    "        desc_opera_bus,",
                    "        f_actualiza,",
                    "        usuario",
                    "   FROM cat_bus_operacion",
                    "  WHERE id_cat_bus_proceso = ?",
                    "    AND id_cat_bus_operacion = ?"
   PREPARE prp_recupera_operacion FROM v_consulta

   LET v_consulta = " SELECT id_sistema,",
                    "        id_ebusiness,",
                    "        id_portafolio,",
                    "        id_servicio,",
                    "        id_cliente,",
                    "        id_canal,",
                    "        url_servicio,",
                    "        num_reintento,",
                    "        intervalo,",
                    "        id_tipo_contrato,",
                    "        programa_servicio",
                    "   FROM cat_bus_cza_operacion",
                    "  WHERE id_cat_bus_operacion = ?"
    PREPARE prp_recupera_cza_operacion FROM v_consulta 
   
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para elegir modulo                               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_elige_modulo()
DEFINE v_modulo    CHAR(3),
       v_cb_modulo ui.ComboBox,
       r_arbol_modulo  DYNAMIC ARRAY OF RECORD        
          v_descripcion          CHAR(40),--LIKE seg_modulo.modulo_desc,
          v_padre                LIKE cat_bus_proceso.desc_proceso_bus,
          v_identificador        LIKE cat_bus_proceso.desc_proceso_bus,
          v_extendido            SMALLINT,
          v_id_cat_bus_proceso   LIKE cat_bus_proceso.id_cat_bus_proceso,
          v_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
          v_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato
       END RECORD,
       r_datos_proceso RECORD
         v_id_cat_bus_proceso LIKE cat_bus_proceso.id_cat_bus_proceso,
         v_cod_proceso        LIKE cat_bus_proceso.cod_proceso_bus,
         v_desc_proceso       LIKE cat_bus_proceso.desc_proceso_bus,
         v_modulo_cod         LIKE cat_bus_proceso.modulo_cod,
         v_f_actualiza        LIKE cat_bus_proceso.f_actualiza,
         v_usuario            LIKE cat_bus_proceso.usuario
       END RECORD,
       r_datos_operacion RECORD
         v_id_cat_bus_proceso   LIKE cat_bus_operacion.id_cat_bus_proceso,
         v_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
         v_cod_opera_bus        LIKE cat_bus_operacion.cod_opera_bus,
         v_desc_opera_bus       LIKE cat_bus_operacion.desc_opera_bus,
         v_f_actualiza          LIKE cat_bus_operacion.f_actualiza,
         v_usuario              LIKE cat_bus_operacion.usuario
       END RECORD,
       r_cabeza_operacion RECORD
         v_id_sistema        LIKE cat_bus_cza_operacion.id_sistema,
         v_id_business       LIKE cat_bus_cza_operacion.id_ebusiness,
         v_id_portafolio     LIKE cat_bus_cza_operacion.id_portafolio,
         v_id_servicio       LIKE cat_bus_cza_operacion.id_servicio,
         v_id_cliente        LIKE cat_bus_cza_operacion.id_cliente,
         v_id_canal          LIKE cat_bus_cza_operacion.id_canal,
         v_url_servicio      LIKE cat_bus_cza_operacion.url_servicio,
         v_num_reintento     LIKE cat_bus_cza_operacion.num_reintento,
         v_intervalo         LIKE cat_bus_cza_operacion.intervalo,
         v_id_tipo_contrato  LIKE cat_bus_cza_operacion.id_tipo_contrato,
         v_programa_servicio LIKE cat_bus_cza_operacion.programa_servicio
       END RECORD,
       r_datos_contrato RECORD
         v_id_cat_bus_operacion LIKE cat_bus_contrato.id_cat_bus_operacion,
         v_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_cod_contrato         LIKE cat_bus_contrato.cod_contrato,
         v_desc_contrato        LIKE cat_bus_contrato.desc_contrato,
         v_ind_vigencia         LIKE cat_bus_contrato.ind_vigencia,
         v_f_ini_vigencia       LIKE cat_bus_contrato.f_ini_vigencia,
         v_f_fin_vigencia       LIKE cat_bus_contrato.f_fin_vigencia,
         v_id_cat_bus_negocio   LIKE cat_bus_contrato.id_cat_bus_negocio,
         v_programa             LIKE cat_bus_negocio.programa,
         v_entidad_negocio      LIKE cat_bus_contrato.entidad_negocio
       END RECORD,
       r_agrega_detalle_contrato DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_seleccionado               BOOLEAN,
         v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
         v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
         v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
         v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                      LIKE cat_bus_detalle_contrato.orden,
         v_sentido                    LIKE cat_bus_detalle_contrato.ind_sentido,
         v_opcional                    LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD,
       # bloque para datos de tipo arreglo
       r_bloque RECORD
	     v_id_cat_bus_entidad           LIKE cat_bus_bloque.id_cat_bus_bloque,
		 v_id_cat_bus_detalle_contrato LIKE cat_bus_bloque.id_cat_bus_detalle_contrato,
	     v_cve_natural                 LIKE cat_bus_bloque.cve_natural,
		 v_desc_bloque                 LIKE cat_bus_bloque.desc_bloque,
		 v_ent_negocio_bloque          LIKE cat_bus_bloque.ent_negocio_bloque	   
	   END RECORD,
       r_detalle_bloque DYNAMIC ARRAY OF RECORD
	      v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
		  v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
          v_seleccionado               BOOLEAN,
		  v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
		  v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
		  v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
		  v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
		  v_orden                      LIKE cat_bus_detalle_contrato.orden,
          v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
	   END RECORD,
       v_indice           SMALLINT,
       v_actualiza_arbol  BOOLEAN,
       v_cb_procedimiento ui.ComboBox,
       v_cb_programa_servicio ui.ComboBox

   OPEN WINDOW vtna_mantenimiento_bus WITH FORM v_ruta_ejecutables CLIPPED||"/PRTM031"
          
      DIALOG ATTRIBUTES(UNBUFFERED)

         INPUT v_modulo FROM modulo_cod
            BEFORE INPUT
               
            ON CHANGE modulo_cod               
               CALL GET_FLDBUF(modulo_cod) RETURNING v_modulo
               CALL r_arbol_modulo.clear()
               CALL fn_recupera_arbol_modulo(v_modulo) RETURNING r_arbol_modulo
               CALL DIALOG.setActionActive("agregar",TRUE)
               CALL DIALOG.setActionActive("modificar",TRUE)
               LET v_cb_programa_servicio = ui.ComboBox.forName("formonly.programa_servicio")
               CALL fn_llena_cb_programa_servicio(v_cb_programa_servicio,v_modulo)

         END INPUT

         DISPLAY ARRAY r_arbol_modulo TO sr_arbol.*

            BEFORE ROW
               # recupera indice actual, para identificar el registro actual
               LET v_indice = ARR_CURR()
               CASE
                  # Consulta proceso
                  WHEN r_arbol_modulo[v_indice].v_id_cat_bus_proceso IS NOT NULL AND r_arbol_modulo[v_indice].v_id_cat_bus_operacion IS NULL
                     
                     # activa la pantalla de consulta
                     CALL v_forma.setElementHidden("gpo_proceso",FALSE)
                     CALL v_forma.setElementHidden("gpo_operacion",TRUE)
                     CALL v_forma.setElementHidden("gpo_encabezado_ws",TRUE)
                     CALL v_forma.setElementHidden("gpo_contrato",TRUE)
                     CALL v_forma.setElementHidden("gpo_detalle_contrato",TRUE)
                     
                     # Consulta proceso
                     CALL fn_recupera_proceso(r_arbol_modulo[v_indice].v_id_cat_bus_proceso) RETURNING r_datos_proceso.*
                     # imprime los datos correspondientes al proceso
                     DISPLAY r_datos_proceso.v_cod_proceso  TO cod_proceso_bus
                     DISPLAY r_datos_proceso.v_desc_proceso TO desc_proceso_bus

                  # Consulta operación
                  WHEN r_arbol_modulo[v_indice].v_id_cat_bus_operacion IS NOT NULL AND r_arbol_modulo[v_indice].v_id_cat_bus_contrato IS NULL
                     
                     # activa la pantalla de consulta
                     CALL v_forma.setElementHidden("gpo_proceso",FALSE)
                     CALL v_forma.setElementHidden("gpo_operacion",FALSE)
                     CALL v_forma.setElementHidden("gpo_encabezado_ws",FALSE)
                     CALL v_forma.setElementHidden("gpo_contrato",TRUE)
                     CALL v_forma.setElementHidden("gpo_detalle_contrato",TRUE)
                     
                     # Consulta proceso
                     CALL fn_recupera_proceso(r_arbol_modulo[v_indice].v_id_cat_bus_proceso) RETURNING r_datos_proceso.*
                     # imprime los datos correspondientes al proceso
                     DISPLAY r_datos_proceso.v_cod_proceso  TO cod_proceso_bus
                     DISPLAY r_datos_proceso.v_desc_proceso TO desc_proceso_bus

                     # Recupera datos de la operación
                     CALL fn_recupera_operacion(r_arbol_modulo[v_indice].v_id_cat_bus_proceso,
                                                r_arbol_modulo[v_indice].v_id_cat_bus_operacion) RETURNING r_datos_operacion.*

                     # Recupera encabezado de operación
                     CALL fn_recupera_cza_operacion(r_arbol_modulo[v_indice].v_id_cat_bus_operacion) RETURNING r_cabeza_operacion.*

                     DISPLAY r_cabeza_operacion.v_id_sistema        TO id_sistema   
                     DISPLAY r_cabeza_operacion.v_id_business       TO id_business 
                     DISPLAY r_cabeza_operacion.v_id_portafolio     TO id_portafolio
                     DISPLAY r_cabeza_operacion.v_id_servicio       TO id_servicio
                     DISPLAY r_cabeza_operacion.v_id_cliente        TO id_cliente
                     DISPLAY r_cabeza_operacion.v_id_canal          TO id_canal
                     DISPLAY r_cabeza_operacion.v_url_servicio      TO url_servicio
                     DISPLAY r_cabeza_operacion.v_num_reintento     TO num_reintento
                     DISPLAY r_cabeza_operacion.v_intervalo         TO intervalo
                     DISPLAY r_cabeza_operacion.v_id_tipo_contrato  TO tipo_contrato
                     DISPLAY r_cabeza_operacion.v_programa_servicio TO programa_servicio
                     
                     # imprime los datos correspondientes a la operación 
                     DISPLAY r_datos_operacion.v_cod_opera_bus  TO cod_opera_bus
                     DISPLAY r_datos_operacion.v_desc_opera_bus TO desc_opera_bus
                     # 1 = Genérico, 2 = Iniciador
                     IF(r_cabeza_operacion.v_id_tipo_contrato = 1)THEN
                        CALL v_forma.setFieldHidden("programa_servicio",TRUE)
                        CALL v_forma.setElementHidden("programa",TRUE)
                     ELSE
                        CALL v_forma.setFieldHidden("programa_servicio",FALSE)
                        CALL v_forma.setElementHidden("programa",FALSE)
                        
                     END IF
                     
                     
                  # Consulta contrato
                  WHEN r_arbol_modulo[v_indice].v_id_cat_bus_contrato IS NOT NULL
                     
                     # activa la pantalla de consulta
                     CALL v_forma.setElementHidden("gpo_proceso",FALSE)
                     CALL v_forma.setElementHidden("gpo_operacion",FALSE)
                     CALL v_forma.setElementHidden("gpo_encabezado_ws",FALSE)
                     CALL v_forma.setElementHidden("gpo_contrato",FALSE)
                     CALL v_forma.setElementHidden("gpo_detalle_contrato",FALSE)
                     
                     # Recuperamos propiedades del combo programa
                     LET v_cb_procedimiento = ui.ComboBox.forName("formonly.programa")
                     # llenamos el combo programa
                     CALL fn_llena_cb_programa_negocio(v_cb_procedimiento)
                     # Consulta proceso
                     CALL fn_recupera_proceso(r_arbol_modulo[v_indice].v_id_cat_bus_proceso) RETURNING r_datos_proceso.*
                     # imprime los datos correspondientes al proceso
                     DISPLAY r_datos_proceso.v_cod_proceso  TO cod_proceso_bus
                     DISPLAY r_datos_proceso.v_desc_proceso TO desc_proceso_bus
                     # Recupera datos de la operacion
                     CALL fn_recupera_operacion(r_arbol_modulo[v_indice].v_id_cat_bus_proceso,
                                                r_arbol_modulo[v_indice].v_id_cat_bus_operacion) RETURNING r_datos_operacion.*
                     # Recupera encabezado de operación
                     CALL fn_recupera_cza_operacion(r_arbol_modulo[v_indice].v_id_cat_bus_operacion) RETURNING r_cabeza_operacion.*
                     DISPLAY r_cabeza_operacion.v_id_sistema        TO id_sistema   
                     DISPLAY r_cabeza_operacion.v_id_business       TO id_business 
                     DISPLAY r_cabeza_operacion.v_id_portafolio     TO id_portafolio
                     DISPLAY r_cabeza_operacion.v_id_servicio       TO id_servicio
                     DISPLAY r_cabeza_operacion.v_id_cliente        TO id_cliente
                     DISPLAY r_cabeza_operacion.v_id_canal          TO id_canal
                     DISPLAY r_cabeza_operacion.v_url_servicio      TO url_servicio
                     DISPLAY r_cabeza_operacion.v_num_reintento     TO num_reintento
                     DISPLAY r_cabeza_operacion.v_intervalo         TO intervalo
                     DISPLAY r_cabeza_operacion.v_id_tipo_contrato  TO tipo_contrato
                     DISPLAY r_cabeza_operacion.v_programa_servicio TO programa_servicio
                     # imprime los datos correspondientes a la operación 
                     DISPLAY r_datos_operacion.v_cod_opera_bus  TO cod_opera_bus
                     DISPLAY r_datos_operacion.v_desc_opera_bus TO desc_opera_bus
                     # Consulta contrato                     
                     CALL fn_recupera_contrato(r_arbol_modulo[v_indice].v_id_cat_bus_operacion,
                                               r_arbol_modulo[v_indice].v_id_cat_bus_contrato) RETURNING r_datos_contrato.*, 
                                                                                                         r_agrega_detalle_contrato
                     DISPLAY r_datos_contrato.v_cod_contrato       TO cod_contrato_bus
                     DISPLAY r_datos_contrato.v_desc_contrato      TO desc_contrato
                     DISPLAY r_datos_contrato.v_f_ini_vigencia     TO f_ini_vigencia
                     DISPLAY r_datos_contrato.v_f_fin_vigencia     TO f_fin_vigencia
                     DISPLAY r_datos_contrato.v_id_cat_bus_negocio TO programa
                     DISPLAY r_datos_contrato.v_entidad_negocio    TO entidad_negocio
                    
                     LET v_tam_arreglo  = r_agrega_detalle_contrato.getLength()
                     LET v_cb_orden     = ui.ComboBox.forName("formonly.orden")      
                     # Lllena combo orden para desplegar los datos recuperados
                     CALL fn_llena_cb_orden(v_cb_orden,v_tam_arreglo,r_agrega_detalle_contrato)
                     LET v_cb_tipo_dato = ui.ComboBox.forName("formonly.tipo_dato")
                     # Llena combo tipo_dato para desplegar los datos recuperados
                     CALL fn_llena_cb_tipo_dato(v_cb_tipo_dato) 
                     # 1 = Genérico, 2 = Iniciador
                     IF(r_cabeza_operacion.v_id_tipo_contrato = 1)THEN
                        CALL v_forma.setFieldHidden("programa_servicio",TRUE)
                        CALL v_forma.setFieldHidden("ind_sentido",TRUE)
                        CALL v_forma.setFieldHidden("ind_opcional",FALSE)
                        CALL v_forma.setElementHidden("programa",TRUE)
                     ELSE
                        CALL v_forma.setFieldHidden("programa_servicio",FALSE)
                        CALL v_forma.setFieldHidden("ind_sentido",FALSE)
                        CALL v_forma.setFieldHidden("ind_opcional",TRUE)
                        CALL v_forma.setElementHidden("programa",FALSE)
                     END IF
                                          
               END CASE

         END DISPLAY 

         # Desplega los datos del detalle de contrato recuperado
         DISPLAY ARRAY r_agrega_detalle_contrato TO sr_detalle_contrato.*
            BEFORE ROW
               IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND
                  r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato = 'A')THEN # Tipo arreglo
                  CALL DIALOG.setActionActive("btn_bloque_arreglo",TRUE)                  
               ELSE
                  CALL DIALOG.setActionActive("btn_bloque_arreglo",FALSE)
               END IF

            ON ACTION btn_bloque_arreglo
               
               CALL fn_recupera_bloque(r_agrega_detalle_contrato[ARR_CURR()].v_id_cat_bus_detalle_entidad,
                                       r_agrega_detalle_contrato[ARR_CURR()].v_cve_natural) RETURNING r_bloque.*,
                                                                                                      r_detalle_bloque
               CALL fn_consulta_detalle_tipo_arreglo(r_bloque.*,
                                                     r_detalle_bloque,
                                                     r_cabeza_operacion.v_id_tipo_contrato)
               # vuelve a recuperar la referncia del form para ocultar grupos segun sean seleccionados
               LET v_ventana = ui.Window.getCurrent()
               LET v_forma = v_ventana.getForm()
               
         END DISPLAY
         
         BEFORE DIALOG
            LET v_actualiza_arbol = FALSE
            LET v_cb_modulo = ui.ComboBox.forName("formonly.modulo_cod")
            # Llena combo de módulo
            CALL fn_llena_cb_modulos(v_cb_modulo)
               
            LET v_ventana = ui.Window.getCurrent()
            IF(p_titulo_ventana IS NOT NULL)THEN
               CALL v_ventana.setText(p_titulo_ventana)
               CALL ui.Interface.setText(p_titulo_ventana)
            END IF
            LET v_forma = v_ventana.getForm()
            # oculta los grupos de proceso, operacion y contrato
            CALL v_forma.setElementHidden("gpo_proceso",TRUE)
            CALL v_forma.setElementHidden("gpo_operacion",TRUE)
            CALL v_forma.setElementHidden("gpo_contrato",TRUE)
            CALL v_forma.setElementHidden("gpo_detalle_contrato",TRUE)
            CALL v_forma.setElementHidden("gpo_consulta_proceso",TRUE)
            CALL v_forma.setElementHidden("gpo_encabezado_ws",TRUE)
            CALL v_forma.setFieldHidden("programa_servicio",TRUE)
            
            # Desactiva los botones si hasta que se elija un módulo 
            CALL DIALOG.setActionActive("agregar",FALSE)
            CALL DIALOG.setActionActive("modificar",FALSE)

         ON ACTION agregar
            # Se recupera el renglon actual del arbol
            LET v_indice = DIALOG.getCurrentRow("sr_arbol")
            IF( v_indice > 0 )THEN
               # activa la pantalla de agregar
               CALL v_forma.setElementHidden("gpo_proceso",FALSE)
               CALL v_forma.setElementHidden("gpo_operacion",FALSE)
               CALL v_forma.setElementHidden("gpo_encabezado_ws",FALSE)
               CALL v_forma.setElementHidden("gpo_contrato",FALSE)
               CALL v_forma.setElementHidden("gpo_detalle_contrato",FALSE)
               CASE
                  # agrega nuevo proceso si esta seleccionado el renglon de modulo o proceso
                  WHEN r_arbol_modulo[v_indice].v_id_cat_bus_operacion IS NULL
                     CALL fn_captura_agrega_proceso(v_modulo) RETURNING v_actualiza_arbol
                                    
                  # agrega una nueva operacion si esta ubicado en una operacion
                  WHEN r_arbol_modulo[v_indice].v_id_cat_bus_operacion IS NOT NULL AND r_arbol_modulo[v_indice].v_id_cat_bus_contrato IS NULL 
                     CALL fn_captura_agrega_operacion(v_modulo,
                                                      r_arbol_modulo[v_indice].v_id_cat_bus_proceso) RETURNING v_actualiza_arbol
                                    
                  # agrega un nuevo contrato si esta ubicado en una contrato
                  WHEN r_arbol_modulo[v_indice].v_id_cat_bus_contrato IS NOT NULL 
                                          
                     CALL fn_captura_agrega_contrato(v_modulo,
                                                     r_arbol_modulo[v_indice].v_id_cat_bus_proceso,
                                                     r_arbol_modulo[v_indice].v_id_cat_bus_operacion) RETURNING v_actualiza_arbol
                     
               END CASE
               # vuelve a recuperar la referncia del form para ocultar grupos segun sean seleccionados
               LET v_ventana = ui.Window.getCurrent()
               LET v_forma = v_ventana.getForm()
               # desactiva la pantalla de agregar
               CALL v_forma.setElementHidden("gpo_proceso",TRUE)
               CALL v_forma.setElementHidden("gpo_operacion",TRUE)
               CALL v_forma.setElementHidden("gpo_encabezado_ws",TRUE)
               CALL v_forma.setElementHidden("gpo_contrato",TRUE)
               CALL v_forma.setElementHidden("gpo_detalle_contrato",TRUE)
               # Regresa los titulos de grupos a su estado inicial
               CALL v_forma.setElementText("gpo_proceso","Proceso")
               CALL v_forma.setElementText("gpo_operacion","Operación")
               CALL v_forma.setElementText("gpo_encabezado_ws","Encabezado operación")
               CALL v_forma.setElementText("gpo_contrato","Contrato")
               CALL v_forma.setElementText("gpo_detalle_contrato","Detalle de contrato")
            ELSE
               CALL fn_mensaje("Advertencia","Seleccione un elemento de la lista","info")
            END IF
            # actualiza arbol de procesos y operacines
            IF(v_actualiza_arbol)THEN
               CALL r_arbol_modulo.clear()
               CALL fn_recupera_arbol_modulo(v_modulo) RETURNING r_arbol_modulo
               LET v_actualiza_arbol = FALSE
            END IF
            


        ON ACTION modificar
           # Se recupera el renglon actual del arbol
           LET v_indice = DIALOG.getCurrentRow("sr_arbol")
           IF( v_indice > 1 )THEN
              CASE
                 # modifica proceso              
                 WHEN r_arbol_modulo[v_indice].v_id_cat_bus_proceso IS NOT NULL AND r_arbol_modulo[v_indice].v_id_cat_bus_operacion IS NULL 
                    # se activa la pantalla de modificacion de proceso                    
                    CALL v_forma.setElementHidden("gpo_proceso",FALSE)
                    CALL v_forma.setElementHidden("gpo_consulta_proceso",FALSE)
                    # se llama la modificacion de proceso
                    CALL fn_captura_modificacion_proceso(r_arbol_modulo[v_indice].v_id_cat_bus_proceso)
                             RETURNING v_actualiza_arbol
                    # vuelve a recuperar la referncia del form para ocultar grupos segun sean seleccionados
                    LET v_ventana = ui.Window.getCurrent()
                    LET v_forma = v_ventana.getForm()
                    # se desactiva la pantalla de modificacion de proceso
                    CALL v_forma.setElementHidden("gpo_proceso",TRUE)
                    CALL v_forma.setElementHidden("gpo_consulta_proceso",TRUE)

                 # modifica operación
                 WHEN r_arbol_modulo[v_indice].v_id_cat_bus_operacion IS NOT NULL AND r_arbol_modulo[v_indice].v_id_cat_bus_contrato IS NULL 
                    # se activa la pantalla de modificacion de operacion
                    CALL v_forma.setElementHidden("gpo_proceso",FALSE)
                    CALL v_forma.setElementHidden("gpo_operacion",FALSE)
                    CALL v_forma.setElementHidden("gpo_encabezado_ws",FALSE)
                    
                    CALL fn_captura_modificacion_operacion(v_modulo,
                                                           r_arbol_modulo[v_indice].v_id_cat_bus_proceso, 
                                                           r_arbol_modulo[v_indice].v_id_cat_bus_operacion) RETURNING v_actualiza_arbol
                    # vuelve a recuperar la referncia del form para ocultar grupos segun sean seleccionados
                    LET v_ventana = ui.Window.getCurrent()
                    LET v_forma = v_ventana.getForm()
                    # se desactiva la pantalla de modificacion de operacion
                    CALL v_forma.setElementHidden("gpo_proceso",TRUE)
                    CALL v_forma.setElementHidden("gpo_operacion",TRUE)
                    CALL v_forma.setElementHidden("gpo_encabezado_ws",TRUE)
                    

                 # modifica contrato
                 WHEN r_arbol_modulo[v_indice].v_id_cat_bus_contrato IS NOT NULL
                    # se activa la pantalla de modificacion de operacion
                    CALL v_forma.setElementHidden("gpo_proceso",FALSE)
                    CALL v_forma.setElementHidden("gpo_operacion",FALSE)
                    CALL v_forma.setElementHidden("gpo_encabezado_ws",FALSE)
                    CALL v_forma.setElementHidden("gpo_contrato",FALSE)
                    CALL v_forma.setElementHidden("gpo_detalle_contrato",FALSE)
                    CALL fn_captura_modificacion_contrato(v_modulo,
                                                          r_arbol_modulo[v_indice].v_id_cat_bus_proceso,
                                                          r_arbol_modulo[v_indice].v_id_cat_bus_operacion,
                                                          r_arbol_modulo[v_indice].v_id_cat_bus_contrato) RETURNING v_actualiza_arbol
                    # vuelve a recuperar la referncia del form para ocultar grupos segun sean seleccionados
                    LET v_ventana = ui.Window.getCurrent()
                    LET v_forma = v_ventana.getForm()
                    # se desactiva la pantalla de modificacion de operacion
                    CALL v_forma.setElementHidden("gpo_proceso",TRUE)
                    CALL v_forma.setElementHidden("gpo_operacion",TRUE)
                    CALL v_forma.setElementHidden("gpo_encabezado_ws",TRUE)
                    CALL v_forma.setElementHidden("gpo_contrato",TRUE)
                    CALL v_forma.setElementHidden("gpo_detalle_contrato",TRUE)
                    
              END CASE

              # Regresa los titulos de grupos a su estado inicial
              CALL v_forma.setElementText("gpo_proceso","Proceso")
              CALL v_forma.setElementText("gpo_operacion","Operación")
              CALL v_forma.setElementText("gpo_encabezado_ws","Encabezado operación")
              CALL v_forma.setElementText("gpo_contrato","Contrato")
              CALL v_forma.setElementText("gpo_detalle_contrato","Detalle de contrato")
           ELSE 
           	  CALL fn_mensaje("Aviso","Seleccione un elemento de la lista, diferente a la raíz","info")
           END IF
           # si se realizó algun cambio, se actualiza el arbol
           IF(v_actualiza_arbol)THEN
              CALL r_arbol_modulo.clear()
              CALL fn_recupera_arbol_modulo(v_modulo) RETURNING r_arbol_modulo
              LET v_actualiza_arbol = FALSE
           END IF

        ON ACTION cancelar
           EXIT DIALOG

      END DIALOG

   CLOSE WINDOW vtna_mantenimiento_bus

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para elegir modulo                               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_llena_cb_modulos(p_cb_modulo)
DEFINE p_cb_modulo      ui.ComboBox,
       v_modulos      RECORD
        v_modulo_cod  CHAR(3),--LIKE seg_modulo.modulo_cod,
        v_modulo_desc VARCHAR(40)--LIKE seg_modulo.modulo_desc
       END RECORD,
       v_error_sql  INTEGER,
       v_error_isam INTEGER,
       v_msg_sql    CHAR(254)
       --v_consulta    STRING
       
   # se limpia el combo a desplegar
   CALL p_cb_modulo.clear()
   
   DECLARE cur_rec_modulos CURSOR FOR prp_rec_modulos
   FOREACH cur_rec_modulos INTO v_modulos.*
      
      CALL p_cb_modulo.addItem(v_modulos.v_modulo_cod, v_modulos.v_modulo_desc)
   END FOREACH
   
   FREE cur_rec_modulos
   
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para recuperar el arbol del módulo               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_recupera_arbol_modulo(p_modulo_cod)
DEFINE p_modulo_cod    CHAR(3),
       v_arbol_modulo  DYNAMIC ARRAY OF RECORD        
          v_descripcion          CHAR(40),--LIKE seg_modulo.modulo_desc,
          v_padre                LIKE cat_bus_proceso.desc_proceso_bus,
          v_identificador        LIKE cat_bus_proceso.desc_proceso_bus,
          v_extendido            SMALLINT,
          v_id_cat_bus_proceso   LIKE cat_bus_proceso.id_cat_bus_proceso,
          v_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
          v_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato
       END RECORD,
       v_modulo_desc      VARCHAR(40),
       v_arbol_modulo_aux RECORD        
        v_id_cat_bus_proceso   LIKE cat_bus_proceso.id_cat_bus_proceso,
        v_desc_proceso         LIKE cat_bus_proceso.desc_proceso_bus,
        v_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
        v_desc_opera           LIKE cat_bus_operacion.desc_opera_bus,        
        v_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato,
        v_desc_contrato        LIKE cat_bus_contrato.desc_contrato        
       END RECORD,
       v_indice           SMALLINT,
       v_desc_proceso_aux STRING,--LIKE cat_bus_proceso.desc_proceso_bus,
       v_desc_opera_aux   STRING,--LIKE cat_bus_operacion.desc_opera_bus,
       v_error_sql        INTEGER,
       v_error_isam       INTEGER,
       v_msg_sql          CHAR(254)       

   # omite el primer registro del módulo
   LET v_indice = 2
   
   LET v_desc_proceso_aux = " "
   LET v_desc_opera_aux   = " "
   
   # recupera la descripcion del modulo de safre_af@iss_tcp   
   EXECUTE prp_rec_descripcion_modulo USING p_modulo_cod
                                       INTO v_modulo_desc

   # agrega el registro del módulo y para indicar que todos decienden de él
   LET v_arbol_modulo[1].v_descripcion   = v_modulo_desc CLIPPED
   LET v_arbol_modulo[1].v_identificador = p_modulo_cod
   LET v_arbol_modulo[1].v_extendido     = 1
   # Recupera arbol
   
   DECLARE cur_rec_arbol_modulo CURSOR FOR prp_rec_arbol_modulo
   FOREACH cur_rec_arbol_modulo USING p_modulo_cod 
                                 INTO v_arbol_modulo_aux.*
      
      # agrega registro si es otro proceso
      IF(v_desc_proceso_aux <> (v_arbol_modulo_aux.v_id_cat_bus_proceso||v_arbol_modulo_aux.v_desc_proceso))THEN        
         LET v_arbol_modulo[v_indice].v_descripcion        = v_arbol_modulo_aux.v_desc_proceso
         LET v_arbol_modulo[v_indice].v_padre              = p_modulo_cod
         LET v_arbol_modulo[v_indice].v_identificador      = v_arbol_modulo_aux.v_id_cat_bus_proceso||v_arbol_modulo_aux.v_desc_proceso
         LET v_arbol_modulo[v_indice].v_extendido          = 0
         LET v_arbol_modulo[v_indice].v_id_cat_bus_proceso = v_arbol_modulo_aux.v_id_cat_bus_proceso
         LET v_indice = v_indice + 1
      END IF
      IF(v_desc_opera_aux <> (v_arbol_modulo_aux.v_id_cat_bus_operacion||v_arbol_modulo_aux.v_desc_opera))THEN
         # se agrega cada operacion
         LET v_arbol_modulo[v_indice].v_descripcion          = v_arbol_modulo_aux.v_desc_opera
         LET v_arbol_modulo[v_indice].v_identificador        = v_arbol_modulo_aux.v_id_cat_bus_operacion||v_arbol_modulo_aux.v_desc_opera
         LET v_arbol_modulo[v_indice].v_padre                = v_arbol_modulo_aux.v_id_cat_bus_proceso||v_arbol_modulo_aux.v_desc_proceso
         LET v_arbol_modulo[v_indice].v_extendido            = 0
         LET v_arbol_modulo[v_indice].v_id_cat_bus_proceso   = v_arbol_modulo_aux.v_id_cat_bus_proceso
         LET v_arbol_modulo[v_indice].v_id_cat_bus_operacion = v_arbol_modulo_aux.v_id_cat_bus_operacion
         LET v_indice = v_indice + 1
      END IF
      # Agrega cada contrato
      LET v_arbol_modulo[v_indice].v_descripcion          = v_arbol_modulo_aux.v_desc_contrato
      LET v_arbol_modulo[v_indice].v_identificador        = v_arbol_modulo_aux.v_id_cat_bus_contrato||v_arbol_modulo_aux.v_desc_contrato
      LET v_arbol_modulo[v_indice].v_padre                = v_arbol_modulo_aux.v_id_cat_bus_operacion||v_arbol_modulo_aux.v_desc_opera
      LET v_arbol_modulo[v_indice].v_extendido            = 0
      LET v_arbol_modulo[v_indice].v_id_cat_bus_proceso   = v_arbol_modulo_aux.v_id_cat_bus_proceso
      LET v_arbol_modulo[v_indice].v_id_cat_bus_operacion = v_arbol_modulo_aux.v_id_cat_bus_operacion
      LET v_arbol_modulo[v_indice].v_id_cat_bus_contrato  = v_arbol_modulo_aux.v_id_cat_bus_contrato
      LET v_indice = v_indice + 1

      # Temporales para determinar si el siguiente registro es diferente proceso u operación
      LET v_desc_proceso_aux = v_arbol_modulo_aux.v_id_cat_bus_proceso||v_arbol_modulo_aux.v_desc_proceso--v_arbol_modulo_aux.v_desc_proceso
      LET v_desc_opera_aux   = v_arbol_modulo_aux.v_id_cat_bus_operacion||v_arbol_modulo_aux.v_desc_opera--v_arbol_modulo_aux.v_desc_opera      
   END FOREACH   
   FREE cur_rec_arbol_modulo
   
   RETURN v_arbol_modulo
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para recuperar los datos del contrato            #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_recupera_contrato(p_id_cat_bus_operacion,p_id_cat_bus_contrato)
DEFINE p_id_cat_bus_operacion LIKE cat_bus_contrato.id_cat_bus_operacion,
       p_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato,
       --v_consulta  STRING,
       v_datos_contrato RECORD
         v_id_cat_bus_operacion LIKE cat_bus_contrato.id_cat_bus_operacion,
         v_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_cod_contrato         LIKE cat_bus_contrato.cod_contrato,
         v_desc_contrato        LIKE cat_bus_contrato.desc_contrato,
         v_ind_vigencia         LIKE cat_bus_contrato.ind_vigencia,
         v_f_ini_vigencia       LIKE cat_bus_contrato.f_ini_vigencia,
         v_f_fin_vigencia       LIKE cat_bus_contrato.f_fin_vigencia,
         v_id_cat_bus_negocio   LIKE cat_bus_contrato.id_cat_bus_negocio,
         v_programa             LIKE cat_bus_negocio.programa,
         v_entidad_negocio      LIKE cat_bus_contrato.entidad_negocio
       END RECORD,
       r_detalle_contrato DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_seleccionado               BOOLEAN,
         v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
         v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
         v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
         v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                      LIKE cat_bus_detalle_contrato.orden,
         v_sentido                    LIKE cat_bus_detalle_contrato.ind_sentido,
         v_opcional                    LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD

   
   EXECUTE prp_recupera_datos_contrato USING p_id_cat_bus_operacion, 
                                             p_id_cat_bus_contrato 
                                        INTO v_datos_contrato.*
                                        
   CALL fn_recupera_detalle_contrato(v_datos_contrato.v_entidad_negocio, # nombre tabla de donde se obtienen los datos
                                     p_id_cat_bus_contrato)              # id padre de tabla de detalle
                                     RETURNING r_detalle_contrato
                                                                        
   RETURN v_datos_contrato.*,
          r_detalle_contrato
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para recuperar el detalle del bloque             #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_recupera_detalle_entidad(p_entidad_negocio,
                                     p_id_entidad_padre,
                                     p_nom_cve_natural_contrato,
                                     p_nom_cve_natural_bloque)
                                     
DEFINE p_entidad_negocio          LIKE cat_bus_contrato.entidad_negocio,
       p_id_entidad_padre         LIKE cat_bus_contrato.id_cat_bus_contrato,
       p_nom_cve_natural_contrato VARCHAR(30),
       p_nom_cve_natural_bloque   VARCHAR(30),
       v_atributos_entidad RECORD
         v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_cve_natural                CHAR(50),
         v_etiqueta                   CHAR(50),
         v_atributo_negocio           CHAR(50),
         v_atributo_negocio_sys       CHAR(50),
         v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                      LIKE cat_bus_detalle_contrato.orden,
         v_orden_sys                  LIKE cat_bus_detalle_contrato.orden,
         v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD,       
       v_error_sql  INTEGER,
       v_error_isam INTEGER,
       v_msg_sql    CHAR(254),       
       v_detalle_entidad DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_seleccionado               BOOLEAN,
         v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
         v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
         v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
         v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                      LIKE cat_bus_detalle_contrato.orden,
         v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD,
       v_indice    SMALLINT

   LET v_indice = 1
   
   DECLARE cur_recuper_detalle_entidad CURSOR FOR prp_recuper_detalle_entidad
   FOREACH cur_recuper_detalle_entidad USING p_id_entidad_padre,
                                             p_entidad_negocio,
                                             p_nom_cve_natural_contrato,
                                             p_nom_cve_natural_bloque
                                        INTO v_error_sql  ,
                                             v_error_isam ,
                                             v_msg_sql    ,
                                             v_atributos_entidad.v_id_cat_bus_detalle_entidad,
                                             v_atributos_entidad.v_id_cat_bus_entidad,
                                             v_atributos_entidad.v_cve_natural,
                                             v_atributos_entidad.v_etiqueta,
                                             v_atributos_entidad.v_atributo_negocio,
                                             v_atributos_entidad.v_atributo_negocio_sys,
                                             v_atributos_entidad.v_tipo_dato,
                                             v_atributos_entidad.v_orden,
                                             v_atributos_entidad.v_orden_sys,
                                             v_atributos_entidad.v_opcional
                                              
      LET v_detalle_entidad[v_indice].v_id_cat_bus_detalle_entidad = v_atributos_entidad.v_id_cat_bus_detalle_entidad
      LET v_detalle_entidad[v_indice].v_id_cat_bus_entidad         = v_atributos_entidad.v_id_cat_bus_entidad
      LET v_detalle_entidad[v_indice].v_cve_natural                = v_atributos_entidad.v_cve_natural
      LET v_detalle_entidad[v_indice].v_etiqueta                   = v_atributos_entidad.v_etiqueta
      LET v_detalle_entidad[v_indice].v_atributo_negocio           = v_atributos_entidad.v_atributo_negocio_sys
      LET v_detalle_entidad[v_indice].v_tipo_dato                  = v_atributos_entidad.v_tipo_dato
      LET v_detalle_entidad[v_indice].v_orden                      = v_atributos_entidad.v_orden
      LET v_detalle_entidad[v_indice].v_opcional                   = v_atributos_entidad.v_opcional
      LET v_atributos_entidad.v_orden_sys = ""
      
      IF(v_atributos_entidad.v_atributo_negocio IS NOT NULL)THEN
         LET v_detalle_entidad[v_indice].v_seleccionado = 1
      ELSE
         LET v_detalle_entidad[v_indice].v_seleccionado = 0
      END IF
  
      LET v_indice = v_indice + 1
   END FOREACH

   FREE cur_recuper_detalle_entidad
   
   RETURN v_detalle_entidad
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para recuperar el detalle del contrato           #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_recupera_detalle_contrato(p_entidad_negocio,
                                      p_id_entidad_padre)
                                     
DEFINE p_entidad_negocio        LIKE cat_bus_contrato.entidad_negocio,
       p_id_entidad_padre       LIKE cat_bus_contrato.id_cat_bus_contrato,
       v_atributos_entidad RECORD
         v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_cve_natural                CHAR(50),
         v_etiqueta                   CHAR(50),
         v_atributo_negocio           CHAR(50),
         v_atributo_negocio_sys       CHAR(50),
         v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                      LIKE cat_bus_detalle_contrato.orden,
         v_orden_sys                  LIKE cat_bus_detalle_contrato.orden,
         v_sentido                    LIKE cat_bus_detalle_contrato.ind_sentido,
         v_opcional                    LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD,       
       v_error_sql  INTEGER,
       v_error_isam INTEGER,
       v_msg_sql    CHAR(254),       
       v_detalle_entidad DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_seleccionado               BOOLEAN,
         v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
         v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
         v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
         v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                      LIKE cat_bus_detalle_contrato.orden,
         v_sentido                    LIKE cat_bus_detalle_contrato.ind_sentido,
         v_opcional                    LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD,
       v_indice    SMALLINT

   LET v_indice = 1
   
   DECLARE cur_recuper_detalle_contrato CURSOR FOR prp_recuper_detalle_contrato
   FOREACH cur_recuper_detalle_contrato USING p_id_entidad_padre,
                                              p_entidad_negocio
                                         INTO v_error_sql  ,
                                              v_error_isam ,
                                              v_msg_sql    ,
                                              v_atributos_entidad.v_id_cat_bus_detalle_entidad,
                                              v_atributos_entidad.v_id_cat_bus_entidad,
                                              v_atributos_entidad.v_cve_natural,
                                              v_atributos_entidad.v_etiqueta,
                                              v_atributos_entidad.v_atributo_negocio,
                                              v_atributos_entidad.v_atributo_negocio_sys,
                                              v_atributos_entidad.v_tipo_dato,
                                              v_atributos_entidad.v_orden,
                                              v_atributos_entidad.v_orden_sys,
                                              v_atributos_entidad.v_sentido,
                                              v_atributos_entidad.v_opcional
                                              
      LET v_detalle_entidad[v_indice].v_id_cat_bus_detalle_entidad = v_atributos_entidad.v_id_cat_bus_detalle_entidad
      LET v_detalle_entidad[v_indice].v_id_cat_bus_entidad         = v_atributos_entidad.v_id_cat_bus_entidad
      LET v_detalle_entidad[v_indice].v_cve_natural                = v_atributos_entidad.v_cve_natural
      LET v_detalle_entidad[v_indice].v_etiqueta                   = v_atributos_entidad.v_etiqueta
      LET v_detalle_entidad[v_indice].v_atributo_negocio           = v_atributos_entidad.v_atributo_negocio_sys
      LET v_detalle_entidad[v_indice].v_tipo_dato                  = v_atributos_entidad.v_tipo_dato
      LET v_detalle_entidad[v_indice].v_orden                      = v_atributos_entidad.v_orden
      LET v_detalle_entidad[v_indice].v_sentido                    = v_atributos_entidad.v_sentido
      LET v_detalle_entidad[v_indice].v_opcional                   = v_atributos_entidad.v_opcional
      LET v_atributos_entidad.v_orden_sys = "" 
      
      IF(v_atributos_entidad.v_atributo_negocio IS NOT NULL)THEN
         LET v_detalle_entidad[v_indice].v_seleccionado = 1
      ELSE
         LET v_detalle_entidad[v_indice].v_seleccionado = 0
      END IF
  
      LET v_indice = v_indice + 1
   END FOREACH

   FREE cur_recuper_detalle_contrato
   
   RETURN v_detalle_entidad
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para la captura de un nuevo proceso              #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_captura_agrega_proceso(p_modulo)
DEFINE p_modulo          LIKE cat_bus_proceso.modulo_cod,
       v_agrega_contrato RECORD
         v_id_cat_bus_operacion LIKE cat_bus_contrato.id_cat_bus_operacion,
         v_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_cod_contrato         LIKE cat_bus_contrato.cod_contrato,
         v_desc_contrato        LIKE cat_bus_contrato.desc_contrato,
         v_ind_vigencia         LIKE cat_bus_contrato.ind_vigencia,
         v_f_ini_vigencia       LIKE cat_bus_contrato.f_ini_vigencia,
         v_f_fin_vigencia       LIKE cat_bus_contrato.f_fin_vigencia,
         v_id_cat_bus_negocio   LIKE cat_bus_contrato.id_cat_bus_negocio,
         v_programa             LIKE cat_bus_negocio.programa,
         v_entidad_negocio      LIKE cat_bus_contrato.entidad_negocio
       END RECORD,
       v_agrega_operacion RECORD
         v_id_cat_bus_proceso   LIKE cat_bus_operacion.id_cat_bus_proceso,
         v_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
         v_cod_opera_bus        LIKE cat_bus_operacion.cod_opera_bus,
         v_desc_opera_bus       LIKE cat_bus_operacion.desc_opera_bus,
         v_f_actualiza          LIKE cat_bus_operacion.f_actualiza,
         v_usuario              LIKE cat_bus_operacion.usuario
       END RECORD,
       v_agrega_cabeza_operacion RECORD
         v_id_sistema        LIKE cat_bus_cza_operacion.id_sistema,
         v_id_business       LIKE cat_bus_cza_operacion.id_ebusiness,
         v_id_portafolio     LIKE cat_bus_cza_operacion.id_portafolio,
         v_id_servicio       LIKE cat_bus_cza_operacion.id_servicio,
         v_id_cliente        LIKE cat_bus_cza_operacion.id_cliente,
         v_id_canal          LIKE cat_bus_cza_operacion.id_canal,
         v_url_servicio      LIKE cat_bus_cza_operacion.url_servicio,
         v_num_reintento     LIKE cat_bus_cza_operacion.num_reintento,
         v_intervalo         LIKE cat_bus_cza_operacion.intervalo,
         v_id_tipo_contrato  LIKE cat_bus_cza_operacion.id_tipo_contrato,
         v_programa_servicio LIKE cat_bus_cza_operacion.programa_servicio
       END RECORD,
       v_agrega_proceso RECORD
         v_id_cat_bus_proceso LIKE cat_bus_proceso.id_cat_bus_proceso,
         v_cod_proceso        LIKE cat_bus_proceso.cod_proceso_bus,
         v_desc_proceso       LIKE cat_bus_proceso.desc_proceso_bus,
         v_modulo_cod         LIKE cat_bus_proceso.modulo_cod,
         v_f_actualiza        LIKE cat_bus_proceso.f_actualiza,
         v_usuario            LIKE cat_bus_proceso.usuario
       END RECORD,
       r_agrega_detalle_contrato DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_detalle_entidad  LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad          LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_seleccionado                BOOLEAN,
         v_cve_natural                 LIKE cat_bus_detalle_contrato.cve_natural,
         v_etiqueta                    LIKE cat_bus_detalle_contrato.etiqueta,
         v_atributo_negocio            LIKE cat_bus_detalle_contrato.atributo_negocio,
         v_tipo_dato                   LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                       LIKE cat_bus_detalle_contrato.orden,
         v_sentido                     LIKE cat_bus_detalle_contrato.ind_sentido,
         v_opcional                    LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD,
       # bloque para datos de tipo arreglo
       v_bloque DYNAMIC ARRAY OF RECORD
	     v_id_cat_bus_entidad          LIKE cat_bus_bloque.id_cat_bus_bloque,
		 v_id_cat_bus_detalle_contrato LIKE cat_bus_bloque.id_cat_bus_detalle_contrato,
	     v_cve_natural                 LIKE cat_bus_bloque.cve_natural,
		 v_desc_bloque                 LIKE cat_bus_bloque.desc_bloque,
		 v_ent_negocio_bloque          LIKE cat_bus_bloque.ent_negocio_bloque	   
	   END RECORD,
       v_detalle_bloque DYNAMIC ARRAY OF RECORD
	      v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
		  v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
          v_seleccionado               BOOLEAN,
		  v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
		  v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
		  v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
		  v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
		  v_orden                      LIKE cat_bus_detalle_contrato.orden,
          v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
	   END RECORD,
       v_cb_procedimiento     ui.ComboBox,
       v_cb_programa_servicio ui.ComboBox,
       r_existe            BOOLEAN, # determina si existe registro
       v_error_sql         BOOLEAN,
       v_confirma          BOOLEAN,
       v_actualiza_arbol   BOOLEAN,
       v_indice            SMALLINT,
       v_tam_arreglo       SMALLINT,
       r_capturo           BOOLEAN       

   # variable para determinar si se realizaron modificaciones y es necesario actualizar arbol
   LET v_actualiza_arbol = FALSE
   DIALOG ATTRIBUTES(UNBUFFERED, FIELD ORDER FORM)
            # proceso
      INPUT v_agrega_proceso.v_cod_proceso,
            v_agrega_proceso.v_desc_proceso,
            # operación
            v_agrega_operacion.v_cod_opera_bus,
            v_agrega_operacion.v_desc_opera_bus,
            # cabeza operacion
            v_agrega_cabeza_operacion.v_id_sistema,
            v_agrega_cabeza_operacion.v_id_business,
            v_agrega_cabeza_operacion.v_id_portafolio,
            v_agrega_cabeza_operacion.v_id_servicio,
            v_agrega_cabeza_operacion.v_id_cliente,
            v_agrega_cabeza_operacion.v_id_canal,
            v_agrega_cabeza_operacion.v_url_servicio,
            v_agrega_cabeza_operacion.v_num_reintento,
            v_agrega_cabeza_operacion.v_intervalo,
            v_agrega_cabeza_operacion.v_id_tipo_contrato,
            v_agrega_cabeza_operacion.v_programa_servicio,
            # contrato
            v_agrega_contrato.v_cod_contrato,
            v_agrega_contrato.v_desc_contrato,
            v_agrega_contrato.v_f_ini_vigencia,
            v_agrega_contrato.v_f_fin_vigencia,
            v_agrega_contrato.v_id_cat_bus_negocio,
            v_agrega_contrato.v_entidad_negocio         
            # proceso
       FROM cod_proceso_bus,
            desc_proceso_bus,
            # operación
            cod_opera_bus,
            desc_opera_bus,
            # cabeza operacion
            id_sistema,
            id_business,
            id_portafolio,
            id_servicio,
            id_cliente,
            id_canal,
            url_servicio,
            num_reintento,
            intervalo,
            tipo_contrato,
            programa_servicio,
            # contrato
            cod_contrato_bus,
            desc_contrato,
            f_ini_vigencia,
            f_fin_vigencia,
            programa,
            entidad_negocio 
    
         # Proceso
         AFTER FIELD cod_proceso_bus
            # verifica que se haya capturado el código de proceso
            IF(v_agrega_proceso.v_cod_proceso IS NULL OR v_agrega_proceso.v_cod_proceso = " ")THEN
               CALL fn_mensaje("AVISO","Capture código de proceso","information")
               NEXT FIELD cod_proceso_bus
            END IF
            CALL fn_existe_cod_proceso(p_modulo,
                                       v_agrega_proceso.v_cod_proceso) RETURNING r_existe
            IF( r_existe)THEN
               CALL fn_mensaje("AVISO","Código de proceso ya existe","information")
               NEXT FIELD cod_proceso_bus
            END IF
   
         AFTER FIELD desc_proceso_bus
            # verifica que se haya capturado el código de proceso
            IF(v_agrega_proceso.v_desc_proceso IS NULL OR v_agrega_proceso.v_desc_proceso = " ")THEN
               CALL fn_mensaje("AVISO","Capture descripción de proceso","information")
               NEXT FIELD desc_proceso_bus
            END IF
   
         # Operación
         AFTER FIELD cod_opera_bus
            # verifica que se haya capturado
            IF(v_agrega_operacion.v_cod_opera_bus IS NULL OR v_agrega_operacion.v_cod_opera_bus = " ")THEN
               CALL fn_mensaje("AVISO","Capture código de operación","about")
               NEXT FIELD cod_opera_bus
            END IF

            CALL fn_existe_cod_operacion(v_agrega_proceso.v_cod_proceso, 
                                         v_agrega_operacion.v_cod_opera_bus)RETURNING r_existe

            IF( r_existe )THEN
               CALL fn_mensaje("AVISO","Código de operación ya existe","about")
               --CALL fn_mensaje("AVISO","Código de operación ya existe para el proceso","about")
               NEXT FIELD cod_opera_bus
            END IF
            
         AFTER FIELD desc_opera_bus
            # verifica que se haya capturado
            IF(v_agrega_operacion.v_desc_opera_bus IS NULL OR v_agrega_operacion.v_desc_opera_bus = " ")THEN
               CALL fn_mensaje("AVISO","Capture descripción de operación","about")
               NEXT FIELD desc_opera_bus
            END IF

         ON CHANGE tipo_contrato
            # 1 = Genérico, 2 = Iniciador
            IF(v_agrega_cabeza_operacion.v_id_tipo_contrato = 2)THEN
               CALL v_forma.setFieldHidden("programa_servicio",FALSE)
               CALL v_forma.setFieldHidden("ind_sentido",FALSE)
               CALL v_forma.setFieldHidden("ind_opcional",TRUE)
               CALL v_forma.setElementHidden("programa",FALSE)
            ELSE
               CALL v_forma.setFieldHidden("programa_servicio",TRUE)
               CALL v_forma.setFieldHidden("ind_sentido",TRUE)
               CALL v_forma.setFieldHidden("ind_opcional",FALSE)
               CALL v_forma.setElementHidden("programa",TRUE)
               INITIALIZE v_agrega_cabeza_operacion.v_programa_servicio TO NULL
            END IF
   
         # Contrato
         AFTER FIELD cod_contrato_bus
            # verifica que se haya capturado
            IF(v_agrega_contrato.v_cod_contrato IS NULL OR v_agrega_contrato.v_cod_contrato = " ")THEN
               CALL fn_mensaje("AVISO","Capture código de contrato","about")
               NEXT FIELD cod_contrato_bus
            END IF
         	
         AFTER FIELD desc_contrato
            # verifica que se haya capturado 
            IF(v_agrega_contrato.v_desc_contrato IS NULL OR v_agrega_contrato.v_desc_contrato = " ")THEN
               CALL fn_mensaje("AVISO","Capture descripción de contrato","about")
               NEXT FIELD desc_contrato
            END IF
   
         AFTER FIELD f_ini_vigencia
            # verifica que se haya capturado 
            IF(v_agrega_contrato.v_f_ini_vigencia IS NULL OR v_agrega_contrato.v_f_ini_vigencia = " ")THEN
               CALL fn_mensaje("AVISO","Capture fecha inicio de vigencia","about")
               NEXT FIELD f_ini_vigencia
            END IF
                       
         AFTER FIELD programa
            # verifica que se haya capturado 
            IF(v_agrega_contrato.v_id_cat_bus_negocio IS NULL OR v_agrega_contrato.v_id_cat_bus_negocio = " ")THEN
               CALL fn_mensaje("AVISO","Capture procedimiento","about")
               NEXT FIELD programa
            END IF
   
         AFTER FIELD entidad_negocio
            # verifica que se haya capturado 
            IF(v_agrega_contrato.v_entidad_negocio IS NULL OR v_agrega_contrato.v_entidad_negocio = " ")THEN
               CALL fn_mensaje("AVISO","Capture entidad negocio","about")
               NEXT FIELD entidad_negocio
            END IF
            CALL fn_verifica_entidad_negocio(v_agrega_contrato.v_entidad_negocio) RETURNING r_existe
            IF NOT(r_existe)THEN
               CALL fn_mensaje("AVISO","No exite entidad negocio","about")
               NEXT FIELD entidad_negocio
            END IF
   
         ON CHANGE entidad_negocio
            CALL r_agrega_detalle_contrato.clear()
            CALL fn_consulta_detalle_entidad_agrega(v_agrega_contrato.v_entidad_negocio) RETURNING r_agrega_detalle_contrato
           
      END INPUT

      INPUT ARRAY r_agrega_detalle_contrato   FROM sr_detalle_contrato.*  
      ATTRIBUTES(APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE)
         BEFORE INPUT      
            LET v_cb_orden     = ui.ComboBox.forName("formonly.orden")      
            LET v_tam_arreglo  = r_agrega_detalle_contrato.getLength()
            CALL fn_llena_cb_orden(v_cb_orden,v_tam_arreglo,r_agrega_detalle_contrato)
            LET v_cb_tipo_dato = ui.ComboBox.forName("formonly.tipo_dato")
            CALL fn_llena_cb_tipo_dato(v_cb_tipo_dato)

         BEFORE ROW
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND
               r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato = 'A')THEN # Tipo arreglo
               CALL DIALOG.setActionActive("btn_bloque_arreglo",TRUE)               
            ELSE
               CALL DIALOG.setActionActive("btn_bloque_arreglo",FALSE)
            END IF

         AFTER FIELD orden
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               (r_agrega_detalle_contrato[ARR_CURR()].v_orden IS NULL OR 
                r_agrega_detalle_contrato[ARR_CURR()].v_orden = 0))THEN
               CALL fn_mensaje("AVISO","Capture orden","information")
               NEXT FIELD orden
            END IF

         AFTER FIELD cve_natural
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               r_agrega_detalle_contrato[ARR_CURR()].v_cve_natural IS NULL)THEN
               CALL fn_mensaje("AVISO","Capture clave natural","information")
               NEXT FIELD cve_natural
            END IF

         AFTER FIELD etiqueta
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               r_agrega_detalle_contrato[ARR_CURR()].v_etiqueta IS NULL)THEN
               CALL fn_mensaje("AVISO","Capture etiqueta","information")
               NEXT FIELD etiqueta
            END IF

         AFTER FIELD tipo_dato
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato IS NULL)THEN
               CALL fn_mensaje("AVISO","Capture tipo dato","information")
               NEXT FIELD tipo_dato
            END IF

         AFTER FIELD ind_sentido            
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND ( 
               r_agrega_detalle_contrato[ARR_CURR()].v_sentido IS NULL OR
               r_agrega_detalle_contrato[ARR_CURR()].v_sentido = 0))THEN
               CALL fn_mensaje("AVISO","Capture sentido de información","information")
               NEXT FIELD ind_sentido
            END IF

         AFTER FIELD ind_opcional            
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND  
               r_agrega_detalle_contrato[ARR_CURR()].v_opcional IS NULL )THEN
               CALL fn_mensaje("AVISO","Capture tipo sentido","information")
               NEXT FIELD ind_opcional
            END IF

         # limpia registro si se des selecciona combo seleccion
         ON CHANGE seleccion
            IF NOT( r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_agrega_detalle_contrato[ARR_CURR()].v_orden       = NULL
               LET r_agrega_detalle_contrato[ARR_CURR()].v_cve_natural = NULL
               LET r_agrega_detalle_contrato[ARR_CURR()].v_etiqueta    = NULL
               LET r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato   = NULL
               LET r_agrega_detalle_contrato[ARR_CURR()].v_sentido     = 1 # ida
               LET r_agrega_detalle_contrato[ARR_CURR()].v_opcional    = 0 # requerido
            END IF
            
         ON CHANGE cve_natural
            IF NOT( r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_agrega_detalle_contrato[ARR_CURR()].v_cve_natural = NULL
               CALL fn_mensaje("AVISO","Registro no seleccionado","information")
            END IF

         ON CHANGE etiqueta
            IF NOT( r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_agrega_detalle_contrato[ARR_CURR()].v_etiqueta = NULL
               CALL fn_mensaje("AVISO","Registro no seleccionado","information")
            END IF

         ON CHANGE tipo_dato
            IF NOT( r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato = NULL
               CALL fn_mensaje("AVISO","Registro no seleccionado","information")
            END IF
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado)THEN
               IF(r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato = 'A')THEN # Arreglo
                  LET v_indice = ARR_CURR() # almacena en variable para no érder la referencia del indice, despues de llamar otra ventana
                  CALL fn_administra_captura_bloque('A', # alta
                                                    v_bloque,
                                                    v_detalle_bloque,
                                                    v_agrega_cabeza_operacion.v_id_tipo_contrato, # 1 = Genérico, 2 = Iniciador
                                                    r_agrega_detalle_contrato[v_indice].*,
                                                    v_indice) # posición (indice) actual del contrato
                                                      RETURNING r_capturo,
                                                                v_bloque,
                                                                v_detalle_bloque
                  IF NOT(r_capturo)THEN
                     INITIALIZE r_agrega_detalle_contrato[v_indice].v_tipo_dato TO NULL
                     NEXT FIELD tipo_dato
                  END IF
               ELSE
                  # elimina el registro de arreglo temporal
                  CALL fn_elimina_bloque_tmp(ARR_CURR(), # el indice del contrato se relacona al bloque
                                             v_bloque,
                                             v_detalle_bloque) RETURNING v_bloque,
                                                                         v_detalle_bloque
               END IF                
            END IF
            
         ON CHANGE orden
            IF NOT( r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_agrega_detalle_contrato[ARR_CURR()].v_orden = NULL
               CALL fn_mensaje("AVISO","Registro no seleccionado","information")
            END IF
          
            LET g_orden = v_cb_orden.getItemText(GET_FLDBUF(orden))
           
            IF( g_orden is null or g_orden = " " OR 
                g_orden = "") THEN
            ELSE    
               FOR i = 1 TO r_agrega_detalle_contrato.getLength()
                  IF i = ARR_CURR() THEN
                     CONTINUE FOR
                  END IF
                  IF g_orden = r_agrega_detalle_contrato[i].v_orden THEN
                     CALL fn_mensaje("AVISO","Posición de orden ya elegida","information")
                     LET r_agrega_detalle_contrato[ARR_CURR()].v_orden = ""                     
                     EXIT FOR                     
                  END IF
               END FOR
            END IF

         ON ACTION btn_bloque_arreglo
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato = 'A')THEN
               LET v_indice = ARR_CURR() # almacena en variable para no érder la referencia del indice, despues de llamar otra ventana
               CALL fn_administra_captura_bloque('M', # alta
                                                 v_bloque,
                                                 v_detalle_bloque,
                                                 v_agrega_cabeza_operacion.v_id_tipo_contrato, # 1 = Genérico, 2 = Iniciador
                                                 r_agrega_detalle_contrato[v_indice].*,
                                                 v_indice) # posición (indice) actual del contrato
                                                   RETURNING r_capturo,
                                                             v_bloque,
                                                             v_detalle_bloque
               IF NOT(r_capturo)THEN
                  INITIALIZE r_agrega_detalle_contrato[v_indice].v_tipo_dato TO NULL
                  NEXT FIELD tipo_dato
               END IF  
            END IF
             
      END INPUT
      
      BEFORE DIALOG
         # Cambia el titulo de los grupos para indicar que se esta agregando nuevo registro      
         CALL v_forma.setElementText("gpo_proceso","Agrega proceso")
         CALL v_forma.setElementText("gpo_operacion","Agrega operación")
         CALL v_forma.setElementText("gpo_contrato","Agrega contrato")         
         CALL v_forma.setElementText("gpo_detalle_contrato","Agrega detalle de contrato")
         CALL v_forma.setElementText("gpo_encabezado_ws","Agrega encabezado operación")
         
         # Recuperamos propiedades del combo programa
         LET v_cb_procedimiento = ui.ComboBox.forName("formonly.programa")
         # llenamos el combo programa
         CALL fn_llena_cb_programa_negocio(v_cb_procedimiento)

         CALL v_forma.setFieldHidden("programa_servicio",TRUE)
         CALL v_forma.setElementHidden("programa",TRUE)
         LET v_cb_programa_servicio = ui.ComboBox.forName("formonly.programa_servicio")
         CALL fn_llena_cb_programa_servicio(v_cb_programa_servicio,p_modulo)
         LET v_agrega_cabeza_operacion.v_id_tipo_contrato = 1 # Genérico
   
      ON ACTION aceptar            
         # verifica que se haya capturado el código de proceso
         IF(v_agrega_proceso.v_cod_proceso IS NULL OR v_agrega_proceso.v_cod_proceso = " ")THEN
            CALL fn_mensaje("AVISO","Capture código de proceso","information")
            NEXT FIELD cod_proceso_bus
         END IF
         CALL fn_existe_cod_proceso(p_modulo,
                                    v_agrega_proceso.v_cod_proceso) RETURNING r_existe
         IF( r_existe)THEN
            CALL fn_mensaje("AVISO","Código de proceso ya existe","information")
            NEXT FIELD cod_proceso_bus
         END IF
         # verifica que se haya capturado el código de proceso
         IF(v_agrega_proceso.v_desc_proceso IS NULL OR v_agrega_proceso.v_desc_proceso = " ")THEN
            CALL fn_mensaje("AVISO","Capture descripción de proceso","information")
            NEXT FIELD desc_proceso_bus
         END IF
         # verifica que se haya capturado
         IF(v_agrega_operacion.v_cod_opera_bus IS NULL OR v_agrega_operacion.v_cod_opera_bus = " ")THEN
            CALL fn_mensaje("AVISO","Capture código de operación","about")
            NEXT FIELD cod_opera_bus
         END IF
         CALL fn_existe_cod_operacion(v_agrega_proceso.v_cod_proceso, 
                                         v_agrega_operacion.v_cod_opera_bus)RETURNING r_existe

         IF( r_existe )THEN
            CALL fn_mensaje("AVISO","Código de operación ya existe","about")
            --CALL fn_mensaje("AVISO","Código de operación ya existe para el proceso","about")
            NEXT FIELD cod_opera_bus
         END IF
         # verifica que se haya capturado
         IF(v_agrega_operacion.v_desc_opera_bus IS NULL OR v_agrega_operacion.v_desc_opera_bus = " ")THEN
            CALL fn_mensaje("AVISO","Capture descripción de operación","about")
            NEXT FIELD cod_opera_bus
         END IF

         # verifica que se haya capturado
         IF(v_agrega_contrato.v_cod_contrato IS NULL OR v_agrega_contrato.v_cod_contrato = " ")THEN
            CALL fn_mensaje("AVISO","Capture código de contrato","about")
            NEXT FIELD cod_contrato_bus
         END IF
         # verifica que se haya capturado 
         IF(v_agrega_contrato.v_desc_contrato IS NULL OR v_agrega_contrato.v_desc_contrato = " ")THEN
            CALL fn_mensaje("AVISO","Capture descripción de contrato","about")
            NEXT FIELD desc_contrato
         END IF
         # verifica que se haya capturado 
         IF(v_agrega_contrato.v_f_ini_vigencia IS NULL OR v_agrega_contrato.v_f_ini_vigencia = " ")THEN
            CALL fn_mensaje("AVISO","Capture fecha inicio de vigencia","about")
            NEXT FIELD f_ini_vigencia
         END IF

         # verifica que se haya capturado 
         IF(v_agrega_contrato.v_id_cat_bus_negocio IS NULL OR v_agrega_contrato.v_id_cat_bus_negocio = " ")THEN
            CALL fn_mensaje("AVISO","Capture procedimiento","about")
            NEXT FIELD programa
         END IF
         # verifica que se haya capturado 
         IF(v_agrega_contrato.v_entidad_negocio IS NULL OR v_agrega_contrato.v_entidad_negocio = " ")THEN
            CALL fn_mensaje("AVISO","Capture entidad negocio","about")
            NEXT FIELD programa
         END IF
         # verifica si existe la entidad capturada
         CALL fn_verifica_entidad_negocio(v_agrega_contrato.v_entidad_negocio) RETURNING r_existe
         IF NOT(r_existe)THEN
            CALL fn_mensaje("AVISO","No exite entidad negocio","about")
            NEXT FIELD programa
         END IF
         
         # Valida que se hayan capturado los campos
         FOR v_indice = 1 TO r_agrega_detalle_contrato.getLength()
            # si el registro esta seleccionado
            IF(r_agrega_detalle_contrato[v_indice].v_seleccionado = TRUE)THEN 
			   IF( r_agrega_detalle_contrato[v_indice].v_orden IS NULL )THEN
                  CALL fn_mensaje("AVISO","Capture orden","about")
                  NEXT FIELD orden
               END IF 
			   
               IF( r_agrega_detalle_contrato[v_indice].v_cve_natural IS NULL )THEN
                  CALL fn_mensaje("AVISO","Capture clave natural","about")
                  NEXT FIELD cve_natural

               END IF 

               IF( r_agrega_detalle_contrato[v_indice].v_etiqueta IS NULL )THEN
                  CALL fn_mensaje("AVISO","Capture etiqueta","about")
                  NEXT FIELD etiqueta
               END IF

               IF( r_agrega_detalle_contrato[v_indice].v_tipo_dato IS NULL )THEN
                  CALL fn_mensaje("AVISO","Capture tipo dato","about")
                  NEXT FIELD tipo_dato
               END IF

               IF( v_agrega_cabeza_operacion.v_id_tipo_contrato = 2 AND # iniciador
                   r_agrega_detalle_contrato[v_indice].v_sentido = 0)THEN
                  CALL fn_mensaje("AVISO","Capture sentido de información","about")
                  NEXT FIELD ind_sentido
               END IF

               IF( v_agrega_cabeza_operacion.v_id_tipo_contrato = 1 AND # Genérico
                   r_agrega_detalle_contrato[v_indice].v_opcional IS NULL)THEN
                  CALL fn_mensaje("AVISO","Capture tipo opcional","about")
                  NEXT FIELD ind_opcional
               END IF
               
            END IF
         END FOR
         
         # confirma actualización de operacion
         CALL fn_ventana_confirma("AVISO","¿Agregar proceso?","about")RETURNING v_confirma
         IF(v_confirma)THEN
            LET v_agrega_operacion.v_f_actualiza = TODAY 
            LET v_agrega_operacion.v_usuario     = p_usuario_cod
            # se actualiza el registro
            CALL fn_agrega_proceso(p_modulo,
                                   v_agrega_proceso.*,
                                   v_agrega_operacion.*,
                                   v_agrega_cabeza_operacion.*,
                                   v_agrega_contrato.*,
                                   r_agrega_detalle_contrato,
                                   v_bloque,
                                   v_detalle_bloque)RETURNING v_error_sql
            IF(v_error_sql)THEN
               # si ha ocurrido un error
               CALL fn_mensaje("AVISO","Ocurrió un error al agregar proceso","about")
            ELSE
               CALL fn_mensaje("AVISO","Proceso agregado correctamente","about")
               LET v_actualiza_arbol = TRUE
               ACCEPT DIALOG 
            END IF
         END IF
   
      ON ACTION cancelar
         # sale de modificacion de operación
         EXIT DIALOG
   
   END DIALOG

   RETURN v_actualiza_arbol
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para llenar combo de procedimiento               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_administra_captura_bloque(p_tpo_operacion,
                                      p_bloque,
                                      p_detalle_bloque,
                                      p_tipo_contrato,
                                      p_agrega_detalle_contrato,
                                      p_indice_contrato)
DEFINE p_tpo_operacion CHAR(1), # indicador de alta o baja
       # bloque para datos de tipo arreglo
       p_bloque DYNAMIC ARRAY OF RECORD
	     v_id_cat_bus_entidad           LIKE cat_bus_bloque.id_cat_bus_bloque,
		 v_id_cat_bus_detalle_contrato LIKE cat_bus_bloque.id_cat_bus_detalle_contrato,
	     v_cve_natural                 LIKE cat_bus_bloque.cve_natural,
		 v_desc_bloque                 LIKE cat_bus_bloque.desc_bloque,
		 v_ent_negocio_bloque          LIKE cat_bus_bloque.ent_negocio_bloque	   
	   END RECORD,
       p_tipo_contrato SMALLINT, # campo ind_tipo_contrato de la cabéza de operación
       p_detalle_bloque DYNAMIC ARRAY OF RECORD
	      v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
		  v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
          v_seleccionado               BOOLEAN,
		  v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
		  v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
		  v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
		  v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
		  v_orden                      LIKE cat_bus_detalle_contrato.orden,
          v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
	   END RECORD,
       p_indice_contrato SMALLINT,
       r_bloque_tmp RECORD
	     v_id_cat_bus_entidad           LIKE cat_bus_bloque.id_cat_bus_bloque,
		 v_id_cat_bus_detalle_contrato LIKE cat_bus_bloque.id_cat_bus_detalle_contrato,
	     v_cve_natural                 LIKE cat_bus_bloque.cve_natural,
		 v_desc_bloque                 LIKE cat_bus_bloque.desc_bloque,
		 v_ent_negocio_bloque          LIKE cat_bus_bloque.ent_negocio_bloque	   
	   END RECORD,
       r_detalle_bloque_tmp DYNAMIC ARRAY OF RECORD
	      v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
		  v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
          v_seleccionado               BOOLEAN,
		  v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
		  v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
		  v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
		  v_tipo_dato                  CHAR(1),--LIKE cat_bus_detalle_bloque.tipo_dato,
		  v_orden                      SMALLINT,--LIKE cat_bus_detalle_bloque.orden
          v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
	   END RECORD,       
       p_agrega_detalle_contrato RECORD
         v_id_cat_bus_detalle_entidad  LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad          LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_seleccionado                BOOLEAN,
         v_cve_natural                 LIKE cat_bus_detalle_contrato.cve_natural,
         v_etiqueta                    LIKE cat_bus_detalle_contrato.etiqueta,
         v_atributo_negocio            LIKE cat_bus_detalle_contrato.atributo_negocio,
         v_tipo_dato                   LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                       LIKE cat_bus_detalle_contrato.orden,
         v_sentido                     LIKE cat_bus_detalle_contrato.ind_sentido,
         v_opcional                    LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD,
       v_mismo_ind          BOOLEAN,
       v_indice             SMALLINT,
       v_indice_det_bloque  SMALLINT,
       v_indice_det_tabla   SMALLINT,
       r_capturo            BOOLEAN,
       v_id_cat_bus_entidad LIKE cat_bus_bloque.id_cat_bus_bloque
       
   
   # si ya se ha capturado clave natural de bloque temporal utiliza mismo id_cat_bus_bloque
   # de lo contrario asigna numeración temporal para identificar bloque y detalle de bloque
   LET g_id_cat_bus_entidad = g_id_cat_bus_entidad + 1
   LET v_id_cat_bus_entidad = g_id_cat_bus_entidad
   CASE p_tpo_operacion
      WHEN 'A' #alta 
         LET r_bloque_tmp.v_id_cat_bus_entidad = v_id_cat_bus_entidad
         
      WHEN 'M' # modificación
         

   END CASE
   LET v_mismo_ind = FALSE
   
   FOR v_indice = 1 TO p_bloque.getLength()      
      # solo los registros del arreglo (bloque) principal que coinsidan con el registro del contrato
      IF(p_bloque[v_indice].v_id_cat_bus_detalle_contrato = p_indice_contrato)THEN
         LET r_bloque_tmp.v_desc_bloque                 = p_bloque[v_indice].v_desc_bloque
         LET r_bloque_tmp.v_cve_natural                 = p_bloque[v_indice].v_cve_natural
         LET r_bloque_tmp.v_ent_negocio_bloque          = p_bloque[v_indice].v_ent_negocio_bloque
         LET r_bloque_tmp.v_id_cat_bus_entidad          = p_bloque[v_indice].v_id_cat_bus_entidad
         LET r_bloque_tmp.v_id_cat_bus_detalle_contrato = p_indice_contrato  
         # recupera los campos de la tabla
         CALL fn_consulta_detalle_lista_entidad(p_bloque[v_indice].v_ent_negocio_bloque,
                                                p_agrega_detalle_contrato.v_cve_natural,
                                                p_bloque[v_indice].v_cve_natural) RETURNING r_detalle_bloque_tmp
         
         LET v_id_cat_bus_entidad = p_bloque[v_indice].v_id_cat_bus_entidad
         --LET r_bloque_tmp.* = p_bloque[v_indice].* # si ya existe el registro de bloque se envia a la pantalla de captura para que los muestre
         FOR v_indice_det_bloque = 1 TO p_detalle_bloque.getLength() # recorreo detalle de bloque principal
            IF(p_detalle_bloque[v_indice_det_bloque].v_id_cat_bus_entidad = v_id_cat_bus_entidad)THEN # recupera los registro del detalle del bloque si ya se han capturado
               FOR v_indice_det_tabla = 1 TO r_detalle_bloque_tmp.getLength()# agrega los datos que ya han sido capturados
                  IF(p_detalle_bloque[v_indice_det_bloque].v_atributo_negocio = r_detalle_bloque_tmp[v_indice_det_tabla].v_atributo_negocio)THEN
                     LET r_detalle_bloque_tmp[v_indice_det_tabla].v_etiqueta                   = p_detalle_bloque[v_indice_det_bloque].v_etiqueta
                     LET r_detalle_bloque_tmp[v_indice_det_tabla].v_cve_natural                = p_detalle_bloque[v_indice_det_bloque].v_cve_natural
                     LET r_detalle_bloque_tmp[v_indice_det_tabla].v_id_cat_bus_detalle_entidad = p_detalle_bloque[v_indice_det_bloque].v_id_cat_bus_detalle_entidad
                     LET r_detalle_bloque_tmp[v_indice_det_tabla].v_id_cat_bus_entidad         = p_detalle_bloque[v_indice_det_bloque].v_id_cat_bus_entidad
                     LET r_detalle_bloque_tmp[v_indice_det_tabla].v_orden                      = p_detalle_bloque[v_indice_det_bloque].v_orden
                     LET r_detalle_bloque_tmp[v_indice_det_tabla].v_seleccionado               = p_detalle_bloque[v_indice_det_bloque].v_seleccionado
                     LET r_detalle_bloque_tmp[v_indice_det_tabla].v_tipo_dato                  = p_detalle_bloque[v_indice_det_bloque].v_tipo_dato
                     LET r_detalle_bloque_tmp[v_indice_det_tabla].v_opcional                   = p_detalle_bloque[v_indice_det_bloque].v_opcional
                  
                  END IF
               END FOR
            END IF
         END FOR
         LET v_mismo_ind = TRUE 
         EXIT FOR
      END IF
   END FOR
   
   CALL fn_captura_detalle_tipo_arreglo(p_tpo_operacion, # A - Alta, M - Modificación
                                        p_agrega_detalle_contrato.v_id_cat_bus_detalle_entidad,
                                        p_agrega_detalle_contrato.v_cve_natural, 
                                        r_bloque_tmp.*,
                                        r_detalle_bloque_tmp,
                                        p_tipo_contrato) 
                                            RETURNING r_capturo,
                                                      r_bloque_tmp.*,
                                                      r_detalle_bloque_tmp
   IF(r_capturo)THEN # Determina si e realizó la captura de detalle del bloque
      IF(v_mismo_ind)THEN # si es la misma cve_natural del contrato, elimina los registros anteriores e ingresa los nuevos
         FOR v_indice = 1 TO p_bloque.getLength() # Elimina Bloque
            IF(p_bloque[v_indice].v_id_cat_bus_entidad = v_id_cat_bus_entidad)THEN
               CALL p_bloque.deleteElement(v_indice)
               EXIT FOR
            END IF
         END FOR
         FOR v_indice = 1 TO p_detalle_bloque.getLength() # Elimina detalle de bloque
            IF(p_detalle_bloque[v_indice].v_id_cat_bus_entidad = v_id_cat_bus_entidad)THEN
               CALL p_detalle_bloque.deleteElement(v_indice)
               LET v_indice = v_indice - 1
            END IF
         END FOR

         LET g_id_cat_bus_entidad = g_id_cat_bus_entidad + 1
         LET v_id_cat_bus_entidad = g_id_cat_bus_entidad
         # agrega los nuevos registros al final
         LET p_bloque[p_bloque.getLength() + 1].* = r_bloque_tmp.* # agrega registro de bloque
         LET p_bloque[p_bloque.getLength()].v_id_cat_bus_entidad          = v_id_cat_bus_entidad
         LET p_bloque[p_bloque.getLength()].v_id_cat_bus_detalle_contrato = p_indice_contrato 
         
         FOR v_indice = 1 TO r_detalle_bloque_tmp.getLength() # agrega detalle de bloque al arreglo temporal
            IF(r_detalle_bloque_tmp[v_indice].v_seleccionado)THEN # sólo almacena registros seleccionados
               LET p_detalle_bloque[p_detalle_bloque.getLength() + 1].* = r_detalle_bloque_tmp[v_indice].*
               LET p_detalle_bloque[p_detalle_bloque.getLength()].v_id_cat_bus_entidad = v_id_cat_bus_entidad # asigan identificador temporal
            END IF
         END FOR
      ELSE
      
         # agrega los nuevos registros al final
         LET p_bloque[p_bloque.getLength() + 1].*                         = r_bloque_tmp.* # agrega registro de bloque
         LET p_bloque[p_bloque.getLength()].v_id_cat_bus_detalle_contrato = p_indice_contrato 
         
         FOR v_indice = 1 TO r_detalle_bloque_tmp.getLength() # agrega detalle de bloque al arreglo temporal
            IF(r_detalle_bloque_tmp[v_indice].v_seleccionado)THEN # sólo almacena registros seleccionados
               LET p_detalle_bloque[p_detalle_bloque.getLength() + 1].* = r_detalle_bloque_tmp[v_indice].*
               LET p_detalle_bloque[p_detalle_bloque.getLength()].v_id_cat_bus_entidad = v_id_cat_bus_entidad # asigan identificador temporal
            END IF
         END FOR
      END IF
   ELSE
      # no se capturo y se eliman los registros del arreglo principal para bloque y detalle de bloque
      {FOR v_indice = 1 TO p_detalle_bloque.getLength()
         IF( p_detalle_bloque[v_indice].v_id_cat_bus_entidad = v_id_cat_bus_entidad )THEN
            CALL p_detalle_bloque.deleteElement(v_indice)
         END IF
      END FOR                   
      FOR v_indice = 1 TO p_bloque.getLength()
         IF( p_bloque[v_indice].v_id_cat_bus_detalle_contrato = p_indice_contrato )THEN
            CALL p_bloque.deleteElement(v_indice)
            EXIT FOR # sale ya que sólo es un registro de bloque por clave natural de contrato
         END IF
      END FOR}
   END IF

   RETURN r_capturo,
          p_bloque,
          p_detalle_bloque

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para eliminar los registros temporales de        #
#                     los arreglos                                             #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_elimina_bloque_tmp(p_id_contrato_tmp, p_bloque, p_detalle_bloque)
DEFINE p_id_contrato_tmp SMALLINT,
       p_bloque DYNAMIC ARRAY OF RECORD
	     v_id_cat_bus_entidad           LIKE cat_bus_bloque.id_cat_bus_bloque,
		 v_id_cat_bus_detalle_contrato LIKE cat_bus_bloque.id_cat_bus_detalle_contrato,
	     v_cve_natural                 LIKE cat_bus_bloque.cve_natural,
		 v_desc_bloque                 LIKE cat_bus_bloque.desc_bloque,
		 v_ent_negocio_bloque          LIKE cat_bus_bloque.ent_negocio_bloque	   
	   END RECORD,
       p_detalle_bloque DYNAMIC ARRAY OF RECORD
	      v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
		  v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
          v_seleccionado               BOOLEAN,
		  v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
		  v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
		  v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
		  v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
		  v_orden                      LIKE cat_bus_detalle_contrato.orden,
          v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
	   END RECORD,
       v_indice         SMALLINT,
       v_indice_detalle SMALLINT

   FOR v_indice = 1 TO p_bloque.getLength()
      IF(p_bloque[v_indice].v_cve_natural = p_id_contrato_tmp)THEN # determina el bloque a eliminar
         FOR v_indice_detalle = 1 TO p_detalle_bloque.getLength()
            IF(p_detalle_bloque[v_indice_detalle].v_id_cat_bus_entidad = p_bloque[v_indice].v_id_cat_bus_entidad)THEN # determina el detalle realcionado al bloque
               CALL p_detalle_bloque.deleteElement(v_indice_detalle) # elimina cada registro detalle del bloque
            END IF
         END FOR
         CALL p_bloque.deleteElement(v_indice) # elimina el bloque
         EXIT FOR
      END IF
   END FOR

   RETURN p_bloque,
          p_detalle_bloque
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para llenar combo de procedimiento               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_llena_cb_programa_negocio(p_cb_procedimiento)
DEFINE p_cb_procedimiento ui.ComboBox,
       v_programa_negocio RECORD
        v_id_cat_bus_negocio LIKE cat_bus_negocio.id_cat_bus_negocio,
        v_programa           LIKE cat_bus_negocio.programa,
        v_desc_programa      LIKE cat_bus_negocio.desc_programa
       END RECORD

   WHENEVER ERROR CONTINUE
   # se limpia el combo a desplegar
   CALL p_cb_procedimiento.clear()
   
   # recupera el catalogo de programas de negocio(cat_bus_negocio)
   DECLARE cur_recupera_programa_negocio CURSOR FOR prp_recupera_programa_negocio
   FOREACH cur_recupera_programa_negocio INTO v_programa_negocio.*
   
      CALL p_cb_procedimiento.addItem(v_programa_negocio.v_id_cat_bus_negocio,
                                      v_programa_negocio.v_programa CLIPPED||" - "||v_programa_negocio.v_desc_programa CLIPPED)
      
   END FOREACH
   FREE cur_recupera_programa_negocio

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para llenar combo de tipo dato                   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_llena_cb_tipo_dato(p_cb_tipo_dato)
DEFINE p_cb_tipo_dato ui.ComboBox,
       v_tipos_dato RECORD
         v_tipo_dato   LIKE cat_bus_tipo_dato.tipo_dato,
         v_descripcion LIKE cat_bus_tipo_dato.descripcion
       END RECORD

   CALL p_cb_tipo_dato.clear()
   
   DECLARE cur_recupera_tipo_dato CURSOR FOR prp_recupera_tipo_dato
   FOREACH cur_recupera_tipo_dato INTO v_tipos_dato.*
      CALL p_cb_tipo_dato.addItem(v_tipos_dato.v_tipo_dato,v_tipos_dato.v_descripcion)
   END FOREACH
   FREE cur_recupera_tipo_dato

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para llenar combo de programa servicio           #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_llena_cb_programa_servicio(p_cb_programa_servicio, p_modulo)
DEFINE p_cb_programa_servicio ui.ComboBox,
       p_modulo LIKE cat_bus_proceso.modulo_cod,
       v_programas_servicio RECORD
         v_programa_servicio LIKE seg_programa.programa_cod,
         v_descripcion       LIKE seg_programa.programa_desc
       END RECORD

   CALL p_cb_programa_servicio.clear()

   DECLARE cur_recupera_program_servicio CURSOR FOR prp_recupera_program_servicio
   FOREACH cur_recupera_program_servicio USING p_modulo
                                          INTO v_programas_servicio.*
      CALL p_cb_programa_servicio.addItem(v_programas_servicio.v_programa_servicio CLIPPED,
                                          v_programas_servicio.v_programa_servicio CLIPPED||" - "||v_programas_servicio.v_descripcion CLIPPED)
      
   END FOREACH
   FREE cur_recupera_program_servicio

END FUNCTION

--==========================================
FUNCTION fn_llena_cb_orden(p_cb_orden,p_tam_arreglo,p_agrega_detalle_contrato)
DEFINE p_cb_orden ui.ComboBox
DEFINE p_tam_arreglo SMALLINT

DEFINE p_agrega_detalle_contrato DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_detalle_entidad  LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad          LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_seleccionado                BOOLEAN,
         v_cve_natural                 LIKE cat_bus_detalle_contrato.cve_natural,
         v_etiqueta                    LIKE cat_bus_detalle_contrato.etiqueta,
         v_atributo_negocio            LIKE cat_bus_detalle_contrato.atributo_negocio,
         v_tipo_dato                   LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                       LIKE cat_bus_detalle_contrato.orden,
         v_sentido                     LIKE cat_bus_detalle_contrato.ind_sentido,
         v_opcional                    LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD

       
DEFINE i,j SMALLINT

   # se limpia el combo a desplegar
   CALL p_cb_orden.clear()
   # recupera el catalogo de programas de negocio(cat_bus_negocio)
   
   FOR i = 1 TO p_tam_arreglo
       FOR j = 1 TO p_tam_arreglo 
         IF p_agrega_detalle_contrato[j].v_orden = i THEN
            CONTINUE FOR
         ELSE
            CALL p_cb_orden.addItem(i,i)
            EXIT FOR            
         END IF
       END FOR
   END FOR   

END FUNCTION

FUNCTION fn_llena_cb_orden_bloque(p_cb_orden,p_tam_arreglo,p_agrega_detalle_bloque)
DEFINE p_cb_orden ui.ComboBox,
       p_tam_arreglo SMALLINT,
       p_agrega_detalle_bloque DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_detalle_entidad  LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad          LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_seleccionado                BOOLEAN,
         v_cve_natural                 LIKE cat_bus_detalle_contrato.cve_natural,
         v_etiqueta                    LIKE cat_bus_detalle_contrato.etiqueta,
         v_atributo_negocio            LIKE cat_bus_detalle_contrato.atributo_negocio,
         v_tipo_dato                   LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                       LIKE cat_bus_detalle_contrato.orden,
         v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD
DEFINE i,j SMALLINT

   # se limpia el combo a desplegar
   CALL p_cb_orden.clear()
   # recupera el catalogo de programas de negocio(cat_bus_negocio)
   FOR i = 1 TO p_tam_arreglo
       FOR j = 1 TO p_tam_arreglo 
         IF p_agrega_detalle_bloque[j].v_orden = i THEN
            CONTINUE FOR
         ELSE
            CALL p_cb_orden.addItem(i,i)
            EXIT FOR            
         END IF
       END FOR
   END FOR   

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para llenar combos de rechazo                    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_llena_cb_rechazo(p_cb_rechazo)
DEFINE p_cb_rechazo ui.ComboBox,
       v_rechazo RECORD
        v_cod_rechazo  LIKE cat_bus_rechazo.cod_rechazo,
        v_desc_rechazo LIKE cat_bus_rechazo.desc_rechazo
       END RECORD

   # se limpia el combo a desplegar
   CALL p_cb_rechazo.clear()
   
   # recupera el catalogo de rechazos(cat_bus_rechazo)
   DECLARE cur_recupera_rechazo CURSOR FOR prp_recupera_rechazo
   FOREACH cur_recupera_rechazo INTO v_rechazo.*
   
      CALL p_cb_rechazo.addItem(v_rechazo.v_cod_rechazo,
                                v_rechazo.v_desc_rechazo)
      
   END FOREACH
   FREE cur_recupera_rechazo

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para recueprar los atributos de la entidad       #
#                     de negocio                                               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_consulta_detalle_entidad_agrega(p_entidad_negocio)
DEFINE p_entidad_negocio LIKE cat_bus_contrato.entidad_negocio,
       v_atributos RECORD
          v_nombre_atributo VARCHAR(128,0),
          v_orden           SMALLINT
       END RECORD,
       v_agrega_detalle_contrato DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_detalle_entidad  LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad          LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_seleccionado                BOOLEAN,
         v_cve_natural                 LIKE cat_bus_detalle_contrato.cve_natural,
         v_etiqueta                    LIKE cat_bus_detalle_contrato.etiqueta,
         v_atributo_negocio            LIKE cat_bus_detalle_contrato.atributo_negocio,
         v_tipo_dato                   LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                       LIKE cat_bus_detalle_contrato.orden,
         v_sentido                     LIKE cat_bus_detalle_contrato.ind_sentido,
         v_opcional                    LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD,
       v_indice     SMALLINT,
       v_error_sql  INTEGER,
       v_error_isam INTEGER,
       v_msg_sql    CHAR(254)

   LET v_indice = 1
   
   DECLARE cur_recupera_atributos_entidad CURSOR FOR prp_recupera_atributos_entidad
   FOREACH cur_recupera_atributos_entidad USING p_entidad_negocio
                                           INTO v_error_sql,
                                                v_error_isam,
                                                v_msg_sql,
                                                v_atributos.v_nombre_atributo,
                                                v_atributos.v_orden
      LET v_agrega_detalle_contrato[v_indice].v_atributo_negocio           = v_atributos.v_nombre_atributo
      # indicializa id con indice temporal para control de datos
      LET v_agrega_detalle_contrato[v_indice].v_id_cat_bus_detalle_entidad = v_indice  
      LET v_agrega_detalle_contrato[v_indice].v_seleccionado               = FALSE
      LET v_agrega_detalle_contrato[v_indice].v_sentido                    = 1 # Ida
      LET v_agrega_detalle_contrato[v_indice].v_sentido                    = 0 # Requerido
      LET v_indice = v_indice + 1

   END FOREACH
   FREE cur_recupera_atributos_entidad

   RETURN v_agrega_detalle_contrato

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para recueprar los atributos de lista  de la     #
#                     entidad de negocio                                       #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_consulta_detalle_lista_entidad(p_entidad_negocio,
                                           p_nom_cve_natural_contrato,
                                           p_nom_cve_natural_bloque)
DEFINE p_entidad_negocio          LIKE cat_bus_contrato.entidad_negocio,
       p_nom_cve_natural_contrato LIKE cat_bus_detalle_contrato.cve_natural,
       p_nom_cve_natural_bloque   LIKE cat_bus_detalle_contrato.cve_natural,
       v_nombre_atributo          LIKE cat_bus_detalle_contrato.atributo_negocio,       
       v_agrega_detalle_contrato DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_detalle_entidad  LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad          LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_seleccionado                BOOLEAN,
         v_cve_natural                 LIKE cat_bus_detalle_contrato.cve_natural,
         v_etiqueta                    LIKE cat_bus_detalle_contrato.etiqueta,
         v_atributo_negocio            LIKE cat_bus_detalle_contrato.atributo_negocio,
         v_tipo_dato                   LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                       LIKE cat_bus_detalle_contrato.orden,
         v_opcional                    LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD,
       v_indice     SMALLINT,
       v_error_sql  INTEGER,
       v_error_isam INTEGER,
       v_msg_sql    CHAR(254)

   LET v_indice = 1
   
   DECLARE cur_recupera_atributos_lista CURSOR FOR prp_recupera_atributos_lista
   FOREACH cur_recupera_atributos_lista USING p_entidad_negocio,
                                              p_nom_cve_natural_contrato,
                                              p_nom_cve_natural_bloque
                                         INTO v_error_sql,
                                              v_error_isam,
                                              v_msg_sql,
                                              v_nombre_atributo
      LET v_agrega_detalle_contrato[v_indice].v_atributo_negocio           = v_nombre_atributo
      # indicializa id con indice temporal para control de datos
      LET v_agrega_detalle_contrato[v_indice].v_id_cat_bus_detalle_entidad = v_indice  
      LET v_agrega_detalle_contrato[v_indice].v_seleccionado               = FALSE
      LET v_indice = v_indice + 1

   END FOREACH
   FREE cur_recupera_atributos_lista

   RETURN v_agrega_detalle_contrato

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para verificar si existe entidad de negocio      #
#                     de negocio                                               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_verifica_entidad_negocio(p_entidad_negocio)
DEFINE p_entidad_negocio LIKE cat_bus_contrato.entidad_negocio,
       v_existe          BOOLEAN,
       v_tabla           SMALLINT

   LET v_existe = TRUE
   # verifica si existe la entidad   
   INITIALIZE v_tabla TO NULL
   EXECUTE prp_verifica_entidad_negocio USING p_entidad_negocio
                                         INTO v_tabla
   IF(v_tabla IS NULL)THEN
      LET v_existe = FALSE   
   END IF
   RETURN v_existe

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para almacenar los datos de un nuevo proceso     #
#                     de negocio                                               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_agrega_proceso(p_modulo,
                           p_proceso,
                           p_operacion,
                           p_cabeza_operacion,
                           p_contrato,
                           p_detalle_contrato,
                           p_bloque,
                           p_detalle_bloque)
DEFINE p_modulo   CHAR(3),--LIKE bat_modulo_serie.modulo_cod,
       p_contrato RECORD
         v_id_cat_bus_operacion LIKE cat_bus_contrato.id_cat_bus_operacion,
         v_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_cod_contrato         LIKE cat_bus_contrato.cod_contrato,
         v_desc_contrato        LIKE cat_bus_contrato.desc_contrato,
         v_ind_vigencia         LIKE cat_bus_contrato.ind_vigencia,
         v_f_ini_vigencia       LIKE cat_bus_contrato.f_ini_vigencia,
         v_f_fin_vigencia       LIKE cat_bus_contrato.f_fin_vigencia,
         v_id_cat_bus_negocio   LIKE cat_bus_contrato.id_cat_bus_negocio,
         v_programa             LIKE cat_bus_negocio.programa,
         v_entidad_negocio      LIKE cat_bus_contrato.entidad_negocio
       END RECORD,
       p_operacion RECORD
         v_id_cat_bus_proceso   LIKE cat_bus_operacion.id_cat_bus_proceso,
         v_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
         v_cod_opera_bus        LIKE cat_bus_operacion.cod_opera_bus,
         v_desc_opera_bus       LIKE cat_bus_operacion.desc_opera_bus,
         v_f_actualiza          LIKE cat_bus_operacion.f_actualiza,
         v_usuario              LIKE cat_bus_operacion.usuario
       END RECORD,
       p_cabeza_operacion RECORD
         v_id_sistema        LIKE cat_bus_cza_operacion.id_sistema,
         v_id_business       LIKE cat_bus_cza_operacion.id_ebusiness,
         v_id_portafolio     LIKE cat_bus_cza_operacion.id_portafolio,
         v_id_servicio       LIKE cat_bus_cza_operacion.id_servicio,
         v_id_cliente        LIKE cat_bus_cza_operacion.id_cliente,
         v_id_canal          LIKE cat_bus_cza_operacion.id_canal,
         v_url_servicio      LIKE cat_bus_cza_operacion.url_servicio,
         v_num_reintento     LIKE cat_bus_cza_operacion.num_reintento,
         v_intervalo         LIKE cat_bus_cza_operacion.intervalo,
         v_id_tipo_contrato  LIKE cat_bus_cza_operacion.id_tipo_contrato,
         v_programa_servicio LIKE cat_bus_cza_operacion.programa_servicio
       END RECORD,
       p_proceso RECORD
         v_id_cat_bus_proceso LIKE cat_bus_proceso.id_cat_bus_proceso,
         v_cod_proceso        LIKE cat_bus_proceso.cod_proceso_bus,
         v_desc_proceso       LIKE cat_bus_proceso.desc_proceso_bus,
         v_modulo_cod         LIKE cat_bus_proceso.modulo_cod,
         v_f_actualiza        LIKE cat_bus_proceso.f_actualiza,
         v_usuario            LIKE cat_bus_proceso.usuario
       END RECORD,
       p_detalle_contrato DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_seleccionado               BOOLEAN,
         v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
         v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
         v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
         v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                      LIKE cat_bus_detalle_contrato.orden,
         v_sentido                    LIKE cat_bus_detalle_contrato.ind_sentido,
         v_opcional                    LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD,
       # bloque para datos de tipo arreglo
       p_bloque DYNAMIC ARRAY OF RECORD
	     v_id_cat_bus_entidad          LIKE cat_bus_bloque.id_cat_bus_bloque,
		 v_id_cat_bus_detalle_contrato LIKE cat_bus_bloque.id_cat_bus_detalle_contrato,
	     v_cve_natural                 LIKE cat_bus_bloque.cve_natural,
		 v_desc_bloque                 LIKE cat_bus_bloque.desc_bloque,
		 v_ent_negocio_bloque          LIKE cat_bus_bloque.ent_negocio_bloque	   
	   END RECORD,
       p_detalle_bloque DYNAMIC ARRAY OF RECORD
	      v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
		  v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
          v_seleccionado               BOOLEAN,
		  v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
		  v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
		  v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
		  v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
		  v_orden                      LIKE cat_bus_detalle_contrato.orden,
          v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
	   END RECORD,
       v_id_cat_bus_proceso LIKE cat_bus_proceso.id_cat_bus_proceso,
       v_fecha_actual DATE,
       v_error_sql BOOLEAN
   
   LET v_error_sql =  FALSE
   LET v_id_cat_bus_proceso = 1
   # recupera la secuencia de la operación
   
   EXECUTE prp_rec_seq_cat_bus_proceso INTO v_id_cat_bus_proceso
   IF(v_id_cat_bus_proceso IS NULL)THEN
      LET v_id_cat_bus_proceso = 1
   END IF
                    
   LET v_fecha_actual = TODAY
   EXECUTE prp_almacena_cat_bus_proceso USING v_id_cat_bus_proceso,
                                              p_proceso.v_cod_proceso,
                                              p_proceso.v_desc_proceso,
                                              p_modulo,
                                              v_fecha_actual,
                                              p_usuario_cod 
      
   IF(SQLCA.SQLCODE <> 0)THEN
      # Ocurrió un error
      LET v_error_sql =  TRUE
   ELSE
      CALL fn_agrega_operacion(v_id_cat_bus_proceso,
                               p_operacion.*,
                               p_cabeza_operacion.*,
                               p_contrato.*,
                               p_detalle_contrato,
                               p_bloque,
                               p_detalle_bloque)RETURNING v_error_sql
   END IF
   
   RETURN v_error_sql
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para almacenar los datos de una nueva operación  #
#                     de negocio                                               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_agrega_operacion(p_id_cat_bus_proceso,
                             p_operacion,
                             p_cabeza_operacion,
                             p_contrato,
                             p_detalle_contrato,
                             p_bloque,
                             p_detalle_bloque)
DEFINE p_id_cat_bus_proceso LIKE cat_bus_proceso.id_cat_bus_proceso,
       p_contrato RECORD
         v_id_cat_bus_operacion LIKE cat_bus_contrato.id_cat_bus_operacion,
         v_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_cod_contrato         LIKE cat_bus_contrato.cod_contrato,
         v_desc_contrato        LIKE cat_bus_contrato.desc_contrato,
         v_ind_vigencia         LIKE cat_bus_contrato.ind_vigencia,
         v_f_ini_vigencia       LIKE cat_bus_contrato.f_ini_vigencia,
         v_f_fin_vigencia       LIKE cat_bus_contrato.f_fin_vigencia,
         v_id_cat_bus_negocio   LIKE cat_bus_contrato.id_cat_bus_negocio,
         v_programa             LIKE cat_bus_negocio.programa,
         v_entidad_negocio      LIKE cat_bus_contrato.entidad_negocio
       END RECORD,
       p_operacion RECORD
         v_id_cat_bus_proceso   LIKE cat_bus_operacion.id_cat_bus_proceso,
         v_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
         v_cod_opera_bus        LIKE cat_bus_operacion.cod_opera_bus,
         v_desc_opera_bus       LIKE cat_bus_operacion.desc_opera_bus,
         v_f_actualiza          LIKE cat_bus_operacion.f_actualiza,
         v_usuario              LIKE cat_bus_operacion.usuario
       END RECORD,
       p_cabeza_operacion RECORD
         v_id_sistema        LIKE cat_bus_cza_operacion.id_sistema,
         v_id_business       LIKE cat_bus_cza_operacion.id_ebusiness,
         v_id_portafolio     LIKE cat_bus_cza_operacion.id_portafolio,
         v_id_servicio       LIKE cat_bus_cza_operacion.id_servicio,
         v_id_cliente        LIKE cat_bus_cza_operacion.id_cliente,
         v_id_canal          LIKE cat_bus_cza_operacion.id_canal,
         v_url_servicio      LIKE cat_bus_cza_operacion.url_servicio,
         v_num_reintento     LIKE cat_bus_cza_operacion.num_reintento,
         v_intervalo         LIKE cat_bus_cza_operacion.intervalo,
         v_id_tipo_contrato  LIKE cat_bus_cza_operacion.id_tipo_contrato,
         v_programa_servicio LIKE cat_bus_cza_operacion.programa_servicio
       END RECORD,
       p_detalle_contrato DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_detalle_entidad  LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad          LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_seleccionado                BOOLEAN,
         v_cve_natural                 LIKE cat_bus_detalle_contrato.cve_natural,
         v_etiqueta                    LIKE cat_bus_detalle_contrato.etiqueta,
         v_atributo_negocio            LIKE cat_bus_detalle_contrato.atributo_negocio,
         v_tipo_dato                   LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                       LIKE cat_bus_detalle_contrato.orden,
         v_sentido                     LIKE cat_bus_detalle_contrato.ind_sentido,
         v_opcional                    LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD,
       # bloque para datos de tipo arreglo
       p_bloque DYNAMIC ARRAY OF RECORD
	     v_id_cat_bus_entidad           LIKE cat_bus_bloque.id_cat_bus_bloque,
		 v_id_cat_bus_detalle_contrato LIKE cat_bus_bloque.id_cat_bus_detalle_contrato,
	     v_cve_natural                 LIKE cat_bus_bloque.cve_natural,
		 v_desc_bloque                 LIKE cat_bus_bloque.desc_bloque,
		 v_ent_negocio_bloque          LIKE cat_bus_bloque.ent_negocio_bloque	   
	   END RECORD,
       p_detalle_bloque DYNAMIC ARRAY OF RECORD
	      v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
		  v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
          v_seleccionado               BOOLEAN,
		  v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
		  v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
		  v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
		  v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
		  v_orden                      LIKE cat_bus_detalle_contrato.orden,
          v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
	   END RECORD,
       v_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
       v_error_sql    BOOLEAN,
       v_fecha_actual DATE

   LET v_error_sql =  FALSE
   LET v_id_cat_bus_operacion = 1
   # recupera la secuencia de la operación
   
   EXECUTE prp_rec_seq_cat_bus_operacion INTO v_id_cat_bus_operacion
   IF(v_id_cat_bus_operacion IS NULL)THEN
      LET v_id_cat_bus_operacion = 1

   END IF

   LET v_fecha_actual = TODAY
   # almacena la operación
   
   EXECUTE prp_almacena_cat_bus_operacion USING v_id_cat_bus_operacion,
                                                p_id_cat_bus_proceso,
                                                p_operacion.v_cod_opera_bus,
                                                p_operacion.v_desc_opera_bus,
                                                v_fecha_actual,
                                                p_usuario_cod

   IF(SQLCA.SQLCODE <> 0)THEN
      DISPLAY "op:",SQLCA.SQLCODE
      DISPLAY "op:",SQLCA.sqlerrm
      # Ocurrió un error
      LET v_error_sql =  TRUE         
   ELSE
       
      EXECUTE prp_almacena_cat_bus_cza_operacion USING v_id_cat_bus_operacion,
                                                       p_cabeza_operacion.v_id_sistema ,
                                                       p_cabeza_operacion.v_id_business ,
                                                       p_cabeza_operacion.v_id_portafolio ,
                                                       p_cabeza_operacion.v_id_servicio  ,
                                                       p_cabeza_operacion.v_id_cliente ,
                                                       p_cabeza_operacion.v_id_canal ,
                                                       p_cabeza_operacion.v_url_servicio ,
                                                       p_cabeza_operacion.v_num_reintento ,
                                                       p_cabeza_operacion.v_intervalo,
                                                       p_cabeza_operacion.v_id_tipo_contrato,
                                                       p_cabeza_operacion.v_programa_servicio
                                                       
      IF(SQLCA.SQLCODE <> 0)THEN
         # Ocurrió un error
         LET v_error_sql =  TRUE         
      ELSE
         CALL fn_agrega_contrato(v_id_cat_bus_operacion,
                                 p_contrato.*,
                                 p_detalle_contrato,
                                 p_bloque,
                                 p_detalle_bloque) RETURNING v_error_sql
      END IF
   END IF

   RETURN v_error_sql
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para almacenar los datos de un nuevo contrato    #
#                     de negocio                                               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_agrega_contrato(p_id_cat_bus_operacion,
                            p_contrato,
                            p_detalle_contrato,
                            p_bloque,
                            p_detalle_bloque)
DEFINE p_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
       p_contrato RECORD
         v_id_cat_bus_operacion LIKE cat_bus_contrato.id_cat_bus_operacion,
         v_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_cod_contrato         LIKE cat_bus_contrato.cod_contrato,
         v_desc_contrato        LIKE cat_bus_contrato.desc_contrato,
         v_ind_vigencia         LIKE cat_bus_contrato.ind_vigencia,
         v_f_ini_vigencia       LIKE cat_bus_contrato.f_ini_vigencia,
         v_f_fin_vigencia       LIKE cat_bus_contrato.f_fin_vigencia,
         v_id_cat_bus_negocio   LIKE cat_bus_contrato.id_cat_bus_negocio,
         v_programa             LIKE cat_bus_negocio.programa,
         v_entidad_negocio      LIKE cat_bus_contrato.entidad_negocio
       END RECORD,
       p_detalle_contrato DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_detalle_entidad  LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad          LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_seleccionado                BOOLEAN,
         v_cve_natural                 LIKE cat_bus_detalle_contrato.cve_natural,
         v_etiqueta                    LIKE cat_bus_detalle_contrato.etiqueta,
         v_atributo_negocio            LIKE cat_bus_detalle_contrato.atributo_negocio,
         v_tipo_dato                   LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                       LIKE cat_bus_detalle_contrato.orden,
         v_sentido                     LIKE cat_bus_detalle_contrato.ind_sentido,
         v_opcional                    LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD,
       # bloque para datos de tipo arreglo
       p_bloque DYNAMIC ARRAY OF RECORD
	     v_id_cat_bus_entidad           LIKE cat_bus_bloque.id_cat_bus_bloque,
		 v_id_cat_bus_detalle_contrato LIKE cat_bus_bloque.id_cat_bus_detalle_contrato,
	     v_cve_natural                 LIKE cat_bus_bloque.cve_natural,
		 v_desc_bloque                 LIKE cat_bus_bloque.desc_bloque,
		 v_ent_negocio_bloque          LIKE cat_bus_bloque.ent_negocio_bloque	   
	   END RECORD,
       p_detalle_bloque DYNAMIC ARRAY OF RECORD
	      v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
		  v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
          v_seleccionado               BOOLEAN,
		  v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
		  v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
		  v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
		  v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
		  v_orden                      LIKE cat_bus_detalle_contrato.orden,
          v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
	   END RECORD,
       v_id_cat_bus_contrato         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
       v_id_cat_bus_detalle_contrato LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
       v_id_cat_bus_entidad           LIKE cat_bus_bloque.id_cat_bus_bloque,
       v_indice            SMALLINT,
       v_indice_bloque     SMALLINT,
       v_indice_det_bloque SMALLINT,
       v_error_sql         BOOLEAN

   LET v_error_sql =  FALSE
   LET v_id_cat_bus_contrato = 1
   
   EXECUTE prp_rec_seq_cat_bus_contrato INTO v_id_cat_bus_contrato
   IF(v_id_cat_bus_contrato IS NULL)THEN
      LET v_id_cat_bus_contrato = 1
   END IF 

   # se indica que el nuevo registro es vigente
   LET p_contrato.v_ind_vigencia = 1
   
   # almacena contrato
   EXECUTE prp_almacena_cat_bus_contrato USING v_id_cat_bus_contrato,
                                               p_id_cat_bus_operacion,
                                               p_contrato.v_cod_contrato,
                                               p_contrato.v_desc_contrato,
                                               p_contrato.v_ind_vigencia,
                                               p_contrato.v_f_ini_vigencia,
                                               p_contrato.v_f_fin_vigencia,
                                               p_contrato.v_id_cat_bus_negocio,
                                               p_contrato.v_entidad_negocio 

   IF(SQLCA.SQLCODE <> 0)THEN
      DISPLAY "error contrato"
      LET v_error_sql =  TRUE         
   ELSE
      # almacena detalle de contrato
      FOR v_indice = 1 TO p_detalle_contrato.getLength()
         # si el registro esta seleccionado se almacena
         IF(p_detalle_contrato[v_indice].v_seleccionado)THEN
            # genera id del registro para el detalle del contrato
            EXECUTE prp_rec_seq_cat_bus_detalle_contrato INTO v_id_cat_bus_detalle_contrato
            # almacena el detalle del contrato
            EXECUTE prp_almacena_detalle_contrato USING v_id_cat_bus_detalle_contrato,
                                                        v_id_cat_bus_contrato,
                                                        p_detalle_contrato[v_indice].v_cve_natural,
                                                        p_detalle_contrato[v_indice].v_etiqueta,
                                                        p_detalle_contrato[v_indice].v_atributo_negocio,
                                                        p_detalle_contrato[v_indice].v_tipo_dato,
                                                        p_detalle_contrato[v_indice].v_orden,
                                                        p_detalle_contrato[v_indice].v_opcional,
                                                        p_detalle_contrato[v_indice].v_sentido
            IF(SQLCA.SQLCODE <> 0)THEN
               DISPLAY "Error detalle contrato"
               DISPLAY SQLCA.SQLCODE
               DISPLAY SQLCA.sqlerrm
               LET v_error_sql =  TRUE
            END IF
            # busca datos del bloque relacionados 
            # posición contrato = v_id_cat_bus_detalle_contrato de bloque
            FOR v_indice_bloque = 1 TO p_bloque.getLength()
               IF(p_bloque[v_indice_bloque].v_id_cat_bus_detalle_contrato = v_indice)THEN
                  # generea id de bloque
                  EXECUTE prp_rec_seq_cat_bus_bloque INTO v_id_cat_bus_entidad
                  # almacena bloque 
                  EXECUTE prp_almacena_bloque_contrato USING v_id_cat_bus_entidad,
                                                             v_id_cat_bus_detalle_contrato,
                                                             p_bloque[v_indice_bloque].v_cve_natural,
                                                             p_bloque[v_indice_bloque].v_desc_bloque,
                                                             p_bloque[v_indice_bloque].v_ent_negocio_bloque
                  IF(SQLCA.SQLCODE <> 0)THEN
                     DISPLAY "Error Bloque contrato"
                     DISPLAY SQLCA.SQLCODE
                     DISPLAY SQLCA.sqlerrm
                     LET v_error_sql =  TRUE
                  END IF
                  # Almacena detalle de bloque                                           
                  FOR v_indice_det_bloque = 1 TO p_detalle_bloque.getLength()
                     # almacena los registros relacionados entre el bloque y el detalle de 
                     # bloque que coincidan con v_id_cat_bus_entidad utilizado como id temporal en la captura del arreglo 
                     IF(p_detalle_bloque[v_indice_det_bloque].v_id_cat_bus_entidad = p_bloque[v_indice_bloque].v_id_cat_bus_entidad)THEN
                        EXECUTE prp_almacena_detalle_bloque USING v_id_cat_bus_entidad,
                                                                  p_detalle_bloque[v_indice_det_bloque].v_cve_natural,
                                                                  p_detalle_bloque[v_indice_det_bloque].v_etiqueta,
                                                                  p_detalle_bloque[v_indice_det_bloque].v_atributo_negocio,
                                                                  p_detalle_bloque[v_indice_det_bloque].v_tipo_dato,
                                                                  p_detalle_bloque[v_indice_det_bloque].v_orden,
                                                                  p_detalle_bloque[v_indice_det_bloque].v_opcional
                     END IF

                  END FOR
                  IF(SQLCA.SQLCODE <> 0)THEN
                     DISPLAY "Error detalle bloque"
                     DISPLAY SQLCA.SQLCODE
                     DISPLAY SQLCA.sqlerrm
                     LET v_error_sql =  TRUE
                  END IF
               END IF
            END FOR
         END IF
      END FOR
   END IF
   
   RETURN v_error_sql
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para capturar la nueva operación de un           #
#                     determinado proceso                                      #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_captura_agrega_operacion(p_modulo,p_proceso)
DEFINE p_modulo            CHAR(3),
       p_proceso           LIKE cat_bus_proceso.id_cat_bus_proceso,
       v_agrega_contrato RECORD
         v_id_cat_bus_operacion LIKE cat_bus_contrato.id_cat_bus_operacion,
         v_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_cod_contrato         LIKE cat_bus_contrato.cod_contrato,
         v_desc_contrato        LIKE cat_bus_contrato.desc_contrato,
         v_ind_vigencia         LIKE cat_bus_contrato.ind_vigencia,
         v_f_ini_vigencia       LIKE cat_bus_contrato.f_ini_vigencia,
         v_f_fin_vigencia       LIKE cat_bus_contrato.f_fin_vigencia,
         v_id_cat_bus_negocio   LIKE cat_bus_contrato.id_cat_bus_negocio,
         v_programa             LIKE cat_bus_negocio.programa,
         v_entidad_negocio      LIKE cat_bus_contrato.entidad_negocio
       END RECORD,
       v_agrega_operacion RECORD
         v_id_cat_bus_proceso   LIKE cat_bus_operacion.id_cat_bus_proceso,
         v_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
         v_cod_opera_bus        LIKE cat_bus_operacion.cod_opera_bus,
         v_desc_opera_bus       LIKE cat_bus_operacion.desc_opera_bus,
         v_f_actualiza          LIKE cat_bus_operacion.f_actualiza,
         v_usuario              LIKE cat_bus_operacion.usuario
       END RECORD,
       v_agrega_cabeza_operacion RECORD
         v_id_sistema        LIKE cat_bus_cza_operacion.id_sistema,
         v_id_business       LIKE cat_bus_cza_operacion.id_ebusiness,
         v_id_portafolio     LIKE cat_bus_cza_operacion.id_portafolio,
         v_id_servicio       LIKE cat_bus_cza_operacion.id_servicio,
         v_id_cliente        LIKE cat_bus_cza_operacion.id_cliente,
         v_id_canal          LIKE cat_bus_cza_operacion.id_canal,
         v_url_servicio      LIKE cat_bus_cza_operacion.url_servicio,
         v_num_reintento     LIKE cat_bus_cza_operacion.num_reintento,
         v_intervalo         LIKE cat_bus_cza_operacion.intervalo,
         v_id_tipo_contrato  LIKE cat_bus_cza_operacion.id_tipo_contrato,
         v_programa_servicio LIKE cat_bus_cza_operacion.programa_servicio
       END RECORD,
       r_agrega_proceso RECORD
         v_id_cat_bus_proceso LIKE cat_bus_proceso.id_cat_bus_proceso,
         v_cod_proceso        LIKE cat_bus_proceso.cod_proceso_bus,
         v_desc_proceso       LIKE cat_bus_proceso.desc_proceso_bus,
         v_modulo_cod         LIKE cat_bus_proceso.modulo_cod,
         v_f_actualiza        LIKE cat_bus_proceso.f_actualiza,
         v_usuario            LIKE cat_bus_proceso.usuario
       END RECORD,
       r_agrega_detalle_contrato DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_seleccionado               BOOLEAN,
         v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
         v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
         v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
         v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                      LIKE cat_bus_detalle_contrato.orden,
         v_sentido                    LIKE cat_bus_detalle_contrato.ind_sentido,
         v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD,
       # bloque para datos de tipo arreglo
       v_bloque DYNAMIC ARRAY OF RECORD
	     v_id_cat_bus_entidad          LIKE cat_bus_bloque.id_cat_bus_bloque,
		 v_id_cat_bus_detalle_contrato LIKE cat_bus_bloque.id_cat_bus_detalle_contrato,
	     v_cve_natural                 LIKE cat_bus_bloque.cve_natural,
		 v_desc_bloque                 LIKE cat_bus_bloque.desc_bloque,
		 v_ent_negocio_bloque          LIKE cat_bus_bloque.ent_negocio_bloque	   
	   END RECORD,
       v_detalle_bloque DYNAMIC ARRAY OF RECORD
	      v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
		  v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
          v_seleccionado               BOOLEAN,
		  v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
		  v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
		  v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
		  v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
		  v_orden                      LIKE cat_bus_detalle_contrato.orden,
          v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
	   END RECORD,
       v_cb_procedimiento     ui.ComboBox,
       v_cb_programa_servicio ui.ComboBox,
       r_capturo           BOOLEAN,
       v_error_sql         BOOLEAN,
       r_confirma          BOOLEAN,
       r_existe            BOOLEAN,       
       v_actualiza_arbol   BOOLEAN,
       v_indice            SMALLINT

   # determina si se tiene que actualizar el arbol de registros
   LET v_actualiza_arbol = FALSE
   DIALOG ATTRIBUTES(UNBUFFERED, FIELD ORDER FORM)
            # Operación
      INPUT v_agrega_operacion.v_cod_opera_bus,
            v_agrega_operacion.v_desc_opera_bus,
            # cabeza operacion
            v_agrega_cabeza_operacion.v_id_sistema,
            v_agrega_cabeza_operacion.v_id_business,
            v_agrega_cabeza_operacion.v_id_portafolio,
            v_agrega_cabeza_operacion.v_id_servicio,
            v_agrega_cabeza_operacion.v_id_cliente,
            v_agrega_cabeza_operacion.v_id_canal,
            v_agrega_cabeza_operacion.v_url_servicio,
            v_agrega_cabeza_operacion.v_num_reintento,
            v_agrega_cabeza_operacion.v_intervalo,
            v_agrega_cabeza_operacion.v_id_tipo_contrato,
            v_agrega_cabeza_operacion.v_programa_servicio,
            # Contrato
            v_agrega_contrato.v_cod_contrato,
            v_agrega_contrato.v_desc_contrato,
            v_agrega_contrato.v_f_ini_vigencia,
            v_agrega_contrato.v_f_fin_vigencia,
            v_agrega_contrato.v_id_cat_bus_negocio,
            v_agrega_contrato.v_entidad_negocio
            # operación
       FROM cod_opera_bus,
            desc_opera_bus,
            # cabeza operacion
            id_sistema,
            id_business,
            id_portafolio,
            id_servicio,
            id_cliente,
            id_canal,
            url_servicio,
            num_reintento,
            intervalo,
            tipo_contrato,
            programa_servicio,
            # contrato
            cod_contrato_bus,
            desc_contrato,
            f_ini_vigencia,
            f_fin_vigencia,
            programa,
            entidad_negocio 
         
         # Operación
         AFTER FIELD cod_opera_bus
            # verifica que se haya capturado
            IF(v_agrega_operacion.v_cod_opera_bus IS NULL OR v_agrega_operacion.v_cod_opera_bus = " ")THEN
               CALL fn_mensaje("AVISO","Capture código de operación","about")
               NEXT FIELD cod_opera_bus
            END IF
            CALL fn_existe_cod_operacion(r_agrega_proceso.v_cod_proceso, 
                                         v_agrega_operacion.v_cod_opera_bus)RETURNING r_existe

            IF( r_existe )THEN
               CALL fn_mensaje("AVISO","Código de operación ya existe","about")
               --CALL fn_mensaje("AVISO","Código de operación ya existe para el proceso","about")
               NEXT FIELD cod_opera_bus
            END IF
   
         AFTER FIELD desc_opera_bus
            # verifica que se haya capturado
            IF(v_agrega_operacion.v_desc_opera_bus IS NULL OR v_agrega_operacion.v_desc_opera_bus = " ")THEN
               CALL fn_mensaje("AVISO","Capture descripción de operación","about")
               NEXT FIELD desc_opera_bus
            END IF

         ON CHANGE tipo_contrato
            # 1 = Genérico, 2 = Iniciador
            IF(v_agrega_cabeza_operacion.v_id_tipo_contrato = 2)THEN
               CALL v_forma.setFieldHidden("programa_servicio",FALSE)
               CALL v_forma.setFieldHidden("ind_sentido",FALSE)
               CALL v_forma.setFieldHidden("ind_opcional",TRUE)
               CALL v_forma.setElementHidden("programa",FALSE)               
            ELSE
               CALL v_forma.setFieldHidden("programa_servicio",TRUE)
               CALL v_forma.setFieldHidden("ind_sentido",TRUE)
               CALL v_forma.setFieldHidden("ind_opcional",FALSE)
               CALL v_forma.setElementHidden("programa",TRUE)
               INITIALIZE v_agrega_cabeza_operacion.v_programa_servicio TO NULL
            END IF
   
         # Contrato
         AFTER FIELD cod_contrato_bus
            # verifica que se haya capturado
            IF(v_agrega_contrato.v_cod_contrato IS NULL OR v_agrega_contrato.v_cod_contrato = " ")THEN
               CALL fn_mensaje("AVISO","Capture código de contrato","about")
               NEXT FIELD cod_contrato_bus
            END IF
         	
         AFTER FIELD desc_contrato
            # verifica que se haya capturado 
            IF(v_agrega_contrato.v_desc_contrato IS NULL OR v_agrega_contrato.v_desc_contrato = " ")THEN
               CALL fn_mensaje("AVISO","Capture descripción de contrato","about")
               NEXT FIELD desc_contrato
            END IF
   
         AFTER FIELD f_ini_vigencia
            # verifica que se haya capturado 
            IF(v_agrega_contrato.v_f_ini_vigencia IS NULL OR v_agrega_contrato.v_f_ini_vigencia = " ")THEN
               CALL fn_mensaje("AVISO","Capture fecha inicio de vigencia","about")
               NEXT FIELD f_ini_vigencia
            END IF
                        
         AFTER FIELD programa
            # verifica que se haya capturado 
            IF(v_agrega_contrato.v_id_cat_bus_negocio IS NULL OR v_agrega_contrato.v_id_cat_bus_negocio = " ")THEN
               CALL fn_mensaje("AVISO","Capture procedimiento","about")
               NEXT FIELD programa
            END IF
   
         AFTER FIELD entidad_negocio
            # verifica que se haya capturado 
            IF(v_agrega_contrato.v_entidad_negocio IS NULL OR v_agrega_contrato.v_entidad_negocio = " ")THEN
               CALL fn_mensaje("AVISO","Capture entidad negocio","about")
               NEXT FIELD entidad_negocio
            END IF
            CALL fn_verifica_entidad_negocio(v_agrega_contrato.v_entidad_negocio) RETURNING r_existe
			
            IF NOT(r_existe)THEN
               CALL fn_mensaje("AVISO","No exite entidad negocio","about")
               NEXT FIELD entidad_negocio
            END IF
            
         ON CHANGE entidad_negocio
            CALL r_agrega_detalle_contrato.clear()
            CALL fn_consulta_detalle_entidad_agrega(v_agrega_contrato.v_entidad_negocio) RETURNING r_agrega_detalle_contrato
   
      END INPUT

      INPUT ARRAY r_agrega_detalle_contrato FROM sr_detalle_contrato.* ATTRIBUTES(APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE)
         BEFORE INPUT      
            LET v_cb_orden     = ui.ComboBox.forName("formonly.orden")      
            LET v_tam_arreglo  = r_agrega_detalle_contrato.getLength()
            CALL fn_llena_cb_orden(v_cb_orden,v_tam_arreglo,r_agrega_detalle_contrato)

            LET v_cb_tipo_dato = ui.ComboBox.forName("formonly.tipo_dato")
            CALL fn_llena_cb_tipo_dato(v_cb_tipo_dato)

         BEFORE ROW
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND
               r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato = 'A')THEN # Tipo arreglo
               CALL DIALOG.setActionActive("btn_bloque_arreglo",TRUE)               
            ELSE
               CALL DIALOG.setActionActive("btn_bloque_arreglo",FALSE)
            END IF

         AFTER FIELD orden
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               (r_agrega_detalle_contrato[ARR_CURR()].v_orden IS NULL OR 
                r_agrega_detalle_contrato[ARR_CURR()].v_orden = 0))THEN
               CALL fn_mensaje("AVISO","Capture orden","information")
               NEXT FIELD orden
            END IF

         AFTER FIELD cve_natural
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               r_agrega_detalle_contrato[ARR_CURR()].v_cve_natural IS NULL)THEN
               CALL fn_mensaje("AVISO","Capture clave natural","information")
               NEXT FIELD cve_natural
            END IF

         AFTER FIELD etiqueta
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               r_agrega_detalle_contrato[ARR_CURR()].v_etiqueta IS NULL)THEN
               CALL fn_mensaje("AVISO","Capture etiqueta","information")
               NEXT FIELD etiqueta
            END IF

         AFTER FIELD tipo_dato
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato IS NULL)THEN
               CALL fn_mensaje("AVISO","Capture tipo dato","information")
               NEXT FIELD tipo_dato
            END IF

         AFTER FIELD ind_sentido            
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND ( 
               r_agrega_detalle_contrato[ARR_CURR()].v_sentido IS NULL OR
               r_agrega_detalle_contrato[ARR_CURR()].v_sentido = 0))THEN
               CALL fn_mensaje("AVISO","Capture sentido de información","information")
               NEXT FIELD ind_sentido
            END IF

         AFTER FIELD ind_opcional            
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND  
               r_agrega_detalle_contrato[ARR_CURR()].v_opcional IS NULL )THEN
               CALL fn_mensaje("AVISO","Capture tipo opcional","information")
               NEXT FIELD ind_opcional
            END IF

         # limpia registro si se des selecciona combo seleccion
         ON CHANGE seleccion
            IF NOT( r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_agrega_detalle_contrato[ARR_CURR()].v_orden       = NULL
               LET r_agrega_detalle_contrato[ARR_CURR()].v_cve_natural = NULL
               LET r_agrega_detalle_contrato[ARR_CURR()].v_etiqueta    = NULL
               LET r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato   = NULL
               LET r_agrega_detalle_contrato[ARR_CURR()].v_sentido     = 1 # ida
               LET r_agrega_detalle_contrato[ARR_CURR()].v_opcional    = 0 # Requerido
            END IF

         ON CHANGE cve_natural
            IF NOT( r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_agrega_detalle_contrato[ARR_CURR()].v_cve_natural = NULL
               CALL fn_mensaje("AVISO","Registro no seleccionado","information")
            END IF

         ON CHANGE etiqueta
            IF NOT( r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_agrega_detalle_contrato[ARR_CURR()].v_etiqueta = NULL
               CALL fn_mensaje("AVISO","Registro no seleccionado","information")
            END IF

         ON CHANGE tipo_dato
            IF NOT( r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato = NULL
               CALL fn_mensaje("AVISO","Registro no seleccionado","information")
            END IF
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado)THEN
               IF(r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato = 'A')THEN # Arreglo
                  LET v_indice = ARR_CURR() # almacena en variable para no érder la referencia del indice, despues de llamar otra ventana
                  CALL fn_administra_captura_bloque('A', # alta
                                                    v_bloque,
                                                    v_detalle_bloque,
                                                    v_agrega_cabeza_operacion.v_id_tipo_contrato, # 1 = Genérico, 2 = Iniciador
                                                    r_agrega_detalle_contrato[v_indice].*,
                                                    v_indice) # posición (indice) actual del contrato
                                                      RETURNING r_capturo,
                                                                v_bloque,
                                                                v_detalle_bloque
                  IF NOT(r_capturo)THEN
                     INITIALIZE r_agrega_detalle_contrato[v_indice].v_tipo_dato TO NULL
                     NEXT FIELD tipo_dato
                  END IF
               ELSE
                  # elimina el registro de arreglo temporal
                  CALL fn_elimina_bloque_tmp(ARR_CURR(), # el indice del contrato se relacona al bloque
                                             v_bloque,
                                             v_detalle_bloque) RETURNING v_bloque,
                                                                         v_detalle_bloque
               END IF
            END IF
             
         ON CHANGE orden
            IF NOT( r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_agrega_detalle_contrato[ARR_CURR()].v_orden = NULL
               CALL fn_mensaje("AVISO","Registro no seleccionado","information")
            END IF
            
            LET g_orden = v_cb_orden.getItemText(GET_FLDBUF(orden))
            IF (g_orden is null or g_orden = " " OR 
                g_orden = "") THEN
            ELSE    
               FOR i = 1 TO r_agrega_detalle_contrato.getLength()
                  IF i = ARR_CURR() THEN
                     CONTINUE FOR
                  END IF
                  IF g_orden = r_agrega_detalle_contrato[i].v_orden THEN
                     CALL fn_mensaje("AVISO","Posición de orden ya elegida","information")
                     LET r_agrega_detalle_contrato[ARR_CURR()].v_orden = ""                     
                     EXIT FOR                     
                  END IF
               END FOR
            END IF

         ON ACTION btn_bloque_arreglo
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato = 'A')THEN
               LET v_indice = ARR_CURR() # almacena en variable para no érder la referencia del indice, despues de llamar otra ventana
               CALL fn_administra_captura_bloque('M', # alta
                                                 v_bloque,
                                                 v_detalle_bloque,
                                                 v_agrega_cabeza_operacion.v_id_tipo_contrato, # 1 = Genérico, 2 = Iniciador
                                                 r_agrega_detalle_contrato[v_indice].*,
                                                 v_indice) # posición (indice) actual del contrato
                                                   RETURNING r_capturo,
                                                             v_bloque,
                                                             v_detalle_bloque
               IF NOT(r_capturo)THEN
                  INITIALIZE r_agrega_detalle_contrato[v_indice].v_tipo_dato TO NULL
                  NEXT FIELD tipo_dato
               END IF  
            END IF
           
      END INPUT
         
      BEFORE DIALOG
         CALL v_forma.setElementText("gpo_proceso","Proceso")
         CALL v_forma.setElementText("gpo_operacion","Agrega operación")
         CALL v_forma.setElementText("gpo_contrato","Agrega contrato")
         CALL v_forma.setElementText("gpo_detalle_contrato","Agrega detalle de contrato")
         CALL v_forma.setElementText("gpo_encabezado_ws","Agrega encabezado operación")
   
         # Recuperamos propiedades del combo programa
         LET v_cb_procedimiento = ui.ComboBox.forName("formonly.programa")
         # llenamos el combo programa
         CALL fn_llena_cb_programa_negocio(v_cb_procedimiento)
   
         # Recupera los datos del proceso
         CALL fn_recupera_proceso(p_proceso) RETURNING r_agrega_proceso.*
         DISPLAY r_agrega_proceso.v_cod_proceso  TO cod_proceso_bus
         DISPLAY r_agrega_proceso.v_desc_proceso TO desc_proceso_bus
         
         LET v_cb_programa_servicio = ui.ComboBox.forName("formonly.programa_servicio")
         CALL fn_llena_cb_programa_servicio(v_cb_programa_servicio,p_modulo)

         LET v_agrega_cabeza_operacion.v_id_tipo_contrato = 1 # Genérico

      ON ACTION aceptar
         # verifica que se haya capturado
         IF(v_agrega_operacion.v_cod_opera_bus IS NULL OR v_agrega_operacion.v_cod_opera_bus = " ")THEN
            CALL fn_mensaje("AVISO","Capture código de operación","about")
            NEXT FIELD cod_opera_bus
         END IF
         CALL fn_existe_cod_operacion(r_agrega_proceso.v_cod_proceso, 
                                      v_agrega_operacion.v_cod_opera_bus)RETURNING r_existe
         IF( r_existe )THEN
            CALL fn_mensaje("AVISO","Código de operación ya existe","about")
            --CALL fn_mensaje("AVISO","Código de operación ya existe para el proceso","about")
            NEXT FIELD cod_opera_bus
         END IF
   
         IF(v_agrega_operacion.v_desc_opera_bus IS NULL OR v_agrega_operacion.v_desc_opera_bus = " ")THEN
            CALL fn_mensaje("AVISO","Capture descripción de operación","about")
            NEXT FIELD cod_opera_bus
         END IF

         # verifica que se haya capturado
         IF(v_agrega_contrato.v_cod_contrato IS NULL OR v_agrega_contrato.v_cod_contrato = " ")THEN
            CALL fn_mensaje("AVISO","Capture código de contrato","about")
            NEXT FIELD cod_contrato_bus
         END IF
      
         IF(v_agrega_contrato.v_desc_contrato IS NULL OR v_agrega_contrato.v_desc_contrato = " ")THEN
            CALL fn_mensaje("AVISO","Capture descripción de contrato","about")
            NEXT FIELD desc_contrato
         END IF
      
         IF(v_agrega_contrato.v_f_ini_vigencia IS NULL OR v_agrega_contrato.v_f_ini_vigencia = " ")THEN
            CALL fn_mensaje("AVISO","Capture fecha inicio de vigencia","about")
            NEXT FIELD f_ini_vigencia
         END IF
      
         IF(v_agrega_contrato.v_id_cat_bus_negocio IS NULL OR v_agrega_contrato.v_id_cat_bus_negocio = " ")THEN
            CALL fn_mensaje("AVISO","Capture procedimiento","about")
            NEXT FIELD programa
         END IF
      
         IF(v_agrega_contrato.v_entidad_negocio IS NULL OR v_agrega_contrato.v_entidad_negocio = " ")THEN
            CALL fn_mensaje("AVISO","Capture entidad negocio","about")
            NEXT FIELD programa
         END IF
         CALL fn_verifica_entidad_negocio(v_agrega_contrato.v_entidad_negocio) RETURNING r_existe
         IF NOT(r_existe)THEN
            CALL fn_mensaje("AVISO","No exite entidad negocio","about")
            NEXT FIELD programa
         END IF
         # Valida que se hayan capturado los campos
         FOR v_indice = 1 TO r_agrega_detalle_contrato.getLength()
            # si el registro esta seleccionado
            IF(r_agrega_detalle_contrato[v_indice].v_seleccionado = TRUE)THEN 
			   IF( r_agrega_detalle_contrato[v_indice].v_orden IS NULL )THEN
                  CALL fn_mensaje("AVISO","Capture orden","about")
                  NEXT FIELD orden
               END IF 
			   
               IF( r_agrega_detalle_contrato[v_indice].v_cve_natural IS NULL )THEN
                  CALL fn_mensaje("AVISO","Capture clave natural","about")
                  NEXT FIELD cve_natural

               END IF 

               IF( r_agrega_detalle_contrato[v_indice].v_etiqueta IS NULL )THEN
                  CALL fn_mensaje("AVISO","Capture etiqueta","about")
                  NEXT FIELD etiqueta
               END IF

               IF( r_agrega_detalle_contrato[v_indice].v_tipo_dato IS NULL )THEN
                  CALL fn_mensaje("AVISO","Capture tipo dato","about")
                  NEXT FIELD tipo_dato
               END IF

               IF(v_agrega_cabeza_operacion.v_id_tipo_contrato = 2 AND # iniciador
                  r_agrega_detalle_contrato[v_indice].v_sentido = 0)THEN
                  CALL fn_mensaje("AVISO","Capture sentido de información","about")
                  NEXT FIELD ind_sentido
               END IF

               IF(v_agrega_cabeza_operacion.v_id_tipo_contrato = 1 AND # Genérico
                  r_agrega_detalle_contrato[v_indice].v_opcional IS NULL)THEN
                  CALL fn_mensaje("AVISO","Capture tipo opcional","about")
                  NEXT FIELD ind_opcional
               END IF
               
            END IF
         END FOR
         # confirma actualización de operacion
         CALL fn_ventana_confirma("AVISO","¿Agregar operación?","about")RETURNING r_confirma
         IF(r_confirma)THEN
            LET v_agrega_operacion.v_f_actualiza = TODAY
            LET v_agrega_operacion.v_usuario     = p_usuario_cod
            # se actualiza el registro
            CALL fn_agrega_operacion(p_proceso,v_agrega_operacion.*,
                                     v_agrega_cabeza_operacion.*,
                                     v_agrega_contrato.*,
                                     r_agrega_detalle_contrato,
                                     v_bloque,
                                     v_detalle_bloque)RETURNING v_error_sql
            IF(v_error_sql)THEN
               # si ha ocurrido un error
               CALL fn_mensaje("AVISO","Ocurrió un error al agregar operación","about")
            ELSE
               CALL fn_mensaje("AVISO","Operación agregada correctamente","about")
               LET v_actualiza_arbol = TRUE
               ACCEPT DIALOG 
            END IF
         END IF
   
      ON ACTION cancelar
         # sale de modificacion de operación
         EXIT DIALOG
         
   END DIALOG
   
   RETURN v_actualiza_arbol
END FUNCTION


################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para capturar el nuevo contrato de una           #
#                     determinada operación                                    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_captura_agrega_contrato(p_modulo,p_proceso,p_operacion)
DEFINE p_modulo            CHAR(3),
       p_proceso           LIKE cat_bus_proceso.id_cat_bus_proceso,
       p_operacion         LIKE cat_bus_operacion.id_cat_bus_operacion,
       v_agrega_contrato RECORD
         v_id_cat_bus_operacion LIKE cat_bus_contrato.id_cat_bus_operacion,
         v_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_cod_contrato         LIKE cat_bus_contrato.cod_contrato,
         v_desc_contrato        LIKE cat_bus_contrato.desc_contrato,
         v_ind_vigencia         LIKE cat_bus_contrato.ind_vigencia,
         v_f_ini_vigencia       LIKE cat_bus_contrato.f_ini_vigencia,
         v_f_fin_vigencia       LIKE cat_bus_contrato.f_fin_vigencia,
         v_id_cat_bus_negocio   LIKE cat_bus_contrato.id_cat_bus_negocio,
         v_programa             LIKE cat_bus_negocio.programa,
         v_entidad_negocio      LIKE cat_bus_contrato.entidad_negocio
       END RECORD,
       r_agrega_operacion RECORD
         v_id_cat_bus_proceso   LIKE cat_bus_operacion.id_cat_bus_proceso,
         v_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
         v_cod_opera_bus        LIKE cat_bus_operacion.cod_opera_bus,
         v_desc_opera_bus       LIKE cat_bus_operacion.desc_opera_bus,
         v_f_actualiza          LIKE cat_bus_operacion.f_actualiza,
         v_usuario              LIKE cat_bus_operacion.usuario
       END RECORD,
       r_agrega_proceso RECORD
         v_id_cat_bus_proceso LIKE cat_bus_proceso.id_cat_bus_proceso,
         v_cod_proceso        LIKE cat_bus_proceso.cod_proceso_bus,
         v_desc_proceso       LIKE cat_bus_proceso.desc_proceso_bus,
         v_modulo_cod         LIKE cat_bus_proceso.modulo_cod,
         v_f_actualiza        LIKE cat_bus_proceso.f_actualiza,
         v_usuario            LIKE cat_bus_proceso.usuario
       END RECORD,
       r_agrega_cabeza_operacion RECORD
         v_id_sistema        LIKE cat_bus_cza_operacion.id_sistema,
         v_id_business       LIKE cat_bus_cza_operacion.id_ebusiness,
         v_id_portafolio     LIKE cat_bus_cza_operacion.id_portafolio,
         v_id_servicio       LIKE cat_bus_cza_operacion.id_servicio,
         v_id_cliente        LIKE cat_bus_cza_operacion.id_cliente,
         v_id_canal          LIKE cat_bus_cza_operacion.id_canal,
         v_url_servicio      LIKE cat_bus_cza_operacion.url_servicio,
         v_num_reintento     LIKE cat_bus_cza_operacion.num_reintento,
         v_intervalo         LIKE cat_bus_cza_operacion.intervalo,
         v_id_tipo_contrato  LIKE cat_bus_cza_operacion.id_tipo_contrato,
         v_programa_servicio LIKE cat_bus_cza_operacion.programa_servicio
       END RECORD,
       r_agrega_detalle_contrato DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_seleccionado               BOOLEAN,
         v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
         v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
         v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
         v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                      LIKE cat_bus_detalle_contrato.orden,
         v_sentido                    LIKE cat_bus_detalle_contrato.ind_sentido,
         v_opcional                    LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD,
       # bloque para datos de tipo arreglo
       v_bloque DYNAMIC ARRAY OF RECORD
	     v_id_cat_bus_entidad           LIKE cat_bus_bloque.id_cat_bus_bloque,
		 v_id_cat_bus_detalle_contrato LIKE cat_bus_bloque.id_cat_bus_detalle_contrato,
	     v_cve_natural                 LIKE cat_bus_bloque.cve_natural,
		 v_desc_bloque                 LIKE cat_bus_bloque.desc_bloque,
		 v_ent_negocio_bloque          LIKE cat_bus_bloque.ent_negocio_bloque	   
	   END RECORD,
       v_detalle_bloque DYNAMIC ARRAY OF RECORD
	      v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
		  v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
          v_seleccionado               BOOLEAN,
		  v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
		  v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
		  v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
		  v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
		  v_orden                      LIKE cat_bus_detalle_contrato.orden,
          v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
	   END RECORD,
       v_cb_procedimiento     ui.ComboBox,
       v_cb_programa_servicio ui.ComboBox,
       v_error_sql        BOOLEAN,
       r_confirma         BOOLEAN,
       r_existe           BOOLEAN,       
       v_actualiza_arbol  BOOLEAN,
       v_indice           SMALLINT,
       r_capturo          BOOLEAN

   # determina si se tiene que actualizar el arbol de registros
   LET v_actualiza_arbol = FALSE
   DIALOG ATTRIBUTES(UNBUFFERED)
            # Detalle contrato
      INPUT v_agrega_contrato.v_cod_contrato,
            v_agrega_contrato.v_desc_contrato,
            v_agrega_contrato.v_f_ini_vigencia,
            v_agrega_contrato.v_f_fin_vigencia,
            v_agrega_contrato.v_id_cat_bus_negocio,
            v_agrega_contrato.v_entidad_negocio
            # contrato
       FROM cod_contrato_bus,
            desc_contrato,
            f_ini_vigencia,
            f_fin_vigencia,
            programa,
            entidad_negocio 
                   
         # Contrato
         AFTER FIELD cod_contrato_bus
            # verifica que se haya capturado
            IF(v_agrega_contrato.v_cod_contrato IS NULL OR v_agrega_contrato.v_cod_contrato = " ")THEN
               CALL fn_mensaje("AVISO","Capture código de contrato","about")
               NEXT FIELD cod_contrato_bus
            END IF
            LET r_existe = FALSE
            EXECUTE prp_existe_cod_contrato USING r_agrega_operacion.v_id_cat_bus_operacion,
                                                  v_agrega_contrato.v_cod_contrato
                                             INTO r_existe 
            --CALL fn_existe_cod_contrato(r_agrega_operacion.v_id_cat_bus_operacion,v_agrega_contrato.v_cod_contrato) RETURNING r_existe
            IF( r_existe )THEN
               CALL fn_mensaje("AVISO","Código de contrato ya existe para la operación","about")
               NEXT FIELD cod_contrato_bus
            END IF
         	
         AFTER FIELD desc_contrato
            # verifica que se haya capturado 
            IF(v_agrega_contrato.v_desc_contrato IS NULL OR v_agrega_contrato.v_desc_contrato = " ")THEN
               CALL fn_mensaje("AVISO","Capture descripción de contrato","about")
               NEXT FIELD desc_contrato
            END IF
   
         AFTER FIELD f_ini_vigencia
            # verifica que se haya capturado 
            IF(v_agrega_contrato.v_f_ini_vigencia IS NULL OR v_agrega_contrato.v_f_ini_vigencia = " ")THEN
               CALL fn_mensaje("AVISO","Capture fecha inicio de vigencia","about")
               NEXT FIELD f_ini_vigencia
            END IF
   
         AFTER FIELD programa
            # verifica que se haya capturado 
            IF(v_agrega_contrato.v_id_cat_bus_negocio IS NULL OR v_agrega_contrato.v_id_cat_bus_negocio = " ")THEN
               CALL fn_mensaje("AVISO","Capture procedimiento","about")
               NEXT FIELD programa
            END IF
   
         AFTER FIELD entidad_negocio
            # verifica que se haya capturado 
            IF(v_agrega_contrato.v_entidad_negocio IS NULL OR v_agrega_contrato.v_entidad_negocio = " ")THEN
               CALL fn_mensaje("AVISO","Capture entidad negocio","about")
               NEXT FIELD entidad_negocio
            END IF
            CALL fn_verifica_entidad_negocio(v_agrega_contrato.v_entidad_negocio) RETURNING r_existe
            IF NOT(r_existe)THEN
               CALL fn_mensaje("AVISO","No exite entidad negocio","about")
               NEXT FIELD entidad_negocio
            END IF
            
         ON CHANGE entidad_negocio
            CALL r_agrega_detalle_contrato.clear()
            CALL fn_consulta_detalle_entidad_agrega(v_agrega_contrato.v_entidad_negocio) RETURNING r_agrega_detalle_contrato
   
      END INPUT

      INPUT ARRAY r_agrega_detalle_contrato FROM sr_detalle_contrato.* ATTRIBUTES(APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE)
         BEFORE INPUT      
            LET v_cb_orden     = ui.ComboBox.forName("formonly.orden")      
            LET v_tam_arreglo  = r_agrega_detalle_contrato.getLength()
            CALL fn_llena_cb_orden(v_cb_orden,v_tam_arreglo,r_agrega_detalle_contrato)

            LET v_cb_tipo_dato = ui.ComboBox.forName("formonly.tipo_dato")
            CALL fn_llena_cb_tipo_dato(v_cb_tipo_dato)

         BEFORE ROW
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND
               r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato = 'A')THEN # Tipo arreglo
               CALL DIALOG.setActionActive("btn_bloque_arreglo",TRUE)               
            ELSE
               CALL DIALOG.setActionActive("btn_bloque_arreglo",FALSE)
            END IF

         AFTER FIELD orden
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               (r_agrega_detalle_contrato[ARR_CURR()].v_orden IS NULL OR 
                r_agrega_detalle_contrato[ARR_CURR()].v_orden = 0))THEN
               CALL fn_mensaje("AVISO","Capture orden","information")
               NEXT FIELD orden
            END IF

         AFTER FIELD cve_natural
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               r_agrega_detalle_contrato[ARR_CURR()].v_cve_natural IS NULL)THEN
               CALL fn_mensaje("AVISO","Capture clave natural","information")
               NEXT FIELD cve_natural
            END IF

         AFTER FIELD etiqueta
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               r_agrega_detalle_contrato[ARR_CURR()].v_etiqueta IS NULL)THEN
               CALL fn_mensaje("AVISO","Capture etiqueta","information")
               NEXT FIELD etiqueta
            END IF

         AFTER FIELD tipo_dato
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato IS NULL)THEN
               CALL fn_mensaje("AVISO","Capture tipo dato","information")
               NEXT FIELD tipo_dato
            END IF

         AFTER FIELD ind_sentido            
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND ( 
               r_agrega_detalle_contrato[ARR_CURR()].v_sentido IS NULL OR
               r_agrega_detalle_contrato[ARR_CURR()].v_sentido = 0))THEN
               CALL fn_mensaje("AVISO","Capture sentido de información","information")
               NEXT FIELD ind_sentido
            END IF

         AFTER FIELD ind_opcional            
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND
               r_agrega_detalle_contrato[ARR_CURR()].v_opcional IS NULL)THEN
               CALL fn_mensaje("AVISO","Capture tipo opcional","information")
               NEXT FIELD ind_opcional
            END IF

         # limpia registro si se des selecciona combo seleccion
         ON CHANGE seleccion
            IF NOT( r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_agrega_detalle_contrato[ARR_CURR()].v_orden       = NULL
               LET r_agrega_detalle_contrato[ARR_CURR()].v_cve_natural = NULL
               LET r_agrega_detalle_contrato[ARR_CURR()].v_etiqueta    = NULL
               LET r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato   = NULL
               LET r_agrega_detalle_contrato[ARR_CURR()].v_sentido     = 1 # ida
               LET r_agrega_detalle_contrato[ARR_CURR()].v_opcional    = 0 # requerido
            END IF

         ON CHANGE cve_natural
            IF NOT( r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_agrega_detalle_contrato[ARR_CURR()].v_cve_natural = NULL
               CALL fn_mensaje("AVISO","Registro no seleccionado","information")
            END IF

         ON CHANGE etiqueta
            IF NOT( r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_agrega_detalle_contrato[ARR_CURR()].v_etiqueta = NULL
               CALL fn_mensaje("AVISO","Registro no seleccionado","information")
            END IF

         ON CHANGE tipo_dato
            IF NOT( r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato = NULL
               CALL fn_mensaje("AVISO","Registro no seleccionado","information")
            END IF
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado)THEN
               IF(r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato = 'A')THEN # Arreglo
                  LET v_indice = ARR_CURR() # almacena en variable para no érder la referencia del indice, despues de llamar otra ventana
                  CALL fn_administra_captura_bloque('A', # alta
                                                    v_bloque,
                                                    v_detalle_bloque,
                                                    r_agrega_cabeza_operacion.v_id_tipo_contrato, # 1 = Genérico, 2 = Iniciador
                                                    r_agrega_detalle_contrato[v_indice].*,
                                                    v_indice) # posición (indice) actual del contrato
                                                      RETURNING r_capturo,
                                                                v_bloque,
                                                                v_detalle_bloque
                  IF NOT(r_capturo)THEN
                     INITIALIZE r_agrega_detalle_contrato[v_indice].v_tipo_dato TO NULL
                     NEXT FIELD tipo_dato
                  END IF
               ELSE
                  # elimina el registro de arreglo temporal
                  CALL fn_elimina_bloque_tmp(ARR_CURR(), # el indice del contrato se relacona al bloque
                                             v_bloque,
                                             v_detalle_bloque) RETURNING v_bloque,
                                                                         v_detalle_bloque
               END IF
            END IF

         ON CHANGE orden            
            IF NOT( r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_agrega_detalle_contrato[ARR_CURR()].v_orden = NULL
               CALL fn_mensaje("AVISO","Registro no seleccionado","information")
            END IF
            LET g_orden = v_cb_orden.getItemText(GET_FLDBUF(orden))
            IF (g_orden is null or g_orden = " " OR 
               g_orden = "") THEN
            ELSE    
               FOR i = 1 TO r_agrega_detalle_contrato.getLength()
                  IF i = ARR_CURR() THEN
                     CONTINUE FOR
                  END IF
                  IF g_orden = r_agrega_detalle_contrato[i].v_orden THEN
                     CALL fn_mensaje("AVISO","Posición de orden ya elegida","information")
                     LET r_agrega_detalle_contrato[ARR_CURR()].v_orden = ""                     
                     EXIT FOR                     
                  END IF
               END FOR
            END IF

         ON ACTION btn_bloque_arreglo
            IF(r_agrega_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               r_agrega_detalle_contrato[ARR_CURR()].v_tipo_dato = 'A')THEN
               LET v_indice = ARR_CURR() # almacena en variable para no érder la referencia del indice, despues de llamar otra ventana
               CALL fn_administra_captura_bloque('M', # alta
                                                 v_bloque,
                                                 v_detalle_bloque,
                                                 r_agrega_cabeza_operacion.v_id_tipo_contrato, # 1 = Genérico, 2 = Iniciador
                                                 r_agrega_detalle_contrato[v_indice].*,
                                                 v_indice) # posición (indice) actual del contrato
                                                   RETURNING r_capturo,
                                                             v_bloque,
                                                             v_detalle_bloque
               IF NOT(r_capturo)THEN
                  INITIALIZE r_agrega_detalle_contrato[v_indice].v_tipo_dato TO NULL
                  NEXT FIELD tipo_dato
               END IF  
            END IF
            
      END INPUT
         
      BEFORE DIALOG
         CALL v_forma.setElementText("gpo_proceso","Proceso")
         CALL v_forma.setElementText("gpo_operacion","Operación")
         CALL v_forma.setElementText("gpo_contrato","Agrega contrato")
         CALL v_forma.setElementText("gpo_detalle_contrato","Agrega detalle de contrato")
         CALL v_forma.setElementText("gpo_encabezado_ws","Encabezado operación")
   
         # Recuperamos propiedades del combo programa
         LET v_cb_procedimiento = ui.ComboBox.forName("formonly.programa")
         # llenamos el combo programa
         CALL fn_llena_cb_programa_negocio(v_cb_procedimiento)

         # Recupera los datos del proceso
         CALL fn_recupera_proceso(p_proceso) RETURNING r_agrega_proceso.*
         DISPLAY r_agrega_proceso.v_cod_proceso  TO cod_proceso_bus
         DISPLAY r_agrega_proceso.v_desc_proceso TO desc_proceso_bus

         # Recupera los datos de la operación
         CALL fn_recupera_operacion(p_proceso,p_operacion) RETURNING r_agrega_operacion.*
         CALL fn_recupera_cza_operacion(p_operacion) RETURNING r_agrega_cabeza_operacion.*
         
         DISPLAY r_agrega_cabeza_operacion.v_id_sistema    TO id_sistema   
         DISPLAY r_agrega_cabeza_operacion.v_id_business   TO id_business 
         DISPLAY r_agrega_cabeza_operacion.v_id_portafolio TO id_portafolio
         DISPLAY r_agrega_cabeza_operacion.v_id_servicio   TO id_servicio
         DISPLAY r_agrega_cabeza_operacion.v_id_cliente    TO id_cliente
         DISPLAY r_agrega_cabeza_operacion.v_id_canal      TO id_canal
         DISPLAY r_agrega_cabeza_operacion.v_url_servicio  TO url_servicio
         DISPLAY r_agrega_cabeza_operacion.v_num_reintento TO num_reintento
         DISPLAY r_agrega_cabeza_operacion.v_intervalo     TO intervalo
         DISPLAY r_agrega_cabeza_operacion.v_id_tipo_contrato  TO tipo_contrato
         DISPLAY r_agrega_cabeza_operacion.v_programa_servicio TO programa_servicio
         
         DISPLAY r_agrega_operacion.v_cod_opera_bus  TO cod_opera_bus
         DISPLAY r_agrega_operacion.v_desc_opera_bus TO desc_opera_bus

         # 1 = Genérico, 2 = Iniciador
         IF(r_agrega_cabeza_operacion.v_id_tipo_contrato = 2)THEN
            LET v_cb_programa_servicio = ui.ComboBox.forName("formonly.programa_servicio")
            CALL fn_llena_cb_programa_servicio(v_cb_programa_servicio,p_modulo)
            CALL v_forma.setFieldHidden("programa_servicio",FALSE)
            CALL v_forma.setFieldHidden("ind_sentido",FALSE)
            CALL v_forma.setFieldHidden("ind_opcional",TRUE)
            CALL v_forma.setElementHidden("programa",FALSE)    
         ELSE
            CALL v_forma.setFieldHidden("programa_servicio",TRUE)
            CALL v_forma.setFieldHidden("ind_sentido",TRUE)
            CALL v_forma.setFieldHidden("ind_opcional",FALSE)
            CALL v_forma.setElementHidden("programa",TRUE)
         END IF

      ON ACTION aceptar
         # verifica que se haya capturado
         IF(v_agrega_contrato.v_cod_contrato IS NULL OR v_agrega_contrato.v_cod_contrato = " ")THEN
            CALL fn_mensaje("AVISO","Capture código de contrato","about")
            NEXT FIELD cod_contrato_bus
         END IF
      
         IF(v_agrega_contrato.v_desc_contrato IS NULL OR v_agrega_contrato.v_desc_contrato = " ")THEN
            CALL fn_mensaje("AVISO","Capture descripción de contrato","about")
            NEXT FIELD desc_contrato
         END IF
      
         IF(v_agrega_contrato.v_f_ini_vigencia IS NULL OR v_agrega_contrato.v_f_ini_vigencia = " ")THEN
            CALL fn_mensaje("AVISO","Capture fecha inicio de vigencia","about")
            NEXT FIELD f_ini_vigencia
         END IF
      
         IF(v_agrega_contrato.v_id_cat_bus_negocio IS NULL OR v_agrega_contrato.v_id_cat_bus_negocio = " ")THEN
            CALL fn_mensaje("AVISO","Capture procedimiento","about")
            NEXT FIELD programa
         END IF
      
         IF(v_agrega_contrato.v_entidad_negocio IS NULL OR v_agrega_contrato.v_entidad_negocio = " ")THEN
            CALL fn_mensaje("AVISO","Capture entidad negocio","about")
            NEXT FIELD programa
         END IF
         CALL fn_verifica_entidad_negocio(v_agrega_contrato.v_entidad_negocio) RETURNING r_existe
         IF NOT(r_existe)THEN
            CALL fn_mensaje("AVISO","No exite entidad negocio","about")
            NEXT FIELD programa
         END IF
         # Valida que se hayan capturado los campos
         FOR v_indice = 1 TO r_agrega_detalle_contrato.getLength()
            # si el registro esta seleccionado
            IF(r_agrega_detalle_contrato[v_indice].v_seleccionado = TRUE)THEN 
			   IF( r_agrega_detalle_contrato[v_indice].v_orden IS NULL )THEN
                  CALL fn_mensaje("AVISO","Capture orden","about")
                  NEXT FIELD orden
               END IF 
			   
               IF( r_agrega_detalle_contrato[v_indice].v_cve_natural IS NULL )THEN
                  CALL fn_mensaje("AVISO","Capture clave natural","about")
                  NEXT FIELD cve_natural

               END IF 

               IF( r_agrega_detalle_contrato[v_indice].v_etiqueta IS NULL )THEN
                  CALL fn_mensaje("AVISO","Capture etiqueta","about")
                  NEXT FIELD etiqueta
               END IF

               IF( r_agrega_detalle_contrato[v_indice].v_tipo_dato IS NULL )THEN
                  CALL fn_mensaje("AVISO","Capture tipo dato","about")
                  NEXT FIELD tipo_dato
               END IF

               IF(r_agrega_cabeza_operacion.v_id_tipo_contrato = 2 AND # iniciador
                  r_agrega_detalle_contrato[v_indice].v_sentido = 0)THEN
                  CALL fn_mensaje("AVISO","Capture sentido de información","about")
                  NEXT FIELD ind_sentido
               END IF

               IF(r_agrega_cabeza_operacion.v_id_tipo_contrato = 1 AND # Genérico
                  r_agrega_detalle_contrato[v_indice].v_opcional IS NULL)THEN
                  CALL fn_mensaje("AVISO","Capture tipo opcional","about")
                  NEXT FIELD ind_sentido
               END IF
               
            END IF
         END FOR
         # confirma actualización de operacion
         CALL fn_ventana_confirma("AVISO","¿Agregar contrato?","about")RETURNING r_confirma
         IF(r_confirma)THEN
            # se actualiza el registro
            CALL fn_agrega_contrato(p_operacion,
                                    v_agrega_contrato.*,
                                    r_agrega_detalle_contrato,
                                    v_bloque,
                                    v_detalle_bloque)RETURNING v_error_sql
            IF(v_error_sql)THEN
               # si ha ocurrido un error
               CALL fn_mensaje("AVISO","Ocurrió un error al agregar contrato","about")
            ELSE
               CALL fn_mensaje("AVISO","Contrato agregado correctamente","about")
               LET v_actualiza_arbol = TRUE
               ACCEPT DIALOG
            END IF
         END IF
   
      ON ACTION cancelar
         # sale de modificacion de operación
         EXIT DIALOG
         
   END DIALOG
   
   RETURN v_actualiza_arbol
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para recuperar los datos de la cabeza operación  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_recupera_cza_operacion(p_operacion)
DEFINE p_operacion  LIKE cat_bus_cza_operacion.id_cat_bus_operacion,
       v_cabeza_operaicon RECORD
         v_id_sistema        LIKE cat_bus_cza_operacion.id_sistema,
         v_id_business       LIKE cat_bus_cza_operacion.id_ebusiness,
         v_id_portafolio     LIKE cat_bus_cza_operacion.id_portafolio,
         v_id_servicio       LIKE cat_bus_cza_operacion.id_servicio,
         v_id_clietne        LIKE cat_bus_cza_operacion.id_cliente,
         v_id_canal          LIKE cat_bus_cza_operacion.id_canal,
         v_url_servicio      LIKE cat_bus_cza_operacion.url_servicio,
         v_num_reintento     LIKE cat_bus_cza_operacion.num_reintento,
         v_intervalo         LIKE cat_bus_cza_operacion.intervalo,
         v_id_tipo_contrato  LIKE cat_bus_cza_operacion.id_tipo_contrato,
         v_programa_servicio LIKE cat_bus_cza_operacion.programa_servicio
       END RECORD

    EXECUTE prp_recupera_cza_operacion USING p_operacion
                                        INTO v_cabeza_operaicon.v_id_sistema,
                                             v_cabeza_operaicon.v_id_business,
                                             v_cabeza_operaicon.v_id_portafolio,
                                             v_cabeza_operaicon.v_id_servicio,
                                             v_cabeza_operaicon.v_id_clietne,
                                             v_cabeza_operaicon.v_id_canal,
                                             v_cabeza_operaicon.v_url_servicio,
                                             v_cabeza_operaicon.v_num_reintento,
                                             v_cabeza_operaicon.v_intervalo,
                                             v_cabeza_operaicon.v_id_tipo_contrato,
                                             v_cabeza_operaicon.v_programa_servicio

   RETURN v_cabeza_operaicon.*
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para recuperar los datos de la operación         #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_recupera_operacion(p_proceso,p_operacion)
DEFINE p_proceso           LIKE cat_bus_proceso.id_cat_bus_proceso,
       p_operacion         LIKE cat_bus_operacion.id_cat_bus_operacion,
       v_operacion RECORD
         v_id_cat_bus_proceso   LIKE cat_bus_operacion.id_cat_bus_proceso,
         v_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
         v_cod_opera_bus        LIKE cat_bus_operacion.cod_opera_bus,
         v_desc_opera_bus       LIKE cat_bus_operacion.desc_opera_bus,
         v_f_actualiza          LIKE cat_bus_operacion.f_actualiza,
         v_usuario              LIKE cat_bus_operacion.usuario
       END RECORD

   
   EXECUTE prp_recupera_operacion USING p_proceso,
                                        p_operacion
                                   INTO v_operacion.v_id_cat_bus_proceso,
                                        v_operacion.v_id_cat_bus_operacion,
                                        v_operacion.v_cod_opera_bus,
                                        v_operacion.v_desc_opera_bus,
                                        v_operacion.v_f_actualiza,
                                        v_operacion.v_usuario

   RETURN v_operacion.*
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para recuperar los datos de la operación         #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_recupera_proceso(p_proceso)
DEFINE p_proceso           LIKE cat_bus_proceso.id_cat_bus_proceso,
       v_proceso RECORD
         v_id_cat_bus_proceso LIKE cat_bus_proceso.id_cat_bus_proceso,
         v_cod_proceso        LIKE cat_bus_proceso.cod_proceso_bus,
         v_desc_proceso       LIKE cat_bus_proceso.desc_proceso_bus,
         v_modulo_cod         LIKE cat_bus_proceso.modulo_cod,
         v_f_actualiza        LIKE cat_bus_proceso.f_actualiza,
         v_usuario            LIKE cat_bus_proceso.usuario
       END RECORD

    EXECUTE prp_recupera_proceso USING p_proceso
                                  INTO v_proceso.v_id_cat_bus_proceso,
                                       v_proceso.v_cod_proceso,
                                       v_proceso.v_desc_proceso,
                                       v_proceso.v_modulo_cod,
                                       v_proceso.v_f_actualiza,
                                       v_proceso.v_usuario
   
   RETURN v_proceso.*
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para modificar el proceso                        #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_captura_modificacion_proceso(p_proceso)
DEFINE p_proceso         LIKE cat_bus_operacion.id_cat_bus_proceso,
       v_modifica_proceso RECORD
         v_id_cat_bus_proceso LIKE cat_bus_proceso.id_cat_bus_proceso,
         v_cod_proceso        LIKE cat_bus_proceso.cod_proceso_bus,
         v_desc_proceso       LIKE cat_bus_proceso.desc_proceso_bus,
         v_modulo_cod         LIKE cat_bus_proceso.modulo_cod,
         v_f_actualiza        LIKE cat_bus_proceso.f_actualiza,
         v_usuario            LIKE cat_bus_proceso.usuario
       END RECORD,
       v_error_sql       BOOLEAN,
       v_confirma        BOOLEAN,
       v_actualiza_arbol BOOLEAN

   # variable para determinar si se realizaron modificaciones y es necesario actualizar arbol
   LET v_actualiza_arbol = FALSE
   INPUT v_modifica_proceso.v_desc_proceso 
         # proceso
    FROM desc_proceso_bus ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

      BEFORE INPUT
         # Cambia el titulo del grupo para indicar que se esta modificando el registro
         CALL v_forma.setElementText("gpo_proceso","Modificar proceso")

         # Recupera los datos del proceso
         CALL fn_recupera_proceso(p_proceso) RETURNING v_modifica_proceso.*
         DISPLAY v_modifica_proceso.v_cod_proceso  TO cod_proceso_bus_consulta
         DISPLAY v_modifica_proceso.v_desc_proceso TO desc_proceso_bus_consulta
    
         # Proceso
         AFTER FIELD desc_proceso_bus
            # verifica que se haya capturado el código de proceso
            IF(v_modifica_proceso.v_desc_proceso IS NULL OR v_modifica_proceso.v_desc_proceso = " ")THEN
               CALL fn_mensaje("AVISO","Capture descripción de proceso","information")
               NEXT FIELD desc_proceso_bus
            END IF
         
         
      ON ACTION aceptar
         # verifica que se haya capturado el código de proceso
         IF(v_modifica_proceso.v_desc_proceso IS NULL OR v_modifica_proceso.v_desc_proceso = " ")THEN
            CALL fn_mensaje("AVISO","Capture descripción de proceso","information")
            NEXT FIELD desc_proceso_bus
         END IF
                  
         # confirma actualización de operacion
         CALL fn_ventana_confirma("AVISO","¿Modificar proceso?","about")RETURNING v_confirma
         IF(v_confirma)THEN
            LET v_modifica_proceso.v_f_actualiza = TODAY
            LET v_modifica_proceso.v_usuario     = p_usuario_cod
            # se actualiza el registro
            CALL fn_modifica_proceso(v_modifica_proceso.*)RETURNING v_error_sql
            IF(v_error_sql)THEN
               # si ha ocurrido un error
               CALL fn_mensaje("AVISO","Ocurrió un error al modificar proceso","about")
            ELSE
               CALL fn_mensaje("AVISO","Proceso modificado correctamente","about")
               LET v_actualiza_arbol = TRUE
               ACCEPT INPUT 
            END IF
         END IF
   
      ON ACTION cancelar
         # sale de modificacion de operación
         EXIT INPUT
           
   END INPUT
   
   RETURN v_actualiza_arbol
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para modificar el proceso en BD                  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_modifica_proceso(p_proceso)
DEFINE p_proceso RECORD
         v_id_cat_bus_proceso LIKE cat_bus_proceso.id_cat_bus_proceso,
         v_cod_proceso        LIKE cat_bus_proceso.cod_proceso_bus,
         v_desc_proceso       LIKE cat_bus_proceso.desc_proceso_bus,
         v_modulo_cod         LIKE cat_bus_proceso.modulo_cod,
         v_f_actualiza        LIKE cat_bus_proceso.f_actualiza,
         v_usuario            LIKE cat_bus_proceso.usuario
       END RECORD,
       v_error_sql BOOLEAN

   # Actualiza el proceso
   EXECUTE prp_modifica_proceso USING p_proceso.v_desc_proceso,
                                      p_proceso.v_f_actualiza,
                                      p_proceso.v_usuario,
                                      p_proceso.v_id_cat_bus_proceso
   
   IF(SQLCA.SQLCODE <> 0)THEN
      # Ocurrió un error
      LET v_error_sql =  TRUE
   ELSE
      # No ocurrió error
      LET v_error_sql =  FALSE
   END IF
   RETURN v_error_sql
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para modificar la operación                      #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_captura_modificacion_operacion(p_modulo,p_proceso, p_operacion)
DEFINE p_modulo             CHAR(3),
       p_proceso            LIKE cat_bus_proceso.id_cat_bus_proceso,
       p_operacion          LIKE cat_bus_operacion.id_cat_bus_operacion,
       v_modifica_operacion RECORD
         v_id_cat_bus_proceso   LIKE cat_bus_operacion.id_cat_bus_proceso,
         v_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
         v_cod_opera_bus        LIKE cat_bus_operacion.cod_opera_bus,
         v_desc_opera_bus       LIKE cat_bus_operacion.desc_opera_bus,
         v_f_actualiza          LIKE cat_bus_operacion.f_actualiza,
         v_usuario              LIKE cat_bus_operacion.usuario
       END RECORD,
       v_modifica_cabeza_operacion RECORD
         v_id_sistema        LIKE cat_bus_cza_operacion.id_sistema,
         v_id_business       LIKE cat_bus_cza_operacion.id_ebusiness,
         v_id_portafolio     LIKE cat_bus_cza_operacion.id_portafolio,
         v_id_servicio       LIKE cat_bus_cza_operacion.id_servicio,
         v_id_cliente        LIKE cat_bus_cza_operacion.id_cliente,
         v_id_canal          LIKE cat_bus_cza_operacion.id_canal,
         v_url_servicio      LIKE cat_bus_cza_operacion.url_servicio,
         v_num_reintento     LIKE cat_bus_cza_operacion.num_reintento,
         v_intervalo         LIKE cat_bus_cza_operacion.intervalo,
         v_id_tipo_contrato  LIKE cat_bus_cza_operacion.id_tipo_contrato,
         v_programa_servicio LIKE cat_bus_cza_operacion.programa_servicio
       END RECORD,
       r_proceso RECORD
         v_id_cat_bus_proceso LIKE cat_bus_proceso.id_cat_bus_proceso,
         v_cod_proceso        LIKE cat_bus_proceso.cod_proceso_bus,
         v_desc_proceso       LIKE cat_bus_proceso.desc_proceso_bus,
         v_modulo_cod         LIKE cat_bus_proceso.modulo_cod,
         v_f_actualiza        LIKE cat_bus_proceso.f_actualiza,
         v_usuario            LIKE cat_bus_proceso.usuario
       END RECORD,
       v_error_sql            BOOLEAN,
       v_cb_programa_servicio ui.ComboBox,
       r_confirma             BOOLEAN,
       v_actualiza_arbol      BOOLEAN

   # determina si se tiene que actualizar el arbol de registros
   LET v_actualiza_arbol = FALSE
   DIALOG ATTRIBUTES(UNBUFFERED)
            # Operación
      INPUT v_modifica_operacion.v_desc_opera_bus,
            # cabeza operación
            v_modifica_cabeza_operacion.v_id_sistema,
            v_modifica_cabeza_operacion.v_id_business,
            v_modifica_cabeza_operacion.v_id_portafolio,
            v_modifica_cabeza_operacion.v_id_servicio,
            v_modifica_cabeza_operacion.v_id_cliente,
            v_modifica_cabeza_operacion.v_id_canal,
            v_modifica_cabeza_operacion.v_url_servicio,
            v_modifica_cabeza_operacion.v_num_reintento,
            v_modifica_cabeza_operacion.v_intervalo,
            v_modifica_cabeza_operacion.v_id_tipo_contrato,
            v_modifica_cabeza_operacion.v_programa_servicio
            # operación
       FROM desc_opera_bus,
            # cabeza operación
            id_sistema,
            id_business,
            id_portafolio,
            id_servicio,
            id_cliente,
            id_canal,
            url_servicio,
            num_reintento,
            intervalo,
            tipo_contrato,
            programa_servicio ATTRIBUTE (WITHOUT DEFAULTS)
         
         # Operación
         AFTER FIELD desc_opera_bus
            # verifica que se haya capturado
            IF(v_modifica_operacion.v_desc_opera_bus IS NULL OR v_modifica_operacion.v_desc_opera_bus = " ")THEN
               CALL fn_mensaje("AVISO","Capture descripción de operación","about")
               NEXT FIELD desc_opera_bus
            END IF

         ON CHANGE tipo_contrato
            # 1 = Genérico, 2 = Iniciador
            IF(v_modifica_cabeza_operacion.v_id_tipo_contrato = 2)THEN
               CALL v_forma.setFieldHidden("programa_servicio",FALSE)            
               CALL v_forma.setElementHidden("programa",FALSE)               
            ELSE
               INITIALIZE v_modifica_cabeza_operacion.v_programa_servicio TO NULL
               CALL v_forma.setFieldHidden("programa_servicio",TRUE)
               CALL v_forma.setElementHidden("programa",TRUE)
            END IF
   
      END INPUT

      BEFORE DIALOG
         CALL v_forma.setElementText("gpo_proceso","Proceso")
         CALL v_forma.setElementText("gpo_operacion","Modificar operación")
         CALL v_forma.setElementText("gpo_encabezado_ws","Modificar encabezado operación")
         
         # Recupera los datos del proceso
         CALL fn_recupera_proceso(p_proceso) RETURNING r_proceso.*
         DISPLAY r_proceso.v_cod_proceso  TO cod_proceso_bus
         DISPLAY r_proceso.v_desc_proceso TO desc_proceso_bus

         # Recupera los datos de la operación
         CALL fn_recupera_operacion(p_proceso,p_operacion) RETURNING v_modifica_operacion.*
         CALL fn_recupera_cza_operacion(p_operacion) RETURNING v_modifica_cabeza_operacion.*
         DISPLAY v_modifica_cabeza_operacion.v_id_sistema        TO id_sistema   
         DISPLAY v_modifica_cabeza_operacion.v_id_business       TO id_business 
         DISPLAY v_modifica_cabeza_operacion.v_id_portafolio     TO id_portafolio
         DISPLAY v_modifica_cabeza_operacion.v_id_servicio       TO id_servicio
         DISPLAY v_modifica_cabeza_operacion.v_id_cliente        TO id_cliente
         DISPLAY v_modifica_cabeza_operacion.v_id_canal          TO id_canal
         DISPLAY v_modifica_cabeza_operacion.v_url_servicio      TO url_servicio
         DISPLAY v_modifica_cabeza_operacion.v_num_reintento     TO num_reintento
         DISPLAY v_modifica_cabeza_operacion.v_intervalo         TO intervalo
         DISPLAY v_modifica_cabeza_operacion.v_id_tipo_contrato  TO tipo_contrato
         DISPLAY v_modifica_cabeza_operacion.v_programa_servicio TO programa_servicio

         DISPLAY v_modifica_operacion.v_cod_opera_bus  TO cod_opera_bus
         DISPLAY v_modifica_operacion.v_desc_opera_bus TO desc_opera_bus
         # 1 = Genérico, 2 = Iniciador
         IF(v_modifica_cabeza_operacion.v_id_tipo_contrato = 2)THEN
            CALL v_forma.setFieldHidden("programa_servicio",FALSE)            
            CALL v_forma.setElementHidden("programa",FALSE)               
         ELSE
            INITIALIZE v_modifica_cabeza_operacion.v_programa_servicio TO NULL
            CALL v_forma.setFieldHidden("programa_servicio",TRUE)
            CALL v_forma.setElementHidden("programa",TRUE)
         END IF
         LET v_cb_programa_servicio = ui.ComboBox.forName("formonly.programa_servicio")
         CALL fn_llena_cb_programa_servicio(v_cb_programa_servicio,p_modulo)
         
      ON ACTION aceptar
         # verifica que se haya capturado
         IF(v_modifica_operacion.v_desc_opera_bus IS NULL OR v_modifica_operacion.v_desc_opera_bus = " ")THEN
            CALL fn_mensaje("AVISO","Capture descripción de operación","about")
            NEXT FIELD cod_opera_bus
         END IF
      
         # confirma actualización de operacion
         CALL fn_ventana_confirma("AVISO","¿Modificar operación?","about")RETURNING r_confirma
         IF(r_confirma)THEN
            LET v_modifica_operacion.v_f_actualiza = TODAY
            LET v_modifica_operacion.v_usuario     = p_usuario_cod
            # se actualiza el registro
            CALL fn_modifica_operacion(v_modifica_operacion.*,v_modifica_cabeza_operacion.*)RETURNING v_error_sql
            IF(v_error_sql)THEN
               # si ha ocurrido un error
               CALL fn_mensaje("AVISO","Ocurrió un error al modificar operación","about")
            ELSE
               CALL fn_mensaje("AVISO","Operación modificada correctamente","about")
               LET v_actualiza_arbol = TRUE
               ACCEPT DIALOG
            END IF
         END IF
   
      ON ACTION cancelar
         # sale de modificacion de operación
         EXIT DIALOG
         
   END DIALOG
   
   RETURN v_actualiza_arbol
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para modificar el proceso en BD                  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_modifica_operacion(p_operacion,p_cabeza_operacion)
DEFINE p_operacion RECORD
         v_id_cat_bus_proceso   LIKE cat_bus_operacion.id_cat_bus_proceso,
         v_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
         v_cod_opera_bus        LIKE cat_bus_operacion.cod_opera_bus,
         v_desc_opera_bus       LIKE cat_bus_operacion.desc_opera_bus,
         v_f_actualiza          LIKE cat_bus_operacion.f_actualiza,
         v_usuario              LIKE cat_bus_operacion.usuario
       END RECORD,
       p_cabeza_operacion RECORD
         v_id_sistema       LIKE cat_bus_cza_operacion.id_sistema,
         v_id_business      LIKE cat_bus_cza_operacion.id_ebusiness,
         v_id_portafolio    LIKE cat_bus_cza_operacion.id_portafolio,
         v_id_servicio      LIKE cat_bus_cza_operacion.id_servicio,
         v_id_cliente       LIKE cat_bus_cza_operacion.id_cliente,
         v_id_canal         LIKE cat_bus_cza_operacion.id_canal,
         v_url_servicio     LIKE cat_bus_cza_operacion.url_servicio,
         v_num_reintento    LIKE cat_bus_cza_operacion.num_reintento,
         v_intervalo        LIKE cat_bus_cza_operacion.intervalo,
         v_id_tipo_contrato LIKE cat_bus_cza_operacion.id_tipo_contrato,
         v_programa_servicio LIKE cat_bus_cza_operacion.programa_servicio
       END RECORD,
       v_error_sql BOOLEAN


   # No ocurrió error
    LET v_error_sql =  FALSE
    
   # Actualiza el proceso   
   EXECUTE prp_modifica_operacion USING p_operacion.v_desc_opera_bus,
                                        p_operacion.v_f_actualiza,
                                        p_operacion.v_usuario,
                                        p_operacion.v_id_cat_bus_proceso,
                                        p_operacion.v_id_cat_bus_operacion 
   
   IF(SQLCA.SQLCODE <> 0)THEN
      # Ocurrió un error
      LET v_error_sql =  TRUE
   ELSE
      
      EXECUTE prp_modifica_cza_operacion USING p_cabeza_operacion.v_id_sistema,
                                               p_cabeza_operacion.v_id_business,
                                               p_cabeza_operacion.v_id_portafolio,
                                               p_cabeza_operacion.v_id_servicio,
                                               p_cabeza_operacion.v_id_cliente,
                                               p_cabeza_operacion.v_id_canal,
                                               p_cabeza_operacion.v_url_servicio,
                                               p_cabeza_operacion.v_num_reintento,
                                               p_cabeza_operacion.v_intervalo,
                                               p_cabeza_operacion.v_id_tipo_contrato,
                                               p_cabeza_operacion.v_programa_servicio,
                                               p_operacion.v_id_cat_bus_operacion

      IF(SQLCA.SQLCODE <> 0)THEN
         # Ocurrió un error
         LET v_error_sql =  TRUE             
      END IF
   END IF
   RETURN v_error_sql
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para modificar el contrato                       #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_captura_modificacion_contrato(p_modulo,p_proceso,p_operacion,p_contrato)
DEFINE p_modulo          CHAR(3),
       p_proceso         LIKE cat_bus_proceso.id_cat_bus_proceso,
       p_operacion       LIKE cat_bus_operacion.id_cat_bus_operacion,
       p_contrato        LIKE cat_bus_contrato.id_cat_bus_contrato,
       v_modifica_contrato RECORD
         v_id_cat_bus_operacion LIKE cat_bus_contrato.id_cat_bus_operacion,
         v_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_cod_contrato         LIKE cat_bus_contrato.cod_contrato,
         v_desc_contrato        LIKE cat_bus_contrato.desc_contrato,
         v_ind_vigencia         LIKE cat_bus_contrato.ind_vigencia,
         v_f_ini_vigencia       LIKE cat_bus_contrato.f_ini_vigencia,
         v_f_fin_vigencia       LIKE cat_bus_contrato.f_fin_vigencia,
         v_id_cat_bus_negocio   LIKE cat_bus_contrato.id_cat_bus_negocio,
         v_programa             LIKE cat_bus_negocio.programa,
         v_entidad_negocio      LIKE cat_bus_contrato.entidad_negocio
       END RECORD,
       r_operacion RECORD
         v_id_cat_bus_proceso   LIKE cat_bus_operacion.id_cat_bus_proceso,
         v_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
         v_cod_opera_bus        LIKE cat_bus_operacion.cod_opera_bus,
         v_desc_opera_bus       LIKE cat_bus_operacion.desc_opera_bus,
         v_f_actualiza          LIKE cat_bus_operacion.f_actualiza,
         v_usuario              LIKE cat_bus_operacion.usuario
       END RECORD,
       r_cabeza_operacion RECORD
         v_id_sistema        LIKE cat_bus_cza_operacion.id_sistema,
         v_id_business       LIKE cat_bus_cza_operacion.id_ebusiness,
         v_id_portafolio     LIKE cat_bus_cza_operacion.id_portafolio,
         v_id_servicio       LIKE cat_bus_cza_operacion.id_servicio,
         v_id_cliente        LIKE cat_bus_cza_operacion.id_cliente,
         v_id_canal          LIKE cat_bus_cza_operacion.id_canal,
         v_url_servicio      LIKE cat_bus_cza_operacion.url_servicio,
         v_num_reintento     LIKE cat_bus_cza_operacion.num_reintento,
         v_intervalo         LIKE cat_bus_cza_operacion.intervalo,
         v_id_tipo_contrato  LIKE cat_bus_cza_operacion.id_tipo_contrato,
         v_programa_servicio LIKE cat_bus_cza_operacion.programa_servicio
       END RECORD,
       r_proceso RECORD
         v_id_cat_bus_proceso LIKE cat_bus_proceso.id_cat_bus_proceso,
         v_cod_proceso        LIKE cat_bus_proceso.cod_proceso_bus,
         v_desc_proceso       LIKE cat_bus_proceso.desc_proceso_bus,
         v_modulo_cod         LIKE cat_bus_proceso.modulo_cod,
         v_f_actualiza        LIKE cat_bus_proceso.f_actualiza,
         v_usuario            LIKE cat_bus_proceso.usuario
       END RECORD,
       r_modifica_detalle_contrato DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_seleccionado               BOOLEAN,
         v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
         v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
         v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
         v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                      LIKE cat_bus_detalle_contrato.orden,
         v_sentido                    LIKE cat_bus_detalle_contrato.ind_sentido,
         v_opcional                    LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD,
       # bloque para datos de tipo arreglo
       v_bloque DYNAMIC ARRAY OF RECORD
	     v_id_cat_bus_entidad           LIKE cat_bus_bloque.id_cat_bus_bloque,
		 v_id_cat_bus_detalle_contrato LIKE cat_bus_bloque.id_cat_bus_detalle_contrato,
	     v_cve_natural                 LIKE cat_bus_bloque.cve_natural,
		 v_desc_bloque                 LIKE cat_bus_bloque.desc_bloque,
		 v_ent_negocio_bloque          LIKE cat_bus_bloque.ent_negocio_bloque	   
	   END RECORD,
       v_detalle_bloque DYNAMIC ARRAY OF RECORD
	      v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
		  v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
          v_seleccionado               BOOLEAN,
		  v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
		  v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
		  v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
		  v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
		  v_orden                      LIKE cat_bus_detalle_contrato.orden,
          v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
	   END RECORD,       
       v_error_sql         BOOLEAN,
       v_cb_programa_servicio ui.ComboBox,
       r_confirma          BOOLEAN,
       r_existe            BOOLEAN,       
       v_actualiza_arbol   BOOLEAN,
       v_indice            SMALLINT,       
       r_capturo           BOOLEAN

   # determina si se tiene que actualizar el arbol de registros
   LET v_actualiza_arbol = FALSE
   
   DIALOG ATTRIBUTES(UNBUFFERED, FIELD ORDER FORM)
            # Detalle contrato
      INPUT v_modifica_contrato.v_desc_contrato,
            v_modifica_contrato.v_f_ini_vigencia,
            v_modifica_contrato.v_f_fin_vigencia,
            v_modifica_contrato.v_id_cat_bus_negocio,
            v_modifica_contrato.v_entidad_negocio
            # contrato
       FROM desc_contrato,
            f_ini_vigencia,
            f_fin_vigencia,
            programa,
            entidad_negocio 
                   
         # Contrato
         AFTER FIELD desc_contrato
            # verifica que se haya capturado 
            IF(v_modifica_contrato.v_desc_contrato IS NULL OR v_modifica_contrato.v_desc_contrato = " ")THEN
               CALL fn_mensaje("AVISO","Capture descripción de contrato","about")
               NEXT FIELD desc_contrato
            END IF
   
         AFTER FIELD f_ini_vigencia
            # verifica que se haya capturado 
            IF(v_modifica_contrato.v_f_ini_vigencia IS NULL OR v_modifica_contrato.v_f_ini_vigencia = " ")THEN
               CALL fn_mensaje("AVISO","Capture fecha inicio de vigencia","about")
               NEXT FIELD f_ini_vigencia
            END IF
                        
         AFTER FIELD programa
            # verifica que se haya capturado 
            IF(v_modifica_contrato.v_id_cat_bus_negocio IS NULL OR v_modifica_contrato.v_id_cat_bus_negocio = " ")THEN
               CALL fn_mensaje("AVISO","Capture procedimiento","about")
               NEXT FIELD programa
            END IF
   
         AFTER FIELD entidad_negocio
            # verifica que se haya capturado 
            IF(v_modifica_contrato.v_entidad_negocio IS NULL OR v_modifica_contrato.v_entidad_negocio = " ")THEN
               CALL fn_mensaje("AVISO","Capture entidad negocio","about")
               NEXT FIELD entidad_negocio
            END IF
            CALL fn_verifica_entidad_negocio(v_modifica_contrato.v_entidad_negocio) RETURNING r_existe
            IF NOT(r_existe)THEN
               CALL fn_mensaje("AVISO","No exite entidad negocio","about")
               NEXT FIELD entidad_negocio
            END IF
            
         ON CHANGE entidad_negocio
            CALL r_modifica_detalle_contrato.clear()
            CALL fn_consulta_detalle_entidad_agrega(v_modifica_contrato.v_entidad_negocio) RETURNING r_modifica_detalle_contrato
   
      END INPUT

      INPUT ARRAY r_modifica_detalle_contrato FROM sr_detalle_contrato.* ATTRIBUTES(APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE)
         BEFORE INPUT      
            LET v_cb_orden     = ui.ComboBox.forName("formonly.orden")      
            LET v_tam_arreglo  = r_modifica_detalle_contrato.getLength()
            CALL fn_llena_cb_orden(v_cb_orden,v_tam_arreglo,r_modifica_detalle_contrato)

         BEFORE ROW
            IF(r_modifica_detalle_contrato[ARR_CURR()].v_seleccionado AND
               r_modifica_detalle_contrato[ARR_CURR()].v_tipo_dato = 'A')THEN # Tipo arreglo
               CALL DIALOG.setActionActive("btn_bloque_arreglo",TRUE)               
            ELSE
               CALL DIALOG.setActionActive("btn_bloque_arreglo",FALSE)
            END IF

         AFTER FIELD orden
            IF(r_modifica_detalle_contrato[ARR_CURR()].v_seleccionado AND 
              (r_modifica_detalle_contrato[ARR_CURR()].v_orden IS NULL OR 
                r_modifica_detalle_contrato[ARR_CURR()].v_orden = 0))THEN
               CALL fn_mensaje("AVISO","Capture orden","information")
               NEXT FIELD orden
            END IF

         AFTER FIELD cve_natural
            IF(r_modifica_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               r_modifica_detalle_contrato[ARR_CURR()].v_cve_natural IS NULL)THEN
               CALL fn_mensaje("AVISO","Capture clave natural","information")
               NEXT FIELD cve_natural
            END IF

         AFTER FIELD etiqueta
            IF(r_modifica_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               r_modifica_detalle_contrato[ARR_CURR()].v_etiqueta IS NULL)THEN
               CALL fn_mensaje("AVISO","Capture etiqueta","information")
               NEXT FIELD etiqueta
            END IF

         AFTER FIELD tipo_dato
            IF(r_modifica_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               r_modifica_detalle_contrato[ARR_CURR()].v_tipo_dato IS NULL)THEN
               CALL fn_mensaje("AVISO","Capture tipo dato","information")
               NEXT FIELD tipo_dato
            END IF

         AFTER FIELD ind_sentido            
            IF(r_modifica_detalle_contrato[ARR_CURR()].v_seleccionado AND ( 
               r_modifica_detalle_contrato[ARR_CURR()].v_sentido IS NULL OR
               r_modifica_detalle_contrato[ARR_CURR()].v_sentido = 0))THEN
               CALL fn_mensaje("AVISO","Capture sentido de información","information")
               NEXT FIELD ind_sentido
            END IF

         AFTER FIELD ind_opcional 
            IF(r_modifica_detalle_contrato[ARR_CURR()].v_seleccionado AND  
               r_modifica_detalle_contrato[ARR_CURR()].v_opcional IS NULL )THEN
               CALL fn_mensaje("AVISO","Capture tipo opcional","information")
               NEXT FIELD ind_opcional
            END IF

         # limpia registro si se des selecciona combo seleccion
         ON CHANGE seleccion
             IF NOT( r_modifica_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
                LET r_modifica_detalle_contrato[ARR_CURR()].v_orden       = NULL
                LET r_modifica_detalle_contrato[ARR_CURR()].v_cve_natural = NULL
                LET r_modifica_detalle_contrato[ARR_CURR()].v_etiqueta    = NULL
                LET r_modifica_detalle_contrato[ARR_CURR()].v_tipo_dato   = NULL
                LET r_modifica_detalle_contrato[ARR_CURR()].v_sentido     = 1 # ida
                LET r_modifica_detalle_contrato[ARR_CURR()].v_opcional    = 0 # requerido
             END IF

         ON CHANGE cve_natural
            IF NOT( r_modifica_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_modifica_detalle_contrato[ARR_CURR()].v_cve_natural = NULL
               CALL fn_mensaje("AVISO","Registro no seleccionado","information")
            END IF

         ON CHANGE etiqueta
            IF NOT( r_modifica_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_modifica_detalle_contrato[ARR_CURR()].v_etiqueta = NULL
               CALL fn_mensaje("AVISO","Registro no seleccionado","information")
            END IF

         ON CHANGE tipo_dato
            IF NOT( r_modifica_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_modifica_detalle_contrato[ARR_CURR()].v_tipo_dato = NULL
               CALL fn_mensaje("AVISO","Registro no seleccionado","information")
            END IF
            IF(r_modifica_detalle_contrato[ARR_CURR()].v_seleccionado)THEN
               IF(r_modifica_detalle_contrato[ARR_CURR()].v_tipo_dato = 'A')THEN # Arreglo
                  LET v_indice = ARR_CURR() # almacena en variable para no érder la referencia del indice, despues de llamar otra ventana
                  CALL fn_administra_captura_bloque('A', # alta
                                                    v_bloque,
                                                    v_detalle_bloque,
                                                    r_cabeza_operacion.v_id_tipo_contrato, # 1 = Genérico, 2 = Iniciador
                                                    r_modifica_detalle_contrato[v_indice].*,
                                                    v_indice) # posición (indice) actual del contrato
                                                      RETURNING r_capturo,
                                                                v_bloque,
                                                                v_detalle_bloque
                  IF NOT(r_capturo)THEN
                     INITIALIZE r_modifica_detalle_contrato[v_indice].v_tipo_dato TO NULL
                     NEXT FIELD tipo_dato
                  END IF
               ELSE
                  # elimina el registro de arreglo temporal
                  CALL fn_elimina_bloque_tmp(ARR_CURR(), # el indice del contrato se relacona al bloque
                                             v_bloque,
                                             v_detalle_bloque) RETURNING v_bloque,
                                                                         v_detalle_bloque
               END IF
            END IF

         ON CHANGE orden
            IF NOT( r_modifica_detalle_contrato[ARR_CURR()].v_seleccionado )THEN
               LET r_modifica_detalle_contrato[ARR_CURR()].v_orden = NULL
               CALL fn_mensaje("AVISO","Registro no seleccionado","information")
            END IF         
            LET g_orden = v_cb_orden.getItemText(GET_FLDBUF(orden))
            IF (g_orden is null or g_orden = " " or 
               g_orden = "") THEN
            ELSE    
               FOR i = 1 TO r_modifica_detalle_contrato.getLength()
                  IF i = ARR_CURR() THEN
                     CONTINUE FOR
                  END IF
                  IF g_orden = r_modifica_detalle_contrato[i].v_orden THEN
                     CALL fn_mensaje("AVISO","Posición de orden ya elegida","information")
                     LET r_modifica_detalle_contrato[ARR_CURR()].v_orden = ""                     
                     EXIT FOR                     
                  END IF
               END FOR
            END IF
           
         ON ACTION btn_bloque_arreglo            
            IF(r_modifica_detalle_contrato[ARR_CURR()].v_seleccionado AND 
               r_modifica_detalle_contrato[ARR_CURR()].v_tipo_dato = 'A')THEN
               LET v_indice = ARR_CURR() # almacena en variable para no érder la referencia del indice, despues de llamar otra ventana
               CALL fn_administra_captura_bloque('M', # modificar
                                                 v_bloque,
                                                 v_detalle_bloque,
                                                 r_cabeza_operacion.v_id_tipo_contrato, # 1 = Genérico, 2 = Iniciador
                                                 r_modifica_detalle_contrato[v_indice].*,
                                                 v_indice) # posición (indice) actual del contrato
                                                   RETURNING r_capturo,
                                                             v_bloque,
                                                             v_detalle_bloque
               IF NOT(r_capturo)THEN
                  INITIALIZE r_modifica_detalle_contrato[v_indice].v_tipo_dato TO NULL
                  NEXT FIELD tipo_dato
               END IF  
            END IF
            
      END INPUT

      BEFORE DIALOG         
         CALL v_forma.setElementText("gpo_proceso","Proceso")
         CALL v_forma.setElementText("gpo_operacion","Operación")
         CALL v_forma.setElementText("gpo_encabezado_ws","Encabezado operación")
         CALL v_forma.setElementText("gpo_contrato","Modificar contrato")
         CALL v_forma.setElementText("gpo_detalle_contrato","Modificar detalle de contrato")
   
         # Recupera los datos del proceso
         CALL fn_recupera_proceso(p_proceso) RETURNING r_proceso.*
         DISPLAY r_proceso.v_cod_proceso  TO cod_proceso_bus
         DISPLAY r_proceso.v_desc_proceso TO desc_proceso_bus

         # Recupera los datos de la operación
         CALL fn_recupera_operacion(p_proceso,p_operacion) RETURNING r_operacion.*
         CALL fn_recupera_cza_operacion(p_operacion) RETURNING r_cabeza_operacion.*
         DISPLAY r_cabeza_operacion.v_id_sistema    TO id_sistema   
         DISPLAY r_cabeza_operacion.v_id_business   TO id_business 
         DISPLAY r_cabeza_operacion.v_id_portafolio TO id_portafolio
         DISPLAY r_cabeza_operacion.v_id_servicio   TO id_servicio
         DISPLAY r_cabeza_operacion.v_id_cliente    TO id_cliente
         DISPLAY r_cabeza_operacion.v_id_canal      TO id_canal
         DISPLAY r_cabeza_operacion.v_url_servicio  TO url_servicio
         DISPLAY r_cabeza_operacion.v_num_reintento TO num_reintento
         DISPLAY r_cabeza_operacion.v_intervalo     TO intervalo
         DISPLAY r_cabeza_operacion.v_id_tipo_contrato  TO tipo_contrato
         DISPLAY r_cabeza_operacion.v_programa_servicio TO programa_servicio
         
         DISPLAY r_operacion.v_cod_opera_bus  TO cod_opera_bus
         DISPLAY r_operacion.v_desc_opera_bus TO desc_opera_bus
         
         # Recupera contrato
         CALL fn_recupera_contrato(p_operacion,
                                   p_contrato) RETURNING v_modifica_contrato.*, 
                                                         r_modifica_detalle_contrato
         # Recupera bloque y detalle de bloque
         CALL fn_recupera_conjunto_bloques(r_modifica_detalle_contrato) RETURNING v_bloque,
                                                                                  v_detalle_bloque
                             
         LET v_cb_orden     = ui.ComboBox.forName("formonly.orden")
         LET v_tam_arreglo  = r_modifica_detalle_contrato.getLength()
         CALL fn_llena_cb_orden(v_cb_orden,v_tam_arreglo,r_modifica_detalle_contrato)

         LET v_cb_tipo_dato = ui.ComboBox.forName("formonly.tipo_dato")
         CALL fn_llena_cb_tipo_dato(v_cb_tipo_dato)

         # 1 = Genérico, 2 = Iniciador
         IF(r_cabeza_operacion.v_id_tipo_contrato = 2)THEN
            CALL v_forma.setFieldHidden("programa_servicio",FALSE)
            CALL v_forma.setFieldHidden("ind_sentido",FALSE)
            CALL v_forma.setFieldHidden("ind_opcional",TRUE)
            CALL v_forma.setElementHidden("programa",FALSE)
            LET v_cb_programa_servicio = ui.ComboBox.forName("formonly.programa_servicio")
            CALL fn_llena_cb_programa_servicio(v_cb_programa_servicio,p_modulo)
         ELSE
            CALL v_forma.setFieldHidden("programa_servicio",TRUE)
            CALL v_forma.setFieldHidden("ind_sentido",TRUE)
            CALL v_forma.setFieldHidden("ind_opcional",FALSE)
            CALL v_forma.setElementHidden("programa",TRUE)
         END IF

      ON ACTION aceptar
         # verifica que se haya capturado
         IF(v_modifica_contrato.v_desc_contrato IS NULL OR v_modifica_contrato.v_desc_contrato = " ")THEN
            CALL fn_mensaje("AVISO","Capture descripción de contrato","about")
            NEXT FIELD desc_contrato
         END IF
      
         IF(v_modifica_contrato.v_f_ini_vigencia IS NULL OR v_modifica_contrato.v_f_ini_vigencia = " ")THEN
            CALL fn_mensaje("AVISO","Capture fecha inicio de vigencia","about")
            NEXT FIELD f_ini_vigencia
         END IF
      
         IF(v_modifica_contrato.v_id_cat_bus_negocio IS NULL OR v_modifica_contrato.v_id_cat_bus_negocio = " ")THEN
            CALL fn_mensaje("AVISO","Capture procedimiento","about")
            NEXT FIELD programa
         END IF
      
         IF(v_modifica_contrato.v_entidad_negocio IS NULL OR v_modifica_contrato.v_entidad_negocio = " ")THEN
            CALL fn_mensaje("AVISO","Capture entidad negocio","about")
            NEXT FIELD programa
         END IF
         
         CALL fn_verifica_entidad_negocio(v_modifica_contrato.v_entidad_negocio) RETURNING r_existe
         IF NOT(r_existe)THEN
            CALL fn_mensaje("AVISO","No exite entidad negocio","about")
            NEXT FIELD programa
         END IF
         # Valida que se hayan capturado los campos
         FOR v_indice = 1 TO r_modifica_detalle_contrato.getLength()
            # si el registro esta seleccionado
            IF(r_modifica_detalle_contrato[v_indice].v_seleccionado = TRUE)THEN 
			   IF( r_modifica_detalle_contrato[v_indice].v_orden IS NULL )THEN
                  CALL fn_mensaje("AVISO","Capture orden","about")
                  NEXT FIELD orden
               END IF 
               IF( r_modifica_detalle_contrato[v_indice].v_cve_natural IS NULL )THEN
                  CALL fn_mensaje("AVISO","Capture clave natural","about")
                  NEXT FIELD cve_natural
               END IF 
               IF( r_modifica_detalle_contrato[v_indice].v_etiqueta IS NULL )THEN
                  CALL fn_mensaje("AVISO","Capture etiqueta","about")
                  NEXT FIELD etiqueta
               END IF
               IF( r_modifica_detalle_contrato[v_indice].v_tipo_dato IS NULL )THEN
                  CALL fn_mensaje("AVISO","Capture tipo dato","about")
                  NEXT FIELD tipo_dato
               END IF
               IF(r_cabeza_operacion.v_id_tipo_contrato = 2 AND # iniciador
                  r_modifica_detalle_contrato[v_indice].v_sentido = 0)THEN
                  CALL fn_mensaje("AVISO","Capture sentido de información","about")
                  NEXT FIELD ind_sentido
               END IF

               IF(r_cabeza_operacion.v_id_tipo_contrato = 1 AND # Genérico
                  r_modifica_detalle_contrato[v_indice].v_opcional IS NULL)THEN
                  CALL fn_mensaje("AVISO","Capture tipo opcional","about")
                  NEXT FIELD ind_opcional
               END IF
            END IF
         END FOR
         # confirma actualización de operacion
         CALL fn_ventana_confirma("AVISO","¿Modificar contrato?","about")RETURNING r_confirma
         IF(r_confirma)THEN            
            # se actualiza el registro
            CALL fn_modifica_contrato(p_operacion,
                                      v_modifica_contrato.*,
                                      r_modifica_detalle_contrato,
                                      v_bloque,
                                      v_detalle_bloque)RETURNING v_error_sql
            IF(v_error_sql)THEN
               # si ha ocurrido un error
               CALL fn_mensaje("AVISO","Ocurrió un error al modificar contrato","about")
            ELSE
               CALL fn_mensaje("AVISO","Contrato modificado correctamente","about")
               LET v_actualiza_arbol = TRUE
               ACCEPT DIALOG
            END IF
         END IF
   
      ON ACTION cancelar
         # sale de modificacion de operación
         EXIT DIALOG
         
   END DIALOG
   
   RETURN v_actualiza_arbol
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para modificar el proceso en BD                  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_modifica_contrato(p_operacion,
                              p_contrato,
                              p_detalle_contrato,
                              p_bloque,
                              p_detalle_bloque)
DEFINE p_operacion   LIKE cat_bus_operacion.id_cat_bus_proceso,
       p_contrato RECORD
         v_id_cat_bus_operacion LIKE cat_bus_contrato.id_cat_bus_operacion,
         v_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_cod_contrato         LIKE cat_bus_contrato.cod_contrato,
         v_desc_contrato        LIKE cat_bus_contrato.desc_contrato,
         v_ind_vigencia         LIKE cat_bus_contrato.ind_vigencia,
         v_f_ini_vigencia       LIKE cat_bus_contrato.f_ini_vigencia,
         v_f_fin_vigencia       LIKE cat_bus_contrato.f_fin_vigencia,
         v_id_cat_bus_negocio   LIKE cat_bus_contrato.id_cat_bus_negocio,
         v_programa             LIKE cat_bus_negocio.programa,
         v_entidad_negocio      LIKE cat_bus_contrato.entidad_negocio
       END RECORD,
       p_detalle_contrato DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_seleccionado               BOOLEAN,
         v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
         v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
         v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
         v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                      LIKE cat_bus_detalle_contrato.orden,
         v_sentido                    LIKE cat_bus_detalle_contrato.ind_sentido,
         v_opcional                    LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD,
       # bloque para datos de tipo arreglo
       p_bloque DYNAMIC ARRAY OF RECORD
	     v_id_cat_bus_entidad           LIKE cat_bus_bloque.id_cat_bus_bloque,
		 v_id_cat_bus_detalle_contrato LIKE cat_bus_bloque.id_cat_bus_detalle_contrato,
	     v_cve_natural                 LIKE cat_bus_bloque.cve_natural,
		 v_desc_bloque                 LIKE cat_bus_bloque.desc_bloque,
		 v_ent_negocio_bloque          LIKE cat_bus_bloque.ent_negocio_bloque	   
	   END RECORD,
       p_detalle_bloque DYNAMIC ARRAY OF RECORD
	      v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
		  v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
          v_seleccionado               BOOLEAN,
		  v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
		  v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
		  v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
		  v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
		  v_orden                      LIKE cat_bus_detalle_contrato.orden,
          v_opcional                    LIKE cat_bus_detalle_contrato.ind_opcional
	   END RECORD,
       v_error_sql       BOOLEAN

   LET v_error_sql =  FALSE
  
   # Actualiza el proceso
   EXECUTE prp_actualiza_baja_contrato USING p_contrato.v_id_cat_bus_operacion,
                                             p_contrato.v_id_cat_bus_contrato
    
   IF(SQLCA.SQLCODE <> 0)THEN
      # Ocurrió un error
      LET v_error_sql =  TRUE
   ELSE
      CALL fn_agrega_contrato(p_operacion,
                              p_contrato.*,
                              p_detalle_contrato,
                              p_bloque,
                              p_detalle_bloque)RETURNING v_error_sql
      
   END IF
   RETURN v_error_sql
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para validar si ya existe el código de proceso   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_existe_cod_proceso(p_modulo,p_cod_proceso)
DEFINE p_modulo          LIKE cat_bus_proceso.modulo_cod,
       p_cod_proceso     LIKE cat_bus_proceso.cod_proceso_bus,
       v_existe_registro BOOLEAN

   LET v_existe_registro = FALSE
   
   EXECUTE prp_existe_cod_proceso USING p_modulo,
                                        p_cod_proceso
                                   INTO v_existe_registro 
     
   RETURN v_existe_registro
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para validar si ya existe el código de operación #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_existe_cod_operacion(p_cod_proceso,p_cod_operacion)
DEFINE p_cod_proceso     LIKE cat_bus_proceso.cod_proceso_bus,
       p_cod_operacion   LIKE cat_bus_operacion.cod_opera_bus,
       v_existe_registro BOOLEAN

   LET v_existe_registro = FALSE
   
   EXECUTE prp_existe_cod_operacion USING p_cod_proceso,
                                          p_cod_operacion
                                     INTO v_existe_registro 
         
   RETURN v_existe_registro
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para capturar los datos del tipo arreglo de un   #
#                     contrato                                                 #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_captura_detalle_tipo_arreglo(p_tipo_operacion,
                                         p_id_cat_bus_detalle_contrato,
                                         p_cve_natural_contrato,
                                         p_bloque_principal,
                                         p_detalle_bloque_principal,
                                         p_tipo_contrato)
DEFINE p_tipo_operacion CHAR(1),
       p_id_cat_bus_detalle_contrato LIKE cat_bus_bloque.id_cat_bus_detalle_contrato,
       p_cve_natural_contrato        LIKE cat_bus_detalle_contrato.cve_natural,
       p_bloque_principal RECORD
	     v_id_cat_bus_entidad           LIKE cat_bus_bloque.id_cat_bus_bloque,
		 v_id_cat_bus_detalle_contrato LIKE cat_bus_bloque.id_cat_bus_detalle_contrato,
	     v_cve_natural                 LIKE cat_bus_bloque.cve_natural,
		 v_desc_bloque                 LIKE cat_bus_bloque.desc_bloque,
		 v_ent_negocio_bloque          LIKE cat_bus_bloque.ent_negocio_bloque	   
	   END RECORD,
       p_detalle_bloque_principal DYNAMIC ARRAY OF RECORD
	      v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
		  v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
          v_seleccionado               BOOLEAN,
		  v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
		  v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
		  v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
		  v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
		  v_orden                      LIKE cat_bus_detalle_contrato.orden,
          v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
	   END RECORD,
       p_tipo_contrato SMALLINT,
       v_indice  SMALLINT,
       r_existe  BOOLEAN,
	   v_capturo BOOLEAN   
  

   OPEN WINDOW vtna_captura_arreglo WITH FORM v_ruta_ejecutables CLIPPED||"/PRTM032" --ATTRIBUTES(STYLE='dialog')
   
      DIALOG ATTRIBUTES(UNBUFFERED)
         # Bloque
         INPUT p_bloque_principal.v_cve_natural,
               p_bloque_principal.v_desc_bloque,
	           p_bloque_principal.v_ent_negocio_bloque
	      FROM cve_natural,
               desc_bloque,
	           ent_negocio_bloque ATTRIBUTES(WITHOUT DEFAULTS)

            AFTER FIELD ent_negocio_bloque
               # verifica que se haya capturado 
               IF(p_bloque_principal.v_ent_negocio_bloque IS NULL OR p_bloque_principal.v_ent_negocio_bloque = " ")THEN
                  CALL fn_mensaje("AVISO","Capture entidad de negocio","about")
                  NEXT FIELD ent_negocio_bloque
               END IF
               CALL fn_verifica_entidad_negocio(p_bloque_principal.v_ent_negocio_bloque) RETURNING r_existe
               IF NOT(r_existe)THEN
                  CALL fn_mensaje("AVISO","No exite entidad de negocio","about")
                  NEXT FIELD ent_negocio_bloque
               END IF
               IF(p_detalle_bloque_principal.getLength() < 1)THEN
                  CALL fn_mensaje("AVISO","No exite estructura de negocio","information")
               END IF
   
            ON CHANGE ent_negocio_bloque
               CALL p_detalle_bloque_principal.clear()
               CALL fn_consulta_detalle_lista_entidad(p_bloque_principal.v_ent_negocio_bloque,
                                                      p_cve_natural_contrato,
                                                      p_bloque_principal.v_cve_natural) RETURNING p_detalle_bloque_principal
               
           
   	     END INPUT
         
		 # Detalle de bloque
		 INPUT ARRAY p_detalle_bloque_principal FROM sr_detalle_bloque.* ATTRIBUTES(APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE)
            BEFORE INPUT      
               LET v_cb_orden     = ui.ComboBox.forName("formonly.orden")      
               LET v_tam_arreglo  = p_detalle_bloque_principal.getLength()
               CALL fn_llena_cb_orden_bloque(v_cb_orden,
                                             v_tam_arreglo,
                                             p_detalle_bloque_principal)
               LET v_cb_tipo_dato = ui.ComboBox.forName("formonly.tipo_dato")
               CALL fn_llena_cb_tipo_dato(v_cb_tipo_dato)

            AFTER FIELD orden
               IF(p_detalle_bloque_principal[ARR_CURR()].v_seleccionado AND 
                  (p_detalle_bloque_principal[ARR_CURR()].v_orden IS NULL OR 
                   p_detalle_bloque_principal[ARR_CURR()].v_orden = 0))THEN
                  CALL fn_mensaje("AVISO","Capture orden","information")
                  NEXT FIELD orden
               END IF

            AFTER FIELD cve_natural
                IF(p_detalle_bloque_principal[ARR_CURR()].v_seleccionado AND 
                   p_detalle_bloque_principal[ARR_CURR()].v_cve_natural IS NULL)THEN
                  CALL fn_mensaje("AVISO","Capture clave natural","information")
                  NEXT FIELD cve_natural
                END IF

            AFTER FIELD etiqueta
               IF(p_detalle_bloque_principal[ARR_CURR()].v_seleccionado AND 
                  p_detalle_bloque_principal[ARR_CURR()].v_etiqueta IS NULL)THEN
                  CALL fn_mensaje("AVISO","Capture etiqueta","information")
                  NEXT FIELD etiqueta
               END IF

            AFTER FIELD tipo_dato
               IF(p_detalle_bloque_principal[ARR_CURR()].v_seleccionado AND 
                  p_detalle_bloque_principal[ARR_CURR()].v_tipo_dato IS NULL)THEN
                  CALL fn_mensaje("AVISO","Capture tipo dato","information")
                  NEXT FIELD tipo_dato
               END IF

            AFTER FIELD ind_opcional
               IF(p_detalle_bloque_principal[ARR_CURR()].v_seleccionado AND
                  p_tipo_contrato = 1 AND # Genérico 
                  p_detalle_bloque_principal[ARR_CURR()].v_opcional IS NULL)THEN
                  CALL fn_mensaje("AVISO","Capture tipo opcional","information")
                  NEXT FIELD ind_opcional
               END IF

            # limpia registro si se des selecciona combo seleccion
            ON CHANGE seleccion
               IF NOT( p_detalle_bloque_principal[ARR_CURR()].v_seleccionado )THEN
                  LET p_detalle_bloque_principal[ARR_CURR()].v_orden       = NULL
                  LET p_detalle_bloque_principal[ARR_CURR()].v_cve_natural = NULL
                  LET p_detalle_bloque_principal[ARR_CURR()].v_etiqueta    = NULL
                  LET p_detalle_bloque_principal[ARR_CURR()].v_opcional    = 0 # Requerido                  
               END IF
             
            ON CHANGE cve_natural
               IF NOT( p_detalle_bloque_principal[ARR_CURR()].v_seleccionado )THEN
                  LET p_detalle_bloque_principal[ARR_CURR()].v_cve_natural = NULL
                  CALL fn_mensaje("AVISO","Registro no seleccionado","information")
               END IF

            ON CHANGE etiqueta
               IF NOT( p_detalle_bloque_principal[ARR_CURR()].v_seleccionado )THEN
                  LET p_detalle_bloque_principal[ARR_CURR()].v_etiqueta = NULL
                  CALL fn_mensaje("AVISO","Registro no seleccionado","information")
               END IF

            ON CHANGE tipo_dato
               IF NOT( p_detalle_bloque_principal[ARR_CURR()].v_seleccionado )THEN
                  LET p_detalle_bloque_principal[ARR_CURR()].v_tipo_dato = NULL
                  CALL fn_mensaje("AVISO","Registro no seleccionado","information")
               END IF

            ON CHANGE orden
               IF NOT( p_detalle_bloque_principal[ARR_CURR()].v_seleccionado )THEN
                  LET p_detalle_bloque_principal[ARR_CURR()].v_orden = NULL
                  CALL fn_mensaje("AVISO","Registro no seleccionado","information")
               END IF
          
               LET g_orden = v_cb_orden.getItemText(GET_FLDBUF(orden))            
               IF( g_orden is null or g_orden = " " OR 
                   g_orden = "") THEN
               ELSE    
                  FOR i = 1 TO p_detalle_bloque_principal.getLength()
                     IF i = ARR_CURR() THEN
                        CONTINUE FOR
                     END IF
                     IF g_orden = p_detalle_bloque_principal[i].v_orden THEN
                        CALL fn_mensaje("AVISO","Posición de orden ya elegida","information")
                        LET p_detalle_bloque_principal[ARR_CURR()].v_orden = ""                     
                        EXIT FOR                     
                     END IF
                  END FOR
               END IF
		 END INPUT
		 
		 BEFORE DIALOG
            LET v_ventana = ui.Window.getCurrent()
            LET v_forma = v_ventana.getForm() 
            IF(p_titulo_ventana IS NOT NULL)THEN
               CALL v_ventana.setText(p_titulo_ventana)
               CALL ui.Interface.setText(p_titulo_ventana)
            END IF
                        
            DISPLAY p_bloque_principal.v_cve_natural        TO cve_natural
            DISPLAY p_bloque_principal.v_desc_bloque        TO desc_bloque
            DISPLAY p_bloque_principal.v_ent_negocio_bloque TO ent_negocio_bloque

            LET v_cb_orden     = ui.ComboBox.forName("formonly.orden")      
            LET v_tam_arreglo  = p_detalle_bloque_principal.getLength()
            CALL fn_llena_cb_orden_bloque(v_cb_orden,
                                          v_tam_arreglo,
                                          p_detalle_bloque_principal)
            LET v_cb_tipo_dato = ui.ComboBox.forName("formonly.tipo_dato")
            CALL fn_llena_cb_tipo_dato(v_cb_tipo_dato)
            IF( p_tipo_contrato = 1 )THEN # Genérico
               CALL v_forma.setFieldHidden("ind_opcional",FALSE)
            ELSE # iniciador
               CALL v_forma.setFieldHidden("ind_opcional",TRUE)
            END IF
            # indica si se realizó captura de información
		    LET v_capturo = FALSE
		 
		 ON ACTION aceptar
            # Verefica que se haya capturado algún registro
            FOR v_indice = 1 TO p_detalle_bloque_principal.getLength()
               IF(p_detalle_bloque_principal[v_indice].v_seleccionado)THEN
                  LET v_capturo = TRUE
                  
			      IF( p_detalle_bloque_principal[v_indice].v_orden IS NULL )THEN
                     CALL fn_mensaje("AVISO","Capture orden","about")
                     NEXT FIELD orden
                  END IF 
			   
                  IF( p_detalle_bloque_principal[v_indice].v_cve_natural IS NULL )THEN
                     CALL fn_mensaje("AVISO","Capture clave natural","about")
                     NEXT FIELD cve_natural
                  END IF 

                  IF( p_detalle_bloque_principal[v_indice].v_etiqueta IS NULL )THEN
                     CALL fn_mensaje("AVISO","Capture etiqueta","about")
                     NEXT FIELD etiqueta
                  END IF

                  IF( p_detalle_bloque_principal[v_indice].v_tipo_dato IS NULL )THEN
                     CALL fn_mensaje("AVISO","Capture tipo dato","about")
                     NEXT FIELD tipo_dato
                  END IF

                  IF( p_tipo_contrato = 1 AND # Genérico
                      p_detalle_bloque_principal[v_indice].v_opcional IS NULL)THEN
                     CALL fn_mensaje("AVISO","Capture tipo opcional","about")
                     NEXT FIELD ind_opcional
                  END IF
               END IF
            END FOR
            IF NOT(v_capturo)THEN
               CALL fn_mensaje("Aviso","Almenos debe seleccionar algún registro","information")
               CONTINUE DIALOG
            END IF
		    ACCEPT DIALOG
		 
		 ON ACTION cancelar
		    LET v_capturo = FALSE		 
		    EXIT DIALOG
		 
      END DIALOG
   
   CLOSE WINDOW vtna_captura_arreglo
   
   RETURN v_capturo,
          p_bloque_principal.*,
          p_detalle_bloque_principal

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para consultar los datos del bloque y su detalle #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_consulta_detalle_tipo_arreglo(p_bloque_principal,
                                          p_detalle_bloque_principal,
                                          p_id_tipo_contrato)
DEFINE p_bloque_principal RECORD
	     v_id_cat_bus_entidad           LIKE cat_bus_bloque.id_cat_bus_bloque,
		 v_id_cat_bus_detalle_contrato LIKE cat_bus_bloque.id_cat_bus_detalle_contrato,
	     v_cve_natural                 LIKE cat_bus_bloque.cve_natural,
		 v_desc_bloque                 LIKE cat_bus_bloque.desc_bloque,
		 v_ent_negocio_bloque          LIKE cat_bus_bloque.ent_negocio_bloque	   
	   END RECORD,
       p_detalle_bloque_principal DYNAMIC ARRAY OF RECORD
	      v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
		  v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
          v_seleccionado              BOOLEAN,
		  v_cve_natural               LIKE cat_bus_detalle_contrato.cve_natural,
		  v_etiqueta                  LIKE cat_bus_detalle_contrato.etiqueta,
		  v_atributo_negocio          LIKE cat_bus_detalle_contrato.atributo_negocio,
		  v_tipo_dato                 LIKE cat_bus_detalle_contrato.tipo_dato,
		  v_orden                     LIKE cat_bus_detalle_contrato.orden,
          v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
	   END RECORD,
       p_id_tipo_contrato SMALLINT
             

   OPEN WINDOW vtna_consulta_arreglo WITH FORM v_ruta_ejecutables CLIPPED||"/PRTM032" --ATTRIBUTES(STYLE='dialog')
      DISPLAY ARRAY p_detalle_bloque_principal TO sr_detalle_bloque.* ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

         BEFORE DISPLAY 
            LET v_ventana = ui.Window.getCurrent()
            LET v_forma = v_ventana.getForm()
            IF(p_titulo_ventana IS NOT NULL)THEN
               CALL v_ventana.setText(p_titulo_ventana)
            END IF
            DISPLAY p_bloque_principal.v_cve_natural        TO cve_natural
            DISPLAY p_bloque_principal.v_desc_bloque        TO desc_bloque
            DISPLAY p_bloque_principal.v_ent_negocio_bloque TO ent_negocio_bloque
            LET v_cb_orden     = ui.ComboBox.forName("formonly.orden")      
            LET v_tam_arreglo  = p_detalle_bloque_principal.getLength()
            CALL fn_llena_cb_orden_bloque(v_cb_orden,
                                          v_tam_arreglo,
                                          p_detalle_bloque_principal)
            LET v_cb_tipo_dato = ui.ComboBox.forName("formonly.tipo_dato")
            CALL fn_llena_cb_tipo_dato(v_cb_tipo_dato)
            # 1 = Genérico, 2 = Iniciador
            IF(p_id_tipo_contrato = 1)THEN
               CALL v_forma.setFieldHidden("ind_opcional",FALSE)
            ELSE
               CALL v_forma.setFieldHidden("ind_opcional",TRUE)
            END IF

         ON ACTION aceptar
            ACCEPT DISPLAY        

      END DISPLAY 
   
   CLOSE WINDOW vtna_consulta_arreglo

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para capturar los datos del tipo arreglo de los  #
#                     contratos                                                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_recupera_conjunto_bloques(p_detalle_contrato)
DEFINE p_detalle_contrato DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
         v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
         v_seleccionado               BOOLEAN,
         v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
         v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
         v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
         v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
         v_orden                      LIKE cat_bus_detalle_contrato.orden,
         v_sentido                    LIKE cat_bus_detalle_contrato.ind_sentido,
         v_opcional                    LIKE cat_bus_detalle_contrato.ind_opcional
       END RECORD,
       v_bloque DYNAMIC ARRAY OF RECORD
	     v_id_cat_bus_entidad           LIKE cat_bus_bloque.id_cat_bus_bloque,
		 v_id_cat_bus_detalle_contrato LIKE cat_bus_bloque.id_cat_bus_detalle_contrato,
	     v_cve_natural                 LIKE cat_bus_bloque.cve_natural,
		 v_desc_bloque                 LIKE cat_bus_bloque.desc_bloque,
		 v_ent_negocio_bloque          LIKE cat_bus_bloque.ent_negocio_bloque	   
	   END RECORD,
       v_detalle_bloque DYNAMIC ARRAY OF RECORD
	      v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
		  v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
          v_seleccionado               BOOLEAN,
		  v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
		  v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
		  v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
		  v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
		  v_orden                      LIKE cat_bus_detalle_contrato.orden,
          v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
	   END RECORD,
       v_detalle_bloque_tmp DYNAMIC ARRAY OF RECORD
	      v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
		  v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
          v_seleccionado               BOOLEAN,
		  v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
		  v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
		  v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
		  v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
		  v_orden                      LIKE cat_bus_detalle_contrato.orden,
          v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
	   END RECORD,
	   v_indice            SMALLINT,
       v_indice_det_bloque SMALLINT
	   
   # Recupera detalle de bloque por cada elemento del contrato de tipo array
   FOR v_indice = 1 TO p_detalle_contrato.getLength() 
      IF( p_detalle_contrato[v_indice].v_tipo_dato = 'A' )THEN # de tipo arreglo
         CALL fn_recupera_bloque(p_detalle_contrato[v_indice].v_id_cat_bus_detalle_entidad,
                                 p_detalle_contrato[v_indice].v_cve_natural) RETURNING v_bloque[v_bloque.getLength() + 1].*,
                                                                                       v_detalle_bloque_tmp
         # asigna un indice temporal al bloque
         LET v_bloque[v_bloque.getLength()].v_id_cat_bus_entidad          = v_bloque.getLength()
         # asigna a bloque indice temporal del contrato para relacionar los datos
         LET v_bloque[v_bloque.getLength()].v_id_cat_bus_detalle_contrato = v_indice
         FOR v_indice_det_bloque = 1 TO v_detalle_bloque_tmp.getLength() # llena arreglo principal de detalle de bloque por cada contrato
            IF(v_detalle_bloque_tmp[v_indice_det_bloque].v_seleccionado)THEN
               
               LET v_detalle_bloque[v_detalle_bloque.getLength() + 1].v_seleccionado   = v_detalle_bloque_tmp[v_indice_det_bloque].v_seleccionado
               # asigna tamaño de arreglo bloque a detalle de bloque para realcionar datos con indice temporal
               LET v_detalle_bloque[v_detalle_bloque.getLength()].v_id_cat_bus_entidad = v_bloque.getLength() 
               LET v_detalle_bloque[v_detalle_bloque.getLength()].v_cve_natural        = v_detalle_bloque_tmp[v_indice_det_bloque].v_cve_natural
               LET v_detalle_bloque[v_detalle_bloque.getLength()].v_etiqueta           = v_detalle_bloque_tmp[v_indice_det_bloque].v_etiqueta
               LET v_detalle_bloque[v_detalle_bloque.getLength()].v_atributo_negocio   = v_detalle_bloque_tmp[v_indice_det_bloque].v_atributo_negocio
               LET v_detalle_bloque[v_detalle_bloque.getLength()].v_tipo_dato          = v_detalle_bloque_tmp[v_indice_det_bloque].v_tipo_dato
               LET v_detalle_bloque[v_detalle_bloque.getLength()].v_orden              = v_detalle_bloque_tmp[v_indice_det_bloque].v_orden
               LET v_detalle_bloque[v_detalle_bloque.getLength()].v_opcional           = v_detalle_bloque_tmp[v_indice_det_bloque].v_opcional
            END IF   
         END FOR
      END IF
   END FOR
   LET g_id_cat_bus_entidad = v_bloque.getLength() # inicializa con el tamaño de regitros recuperados
   
   RETURN v_bloque,
          v_detalle_bloque

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM03                                                   #
#Descripcion       => Función para capturar los datos del tipo arreglo de un   #
#                     contrato                                                 #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_recupera_bloque(p_id_cat_bus_detalle_contrato,
                            p_cve_natural_contrato)
DEFINE p_id_cat_bus_detalle_contrato LIKE cat_bus_bloque.id_cat_bus_detalle_contrato,
       p_cve_natural_contrato        LIKE cat_bus_detalle_contrato.cve_natural,
       v_bloque RECORD
	     v_id_cat_bus_entidad           LIKE cat_bus_bloque.id_cat_bus_bloque,
		 v_id_cat_bus_detalle_contrato LIKE cat_bus_bloque.id_cat_bus_detalle_contrato,
	     v_cve_natural                 LIKE cat_bus_bloque.cve_natural,
		 v_desc_bloque                 LIKE cat_bus_bloque.desc_bloque,
		 v_ent_negocio_bolque          LIKE cat_bus_bloque.ent_negocio_bloque	   
	   END RECORD,
       v_detalle_bloque DYNAMIC ARRAY OF RECORD
	      v_id_cat_bus_detalle_entidad LIKE cat_bus_detalle_contrato.id_cat_bus_detalle_contrato,
		  v_id_cat_bus_entidad         LIKE cat_bus_detalle_contrato.id_cat_bus_contrato,
          v_seleccionado               BOOLEAN,
		  v_cve_natural                LIKE cat_bus_detalle_contrato.cve_natural,
		  v_etiqueta                   LIKE cat_bus_detalle_contrato.etiqueta,
		  v_atributo_negocio           LIKE cat_bus_detalle_contrato.atributo_negocio,
		  v_tipo_dato                  LIKE cat_bus_detalle_contrato.tipo_dato,
		  v_orden                      LIKE cat_bus_detalle_contrato.orden,
          v_opcional                   LIKE cat_bus_detalle_contrato.ind_opcional
	   END RECORD

   # Recupera el bloque
   EXECUTE prp_recupera_bloque_contrato USING p_id_cat_bus_detalle_contrato
                                         INTO v_bloque.*

   CALL fn_recupera_detalle_entidad(v_bloque.v_ent_negocio_bolque, # nombre tabla de donde se obtienen los datos
                                    v_bloque.v_id_cat_bus_entidad, # id padre de tabla de detalle
                                    p_cve_natural_contrato,
                                    v_bloque.v_cve_natural)
                                        RETURNING v_detalle_bloque

   RETURN v_bloque.*,
          v_detalle_bloque
END FUNCTION
