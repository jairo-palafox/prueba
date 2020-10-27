################################################################################
#Modulo           => PRT                                                       #
#Programa         => PRTC08                                                    #
#Objetivo         => Consulta de Aclaraciones Portabilidad                     #
#Fecha de Inicio  => 10 de Noviembre de 2015                                   #
################################################################################
DATABASE "safre_viv"

DEFINE p_usuario_cod     CHAR(20),
       p_tpo_ejecucion   INTEGER,
       v_ventana         ui.Window,
       p_cad_ventana     STRING,
       v_forma           ui.Form,
       v_ruta_listados   CHAR(40),
       v_ruta_bin        CHAR(40)

DEFINE t_consulta DYNAMIC ARRAY OF RECORD
   v_id             INTEGER,
   v_nss            CHAR(11),
   v_num_caso       DECIMAL(10,0),
   v_fecha          CHAR(10),
   v_estatus        CHAR(40),
   v_tipifica       CHAR(120),   
   v_diagnostico    CHAR(120)
END RECORD
--Datos para filtrar la búsqueda
DEFINE v_nss       CHAR(11),
       v_num_caso  DECIMAL(10,0),
       v_estatus   SMALLINT,
       v_f_ini     DATE,
       v_f_fin     DATE

MAIN

   DEFINE c_status ui.ComboBox
   DEFINE c_estatus         SMALLINT
   DEFINE c_estatus_desc    CHAR(40)
   DEFINE v_total_reg       INTEGER
   
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)
   --Tabla Temporal no ha sido creada

   --Se obtiene la ruta de control del módulo
   SELECT l.ruta_listados,l.ruta_bin
      INTO v_ruta_listados,v_ruta_bin
      FROM seg_modulo l
      WHERE l.modulo_cod = 'prt'

   CALL STARTLOG(v_ruta_listados CLIPPED ||"/"||p_usuario_cod CLIPPED||".PRTC08.log")
   OPEN WINDOW vtna_consulta WITH FORM v_ruta_bin CLIPPED||"/PRTC081"
   #Se asigna el titulo de la ventana y toma el control de la forma
      IF ( p_cad_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_cad_ventana)
         LET v_ventana = ui.Window.getCurrent()
         LET v_forma = v_ventana.getForm()
         CALL v_ventana.setText(p_cad_ventana)
      END IF

      --Llenando el combo box
      LET  c_status = ui.ComboBox.forName("v_estatus")
      CALL c_status.clear()
      PREPARE prp_cb FROM "SELECT estatus_aclaracion,estatus_aclaracion_desc FROM prt_cat_estatus_aclaracion"
      DECLARE cur_cb CURSOR FOR prp_cb
      FOREACH cur_cb INTO c_estatus,c_estatus_desc
         CALL c_status.addItem(c_estatus,c_estatus_desc)
      END FOREACH
      FREE cur_cb
      FREE prp_cb

      --INICIA DIALOG
      DIALOG ATTRIBUTES(UNBUFFERED)

         INPUT BY NAME v_nss,v_num_caso,v_estatus,v_f_ini,v_f_fin

            ON ACTION aceptar
               CALL fn_llena_tabla() RETURNING v_total_reg
               IF v_total_reg > 0 THEN
                  CALL v_forma.setElementHidden("t_consulta",0)
               ELSE 
                  CALL v_forma.setElementHidden("t_consulta",1)
               END IF
    
            ON ACTION cancelar
               EXIT DIALOG
            
         END INPUT

         DISPLAY ARRAY t_consulta TO t_consulta.*
            ON ACTION aceptar
            ON ACTION reporte
               CALL fn_genera_reporte()
            ON ACTION cancelar
               EXIT DIALOG
         END DISPLAY

         BEFORE DIALOG
            CALL v_forma.setElementHidden("t_consulta",1)
            
      END DIALOG
      
  CLOSE WINDOW vtna_consulta
  
END MAIN

FUNCTION fn_llena_tabla()

   DEFINE v_query STRING
   DEFINE i         INTEGER

   CALL t_consulta.clear()
   LET v_query = ""
   --Creando tabla temporal 
   DROP TABLE IF EXISTS tmp_consulta_aclaracion_portabilidad
   CREATE TABLE tmp_consulta_aclaracion_portabilidad(
         id 					SERIAL,
         nss					CHAR(11),
         numero_caso 		DECIMAL(10,0),
         f_aclaracion    	CHAR(10),
         estatus_aclaracion 	CHAR(40),
         tipo_aclaracion     CHAR(120),
         diagnostico_aclaracion CHAR(120)
         )
   
   ---Formando el Query dependiendo de los valores de entrada 
   LET v_query = "INSERT INTO tmp_consulta_aclaracion_portabilidad SELECT 0,
	             a.nss,
                 a.numero_caso,
	             TO_CHAR((DATE(a.f_aclaracion)),'%d/%m/%Y') AS f_aclaracion,
	             (SELECT estatus_aclaracion_desc 
                    FROM prt_cat_estatus_aclaracion 
		            WHERE estatus_aclaracion = a.estatus_aclaracion),
                 (SELECT tipo_aclaracion_desc
	                FROM prt_cat_tipo_aclaracion
		            WHERE tipo_aclaracion = a.tipo_aclaracion),
                 (SELECT diagnostico_aclaracion_desc
	                FROM prt_cat_diagnostico_aclaracion
		            WHERE diagnostico_aclaracion = a.diagnostico_aclaracion)		  
                FROM prt_aclaracion a WHERE 1=1 "                
   IF v_nss IS NOT NULL THEN
      LET v_query = v_query," AND a.nss = ",v_nss
   END IF
   IF v_num_caso IS NOT NULL THEN
      LET v_query = v_query," AND a.numero_caso = ",v_num_caso
   END IF
   IF v_estatus IS NOT NULL THEN
      LET v_query = v_query," AND a.estatus_aclaracion = ",v_estatus
   END IF
   IF v_f_ini IS NOT NULL AND v_f_fin IS NULL THEN
      LET v_query = v_query," AND a.f_aclaracion::DATE >= '",v_f_ini,"'"
   END IF
   IF v_f_ini IS NULL AND v_f_fin IS NOT NULL THEN
      LET v_query = v_query," AND a.f_aclaracion::DATE <= '",v_f_fin,"'"
   END IF
   IF v_f_ini IS NOT NULL AND v_f_fin IS NOT NULL THEN
      LET v_query = v_query," AND a.f_aclaracion::DATE BETWEEN '",v_f_ini,"' AND '",v_f_fin,"'"
   END IF
      LET v_query = v_query," ORDER BY a.nss"
   
   --SE EJECUTA EL QUERY
   EXECUTE IMMEDIATE v_query

   --Se procede a llenar el arreglo dinámico
   LET i = 1
   PREPARE prp_llena_t FROM "SELECT * FROM tmp_consulta_aclaracion_portabilidad"
   DECLARE cur_llena_t CURSOR FOR prp_llena_t
   FOREACH cur_llena_t INTO t_consulta[i].*
      LET i = i+1
   END FOREACH
   CALL t_consulta.deleteElement(i)
   LET i = i-1
   
   RETURN i
   
END FUNCTION

FUNCTION fn_genera_reporte()

   DEFINE g_pid             LIKE bat_ctr_proceso.pid,     -- ID del proceso
       g_proceso_cod     LIKE cat_proceso.proceso_cod, -- Código del proceso
       g_opera_cod       LIKE cat_operacion.opera_cod, -- Código de operacion
       g_opera_cod_ant   LIKE cat_operacion.opera_cod, -- codigo de operacion anterior
       r_resultado_opera SMALLINT,
       v_folio           LIKE glo_folio.folio,
       v_comando         STRING,
       bat_ruta_listados LIKE seg_modulo.ruta_listados

   --CARGANDO EL FILTRO PARA EL FUTURO REPORTE
   DROP TABLE IF EXISTS tmp_aclaracion_filtro
   CREATE TABLE tmp_aclaracion_filtro(
      nss                  CHAR(11),
      num_caso             DECIMAL(10,0),
      estatus_aclaracion   SMALLINT,
      f_ini                DATE,
      f_fin                DATE
   )   
   INSERT INTO tmp_aclaracion_filtro VALUES(v_nss,
                                            v_num_caso,
                                            v_estatus,
                                            v_f_ini,
                                            v_f_fin)

   -- se asigna proceso y operacion
   LET g_proceso_cod = 2813
   LET g_opera_cod   = 1     
   LET g_opera_cod_ant = 0   

   SELECT b.ruta_listados
     INTO bat_ruta_listados
     FROM seg_modulo b
     WHERE b.modulo_cod = 'bat'

   CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod) RETURNING r_resultado_opera
   IF(r_resultado_opera = 0)THEN

      CALL fn_genera_pid(g_proceso_cod,g_opera_cod,p_usuario_cod) RETURNING g_pid
      CALL fn_inicializa_proceso(g_pid,
                              g_proceso_cod,
                              g_opera_cod,
                              v_folio,
                              "PRTC08",
                              "NA",
                              p_usuario_cod) RETURNING r_resultado_opera
      IF(r_resultado_opera <> 0)THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      END IF
      CALL fn_actualiza_opera_ini(g_pid,
                               g_proceso_cod,
                               g_opera_cod,
                               v_folio,
                               "PRTC08",
                               "NA",
                               p_usuario_cod)
      RETURNING r_resultado_opera
      IF ( r_resultado_opera = 0 ) THEN
         LET v_comando = " nohup fglrun ",v_ruta_bin CLIPPED,"/PRTI08.42r ",
                         p_usuario_cod CLIPPED, " ",
                         g_pid                , " ",
                         g_proceso_cod        , " ",
                         g_opera_cod          , " ",
                         v_folio              , " '",
                         "NA" CLIPPED    , "' ",
                         " 1>",bat_ruta_listados clipped,
                         "/nohup:",g_pid USING "&&&&&",":",
                         g_proceso_cod   USING "&&&&&",":",
                         g_opera_cod     USING "&&&&&" ,
                         " 2>&1 &"
                 RUN v_comando
                 IF(STATUS)THEN
                        CALL fn_mensaje(p_cad_ventana,"Ocurrio un error al generar el reporte","about")
                 ELSE
                        CALL fn_mensaje(p_cad_ventana,"Se ha enviado la operación.\nPodrá revisar el detalle en el monitoreo de procesos","about")
                 END IF
       ELSE
          # Muestra el mensaje de cual es la causa de que no se puede iniciar con la operacion
         CALL fn_muestra_inc_operacion(r_resultado_opera)
       END IF
   END IF
   
END FUNCTION