################################################################################
#Modulo           => PRT                                                       #
#Programa         => PRTC09                                                    #
#Objetivo         => Consulta de Cancelaciones de Portabilidad                 #
#Fecha de Inicio  => 23 de Noviembre de 2015                                   #
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
   v_diagnostico    CHAR(40)
END RECORD
--Datos para filtrar la búsqueda
DEFINE v_nss       CHAR(11),
       v_num_caso  CHAR(10),
       v_estatus   SMALLINT,
       v_f_ini     DATE,
       v_f_fin     DATE

MAIN

   DEFINE c_status       ui.ComboBox
   DEFINE c_estatus      SMALLINT
   DEFINE c_estatus_desc CHAR(40)
   DEFINE v_total_reg    INTEGER
   DEFINE v_valida       SMALLINT
   
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   --Se obtiene la ruta de control del módulo
   SELECT l.ruta_listados,l.ruta_bin
      INTO v_ruta_listados,v_ruta_bin
      FROM seg_modulo l
      WHERE l.modulo_cod = 'prt'

   CALL STARTLOG(v_ruta_listados CLIPPED ||"/"||p_usuario_cod CLIPPED||".PRTC09.log")
   OPEN WINDOW vtna_consulta WITH FORM v_ruta_bin CLIPPED||"/PRTC091"
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
      PREPARE prp_cb FROM "SELECT estatus_cancelacion,estatus_cancelacion_desc FROM prt_cat_estatus_cancelacion"
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
               CALL fn_valida_campos(v_nss,v_num_caso) RETURNING v_valida
               IF v_valida THEN 
                  CALL fn_llena_tabla() RETURNING v_total_reg
                  IF v_total_reg > 0 THEN
                     CALL v_forma.setElementHidden("t_consulta",0)
                     CALL DIALOG.setActionActive("reporte",1)
                  ELSE 
                     CALL fn_mensaje ("information","LA BUSQUEDA NO GENERÓ NINGUN RESULTADO","question")
                     CALL v_forma.setElementHidden("t_consulta",1)
                     CALL DIALOG.setActionActive("reporte",0)
                  END IF
                END IF
    
            ON ACTION cancelar
               EXIT DIALOG
            
         END INPUT

         DISPLAY ARRAY t_consulta TO t_consulta.*
            ON ACTION aceptar
            ON ACTION cancelar
         END DISPLAY

         BEFORE DIALOG
            CALL v_forma.setElementHidden("t_consulta",1)
            CALL DIALOG.setActionActive("reporte",0)

         ON ACTION aceptar   
         ON ACTION reporte
            CALL fn_genera_reporte()
         ON ACTION cancelar
         
            
      END DIALOG
      
  CLOSE WINDOW vtna_consulta
  
END MAIN

FUNCTION fn_llena_tabla()

   DEFINE v_query    STRING
   DEFINE i          INTEGER
   DEFINE p_num_caso DECIMAL(10,0) 

   LET p_num_caso = v_num_caso
   CALL t_consulta.clear()
   LET v_query = ""
   --Creando tabla temporal 
   DROP TABLE IF EXISTS tmp_consulta_cancelacion_portabilidad
   CREATE TABLE tmp_consulta_cancelacion_portabilidad(
         id 					 SERIAL,
         nss					 CHAR(11),
         numero_caso 		     DECIMAL(10,0),
         f_cancelacion    	     CHAR(10),
         estatus_cancelacion 	 CHAR(40),
         diagnostico_cancelacion CHAR(40)
         )
   
   ---Formando el Query dependiendo de los valores de entrada 
   LET v_query = "INSERT INTO tmp_consulta_cancelacion_portabilidad SELECT 0,
	             a.nss,
                 a.numero_caso,
	             TO_CHAR((DATE(a.f_cancelacion)),'%d/%m/%Y') AS f_cancelacion,
	             (SELECT estatus_cancelacion_desc 
                    FROM prt_cat_estatus_cancelacion 
		            WHERE estatus_cancelacion = a.estatus_cancelacion),
                 (SELECT diagnostico_cancelacion_desc
	                FROM prt_cat_diagnostico_cancelacion
		            WHERE diagnostico_cancelacion = a.diagnostico_cancelacion)		  
                FROM prt_cancelacion_crm a WHERE 1=1 "                
   IF v_nss IS NOT NULL THEN
      LET v_query = v_query," AND a.nss = ",v_nss
   END IF
   IF v_num_caso IS NOT NULL THEN
      LET v_query = v_query," AND a.numero_caso = ",p_num_caso
   END IF
   IF v_estatus IS NOT NULL THEN
      LET v_query = v_query," AND a.estatus_cancelacion = ",v_estatus
   END IF
   IF v_f_ini IS NOT NULL AND v_f_fin IS NULL THEN
      LET v_query = v_query," AND a.f_cancelacion::DATE >= '",v_f_ini,"'"
   END IF
   IF v_f_ini IS NULL AND v_f_fin IS NOT NULL THEN
      LET v_query = v_query," AND a.f_cancelacion::DATE <= '",v_f_fin,"'"
   END IF
   IF v_f_ini IS NOT NULL AND v_f_fin IS NOT NULL THEN
      LET v_query = v_query," AND a.f_cancelacion::DATE BETWEEN '",v_f_ini,"' AND '",v_f_fin,"'"
   END IF
      LET v_query = v_query," ORDER BY a.nss"
   
   --SE EJECUTA EL QUERY
   EXECUTE IMMEDIATE v_query

   --Se procede a llenar el arreglo dinámico
   LET i = 1
   PREPARE prp_llena_t FROM "SELECT * FROM tmp_consulta_cancelacion_portabilidad"
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
   DROP TABLE IF EXISTS tmp_cancelacion_filtro
   CREATE TABLE tmp_cancelacion_filtro(
      nss                  CHAR(11),
      num_caso             DECIMAL(10,0),
      estatus_cancelacion  SMALLINT,
      f_ini                DATE,
      f_fin                DATE
   )   
   INSERT INTO tmp_cancelacion_filtro VALUES(v_nss,
                                            v_num_caso,
                                            v_estatus,
                                            v_f_ini,
                                            v_f_fin)

   -- se asigna proceso y operacion
   LET g_proceso_cod = 2814
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
                              "PRTC09",
                              "NA",
                              p_usuario_cod) RETURNING r_resultado_opera
      IF(r_resultado_opera <> 0)THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      END IF
      CALL fn_actualiza_opera_ini(g_pid,
                               g_proceso_cod,
                               g_opera_cod,
                               v_folio,
                               "PRTC09",
                               "NA",
                               p_usuario_cod)
      RETURNING r_resultado_opera
      IF ( r_resultado_opera = 0 ) THEN
         LET v_comando = " nohup fglrun ",v_ruta_bin CLIPPED,"/PRTI09.42r ",
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

FUNCTION fn_valida_campos(p_nss,p_n_caso)

DEFINE p_nss       CHAR(11)
DEFINE p_nss_len   SMALLINT
DEFINE p_nss_isnum BIGINT
DEFINE p_n_caso    CHAR(10)
DEFINE p_valida    SMALLINT

LET p_valida = 1

   --// VALIDACION 2 NSS debe ser de 11 posiciones y numerico
   IF p_nss IS NOT NULL THEN
       LET p_nss_len = LENGTH(p_nss)
       LET p_nss_isnum = p_nss
       IF p_nss_len = 11 AND p_nss_isnum IS NOT NULL THEN
          LET p_valida = 1
       ELSE
          CALL fn_mensaje ("information","El NSS debe ser un valor de 11 posisiones y numérico","question")
          LET p_valida = 0
          RETURN p_valida
       END IF
   END IF

   --// VALIDACION 2 n_caso 10 posiciones y numerico
   IF p_n_caso IS NOT NULL THEN
       LET p_nss_isnum = p_n_caso
       IF p_nss_isnum IS NOT NULL THEN
          LET p_valida = 1
       ELSE
          CALL fn_mensaje ("information","El Número de Caso debe ser un valor de máximo 10 posisiones y numérico","question")
          LET p_valida = 0
          RETURN p_valida
       END IF
   END IF

   RETURN p_valida

END FUNCTION