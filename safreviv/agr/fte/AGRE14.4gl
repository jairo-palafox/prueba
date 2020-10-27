--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRE14                                        #
#Objetivo          =>Programa que permite integrar                 #
#                    del archivo de registros de reenvío del WS    #
#                    Anualidades Garantizadas                      #
#Autor             =>Esteban Sánchez, EFP                          #
#Fecha inicio      =>25 Septiembre 2013                            #
####################################################################
DATABASE safre_viv
GLOBALS "AGRG01.4gl"
DEFINE g_v_usuario          LIKE seg_usuario.usuario, -- nombre del usuario
       g_d_pid              LIKE bat_ctr_proceso.pid, -- pid
       g_i_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_i_opera_cod        LIKE cat_operacion.opera_cod, -- codigo de la operacion
       g_d_folio            LIKE glo_ctr_archivo.folio, -- numero de folio
       g_v_arch_proceso     VARCHAR(100), -- nombre del archivo a integrar
       g_id_cre_ctr_archivo LIKE cre_acreditado.id_cre_ctr_archivo, -- id del archivo
       g_c_ruta_bin         LIKE seg_modulo.ruta_bin, -- ruta del bin del módulo
       g_c_ruta_listados    LIKE seg_modulo.ruta_listados, -- ruta de listados del módulo}
       g_d_total_registros      DECIMAL(11,0),--Almacena el número de registros total recuperados del archivo
       g_d_total_aceptados      DECIMAL(9,0), --Almacena el número de registros aceptados 
       g_d_total_rechazados     DECIMAL(9,0)  --Almacena el número total de registros rechazados
          
MAIN
  DEFINE v_s_sql     STRING,   --String para las instrucciones sql
          r_regs_marcaje      RECORD   --Arreglo de resultado de las tablas temporales
          	nss                CHAR(11),
          	num_credito        DECIMAL(10,0),
          	tipo_credito       CHAR(3),
          	accion             CHAR(1),--marcar,desmarcar
          	fecha_solicitud  	 VARCHAR(10)
           END RECORD,
          arr_rechazados DYNAMIC ARRAY OF RECORD
           	nss                CHAR(11),
          	num_credito        DECIMAL(10,0),
          	tipo_credito       CHAR(3),
          	accion             VARCHAR(10),
          	fecha_solicitud  	 VARCHAR(10)
           END RECORD
           
   
   DEFINE v_ax_error               SMALLINT, 
          v_isam_err               INTEGER, 
          v_c_msj                  VARCHAR(250), 
          v_c_bandera              SMALLINT,
          v_c_ban_aceptado         SMALLINT,
          v_fecha                  DATE,
          v_i_indice               INTEGER,
          r_bnd_edo_act_archivo    SMALLINT,
          r_b_valida               SMALLINT,
          r_bnd_oera_error         SMALLINT
          
         
   -- se recuperan los parametros que envia el programa lanzador
   LET g_v_usuario          = ARG_VAL(1)
   LET g_d_pid              = ARG_VAL(2)
   LET g_i_proceso_cod      = ARG_VAL(3)
   LET g_i_opera_cod        = ARG_VAL(4)
   LET g_d_folio            = ARG_VAL(5)
   LET g_v_arch_proceso     = ARG_VAL(6)
   
   -- se crea el archivo log
   CALL STARTLOG(g_v_usuario CLIPPED|| ".AGRE14.log")
   
   --Inicializa variables
   LET g_d_total_registros = 0 
   LET g_d_total_aceptados = 0 
   LET g_d_total_rechazados= 0 
   LET v_i_indice = 1
   
   -- se genera el folio
  -- LET g_d_folio = fn_genera_folio(g_i_proceso_cod, g_i_opera_cod, g_v_usuario)
   DISPLAY " FOLIO         : ",g_d_folio USING "#########&"
   DISPLAY "##########COMIENZA PROCESO DE INTEGRACIÓN PARA REENVÍO##########"

   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("agr") RETURNING g_c_ruta_bin, g_c_ruta_listados

   -- se construye el query para obtener los datos cargados
   LET v_s_sql="SELECT nss,\n         ",
                "      num_credito, \n",
                "      tipo_credito,\n",
                "      accion,      \n",
                "      fecha_solicitud \n",
                "FROM safre_tmp:tmp_agr_marcaws_e2"
   
   --Preparación del statement
   PREPARE stm_selec_registros FROM v_s_sql
   DECLARE cur_selec_registros CURSOR FOR stm_selec_registros
   	
  --Iteración de los registros
  FOREACH cur_selec_registros INTO r_regs_marcaje.*
  	
  	  --Formatea fecha
  	  LET r_regs_marcaje.fecha_solicitud = r_regs_marcaje.fecha_solicitud[1,2],"-",
  	                                       r_regs_marcaje.fecha_solicitud[3,4],"-",
  	                                       r_regs_marcaje.fecha_solicitud[5,8]
  	 
 	 LET v_fecha=r_regs_marcaje.fecha_solicitud
  	   	
  	  --Creación del statement de ejecución del store	
  	  LET v_s_sql="EXECUTE FUNCTION fn_agr_integra_reenvio_marcaje(?,?,?,?,?,?,?)"
  	  PREPARE stm_integra_reenvio FROM v_s_sql
  	  EXECUTE stm_integra_reenvio INTO  v_ax_error ,
  	                                    v_isam_err ,
  	                                    v_c_msj    ,
  	                                    v_c_bandera,
  	                                    v_c_ban_aceptado
  	                              USING g_v_usuario,
  	                                    g_d_folio,
  	                                    r_regs_marcaje.nss            ,
  	                                    r_regs_marcaje.num_credito    ,
  	                                    r_regs_marcaje.tipo_credito   ,
  	                                    r_regs_marcaje.accion         ,
  	                                    v_fecha
  	  
  	  --Verifica que no haya sucedido error                                  
  	  IF  v_c_bandera THEN
  	  	
  	  	  	  	--Valida si el registro fué aceptado o rechazado para realizar el conteo  	  	
  	  	      IF v_c_ban_aceptado THEN
  	  	      	
  	  	         --Incrementa la suma de aceptados
  	  	         LET g_d_total_aceptados = g_d_total_aceptados + 1 
  	  	      
  	  	      ELSE
  	  	      	--Asigna valores a los rechazados
  	  	      	
  	  	      	LET arr_rechazados[v_i_indice].nss             = r_regs_marcaje.nss             
  	  	      	LET arr_rechazados[v_i_indice].num_credito     = r_regs_marcaje.num_credito           
  	  	      	LET arr_rechazados[v_i_indice].tipo_credito    = r_regs_marcaje.tipo_credito   
  	  	      	LET arr_rechazados[v_i_indice].fecha_solicitud = r_regs_marcaje.fecha_solicitud 
  	  	      	
  	  	      	IF  	r_regs_marcaje.accion ='1' THEN
  	  	      		
  	  	      		 	LET arr_rechazados[v_i_indice].accion          = "MARCA"
  	  	      	ELSE
  	  	      		 	LET arr_rechazados[v_i_indice].accion          = "DESMARCA"	 	
  	  	      	END IF
  	  	      		
  	  	      	
  	  	      	      
  	  	      	
  	  	         --Incrementa la suma de rechazados
  	  	         LET g_d_total_rechazados = g_d_total_rechazados + 1
  	  	         
  	  	         --Incrementa el índice
  	  	         LET v_i_indice = v_i_indice + 1
  	  	       
  	  	      END IF
  	  	
  	  ELSE
  	  	   --Despliega mensajes de error
  	  	   DISPLAY "*OCURRIÓ UN ERROR AL CARGAR EL REGISTRO CON NSS: ",r_regs_marcaje.nss 
  	  	   DISPLAY "*CLAVE DE ERROR: ",v_ax_error ,"-", v_isam_err
  	  	   DISPLAY "*DESCRIPCIÓN: ",v_c_msj
  	  	   
  	  END IF
  	        
  	  --Incrementa la suma total
  	  LET g_d_total_registros = g_d_total_registros + 1
  	  
  	           
  END FOREACH
  
  --Valida la correcta conclusión del proceso
  IF v_c_bandera THEN  	 
  	 --Finalización exitosa
  	 DISPLAY "##########CONCLUYÓ SATISFACTORIAMENTE LA INTEGRACIÓN PARA EL REENVÍO##########"
  	
  ELSE   	
  	--Existió un error
  	DISPLAY "*******OCURRIÓ UN ERROR AL REENVIAR ALGUNO DE LOS REGISTROS********"
  	
  END IF
  
     --Si la operacin no se finaliza, envia mensaje de error
     IF NOT v_c_bandera THEN
             CALL fn_error_opera(g_d_pid,g_i_proceso_cod,g_i_opera_cod)
             RETURNING r_bnd_oera_error
     ELSE
      	     -- Actualiza el estado del archivo procesado
             CALL fn_act_edo_archivo(g_v_arch_proceso,g_d_folio,2,g_v_usuario)
             RETURNING r_bnd_edo_act_archivo
              
             -- Función para finalizar la operacion
             CALL fn_actualiza_opera_fin(g_d_pid,g_i_proceso_cod,g_i_opera_cod)
             RETURNING r_b_valida
             
             --Envía generación de reporte
             CALL fn_genera_reporte_reenvio_ws(arr_rechazados)
     
     END IF

END MAIN
{
======================================================================
Clave: 
Nombre: fn_genera_reporte_reenvio_ws
Fecha creacion: Octubre 1, 2013
Autor: Esteban Sánchez Zepeda, EFP
Narrativa del proceso que realiza:
Genera el reporte producto del proceso de integración de registros a reenviar


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_genera_reporte_reenvio_ws(arr_reporte)
DEFINE   arr_reporte DYNAMIC ARRAY OF RECORD
           	nss                CHAR(11),
          	num_credito        DECIMAL(10,0),
          	tipo_credito       CHAR(3),
          	accion             VARCHAR(10),
          	fecha_solicitud  	 VARCHAR(10)
           END RECORD,
           v_i_indice          INTEGER,
           v_s_ban_existe      SMALLINT,
           v_c_fec_hoy         DATE,
           v_origen_datos      STRING,
           v_ruta_reporte      STRING, -- ruta del archivo del reporte
           v_ruta_listados     STRING, -- ruta de los listados 
           v_ruta_ejecutable   STRING, -- ruta del ejecutable
           manejador_rpt       om.SaxDocumentHandler --Contenedor documentos para reporte 

   -- se inicializan variables
   LET v_c_fec_hoy = TODAY USING "DD/MM/YYYY"   
   LET v_origen_datos = g_v_usuario
   LET v_s_ban_existe = FALSE

  -- se construye la ruta del archivo
  CALL fn_rutas("agr") RETURNING v_ruta_ejecutable, v_ruta_listados
  
  LET v_ruta_reporte = v_ruta_listados.trim(),
                       "/",
                       v_origen_datos.trim(),"-",
                       "AGRL49","-",
                       g_d_pid USING "&&&&&","-",
                       g_i_proceso_cod USING "&&&&&","-",
                       g_i_opera_cod USING "&&&&&",".pdf"     
                       
  DISPLAY "Ruta del reporte: ",v_ruta_reporte
  --Se asigna la plantilla para generar el reporte
  IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/AGRE141.4rp") THEN

     CALL fgl_report_selectDevice ("PDF")        
     CALL fgl_report_selectPreview(0)
     CALL fgl_report_setOutputFileName(v_ruta_reporte)

     LET manejador_rpt = fgl_report_commitCurrentSettings()
     
      --Inicia el reporte
     START REPORT rp_reenvio_registros_ws TO XML HANDLER manejador_rpt
     
     FOR v_i_indice=1 TO arr_reporte.getLength()
     	
     	   OUTPUT TO REPORT rp_reenvio_registros_ws(v_c_fec_hoy,
                                                  arr_reporte[v_i_indice].*)
         --Actualiza valor de la bandera de existencia
         LET v_s_ban_existe = TRUE
     	
     END FOR
     
     --Valida que haya entrado al menos una vez al reporte 
     IF NOT v_s_ban_existe THEN
     	 --Se envía arreglo vacío
     	 LET arr_reporte[v_i_indice].nss             =" " 
     	 LET arr_reporte[v_i_indice].num_credito     =" " 
     	 LET arr_reporte[v_i_indice].accion          =" " 
     	 LET arr_reporte[v_i_indice].fecha_solicitud =" "
       OUTPUT TO REPORT rp_reenvio_registros_ws(v_c_fec_hoy,
                                                    arr_reporte[v_i_indice].*)
                                                    
     END IF
     
     
     FINISH REPORT rp_reenvio_registros_ws

  ELSE
  	 --Ocurrió un error al generar el reporte
    DISPLAY "Ocurrió un error al generar el reporte"
    EXIT PROGRAM
    
  END IF


END FUNCTION
{
======================================================================
Clave: 
Nombre: rp_reenvio_registros_ws
Fecha creacion: Octubre 1, 2013
Autor: Esteban Sánchez Zepeda, EFP
Narrativa del proceso que realiza:
Emite el reporte de registros reenviados por el ws a PROCESAR

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}

REPORT rp_reenvio_registros_ws(v_fecha,r_reporte)

DEFINE    v_fecha    DATE,
          r_reporte  RECORD
           	nss                CHAR(11),
          	num_credito        DECIMAL(10,0),
          	tipo_credito       CHAR(3),
          	accion             VARCHAR(10),
          	fecha_solicitud  	 VARCHAR(10)
           END RECORD

FORMAT
   FIRST PAGE HEADER
              
      PRINTX g_v_usuario 
      PRINTX g_d_folio
      PRINTX v_fecha                                     
      PRINTX g_v_arch_proceso
      PRINTX g_d_total_registros
      PRINTX g_d_total_aceptados
      PRINTX g_d_total_rechazados


  ON EVERY ROW     
  
       PRINTX r_reporte.nss            
       PRINTX r_reporte.num_credito    
       PRINTX r_reporte.tipo_credito   
       PRINTX r_reporte.accion         
       PRINTX r_reporte.fecha_solicitud
            
END REPORT