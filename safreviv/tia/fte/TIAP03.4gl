--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => TIA                                                                    #
#Programa     => TIAC05                                                                 #
#Objetivo     => Lanzado de consulta de excepcion de traspaso de I-A                    #
#Autor        => Rubén Haro Castro                                                      #
#Fecha inicio => Mayo 21, 2012                                                          #
#########################################################################################
DATABASE safre_viv
GLOBALS "TIAG01.4gl"
MAIN 
DEFINE  p_folio                LIKE  tia_det_traspaso.folio,  --folio a desplegar
        v_s_SqlQry             STRING,                        --variabloe que almacena la consulta
        l_r_excepcion     DYNAMIC ARRAY OF RECORD             --record de registros 
        	 chkb1                SMALLINT  ,   
           folio                LIKE  tia_det_traspaso.folio              ,
           id_referencia        LIKE  tia_det_traspaso.id_referencia      ,
           tpo_ent_receptora    LIKE  tia_det_traspaso.tpo_ent_receptora  ,
           cve_ent_receptora    LIKE  tia_det_traspaso.cve_ent_receptora  ,
           tpo_ent_cedente      LIKE  tia_det_traspaso.tpo_ent_cedente    ,
           cve_ent_cedente      LIKE  tia_det_traspaso.cve_ent_cedente    ,
           origen_traspaso      LIKE  tia_det_traspaso.origen_traspaso    ,
           f_presentacion       LIKE  tia_det_traspaso.f_presentacion     ,
           f_movimiento         LIKE  tia_det_traspaso.f_movimiento       ,
           id_decreto           LIKE  tia_det_traspaso.id_decreto         ,
           curp                 LIKE  tia_det_traspaso.curp               ,
           nss_afo_recep        LIKE  tia_det_traspaso.nss_afo_recep      ,
           rfc_afo_recep        LIKE  tia_det_traspaso.rfc_afo_recep      ,
           paterno_afo_recep    LIKE  tia_det_traspaso.paterno_afo_recep  ,
           materno_afo_recep    LIKE  tia_det_traspaso.materno_afo_recep  ,
           nombres_afo_recep    LIKE  tia_det_traspaso.nombres_afo_recep  ,
           cve_sector           LIKE  tia_det_traspaso.cve_sector         ,
           f_recep_solicitud    LIKE  tia_det_traspaso.f_recep_solicitud  ,
           id_lote_solicitud    LIKE  tia_det_traspaso.id_lote_solicitud  ,
           nss_icefa            LIKE  tia_det_traspaso.nss_icefa          ,
           rfc_icefa            LIKE  tia_det_traspaso.rfc_icefa          ,
           nci_icefa            LIKE  tia_det_traspaso.nci_icefa          ,
           paterno_icefa        LIKE  tia_det_traspaso.paterno_icefa      ,
           materno_icefa        LIKE  tia_det_traspaso.materno_icefa      ,
           nombres_icefa        LIKE  tia_det_traspaso.nombres_icefa      ,
           sdo_viv92            LIKE  tia_det_traspaso.sdo_viv92          ,
           int_viv92            LIKE  tia_det_traspaso.int_viv92          ,
           result_operacion     LIKE  tia_det_traspaso.result_operacion   
        END RECORD,
       v_i_contador_registros          INTEGER,  ---contador de registros del array 
       p_opera_cod_carga               LIKE cat_operacion.opera_cod, # Código de operacion
       p_usuario_cod                   LIKE seg_usuario.usuario_cod, # Clave de usuario
       p_nom_archivo                   STRING, 
       P_pid                           LIKE bat_ctr_proceso.pid,     # ID del proceso
       P_proceso_cod                   LIKE cat_proceso.proceso_cod, # Código del proceso
       P_opera_cod_integracion         LIKE cat_operacion.opera_cod, # Código de operación
       P_opera_cod_preliquidacion      LIKE cat_operacion.opera_cod, # Código de operación
       v_i_inicio                      INTEGER,
       v_contador_chkb1                INTEGER,
       v_r_cza_traspaso                RECORD LIKE tia_cza_traspaso.*,
       v_r_sum_traspaso                RECORD LIKE tia_sum_traspaso.*,
       l_s_cadena_detalle              STRING,
       l_s_cadena_cza_traspaso         STRING,
       l_s_cadena_sum_traspaso         STRING,
       v_c_filler1                     CHAR(14),
       v_c_filler2                     CHAR(3),
       v_c_filler3                     CHAR(9),
       v_c_filler4                     CHAR(14),
       v_c_filler5                     CHAR(119),
       v_c_filler6                     CHAR(94),
       v_c_filler7                     CHAR(40),
       v_c_filler8                     CHAR(697),--se asigna según el layout de encabezado de tia 
       v_c_filler9                     CHAR(553),--se asigna según el layout de suamario de tia 
       v_d_sdo_viv92                   LIKE  tia_det_traspaso.sdo_viv92,
       v_d_int_viv92                   LIKE  tia_det_traspaso.int_viv92,
       v_v_nom_archi                   VARCHAR(50),--STRING, -- nombre del archivo de salida
       v_v_ruta_nomarch                STRING, -- ruta y nombre del archivo de salida
       v_c_ruta_env_acr                LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_solTransf             BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_s_registro                    STRING -- registro a insertar,


   WHENEVER ERROR CONTINUE
   
   #Si se ha recibido parámetros se continua    
   #Primer parámetro
   LET p_usuario_cod = ARG_VAL(1)
   #Segundo parámetro
   LET p_folio       = ARG_VAL(2)
   
    LET p_usuario_cod = ARG_VAL(1)
   #Segundo parámetro
   LET g_pid         = ARG_VAL(2)
   #Tercer parámetro
   LET g_proceso_cod = ARG_VAL(3)
   #Cuarto parámetro
   LET g_opera_cod_integracion = ARG_VAL(4) # Paso de información a las tablas históricas
   #Quinto parámetro
   LET r_folio       = ARG_VAL(5)
   #Segundo parámetro
   LET p_nom_archivo = ARG_VAL(6)
   # Indicamos operacion de carga
   
   
   
   
   
   
   LET INT_FLAG = 0
   --se inicializa los contador de registros 
   LET v_i_inicio = 1    
   LET v_i_contador_registros = 1
   LET v_d_sdo_viv92 = 0
   LET v_d_int_viv92 = 0
   LET v_contador_chkb1 = 0 

   LET v_v_nom_archi = TIME
   LET v_v_nom_archi = "Excepciones", v_v_nom_archi[4,5], v_v_nom_archi[7,8],'.TIA'

   CALL STARTLOG(p_usuario_cod CLIPPED||".TIAP03.log")
   
   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
     INTO v_c_ruta_env_acr
     FROM seg_modulo
    WHERE modulo_cod = 'tia'

   -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   --LET v_v_nom_archi = "/",YEAR(TODAY) USING "&&&&",MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&",".TIA"

   LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED || v_v_nom_archi
   --DISPLAY "  v_v_ruta_nomarch = ",v_v_ruta_nomarch
   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()
   
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")

   LET v_s_SqlQry = 
   "SELECT 0 ,*   \n
      FROM tia_det_traspaso   \n
     WHERE folio = ?   \n
       AND result_operacion = '02'"
   --DISPLAY " v_s_SqlQry  = " ,v_s_SqlQry
   PREPARE con_excepcion FROM v_s_SqlQry
   DECLARE c_excepcion  CURSOR FOR con_excepcion
   FOREACH c_excepcion USING p_folio INTO l_r_excepcion[v_i_contador_registros].*
   		 
      DISPLAY "   folio",       l_r_excepcion[v_i_contador_registros].folio
           
      DISPLAY "   id_decreto",     l_r_excepcion[v_i_contador_registros].id_decreto  
           
      DISPLAY "   nss_afo_recep",     l_r_excepcion[v_i_contador_registros].nss_afo_recep  
      DISPLAY "   rfc_afo_recep",     l_r_excepcion[v_i_contador_registros].rfc_afo_recep  
           
           
           
           
           
           
      DISPLAY "   nss_icefa",     l_r_excepcion[v_i_contador_registros].nss_icefa      
      DISPLAY "   rfc_icefa",     l_r_excepcion[v_i_contador_registros].rfc_icefa      
      DISPLAY "   nci_icefa",     l_r_excepcion[v_i_contador_registros].nci_icefa      
           
           
           
      DISPLAY "   sdo_viv92",     l_r_excepcion[v_i_contador_registros].sdo_viv92      
      DISPLAY "   int_viv92",     l_r_excepcion[v_i_contador_registros].int_viv92      
           
   	 
   	 
   	
      LET v_i_contador_registros =  v_i_contador_registros + 1
   END FOREACH

  LET v_i_contador_registros = v_i_contador_registros - 1
  --se elimina el registro que esta de mas 
  CALL l_r_excepcion.deleteElement([l_r_excepcion.getLength()])
   
   --se abre vetana para la seleccion de excepciones 
   OPEN WINDOW w_excepcion  WITH FORM "TIAC06"

   INPUT ARRAY l_r_excepcion FROM tbl_excepcion.*
    	
     	ATTRIBUTES(WITHOUT DEFAULTS, UNBUFFERED,
                 INSERT ROW = FALSE,
                 DELETE ROW = FALSE,
                 AUTO APPEND= FALSE,
                 APPEND ROW = FALSE)
                 
      --en el botón aceptar 
     	ON ACTION ACCEPT
     	
     	  --se cuentan todos los registros seleccionados
     	  FOR v_i_inicio = 1 TO  v_i_contador_registros 
     	  	--si se seleciona alguno se contabiliza    
     	  	IF(l_r_excepcion[v_i_inicio].chkb1  = 1) THEN
     	  		--se suma el conatdor de registros 
     	  		LET v_contador_chkb1  = v_contador_chkb1 + 1 
     	    END IF
     	  END FOR

     	  --DISPLAY " v_contador_chkb1 ",v_contador_chkb1
     	  --si el contador de selecion es = 0 se envia mensaje 
     	  IF v_contador_chkb1 = 0 THEN      	    
     	    CALL fn_mensaje("Confirma Excepción","Es necesario seleccionar un registro  ","about")
     	    CONTINUE INPUT
     	  --en el caso contrario se seleciono un aexcepción por lo que se crea el archivo de excepción   	
     	  ELSE   
     	  	
     	  	--se consulta el encabezado 
     	  	LET v_s_SqlQry =  
     	  	" SELECT * \n", 
     	  	"   FROM tia_cza_traspaso \n", 
     	  	"  WHERE folio =", p_folio
     	  	--DISPLAY " v_s_SqlQry ",v_s_SqlQry
     	  	PREPARE prp_cza_traspaso FROM v_s_SqlQry
     	  	EXECUTE prp_cza_traspaso INTO v_r_cza_traspaso.*   
     	  	
     	  	LET l_s_cadena_cza_traspaso  = v_r_cza_traspaso.tpo_registro      ,
                                         v_r_cza_traspaso.id_servicio       ,
                                         v_r_cza_traspaso.id_operacion      ,
                                         v_r_cza_traspaso.tpo_ent_origen    ,
                                         v_r_cza_traspaso.cve_ent_origen    ,
                                         v_r_cza_traspaso.tpo_ent_destino   ,
                                         v_r_cza_traspaso.cve_ent_destino   ,
                                         v_c_filler2                        ,--se asigna por el tamaño de variable
                                         v_r_cza_traspaso.f_presentacion    ,
                                         v_r_cza_traspaso.consecutivo_dia   ,
                                         v_r_cza_traspaso.cve_mod_presenta  , 
                                         v_c_filler8

          --se escribe el encabezado en el archivo
          CALL v_ch_arch_solTransf.writeLine([l_s_cadena_cza_traspaso])
     	  	--se cicla el array para cada regisro selecionado chkb1  = 1
     	  	FOR v_i_inicio = 1 TO  v_i_contador_registros    
     	  		 --DISPLAY " l_r_excepcion[v_i_inicio].chkb1",l_r_excepcion[v_i_inicio].chkb1
     	  	  IF(l_r_excepcion[v_i_inicio].chkb1  = 1) THEN
              LET  l_s_cadena_detalle  =  "02",l_r_excepcion[v_i_inicio].id_referencia
                                             ,l_r_excepcion[v_i_inicio].tpo_ent_receptora   
                                             ,l_r_excepcion[v_i_inicio].cve_ent_receptora   
                                             ,l_r_excepcion[v_i_inicio].tpo_ent_cedente     
                                             ,l_r_excepcion[v_i_inicio].cve_ent_cedente     
                                             ,l_r_excepcion[v_i_inicio].origen_traspaso     
                                             ,l_r_excepcion[v_i_inicio].f_presentacion      
                                             ,l_r_excepcion[v_i_inicio].f_movimiento
                                             ,l_r_excepcion[v_i_inicio].curp                
                                             ,l_r_excepcion[v_i_inicio].nss_afo_recep
                                             ,v_c_filler1
                                             ,l_r_excepcion[v_i_inicio].rfc_afo_recep       
                                             ,l_r_excepcion[v_i_inicio].paterno_afo_recep   
                                             ,l_r_excepcion[v_i_inicio].materno_afo_recep   
                                             ,l_r_excepcion[v_i_inicio].nombres_afo_recep
                                             ,v_c_filler2   
                                             ,l_r_excepcion[v_i_inicio].cve_sector     
                                             ,v_c_filler3    
                                             ,l_r_excepcion[v_i_inicio].f_recep_solicitud   
                                             ,l_r_excepcion[v_i_inicio].id_lote_solicitud   
                                             ,v_c_filler4 
                                             ,l_r_excepcion[v_i_inicio].nss_icefa           
                                             ,l_r_excepcion[v_i_inicio].rfc_icefa           
                                             ,l_r_excepcion[v_i_inicio].nci_icefa           
                                             ,l_r_excepcion[v_i_inicio].paterno_icefa       
                                             ,l_r_excepcion[v_i_inicio].materno_icefa       
                                             ,l_r_excepcion[v_i_inicio].nombres_icefa       
                                             ,v_c_filler5
                                             ,l_r_excepcion[v_i_inicio].sdo_viv92      
                                             ,v_c_filler6     
                                             ,l_r_excepcion[v_i_inicio].int_viv92       
                                             ,v_c_filler7    

                LET v_d_sdo_viv92 = v_d_sdo_viv92 + l_r_excepcion[v_i_inicio].sdo_viv92 
                LET v_d_int_viv92 = v_d_int_viv92 + l_r_excepcion[v_i_inicio].int_viv92
              --se escribe el deatalle en el archivo
               CALL v_ch_arch_solTransf.writeLine([l_s_cadena_detalle])

     	      END IF
     	    END FOR

         
               	    
     	  	--se consulta el sumario 
     	  	LET v_s_SqlQry =  
     	  	" SELECT * \n", 
     	  	"   FROM tia_sum_traspaso \n", 
     	  	"  WHERE folio =", p_folio
     	  	PREPARE prp_sum_traspaso FROM v_s_SqlQry
     	  	EXECUTE prp_sum_traspaso INTO v_r_sum_traspaso.*

          LET  l_s_cadena_sum_traspaso  = v_r_sum_traspaso.tpo_registro       ,
                                          v_r_sum_traspaso.registros_detalle  ,
                                          v_c_filler5                         ,
                                          v_d_sdo_viv92                       ,
                                          v_c_filler1                         ,
                                          v_d_int_viv92                       ,
                                          v_c_filler9

          --se escribe el sumario en el archivo
          CALL v_ch_arch_solTransf.writeLine([l_s_cadena_sum_traspaso])
         LET INT_FLAG = 1
     	  END IF
     	  EXIT INPUT

     	ON ACTION CANCEL
     	  LET INT_FLAG = 0
     	  EXIT INPUT

     END INPUT 

  CLOSE WINDOW w_excepcion
END MAIN 
