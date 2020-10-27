-----------------------------------------------------------------------------------------
-- Modulo       => TIA                                                                    
-- Programa     => TIAP04                                                                 
-- Objetivo     => Programa que realiza la generación del archivo de Excepciones de TIA
-- Autor        => GERARDO ALFONSO VEGA PAREDES                                           
-- Fecha inicio => 9 de Marzo de 2015.
-----------------------------------------------------------------------------------------
-- Actualizado  => Gerardo Alfonso Vega Paredes.                             
-- Fec Mod.     => 31 de Agosto de 2017.                                     
-- Modificación => Agregar consulta de detalle de AGR                        
-- Clave cambio => proinfxvii-81
-- Descripción  => Emitir solo rechazos por result_operacion="04" nss no encontrado
-----------------------------------------------------------------------------------------
-- Actualizado  => Gerardo Alfonso Vega Paredes.                             
-- Fec Mod.     => 29 de Septiembre de 2017.                                     
-- Modificación => Cambiar nombre de archivo ejemplo agregando letra "A" ExcepcionesA000060379.tia
--              => y gnerar una copia del mismo archivo sin registros duplicados.
--              => Archivo sin duplicados funciones f_excepcion() y envio a TRM
--              => Archivo con duplicados funciones f_excepcion2() sin envio a TRM
-- Clave cambio => xvii-81-02
-----------------------------------------------------------------------------------------
-- Actualizado  => Gerardo Alfonso Vega Paredes.                             
-- Fec Mod.     => 12 de Diciembre de 2017.                                     
-- Modificación => Adaptar importes de sumario a layout antigüo "Propuesga nuevo layout TIA V3,0.xlsx"
--              => Porque al cargar archivo de excepciones están corridas las posiciones de los importes
-- Clave cambio => xvii-81-03
-----------------------------------------------------------------------------------------
-- Actualizado  => Gerardo Alfonso Vega Paredes.                             
-- Fec Mod.     => 26 de Fevrero de 2018.                                     
-- Modificación => Aducuación para que imprima el último nss de la función f_exepcion()
-- Clave cambio => xvii-81-04
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS "TIAG01.4gl" --archivo de variables globales proceso_cod

GLOBALS
   DEFINE g_pid         LIKE bat_ctr_proceso.pid,     --  ID del proceso
          g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
          g_opera_cod   LIKE cat_operacion.opera_cod  -- codigo de operacion

   DEFINE g_reg_modulo RECORD
      ruta_exp      CHAR(40),
      ruta_rescate  CHAR(40),
      ruta_listados CHAR(40)
   END RECORD
   
   DEFINE seg_modulo_bat RECORD
      ruta_listados CHAR(40)
   END RECORD

END GLOBALS

MAIN
   DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          p_s_titulo    STRING,                       -- titulo de la ventana
          v_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
          v_opera_cod   LIKE cat_operacion.opera_cod, -- codigo de operacion
          v_ruta_vacia  STRING,
          p_folio       LIKE glo_folio.folio    --SE CREO VARIABLE 

   DEFINE v_c_ruta_bin_acr  LIKE seg_modulo.ruta_bin, -- ruta del bin de acr
          v_c_ruta_list_bat LIKE seg_modulo.ruta_listados -- ruta listados de bat

   DEFINE v_folio_tia_det_traspaso  LIKE tia_det_traspaso.folio,  --folio 
          v_i_contador_tia_det_tras INTEGER
       
   INITIALIZE v_folio_tia_det_traspaso TO NULL 

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod = ARG_VAL(1)
   LET p_folio       = ARG_VAL(2)
   LET p_s_titulo    = ARG_VAL(3)
   
   -- se asigna proceso y operacion
   LET v_proceso_cod = g_proceso_cod_tia            -- traspasos  I-A 
   LET v_opera_cod   = g_opera_cod_tia_liquidacion  -- archivo de salida para tesoreria

   --se inicializa el contador de regisdtros de detallle en cero 
   LET v_i_contador_tia_det_tras = 0

   --llamad a funciones generales que regresan las rutas para generación de archvivos
   CALL fn_rutas("tia") RETURNING v_c_ruta_bin_acr, v_ruta_vacia
   CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_c_ruta_list_bat

       --se contabilizan los registros a seleccionar para saber si existen o no 
   SELECT COUNT (DISTINCT folio)
   INTO   v_i_contador_tia_det_tras 
   FROM   tia_det_traspaso
   WHERE  result_operacion  = "04"                    --proinfxvii-81
--   WHERE  result_operacion NOT IN ("01","99","10")  --proinfxvii-81
   AND    folio = p_folio
      
   --si existen registros en 02
   IF (v_i_contador_tia_det_tras > 0) THEN 
       CALL f_excepcion(p_folio)
       CALL f_excepcion2(p_folio)
     ELSE	 
        --CALL fn_mensaje("Atención","No existen registros a ser confirmados","Info") 
        DISPLAY "No se encontraron folio a liquidar, por lo tanto no se genero el archivo	" 
   END IF 	  

END MAIN

FUNCTION f_excepcion(p_folio)
   
   DEFINE p_folio    LIKE  tia_det_traspaso.folio, --folio a desplegar
          v_s_SqlQry STRING                        --variabloe que almacena la consulta
          
   DEFINE l_r_excepcion DYNAMIC ARRAY OF RECORD             --record de registros 
      chkb              SMALLINT  ,   
      folio             LIKE tia_det_traspaso.folio              ,
      id_referencia     LIKE tia_det_traspaso.id_referencia      ,
      tpo_ent_receptora LIKE tia_det_traspaso.tpo_ent_receptora  ,
      cve_ent_receptora LIKE tia_det_traspaso.cve_ent_receptora  ,
      tpo_ent_cedente   LIKE tia_det_traspaso.tpo_ent_cedente    ,
      cve_ent_cedente   LIKE tia_det_traspaso.cve_ent_cedente    ,
      origen_traspaso   LIKE tia_det_traspaso.origen_traspaso    ,
      f_presentacion    LIKE tia_det_traspaso.f_presentacion     ,
      f_movimiento      LIKE tia_det_traspaso.f_movimiento       ,
      id_decreto        LIKE tia_det_traspaso.id_decreto         ,
      curp              LIKE tia_det_traspaso.curp               ,
      nss_afo_recep     LIKE tia_det_traspaso.nss_afo_recep      ,
      rfc_afo_recep     LIKE tia_det_traspaso.rfc_afo_recep      ,
      paterno_afo_recep LIKE tia_det_traspaso.paterno_afo_recep  ,
      materno_afo_recep LIKE tia_det_traspaso.materno_afo_recep  ,
      nombres_afo_recep LIKE tia_det_traspaso.nombres_afo_recep  ,
      cve_sector        LIKE tia_det_traspaso.cve_sector         ,
      f_recep_solicitud LIKE tia_det_traspaso.f_recep_solicitud  ,
      id_lote_solicitud LIKE tia_det_traspaso.id_lote_solicitud  ,
      nss_icefa         LIKE tia_det_traspaso.nss_icefa          ,
      rfc_icefa         LIKE tia_det_traspaso.rfc_icefa          ,
      nci_icefa         LIKE tia_det_traspaso.nci_icefa          ,
      paterno_icefa     LIKE tia_det_traspaso.paterno_icefa      ,
      materno_icefa     LIKE tia_det_traspaso.materno_icefa      ,
      nombres_icefa     LIKE tia_det_traspaso.nombres_icefa      ,
      sdo_viv92         LIKE tia_det_traspaso.sdo_viv92          ,
      int_viv92         LIKE tia_det_traspaso.int_viv92          ,
      result_operacion  LIKE tia_det_traspaso.result_operacion    ,
      aivs_viv92        LIKE tia_det_traspaso.aivs_viv92
   END RECORD
        
   DEFINE v_i_contador_registros     INTEGER,  ---contador de registros del array 
          p_usuario_cod              LIKE seg_usuario.usuario_cod, # Clave de usuario
          v_i_inicio                 INTEGER,
          v_contador_chkb1           INTEGER,
          v_r_cza_traspaso           RECORD LIKE tia_cza_traspaso.*,
          v_r_sum_traspaso           RECORD LIKE tia_sum_traspaso.*,
          l_s_cadena_detalle         STRING,
          l_s_cadena_cza_traspaso    STRING,
          l_s_cadena_sum_traspaso    STRING,
          v_c_filler1                CHAR(15),
          v_c_filler2                CHAR(3),
          v_c_filler3                CHAR(10),
          v_c_filler4                CHAR(15),
          v_c_consecutivo_cuenta     CHAR(11),
          v_c_filler5                CHAR(109),
          v_c_filler6                CHAR(80),
          v_c_filler7                CHAR(41),
          v_c_filler8                CHAR(698),--se asigna según el layout de encabezado de tia 
          v_c_filler9                CHAR(554),--se asigna según el layout de suamario de tia 
          v_c_filler10               CHAR(120),--se asigna según el layout de suamario de tia        
          v_d_sdo_viv92              DECIMAL(22,2),--LIKE  tia_det_traspaso.sdo_viv92,
          v_d_aiv_viv92              DECIMAL(22,6),
          v_d_int_viv92              DECIMAL(22,2),--LIKE  tia_det_traspaso.int_viv92,
          v_v_nom_archi              VARCHAR(50),--STRING, -- nombre del archivo de salida
          v_v_ruta_nomarch           STRING, -- ruta y nombre del archivo de salida
          v_c_ruta_env_acr           LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
          v_c_ruta_res               LIKE seg_modulo.ruta_rescate,
          v_ch_arch_solTransf        BASE.CHANNEL, -- manejador de apuntador hacia archivo
          v_d_fecha_presentacion_cza VARCHAR (8,0),
          v_d_fecha_presentacion     VARCHAR (8,0),
          v_d_fecha_movimiento       VARCHAR (8,0),
          v_d_fecharecep_solicitud   VARCHAR (8,0),
          v_c_tipo_registro          VARCHAR (2),
          v_c_id_referencia          VARCHAR (10),
          v_c_num_control_interno    VARCHAR (30),
          v_c_monto_pesos            VARCHAR (15),       
          v_c_num_monto_interes      VARCHAR (14),
          v_c_num_sum_monto_sdo      VARCHAR (15),
--          v_c_num_sum_monto_aiv      VARCHAR (18),  --xvii-81-03
--          v_c_num_sum_monto_interes  VARCHAR (16),  --xvii-81-03
          v_c_num_sum_monto_aiv      VARCHAR (15),    --xvii-81-03
          v_c_num_sum_monto_interes  VARCHAR (15),    --xvii-81-03      
          v_respuesta_confirma       INTEGER,
          v_consec_cuenta            DECIMAL(11,0),
          v_nss_unico                CHAR(11)         --xvii-81-04
       
   DEFINE v_comando STRING 

   LET INT_FLAG = 0
   
   --se inicializa los contador de registros 
   LET v_i_inicio             = 1    
   LET v_i_contador_registros = 1
   LET v_d_sdo_viv92          = 0
   LET v_d_aiv_viv92          = 0
   LET v_d_int_viv92          = 0
   LET v_contador_chkb1       = 0    
   LET v_c_tipo_registro      = 02
   LET v_respuesta_confirma   = 0

   --LET v_v_nom_archi = "Excepciones", v_v_nom_archi[4,5], v_v_nom_archi[7,8],'.TIA'
   -- se le agrega el folio al nombre del archivo
   LET l_s_cadena_detalle = p_folio USING "&&&&&&&&&"
   LET v_v_nom_archi = "Excepciones", l_s_cadena_detalle,'.tia'

   CALL STARTLOG(p_usuario_cod CLIPPED||".TIAP04.log")
   
   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio,
          ruta_rescate 
   INTO   v_c_ruta_env_acr,
          v_c_ruta_res
   FROM   seg_modulo
   WHERE  modulo_cod = 'tia'

   LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED,"/"|| v_v_nom_archi

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()
   
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")

   LET v_s_SqlQry = " SELECT 0 ,* ",
                    " FROM tia_det_traspaso",
                    " WHERE folio = ?",
                    " AND result_operacion = '04' ",                      --proinfxvii-81 
--                    " AND result_operacion not in ('01', '99', '10') "  --proinfxvii-81
                    " ORDER BY nss_afo_recep "         --xvii-81-02

   --DISPLAY " v_s_SqlQry  = " ,v_s_SqlQry

   -- se prepara y ejecuta la consulta
   PREPARE con_excepcion FROM v_s_SqlQry
   DECLARE c_excepcion  CURSOR FOR con_excepcion

   -- se transfieren los datos
   FOREACH c_excepcion USING p_folio 
      INTO l_r_excepcion[v_i_contador_registros].*
      LET  l_r_excepcion [v_i_contador_registros].chkb = 1 
   	  LET v_i_contador_registros =  v_i_contador_registros + 1
   END FOREACH

   --LET v_i_contador_registros = v_i_contador_registros - 1
   
   --se elimina el registro que esta de mas 
   CALL l_r_excepcion.deleteElement([l_r_excepcion.getLength()])
   
   DISPLAY "@@ v_i_contador_registros: ", v_i_contador_registros
     	
   --se cuentan todos los registros seleccionados
   FOR v_i_inicio = 1 TO  l_r_excepcion.getLength()

      LET l_r_excepcion[v_i_inicio].chkb  = 1
   	  --si se seleciona alguno se contabiliza    
   	  IF(l_r_excepcion[v_i_inicio].chkb  = 1) THEN
   	     --se suma el conatdor de registros 
   	     LET v_contador_chkb1  = v_contador_chkb1 + 1 
      END IF
   END FOR

   DISPLAY "@@ v_contador_chkb1: ", v_contador_chkb1

   --se consulta el encabezado 
   LET v_s_SqlQry = " SELECT * ",
                    " FROM tia_cza_traspaso",
                    " WHERE folio = ", p_folio
                    
   --DISPLAY " v_s_SqlQry ",v_s_SqlQry
   PREPARE prp_cza_traspaso FROM v_s_SqlQry
   EXECUTE prp_cza_traspaso INTO v_r_cza_traspaso.*  
    
   LET v_d_fecha_presentacion_cza = YEAR(v_r_cza_traspaso.f_presentacion ) USING "&&&&",MONTH(v_r_cza_traspaso.f_presentacion ) USING "&&",DAY(v_r_cza_traspaso.f_presentacion ) USING "&&"
    
   LET l_s_cadena_cza_traspaso = v_r_cza_traspaso.tpo_registro      ,
                                 v_r_cza_traspaso.id_servicio       ,
                                 v_r_cza_traspaso.id_operacion      ,
                                 v_r_cza_traspaso.tpo_ent_origen    ,
                                 v_r_cza_traspaso.cve_ent_origen    ,
                                 v_r_cza_traspaso.tpo_ent_destino   ,
                                 v_r_cza_traspaso.cve_ent_destino   ,
                                 v_c_filler2                        ,--se asigna por el tamaño de variable
                                 v_d_fecha_presentacion_cza         ,--PARA QUE NO SE IMPRIMNA LAS DIAGONALES DEL FORMATO DE FECHA 
                                 v_r_cza_traspaso.consecutivo_dia   ,
                                 v_r_cza_traspaso.cve_mod_presenta  , 
                                 v_c_filler8
   
   --se escribe el encabezado en el archivo
   CALL v_ch_arch_solTransf.writeLine([l_s_cadena_cza_traspaso])
   
   LET v_i_contador_registros = 0   --xvii-81-02
   
   --se cicla el array para cada regisro selecionado chkb1 = 1
   FOR v_i_inicio = 1 TO l_r_excepcion.getLength()    

      LET v_consec_cuenta = 0
     
   	  --DISPLAY " l_r_excepcion[v_i_inicio].chkb1",l_r_excepcion[v_i_inicio].chkb1
      IF l_r_excepcion[v_i_inicio].chkb = 1 THEN

     	 IF l_r_excepcion[v_i_inicio+1].nss_afo_recep IS NULL THEN        --xvii-81-04
            LET v_nss_unico = "00000000000"                               --xvii-81-04
         ELSE                                                             --xvii-81-04
            LET v_nss_unico = l_r_excepcion[v_i_inicio+1].nss_afo_recep   --xvii-81-04
     	 END IF                                                           --xvii-81-04
      
--     	 IF l_r_excepcion[v_i_inicio].nss_afo_recep <> l_r_excepcion[v_i_inicio+1].nss_afo_recep THEN  --xvii-81-02
     	 IF l_r_excepcion[v_i_inicio].nss_afo_recep <> v_nss_unico THEN  --xvii-81-04

     	   	LET v_i_contador_registros = v_i_contador_registros + 1   --xvii-81-02

            LET v_d_fecha_presentacion   = YEAR(l_r_excepcion[v_i_inicio].f_presentacion) USING "&&&&",MONTH(l_r_excepcion[v_i_inicio].f_presentacion) USING "&&",DAY(l_r_excepcion[v_i_inicio].f_presentacion) USING "&&"
            LET v_d_fecha_movimiento     = YEAR(l_r_excepcion[v_i_inicio].f_movimiento) USING "&&&&",MONTH(l_r_excepcion[v_i_inicio].f_movimiento) USING "&&",DAY(l_r_excepcion[v_i_inicio].f_movimiento) USING "&&"
            LET v_d_fecharecep_solicitud = YEAR(l_r_excepcion[v_i_inicio].f_recep_solicitud) USING "&&&&",MONTH(l_r_excepcion[v_i_inicio].f_recep_solicitud) USING "&&",DAY(l_r_excepcion[v_i_inicio].f_recep_solicitud) USING "&&"
            
            LET v_c_id_referencia  = l_r_excepcion[v_i_inicio].id_referencia USING "&&&&&&&&&&"
            LET v_c_num_control_interno = l_r_excepcion[v_i_inicio].nci_icefa   USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"        
            
            --se  acumulan los importes
            LET v_d_sdo_viv92 = v_d_sdo_viv92 + l_r_excepcion[v_i_inicio].sdo_viv92 
            LET v_d_aiv_viv92 = v_d_aiv_viv92 + l_r_excepcion[v_i_inicio].aivs_viv92
            LET v_d_int_viv92 = v_d_int_viv92 + l_r_excepcion[v_i_inicio].int_viv92 
            
            
            LET l_r_excepcion[v_i_inicio].sdo_viv92  = l_r_excepcion[v_i_inicio].sdo_viv92 * 100     
            LET l_r_excepcion[v_i_inicio].int_viv92  = l_r_excepcion[v_i_inicio].int_viv92 * 100  
            
            LET v_c_monto_pesos         =  l_r_excepcion[v_i_inicio].sdo_viv92 USING "&&&&&&&&&&&&&&&"
            LET v_c_num_monto_interes   =  l_r_excepcion[v_i_inicio].int_viv92 USING "&&&&&&&&&&&&&&&"
            
            -- se obtiene el consecutivo de la cuenta
            SELECT consec_cuenta
            INTO   v_consec_cuenta
            FROM   afi_decreto
            WHERE  id_decreto = l_r_excepcion[v_i_inicio].id_decreto
            
            IF v_consec_cuenta IS NULL THEN
               LET v_consec_cuenta = 0
            END IF 
            
            -- se cambia el consecutivo a texto
            LET v_c_consecutivo_cuenta = v_consec_cuenta USING "&&&&&&&&&&&"
            
            LET l_s_cadena_detalle = "02",
                                     v_c_id_referencia,
                                     l_r_excepcion[v_i_inicio].tpo_ent_receptora,   
                                     l_r_excepcion[v_i_inicio].cve_ent_receptora,   
                                     l_r_excepcion[v_i_inicio].tpo_ent_cedente,     
                                     l_r_excepcion[v_i_inicio].cve_ent_cedente,     
                                     l_r_excepcion[v_i_inicio].origen_traspaso,     
                                     v_d_fecha_presentacion,
                                     v_d_fecha_movimiento,
                                     l_r_excepcion[v_i_inicio].curp,                
                                     l_r_excepcion[v_i_inicio].nss_afo_recep,
                                     v_c_filler1,
                                     l_r_excepcion[v_i_inicio].rfc_afo_recep,       
                                     l_r_excepcion[v_i_inicio].paterno_afo_recep,   
                                     l_r_excepcion[v_i_inicio].materno_afo_recep,   
                                     l_r_excepcion[v_i_inicio].nombres_afo_recep,
                                     v_c_filler2,   
                                     l_r_excepcion[v_i_inicio].cve_sector,     
                                     v_c_filler3,    
                                     v_d_fecharecep_solicitud,
                                     l_r_excepcion[v_i_inicio].id_lote_solicitud,   
                                     v_c_filler4, 
                                     l_r_excepcion[v_i_inicio].nss_icefa,           
                                     l_r_excepcion[v_i_inicio].rfc_icefa,           
                                     v_c_num_control_interno,           
                                     l_r_excepcion[v_i_inicio].paterno_icefa,       
                                     l_r_excepcion[v_i_inicio].materno_icefa,       
                                     l_r_excepcion[v_i_inicio].nombres_icefa,
                                     v_c_consecutivo_cuenta,
                                     v_c_filler5,
                                     v_c_monto_pesos,
                                     l_r_excepcion[v_i_inicio].aivs_viv92 * 1000000 USING "&&&&&&&&&&&&&&&",
                                     v_c_filler6,     
                                     v_c_num_monto_interes,
                                     v_c_filler7
            
            --se escribe el deatalle en el archivo
            CALL v_ch_arch_solTransf.writeLine([l_s_cadena_detalle])
            
            --se prepara el steatement de actualización de  result_operacion = 99  excepción confirmada 
            LET v_s_SqlQry = " UPDATE tia_det_traspaso",
                             " SET result_operacion = '99'",
                             " WHERE folio = ", l_r_excepcion[v_i_inicio].folio,
                             " AND id_referencia = ", l_r_excepcion[v_i_inicio].id_referencia,
                             " AND result_operacion in ('04') "     --proinfxvii-81
--                             " AND result_operacion in ('02','03','04','05','06','08') "  --proinfxvii-81
            
            --DISPLAY " v_s_SqlQry ",v_s_SqlQry
            PREPARE prp_actualiza_resul_opera FROM v_s_SqlQry
            --------EXECUTE prp_actualiza_resul_opera      -- se comenta para que siempre pueda generar las excepciones
         END IF
         
      END IF
   END FOR 
   
   --se asiga en variable caracter para escribir en el archivo	
   LET v_c_num_sum_monto_sdo     = v_d_sdo_viv92 * 100     USING "&&&&&&&&&&&&&&&" 
--   LET v_c_num_sum_monto_aiv     = v_d_aiv_viv92 * 1000000 USING "&&&&&&&&&&&&&&&&&&"   --xvii-81-03 
--   LET v_c_num_sum_monto_interes = v_d_int_viv92 * 100     USING "&&&&&&&&&&&&&&&&"     --xvii-81-03   
   LET v_c_num_sum_monto_aiv     = v_d_aiv_viv92 * 1000000 USING "&&&&&&&&&&&&&&&"        --xvii-81-03 
   LET v_c_num_sum_monto_interes = v_d_int_viv92 * 100     USING "&&&&&&&&&&&&&&&"        --xvii-81-03


   --se consulta el sumario 
   LET v_s_SqlQry = " SELECT *",
                    " FROM tia_sum_traspaso",
                    " WHERE folio =", p_folio
                    
   PREPARE prp_sum_traspaso FROM v_s_SqlQry
   EXECUTE prp_sum_traspaso INTO v_r_sum_traspaso.*
   
   LET  l_s_cadena_sum_traspaso  = v_r_sum_traspaso.tpo_registro   USING "&&",
                                   --v_contador_chkb1                USING "&&&&&&&&&",  --xvii-81-02
                                   v_i_contador_registros USING "&&&&&&&&&",             --xvii-81-02   
                                   v_c_filler10                     ,
                                   v_c_num_sum_monto_sdo            ,
                                   v_c_num_sum_monto_aiv            ,
                                   v_c_num_sum_monto_interes        ,
                                   v_c_filler9
   
   --se escribe el sumario en el archivo
   CALL v_ch_arch_solTransf.writeLine([l_s_cadena_sum_traspaso])
   
   LET INT_FLAG = 1

   -- HACE COPIA DE ARCHIVO DE RUTA ENVIO EN RUTA RESCATE
   LET v_comando = "cp ",v_c_ruta_env_acr CLIPPED,"/"|| v_v_nom_archi," ",v_c_ruta_res CLIPPED,"/"|| v_v_nom_archi --v_v_nom_archi2
   RUN v_comando

   LET v_comando = "cp ",v_c_ruta_env_acr CLIPPED,"/"|| v_v_nom_archi," ",v_c_ruta_env_acr CLIPPED,"/"|| "Excepciones.tia"
   RUN v_comando

   LET v_comando = "sh /opt/Interpel/Scripts/Excepciones.sh" 
   RUN v_comando

END FUNCTION

FUNCTION f_excepcion2(p_folio)
   
   DEFINE p_folio    LIKE  tia_det_traspaso.folio, --folio a desplegar
          v_s_SqlQry STRING                        --variabloe que almacena la consulta
          
   DEFINE l_r_excepcion DYNAMIC ARRAY OF RECORD             --record de registros 
      chkb              SMALLINT  ,   
      folio             LIKE tia_det_traspaso.folio              ,
      id_referencia     LIKE tia_det_traspaso.id_referencia      ,
      tpo_ent_receptora LIKE tia_det_traspaso.tpo_ent_receptora  ,
      cve_ent_receptora LIKE tia_det_traspaso.cve_ent_receptora  ,
      tpo_ent_cedente   LIKE tia_det_traspaso.tpo_ent_cedente    ,
      cve_ent_cedente   LIKE tia_det_traspaso.cve_ent_cedente    ,
      origen_traspaso   LIKE tia_det_traspaso.origen_traspaso    ,
      f_presentacion    LIKE tia_det_traspaso.f_presentacion     ,
      f_movimiento      LIKE tia_det_traspaso.f_movimiento       ,
      id_decreto        LIKE tia_det_traspaso.id_decreto         ,
      curp              LIKE tia_det_traspaso.curp               ,
      nss_afo_recep     LIKE tia_det_traspaso.nss_afo_recep      ,
      rfc_afo_recep     LIKE tia_det_traspaso.rfc_afo_recep      ,
      paterno_afo_recep LIKE tia_det_traspaso.paterno_afo_recep  ,
      materno_afo_recep LIKE tia_det_traspaso.materno_afo_recep  ,
      nombres_afo_recep LIKE tia_det_traspaso.nombres_afo_recep  ,
      cve_sector        LIKE tia_det_traspaso.cve_sector         ,
      f_recep_solicitud LIKE tia_det_traspaso.f_recep_solicitud  ,
      id_lote_solicitud LIKE tia_det_traspaso.id_lote_solicitud  ,
      nss_icefa         LIKE tia_det_traspaso.nss_icefa          ,
      rfc_icefa         LIKE tia_det_traspaso.rfc_icefa          ,
      nci_icefa         LIKE tia_det_traspaso.nci_icefa          ,
      paterno_icefa     LIKE tia_det_traspaso.paterno_icefa      ,
      materno_icefa     LIKE tia_det_traspaso.materno_icefa      ,
      nombres_icefa     LIKE tia_det_traspaso.nombres_icefa      ,
      sdo_viv92         LIKE tia_det_traspaso.sdo_viv92          ,
      int_viv92         LIKE tia_det_traspaso.int_viv92          ,
      result_operacion  LIKE tia_det_traspaso.result_operacion    ,
      aivs_viv92        LIKE tia_det_traspaso.aivs_viv92
   END RECORD
        
   DEFINE v_i_contador_registros     INTEGER,  ---contador de registros del array 
          p_usuario_cod              LIKE seg_usuario.usuario_cod, # Clave de usuario
          v_i_inicio                 INTEGER,
          v_contador_chkb1           INTEGER,
          v_r_cza_traspaso           RECORD LIKE tia_cza_traspaso.*,
          v_r_sum_traspaso           RECORD LIKE tia_sum_traspaso.*,
          l_s_cadena_detalle         STRING,
          l_s_cadena_cza_traspaso    STRING,
          l_s_cadena_sum_traspaso    STRING,
          v_c_filler1                CHAR(15),
          v_c_filler2                CHAR(3),
          v_c_filler3                CHAR(10),
          v_c_filler4                CHAR(15),
          v_c_consecutivo_cuenta     CHAR(11),
          v_c_filler5                CHAR(109),
          v_c_filler6                CHAR(80),
          v_c_filler7                CHAR(41),
          v_c_filler8                CHAR(698),     --se asigna según el layout de encabezado de tia 
          v_c_filler9                CHAR(554),     --se asigna según el layout de suamario de tia 
          v_c_filler10               CHAR(120),     --se asigna según el layout de suamario de tia        
          v_d_sdo_viv92              DECIMAL(22,2), --LIKE  tia_det_traspaso.sdo_viv92,
          v_d_aiv_viv92              DECIMAL(22,6),
          v_d_int_viv92              DECIMAL(22,2), --LIKE  tia_det_traspaso.int_viv92,
          v_v_nom_archi              VARCHAR(50),   --STRING, -- nombre del archivo de salida
          v_v_ruta_nomarch           STRING,        -- ruta y nombre del archivo de salida
          v_c_ruta_env_acr           LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
          v_c_ruta_res               LIKE seg_modulo.ruta_rescate,
          v_ch_arch_solTransf        BASE.CHANNEL,  -- manejador de apuntador hacia archivo
          v_d_fecha_presentacion_cza VARCHAR (8,0),
          v_d_fecha_presentacion     VARCHAR (8,0),
          v_d_fecha_movimiento       VARCHAR (8,0),
          v_d_fecharecep_solicitud   VARCHAR (8,0),
          v_c_tipo_registro          VARCHAR (2),
          v_c_id_referencia          VARCHAR (10),
          v_c_num_control_interno    VARCHAR (30),
          v_c_monto_pesos            VARCHAR (15),       
          v_c_num_monto_interes      VARCHAR (14),
          v_c_num_sum_monto_sdo      VARCHAR (15),
--          v_c_num_sum_monto_aiv      VARCHAR (18),  --xvii-81-03
--          v_c_num_sum_monto_interes  VARCHAR (16),  --xvii-81-03
          v_c_num_sum_monto_aiv      VARCHAR (15),    --xvii-81-03
          v_c_num_sum_monto_interes  VARCHAR (15),    --xvii-81-03          
          v_respuesta_confirma       INTEGER,
          v_consec_cuenta            DECIMAL(11,0)
       
   DEFINE v_comando STRING 

   LET INT_FLAG = 0
   
   --se inicializa los contador de registros 
   LET v_i_inicio             = 1    
   LET v_i_contador_registros = 1
   LET v_d_sdo_viv92          = 0
   LET v_d_aiv_viv92          = 0
   LET v_d_int_viv92          = 0
   LET v_contador_chkb1       = 0    
   LET v_c_tipo_registro      = 02
   LET v_respuesta_confirma   = 0

   --LET v_v_nom_archi = "Excepciones", v_v_nom_archi[4,5], v_v_nom_archi[7,8],'.TIA'
   -- se le agrega el folio al nombre del archivo

   LET l_s_cadena_detalle = p_folio USING "&&&&&&&&&"
   LET v_v_nom_archi = "ExcepcionesA", l_s_cadena_detalle,'.tia'   --xvii-81-02 se agregó letra "A"

   CALL STARTLOG(p_usuario_cod CLIPPED||".TIAP04.log")
   
   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio,
          ruta_rescate 
   INTO   v_c_ruta_env_acr,
          v_c_ruta_res
   FROM   seg_modulo
   WHERE  modulo_cod = 'tia'

   LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED,"/"|| v_v_nom_archi

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()
   
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")

   LET v_s_SqlQry = " SELECT 0 ,* ",
                    " FROM tia_det_traspaso",
                    " WHERE folio = ?",
                    " AND result_operacion = '04' "     --proinfxvii-81 
--                    " AND result_operacion not in ('01', '99', '10') "    --proinfxvii-81

   --DISPLAY " v_s_SqlQry  = " ,v_s_SqlQry

   -- se prepara y ejecuta la consulta
   PREPARE con_excepcion2 FROM v_s_SqlQry
   DECLARE c_excepcion2  CURSOR FOR con_excepcion2

   -- se transfieren los datos
   FOREACH c_excepcion2 USING p_folio 
      INTO l_r_excepcion[v_i_contador_registros].*
      LET  l_r_excepcion [v_i_contador_registros].chkb = 1 
   	  LET v_i_contador_registros =  v_i_contador_registros + 1
   END FOREACH

   --LET v_i_contador_registros = v_i_contador_registros - 1
   
   --se elimina el registro que esta de mas 
   CALL l_r_excepcion.deleteElement([l_r_excepcion.getLength()])
   
   DISPLAY "@@ v_i_contador_registros: ", v_i_contador_registros
     	
   --se cuentan todos los registros seleccionados
   FOR v_i_inicio = 1 TO  l_r_excepcion.getLength()

      LET l_r_excepcion[v_i_inicio].chkb  = 1
   	  --si se seleciona alguno se contabiliza    
   	  IF(l_r_excepcion[v_i_inicio].chkb  = 1) THEN
   	     --se suma el conatdor de registros 
   	     LET v_contador_chkb1  = v_contador_chkb1 + 1 
      END IF
   END FOR

   DISPLAY "@@ v_contador_chkb1: ", v_contador_chkb1

   --se consulta el encabezado 
   LET v_s_SqlQry = " SELECT * ",
                    " FROM tia_cza_traspaso",
                    " WHERE folio = ", p_folio
                    
   --DISPLAY " v_s_SqlQry ",v_s_SqlQry
   PREPARE prp_cza_traspaso2 FROM v_s_SqlQry
   EXECUTE prp_cza_traspaso2 INTO v_r_cza_traspaso.*  
    
   LET v_d_fecha_presentacion_cza = YEAR(v_r_cza_traspaso.f_presentacion ) USING "&&&&",MONTH(v_r_cza_traspaso.f_presentacion ) USING "&&",DAY(v_r_cza_traspaso.f_presentacion ) USING "&&"
    
   LET l_s_cadena_cza_traspaso = v_r_cza_traspaso.tpo_registro      ,
                                 v_r_cza_traspaso.id_servicio       ,
                                 v_r_cza_traspaso.id_operacion      ,
                                 v_r_cza_traspaso.tpo_ent_origen    ,
                                 v_r_cza_traspaso.cve_ent_origen    ,
                                 v_r_cza_traspaso.tpo_ent_destino   ,
                                 v_r_cza_traspaso.cve_ent_destino   ,
                                 v_c_filler2                        ,--se asigna por el tamaño de variable
                                 v_d_fecha_presentacion_cza         ,--PARA QUE NO SE IMPRIMNA LAS DIAGONALES DEL FORMATO DE FECHA 
                                 v_r_cza_traspaso.consecutivo_dia   ,
                                 v_r_cza_traspaso.cve_mod_presenta  , 
                                 v_c_filler8
   
   --se escribe el encabezado en el archivo
   CALL v_ch_arch_solTransf.writeLine([l_s_cadena_cza_traspaso])
   
   --se cicla el array para cada regisro selecionado chkb1 = 1
   FOR v_i_inicio = 1 TO l_r_excepcion.getLength()    

      LET v_consec_cuenta = 0
     
   	  --DISPLAY " l_r_excepcion[v_i_inicio].chkb1",l_r_excepcion[v_i_inicio].chkb1
      IF l_r_excepcion[v_i_inicio].chkb = 1 THEN
      
         LET v_d_fecha_presentacion   = YEAR(l_r_excepcion[v_i_inicio].f_presentacion) USING "&&&&",MONTH(l_r_excepcion[v_i_inicio].f_presentacion) USING "&&",DAY(l_r_excepcion[v_i_inicio].f_presentacion) USING "&&"
         LET v_d_fecha_movimiento     = YEAR(l_r_excepcion[v_i_inicio].f_movimiento) USING "&&&&",MONTH(l_r_excepcion[v_i_inicio].f_movimiento) USING "&&",DAY(l_r_excepcion[v_i_inicio].f_movimiento) USING "&&"
         LET v_d_fecharecep_solicitud = YEAR(l_r_excepcion[v_i_inicio].f_recep_solicitud) USING "&&&&",MONTH(l_r_excepcion[v_i_inicio].f_recep_solicitud) USING "&&",DAY(l_r_excepcion[v_i_inicio].f_recep_solicitud) USING "&&"
         
         LET v_c_id_referencia  = l_r_excepcion[v_i_inicio].id_referencia USING "&&&&&&&&&&"
         LET v_c_num_control_interno = l_r_excepcion[v_i_inicio].nci_icefa   USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"        
         
         --se  acumulan los importes
         LET v_d_sdo_viv92 = v_d_sdo_viv92 + l_r_excepcion[v_i_inicio].sdo_viv92 
         LET v_d_aiv_viv92 = v_d_aiv_viv92 + l_r_excepcion[v_i_inicio].aivs_viv92
         LET v_d_int_viv92 = v_d_int_viv92 + l_r_excepcion[v_i_inicio].int_viv92 
         
         
         LET l_r_excepcion[v_i_inicio].sdo_viv92  = l_r_excepcion[v_i_inicio].sdo_viv92 * 100     
         LET l_r_excepcion[v_i_inicio].int_viv92  = l_r_excepcion[v_i_inicio].int_viv92 * 100  
         
         LET v_c_monto_pesos         =  l_r_excepcion[v_i_inicio].sdo_viv92 USING "&&&&&&&&&&&&&&&"
         LET v_c_num_monto_interes   =  l_r_excepcion[v_i_inicio].int_viv92 USING "&&&&&&&&&&&&&&&"
         
         -- se obtiene el consecutivo de la cuenta
         SELECT consec_cuenta
         INTO   v_consec_cuenta
         FROM   afi_decreto
         WHERE  id_decreto = l_r_excepcion[v_i_inicio].id_decreto
         
         IF v_consec_cuenta IS NULL THEN
            LET v_consec_cuenta = 0
         END IF 
         
         -- se cambia el consecutivo a texto
         LET v_c_consecutivo_cuenta = v_consec_cuenta USING "&&&&&&&&&&&"
         
         LET l_s_cadena_detalle = "02",
                                  v_c_id_referencia,
                                  l_r_excepcion[v_i_inicio].tpo_ent_receptora,   
                                  l_r_excepcion[v_i_inicio].cve_ent_receptora,   
                                  l_r_excepcion[v_i_inicio].tpo_ent_cedente,     
                                  l_r_excepcion[v_i_inicio].cve_ent_cedente,     
                                  l_r_excepcion[v_i_inicio].origen_traspaso,     
                                  v_d_fecha_presentacion,
                                  v_d_fecha_movimiento,
                                  l_r_excepcion[v_i_inicio].curp,                
                                  l_r_excepcion[v_i_inicio].nss_afo_recep,
                                  v_c_filler1,
                                  l_r_excepcion[v_i_inicio].rfc_afo_recep,       
                                  l_r_excepcion[v_i_inicio].paterno_afo_recep,   
                                  l_r_excepcion[v_i_inicio].materno_afo_recep,   
                                  l_r_excepcion[v_i_inicio].nombres_afo_recep,
                                  v_c_filler2,   
                                  l_r_excepcion[v_i_inicio].cve_sector,     
                                  v_c_filler3,    
                                  v_d_fecharecep_solicitud,
                                  l_r_excepcion[v_i_inicio].id_lote_solicitud,   
                                  v_c_filler4, 
                                  l_r_excepcion[v_i_inicio].nss_icefa,           
                                  l_r_excepcion[v_i_inicio].rfc_icefa,           
                                  v_c_num_control_interno,           
                                  l_r_excepcion[v_i_inicio].paterno_icefa,       
                                  l_r_excepcion[v_i_inicio].materno_icefa,       
                                  l_r_excepcion[v_i_inicio].nombres_icefa,
                                  v_c_consecutivo_cuenta,
                                  v_c_filler5,
                                  v_c_monto_pesos,
                                  l_r_excepcion[v_i_inicio].aivs_viv92 * 1000000 USING "&&&&&&&&&&&&&&&",
                                  v_c_filler6,     
                                  v_c_num_monto_interes,
                                  v_c_filler7
         
         --se escribe el deatalle en el archivo
         CALL v_ch_arch_solTransf.writeLine([l_s_cadena_detalle])
         
         --se prepara el steatement de actualización de  result_operacion = 99  excepción confirmada 
         LET v_s_SqlQry = " UPDATE tia_det_traspaso",
                          " SET result_operacion = '99'",
                          " WHERE folio = ", l_r_excepcion[v_i_inicio].folio,
                          " AND id_referencia = ", l_r_excepcion[v_i_inicio].id_referencia,
                          " AND result_operacion in ('04') "                             --proinfxvii-81
--                          " AND result_operacion in ('02','03','04','05','06','08') "  --proinfxvii-81
         
         --DISPLAY " v_s_SqlQry ",v_s_SqlQry
         PREPARE prp_actualiza_resul_opera2 FROM v_s_SqlQry
         --------EXECUTE prp_actualiza_resul_opera2      -- se comenta para que siempre pueda generar las excepciones
      END IF
   END FOR 
   
   --se asiga en variable caracter para escribir en el archivo	
   LET v_c_num_sum_monto_sdo     = v_d_sdo_viv92 * 100     USING "&&&&&&&&&&&&&&&" 
--   LET v_c_num_sum_monto_aiv     = v_d_aiv_viv92 * 1000000 USING "&&&&&&&&&&&&&&&&&&" --xvii-81-03
--   LET v_c_num_sum_monto_interes = v_d_int_viv92 * 100     USING "&&&&&&&&&&&&&&&&"   --xvii-81-03
   LET v_c_num_sum_monto_aiv     = v_d_aiv_viv92 * 1000000 USING "&&&&&&&&&&&&&&&"      --xvii-81-03
   LET v_c_num_sum_monto_interes = v_d_int_viv92 * 100     USING "&&&&&&&&&&&&&&&"      --xvii-81-03

   --se consulta el sumario 
   LET v_s_SqlQry = " SELECT *",
                    " FROM tia_sum_traspaso",
                    " WHERE folio =", p_folio
                    
   PREPARE prp_sum_traspaso2 FROM v_s_SqlQry
   EXECUTE prp_sum_traspaso2 INTO v_r_sum_traspaso.*
   
   LET  l_s_cadena_sum_traspaso  = v_r_sum_traspaso.tpo_registro   USING "&&",
                                   v_contador_chkb1                USING "&&&&&&&&&"   , --v_r_sum_traspaso.registros_detalle,
                                   v_c_filler10                     ,
                                   v_c_num_sum_monto_sdo            ,
                                   v_c_num_sum_monto_aiv            ,
                                   v_c_num_sum_monto_interes        ,
                                   v_c_filler9
   
   --se escribe el sumario en el archivo
   CALL v_ch_arch_solTransf.writeLine([l_s_cadena_sum_traspaso])
   
   LET INT_FLAG = 1

   -- HACE COPIA DE ARCHIVO DE RUTA ENVIO EN RUTA RESCATE
   LET v_comando = "cp ",v_c_ruta_env_acr CLIPPED,"/"|| v_v_nom_archi," ",v_c_ruta_res CLIPPED,"/"|| v_v_nom_archi   --v_v_nom_archi2
   RUN v_comando

   LET v_comando = "cp ",v_c_ruta_env_acr CLIPPED,"/"|| v_v_nom_archi," ",v_c_ruta_env_acr CLIPPED,"/"|| "ExcepcionesA.tia"
   RUN v_comando

--   LET v_comando = "sh /opt/Interpel/Scripts/Excepciones.sh"  --xvii-81-02  se comenta porque este archivo no se envía a TRM
--   RUN v_comando

END FUNCTION