--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
--=============================================================================
-- Modulo       => TIA                                                                    
-- Programa     => TIAC12                                                                
-- Descripcion  => Lanzado Consulta de excepcion de Historicos del decreto
--              => registros que no se subieron en carga inicial y se
--              => creo la tabla cta_his_decreto guardarlos
-- Autor        => GERARDO ALFONSO VEGA PAREDES                                                     
-- Fecha inicio => 25-Feb-2015
--=============================================================================
DATABASE safre_viv

GLOBALS "TIAG01.4gl" --archivo de variables globales proceso_cod

GLOBALS
   DEFINE g_pid         LIKE bat_ctr_proceso.pid,      --  ID del proceso
          g_proceso_cod LIKE cat_proceso.proceso_cod,  -- codigo del proceso
          g_opera_cod   LIKE cat_operacion.opera_cod   -- codigo de operacion

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
   DEFINE p_usuario_cod             LIKE seg_usuario.usuario_cod,    -- clave del usuario firmado
          p_tipo_ejecucion          SMALLINT,                        -- forma como ejecutara el programa
          p_s_titulo                STRING,                          -- titulo de la ventana
          v_cbx_folios              ui.ComboBox,                     -- combo de afores
          v_proceso_cod             LIKE cat_proceso.proceso_cod,    -- codigo del proceso
          v_opera_cod               LIKE cat_operacion.opera_cod,    -- codigo de operacion
          v_folio_tia_det_traspaso  LIKE tia_det_traspaso.folio,     -- folio 
          v_query                STRING, 
          v_i_contador_tia_det_tras INTEGER,
          v_c_ruta_bin_acr          LIKE seg_modulo.ruta_bin,        -- ruta del bin de acr
          v_c_ruta_list_bat         LIKE seg_modulo.ruta_listados,   -- ruta listados de bat
          v_d_pid                   LIKE bat_ctr_proceso.pid,        -- identificador del proceso
          v_s_qryTxt                STRING,                          -- cadena del la sentencioa	SQL 
          v_ruta_vacia              STRING
       
   INITIALIZE v_folio_tia_det_traspaso TO NULL 

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se asigna proceso y operacion
   LET v_proceso_cod = g_proceso_cod_tia -- traspasos  I-A 
   LET v_opera_cod   = g_opera_cod_tia_liquidacion  -- archivo de salida para tesoreria

   -- se crea la sentencia sql que obtiene el pid perteneciente al folio
   LET v_s_qryTxt = "\n SELECT MAX(pid)",
                    "\n FROM   bat_ctr_proceso",
                    "\n WHERE  proceso_cod = ",v_proceso_cod
   
   --se prepara la sentencia SQL para su ejecución 
   PREPARE prp_unq_pid_batCtrProc FROM v_s_qryTxt
   --se ejecuta el steatment 
   EXECUTE prp_unq_pid_batCtrProc INTO v_d_pid
   
   --se inicializa el contador de regisdtros de detallle en cero 
   LET v_i_contador_tia_det_tras = 0
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF p_s_titulo IS NOT NULL THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   --llamad a funciones generales que regresan las rutas para generación de archvivos
   CALL fn_rutas("tia") RETURNING v_c_ruta_bin_acr, v_ruta_vacia
   CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_c_ruta_list_bat
    
   --se contabilizan los registros a seleccionar para saber si existen o no 
   SELECT COUNT (DISTINCT folio)
   INTO   v_i_contador_tia_det_tras 
   FROM   tia_det_traspaso
   WHERE  result_operacion = "10"
      
   --si existen registros en 02
   IF v_i_contador_tia_det_tras > 0 THEN 
   	  --abre ventana de despliageu de folios en estado 02
   	  OPEN WINDOW w_consulta_confirma_excepecion  WITH FORM "TIAC121"
     
      -- se le asigna el apuntado del combo a la variable
      LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio_tia_det_traspaso")
     
      -- se inicia el combobox en blanco
      CALL v_cbx_folios.clear()
      
      INPUT v_folio_tia_det_traspaso
      FROM cmb_folio_tia_det_traspaso 
      ATTRIBUTES (UNBUFFERED)

         BEFORE INPUT
         	  --se asigna la consulta 
            LET v_query = "\n SELECT DISTINCT folio",
                          "\n FROM   tia_det_traspaso",
                          "\n WHERE  result_operacion = '10' "
            
            --se prepara el steatment 
            PREPARE con_folio_tia FROM v_query
            
            --se declara el cursor 
            DECLARE cur_folio_tia CURSOR FOR con_folio_tia
            
            FOREACH cur_folio_tia INTO  v_folio_tia_det_traspaso   
             	 --se agregan los folios encontrados al combo
               CALL v_cbx_folios.addItem(v_folio_tia_det_traspaso, v_folio_tia_det_traspaso )
            END FOREACH
         
         ON ACTION CANCEL
           EXIT INPUT

         ON ACTION ACCEPT
         
            IF v_folio_tia_det_traspaso IS NULL THEN 
               CALL fn_mensaje("Atención","Es necesario seleccionar un folio	","Info") 
               NEXT FIELD cmb_folio_tia_det_traspaso
            ELSE 	
            	  --en el caso contrario se invoca a la función que despliega los registros a seleccionar 
               CALL f_excepcion (v_folio_tia_det_traspaso)	  
               EXIT INPUT
            END IF 
            EXIT INPUT
   
      END INPUT
      CLOSE WINDOW w_consulta_confirma_excepecion  
   ELSE	 
      --caso contrario no existen registros a mostrar
      CALL fn_mensaje("Atención","No existen registros a ser confirmados","Info") 
     
   END IF 
END MAIN

FUNCTION f_excepcion (p_folio)
   DEFINE 
      p_folio LIKE tia_det_traspaso.folio,  --folio a desplegar
      v_query STRING                        --variabloe que almacena la consulta
          
   DEFINE l_r_excepcion DYNAMIC ARRAY OF RECORD             --record de registros 
      chkb                 SMALLINT  ,   
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
      result_operacion     LIKE  tia_det_traspaso.result_operacion   ,
      aivs_viv92           LIKE  tia_det_traspaso.aivs_viv92
   END RECORD
   
   DEFINE
      v_i_contador_registros          INTEGER,  ---contador de registros del array 
      p_usuario_cod                   LIKE seg_usuario.usuario_cod, # Clave de usuario
      v_i_inicio                      INTEGER,
      v_contador_chkb1                INTEGER,
      v_r_cza_traspaso                RECORD LIKE tia_cza_traspaso.*,
      v_r_sum_traspaso                RECORD LIKE tia_sum_traspaso.*,
      l_s_cadena_detalle              STRING,
      l_s_cadena_cza_traspaso         STRING,
      l_s_cadena_sum_traspaso         STRING,
      v_c_filler1                     CHAR(15),
      v_c_filler2                     CHAR(3),
      v_c_filler3                     CHAR(10),
      v_c_filler4                     CHAR(15),
      v_c_consecutivo_cuenta          CHAR(11),
      v_c_filler5                     CHAR(109),
      v_c_filler6                     CHAR(80),
      v_c_filler7                     CHAR(41),
      v_c_filler8                     CHAR(698),--se asigna según el layout de encabezado de tia 
      v_c_filler9                     CHAR(554),--se asigna según el layout de suamario de tia 
      v_c_filler10                    CHAR(120),--se asigna según el layout de suamario de tia        
      v_d_sdo_viv92                   DECIMAL(22,2),--LIKE  tia_det_traspaso.sdo_viv92,
      v_d_aiv_viv92                   DECIMAL(22,6),
      v_d_int_viv92                   DECIMAL(22,2),--LIKE  tia_det_traspaso.int_viv92,
      v_v_nom_archi                   VARCHAR(50),--STRING, -- nombre del archivo de salida
      v_v_nom_archi2                  VARCHAR(50),--STRING, -- nombre del archivo de salida
      v_v_ruta_nomarch                STRING, -- ruta y nombre del archivo de salida
      v_c_ruta_env_acr                LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
      v_c_ruta_res                    LIKE seg_modulo.ruta_rescate,
      v_ch_arch_solTransf             BASE.CHANNEL, -- manejador de apuntador hacia archivo
      v_d_fecha_presentacion_cza      VARCHAR (8,0),
      v_d_fecha_presentacion          VARCHAR (8,0),
      v_d_fecha_movimiento            VARCHAR (8,0),
      v_d_fecharecep_solicitud        VARCHAR (8,0),
      v_c_tipo_registro               VARCHAR (2),
      v_c_id_referencia               VARCHAR (10),
      v_c_num_control_interno         VARCHAR (30),
      v_c_monto_pesos                 VARCHAR (15),       
      v_c_num_monto_interes           VARCHAR (14),
      v_c_num_sum_monto_sdo           VARCHAR (15),
      v_c_num_sum_monto_aiv           VARCHAR (15),
      v_c_num_sum_monto_interes       VARCHAR (15),
      v_respuesta_confirma            INTEGER,
      v_consec_cuenta                 DECIMAL(11,0)
       
   DEFINE 
      v_ind_consistencia SMALLINT,
      v_pid DECIMAL(9,0),
      v_cadena_txt STRING,
      v_aivs_viv_string STRING,
      v_sdo_viv_string STRING
       
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

   LET l_s_cadena_detalle = p_folio USING "&&&&&&&&&"
   LET v_v_nom_archi = "Historicos", l_s_cadena_detalle,'.tia'

   CALL STARTLOG(p_usuario_cod CLIPPED||".TIAC12.log")
   
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

   LET v_query = "\n SELECT 0 ,* ",
                 "\n FROM tia_det_traspaso",
                 "\n WHERE folio = ?",
                 "\n AND result_operacion = 10 "   -- estado de si encontro en his_decreto

   --DISPLAY " v_query = " ,v_query

   -- se prepara y ejecuta la consulta
   PREPARE con_excepcion FROM v_query
   DECLARE c_excepcion  CURSOR FOR con_excepcion

   -- se transfieren los datos
   FOREACH c_excepcion USING p_folio 
   INTO l_r_excepcion[v_i_contador_registros].*
   	   LET v_i_contador_registros =  v_i_contador_registros + 1
   END FOREACH

   --LET v_i_contador_registros = v_i_contador_registros - 1
   
   --se elimina el registro que esta de mas 
   CALL l_r_excepcion.deleteElement([l_r_excepcion.getLength()])
   
   DISPLAY "@@ v_i_contador_registros: ", v_i_contador_registros
   
   --se abre vetana para la seleccion de excepciones 
   OPEN WINDOW w_excepcion  WITH FORM "TIAC122"

   INPUT ARRAY l_r_excepcion FROM tbl_excepcion.*
   ATTRIBUTES(WITHOUT DEFAULTS, UNBUFFERED,
              INSERT ROW = FALSE,
              DELETE ROW = FALSE,
              AUTO APPEND= FALSE,
              APPEND ROW = FALSE)
                 
      --en el botón aceptar 
      ON ACTION ACCEPT

         --se cuentan todos los registros seleccionados
         FOR v_i_inicio = 1 TO  l_r_excepcion.getLength()
             --si se seleciona alguno se contabiliza    
             IF(l_r_excepcion[v_i_inicio].chkb  = 1) THEN
                --se suma el conatdor de registros 
                LET v_contador_chkb1  = v_contador_chkb1 + 1 
             END IF
         END FOR

         DISPLAY "@@ v_contador_chkb1: ", v_contador_chkb1
       
         --DISPLAY " v_contador_chkb1 ",v_contador_chkb1
         --si el contador de selecion es = 0 se envia mensaje 
         IF v_contador_chkb1 = 0 THEN      	    
            CALL fn_mensaje("Confirma Hitórico","Es necesario seleccionar un registro  ","about")
            CONTINUE INPUT
         --en el caso contrario se seleciono un aexcepción por lo que se crea el archivo de excepción   	
         ELSE   
            --se cicla el array para cada regisro selecionado chkb1  = 1
            FOR v_i_inicio = 1 TO  l_r_excepcion.getLength()    

               LET v_consec_cuenta = 0
               
               IF (l_r_excepcion[v_i_inicio].chkb  = 1) THEN
              
                  -- se obtiene el consecutivo de la cuenta
                  SELECT ind_consistencia
                  INTO   v_ind_consistencia
                  FROM   afi_decreto
                  WHERE  id_decreto = l_r_excepcion[v_i_inicio].id_decreto

                  IF v_consec_cuenta IS NULL THEN
                     LET v_consec_cuenta = 0
                  END IF 
             
                  -- ejecuta liquidación de históricos
                  LET v_query = "EXECUTE PROCEDURE sp_preliquida_tia ( ",
                                 p_folio,",'",
                                 p_usuario_cod,"',",
                                 l_r_excepcion[v_i_inicio].id_referencia,",", 
                                 l_r_excepcion[v_i_inicio].sdo_viv92,",",
                                 l_r_excepcion[v_i_inicio].id_decreto,",'",
                                 l_r_excepcion[v_i_inicio].f_movimiento,"','",
                                 l_r_excepcion[v_i_inicio].nci_icefa,"',",
                                 l_r_excepcion[v_i_inicio].aivs_viv92,",",
                                 v_ind_consistencia,",'",
                                 l_r_excepcion[v_i_inicio].nss_afo_recep,"',",
                                 l_r_excepcion[v_i_inicio].origen_traspaso,",'",
                                 l_r_excepcion[v_i_inicio].curp,"'",
                                 ")"
                                 
                   PREPARE exe_preliquida FROM v_query
                   EXECUTE exe_preliquida 

                  LET v_aivs_viv_string= l_r_excepcion[v_i_inicio].aivs_viv92
                  LET v_sdo_viv_string=l_r_excepcion[v_i_inicio].sdo_viv92

                CALL v_ch_arch_solTransf.write("Folio|"||"ID Referencia|"||"Fecha Movimiento|"
                                                                            ||"NCI Icefa|"||"CURP|"||"Tot SDO VIV 92|"||"Tot AIVS VIV 92|"
                                                                            ||"Ind. Consistencia|"||"NSS| "||"Origen traspaso")


                 LET v_cadena_txt=p_folio,"|",
                                                     l_r_excepcion[v_i_inicio].id_referencia CLIPPED,"|", 
                                                     l_r_excepcion[v_i_inicio].f_movimiento,"|",
                                                     l_r_excepcion[v_i_inicio].nci_icefa CLIPPED,"|", 
                                                     l_r_excepcion[v_i_inicio].curp,"|" ,                                                
                                                     v_sdo_viv_string,"|",
                                                     v_aivs_viv_string,"|",
                                                     v_ind_consistencia,"|",
                                                     l_r_excepcion[v_i_inicio].nss_afo_recep,"|",
                                                     l_r_excepcion[v_i_inicio].origen_traspaso,"|"

                  CALL v_cadena_txt.trim() RETURNING v_cadena_txt
                  CALL v_ch_arch_solTransf.writeLine(v_cadena_txt)
                  -- se cambia el consecutivo a texto
                  LET v_c_consecutivo_cuenta = v_consec_cuenta USING "&&&&&&&&&&&"
                  
                  --se prepara el steatement de actualización de  result_operacion = 01 p/q quede p/preliquidar 
                  LET v_query = "\n UPDATE tia_det_traspaso ",
                                "\n SET    result_operacion = '01' ",
                                "\n WHERE  folio = ", l_r_excepcion[v_i_inicio].folio,
                                "\n AND    id_referencia = ", l_r_excepcion[v_i_inicio].id_referencia,
                                "\n AND    result_operacion = '10' "
                
                  PREPARE prp_actualiza_resul_opera FROM v_query
                  EXECUTE prp_actualiza_resul_opera

              --========== FUNCIONALIDAD DE REVERSO DE LIQUIDACION =============

                  LET v_pid = NULL
                  SELECT pid
                  INTO   v_pid
                  FROM   bat_ctr_proceso
                  WHERE  folio = l_r_excepcion[v_i_inicio].folio
                  AND    estado_cod = 4

                  IF v_pid IS NOT NULL THEN   
              
                     --SE ACTUALIZA LA TABLA GLO_FOLIO A ESTATUS 1
                     LET v_query  = "\n UPDATE glo_folio ",
                                    "\n SET    status = 1 ",
                                    "\n WHERE  proceso_cod = 1701 ",
                                    "\n AND    status = 2 ",
                                    "\n AND    folio  = ", l_r_excepcion[v_i_inicio].folio
                  
                     PREPARE prp_actualiza_status FROM v_query
                     EXECUTE prp_actualiza_status

                     --SE ACTUALIZA LA TABLA GLO_CTR_ARCHIVO A ESTADO 3
                     LET v_query ="\n UPDATE glo_ctr_archivo  ",
                                  "\n SET    estado = 3  ",
                                  "\n WHERE proceso_cod = 1701 ",
                                  "\n AND   estado = 4 ",
                                  "\n AND   folio = ",l_r_excepcion[v_i_inicio].folio

                     PREPARE prp_actualiza_estado FROM v_query
                     EXECUTE prp_actualiza_estado

                     LET v_query = "\n EXECUTE FUNCTION fn_reversa_operacion (",v_pid,",1701,4) "
                     DISPLAY "v_query ",v_query
                     PREPARE prp_actualiza_reverso FROM v_query
                     EXECUTE prp_actualiza_reverso INTO v_pid

                  END IF 
                --==============================================================                         
                  
               END IF
            END FOR 
             
            LET INT_FLAG = 1
            --se manda mensaje de la generación del archivo de excepción                       
            CALL fn_mensaje ("Atención","Se generó Pre Liquidación de Histórico","Info")
            EXIT INPUT	

         END IF
         CALL v_ch_arch_solTransf.close()
         EXIT INPUT

      -- ON ACTION Reporte

      ON ACTION CANCEL
         LET INT_FLAG = 0
         EXIT INPUT

      -- seleccionar todos
      ON ACTION Todos
         FOR v_i_inicio = 1 TO  l_r_excepcion.getLength()
            -- se marcan los registros
            LET l_r_excepcion[v_i_inicio].chkb = 1
         END FOR

      ON ACTION Ninguno
         FOR v_i_inicio = 1 TO  l_r_excepcion.getLength()
          -- se marcan los registros
          LET l_r_excepcion[v_i_inicio].chkb = 0
         END FOR

   END INPUT 
  
   CLOSE WINDOW w_excepcion
END FUNCTION
