--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGC04                                                                 #
#Objetivo     => Lanzador del  consulta de archivo de rechazos                          #
#Autor        => Rubén Haro Castro                                                      #
#Fecha inicio => Junio 20, 2012                                                          #
#########################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo     RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat   RECORD
        ruta_listados   CHAR(40)
       END RECORD,
       g_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado

END GLOBALS

MAIN
DEFINE p_tipo_ejecucion      SMALLINT
       ,p_s_titulo           STRING                       -- titulo de la ventana      
       ,v_cbx_folios         ui.ComboBox                  -- combo de afores              
       ,v_folio_pag_fc       LIKE pag_det_fc.folio --folio 
       ,v_s_SqlQry           STRING 
       ,v_i_contador_pag_fc  INTEGER
       ,v_c_ruta_bin_acr     LIKE seg_modulo.ruta_bin -- ruta del bin de acr
       ,v_c_ruta_list_bat    LIKE seg_modulo.ruta_listados -- ruta listados de bat
       ,v_ruta_vacia         STRING
       
   INITIALIZE v_folio_pag_fc TO NULL 

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se asigna proceso y operacion
   --LET v_proceso_cod = 1405  --proceso de fortalecimiento de crédito
   --se asigan 0 al contador de registros
   LET v_i_contador_pag_fc = 0
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   --llamad a funciones generales que regresan las rutas para generación de archvivos
   CALL fn_rutas("pag") RETURNING v_c_ruta_bin_acr, v_ruta_vacia
   CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_c_ruta_list_bat
    
    --se contabilizan los registros a seleccionar para saber si existen o no 
    SELECT COUNT (DISTINCT folio)
       INTO v_i_contador_pag_fc 
       FROM pag_det_fc
      WHERE result_operacion = '02'
   --sí existen registros en 02
   IF (v_i_contador_pag_fc > 0) THEN 
   	 --abre ventana de despliageu de folios en estado 02
   	 OPEN WINDOW w_consulta_confirma_rechazos  WITH FORM "PAGC041"
     
       -- se le asigna el apuntado del combo a la variable
       LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio_pag_fc")
     
       -- se inicia el combobox en blanco
       CALL v_cbx_folios.clear()
       
     
       INPUT v_folio_pag_fc
        FROM cmb_folio_pag_fc 
        ATTRIBUTES (UNBUFFERED)

          BEFORE INPUT
          	--se asigna la consulta 
            LET v_s_SqlQry = 
            "SELECT DISTINCT folio  \n",
            " FROM pag_det_fc  \n",
            " WHERE  result_operacion = '02' "
            --se prepara el steatment 
            PREPARE con_folio_fc FROM v_s_SqlQry
            --se declara el cursor 
            DECLARE cur_folio_fc CURSOR FOR con_folio_fc
            FOREACH cur_folio_fc INTO  v_folio_pag_fc   
            	--se agregan los folios encontrados al combo
              CALL v_cbx_folios.addItem(v_folio_pag_fc, v_folio_pag_fc )
            END FOREACH
          
          ON ACTION CANCEL
            EXIT INPUT

          ON ACTION ACCEPT
           --DISPLAY "v_c_ruta_bin_acr",v_c_ruta_bin_acr
           
            IF (v_folio_pag_fc IS NULL) THEN 
            	CALL fn_mensaje("Atención","Es necesario seleccionar un folio	","Info") 
              NEXT FIELD cmb_folio_pag_fc
            ELSE 	
            	 --en el caso contrario se invoca a la función que despliega los registros a seleccionar 
               CALL f_rechazos (v_folio_pag_fc)

               --se manda mensaje de la generación del archivo de excepción 
               CALL fn_mensaje ("Atención","Se generó el archivo de Rechazados","Info")

               EXIT INPUT
            	END IF 
            EXIT INPUT
       END INPUT
     CLOSE WINDOW w_consulta_confirma_rechazos  
   ELSE	 
     --caso contrario no existen registros a mostrar
     CALL fn_mensaje("Atención","No existen registros rechazados","Info") 
     
   END IF 
END MAIN

{
======================================================================
Clave: 
Nombre: f_rechazos
Fecha creacion: Marzo 02, 2012
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Ejecurta el programa que genera el archivo de rechazos

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION f_rechazos(p_folio)
DEFINE  p_folio                   LIKE  tia_det_traspaso.folio,  --folio a desplegar
        v_s_SqlQry                STRING,                        --variabloe que almacena la consulta        
        v_r_pag_cza_fc            RECORD LIKE  pag_cza_fc.*  ,
        v_r_pag_det_fc            DYNAMIC ARRAY OF RECORD LIKE  pag_det_fc.*,
        v_r_pag_sum_fc            RECORD LIKE  pag_sum_fc.*,
        v_c_fecha_hoy             VARCHAR(8),
        v_c_nss                   CHAR(11),
        --v_c_f_pago                VARCHAR(8),
        l_s_cadena_detalle        STRING,
        l_s_cadena_cza_fc         STRING,
        l_s_cadena_sum_fc         STRING,
        v_v_nom_archi             VARCHAR(50),--STRING, -- nombre del archivo de salida
        v_v_ruta_nomarch          STRING, -- ruta y nombre del archivo de salida
        v_c_ruta_env_acr          LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
        v_ch_arch_solTransf       BASE.CHANNEL, -- manejador de apuntador hacia archivo
        v_c_tipo_registro         VARCHAR (2),
        v_i_contador_reg          INTEGER,
        v_s_comando               STRING,
        v_c_fecha_pag             VARCHAR(8),
        v_ruta_bin                VARCHAR(40) -- ruta del directorio ejecutable del modulo
        
   -- se obtiene la ruta del directorio ejecutable
   SELECT ruta_bin
   INTO   v_ruta_bin
   FROM   seg_modulo
   WHERE  modulo_cod = "pag"
        
   -- se ejecuta el programa que genera el archivo de salida
   -- solo se necesita el folio
   LET v_s_comando = " fglrun ", v_ruta_bin CLIPPED,"/PAGS01 ",
                     g_usuario_cod CLIPPED, " ",
                     0 , " " , -- pid
                     g_proceso_cod_pag_registro_pagos_fc , " " , -- proceso
                     0 ," ", -- opera
                     p_folio ," ",
                     "NA" -- archivo
   DISPLAY v_s_comando                        
   RUN v_s_comando

END FUNCTION









#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGC04                                                                 #
#Objetivo     => Función que recibe el folio y selecciona los rechazos                  #
#                y genera el archivo de excepciones generadas                           #
#Autor        => Rubén Haro Castro                                                      #
#Fecha inicio => Junio  20, 2012                                                        #
#########################################################################################
{
FUNCTION f_rechazos(p_folio)
DEFINE  p_folio                   LIKE  tia_det_traspaso.folio,  --folio a desplegar
        v_s_SqlQry                STRING,                        --variabloe que almacena la consulta        
        v_r_pag_cza_fc            RECORD LIKE  pag_cza_fc.*  ,
        v_r_pag_det_fc            DYNAMIC ARRAY OF RECORD LIKE  pag_det_fc.*,
        v_r_pag_sum_fc            RECORD LIKE  pag_sum_fc.*,
        v_c_fecha_hoy             VARCHAR(8),
        v_c_nss                   CHAR(11),
        --v_c_f_pago                VARCHAR(8),
        l_s_cadena_detalle        STRING,
        l_s_cadena_cza_fc         STRING,
        l_s_cadena_sum_fc         STRING,
        v_v_nom_archi             VARCHAR(50),--STRING, -- nombre del archivo de salida
        v_v_ruta_nomarch          STRING, -- ruta y nombre del archivo de salida
        v_c_ruta_env_acr          LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
        v_ch_arch_solTransf       BASE.CHANNEL, -- manejador de apuntador hacia archivo
        v_c_tipo_registro         VARCHAR (2),
        v_i_contador_reg          INTEGER,
        v_s_fec_tmp               STRING,
        v_c_fecha_pag             VARCHAR(8)

   LET INT_FLAG = 0
   --se inicializa los contador de registros 

   LET v_c_tipo_registro = '02'
   LET v_v_nom_archi = TIME
   LET v_v_nom_archi = "Rechazos", v_v_nom_archi[4,5], v_v_nom_archi[7,8],'.FORT'
   
   LET v_s_fec_tmp = TODAY  USING "yyyymmdd "
   LET v_c_fecha_hoy = v_s_fec_tmp
       
   LET v_i_contador_reg = 1     
                               
 
   DISPLAY "v_c_fecha_hoy",v_c_fecha_hoy

   ---CALL STARTLOG(p_usuario_cod CLIPPED||".PAGC04.log")
   
   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
     INTO v_c_ruta_env_acr
     FROM seg_modulo
    WHERE modulo_cod = 'pag'

   -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   --LET v_v_nom_archi = "/",YEAR(TODAY) USING "&&&&",MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&",".TIA"

   LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED ||"/"|| v_v_nom_archi
   DISPLAY "  v_v_ruta_nomarch = ",v_v_ruta_nomarch
   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()
   
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")

    --se consulta el encabezado 
    LET v_s_SqlQry =  
    " SELECT * \n", 
    "   FROM pag_cza_fc \n", 
    "  WHERE folio =", p_folio
    --DISPLAY " v_s_SqlQry ",v_s_SqlQry
    PREPARE prp_cza_fc FROM v_s_SqlQry
    EXECUTE prp_cza_fc INTO v_r_pag_cza_fc.*  
    --se escribe el encabezado en el archivo    
    LET l_s_cadena_cza_fc  = v_r_pag_cza_fc.tpo_registro   ,
                             v_r_pag_cza_fc.proceso_cod    ,
                             "01"     ,
                             v_c_fecha_hoy

    --se escribe el encabezado en el archivo
    CALL v_ch_arch_solTransf.writeLine([l_s_cadena_cza_fc])  
    --se asigna en cero el número de registros en estado rechazados
    LET v_r_pag_sum_fc.num_reg_detalle = 0              
    --se asigna en cero el importe de registros en estado rechazados
    LET v_r_pag_sum_fc.tot_ap_fc = 0
    --se consulta el detalle 
    LET v_s_SqlQry =
    " SELECT * \n", 
    "   FROM pag_det_fc \n", 
    "  WHERE folio =", p_folio ,"\n",
    "    AND result_operacion = '02' "  
    --DISPLAY " v_s_SqlQry ",v_s_SqlQry
    PREPARE con_det_fc FROM v_s_SqlQry
    DECLARE c_det_fc CURSOR FOR  con_det_fc
    FOREACH c_det_fc INTO v_r_pag_det_fc[v_i_contador_reg].*  
      
       SELECT nss
         INTO v_c_nss
         FROM afi_derechohabiente 
        WHERE id_derechohabiente = v_r_pag_det_fc[v_i_contador_reg].id_derechohabiente
         
       LET v_c_fecha_pag =  v_r_pag_det_fc[v_i_contador_reg].f_pago  USING "yyyymmdd"
       
       LET  l_s_cadena_detalle   = v_c_tipo_registro,
                        v_r_pag_det_fc[v_i_contador_reg].id_referencia   ,
                        v_c_fecha_pag          , 
                        v_c_nss                ,
                        v_r_pag_det_fc[v_i_contador_reg].imp_ap_fc
       --se escribe el deatalle en el archivo
       CALL v_ch_arch_solTransf.writeLine([l_s_cadena_detalle])       
       --se suma el importe de registros en estado rechazado
       LET v_r_pag_sum_fc.tot_ap_fc = v_r_pag_sum_fc.tot_ap_fc + v_r_pag_det_fc[v_i_contador_reg].imp_ap_fc 
       --se hace el conteo de registros en estado rechazado   
       LET v_i_contador_reg =  v_i_contador_reg + 1
    END FOREACH
    
    --se resta el indice de mas 
    LET v_i_contador_reg = v_i_contador_reg - 1
    
    --se asigna el total de registros para el suamrio
    LET  v_r_pag_sum_fc.num_reg_detalle =  v_i_contador_reg
     --se consulta el sumario 
     LET v_s_SqlQry =  
     " SELECT * \n", 
     "   FROM pag_sum_fc \n", 
     "  WHERE folio =", p_folio
     PREPARE prp_sum_traspaso FROM v_s_SqlQry
     EXECUTE prp_sum_traspaso INTO v_r_pag_sum_fc.*
     

      LET  l_s_cadena_sum_fc  = "09"           ,
                               v_i_contador_reg   ,
                               v_r_pag_sum_fc.tot_ap_fc    
                                     
      --se escribe el sumario en el archivo
      CALL v_ch_arch_solTransf.writeLine([l_s_cadena_sum_fc])
END FUNCTION
}