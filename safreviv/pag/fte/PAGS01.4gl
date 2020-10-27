--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGS01                                                                 #
#Objetivo     => Generacion del archivo de rechazos de FORTALECIMIENTO AL CREDITO       #
#Autor        => Rubén Haro Castro                                                      #
#Fecha inicio => Junio 20, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_i_folio            LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo     LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera            INTEGER
       ,v_i_contador_reg     INTEGER
       ,v_mensaje            VARCHAR(255)
       ,v_s_sql              STRING, -- cadena con un enunciado SQL
       v_marca_disposicion   SMALLINT,
       v_edo_maraca          SMALLINT,
       v_caus_marac          SMALLINT,
       v_cod_rechazo         SMALLINT 

   -- se recuperan los parametros de la linea de comandos
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_i_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)    
   
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".PAGS01.log")

   -- en el caso contrario se invoca a la función que despliega los registros a seleccionar 
   CALL f_rechazos (p_i_folio)

END MAIN
#########################################################################################
#Funcion      => f_rechazos                                                             #
#Objetivo     => Funcion que recibe el folio y selecciona los rechazos                  #
#                y genera el archivo de excepciones generadas                           #
#Autor        => Rubén Haro Castro                                                      #
#Fecha inicio => Junio  20, 2012                                                        #
#########################################################################################

FUNCTION f_rechazos (p_folio)
DEFINE  p_folio                   LIKE  tia_det_traspaso.folio,  --folio a desplegar
        v_s_SqlQry                STRING,                        --variabloe que almacena la consulta        
        v_r_pag_cza_fc            RECORD LIKE  pag_cza_fc.*  ,
        v_r_pag_det_fc            RECORD LIKE  pag_det_fc.*,
        v_r_pag_sum_fc            RECORD LIKE  pag_sum_fc.*,
        v_imp_ap_fc               INTEGER,
        v_c_fecha_hoy             VARCHAR(8),
        v_c_nss                   CHAR(11),
        l_s_cadena_detalle        STRING,
        l_s_cadena_cza_fc         STRING,
        l_s_cadena_sum_fc         STRING,
        v_v_nom_archi             VARCHAR(100),--STRING, -- nombre del archivo de salida
        v_v_ruta_nomarch          STRING, -- ruta y nombre del archivo de salida
        v_c_ruta_env_acr          LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
        v_ch_arch_solTransf       BASE.CHANNEL, -- manejador de apuntador hacia archivo
        v_c_tipo_registro         VARCHAR (2),
        v_i_contador_reg          INTEGER,
        v_s_fec_tmp               STRING,
        v_c_fecha_pag             VARCHAR(8),
        v_folio_formato           VARCHAR(9),
        v_archivo_original        VARCHAR(40),
        v_longitud                SMALLINT

   -- se verifica si hay rechazados, de lo contrario no se genera archivo
   SELECT COUNT(*)
   INTO   v_i_contador_reg
   FROM   pag_det_fc
   WHERE  folio = p_folio
   AND    result_operacion = '02'

   -- si no hay rechazados, entonces no se genera archivo
   IF ( v_i_contador_reg < 1 ) THEN
      DISPLAY "No existen registros rechazados. No se generará archivo de rechazos."
      EXIT PROGRAM
   END IF

   LET INT_FLAG = 0
   
   LET v_c_tipo_registro = '02'
   LET v_v_nom_archi = TIME
   LET v_v_nom_archi = "Rechazos", v_v_nom_archi[4,5], v_v_nom_archi[7,8],'.FORT'
   
   LET v_s_fec_tmp = TODAY  USING "yyyymmdd "
   LET v_c_fecha_hoy = v_s_fec_tmp
                                    
   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
     INTO v_c_ruta_env_acr
     FROM seg_modulo
    WHERE modulo_cod = 'pag'

   -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   --LET v_v_nom_archi = "/",YEAR(TODAY) USING "&&&&",MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&",".TIA"

   -- el nombre del archivo es:
   -- el mismo nombre que el archivo original + _FOLIO_rechazoFC.fort
   SELECT nombre_archivo
   INTO   v_archivo_original
   FROM   glo_ctr_archivo
   WHERE  proceso_cod = g_proceso_cod
   AND    folio = p_folio

   DISPLAY "archivo ", v_archivo_original

   -- longitud del nombre del archivo original (menus 5 para quitar .fort)
   LET v_longitud = LENGTH(v_archivo_original CLIPPED) - 5

   LET v_folio_formato = p_folio USING "&&&&&&&&&"
 
   DISPLAy "longitud ", v_longitud
 
   LET v_v_nom_archi    = v_archivo_original[1,v_longitud] || "_" || v_folio_formato || "_rechazoFC.fort"
   LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED ||"/"|| v_v_nom_archi
   
   
   DISPLAY " ARCHIVO DE RECHAZOS GENERADO: ",v_v_ruta_nomarch
   
   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()
   
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")

    --se consulta el encabezado 
    LET v_s_SqlQry = "\n SELECT * "       , 
                     "\n FROM  pag_cza_fc",
                     "\n WHERE folio = "  , p_folio
    
    --DISPLAY " v_s_SqlQry ",v_s_SqlQry
    
    PREPARE prp_cza_fc FROM v_s_SqlQry
    EXECUTE prp_cza_fc INTO v_r_pag_cza_fc.*  
    
    --se escribe el encabezado en el archivo    
    {
    1	Tipo de Registro	X	02	0	001	-	002	01 Encabezado
    2	Identificador de Proceso	X	04	0	003	-	006	1405 Fortalecimeinto Crédito
    3	Identificador de Operación	X	02	0	007	-	008	01 Carga Archivo
    4	Fecha de Archivo	X	08	0	009	-	016	Fecha válida con formato igual a AAAAMMDD
    }
    --LET l_s_cadena_cza_fc  = v_r_pag_cza_fc.tpo_registro   ,
    --                         v_r_pag_cza_fc.proceso_cod    ,
    --                         "01"     ,
    --                         v_c_fecha_hoy

    LET l_s_cadena_cza_fc  = "01140501", v_c_fecha_hoy

    --se escribe el encabezado en el archivo
    CALL v_ch_arch_solTransf.writeLine([l_s_cadena_cza_fc])  
    
    --se asigna en cero el número de registros en estado rechazados
    LET v_r_pag_sum_fc.num_reg_detalle = 0              
    
    --se asigna en cero el importe de registros en estado rechazados
    LET v_r_pag_sum_fc.tot_ap_fc = 0
    
    --se consulta el detalle 
    LET v_s_SqlQry = "\n SELECT * "                       , 
                     "\n   FROM pag_det_fc "              ,
                     "\n  WHERE folio = ", p_folio        ,
                     "\n    AND result_operacion = '02' "  

    --DISPLAY " v_s_SqlQry ",v_s_SqlQry
    LET v_i_contador_reg = 0
    
    PREPARE con_det_fc FROM v_s_SqlQry
    DECLARE c_det_fc CURSOR FOR  con_det_fc
    FOREACH c_det_fc INTO v_r_pag_det_fc.*  
              
       LET v_c_fecha_pag = v_r_pag_det_fc.f_pago USING "yyyymmdd"
       
       -- se le quitan los decimales al monto de aportacion
       LET v_imp_ap_fc   = v_r_pag_det_fc.imp_ap_fc * 100
       
       LET l_s_cadena_detalle = v_c_tipo_registro                             ,
                                v_r_pag_det_fc.id_referencia USING "&&&&&&&&&&",
                                v_c_fecha_pag                                 , 
                                v_r_pag_det_fc.nss                            ,
                                v_imp_ap_fc USING "&&&&&&&&"

       --se escribe el deatalle en el archivo
       CALL v_ch_arch_solTransf.writeLine([l_s_cadena_detalle])       

       --se suma el importe de registros en estado rechazado
       LET v_r_pag_sum_fc.tot_ap_fc = v_r_pag_sum_fc.tot_ap_fc + v_r_pag_det_fc.imp_ap_fc 

       --se hace el conteo de registros en estado rechazado   
       LET v_i_contador_reg = v_i_contador_reg + 1
    END FOREACH
       
    --se asigna el total de registros para el suamrio
    LET  v_r_pag_sum_fc.num_reg_detalle =  v_i_contador_reg

     LET l_s_cadena_sum_fc  = "09"            ,
                              v_i_contador_reg USING "&&&&&&&&&&",
                              (v_r_pag_sum_fc.tot_ap_fc * 100) USING "&&&&&&&&&&&"    
                                     
      --se escribe el sumario en el archivo
      CALL v_ch_arch_solTransf.writeLine([l_s_cadena_sum_fc])
END FUNCTION
