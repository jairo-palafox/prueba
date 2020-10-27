#########################################################################################
#Modulo       => CBD                                                                    #
#Programa     => CBDS06                                                                 #
#Objetivo     => Programa que genera el archivo de la BDNSVIV-plus con saldos especiales#
#Fecha inicio => 08/11/2017                                                             #
#########################################################################################

DATABASE safre_viv

GLOBALS "CBDS06.inc"

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                               -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                               -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)

PRIVATE DEFINE v_proceso_desc             CHAR(40)
PRIVATE DEFINE v_extension                CHAR(10)
PRIVATE DEFINE v_opera_desc               CHAR(40)
PRIVATE DEFINE v_layout                   SMALLINT
PRIVATE DEFINE v_usuario_proceso          CHAR(20)
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            CHAR(40)
PRIVATE DEFINE v_ruta_envio               CHAR(40)

PRIVATE DEFINE v_archivo_temporal         STRING
PRIVATE DEFINE v_archivo_imss             STRING
PRIVATE DEFINE v_archivo_infonavit        STRING

PRIVATE DEFINE v_fcorte                   DATE
PRIVATE DEFINE v_valor_fondo              DECIMAL(19,14)

MAIN
   DEFINE r_resultado_opera               INTEGER

   DEFINE v_resultado                     SMALLINT

   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET v_fcorte         = ARG_VAL(7)


   WHENEVER ERROR CONTINUE

	CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario_proceso  

   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'cbd'
   
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

   #Se manda a generar el archivo de la bdnsviv-plus
   CALL fn_genera_salida(ARCHIVO_INF) RETURNING v_resultado
   
   CALL fn_genera_salida(ARCHIVO_IMSS) RETURNING v_resultado

   IF v_resultado = 0 THEN
      # Finaliza la operacion de generacion del archivo
      CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
      RETURNING r_resultado_opera
      IF(r_resultado_opera <> 0)THEN
         # Actualiza a estado erróneo
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
         RETURNING r_resultado_opera
      END IF
      DISPLAY ""
      DISPLAY "*******************************************************************"
      DISPLAY " El proceso finalizó correctamente"
      DISPLAY " PROCESO            : ",v_proceso_desc
      DISPLAY " OPERACIÓN          : ",v_opera_desc
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY "*******************************************************************"
   ELSE
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
         RETURNING r_resultado_opera
   END IF
END MAIN

PRIVATE FUNCTION fn_genera_salida(p_tipo_archivo)
   DEFINE p_tipo_archivo            SMALLINT
   DEFINE v_folio_str               STRING
   DEFINE v_proceso_cod_str         STRING
   DEFINE v_proyecto                STRING

   LET v_folio_str = v_folio
   LET v_proceso_cod_str = p_proceso_cod
   --LET v_archivo_temporal = "notificacion_", v_folio_str CLIPPED
   LET v_archivo_temporal = "bdnsviv_plus_", v_proceso_cod_str CLIPPED
   LET v_proyecto = "plus_", v_proceso_cod_str CLIPPED

   #Genera device HPL para archivo de Notificaciones
   CALL fn_crea_project(v_proyecto)

   CALL fn_genera_device(v_ruta_envio,
                         v_archivo_temporal,
                         p_usuario_cod,
                         p_tipo_archivo)

   CALL fn_genera_query(v_proyecto,
                        v_ruta_envio,
                        v_archivo_temporal,
                        p_usuario_cod,
                        p_tipo_archivo)

   CALL fn_genera_format(v_proyecto,
                         v_ruta_envio,
                         v_archivo_temporal,
                         p_usuario_cod)
   
   CALL fn_genera_map(v_proyecto,
                      v_ruta_envio,
                      v_archivo_temporal,
                      p_usuario_cod)


   DISPLAY ""
   DISPLAY "Inicia la generacion de los detalles del archivo"
   DISPLAY ""

   PREPARE exe_pdq FROM "SET PDQPRIORITY HIGH"
   EXECUTE exe_pdq

   CALL fn_genera_job(v_ruta_envio,
                      v_archivo_temporal,
                      p_usuario_cod,
                      v_proyecto)

   CALL fn_ejecuta_hpl(v_ruta_envio,
                      v_archivo_temporal,
                      p_usuario_cod,
                      v_proyecto,
                      p_tipo_archivo)
   
   RETURN 0
END FUNCTION

FUNCTION fn_crea_project(p_proyecto)
   DEFINE p_proyecto   STRING
   DEFINE v_comando    STRING
   DEFINE r_ejecucion  SMALLINT

   LET v_comando = "onpladm describe project ", p_proyecto, " 1>/dev/null 2>/dev/null"
   #DISPLAY v_comando
   RUN v_comando RETURNING r_ejecucion

   IF r_ejecucion <> 0 THEN
      LET v_comando = "onpladm create project ",p_proyecto, " 1>/dev/null 2>/dev/null"
      DISPLAY "CREA PROYECTO HPL ",p_proyecto
      DISPLAY v_comando
      RUN v_comando
   END IF
END FUNCTION

FUNCTION fn_genera_device(p_ruta_envio,p_archivo_salida,p_usuario,p_tipo_archivo)
    DEFINE p_ruta_envio   STRING,
           p_archivo_salida LIKE cat_layout.archivo,
           p_usuario        LIKE seg_modulo.usuario,
           p_tipo_archivo   SMALLINT,
           v_str_archivo    STRING,
           v_canal          base.Channel,
           v_cadena         STRING,
           v_folio_str      STRING

   --LET v_fcorte = MDY(10,31,2015)

   SELECT
      precio_fondo
   INTO
      v_valor_fondo
   FROM glo_valor_fondo
   WHERE fondo = 11
   AND f_valuacion = v_fcorte
   
   LET v_folio_str = v_folio
   LET v_str_archivo = p_ruta_envio CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".device"
   LET v_canal = base.Channel.create()
   CALL v_canal.openFile(v_str_archivo,"w")

   IF p_tipo_archivo = ARCHIVO_IMSS THEN
      LET v_cadena = p_ruta_envio CLIPPED, '/SALDO_ESPECIAL_', v_fcorte USING "YYYYMMDD", '.saldos'
      LET v_archivo_imss = v_cadena CLIPPED
   ELSE
      LET v_cadena = p_ruta_envio CLIPPED, '/SALDO_ESPECIAL_INF_', v_fcorte USING "YYYYMMDD", '.saldoinfonavit'
      LET v_archivo_infonavit = v_cadena CLIPPED
   END IF
   
   CALL v_canal.writeLine('BEGIN OBJECT DEVICEARRAY '||p_archivo_salida CLIPPED||".device \n")
   CALL v_canal.writeLine('BEGIN SEQUENCE ')
   CALL v_canal.writeLine('TYPE           FILE ')
   CALL v_canal.writeLine('FILE           '||v_cadena)
   CALL v_canal.writeLine('TAPEBLOCKSIZE  0 ')
   CALL v_canal.writeLine('TAPEDEVICESIZE 0 ')
   CALL v_canal.writeLine('PIPECOMMAND      ')
   CALL v_canal.writeLine('END  SEQUENCE    \n')
   CALL v_canal.writeLine('END  OBJECT      ')

   CALL v_canal.close()

   CALL fn_crea_objeto(v_str_archivo, p_archivo_salida CLIPPED||".device", "device", "")
END FUNCTION

FUNCTION fn_genera_format(p_proyecto, p_ruta_envio, p_archivo_salida, p_usuario)
   DEFINE p_proyecto       STRING
   DEFINE v_ch_format      base.Channel
   DEFINE p_ruta_envio     STRING
   DEFINE p_usuario        STRING
   DEFINE p_archivo_salida STRING
   DEFINE v_archivo_format STRING

   LET v_archivo_format = p_ruta_envio CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".format"

   LET v_ch_format = base.Channel.create()
   CALL v_ch_format.openFile(v_archivo_format,"w")
   CALL v_ch_format.writeLine('BEGIN OBJECT COBOLFORMAT '||p_archivo_salida CLIPPED||".format \n")
	
   CALL v_ch_format.writeLine('PROJECT       ' || p_proyecto)
   CALL v_ch_format.writeLine('CHARACTERSET  ASCII ')
   CALL v_ch_format.writeLine('MACHINE       Intel ')
	CALL v_ch_format.writeLine('DRIVER       	COBOL \n')
   
   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	nss')
	CALL v_ch_format.writeLine('PICTURE      	A(11)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	rfc')
	CALL v_ch_format.writeLine('PICTURE      	A(13)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	curp')
	CALL v_ch_format.writeLine('PICTURE      	A(18)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	ape_paterno')
	CALL v_ch_format.writeLine('PICTURE      	A(40)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	ape_materno')
	CALL v_ch_format.writeLine('PICTURE      	A(40)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	nombre')
	CALL v_ch_format.writeLine('PICTURE      	A(40)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	nombre_imss')
	CALL v_ch_format.writeLine('PICTURE      	A(50)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	tipo_trabajador')
	CALL v_ch_format.writeLine('PICTURE      	A(1)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	cve_afore')
	CALL v_ch_format.writeLine('PICTURE      	A(3)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	aport_viv97')
	CALL v_ch_format.writeLine('PICTURE      	A(13)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	num_aportaciones97')
	CALL v_ch_format.writeLine('PICTURE      	A(5)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	f_ssv97')
	CALL v_ch_format.writeLine('PICTURE      	A(6)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	aport_viv92')
	CALL v_ch_format.writeLine('PICTURE      	A(11)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	num_aportaciones92')
	CALL v_ch_format.writeLine('PICTURE      	A(5)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	f_ssv92')
	CALL v_ch_format.writeLine('PICTURE      	A(6)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	fondo_viv72')
	CALL v_ch_format.writeLine('PICTURE      	A(7)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	f_fondo_anterior')
	CALL v_ch_format.writeLine('PICTURE      	A(6)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	aivs_viv97')
	CALL v_ch_format.writeLine('PICTURE      	A(15)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	aivs_viv92')
	CALL v_ch_format.writeLine('PICTURE      	A(15)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	valor_paivs')
	CALL v_ch_format.writeLine('PICTURE      	A(12)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	f_paivs')
	CALL v_ch_format.writeLine('PICTURE      	A(6)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	marca_modif_nombre')
	CALL v_ch_format.writeLine('PICTURE      	A(1)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	estatus_unificacion')
	CALL v_ch_format.writeLine('PICTURE      	A(1)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	marca_unificacion')
	CALL v_ch_format.writeLine('PICTURE      	A(1)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	estatus_retiro')
	CALL v_ch_format.writeLine('PICTURE      	A(1)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	marca_retiro')
	CALL v_ch_format.writeLine('PICTURE      	A(1)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	estatus_traspaso_afore')
	CALL v_ch_format.writeLine('PICTURE      	A(1)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	estatus_devolucion_pagos')
	CALL v_ch_format.writeLine('PICTURE      	A(1)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	estatus_trans_acreditados')
	CALL v_ch_format.writeLine('PICTURE      	A(1)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	marca_trans_43bis')
	CALL v_ch_format.writeLine('PICTURE      	A(1)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	estatus_trabajador_credito')
	CALL v_ch_format.writeLine('PICTURE      	A(1)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	solicitud_credito')
	CALL v_ch_format.writeLine('PICTURE      	A(1)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	f_credito')
	CALL v_ch_format.writeLine('PICTURE      	A(6)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	f_aivs_retiro')
	CALL v_ch_format.writeLine('PICTURE      	A(6)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	f_ultimo_movimiento')
	CALL v_ch_format.writeLine('PICTURE      	A(6)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	diferencia_nombre')
	CALL v_ch_format.writeLine('PICTURE      	A(1)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	estatus_traspaso_sar92')
	CALL v_ch_format.writeLine('PICTURE      	A(1)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	estatus_acreditados_43pr')
	CALL v_ch_format.writeLine('PICTURE      	A(1)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	estatus_acreditados_43gt')
	CALL v_ch_format.writeLine('PICTURE      	A(1)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	estatus_acreditados_43su')
	CALL v_ch_format.writeLine('PICTURE      	A(1)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	ind_inhabilita')
	CALL v_ch_format.writeLine('PICTURE      	A(1)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   dummycr')
   CALL v_ch_format.writeLine('PICTURE      	X')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')
   
   CALL v_ch_format.writeLine('END OBJECT')

   CALL v_ch_format.close()

   CALL fn_crea_objeto(v_archivo_format, p_archivo_salida CLIPPED||".format", "format", p_proyecto)

END FUNCTION

FUNCTION fn_genera_map(p_proyecto, p_ruta_envio, p_archivo_salida, p_usuario)
   DEFINE p_proyecto       STRING
   DEFINE p_ruta_envio     STRING
   DEFINE p_usuario        STRING
   DEFINE p_archivo_salida STRING
   DEFINE v_archivo_map    STRING
   DEFINE v_comando        STRING
   DEFINE v_ch_map         base.Channel

   LET v_archivo_map = p_ruta_envio CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".map"

   LET v_ch_map = base.Channel.create()
   CALL v_ch_map.openFile(v_archivo_map,"w")

   CALL v_ch_map.writeLine('BEGIN OBJECT UNLOADMAP '||p_archivo_salida CLIPPED||".map")

   CALL v_ch_map.writeLine('PROJECT      ' || p_proyecto)
   CALL v_ch_map.writeLine('FORMAT       ' || p_archivo_salida CLIPPED||'.format')
   CALL v_ch_map.writeLine('DATABASE     safre_viv')
   CALL v_ch_map.writeLine('QUERY        '|| p_archivo_salida CLIPPED||'.query')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      nss')
   CALL v_ch_map.writeLine('FIELDNAME       nss')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      rfc')
   CALL v_ch_map.writeLine('FIELDNAME       rfc')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      curp')
   CALL v_ch_map.writeLine('FIELDNAME       curp')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      ape_paterno')
   CALL v_ch_map.writeLine('FIELDNAME       ape_paterno')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      ape_materno')
   CALL v_ch_map.writeLine('FIELDNAME       ape_materno')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      nombre')
   CALL v_ch_map.writeLine('FIELDNAME       nombre')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      nombre_imss')
   CALL v_ch_map.writeLine('FIELDNAME       nombre_imss')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      tipo_trabajador')
   CALL v_ch_map.writeLine('FIELDNAME       tipo_trabajador')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      cve_afore')
   CALL v_ch_map.writeLine('FIELDNAME       cve_afore')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      aport_viv97')
   CALL v_ch_map.writeLine('FIELDNAME       aport_viv97')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      num_aportaciones97')
   CALL v_ch_map.writeLine('FIELDNAME       num_aportaciones97')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      f_ssv97')
   CALL v_ch_map.writeLine('FIELDNAME       f_ssv97')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      aport_viv92')
   CALL v_ch_map.writeLine('FIELDNAME       aport_viv92')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      num_aportaciones92')
   CALL v_ch_map.writeLine('FIELDNAME       num_aportaciones92')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      f_ssv92')
   CALL v_ch_map.writeLine('FIELDNAME       f_ssv92')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      fondo_viv72')
   CALL v_ch_map.writeLine('FIELDNAME       fondo_viv72')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      f_fondo_anterior')
   CALL v_ch_map.writeLine('FIELDNAME       f_fondo_anterior')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      aivs_viv97')
   CALL v_ch_map.writeLine('FIELDNAME       aivs_viv97')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      aivs_viv92')
   CALL v_ch_map.writeLine('FIELDNAME       aivs_viv92')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      valor_paivs')
   CALL v_ch_map.writeLine('FIELDNAME       valor_paivs')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      f_paivs')
   CALL v_ch_map.writeLine('FIELDNAME       f_paivs')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      marca_modif_nombre')
   CALL v_ch_map.writeLine('FIELDNAME       marca_modif_nombre')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      estatus_unificacion')
   CALL v_ch_map.writeLine('FIELDNAME       estatus_unificacion')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      marca_unificacion')
   CALL v_ch_map.writeLine('FIELDNAME       marca_unificacion')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      estatus_retiro')
   CALL v_ch_map.writeLine('FIELDNAME       estatus_retiro')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      marca_retiro')
   CALL v_ch_map.writeLine('FIELDNAME       marca_retiro')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      estatus_traspaso_afore')
   CALL v_ch_map.writeLine('FIELDNAME       estatus_traspaso_afore')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      estatus_devolucion_pagos')
   CALL v_ch_map.writeLine('FIELDNAME       estatus_devolucion_pagos')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      estatus_trans_acreditados')
   CALL v_ch_map.writeLine('FIELDNAME       estatus_trans_acreditados')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      marca_trans_43bis')
   CALL v_ch_map.writeLine('FIELDNAME       marca_trans_43bis')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      estatus_trabajador_credito')
   CALL v_ch_map.writeLine('FIELDNAME       estatus_trabajador_credito')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      solicitud_credito')
   CALL v_ch_map.writeLine('FIELDNAME       solicitud_credito')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      f_credito')
   CALL v_ch_map.writeLine('FIELDNAME       f_credito')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      f_aivs_retiro')
   CALL v_ch_map.writeLine('FIELDNAME       f_aivs_retiro')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      f_ultimo_movimiento')
   CALL v_ch_map.writeLine('FIELDNAME       f_ultimo_movimiento')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      diferencia_nombre')
   CALL v_ch_map.writeLine('FIELDNAME       diferencia_nombre')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      estatus_traspaso_sar92')
   CALL v_ch_map.writeLine('FIELDNAME       estatus_traspaso_sar92')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      estatus_acreditados_43pr')
   CALL v_ch_map.writeLine('FIELDNAME       estatus_acreditados_43pr')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      estatus_acreditados_43gt')
   CALL v_ch_map.writeLine('FIELDNAME       estatus_acreditados_43gt')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      estatus_acreditados_43su')
   CALL v_ch_map.writeLine('FIELDNAME       estatus_acreditados_43su')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      ind_inhabilita')
   CALL v_ch_map.writeLine('FIELDNAME       ind_inhabilita')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')
   
   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      dummycr')
   CALL v_ch_map.writeLine('FIELDNAME       dummycr')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING     BINARY')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('END OBJECT')

   LET v_comando = "onpladm delete map ",p_archivo_salida CLIPPED||".map -fu -p " || p_proyecto CLIPPED || " ",
                   " 1>/dev/null 2>/dev/null"
   RUN v_comando

   CALL fn_crea_objeto(v_archivo_map, p_archivo_salida CLIPPED||".map", "map", p_proyecto)

END FUNCTION

FUNCTION fn_genera_query(p_proyecto, p_ruta_envio, p_archivo_salida, p_usuario, p_tipo_archivo)
   DEFINE p_proyecto       STRING
   DEFINE p_ruta_envio     STRING
   DEFINE p_usuario        STRING
   DEFINE p_archivo_salida STRING
   DEFINE p_tipo_archivo   SMALLINT
   DEFINE v_archivo_query  STRING
   DEFINE v_query          STRING
   
   DEFINE v_ch_query       base.Channel

   LET v_archivo_query = p_ruta_envio CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".query"

   LET v_ch_query = base.Channel.create()
   CALL v_ch_query.openFile(v_archivo_query,"w")

   CALL v_ch_query.writeLine('BEGIN OBJECT QUERY   ' || p_archivo_salida CLIPPED || ".query")

   CALL v_ch_query.writeLine('PROJECT ' || p_proyecto)
   CALL v_ch_query.writeLine('DATABASE safre_viv')

   LET v_query = "\"SELECT ",
                     "nss, ",
                     "rfc, ",
                     "curp, ",
                     "ape_paterno, ",
                     "ape_materno, ",
                     "nombre, ",
                     "nombre_imss, ",
                     "' ' tipo_trabajador, ",
                     "'000' cve_afore, ",
                     "aport_viv97, ",
                     "'00000' num_aportaciones97, ",
                     "f_ssv97, ",
                     "aport_viv92, ",               
                     "'00000' num_aportaciones92, ",        
                     "f_ssv92, ",                   
                     "'0000000' fondo_viv72, ",
                     "'      ' f_fondo_anterior, ",
                     "aivs_viv97, ",
                     "aivs_viv92, ",                
                     "'",(v_valor_fondo * 1000000) USING "&&&&&&&&&&&&","' valor_paivs,",
                     "f_paivs, ",                   
                     "'0' marca_modif_nombre, ",
                     "'0' estatus_unificacion, ",
                     "'0' marca_unificacion, ",
                     "'0' estatus_retiro, ",
                     "'0' marca_retiro, ",
                     "'0' estatus_traspaso_afore, ",
                     "'0' estatus_devolucion_pagos, ",
                     "'0' estatus_trans_acreditados, ",
                     "'0' marca_trans_43bis, ",
                     "' ' estatus_trabajador_credito, ",
                     "' ' solicitud_credito, ",
                     "f_credito, ",                 
                     "'      ' f_aivs_retiro, ",
                     "f_ultimo_movimiento, ",       
                     "'0' diferencia_nombre, ",
                     "'0' estatus_traspaso_sar92, ",
                     "'0' estatus_acreditados_43pr, ",
                     "'0' estatus_acreditados_43gt, ",
                     "'0' estatus_acreditados_43su, ",
                     "ind_inhabilita, ",
                     "fn_salto_linea() dummycr "
                     
   IF p_tipo_archivo = ARCHIVO_IMSS THEN
      LET v_query = v_query, "FROM cbd_saldo_especial_imss\""
   ELSE
      LET v_query = v_query, "FROM cbd_saldo_especial_inf\""
   END IF
   
   CALL v_ch_query.writeLine('SELECTSTATEMENT '||v_query)
   CALL v_ch_query.writeLine('END OBJECT')

   CALL v_ch_query.close()

   CALL fn_crea_objeto(v_archivo_query, p_archivo_salida CLIPPED||".query", "query", p_proyecto)
END FUNCTION

FUNCTION fn_genera_job(p_ruta_envio,p_archivo_salida,p_usuario,p_proyecto)

    DEFINE p_ruta_envio   STRING,
           p_archivo_salida LIKE cat_layout.archivo,
           p_usuario        LIKE seg_modulo.usuario,
           p_proyecto       STRING,
           v_str_archivo    STRING,
           v_canal          base.Channel

   LET v_str_archivo = p_ruta_envio CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".job"
   LET v_canal = base.Channel.create()
   CALL v_canal.openFile(v_str_archivo,"w")

   CALL v_canal.writeLine('BEGIN OBJECT UNLOADJOB '||p_archivo_salida CLIPPED||".job")
   CALL v_canal.writeLine('PROJECT           '||p_proyecto CLIPPED)
   CALL v_canal.writeLine('DEVICE            '||p_archivo_salida CLIPPED||'.device ')
   CALL v_canal.writeLine('MAP               '||p_archivo_salida CLIPPED||'.map ')
   CALL v_canal.writeLine('FILTER            ')
   CALL v_canal.writeLine('SERVER            vivop_tcp') 
   CALL v_canal.writeLine('DATABASE          safre_viv ')
   CALL v_canal.writeLine('REJECTFILE        '||
                           p_ruta_envio CLIPPED||'/'||p_usuario CLIPPED||'.'||
                           p_archivo_salida CLIPPED||'.rjt ')
   CALL v_canal.writeLine('LOGFILE           '||
                           p_ruta_envio CLIPPED||'/'||p_usuario CLIPPED||'.'||
                           p_archivo_salida CLIPPED||'.log ')
   CALL v_canal.writeLine('ISOLATIONLEVEL    DR ')
   CALL v_canal.writeLine('MAXERRORS         100 ')
   CALL v_canal.writeLine('END  OBJECT      \n')

   CALL v_canal.close()

   CALL fn_crea_objeto(v_str_archivo, p_archivo_salida CLIPPED||".job", "job", p_proyecto )
END FUNCTION

FUNCTION fn_crea_objeto(p_str_archivo, p_objeto, p_tipo, p_proyecto)
   DEFINE p_tipo            STRING
   DEFINE p_objeto          STRING
   DEFINE p_str_archivo     STRING
   DEFINE p_proyecto        STRING
   DEFINE v_comando         STRING
   DEFINE r_ejecucion       SMALLINT

   #Se ejecuta primero el create y despues el modify, para no verificar si ya
   #existe el objeto

   CASE p_tipo
      WHEN "device"
         LET v_comando = "onpladm describe device ", p_objeto, " 1>/dev/null 2>/dev/null"
         
      WHEN "job" 
         LET v_comando = "onpladm describe job ", p_objeto,
                         " -fu -p ", p_proyecto , " 1>/dev/null 2>/dev/null"
      WHEN "format"
         LET v_comando = "onpladm describe format ", p_objeto,
                         " -p ",p_proyecto , " 1>/dev/null 2>/dev/null"
     WHEN "map"
         DISPLAY ""
         LET v_comando = "onpladm describe map ", p_objeto,
                         " -fu -p ",p_proyecto , " 1>/dev/null 2>/dev/null"
     WHEN "query"
         DISPLAY ""
         LET v_comando = "onpladm describe query ", p_objeto,
                         " -p ",p_proyecto , " 1>/dev/null 2>/dev/null"

   END CASE

   #DISPLAY v_comando
   RUN v_comando RETURNING r_ejecucion

   IF r_ejecucion <> 0 THEN
      LET v_comando = "onpladm create object -F ",
                      p_str_archivo #, " 1>/dev/null 2>/dev/null"

		DISPLAY ""
		DISPLAY "CREA OBJETO HPL ", p_str_archivo
      DISPLAY v_comando
		DISPLAY ""
   ELSE
      LET v_comando = "onpladm modify object -F ",
                      p_str_archivo #, " 1>/dev/null 2>/dev/null"
		DISPLAY ""
		DISPLAY "MODIFICA OBJETO HPL ", p_str_archivo
      DISPLAY v_comando
		DISPLAY ""
   END IF

   RUN v_comando

END FUNCTION

#Objetivo: Recupera la informacion necesaria del proceso para cargar el archivo
FUNCTION fn_recupera_inf_proceso(p_proceso,p_operacion)
DEFINE p_proceso       LIKE cat_proceso.proceso_cod,
       p_operacion     LIKE cat_operacion.opera_cod,
       v_layout        LIKE cat_operacion.layout_cod,
       v_extension     LIKE cat_operacion.extension,
       v_modulo        LIKE cat_proceso.modulo_cod,
       v_proceso_desc  LIKE cat_proceso.proceso_desc,
       v_opera_desc    LIKE cat_operacion.opera_desc,
       v_ruta_rescate  LIKE seg_modulo.ruta_rescate,
       v_usuario       LIKE seg_modulo.usuario,
       v_ruta_listados LIKE seg_modulo.ruta_listados
       
   #Consulta que recupera el módulo y descripción del proceso
   SELECT modulo_cod, proceso_desc
     INTO v_modulo, v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso

   #Consulta que recupera los datos necesarios para conformar las tablas temporales del proceso
   SELECT extension, opera_desc, layout_cod
     INTO v_extension, v_opera_desc, v_layout
     FROM cat_operacion
    WHERE proceso_cod = p_proceso
      AND opera_cod   = p_operacion
      
   #Consulta que recupera el usuario y la ruta donde estan ubicados los archivos del proceso
   SELECT ruta_rescate, usuario
     INTO v_ruta_rescate, v_usuario
     FROM seg_modulo
    WHERE modulo_cod = v_modulo

   #Ruta donde se coloca el archivo de monitoreo
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

   RETURN v_proceso_desc, v_extension, v_opera_desc,
          v_layout, v_ruta_rescate, v_ruta_listados,  v_usuario
END FUNCTION

FUNCTION fn_ejecuta_hpl(p_ruta_envio, p_archivo_salida,p_usuario,p_modulo_cod,p_tipo_archivo)

    DEFINE p_ruta_envio     STRING,
           p_modulo_cod     STRING,
           p_archivo_salida LIKE cat_layout.archivo,
           p_usuario        LIKE seg_usuario.usuario_cod,
           p_tipo_archivo   SMALLINT,
           v_comando        STRING,
           v_fecha          DATE
   
           
   LET v_comando = "onpladm run job ", p_archivo_salida CLIPPED, ".job -fu -p ",
                    p_modulo_cod, " 1>/dev/null 2>/dev/null"
   
   DISPLAY " INICIO ETAPA       : EJECUTA JOB CARGA HPL"
   LET v_fecha = TODAY
   DISPLAY " FECHA              : ",v_fecha USING "DD-MM-YYYY"
   DISPLAY " HORA               : ",TIME(CURRENT),"\n"
   DISPLAY ""
   #registra etapa en archivo de monitoreo

   DISPLAY v_comando
   RUN v_comando
   
   DISPLAY " FIN ETAPA          : EJECUTA JOB CARGA HPL"
   LET v_fecha = TODAY
   DISPLAY " FECHA              : ",v_fecha USING "DD-MM-YYYY"
   DISPLAY " HORA               : ",TIME(CURRENT),"\n"
   DISPLAY ""

	DISPLAY ""
	DISPLAY "Eliminando los archivos de control de HPL..."
	DISPLAY ""
	LET v_comando = "rm ", p_ruta_envio CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,"*"
	--RUN v_comando
END FUNCTION
