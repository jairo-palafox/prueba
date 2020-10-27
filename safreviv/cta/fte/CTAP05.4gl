#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => CTA                                                     #
#Programa          => CTAP05                                                  #
#Objetivo          => PROGRAMA PARA GENERAR EL ARCHIVO DE SALDOS PARA         #
#                     EL AREA DE PRECALIFICACION                              #
#Fecha Inicio      => 17-ABRIL-2013                                           #
###############################################################################
DATABASE safre_viv

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                            -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                            -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)
PRIVATE DEFINE v_fcorte                   DATE

PRIVATE DEFINE v_proceso_desc             LIKE cat_proceso.proceso_desc
PRIVATE DEFINE v_extension                LIKE cat_operacion.extension
PRIVATE DEFINE v_opera_desc               LIKE cat_operacion.opera_desc
PRIVATE DEFINE v_layout                   LIKE cat_operacion.layout_cod
PRIVATE DEFINE v_usuario_proceso          LIKE seg_modulo.usuario
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            LIKE seg_modulo.ruta_listados
PRIVATE DEFINE v_ruta_envio               LIKE seg_modulo.ruta_envio

PRIVATE DEFINE v_total_pesos92				DECIMAL(22,2)
PRIVATE DEFINE v_total_pesos97				DECIMAL(22,2)
PRIVATE DEFINE v_total_registros				DECIMAL(10,0)

PRIVATE DEFINE v_archivo_preca            STRING
PRIVATE DEFINE v_ruta_listado             STRING

MAIN
   DEFINE r_resultado_opera               INTEGER
   DEFINE v_resultado_gen                 INTEGER
   DEFINE v_fecha_actual                  DATE

   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)


   WHENEVER ERROR CONTINUE
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario_proceso 

                                         
   #Encabezado para el archivo de monitoreo
   LET v_fecha_actual = TODAY
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"
      
   #Primero se valida si se puede ejecutar la generacion de saldos
   --Se establece la fecha de corte con la fecha del ultimo saldo generado
   --LET v_fcorte = TODAY - 1;
	SELECT MAX(f_saldo) 
	INTO v_fcorte
	FROM safre_sdo@vivws_tcp:glo_ctr_saldo
	WHERE tpo_saldo = 3								#Saldo de precalificacion
	AND estado_genera = 2							#Proceso exitoso

	DISPLAY ""
	DISPLAY "El ultimo saldo generado para precalificacion tiene fecha de corte ", v_fcorte USING 'dd-mm-yyyy'
	DISPLAY ""

	-- se solicita el numero de folio asociado a la operacion
	CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
	RETURNING v_folio

	#Se actualiza el folio del proceso               
	UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid

	UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid

	DISPLAY "Inicia la generacion del archivo de Saldos para Precalificacion"
	#Se manda a generar el archivo de movimientos adelantados
	CALL fn_genera_archivo() RETURNING v_resultado_gen
	IF v_resultado_gen = 0 THEN
		# Finaliza la operacion de generacion del archivo
		CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
		RETURNING r_resultado_opera
		IF(r_resultado_opera <> 0)THEN         
			# Actualiza a estado erróneo
			CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
			RETURNING r_resultado_opera
		ELSE
			#CALL fn_cifras_control()

         CALL fn_correo_proceso(p_pid,
                             p_proceso_cod,
                             p_opera_cod,
                             '',
                             v_proceso_desc,
                             'ID Proceso   : '||p_pid||
                             'Proceso      : '||p_proceso_cod||
                             'Operacion    : '||p_opera_cod||
                             'Fecha Inicio : '||v_fecha_actual||
                             'Fecha Fin    : '||DATE
                             )

                             
		END IF 
	ELSE
		CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
			RETURNING r_resultado_opera
	END IF
END MAIN

FUNCTION fn_genera_archivo()
   
   DISPLAY "Se inicia la creación de archivos HPL"

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'cta'

   LET v_archivo_preca      = "saldo_safre_tmp"

   --LET v_fcorte = TODAY - 1

   #Genera device HPL para archivo de Saldos IMSS

   CALL fn_crea_project("cta")

   CALL fn_genera_device(v_ruta_envio,
                         v_archivo_preca,
                         p_usuario_cod)

   CALL fn_genera_query(v_ruta_envio,
                      v_archivo_preca,
                      p_usuario_cod)

   CALL fn_genera_format(v_ruta_envio,
                         v_archivo_preca,
                         p_usuario_cod)
   
   CALL fn_genera_map(v_ruta_envio,
                      v_archivo_preca,
                      p_usuario_cod)


   DISPLAY ""
   DISPLAY "Inicia la generacion de los detalles del archivo"
   DISPLAY ""

   PREPARE exe_pdq FROM "SET PDQPRIORITY HIGH"
   EXECUTE exe_pdq

   CALL fn_genera_job(v_ruta_envio,
                      v_archivo_preca,
                      p_usuario_cod,
                      'cta')

   CALL fn_ejecuta_hpl(v_ruta_envio,
                      v_archivo_preca,
                      p_usuario_cod,
                      'cta')

   
   RETURN 0
END FUNCTION

FUNCTION fn_crea_project(p_proyecto)
   DEFINE p_proyecto   STRING
   DEFINE v_comando    STRING
   DEFINE r_ejecucion  SMALLINT

   LET v_comando = "onpladm describe project ", p_proyecto, " 1>/dev/null 2>/dev/null"
   RUN v_comando RETURNING r_ejecucion

   IF r_ejecucion <> 0 THEN
      LET v_comando = "onpladm create project ",p_proyecto, " 1>/dev/null 2>/dev/null"
      DISPLAY "CREA PROYECTO HPL ",p_proyecto
      RUN v_comando
   END IF
   
END FUNCTION

FUNCTION fn_genera_device(p_ruta_rescate,p_archivo_salida,p_usuario)
    DEFINE p_ruta_rescate   STRING,--LIKE seg_modulo.ruta_rescate,
           p_archivo_salida LIKE cat_layout.archivo,
           p_usuario        LIKE seg_modulo.usuario,
           v_str_archivo    STRING,
           v_canal          base.Channel,
           v_cadena         STRING

   LET v_str_archivo = p_ruta_rescate CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".device"
   LET v_canal = base.Channel.create()
   CALL v_canal.openFile(v_str_archivo,"w")

   LET v_cadena = p_ruta_rescate CLIPPED, '/SALDO_SAFRE_', v_fcorte USING 'ddmmyyyy'
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

FUNCTION fn_genera_format(p_ruta_rescate, p_archivo_salida, p_usuario)

   DEFINE v_ch_format      base.Channel
   DEFINE p_ruta_rescate   STRING
   DEFINE p_usuario        STRING
   DEFINE p_archivo_salida STRING
   DEFINE v_archivo_format STRING

   LET v_archivo_format = p_ruta_rescate CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".format"

   LET v_ch_format = base.Channel.create()
   CALL v_ch_format.openFile(v_archivo_format,"w")
   CALL v_ch_format.writeLine('BEGIN OBJECT COBOLFORMAT '||p_archivo_salida CLIPPED||".format \n")
	
   CALL v_ch_format.writeLine('PROJECT       cta')
   CALL v_ch_format.writeLine('CHARACTERSET  ASCII ')
   CALL v_ch_format.writeLine('MACHINE       Intel ')
	CALL v_ch_format.writeLine('DRIVER       	COBOL \n')
   
   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	nss')
	CALL v_ch_format.writeLine('PICTURE      	A(11)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	f_proceso')
   CALL v_ch_format.writeLine('PICTURE      	99999999')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	rfc')
   CALL v_ch_format.writeLine('PICTURE      	A(13)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   curp')
   CALL v_ch_format.writeLine('PICTURE      	A(18)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   ap_paterno')
   CALL v_ch_format.writeLine('PICTURE      	A(50)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   ap_materno')
   CALL v_ch_format.writeLine('PICTURE      	A(50)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   nombre')
   CALL v_ch_format.writeLine('PICTURE      	A(50)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   subcuenta')
   CALL v_ch_format.writeLine('PICTURE      	 99')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   importe')
   CALL v_ch_format.writeLine('PICTURE      	 9(13)V99')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   f_valor')
   CALL v_ch_format.writeLine('PICTURE      	99999999')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   afore')
   CALL v_ch_format.writeLine('PICTURE      	999')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   dummycr')
   CALL v_ch_format.writeLine('PICTURE      	X')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')
   
   CALL v_ch_format.writeLine('END OBJECT')

   CALL v_ch_format.close()

   CALL fn_crea_objeto(v_archivo_format, p_archivo_salida CLIPPED||".format", "format", "cta")

END FUNCTION

FUNCTION fn_genera_map(p_ruta_rescate, p_archivo_salida, p_usuario)
   DEFINE p_ruta_rescate   STRING
   DEFINE p_usuario        STRING
   DEFINE p_archivo_salida STRING
   DEFINE v_archivo_map    STRING
   DEFINE v_comando        STRING
   DEFINE v_ch_map         base.Channel

   LET v_archivo_map = p_ruta_rescate CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".map"

   LET v_ch_map = base.Channel.create()
   CALL v_ch_map.openFile(v_archivo_map,"w")

   CALL v_ch_map.writeLine('BEGIN OBJECT UNLOADMAP '||p_archivo_salida CLIPPED||".map")

   CALL v_ch_map.writeLine('PROJECT      cta')
   CALL v_ch_map.writeLine('FORMAT       '||p_archivo_salida CLIPPED||'.format')
   CALL v_ch_map.writeLine('DATABASE     safre_viv')
   CALL v_ch_map.writeLine('QUERY        '||p_archivo_salida CLIPPED||'.query')

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
   CALL v_ch_map.writeLine('COLUMNNAME      f_proceso')
   CALL v_ch_map.writeLine('FIELDNAME       f_proceso')
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
   CALL v_ch_map.writeLine('FILLCHARACTER		" "')
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
   CALL v_ch_map.writeLine('COLUMNNAME      ap_paterno')
   CALL v_ch_map.writeLine('FIELDNAME       ap_paterno')
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
   CALL v_ch_map.writeLine('COLUMNNAME      ap_materno')
   CALL v_ch_map.writeLine('FIELDNAME       ap_materno')
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
   CALL v_ch_map.writeLine('FILLCHARACTER')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      subcuenta')
   CALL v_ch_map.writeLine('FIELDNAME       subcuenta')
   CALL v_ch_map.writeLine('JUSTIFICATION   ')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES   0')
   CALL v_ch_map.writeLine('COLUMNOFFSET    0')
   CALL v_ch_map.writeLine('FIELDOFFSET     0')
   CALL v_ch_map.writeLine('FIELDMINIMUM     ')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      importe')
   CALL v_ch_map.writeLine('FIELDNAME       importe')
   CALL v_ch_map.writeLine('JUSTIFICATION   RIGHT')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE')
   CALL v_ch_map.writeLine('TRANSFERBYTES   0')
   CALL v_ch_map.writeLine('COLUMNOFFSET    0')
   CALL v_ch_map.writeLine('FIELDOFFSET     0')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER')
   CALL v_ch_map.writeLine('PICTURE')
   CALL v_ch_map.writeLine('FUNCTION')
   CALL v_ch_map.writeLine('STORAGECODING')
   CALL v_ch_map.writeLine('BLOBCOLUMN')
   CALL v_ch_map.writeLine('END SEQUENCE \n')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      f_valor')
   CALL v_ch_map.writeLine('FIELDNAME       f_valor')
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
   CALL v_ch_map.writeLine('COLUMNNAME      afore')
   CALL v_ch_map.writeLine('FIELDNAME       afore')
   CALL v_ch_map.writeLine('JUSTIFICATION')
   CALL v_ch_map.writeLine('CASECONVERT')
   CALL v_ch_map.writeLine('DEFAULTVALUE    ')
   CALL v_ch_map.writeLine('TRANSFERBYTES')
   CALL v_ch_map.writeLine('COLUMNOFFSET')
   CALL v_ch_map.writeLine('FIELDOFFSET')
   CALL v_ch_map.writeLine('FIELDMINIMUM')
   CALL v_ch_map.writeLine('FIELDMAXIMUM')
   CALL v_ch_map.writeLine('FILLCHARACTER')
   CALL v_ch_map.writeLine('PICTURE    ')
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

   LET v_comando = "onpladm delete map ",p_archivo_salida CLIPPED||".map -fu -p cta ",
                   " 1>/dev/null 2>/dev/null"
   RUN v_comando

   CALL fn_crea_objeto(v_archivo_map, p_archivo_salida CLIPPED||".map", "map", "cta" )

END FUNCTION

FUNCTION fn_genera_query(p_ruta_rescate, p_archivo_salida, p_usuario)
   DEFINE p_ruta_rescate   STRING
   DEFINE p_usuario        STRING
   DEFINE p_archivo_salida STRING
   DEFINE v_archivo_query  STRING
   DEFINE v_query          STRING
   
   DEFINE v_ch_query       base.Channel

   LET v_archivo_query = p_ruta_rescate CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".query"

   LET v_ch_query = base.Channel.create()
   CALL v_ch_query.openFile(v_archivo_query,"w")

   CALL v_ch_query.writeLine('BEGIN OBJECT QUERY   '||p_archivo_salida CLIPPED||".query")

   CALL v_ch_query.writeLine('PROJECT cta')
   CALL v_ch_query.writeLine('DATABASE safre_viv')

   LET v_query = '"SELECT ',
                     "nss, ",
		     "'",TODAY USING "YYYYMMDD","' f_proceso,",
                     "rfc, ",
                     "curp, ",
                     "ap_paterno, ",
                     "ap_materno, ",
                     "nombre, ",
                     "subcuenta, ",
                     "importe, ",
		     "'",v_fcorte USING "YYYYMMDD","' f_valor,",
                     "afore, ",
                     "fn_salto_linea() dummycr ",
                  'FROM cta_saldo_preca "'

   CALL v_ch_query.writeLine('SELECTSTATEMENT '||v_query)
   CALL v_ch_query.writeLine('END OBJECT')

   CALL v_ch_query.close()

   CALL fn_crea_objeto(v_archivo_query, p_archivo_salida CLIPPED||".query", "query", "cta")
END FUNCTION

FUNCTION fn_genera_job(p_ruta_rescate,p_archivo_salida,p_usuario,p_modulo_cod)

    DEFINE p_ruta_rescate   STRING,--LIKE seg_modulo.ruta_rescate,
           p_archivo_salida LIKE cat_layout.archivo,
           p_usuario        LIKE seg_modulo.usuario,
           p_modulo_cod     CHAR(3),
           v_str_archivo    STRING,
           v_canal          base.Channel

   LET v_str_archivo = p_ruta_rescate CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".job"
   LET v_canal = base.Channel.create()
   CALL v_canal.openFile(v_str_archivo,"w")

   CALL v_canal.writeLine('BEGIN OBJECT UNLOADJOB '||p_archivo_salida CLIPPED||".job")
   CALL v_canal.writeLine('PROJECT           '||p_modulo_cod CLIPPED)
   CALL v_canal.writeLine('DEVICE            '||p_archivo_salida CLIPPED||'.device ')
   CALL v_canal.writeLine('MAP               '||p_archivo_salida CLIPPED||'.map ')
   CALL v_canal.writeLine('FILTER            ')
   CALL v_canal.writeLine('SERVER            vivop_tcp') 
   CALL v_canal.writeLine('DATABASE          safre_viv ')
   CALL v_canal.writeLine('REJECTFILE        '||
                           p_ruta_rescate CLIPPED||'/'||p_usuario CLIPPED||'.'||
                           p_archivo_salida CLIPPED||'.rjt ')
   CALL v_canal.writeLine('LOGFILE           '||
                           p_ruta_rescate CLIPPED||'/'||p_usuario CLIPPED||'.'||
                           p_archivo_salida CLIPPED||'.log ')
   CALL v_canal.writeLine('ISOLATIONLEVEL    DR ')
   CALL v_canal.writeLine('MAXERRORS         100 ')
   CALL v_canal.writeLine('END  OBJECT      \n')

   CALL v_canal.close()


   CALL fn_crea_objeto(v_str_archivo, p_archivo_salida CLIPPED||".job", "job", p_modulo_cod )
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
		DISPLAY ""
   ELSE
      LET v_comando = "onpladm modify object -F ",
                      p_str_archivo #, " 1>/dev/null 2>/dev/null"
		DISPLAY ""
		DISPLAY "MODIFICA OBJETO HPL ", p_str_archivo
		DISPLAY ""
   END IF

	#DISPLAY v_comando
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

FUNCTION fn_ejecuta_hpl(p_ruta_rescate, p_archivo_salida,p_usuario,p_modulo_cod)

    DEFINE p_ruta_rescate   STRING,
           p_modulo_cod     CHAR(3),
           p_archivo_salida LIKE cat_layout.archivo,
           v_subcuenta      SMALLINT,
           v_importe	    DECIMAL(22,2),
           p_usuario        LIKE seg_usuario.usuario_cod,
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

   RUN v_comando
   DISPLAY v_comando
   DISPLAY " FIN ETAPA          : EJECUTA JOB CARGA HPL"
   LET v_fecha = TODAY
   DISPLAY " FECHA              : ",v_fecha USING "DD-MM-YYYY"
   DISPLAY " HORA               : ",TIME(CURRENT),"\n"
   DISPLAY ""

	LET v_comando = "mv ", 	p_ruta_rescate CLIPPED, "/SALDO_SAFRE_", v_fcorte USING 'ddmmyyyy', " ", 
									p_ruta_rescate CLIPPED, "/SALDO_SAFRE.preca"
   RUN v_comando

   DISPLAY ""
   DISPLAY "Generando el sumario..."
   DISPLAY ""
   DECLARE c_saldos CURSOR FOR
	
   SELECT subcuenta,SUM(importe)
     FROM cta_saldo_preca
    GROUP BY subcuenta

   FOREACH c_saldos INTO v_subcuenta,
                         v_importe

      IF v_subcuenta = 4 THEN
         LET v_total_pesos97 = v_importe 
      ELSE
         LET v_total_pesos92 = v_importe 
      END IF
    END FOREACH

    CLOSE c_saldos
    FREE c_saldos

    SELECT COUNT(unique nss)
      INTO v_total_registros
      FROM cta_saldo_preca

	LET v_comando = "echo 'T", v_fcorte USING 'yyyymmdd', 
										(v_total_pesos92 * 100) USING '&&&&&&&&&&&&&&&&&&',
										(v_total_pesos97 * 100) USING '&&&&&&&&&&&&&&&&&&',
										v_total_registros USING '&&&&&&&&&&',
						"' >> ", p_ruta_rescate CLIPPED, "/SALDO_SAFRE.preca"
	RUN v_comando

	#DISPLAY ""
	#DISPLAY "Eliminando los archivos de control de HPL..."
	#DISPLAY ""
	#LET v_comando = "rm ", p_ruta_rescate CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,"*"
	#DISPLAY v_comando
	#RUN v_comando

   DISPLAY ""
   DISPLAY "Enviando archivo..."
   LET v_comando = "cd /opt/Interpel/Scripts; ./SALDO_SAFRE.sh"
   RUN v_comando

END FUNCTION

FUNCTION fn_cifras_control()

   DEFINE preview   SMALLINT
   DEFINE vhandler   om.SaxDocumentHandler

	DEFINE v_ruta_ejecutable   LIKE seg_modulo.ruta_bin -- Ruta del ejecutable
	DEFINE v_nombre	STRING
	

   DEFINE v_rec_cifras  RECORD
      ind_infonavit    CHAR(1),
      f_proceso        DATE,
      f_valor          DATE,
      subcuenta        SMALLINT,
      importe          DECIMAL(22,2)
   END RECORD

	SELECT ruta_bin
     INTO v_ruta_ejecutable
   FROM seg_modulo 
   WHERE modulo_cod = 'cta'

   LET preview = FALSE
   INITIALIZE vhandler TO NULL

	LET v_nombre = v_ruta_ejecutable CLIPPED, "/CTAP051.4rp"
   LET vhandler = fn_configuracion(v_nombre, "PDF", preview )

   DECLARE cur_cifras CURSOR FOR SELECT tipo_trabajador,
                                        f_proceso,
                                        f_valor,
                                        subcuenta,
                                        SUM(importe)
                                   FROM cta_saldo_preca
                                  GROUP BY 1,2,3,4
                                  ORDER BY 1 
   
   START REPORT rep_cifras TO XML HANDLER vhandler
      FOREACH cur_cifras INTO v_rec_cifras.*
         OUTPUT TO REPORT rep_cifras (v_rec_cifras.*)
      END FOREACH
   FINISH REPORT rep_cifras
   
   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY "Termino la generacion del archivo de cifras control, ", v_ruta_listado
   DISPLAY ""
   DISPLAY "*******************************************************************"
 
END FUNCTION

FUNCTION fn_configuracion(v_reporte, v_formato, v_preview)
---------------------------------------------------------------------------

  DEFINE 
    v_reporte                STRING,
    v_formato                STRING,
    v_preview                INTEGER

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'cta'

   LET v_ruta_listado = v_ruta_listados CLIPPED , "/" ,
                        p_usuario_cod CLIPPED , "-", -- usuario
                        "CTAP05" CLIPPED, "-", -- programa
                        p_pid USING "&&&&&","-", -- PID
                        p_proceso_cod USING "&&&&&", "-", -- codigo del proceso
                        p_opera_cod   USING "&&&&&",".pdf" -- codigo de la operación

   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- if  the file loaded OK
      CALL fgl_report_setOutputFileName(v_ruta_listado)
      CALL fgl_report_selectDevice(v_formato)
      CALL fgl_report_selectPreview(v_preview)
   ELSE
       DISPLAY "Error: No se pudo encontrar el archivo ", v_reporte
       EXIT PROGRAM
   END IF
  
   RETURN fgl_report_commitCurrentSettings()

END FUNCTION


REPORT rep_cifras (p_rec_cifras) 
   DEFINE p_rec_cifras  RECORD
      ind_infonavit    CHAR(1),
      f_proceso        DATE,
      f_valor          DATE,
      subcuenta        SMALLINT,
      importe          DECIMAL(22,2)
   END RECORD

   DEFINE v_fecha               DATE
   DEFINE v_tipo_trabajador     STRING
   DEFINE v_pesos92    ,
          v_pesos97    DECIMAL(22,2)
   DEFINE v_registros  INTEGER

  ORDER EXTERNAL BY p_rec_cifras.ind_infonavit

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha = TODAY
      PRINTX p_rec_cifras.f_valor USING "dd-mm-yyyy"
      PRINTX v_fecha USING "dd-mm-yyyy"

  
  ON EVERY ROW
     IF p_rec_cifras.subcuenta = 4 THEN
        LET v_pesos97 = p_rec_cifras.importe
     ELSE
        LET v_pesos92 = p_rec_cifras.importe
     END IF
     
  BEFORE GROUP OF p_rec_cifras.ind_infonavit
      LET v_pesos97 = 0
      LET v_pesos92 = 0
      LET v_registros = 0

  AFTER GROUP OF p_rec_cifras.ind_infonavit

   SELECT COUNT(UNIQUE nss)
     INTO v_registros
     FROM cta_saldo_preca
    WHERE tipo_trabajador = p_rec_cifras.ind_infonavit

   CASE p_rec_cifras.ind_infonavit
       WHEN "I"   LET v_tipo_trabajador = "IMSS" 
       WHEN "S"   LET v_tipo_trabajador = "SOLO INFONAVIT"
       WHEN "E"   LET v_tipo_trabajador = "ESTADO-MUNICIPIO"
       OTHERWISE  LET v_tipo_trabajador = "TIPO NO ESPECIFICADO"
   END CASE

   PRINTX v_tipo_trabajador,
          p_rec_cifras.f_proceso USING 'dd-mm-yyyy',
          v_pesos92,
          v_pesos97,
          v_registros
END REPORT
