#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => CTA                                                     #
#Programa          => DESCARGA_EDO_CTA                                        #
#Objetivo          => PROGRAMA QUE GENERA EL ESTADO DE CUENTA MASIVO DE UNA   #
#                     TABLA YA PREPARADA                                      #
#Fecha Inicio      => 17-ENERO-2014                                           #
###############################################################################
DATABASE safre_viv

PRIVATE DEFINE p_usuario_cod              CHAR(20)

#Variables para la generacion del archivo
PRIVATE DEFINE v_ffin                     DATE
PRIVATE DEFINE v_finicio                  DATE
PRIVATE DEFINE v_archivo_edo_cta          STRING
PRIVATE DEFINE v_ruta_envio               LIKE seg_modulo.ruta_envio

MAIN
   DEFINE p_num_archivo               INTEGER
   DEFINE v_fproceso                      DATE

   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_num_archivo    = ARG_VAL(1)
   LET p_usuario_cod = 'safreviv'

   #Primero se valida si se puede ejecutar la generacion de saldos
   LET v_fproceso = TODAY
   --LET v_fproceso = MDY(2,5,2014)
   --Se establece la fecha de corte como el ultimo dia natural del mes non inmediato anterior
   IF MONTH(v_fproceso) MOD 2 = 0 THEN
      #En este caso nos encontramos en mes par, por lo que obtenemos el ultimo dia del mes anterior
      LET v_ffin = MDY(MONTH(v_fproceso),1,YEAR(v_fproceso)) - 1
   ELSE
      #En este caso nos encontramos en un mes non por lo que aun no termina el periodo en curso, 
      #se calcula la fecha fin como el ultimo dia del mes non anterior
      LET v_ffin = MDY(MONTH(MDY(MONTH(v_fproceso),1,YEAR(v_fproceso)) - 1),1,YEAR(MDY(MONTH(v_fproceso),1,YEAR(v_fproceso)) - 1)) - 1
   END IF
   #Se establece la fecha de inicio como el primer dia del mes anterior a la fecha fin
   LET v_finicio = MDY(MONTH(MDY(MONTH(v_ffin),1,YEAR(v_ffin)) -1),1,YEAR(MDY(MONTH(v_ffin),1,YEAR(v_ffin)) -1))

   CALL fn_genera_archivo(p_num_archivo)

END MAIN


PRIVATE FUNCTION fn_genera_archivo(p_num_archivo)

   DEFINE p_num_archivo                SMALLINT
   
   DISPLAY "Se inicia la creación del archivo ", p_num_archivo

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'cta'

   LET v_archivo_edo_cta      = "edo_cta_masivo_tmp"

   #Genera device HPL para archivo de estado de cuenta masivo

   CALL fn_crea_project("edo_cta")

   CALL fn_genera_device(v_ruta_envio,
                         v_archivo_edo_cta,
                         p_usuario_cod)

   CALL fn_genera_query(v_ruta_envio,
                      v_archivo_edo_cta,
                      p_usuario_cod,
                      p_num_archivo)

   CALL fn_genera_format(v_ruta_envio,
                         v_archivo_edo_cta,
                         p_usuario_cod)
   
   CALL fn_genera_map(v_ruta_envio,
                      v_archivo_edo_cta,
                      p_usuario_cod)


   DISPLAY ""
   DISPLAY "Inicia la generacion de los detalles del archivo"
   DISPLAY ""

   PREPARE exe_pdq FROM "SET PDQPRIORITY HIGH"
   EXECUTE exe_pdq

   CALL fn_genera_job(v_ruta_envio,
                      v_archivo_edo_cta,
                      p_usuario_cod,
                      'edo_cta')

   CALL fn_ejecuta_hpl(v_ruta_envio,
                      v_archivo_edo_cta,
                      p_usuario_cod,
                      'edo_cta',
                      p_num_archivo)
END FUNCTION

PRIVATE FUNCTION fn_crea_project(p_proyecto)
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

PRIVATE FUNCTION fn_genera_device(p_ruta_envio,p_archivo_salida,p_usuario)
    DEFINE p_ruta_envio       STRING,
           p_archivo_salida   LIKE cat_layout.archivo,
           p_usuario          LIKE seg_modulo.usuario,
           v_str_archivo      STRING,
           v_canal            base.Channel,
           v_cadena           STRING

   LET v_str_archivo = p_ruta_envio CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".device"
   LET v_canal = base.Channel.create()
   CALL v_canal.openFile(v_str_archivo,"w")

   LET v_cadena = p_ruta_envio CLIPPED, '/ESTADO_DE_CUENTA_MASIVO_', v_ffin USING 'ddmmyyyy'
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

PRIVATE FUNCTION fn_genera_format(p_ruta_envio, p_archivo_salida, p_usuario)

   DEFINE v_ch_format      base.Channel
   DEFINE p_ruta_envio     STRING
   DEFINE p_usuario        STRING
   DEFINE p_archivo_salida STRING
   DEFINE v_archivo_format STRING

   LET v_archivo_format = p_ruta_envio CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".format"

   LET v_ch_format = base.Channel.create()
   CALL v_ch_format.openFile(v_archivo_format,"w")
   CALL v_ch_format.writeLine('BEGIN OBJECT COBOLFORMAT '||p_archivo_salida CLIPPED||".format \n")
	
   CALL v_ch_format.writeLine('PROJECT       edo_cta')
   CALL v_ch_format.writeLine('CHARACTERSET  ASCII ')
   CALL v_ch_format.writeLine('MACHINE       Intel ')
	CALL v_ch_format.writeLine('DRIVER       	COBOL \n')
   
   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	tipo_registro')
	CALL v_ch_format.writeLine('PICTURE      	A(2)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   	registro')
   CALL v_ch_format.writeLine('PICTURE      	A(405)')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')

   CALL v_ch_format.writeLine('BEGIN SEQUENCE')
   CALL v_ch_format.writeLine('FIELDNAME   dummycr')
   CALL v_ch_format.writeLine('PICTURE      	X')
   CALL v_ch_format.writeLine('USAGE    		Chars')
   CALL v_ch_format.writeLine('END SEQUENCE \n')
   
   CALL v_ch_format.writeLine('END OBJECT')

   CALL v_ch_format.close()

   CALL fn_crea_objeto(v_archivo_format, p_archivo_salida CLIPPED||".format", "format", "edo_cta")

END FUNCTION

PRIVATE FUNCTION fn_genera_map(p_ruta_envio, p_archivo_salida, p_usuario)
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

   CALL v_ch_map.writeLine('PROJECT      edo_cta')
   CALL v_ch_map.writeLine('FORMAT       '||p_archivo_salida CLIPPED||'.format')
   CALL v_ch_map.writeLine('DATABASE     safre_viv')
   CALL v_ch_map.writeLine('QUERY        '||p_archivo_salida CLIPPED||'.query')

   CALL v_ch_map.writeLine('BEGIN SEQUENCE')
   CALL v_ch_map.writeLine('COLUMNNAME      tipo_registro')
   CALL v_ch_map.writeLine('FIELDNAME       tipo_registro')
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
   CALL v_ch_map.writeLine('COLUMNNAME      registro')
   CALL v_ch_map.writeLine('FIELDNAME       registro')
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

   LET v_comando = "onpladm delete map ",p_archivo_salida CLIPPED||".map -fu -p edo_cta ",
                   " 1>/dev/null 2>/dev/null"
   RUN v_comando

   CALL fn_crea_objeto(v_archivo_map, p_archivo_salida CLIPPED||".map", "map", "edo_cta" )
END FUNCTION

PRIVATE FUNCTION fn_genera_query(p_ruta_envio, p_archivo_salida, p_usuario, p_num_archivo)
   DEFINE p_ruta_envio   STRING
   DEFINE p_usuario        STRING
   DEFINE p_archivo_salida STRING
   DEFINE p_num_archivo    SMALLINT
   DEFINE v_archivo_query  STRING
   DEFINE v_query          STRING

   DEFINE v_txt            CHAR(2)
   DEFINE v_ch_query       base.Channel

   LET v_txt = p_num_archivo

   LET v_archivo_query = p_ruta_envio CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,".query"

   LET v_ch_query = base.Channel.create()
   CALL v_ch_query.openFile(v_archivo_query,"w")

   CALL v_ch_query.writeLine('BEGIN OBJECT QUERY   '||p_archivo_salida CLIPPED||".query")

   CALL v_ch_query.writeLine('PROJECT edo_cta')
   CALL v_ch_query.writeLine('DATABASE safre_viv')

   LET v_query = '"SELECT ',
                     "tipo_registro, ",
                     "registro, ",
                     "fn_salto_linea() dummycr ",
                  "FROM edo_cuenta_", v_txt CLIPPED, " ",
                  'ORDER BY id_edo_cuenta"'

   CALL v_ch_query.writeLine('SELECTSTATEMENT '||v_query)
   CALL v_ch_query.writeLine('END OBJECT')

   CALL v_ch_query.close()

   CALL fn_crea_objeto(v_archivo_query, p_archivo_salida CLIPPED||".query", "query", "edo_cta")
END FUNCTION

PRIVATE FUNCTION fn_genera_job(p_ruta_envio,p_archivo_salida,p_usuario,p_proyecto)

    DEFINE p_ruta_envio     STRING,
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
   CALL v_canal.writeLine('MAXERRORS         1 ')
   CALL v_canal.writeLine('END  OBJECT      \n')

   CALL v_canal.close()


   CALL fn_crea_objeto(v_str_archivo, p_archivo_salida CLIPPED||".job", "job", p_proyecto )
END FUNCTION

PRIVATE FUNCTION fn_crea_objeto(p_str_archivo, p_objeto, p_tipo, p_proyecto)
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
         LET v_comando = "onpladm describe map ", p_objeto,
                         " -fu -p ",p_proyecto , " 1>/dev/null 2>/dev/null"
     WHEN "query"
         LET v_comando = "onpladm describe query ", p_objeto,
                         " -p ",p_proyecto , " 1>/dev/null 2>/dev/null"

   END CASE

	#DISPLAY v_comando
   RUN v_comando RETURNING r_ejecucion

   IF r_ejecucion <> 0 THEN
      LET v_comando = "onpladm create object -F ",
                      p_str_archivo , " 1>/dev/null 2>/dev/null"

		DISPLAY ""
		DISPLAY "CREA OBJETO HPL ", p_str_archivo
   ELSE
      LET v_comando = "onpladm modify object -F ",
                      p_str_archivo , " 1>/dev/null 2>/dev/null"
		DISPLAY ""
		DISPLAY "MODIFICA OBJETO HPL ", p_str_archivo
   END IF

	#DISPLAY v_comando
   RUN v_comando
END FUNCTION

PRIVATE FUNCTION fn_ejecuta_hpl(p_ruta_envio, p_archivo_salida,p_usuario,p_proyecto, p_num_archivo)

    DEFINE p_ruta_envio   STRING,
           p_proyecto       STRING,
           p_archivo_salida LIKE cat_layout.archivo,
           p_usuario        LIKE seg_usuario.usuario_cod,
           p_num_archivo    SMALLINT,
           v_comando        STRING,
           v_fecha          DATE

   DEFINE v_txt            CHAR(2)

   LET v_txt = p_num_archivo
   LET v_comando = "onpladm run job ", p_archivo_salida CLIPPED, ".job -fu -p ",
                    p_proyecto , " 1>/dev/null 2>/dev/null"
   
   DISPLAY " INICIO ETAPA       : EJECUTA JOB DESCARGA HPL"
   LET v_fecha = TODAY
   DISPLAY " FECHA              : ",v_fecha USING "DD-MM-YYYY"
   DISPLAY " HORA               : ",TIME(CURRENT),"\n"
   DISPLAY ""
   #registra etapa en archivo de monitoreo

   RUN v_comando
   --DISPLAY v_comando
   DISPLAY " FIN ETAPA          : EJECUTA JOB DESCARGA HPL"
   LET v_fecha = TODAY
   DISPLAY " FECHA              : ",v_fecha USING "DD-MM-YYYY"
   DISPLAY " HORA               : ",TIME(CURRENT),"\n"
   DISPLAY ""

	LET v_comando = "mv ", 	p_ruta_envio CLIPPED, "/ESTADO_DE_CUENTA_MASIVO_", v_ffin USING 'ddmmyyyy', " ", 
									p_ruta_envio CLIPPED, "/ESTADO_DE_CUENTA_MASIVO_", v_ffin USING 'ddmmyyyy', "_",v_txt CLIPPED, ".edo_cta"
   RUN v_comando

	DISPLAY ""
	DISPLAY "Eliminando los archivos de control de HPL..."
	DISPLAY ""
	LET v_comando = "rm ", p_ruta_envio CLIPPED,"/",p_usuario CLIPPED,".",p_archivo_salida CLIPPED,"*"
	RUN v_comando

	LET v_comando = "rm ", p_ruta_envio CLIPPED,"/edo_cuenta_",v_txt CLIPPED,".sql"
	RUN v_comando
   

END FUNCTION