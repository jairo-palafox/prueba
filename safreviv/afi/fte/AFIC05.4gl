#########################################################################
#Proyecto          => INFONAVIT (MEXICO)                                #
#Propietario       => E.F.P.                                            #
#Programa AFIC04   => Consulta de altas de NSS nuevos                   #
#MODULO            => AFIC05                                            #
#Fecha             => 11 julio 2013                                     #
#########################################################################
DATABASE safre_viv

GLOBALS
DEFINE g_i_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_usuario_cod   LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       g_v_nom_prog    VARCHAR(30) -- nombre del programa

END GLOBALS

MAIN
--Sección de variables UI
DEFINE v_fecha_inicio   DATE,
       v_fecha_fin      DATE,
       v_sql            STRING, -- cadena SQL de consulta
       v_registro       RECORD
         nss             LIKE afi_derechohabiente.nss             ,
         tipo_trabajador LIKE afi_derechohabiente.tipo_trabajador ,
         nombre_imss     LIKE afi_derechohabiente.nombre_imss     ,
         folio_lote      LIKE afi_derechohabiente.folio_lote      ,
         f_apertura      LIKE afi_derechohabiente.f_apertura      
       END RECORD,
       v_registro_salida RECORD
         nss              STRING,
         tipo_trabajador  STRING,
         nombre_imss      STRING,
         folio_lote       STRING,
         f_apertura       STRING
       END RECORD,
       v_cadena          STRING,
       v_ruta_salida     LIKE seg_modulo.ruta_envio,
       v_ruta_archivo    STRING,
       v_archivo_salida base.channel -- archivo de salida

   -- se recuperan los parametros 
   LET g_usuario_cod          = ARG_VAL(1) -- Recibe la variable de usuario
   LET g_i_proceso_cod        = ARG_VAL(2) -- Recibe el tipo de proceso
   LET g_v_nom_prog           = ARG_VAL(3) -- Recibe el nombre del programa

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( g_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_v_nom_prog)
   END IF

   OPEN WINDOW w_consulta WITH FORM "AFIC051"

   -- como fechas por omision se usa la fecha del dia
   LET v_fecha_inicio = TODAY
   LET v_fecha_fin    = TODAY
      
   -- se obtienen los parametros de consulta
   INPUT BY NAME v_fecha_inicio, v_fecha_fin WITHOUT DEFAULTS
   ATTRIBUTES ( UNBUFFERED )
   
   
      ON ACTION ACCEPT
         -- ninguna de las fechas puede ser posterior a la actual
         IF ( v_fecha_inicio > TODAY OR v_fecha_fin > TODAY ) THEN
            CALL fn_mensaje("Atención","La fecha de consulta no puede ser posterior a la fecha actual","stop")
            CONTINUE INPUT
         END IF
         
         -- la fecha de inicio no puede ser mayor a la final
         IF ( v_fecha_inicio > v_fecha_fin ) THEN
            CALL fn_mensaje("Atención","La fecha de inicio no puede ser posterior a la fecha final","stop")
            CONTINUE INPUT
         END IF

         -- ninguna de las fechas puede estar vacia
         IF ( v_fecha_inicio IS NULL OR v_fecha_fin IS NULL) THEN
            CALL fn_mensaje("Atención","Es necesario capturar ambas fechas","stop")
            CONTINUE INPUT
         END IF

         -- se da alta prioridad
         EXECUTE IMMEDIATE "SET PDQPRIORITY HIGH"
         
         -- se consultan los datos
         LET v_sql = "\n SELECT          ",
                     "\n nss            ,",
                     "\n tipo_trabajador,",
                     "\n nombre_imss    ,",
                     "\n folio_lote     ,",
                     "\n f_apertura      ",
                     "\n FROM afi_derechohabiente ",
                     "\n WHERE f_apertura between ? and ?"

         -- se prepara y ejecuta la consulta
         PREPARE sid_consultanss FROM v_sql
         
         DECLARE cur_consultanss CURSOR FOR sid_consultanss
         
         -- se crea el archivo
         LET v_archivo_salida = base.channel.create()
         
         CALL v_archivo_salida.setDelimiter("|")
         
         SELECT ruta_envio
         INTO   v_ruta_salida
         FROM   seg_modulo
         WHERE  modulo_cod = "afi"
         
         -- se crea la ruta del archivo
         LET v_ruta_archivo = v_ruta_salida CLIPPED, "/nuevos_nss.txt"
         
         CALL v_archivo_salida.openFile(v_ruta_archivo, "w")
         
         FOREACH cur_consultanss USING v_fecha_inicio, v_fecha_fin
         INTO v_registro.*
            
            -- se transfieren y formatean los datos en el registro de salida
            LET v_registro_salida.nss             = v_registro.nss
            
            -- tipo de trabajador
            CASE v_registro.tipo_trabajador
               WHEN "I"
                  LET v_registro_salida.tipo_trabajador = "IMSS"
               
               WHEN "S"
                  LET v_registro_salida.tipo_trabajador = "SOLOINFO"
               
               WHEN "E"
                  LET v_registro_salida.tipo_trabajador = "EDO-MPIO"
               
               OTHERWISE 
                  LET v_registro_salida.tipo_trabajador = "NO_DEFINIDO"
           
            END CASE
            
            LET v_registro_salida.nombre_imss     = v_registro.nombre_imss
            LET v_registro_salida.folio_lote      = v_registro.folio_lote USING "########&"
            LET v_registro_salida.f_apertura      = v_registro.f_apertura USING "yyyymmdd"
         
            CALL v_archivo_salida.write([v_registro_salida.*])
         END FOREACH
         
         -- se libera el cursor
         FREE cur_consultanss
         
         -- se cierra el archivo
         CALL v_archivo_salida.close()

         -- se devuelve la prioridad
         EXECUTE IMMEDIATE "SET PDQPRIORITY LOW"
         
         -- se indica al usuario en que archivo quedo su consulta
         LET v_cadena = "Consulta finalizada\n\n",
                        "El resultado de la consulta se encuentra en el archivo:\n",
                        v_ruta_archivo
         
         -- mensaje a pantalla
         CALL fn_mensaje("Atención", v_cadena, "information")
         EXIT INPUT

      ON ACTION Cancel
         EXIT INPUT
   END INPUT 
      
   CLOSE WINDOW w_consulta
   

END MAIN