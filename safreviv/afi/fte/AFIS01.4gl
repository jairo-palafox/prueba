#####################################################################################################
#Proyecto          => INFONAVIT (MEXICO)                                                            #
#Propietario       => E.F.P.                                                                        #
#Programa AFIC03   => Programa que genera archivo de salida con registros rechazados de movimientos #
#                     afiliatorios                                                                  #
#Sistema           => AFIS01                                                                        #
#Fecha             => 30 de junio de 2012                                                           #
#####################################################################################################
DATABASE safre_viv

GLOBALS

   DEFINE v_d_pid               DECIMAL(9,0) -- identificador del proceso
   DEFINE v_i_opera_cod         LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE v_proceso_cod         LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   DEFINE v_folio               DECIMAL (9,0)
   DEFINE p_v_nom_prog          VARCHAR(30) -- nombre del programa

END GLOBALS

MAIN

   DEFINE v_sql                 STRING
   DEFINE v_ruta_envio          LIKE seg_modulo.ruta_envio
   DEFINE v_ruta_completa       STRING -- ruta completa del archivo
   DEFINE v_nombre_archivo      STRING -- nombre del archivo
   DEFINE v_channel             base.Channel -- apuntador para archivo
   DEFINE v_cadena              STRING -- para escribir registro en archivo

   DEFINE v_registro_archivo RECORD
      tpo_movimiento            CHAR(2),
      espacios                  CHAR(2),
      nrp                       CHAR(11),
      f_movimiento              CHAR(8),
      curp_rfc                  CHAR(18),
      t_trabajador              CHAR(1),
      nss                       CHAR(11),
      nombre                    CHAR(50),
      presentacion_extemp       CHAR(1),
      jornada_semana            CHAR(1),
      sdi                       CHAR(6),
      sexo                      CHAR(1),
      nss_correcto              CHAR(11),
      nombre_correcto           CHAR(50),
      riss_imss                 CHAR(1),
      cod_rechazo               CHAR(100)
   END RECORD

   DEFINE arr_afi_rch RECORD 
      tpo_movimiento            LIKE afi_rch_afiliatorio.tpo_movimiento,
      nrp                       LIKE afi_rch_afiliatorio.nrp,
      f_movimiento              LIKE afi_rch_afiliatorio.f_movimiento,
      curp_rfc                  LIKE afi_rch_afiliatorio.curp_rfc,
      t_trabajador              LIKE afi_rch_afiliatorio.t_trabajador,
      nss                       LIKE afi_rch_afiliatorio.nss,
      nombre                    LIKE afi_rch_afiliatorio.nombre,
      presentacion_extemp       LIKE afi_rch_afiliatorio.presentacion_extemp,
      jornada_semana            LIKE afi_rch_afiliatorio.jornada_semana,
      sdi                       LIKE afi_rch_afiliatorio.sdi,
      sexo                      LIKE afi_rch_afiliatorio.sexo,
      nss_correcto              LIKE afi_rch_afiliatorio.nss_correcto,
      nombre_correcto           LIKE afi_rch_afiliatorio.nombre_correcto,
      riss_imss                 SMALLINT,
      cod_rechazo               CHAR(100)
   END RECORD

   DEFINE v_registro_sumario RECORD
      control                   CHAR(1),
      num_altas                 CHAR(8),
      num_bajas                 CHAR(8),
      num_cambio_nss            CHAR(8),
      num_cambio_nombre         CHAR(8),
      num_cambio_salario        CHAR(8),
      num_reingresos            CHAR(8),
      num_riss                  CHAR(8),
      num_total_movs            CHAR(8),
      f_proceso                 CHAR(8)
   END RECORD

   DEFINE v_num_altas           DECIMAL(8,0)
   DEFINE v_num_bajas           DECIMAL(8,0)
   DEFINE v_num_cambio_nss      DECIMAL(8,0)
   DEFINE v_num_cambio_nombre   DECIMAL(8,0)
   DEFINE v_num_cambio_salario  DECIMAL(8,0)
   DEFINE v_num_reingresos      DECIMAL(8,0)
   DEFINE v_num_riss            DECIMAL(8,0)
   DEFINE v_num_total_movs      DECIMAL(8,0)
   DEFINE v_folio               LIKE glo_folio.folio -- folio del proceso
   DEFINE v_ext                 CHAR(10)

   -- se toman los parametros
   LET p_usuario_cod  = ARG_VAL(1) -- usuario que ejecuta el proceso
   LET v_folio        = ARG_VAL(2) -- folio del proceso
   LET v_proceso_cod  = ARG_VAL(3) -- codigo de proceso

   -- se obtiene la ruta envío
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = "afi"

    SELECT extension
      INTO v_ext
      FROM cat_operacion
     WHERE proceso_cod = 1801
       AND opera_cod = 1

   -- se conforma la ruta del archivo
   IF ( v_proceso_cod = 1801 ) THEN
      -- IMSS
      LET v_nombre_archivo = "AFI_IM_RCH_", v_folio USING "&&&&&&&&&"
   ELSE
      -- SOLO INFONAVIT
      LET v_nombre_archivo = "AFI_SI_RCH_", v_folio USING "&&&&&&&&&"
   END IF

   LET v_ruta_completa = v_ruta_envio CLIPPED, "/", v_nombre_archivo, v_ext CLIPPED,".txt"

   DISPLAY "_________________________________________________________________"
   DISPLAY "Generando archivo de registros rechazados"
   DISPLAY "\nArchivo: ", v_ruta_completa

   -- se crea el apuntador para escribir el archivo
   LET v_channel = base.Channel.create()

   -- se abre el channel
   CALL v_channel.setDelimiter("")
   CALL v_channel.openFile(v_ruta_completa, "w")

   -- se prepara la consulta
   LET v_sql = "\n SELECT afi.tpo_movimiento                                   ,",
               "\n        afi.nrp                                              ,",
               "\n        afi.f_movimiento                                     ,",
               "\n        afi.curp_rfc                                         ,",
               "\n        afi.t_trabajador                                     ,",
               "\n        afi.nss                                              ,",
               "\n        afi.nombre                                           ,",
               "\n        afi.presentacion_extemp                              ,",
               "\n        afi.jornada_semana                                   ,",
               "\n        afi.sdi                                              ,",
               "\n        afi.sexo                                             ,",
               "\n        afi.nss_correcto                                     ,",
               "\n        afi.nombre_correcto                                  ,",
               "\n        afi.riss_imss                                        ,",
               "\n        afi.cod_rechazo || '-' ||  CT.des_rechazo descripcion ",
               "\n FROM afi_rch_afiliatorio afi, afi_cat_rch ct ",
               "\n WHERE afi.cod_rechazo = ct.cod_rechazo ",
               "\n AND   folio_lote = ", v_folio

   -- se prepara y ejecuta el enunciado
   PREPARE sid_rechazos FROM v_sql
 
   DECLARE cur_rechazos CURSOR FOR sid_rechazos

   -- se inician los contadores
   LET v_num_altas          = 0
   LET v_num_bajas          = 0
   LET v_num_cambio_nss     = 0
   LET v_num_cambio_nombre  = 0
   LET v_num_cambio_salario = 0
   LET v_num_reingresos     = 0
   LET v_num_riss           = 0
   LET v_num_total_movs     = 0

   -- se escriben los datos en el archivo
   FOREACH cur_rechazos INTO arr_afi_rch.*
      -- se transfieren los datos al registro que se escribe en archivo
      LET v_registro_archivo.tpo_movimiento       = arr_afi_rch.tpo_movimiento
      LET v_registro_archivo.espacios             = "00"
      LET v_registro_archivo.nrp                  = arr_afi_rch.nrp
      LET v_registro_archivo.f_movimiento         = arr_afi_rch.f_movimiento
      LET v_registro_archivo.curp_rfc             = arr_afi_rch.curp_rfc
      LET v_registro_archivo.t_trabajador         = arr_afi_rch.t_trabajador
      LET v_registro_archivo.nss                  = arr_afi_rch.nss
      LET v_registro_archivo.nombre               = arr_afi_rch.nombre
      LET v_registro_archivo.presentacion_extemp  = arr_afi_rch.presentacion_extemp
      LET v_registro_archivo.jornada_semana       = arr_afi_rch.jornada_semana
      LET v_registro_archivo.sdi                  = arr_afi_rch.sdi USING "&&&&&&"
      LET v_registro_archivo.sexo                 = arr_afi_rch.sexo
      LET v_registro_archivo.nss_correcto         = arr_afi_rch.nss_correcto
      LET v_registro_archivo.nombre_correcto      = arr_afi_rch.nombre_correcto
      LET v_registro_archivo.riss_imss            = arr_afi_rch.riss_imss
      LET v_registro_archivo.cod_rechazo          = arr_afi_rch.cod_rechazo

      -- los registros de baja se cambiaron a 09 para ordernalos, se devuelven a 02
      IF ( v_registro_archivo.tpo_movimiento = "09" ) THEN
         LET v_registro_archivo.tpo_movimiento = "02"
      END IF

      -- se cuentan los registros
      CASE v_registro_archivo.tpo_movimiento
        WHEN "01" -- altas
           LET v_num_altas          = v_num_altas + 1
        WHEN "02" -- baja
           LET v_num_bajas          = v_num_bajas + 1
        WHEN "05" -- cambio NSS
           LET v_num_cambio_nss     = v_num_cambio_nss + 1
        WHEN "06" -- cambio nombre
           LET v_num_cambio_nombre  = v_num_cambio_nombre + 1
        WHEN "07" -- cambio salario
           LET v_num_cambio_salario = v_num_cambio_salario + 1
        WHEN "08" -- reingresos
           LET v_num_reingresos     = v_num_reingresos + 1
        WHEN "21" -- altas riss
           LET v_num_riss           = v_num_riss + 1
      END CASE

      -- se cuenta un registro en el total
      LET v_num_total_movs     = v_num_total_movs + 1

      -- se concatenan los campos
      LET v_cadena = v_registro_archivo.tpo_movimiento     ,
                     v_registro_archivo.espacios           ,
                     v_registro_archivo.nrp                ,
                     v_registro_archivo.f_movimiento       ,
                     v_registro_archivo.curp_rfc           ,
                     v_registro_archivo.t_trabajador       ,
                     v_registro_archivo.nss                ,
                     v_registro_archivo.nombre             ,
                     v_registro_archivo.presentacion_extemp,
                     v_registro_archivo.jornada_semana     ,
                     v_registro_archivo.sdi                ,
                     v_registro_archivo.sexo               ,
                     v_registro_archivo.nss_correcto       ,
                     v_registro_archivo.nombre_correcto    ,
                     v_registro_archivo.riss_imss          ,
                     v_registro_archivo.cod_rechazo

      -- se escribe registro en archivo
      CALL v_channel.writeLine(v_cadena)

   END FOREACH

   -- se escribe el registro de sumario
   LET v_registro_sumario.control             = "@"
   LET v_registro_sumario.num_altas           = v_num_altas USING "&&&&&&&&"
   LET v_registro_sumario.num_bajas           = v_num_bajas USING "&&&&&&&&"
   LET v_registro_sumario.num_cambio_nss      = v_num_cambio_nss USING "&&&&&&&&"
   LET v_registro_sumario.num_cambio_nombre   = v_num_cambio_nombre USING "&&&&&&&&"
   LET v_registro_sumario.num_cambio_salario  = v_num_cambio_salario USING "&&&&&&&&"
   LET v_registro_sumario.num_reingresos      = v_num_reingresos USING "&&&&&&&&"
   LET v_registro_sumario.num_riss            = v_num_riss using "&&&&&&&&"
   LET v_registro_sumario.num_total_movs      = v_num_total_movs USING "&&&&&&&&"
   LET v_registro_sumario.f_proceso           = TODAY USING "yyyymmdd"

   CALL v_channel.write([v_registro_sumario.*])

   -- se cierra el archivo
   CALL v_channel.close()

   -- se libera el cursor
   FREE cur_rechazos

   DISPLAY "\n\nFinalizado...\n\n"
   DISPLAY "_________________________________________________________________\n"

END MAIN 