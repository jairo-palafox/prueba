#################################################################
#Modulo       => AFI                                            #
#Programa     => AFIP29                                         #
#Objetivo     => Generación de archivo causa raíz delta         #
#Autor        => Mauro Muñiz Caballero                          #
#Fecha inicio => 26 de julio de 2018                            #
#################################################################


DATABASE safre_viv

GLOBALS

--variable con intervalos de fecha para generación de archivos
   DEFINE f_inicial                 DATE           --fecha inicial
   DEFINE f_final                   DATE           --fecha final
   DEFINE v_dia_sem                 SMALLINT

--variables globales que se envían de lanzador
   DEFINE g_pid                     DECIMAL(9,0) 
   DEFINE g_proceso_cod             INTEGER
   DEFINE g_opera_cod               INTEGER
   DEFINE g_usuario                 CHAR(20)
   DEFINE g_folio                   DECIMAL(10,0)

   DEFINE v_nombre_archivo          STRING         -- variable de direcciÓn para generaciÓn de archivo K 
   DEFINE v_ruta_envio              LIKE seg_modulo.ruta_envio
   DEFINE r_b_valida                SMALLINT       --variable que regresa función opera_ini
   DEFINE v_ruta_listados           LIKE seg_modulo.ruta_listados

 -- variables para nuevo archivo nohup 
   DEFINE v_nohup                   STRING

--banderas para validar descargas de información
   DEFINE v_s_comando               STRING
   DEFINE f_ejecucion               DATE

   DEFINE v_proceso_desc            LIKE cat_proceso.proceso_desc
   DEFINE v_extension               LIKE cat_operacion.extension
   DEFINE v_opera_desc              LIKE cat_operacion.opera_desc
   DEFINE v_layout                  LIKE cat_operacion.layout_cod
   DEFINE v_usuario_proceso         LIKE seg_modulo.usuario
   DEFINE v_ruta_rescate            STRING
   DEFINE v_ruta_listados           LIKE seg_modulo.ruta_listados
   DEFINE r_resultado_opera         INTEGER

   DEFINE v_s_registro              STRING -- registro a insertar
   DEFINE v_ch_arch_delta           BASE.CHANNEL -- manejador de apuntador hacia archivo

END GLOBALS

MAIN

-- parametros que vienen de lanzador
   LET g_usuario       = ARG_VAL(1)
   LET g_pid           = ARG_VAL(2)
   LET g_proceso_cod   = ARG_VAL(3)
   LET g_opera_cod     = ARG_VAL(4)
   ---LET g_folio         = ARG_VAL(5)

   LET v_dia_sem       = WEEKDAY(TODAY)

  {
   IF v_dia_sem = 1 THEN
      LET f_inicial = TODAY - 3 UNITS DAY
   ELSE
      LET f_inicial = TODAY - 2 UNITS DAY
   END IF
  }

   LET f_ejecucion = TODAY
   LET f_inicial   = TODAY - 2 UNITS DAY
   LET f_final     = TODAY - 1 UNITS DAY

   CALL STARTLOG('/safreviv_log/afi/'||g_usuario CLIPPED|| ".AFIP29.log")

   -- se obtienen rutas
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'afi'

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

   WHENEVER ERROR CONTINUE

   CALL fn_recupera_inf_proceso(g_proceso_cod, g_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario_proceso

   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",f_ejecucion USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

   -- se solicita el número de folio asociado a la operación
   CALL fn_genera_folio(g_proceso_cod,g_opera_cod,g_usuario)
   RETURNING g_folio

   #Se actualiza el folio del proceso
   UPDATE bat_ctr_proceso
      SET folio = g_folio
    WHERE pid = g_pid

   UPDATE bat_ctr_operacion
      SET folio = g_folio
    WHERE pid = g_pid

   DISPLAY ""
   DISPLAY "Iniciando generación de archivo causa raiz delta"
   DISPLAY ""

   #Se ejecuta la funcion principal que generará el archivo
   CALL fn_genera_archivo()

   CALL  fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING r_resultado_opera

   IF(r_resultado_opera <> 0)THEN
      # Actualiza a estado erróneo
      DISPLAY "Ocurrió un ERROR al intentar actualizar el estado de la operación"
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod) 
      RETURNING r_resultado_opera
   END IF

   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY "Terminó la generación del archivo causa raíz delta: "
   DISPLAY ""
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY ""
   DISPLAY "*******************************************************************"

   WHENEVER ERROR STOP

END MAIN

FUNCTION fn_genera_archivo()

   DEFINE v_tpo_registro            CHAR(2)
   DEFINE v_encabezado              CHAR(5)
   DEFINE v_fecha                   CHAR(8)
   DEFINE v_nss                     CHAR(11)
   DEFINE v_ap_paterno_af           CHAR(40)
   DEFINE v_ap_materno_af           CHAR(40)
   DEFINE v_nombre_af               CHAR(40)
   DEFINE v_rfc                     CHAR(13)
   DEFINE v_curp                    CHAR(18)
   DEFINE v_cuenta_reg              INTEGER
   DEFINE v_cuenta_mod              INTEGER

   LET v_nombre_archivo = v_ruta_envio CLIPPED ,"/CausaRaizD_",TODAY USING "DDMMYYYY",".afcrd"

   LET v_tpo_registro = "01"
   LET v_encabezado   = "DELTA"
   LET v_fecha        = TODAY USING "DDMMYYYY"

   LET v_cuenta_reg = 0
   LET v_cuenta_mod = 0

   -- se crea el manejador de archivo
   LET v_ch_arch_delta = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_delta.openFile(v_nombre_archivo, "w" )
   CALL v_ch_arch_delta.setDelimiter("")

   -- se concatenan los campos a insertar
   LET v_s_registro = v_tpo_registro,"|",
                      v_encabezado,"|",
                      v_fecha

   -- se escribe el registro (encabezado) en el archivo
   CALL v_ch_arch_delta.write([v_s_registro])

  {
   SELECT COUNT(*)
     INTO v_cuenta_reg
     FROM afi_derechohabiente
    WHERE f_apertura BETWEEN f_inicial AND f_final

   SELECT COUNT(*)
     INTO v_cuenta_mod
     FROM afi_his_derechohabiente h,
          afi_derechohabiente a
    WHERE h.id_derechohabiente = a.id_derechohabiente
      AND h.f_modifica BETWEEN f_inicial AND f_final
      AND h.ind_modifica IN(1,2,6,9,10,11,13)

   LET v_cuenta_reg = v_cuenta_reg + v_cuenta_mod

   DISPLAY ""
   DISPLAY "Archvio generado   : ",v_nombre_archivo
   DISPLAY "Total de registros : ",v_cuenta_reg
   DISPLAY ""
  }

   LET v_tpo_registro = "02"

   LET v_s_comando = " SELECT d.nss,\n",
                     "        TRIM(d.ap_paterno_af) paterno,\n",
                     "        TRIM(d.ap_materno_af) materno,\n",
                     "        TRIM(d.nombre_af) nombre,\n",
                     "        TRIM(d.rfc) rfc,\n",
                     "        TRIM(d.curp) curp\n",
                     "   FROM afi_derechohabiente d\n",
                     "  WHERE d.f_apertura BETWEEN '",f_inicial,"' AND '",f_final,"'\n",
                     " UNION ALL\n",
                     " SELECT a.nss,\n",
                     "        TRIM(a.ap_paterno_af) paterno,\n",
                     "        TRIM(a.ap_materno_af) paterno,\n",
                     "        TRIM(a.nombre_af) nombre,\n",
                     "        TRIM(a.rfc) rfc,\n",
                     "        TRIM(a.curp) curp\n",
                     "   FROM afi_his_derechohabiente h,\n",
                     "        afi_derechohabiente a\n",
                     "  WHERE h.id_derechohabiente = a.id_derechohabiente\n",
                     "    AND h.f_modifica BETWEEN '",f_inicial,"' AND '",f_final,"'\n",
                     "    AND h.ind_modifica IN(1,2,6,9,10,11,13)\n",
                     "INTO TEMP tmp_delta"

   PREPARE prp_regs FROM v_s_comando
   EXECUTE prp_regs

   LET v_s_comando = " SELECT UNIQUE nss,\n",
                     "        TRIM(paterno),\n",
                     "        TRIM(materno),\n",
                     "        TRIM(nombre),\n",
                     "        TRIM(rfc),\n",
                     "        TRIM(curp)\n",
                     "  FROM tmp_delta"

   PREPARE prp_delta FROM v_s_comando
   DECLARE cur_delta CURSOR FOR prp_delta

   FOREACH cur_delta INTO v_nss,
                          v_ap_paterno_af,
                          v_ap_materno_af,
                          v_nombre_af,
                          v_rfc,
                          v_curp

      -- se concatenan los campos a insertar
      LET v_s_registro = v_tpo_registro,"|",
                         v_nss,"|",
                         v_ap_paterno_af CLIPPED,"|",
                         v_ap_materno_af CLIPPED,"|",
                         v_nombre_af CLIPPED,"|",
                         v_rfc CLIPPED,"|",
                         v_curp

      -- se escribe el registro (encabezado) en el archivo
      CALL v_ch_arch_delta.write([v_s_registro])
   END FOREACH

   SELECT COUNT(UNIQUE nss)
     INTO v_cuenta_reg
     FROM tmp_delta

   DISPLAY ""
   DISPLAY "Archvio generado   : ",v_nombre_archivo
   DISPLAY "Total de registros : ",v_cuenta_reg
   DISPLAY ""


   LET v_tpo_registro = "03"

   LET v_s_registro = v_tpo_registro,"|",
                      v_cuenta_reg USING "&&&&&&&&&"

   -- se escribe el registro (encabezado) en el archivo
   CALL v_ch_arch_delta.write([v_s_registro])

   -- se cierra el manejador de lectura
   CALL v_ch_arch_delta.close()

   INSERT INTO afi_ctr_causa_raiz
   VALUES(2,
          f_ejecucion,
          v_cuenta_reg,
          g_usuario)

END FUNCTION

