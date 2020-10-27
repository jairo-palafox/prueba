DATABASE safre_viv

PRIVATE DEFINE v_fecha                    STRING

MAIN
    DEFINE v_nombre_archivo    STRING
    CALL fn_prepara_salida() RETURNING v_nombre_archivo
END MAIN

PRIVATE FUNCTION fn_prepara_salida()
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio
   DEFINE v_nombre_archivo    STRING
   DEFINE v_archivo           base.Channel

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'ret'

   LET v_nombre_archivo = v_ruta_envio CLIPPED,"/GENERA_MASIVO_FONDO72.sql"
   LET v_fecha = TODAY USING 'yyyymmdd'

   LET v_archivo = base.Channel.create()
   CALL v_archivo.openFile(v_nombre_archivo,"w")
   CALL v_archivo.writeLine('SET PDQPRIORITY HIGH;')
   CALL v_archivo.writeLine('unload TO ' || v_ruta_envio CLIPPED || '/PAGO_MASIVO_FONDO72_' || v_fecha || '.PENDIENTE')
   CALL v_archivo.writeLine('SELECT')
   CALL v_archivo.writeLine('ret.nss,')
   CALL v_archivo.writeLine('afi.rfc,')
   CALL v_archivo.writeLine('afi.nombre,')
   CALL v_archivo.writeLine('ret.importe_viv72,')
   CALL v_archivo.writeLine('"0"')
   CALL v_archivo.writeLine('FROM ret_fondo_ahorro_masivo ret')
   CALL v_archivo.writeLine('INNER JOIN afi_fondo72 afi ON afi.id_afi_fondo72 = ret.id_afi_fondo72')
   CALL v_archivo.writeLine('WHERE ret.estado_solicitud IN (65,66);')
   CALL v_archivo.writeLine('')
   CALL v_archivo.writeLine('unload TO ' || v_ruta_envio CLIPPED || '/PAGO_MASIVO_FONDO72_' || v_fecha || '.RECHAZO')
   CALL v_archivo.writeLine('SELECT')
   CALL v_archivo.writeLine('ret.nss,')
   CALL v_archivo.writeLine('afi.rfc,')
   CALL v_archivo.writeLine('afi.nombre,')
   CALL v_archivo.writeLine('ret.importe_viv72,')
   CALL v_archivo.writeLine('ret.cod_rechazo ')
   CALL v_archivo.writeLine('FROM ret_fondo_ahorro_masivo ret')
   CALL v_archivo.writeLine('INNER JOIN afi_fondo72 afi ON afi.id_afi_fondo72 = ret.id_afi_fondo72')
   CALL v_archivo.writeLine('WHERE ret.estado_solicitud = 100 ')
   CALL v_archivo.writeLine('ORDER BY ret.cod_rechazo DESC;')

   CALL v_archivo.close()

   RETURN v_nombre_archivo
END FUNCTION