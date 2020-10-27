####################################################################
#Modulo            =>ACR                                           #
#Programa          =>ACRP01                                        #
#Objetivo          =>Programa para generar archivo txt con causal  #
#                    de rechazos para los módulos  ACR,AGR,GRT     #
#Autor             =>José Eduardo Ventura                          #
#Fecha inicio      =>04 Noviembre 2015                             #
####################################################################

DATABASE safre_viv

MAIN

   DEFINE v_qry STRING
   DEFINE a INTEGER
   DEFINE ch                       base.Channel
   DEFINE v_ruta_envio             CHAR (40)
   DEFINE v_modulo_cod             CHAR (3)
   DEFINE v_nom_arch STRING
   DEFINE v_cadena STRING
   DEFINE b INTEGER
   DEFINE v_cmd STRING
   DEFINE v_nom_archivo STRING
   DEFINE v_fecha CHAR(8)
   DEFINE v_tabla CHAR(40)
   DEFINE v_proceso SMALLINT
   DEFINE v_ext  CHAR(10)

   DEFINE arr_codigo DYNAMIC ARRAY OF RECORD
          codigo CHAR(3),
          descripcion CHAR (150)
   END RECORD

   DEFINE arr_detalle DYNAMIC ARRAY OF RECORD
          nss CHAR (11),
          nombre CHAR (120)
   END RECORD

   LET v_tabla             = ARG_VAL(1)
   LET v_modulo_cod        = ARG_VAL(2)

   CASE v_modulo_cod
      WHEN "acr" LET v_proceso = 215
      WHEN "agr" LET v_proceso = 314
      WHEN "grt" LET v_proceso = 1220
   END CASE

   LET v_ext = fn_recupera_extension(v_proceso,1)

   LET v_qry = "SELECT UNIQUE(tmp.diagnostico[1,3]),
                       cat.desc_rechazo
                  FROM ",v_tabla," tmp,
                       cat_rechazo cat
                 WHERE tmp.diagnostico[1,3] = cat.cod_rechazo
                   AND cat.tpo_rechazo = 'RCH' "

   PREPARE prp_codigo FROM v_qry
   DECLARE cur_codigo CURSOR FOR prp_codigo

   LET a= 1
   FOREACH cur_codigo INTO arr_codigo[a].*
      LET a = a+1
   END FOREACH

   IF arr_codigo[a].codigo IS NULL THEN
      CALL arr_codigo.deleteElement(a)
   END IF

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = v_modulo_cod

   LET ch = base.Channel.create()
   LET v_fecha = TODAY USING "ddmmyyyy"
   LET v_nom_arch = v_ruta_envio CLIPPED,"/","desc_rch_",v_modulo_cod,v_fecha,"1",".txt"

   CALL ch.openFile(v_nom_arch,"w")

   FOR a = 1 TO arr_codigo.getLength()

      LET v_cadena = arr_codigo[a].codigo,"-",arr_codigo[a].descripcion CLIPPED
      CALL ch.write([v_cadena])
      LET v_cadena =""
      CALL ch.write([v_cadena])
      LET v_qry = "SELECT tmp.nss,TRIM(a.ap_paterno_af)
                            ||' '||TRIM(a.ap_materno_af)
                            ||' '||TRIM(a.nombre_af)
                       FROM safre_tmp:tmp_rch_devolucion tmp,
                            afi_derechohabiente a
                      WHERE tmp.nss = a.nss
                        AND tmp.diagnostico[1,3] = ","'",arr_codigo[a].codigo,"'"

      PREPARE prp_detalle FROM v_qry
      DECLARE cur_detalle CURSOR FOR prp_detalle

      LET b= 1
      FOREACH cur_detalle INTO arr_detalle[b].*
         LET b = b+1
      END FOREACH

      IF arr_detalle[b].nss IS NULL THEN
         CALL arr_detalle.deleteElement(b)
      END IF
      FOR b= 1 TO arr_detalle.getLength()
         LET v_cadena = arr_detalle[b].nss," ",arr_detalle[b].nombre CLIPPED
         CALL ch.write([v_cadena])
      END FOR
      LET v_cadena = ""
      CALL ch.write([v_cadena])
      CALL ch.write([v_cadena])
   END FOR

   CALL ch.close()

   --se crea comando que elimina pipes
   LET v_nom_archivo = v_ruta_envio CLIPPED,"/","desc_rch_",v_modulo_cod,v_fecha,".",v_ext CLIPPED,".txt"
   LET v_cmd = "sed 's/|//g' ",v_nom_arch," > ",v_nom_archivo
   RUN v_cmd

   --DISPLAY v_nom_archivo

   LET v_cmd = "rm ",v_nom_arch
   RUN v_cmd

END MAIN