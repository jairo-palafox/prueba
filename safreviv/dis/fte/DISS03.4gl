################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 27/03/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISS03                                                   #
#Objetivo          => Programa que genra la intefaz de los registros           #
#                     con las aportaciones que se envian a las                 #
#                     Entidades Financieras                                    #
#Fecha inicio      => 21/03/2012                                               #
################################################################################
DATABASE
     safre_viv


GLOBALS 
   DEFINE 
      v_archivo_copia      VARCHAR (25),
      v_comando_dos        STRING 
END GLOBALS

     
MAIN
DEFINE v_ch_arch_salida BASE.CHANNEL,
       v_folio_liquida LIKE dis_preliquida.folio_liquida,
       v_ruta_envio_dis LIKE seg_modulo.ruta_envio,
       v_modulo_cod LIKE seg_modulo.modulo_cod,
       v_ruta_nomarch VARCHAR(100), -- ruta y nombre del archivo de salida
       v_nom_archivo VARCHAR(40), -- nombre del archivo de salida
       v_ddmmaaaa    VARCHAR(08), -- fecha del archivo de salida
       v_cont_dia    SMALLINT, -- consecutivo por dia de archivo generado
       v_reg_dia     CHAR(03), -- Parametro consecutivo de registro por dia
       v_ins_reg     STRING,  -- almacena cadena a insertar en el archivo
       v_qry_txt     STRING, -- cadena para preparar consultas 
       v_busca_nom_archivo STRING -- busca nombre de archivo
DEFINE v_arr_tipo_reg RECORD  --Record para tipo de registro cero
         v_id_derechohabiente LIKE dis_interface_ef.id_derechohabiente,
         v_periodo_pago LIKE dis_interface_ef.periodo_pago,
         v_aportacion   LIKE dis_interface_ef.imp_ap_pat,
         v_folio_sua    CHAR (6)--LIKE dis_interface_ef.folio_sua
       END RECORD
DEFINE v_fecha_dia    CHAR(08),
       v_interface    CHAR(02),
       v_interface_finnanciera CHAR(02),
       v_nss          LIKE afi_derechohabiente.nss,
       v_nombre_af    LIKE afi_derechohabiente.nombre_af,
       v_paterno_af   LIKE afi_derechohabiente.ap_paterno_af,
       v_materno_af   LIKE afi_derechohabiente.ap_materno_af,
       v_nombre       CHAR(30)

DEFINE 
   v_cuenta_ef    INTEGER,
   c_aportacion   CHAR (10)

DEFINE
    v_estado                 INTEGER, 
    r_bnd_proceso            INTEGER, 
    v_status_err             INTEGER, 
    v_desc_err               CHAR(50),
    p_proceso_cod            INTEGER, 
    p_opera_cod              INTEGER, 
    v_destino_dis            INTEGER,
    v_usuario                CHAR(20)
       
   --Asignación de parametros generales
   LET v_folio_liquida = ARG_VAL(1) -- Valor de argumento uno de DISL04
   
   LET v_modulo_cod  = "dis"
   LET v_cont_dia    = 1
   LET v_cuenta_ef   = 0

   LET v_estado      = 101
   LET r_bnd_proceso = 0 
   LET p_proceso_cod = 901
   LET p_opera_cod   = 1
   LET v_destino_dis = 3
   LET v_usuario     = "SAFREVIV"

   SELECT COUNT (*)
   INTO v_cuenta_ef
   FROM dis_interface_ef
   WHERE folio_liquida = v_folio_liquida
   IF v_cuenta_ef = 0 OR v_cuenta_ef IS NULL THEN 
      DISPLAY "No se generó la interface de registros a Entidades Financieras por falta de información"
      EXIT PROGRAM 
   END IF 
   
   --Sección consultas preparadas
   --Consulta tipo registro
   {LET v_qry_txt = "\n SELECT afi.nss, ",
                   "\n        afi.nombre_af, ",
                   "\n        afi.ap_paterno_af, ",
                   "\n        afi.ap_materno_af, ",
                   "\n        det.id_derechohabiente, ",
                   "\n        det.periodo_pago,",
                   "\n        det.imp_ap_pat, ",
                   "\n        det.folio_sua",
                   "\n FROM   afi_derechohabiente afi, ",
                   "\n        dis_interface_ef det",
                   "\n WHERE  afi.id_derechohabiente = det.id_derechohabiente ",
                   "\n AND    det.folio_liquida = ",v_folio_liquida
   PREPARE prp_consulta_folio_interface_ef FROM v_qry_txt}

   WHENEVER ERROR CONTINUE;
     DROP TABLE tmp_dis_interface_ef;
   
   SELECT afi.nss, 
          afi.nombre_af, 
          afi.ap_paterno_af, 
          afi.ap_materno_af, 
          det.id_derechohabiente, 
          det.periodo_pago,
          det.imp_ap_pat, 
          det.folio_sua
   FROM   afi_derechohabiente afi, 
          dis_interface_ef det
   WHERE  afi.id_derechohabiente = det.id_derechohabiente 
   AND    det.folio_liquida      = v_folio_liquida
   INTO TEMP tmp_dis_interface_ef

   UPDATE STATISTICS FOR TABLE tmp_dis_interface_ef

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
    INTO v_ruta_envio_dis
   FROM seg_modulo
   WHERE modulo_cod = v_modulo_cod

   --Se obtiene el usuario del folio liquidado
   SELECT a.usuario
   INTO   v_usuario
   FROM   glo_folio a
   WHERE  a.folio = v_folio_liquida

   -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   LET v_nom_archivo = "/ent_fin_"
   #LET v_nom_archivo = "/ent_fin"
   
   LET v_ddmmaaaa = TODAY USING "ddmmyyyy"
   LET v_busca_nom_archivo = "ent_fin_" || v_ddmmaaaa
   --Obtine consecutivo para archivo por día
   CALL fn_crea_nombre_archivo(v_ruta_envio_dis,v_busca_nom_archivo)
        RETURNING v_cont_dia
   LET v_reg_dia = v_cont_dia USING "&&&"

   LET v_nom_archivo = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
   #LET v_nom_archivo = v_nom_archivo CLIPPED||"."|| v_modulo_cod

   LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo
   --DISPLAY "v_ruta_nomarch ",v_ruta_nomarch

   WHENEVER ERROR CONTINUE

   PREPARE prp_fn_transaccion24
   FROM    "EXECUTE FUNCTION sp_dis_transaccion24(?,?,?,?,?,?,?)"
   EXECUTE prp_fn_transaccion24 INTO r_bnd_proceso, v_status_err, v_desc_err                                    
                              USING p_proceso_cod,
                                    p_opera_cod, 
                                    v_nom_archivo, 
                                    v_folio_liquida,
                                    v_estado, 
                                    v_destino_dis,
                                    v_usuario

   WHENEVER ERROR STOP 

   DISPLAY "Función Transaccion 24 ",r_bnd_proceso
   DISPLAY "Código:",v_status_err         
   DISPLAY "mensaje", v_desc_err
         
   IF r_bnd_proceso <> 0 THEN
     DISPLAY "Error en la transacción 24 ",v_status_err," ",v_desc_err
     EXIT PROGRAM
   END IF
   
   -- se crea el manejador de archivo
   LET v_ch_arch_salida = base.Channel.create()
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
   CALL v_ch_arch_salida.setDelimiter("")

   ######################################
   #####  Carga  tipo de registro   #####
   ######################################
   LET v_fecha_dia = TODAY USING "yyyymmdd"
   LET v_interface = "AS"
   LET v_interface_finnanciera = "  "

   {DECLARE cur_folio_interface_ef CURSOR FOR prp_consulta_folio_interface_ef
      FOREACH cur_folio_interface_ef INTO v_nss,
                                          v_nombre_af,
                                          v_paterno_af,
                                          v_materno_af,  
                                          v_arr_tipo_reg.v_id_derechohabiente,
                                          v_arr_tipo_reg.v_periodo_pago,
                                          v_arr_tipo_reg.v_aportacion,
                                          v_arr_tipo_reg.v_folio_sua}

   DECLARE cur_folio_interface_ef CURSOR FOR
   SELECT *
   FROM   tmp_dis_interface_ef
   FOREACH cur_folio_interface_ef INTO v_nss,
                                       v_nombre_af,
                                       v_paterno_af,
                                       v_materno_af,  
                                       v_arr_tipo_reg.v_id_derechohabiente,
                                       v_arr_tipo_reg.v_periodo_pago,
                                       v_arr_tipo_reg.v_aportacion,
                                       v_arr_tipo_reg.v_folio_sua

         --Se multiplica por 100 para quitar punto decimal
         LET c_aportacion = (v_arr_tipo_reg.v_aportacion * 100) USING "&&&&&&&&&&"

         LET v_arr_tipo_reg.v_folio_sua = v_arr_tipo_reg.v_folio_sua USING "&&&&&&"
         
         --Se deja el folio_sua en ceros
         --LET v_arr_tipo_reg.v_folio_sua = '      '

         {SELECT nss,nombre_af,ap_paterno_af,ap_materno_af
           INTO v_nss,v_nombre_af,v_paterno_af,v_materno_af
         FROM afi_derechohabiente
         WHERE id_derechohabiente = v_arr_tipo_reg.v_id_derechohabiente}

         --DISPLAY "nombre_af ",v_nombre_af
         --DISPLAY "ap_paterno_af ",v_paterno_af
         --DISPLAY "ap_materno_af ",v_materno_af

         
         --Concatena nombre completo
         LET v_nombre = v_nombre_af CLIPPED ," ",v_paterno_af CLIPPED ," ",v_materno_af CLIPPED 

         LET v_ins_reg = v_nss,v_nombre,v_arr_tipo_reg.v_periodo_pago,
                                        c_aportacion,
                                        v_arr_tipo_reg.v_folio_sua,
                         v_fecha_dia,v_fecha_dia,v_interface,v_interface_finnanciera
         CALL v_ch_arch_salida.write([v_ins_reg])
   END FOREACH

   CALL v_ch_arch_salida.close()
   DISPLAY "Se ha generado la interface de Entidades Financieras correctamente: ",v_nom_archivo

   --Cambia el formato del archivo a DOS
   LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
   RUN v_comando_dos

   --Genera una copia de la interface con el nombre corto
   CALL fn_genera_copia_interface_ent(v_ruta_nomarch,v_ruta_envio_dis)
   
END MAIN
#Objetivo: genera el número consecutivo por día para el archivo de salida
FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis,p_busca_nom_archivo)
DEFINE p_ruta_envio_dis     LIKE seg_modulo.ruta_envio,
       p_busca_nom_archivo  VARCHAR(40),
       v_cmd                STRING,
       v_consecutivo        INTEGER
DEFINE fn CHAR(22)
DEFINE ch base.Channel

    LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

    LET ch = base.Channel.create()
    CALL ch.setDelimiter(".")
    CALL ch.openPipe(v_cmd,"r")
    WHILE ch.read([fn])
       LET v_consecutivo = fn[17,19]
    END WHILE
    CALL ch.close()
    LET v_consecutivo = v_consecutivo + 1

    IF length(v_consecutivo) = 0 THEN
       LET v_consecutivo = 1
    END IF

    RETURN v_consecutivo

END FUNCTION


--Genera la copia del archivo generado
FUNCTION fn_genera_copia_interface_ent(p_archivo_envio,p_ruta_destino)

    DEFINE 
        v_cmd                STRING,
        p_archivo_envio      VARCHAR(100),
        p_ruta_destino       VARCHAR(40)

    --DISPLAY "p_archivo_envio: ",p_archivo_envio
    --DISPLAY "p_ruta_destino: ",p_ruta_destino
    

    LET v_archivo_copia = "ent_fin"
    LET v_archivo_copia = v_archivo_copia CLIPPED,".dis" 
    --DISPLAY "v_archivo_copia -- ",v_archivo_copia
    
    LET v_cmd = "cat ",p_archivo_envio CLIPPED, " > ",p_ruta_destino CLIPPED, "/",v_archivo_copia CLIPPED 

    --DISPLAY "v_cmd -- ",v_cmd

    RUN v_cmd

    
    --Cambia el formato del archivo a DOS
    LET v_comando_dos = "unix2dos ",p_ruta_destino CLIPPED, " ", v_archivo_copia CLIPPED
    --DISPLAY "v_comando_dos ",v_comando_dos 
    RUN v_comando_dos

    DISPLAY "     Se ha realizado la copia de la interface de Entidades Financieras: ",v_archivo_copia
END FUNCTION 
