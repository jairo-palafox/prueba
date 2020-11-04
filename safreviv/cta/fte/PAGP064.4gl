{===============================================================================
Proyecto          => SISTEMA DE AFORE( SAFRE )
Propietario       => E.F.P.
Modulo            => PAG
Programa          => PAGP064
Descripcion       => Programa Lanzado Integración Archivo de Patrones
Fecha creacion    => Enero 23, 2020.
================================================================================}
DATABASE safre_viv

GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN

DEFINE  p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod -- codigo del proceso
       ,p_opera_cod            LIKE bat_ctr_operacion.opera_cod -- codigo de la operacion
       ,p_usuario_cod          LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,v_s_sql                STRING -- cadena con una instruccion SQL
       ,v_i_resultado          INTEGER -- resultado del proceso
       ,r_bnd_fin_oper         SMALLINT
       ,v_si_correcto_integra  SMALLINT
       ,p_nombre_archivo       LIKE glo_ctr_archivo.nombre_archivo -- nombre dle archivo
       ,v_folio                LIKE deo_preliquida.folio_liquida
       --
       ,p_titulo               STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje              STRING -- cuerpo del mensaje enviado
       ,v_layout               LIKE cat_operacion.layout_cod
       ,v_ruta_rescate         STRING
       ,v_usuario              LIKE seg_modulo.usuario
       ,v_proceso_desc         LIKE cat_proceso.proceso_desc
       ,v_extension            LIKE cat_operacion.extension
       ,v_opera_desc           LIKE cat_operacion.opera_desc
       ,v_ruta_listados        LIKE seg_modulo.ruta_listados
       ,v_nss                  CHAR(11)  
       ,v_dte_fecha_hoy        DATE
       ,v_fondo                SMALLINT
       ,v_error_sql            INTEGER
       ,v_error_isam           INTEGER
       ,v_mensaje_error        VARCHAR(250)
       ,v_tot_rechazados       INTEGER
       ,v_s_comando            STRING
       ,v_tabla_activa         CHAR(20)
       ,vQryTxt STRING
       ,v_tabla_inserta CHAR (20)


DEFINE g_reg_modulo   RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD

DEFINE arr_detalles DYNAMIC ARRAY OF RECORD
          v_tmp_nrp CHAR(40),
          v_tmp_raz CHAR(160),
          v_tmp_dom CHAR(160),
          v_tmp_loc CHAR(80),
          v_tmp_cod CHAR(10)
END RECORD

DEFINE i INTEGER

   -- se recuperan los parametros la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".PAGP064.log")

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
   INTO   g_reg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'pag'

   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'

   LET v_dte_fecha_hoy = TODAY

   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
        RETURNING v_proceso_desc,
                  v_extension, 
                  v_opera_desc,
                  v_layout, 
                  v_ruta_rescate,
                  v_ruta_listados,
                  v_usuario

   LET v_i_resultado = 0
   LET v_si_correcto_integra = 0

   CALL fn_genera_folio(g_proceso_cod,g_opera_cod,p_usuario_cod)
   RETURNING v_folio

   DISPLAY " ================================= \n"
   DISPLAY "   Folio : ", v_folio

   SELECT tabla
   INTO   v_tabla_activa
   FROM   pag_ctr_patron

   IF v_tabla_activa IS NULL THEN
      INSERT INTO pag_ctr_patron VALUES ("pag_patron_bis")
   END IF

   IF v_tabla_activa = "pag_patron" THEN
      LET v_tabla_inserta = "pag_patron_bis"
   ELSE
      LET v_tabla_inserta = "pag_patron"
   END IF

   LET i = 1
   --LET vQryTxt = "DELETE FROM ", v_tabla_inserta
   LET vQryTxt = "TRUNCATE TABLE ", v_tabla_inserta
   PREPARE prp_delete FROM vQryTxt
   EXECUTE prp_delete

   LET vQryTxt = "INSERT INTO ", v_tabla_inserta CLIPPED, " VALUES (?,?,?,?,?)"

   --Se guarda en tabla alternativa, a la tabla activa
   DECLARE cur_detalles CURSOR FOR SELECT *
                                   FROM safre_tmp:tmp_arch_patrones

   PREPARE prp_insert FROM vQryTxt

   FOREACH cur_detalles INTO arr_detalles[i].*
   EXECUTE prp_insert USING arr_detalles[i].v_tmp_nrp,
                            arr_detalles[i].v_tmp_raz,
                            arr_detalles[i].v_tmp_dom,
                            arr_detalles[i].v_tmp_loc,
                            arr_detalles[i].v_tmp_cod

      LET i = i + 1;
   END FOREACH

   LET vQryTxt = "UPDATE STATISTICS FOR TABLE ", v_tabla_inserta
   PREPARE prp_statistics FROM vQryTxt
   EXECUTE prp_statistics 

   FREE cur_detalles
   CALL arr_detalles.deleteElement(arr_detalles.getLength())

   UPDATE pag_ctr_patron
   SET tabla = v_tabla_inserta;
   
   UPDATE glo_ctr_archivo
   SET estado = 2,
       folio = v_folio
   WHERE nombre_archivo = p_nombre_archivo;

   UPDATE bat_ctr_operacion
   SET    folio = v_folio,
          nom_archivo = p_nombre_archivo
   WHERE  pid = g_pid
   AND    opera_cod = g_opera_cod;

   IF ( SQLCA.SQLCODE = 0 ) THEN
      DISPLAY " "
      DISPLAY "   La integración se terminó completamente."
      DISPLAY " "
      DISPLAY "   Integración realizada con exito"
      DISPLAY " " 
      DISPLAY "   Total de registros: ", i
      DISPLAY " " 
      DISPLAY "   Estatus Resultado :",v_error_sql        

      LET p_mensaje = "La integración se terminó completamente.","\n",
                      "   Integración realizada con exito","\n",
                      "   Folio     : ",v_folio,"\n",
                      "   Estatus   :",SQLCA.SQLCODE,"\n"

      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING r_bnd_fin_oper
   ELSE
      DISPLAY "Ocurrió un error al realizar el proceso de integración."
      DISPLAY "Error (SQL) : ",SQLCA.SQLCODE

      -- se marca la operacion en error
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bnd_fin_oper

      IF ( v_i_resultado <> 100 ) THEN
         DISPLAY "   Error. No se integró ninguna solicitud"
      END IF
      
      LET p_mensaje = " --- ERROR ---\n",
                   " El proceso de Integración no terminó correctamente.\n",
                   " Código de error : ", v_error_sql,"\n ",
                   " FECHA           : ",TODAY,"\n",
                   " HORA            : ",CURRENT HOUR TO SECOND,"\n"           
      DISPLAY "\n"
   END IF

   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - INTEGRACION"
END MAIN