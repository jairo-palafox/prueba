--===============================================================
-- Version: 1.0.0
--===============================================================

#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETC441                                                                #
#Objetivo     => Consulta Revolvente SIAFF                                              #
#Fecha inicio => JULIO 11, 2017                                                         # 
#########################################################################################

DATABASE safre_viv

GLOBALS "RETG01.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
	DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
          ,v_ventana                ui.WINDOW
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   --Se inicia el log del programa
   CALL STARTLOG (p_usuario_cod CLIPPED||".RETC441.log")
   
   -- consulta de informacion recibida 
   CALL fn_consulta_detalle(p_usuario_cod)

END MAIN

{ ============================================================================
Clave: RETC441
Nombre: fn_consulta_detalle
Fecha creacion: JULIO 11, 2017
Registro de modificaciones:
Descrip: Consulta Cifras Globales del Control del Saldo de la Subcuenta TESOFE
==============================================================================
}
FUNCTION fn_consulta_detalle(p_usuario_cod)
DEFINE 
      p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario
      v_folio          DECIMAL(9,0), -- folio
      v_archivo        LIKE glo_ctr_archivo.nombre_archivo,
      v_contador       INTEGER,
      v_cbx_folios     ui.ComboBox, -- combo de folios
      cmb_folios       SMALLINT,
      v_resultado      SMALLINT,


      v_imp_pagados_siaff          DECIMAL(22,2),
      v_nss_pagados_siaff          DECIMAL(11,0),
      v_imp_pagados_x_recuperar    DECIMAL(22,2),
      v_nss_pagados_x_recuperar    DECIMAL(11,0),
      v_imp_pagados_y_recuperados  DECIMAL(22,2),
      v_nss_pagados_y_recuperados  DECIMAL(11,0),
      v_imp_recuperados_sin_pago   DECIMAL(22,2),
      v_nss_recuperados_sin_pago   DECIMAL(11,0),
      v_imp_sin_pago               DECIMAL(22,2),
      v_nss_sin_pago               DECIMAL(11,0),
      v_imp_totales                DECIMAL(22,2),
      v_nss_totales                DECIMAL(11,0),
      v_imp_gt                     DECIMAL(22,2),
      v_nss_gt                     DECIMAL(11,0),
      v_imp_contracargo            DECIMAL(22,2),
      v_nss_contracargo            DECIMAL(11,0),
      v_estado_archivo             CHAR(8),
      v_r_grupo   DYNAMIC ARRAY OF RECORD -- registro de despliegue del agrupador
         grupo            LIKE glo_folio.folio,
         aivs             DECIMAL(18,6),
         pesos            DECIMAL(14,2)
      END RECORD,

      v_r_grupo_indiv   RECORD -- registro de despliegue del agrupador
         grupo            LIKE glo_folio.folio,
         aivs             DECIMAL(18,6),
         pesos            DECIMAL(14,2)
      END RECORD,

      v_arr_reg_detalle         RECORD
         ed_tot_reg               INTEGER                   ,
         ed_pesos_debita          DECIMAL(14,2)                                 ,
         ed_aivs_debita           DECIMAL(18,6),
         ed_total_pagos           INTEGER, 
         ed_sin_dif               INTEGER,
         ed_fch_carga             DATE,
         ed_aivs_viv_97           DECIMAL(18,6),
         ed_aivs_viv_92           DECIMAL(18,6),
         ed_pesos_viv_97          DECIMAL(14,2),
         ed_pesos_viv_92          DECIMAL(14,2),
         ed_nss_no_loc            INTEGER,
         ed_pesos_nss_no_loc      DECIMAL(14,2),
         ed_aivs_nss_no_loc       DECIMAL(18,6)
       END RECORD,
       v_query                       STRING, -- detalle
       v_indice                      SMALLINT, -- indice de arreglo
       v_precio_fondo                LIKE ret_cza_transferencia.precio_fondo

       
   OPEN WINDOW w_consulta_saldos WITH FORM "RETC4411" 
   LET  v_ventana = UI.WINDOW.GETCURRENT()
   CALL v_ventana.SETTEXT("Consulta Cifras Globales del Control del Saldo de la Subcuenta TESOFE")

   LET v_imp_pagados_siaff          = 0
   LET v_nss_pagados_siaff          = 0
   LET v_imp_pagados_x_recuperar    = 0
   LET v_nss_pagados_x_recuperar    = 0
   LET v_imp_pagados_y_recuperados  = 0
   LET v_nss_pagados_y_recuperados  = 0
   LET v_imp_recuperados_sin_pago   = 0
   LET v_nss_recuperados_sin_pago   = 0
   LET v_imp_sin_pago               = 0
   LET v_nss_sin_pago               = 0
   LET v_imp_totales                = 0
   LET v_nss_totales                = 0
   LET v_imp_gt                     = 0
   LET v_nss_gt                     = 0
   LET v_imp_contracargo            = 0
   LET v_nss_contracargo            = 0
   LET v_estado_archivo             = "GENERADO"

--        DISPLAY "variables capturadas"
--        DISPLAY "@..", v_folio, v_afore, v_nss, v_f_inicial, v_f_final
  MENU 



   BEFORE MENU  
   
      -- Se obtiene el total de nss y monto del A
      SELECT SUM(pesos_viv97_transf+ pesos_viv97_no_transf)  
      INTO   v_imp_gt
      FROM   ret_his_anexo1
      WHERE  estado_solicitud = 50;

      SELECT SUM(monto)
      INTO   v_imp_contracargo
      FROM   ret_his_anexo1_contracargos
      WHERE  estado_solicitud <> 100;

      SELECT COUNT(DISTINCT nss) 
      INTO   v_nss_gt
      FROM   ret_his_anexo1
      WHERE  estado_solicitud = 50;
      

      SELECT COUNT(DISTINCT nss)
      INTO   v_nss_contracargo
      FROM   ret_his_anexo1_contracargos
      WHERE  estado_solicitud <> 100;
      
      LET v_imp_gt = v_imp_gt - v_imp_contracargo
      --LET v_nss_gt = v_nss_gt - v_nss_contracargo

---   Se crea la tabla tem poral con los folios de proceso de Respuesta siaff y cargos ssv siaff

      CALL f_genera_datos()
      
      -- Se obtienen los totales Pagados SIAFF (Cargos Tesofe)
      SELECT SUM(a.monto_acciones) * (-1), COUNT(DISTINCT a.id_derechohabiente)
      INTO   v_imp_pagados_siaff, v_nss_pagados_siaff
      FROM   tmp_movtos_revolvente a, ret_his_respuesta_siaff b
      WHERE  b.folio = a.folio_liquida
      AND    b.id_solicitud = a.id_referencia;


      -- Se obtienen los pagados por recuperar, los que estan ret_cargos_ssv_siaff y no en ret_revolvente_siaff
      SELECT SUM(b.monto_acciones) * (-1), COUNT(DISTINCT nss)  
      INTO   v_imp_pagados_x_recuperar, v_nss_pagados_x_recuperar 
      FROM   ret_cargos_ssv_siaff a, tmp_movtos_revolvente b
      WHERE  a.folio = b.folio_liquida
      AND    a.id_solicitud = b.id_referencia
      AND    b.subcuenta = 47
      AND    a.estado_solicitud = 300;

      -- Se obtienen los pagados y recuperados
      SELECT SUM(b.monto_acciones) * (-1), COUNT(DISTINCT nss)  
      INTO   v_imp_pagados_y_recuperados, v_nss_pagados_y_recuperados
      FROM   ret_cargos_ssv_siaff a, tmp_movtos_revolvente b
      WHERE  a.folio = b.folio_liquida
      AND    a.id_solicitud = b.id_referencia
      AND    b.subcuenta = 47
      AND    a.estado_solicitud = 301;

      -- Se obtienen los Recuperados sin Pago
      SELECT SUM(importe), COUNT(DISTINCT nss)
      INTO   v_imp_recuperados_sin_pago, v_nss_recuperados_sin_pago
      FROM   ret_revolvente_siaff
      WHERE  estado_solicitud = 302;

      -- se crea la tabla de movimientos de cuentas con saldo en subcuebta 47 > 0
      CALL f_genera_movtos_47() 
      -- Se obtienen los registros con saldo Sin Pago
      SELECT SUM(acciones), COUNT(id_derechohabiente)
      INTO v_imp_sin_pago, v_nss_sin_pago
      FROM tmp_movtos_47;

      IF v_imp_pagados_siaff IS NULL THEN 
         LET v_imp_pagados_siaff = 0
      END IF 
      IF v_imp_pagados_x_recuperar IS NULL THEN 
         LET v_imp_pagados_x_recuperar = 0
      END IF 
      IF v_imp_pagados_y_recuperados IS NULL THEN 
         LET v_imp_pagados_y_recuperados = 0
      END IF 
      IF v_imp_recuperados_sin_pago IS NULL THEN 
         LET v_imp_recuperados_sin_pago = 0
      END IF 
      IF v_imp_sin_pago IS NULL THEN 
         LET v_imp_sin_pago = 0
      END IF 
      IF v_nss_pagados_siaff IS NULL THEN 
         LET v_nss_pagados_siaff = 0
      END IF 
      IF v_nss_pagados_x_recuperar IS NULL THEN 
         LET v_nss_pagados_x_recuperar = 0
      END IF 
      IF v_nss_pagados_y_recuperados IS NULL THEN 
         LET v_nss_pagados_y_recuperados = 0
      END IF 
      IF v_nss_recuperados_sin_pago IS NULL THEN 
         LET v_nss_recuperados_sin_pago = 0
      END IF 
      IF v_nss_sin_pago IS NULL THEN 
         LET v_nss_sin_pago = 0
      END IF 
      
      LET v_imp_totales = v_imp_pagados_siaff + v_imp_pagados_x_recuperar + v_imp_pagados_y_recuperados + v_imp_recuperados_sin_pago + v_imp_sin_pago
      LET v_nss_totales = v_nss_pagados_siaff + v_nss_pagados_x_recuperar + v_nss_pagados_y_recuperados + v_nss_recuperados_sin_pago + v_nss_sin_pago

      DISPLAY "Las cifras a desplegar son :"   
      DISPLAY "                   v_nss_gt > ", v_nss_gt, "<"
      DISPLAY "                   v_imp_gt > ", v_imp_gt, "<"
      DISPLAY "        v_nss_pagados_siaff > ", v_nss_pagados_siaff, "<"
      DISPLAY "        v_imp_pagados_siaff > ", v_imp_pagados_siaff, "<"
      DISPLAY "  v_nss_pagados_x_recuperar > ", v_nss_pagados_x_recuperar, "<"
      DISPLAY "  v_imp_pagados_x_recuperar > ", v_imp_pagados_x_recuperar, "<"
      DISPLAY "v_nss_pagados_y_recuperados > ", v_nss_pagados_y_recuperados, "<"
      DISPLAY "v_imp_pagados_y_recuperados > ", v_imp_pagados_y_recuperados, "<"
      DISPLAY " v_nss_recuperados_sin_pago > ", v_nss_recuperados_sin_pago, "<"
      DISPLAY " v_imp_recuperados_sin_pago > ", v_imp_recuperados_sin_pago, "<"
      DISPLAY "             v_nss_sin_pago > ", v_nss_sin_pago, "<"
      DISPLAY "             v_imp_sin_pago > ", v_imp_sin_pago, "<"
      DISPLAY "              v_nss_totales > ", v_nss_totales, "<"
      DISPLAY "              v_imp_totales > ", v_imp_totales, "<"
      
      DISPLAY v_nss_gt TO ed_nss_gt
      DISPLAY v_imp_gt TO ed_imp_gt
      DISPLAY v_nss_pagados_siaff TO ed_nss_pagados_siaff
      DISPLAY v_imp_pagados_siaff TO ed_imp_pagados_siaff
      DISPLAY v_nss_pagados_x_recuperar TO ed_nss_pagados_x_recuperar
      DISPLAY v_imp_pagados_x_recuperar TO ed_imp_pagados_x_recuperar
      DISPLAY v_nss_pagados_y_recuperados TO ed_nss_pagados_y_recuperados
      DISPLAY v_imp_pagados_y_recuperados TO ed_imp_pagados_y_recuperados
      DISPLAY v_nss_recuperados_sin_pago TO ed_nss_recuperados_sin_pago
      DISPLAY v_imp_recuperados_sin_pago TO ed_imp_recuperados_sin_pago
      DISPLAY v_nss_sin_pago TO ed_nss_sin_pago
      DISPLAY v_imp_sin_pago TO ed_imp_sin_pago
      DISPLAY v_nss_totales TO ed_nss_totales
      DISPLAY v_imp_totales TO ed_imp_totales

   ON ACTION Salir
      EXIT MENU  
   ON ACTION btn_pagados_siaff
      CALL fn_exporta_pagados_siaff() RETURNING v_resultado
      DISPLAY v_estado_archivo TO ed_edo_pagados_siaff
   ON ACTION btn_pagados_x_recuperar
      CALL fn_exporta_pagados_x_recuperar() RETURNING v_resultado
      DISPLAY v_estado_archivo TO ed_edo_pagados_x_recuperar
   ON ACTION btn_pagados_y_recuperados
      CALL fn_exporta_pagados_y_recuperados() RETURNING v_resultado
      DISPLAY v_estado_archivo TO ed_edo_pagados_y_recuperados
   ON ACTION btn_recuperados_sin_pago
      CALL fn_exporta_recuperados_sin_pago() RETURNING v_resultado
      DISPLAY v_estado_archivo TO ed_edo_recuperados_sin_pago
   ON ACTION btn_sin_pago
      CALL fn_exporta_sin_pago() RETURNING v_resultado
      DISPLAY v_estado_archivo TO ed_edo_sin_pago
   END MENU  
   CLOSE WINDOW w_consulta_saldos

END FUNCTION

---------------------------------------------------------------------
--  Funcion que exporta los registros de Pagados SIAFF
---------------------------------------------------------------------

FUNCTION fn_exporta_pagados_siaff()
DEFINE p_detalle_exp      RECORD
         folio               DECIMAL(9,0),
         f_liquida           DATE,
         num_registros       INTEGER,
         importe             DECIMAL(22,2)
      END RECORD
   DEFINE
      v_c_ruta_env            LIKE seg_modulo.ruta_envio,
      v_extension_txt         STRING,
      v_nom_archivo           STRING,
      v_archivo_txt           STRING,
      v_v_ruta_nomarch        STRING,
      v_mensaje_archivo       STRING,
      v_s_detalle             STRING,
      v_ch_arch_ret_generico  BASE.CHANNEL,  -- manejador de apuntador hacia archivo
      v_hora                  CHAR(8),
      v_regresa               SMALLINT,
      v_query                 STRING

   LET v_regresa = FALSE
   
   -- se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio
   INTO   v_c_ruta_env 
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- las extensiones del archivo son csv para el detalle
   LET v_extension_txt = ".csv"
   LET v_hora = CURRENT HOUR TO SECOND

   -- Se genera el nombre del archivo 
   LET v_nom_archivo = "Pagados_SIAFF_", TODAY USING "yyyymmdd"
   LET v_archivo_txt = v_nom_archivo CLIPPED, v_extension_txt

   -- El archivo con ruta destino que contiene el detalle de Rechazados
   LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se muestra mensaje
   DISPLAY "Se generará el siguiente archivo: ", v_v_ruta_nomarch
   LET v_mensaje_archivo = "Se generará el siguiente archivo:\n\n\t", v_v_ruta_nomarch
   
   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
   -- nombre de archivo generado

   -- Se crea el manejador de archivo
   LET v_ch_arch_ret_generico = base.Channel.create()
   CALL v_ch_arch_ret_generico.setDelimiter(NULL)

   -- Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )
   -- Escribe el encabezado del archivo de Rechazados
   LET v_s_detalle = "FOLIO SACI|FECHA DE LIQUIDACIÓN|NUMERO DE REGISTROS|IMPORTE DE CARGO"

   CALL v_ch_arch_ret_generico.write(v_s_detalle)

   -- Ahora imprimo todos los registros Rechazados
   LET v_query = " SELECT                       ",
                 "        DISTINCT folio        ",
                 "   FROM ret_his_respuesta_siaff ",
                 "  WHERE  estado_solicitud = 50 "

   -- se llena el arreglo 
   PREPARE s_pagados_siaff FROM v_query
   DECLARE cur_pagados_siaff CURSOR FOR s_pagados_siaff

   -- se mandan al archivo los registros rechazados
   FOREACH cur_pagados_siaff INTO p_detalle_exp.folio
         --- Obtiene la información para cada folio encontrado
         SELECT NVL(SUM(importe),0), NVL(COUNT(DISTINCT nss),0)
         INTO   p_detalle_exp.importe, p_detalle_exp.num_registros
         FROM   ret_his_respuesta_siaff
         WHERE  folio = p_detalle_exp.folio
         AND    estado_solicitud = 50;
         SELECT MAX(f_liquida)
         INTO   p_detalle_exp.f_liquida
         FROM   ret_preliquida
         WHERE  folio_liquida = p_detalle_exp.folio;
         LET v_s_detalle = p_detalle_exp.folio,"|",
                           p_detalle_exp.f_liquida USING "dd-mm-yyyy","|",
                           p_detalle_exp.num_registros,"|",
                           p_detalle_exp.importe USING "&&&&&&&&&&&&&&&&&&.&&"
                           
         CALL v_ch_arch_ret_generico.write(v_s_detalle)
         --LET v_contador = v_contador + 1
   END FOREACH

   -- Se cierra el archivo de Rechazados
   CALL v_ch_arch_ret_generico.close()
   DISPLAY "El archivo se generó exitosamente: ", v_v_ruta_nomarch
   LET v_mensaje_archivo = "El archivo se generó exitosamente:\n\n\t", v_v_ruta_nomarch

   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")

   RETURN v_regresa
   
END FUNCTION

---------------------------------------------------------------------
--  Funcion que exporta los registros de Pagados por Recuperar
---------------------------------------------------------------------

FUNCTION fn_exporta_pagados_x_recuperar()
DEFINE p_detalle_exp      RECORD
         nss              CHAR(11),
         fch_contable     DATE,
         importe          DECIMAL(22,2),
         grupo            SMALLINT,
         importe_tesofe   DECIMAL(22,2)
      END RECORD
   DEFINE
      v_c_ruta_env            LIKE seg_modulo.ruta_envio,
      v_extension_txt         STRING,
      v_nom_archivo           STRING,
      v_archivo_txt           STRING,
      v_v_ruta_nomarch        STRING,
      v_mensaje_archivo       STRING,
      v_s_detalle             STRING,
      v_ch_arch_ret_generico  BASE.CHANNEL,  -- manejador de apuntador hacia archivo
      v_hora                  CHAR(8),
      v_regresa               SMALLINT,
      v_query                 STRING

   LET v_regresa = FALSE
   
   -- se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio
   INTO   v_c_ruta_env 
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- las extensiones del archivo son csv para el detalle
   LET v_extension_txt = ".csv"
   LET v_hora = CURRENT HOUR TO SECOND

   -- Se genera el nombre del archivo 
   LET v_nom_archivo = "Pagados_Por_Recuperar_", TODAY USING "yyyymmdd"
   LET v_archivo_txt = v_nom_archivo CLIPPED, v_extension_txt

   -- El archivo con ruta destino que contiene el detalle de Rechazados
   LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se muestra mensaje
   DISPLAY "Se generará el siguiente archivo: ", v_v_ruta_nomarch
   LET v_mensaje_archivo = "Se generará el siguiente archivo:\n\n\t", v_v_ruta_nomarch
   
   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
   -- nombre de archivo generado

   -- Se crea el manejador de archivo
   LET v_ch_arch_ret_generico = base.Channel.create()
   CALL v_ch_arch_ret_generico.setDelimiter(NULL)

   -- Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )
   -- Escribe el encabezado del archivo de Rechazados
   LET v_s_detalle = "NSS|FECHA DE PAGO|IMPORTE|GRUPO|IMPORTE TESOFE"

   CALL v_ch_arch_ret_generico.write(v_s_detalle)

   LET v_query = " SELECT                                           ",
                 "        a.nss, a.fch_contable, a.importe, a.grupo, b.monto_pesos ",
                 "   FROM ret_cargos_ssv_siaff a, tmp_movtos_revolvente b  ",
                 "  WHERE a.folio = b.folio_liquida                 ",
                 "    AND a.id_solicitud = b.id_referencia          ",
                 "    AND b.subcuenta = 47                          ",
                 "    AND a.estado_solicitud = 300                  "

   -- se llena el arreglo 
   PREPARE s_pagados_x_recuperar FROM v_query
   DECLARE cur_pagados_x_recuperar CURSOR FOR s_pagados_x_recuperar

   -- se mandan al archivo los registros rechazados
   FOREACH cur_pagados_x_recuperar INTO p_detalle_exp.*
         LET v_s_detalle = p_detalle_exp.nss USING "&&&&&&&&&&&","|",
                           p_detalle_exp.fch_contable USING "dd-mm-yyyy","|",
                           p_detalle_exp.importe USING "&&&&&&&&&&&&&&&&&&.&&","|",
                           p_detalle_exp.grupo,"|",
                           p_detalle_exp.importe_tesofe USING "&&&&&&&&&&&&&&&&&&.&&"
         CALL v_ch_arch_ret_generico.write(v_s_detalle)
         --LET v_contador = v_contador + 1
   END FOREACH

   -- Se cierra el archivo de Rechazados
   CALL v_ch_arch_ret_generico.close()
   DISPLAY "El archivo se generó exitosamente: ", v_v_ruta_nomarch
   LET v_mensaje_archivo = "El archivo se generó exitosamente:\n\n\t", v_v_ruta_nomarch

   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")

   RETURN v_regresa
   
END FUNCTION

---------------------------------------------------------------------
--  Funcion que exporta los registros de Pagados y Recuperados
---------------------------------------------------------------------

FUNCTION fn_exporta_pagados_y_recuperados()
DEFINE p_detalle_exp      RECORD
         nss              CHAR(11),
         fch_contable     DATE,
         importe          DECIMAL(22,2),
         importe_tesofe   DECIMAL(22,2)
      END RECORD
   DEFINE
      v_c_ruta_env            LIKE seg_modulo.ruta_envio,
      v_extension_txt         STRING,
      v_nom_archivo           STRING,
      v_archivo_txt           STRING,
      v_v_ruta_nomarch        STRING,
      v_mensaje_archivo       STRING,
      v_s_detalle             STRING,
      v_ch_arch_ret_generico  BASE.CHANNEL,  -- manejador de apuntador hacia archivo
      v_hora                  CHAR(8),
      v_regresa               SMALLINT,
      v_query                 STRING

   LET v_regresa = FALSE
   
   -- se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio
   INTO   v_c_ruta_env 
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- las extensiones del archivo son csv para el detalle
   LET v_extension_txt = ".csv"
   LET v_hora = CURRENT HOUR TO SECOND

   -- Se genera el nombre del archivo 
   LET v_nom_archivo = "Pagados_y_Recuperados_", TODAY USING "yyyymmdd"
   LET v_archivo_txt = v_nom_archivo CLIPPED, v_extension_txt

   -- El archivo con ruta destino que contiene el detalle de Rechazados
   LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se muestra mensaje
   DISPLAY "Se generará el siguiente archivo: ", v_v_ruta_nomarch
   LET v_mensaje_archivo = "Se generará el siguiente archivo:\n\n\t", v_v_ruta_nomarch
   
   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
   -- nombre de archivo generado

   -- Se crea el manejador de archivo
   LET v_ch_arch_ret_generico = base.Channel.create()
   CALL v_ch_arch_ret_generico.setDelimiter(NULL)

   -- Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )
   -- Escribe el encabezado del archivo de Rechazados
   LET v_s_detalle = "NSS|FECHA DE PAGO|IMPORTE|IMPORTE TESOFE"

   CALL v_ch_arch_ret_generico.write(v_s_detalle)

--   LET v_query = " SELECT                            ",
--                 "        nss, fch_contable, importe ",
--                 "   FROM ret_cargos_ssv_siaff       ",
--                 "  WHERE estado_solicitud = 301     "
   LET v_query = " SELECT                                           ",
                 "        a.nss, a.fch_contable, a.importe, b.monto_pesos          ",
                 "   FROM ret_cargos_ssv_siaff a, tmp_movtos_revolvente b  ",
                 "  WHERE a.folio = b.folio_liquida                 ",
                 "    AND a.id_solicitud = b.id_referencia          ",
                 "    AND b.subcuenta = 47                          ",
                 "    AND a.estado_solicitud = 301                  "

   -- se llena el arreglo 
   PREPARE s_pagados_y_recuperados FROM v_query
   DECLARE cur_pagados_y_recuperados CURSOR FOR s_pagados_y_recuperados

   -- se mandan al archivo los registros rechazados
   FOREACH cur_pagados_y_recuperados INTO p_detalle_exp.*
         LET v_s_detalle = p_detalle_exp.nss USING "&&&&&&&&&&&","|",
                           p_detalle_exp.fch_contable USING "dd-mm-yyyy","|",
                           p_detalle_exp.importe USING "&&&&&&&&&&&&&&&&&&.&&","|",
                           p_detalle_exp.importe_tesofe USING "&&&&&&&&&&&&&&&&&&.&&"
         CALL v_ch_arch_ret_generico.write(v_s_detalle)
         --LET v_contador = v_contador + 1
   END FOREACH

   -- Se cierra el archivo de Rechazados
   CALL v_ch_arch_ret_generico.close()
   DISPLAY "El archivo se generó exitosamente: ", v_v_ruta_nomarch
   LET v_mensaje_archivo = "El archivo se generó exitosamente:\n\n\t", v_v_ruta_nomarch

   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")

   RETURN v_regresa
   
END FUNCTION

---------------------------------------------------------------------
--  Funcion que exporta los registros Recuperados sin Pago
---------------------------------------------------------------------

FUNCTION fn_exporta_recuperados_sin_pago()
DEFINE p_detalle_exp      RECORD
         nss              CHAR(11),
         importe          DECIMAL(22,2),
         fch_recupera     DATE,
         folio            DECIMAL(11,0)
      END RECORD
   DEFINE
      v_c_ruta_env            LIKE seg_modulo.ruta_envio,
      v_extension_txt         STRING,
      v_nom_archivo           STRING,
      v_archivo_txt           STRING,
      v_v_ruta_nomarch        STRING,
      v_mensaje_archivo       STRING,
      v_s_detalle             STRING,
      v_ch_arch_ret_generico  BASE.CHANNEL,  -- manejador de apuntador hacia archivo
      v_hora                  CHAR(8),
      v_regresa               SMALLINT,
      v_query                 STRING

   LET v_regresa = FALSE
   
   -- se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio
   INTO   v_c_ruta_env 
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- las extensiones del archivo son csv para el detalle
   LET v_extension_txt = ".csv"
   LET v_hora = CURRENT HOUR TO SECOND

   -- Se genera el nombre del archivo 
   LET v_nom_archivo = "Recuperados_sin_Pago_", TODAY USING "yyyymmdd"
   LET v_archivo_txt = v_nom_archivo CLIPPED, v_extension_txt

   -- El archivo con ruta destino que contiene el detalle de Rechazados
   LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se muestra mensaje
   DISPLAY "Se generará el siguiente archivo: ", v_v_ruta_nomarch
   LET v_mensaje_archivo = "Se generará el siguiente archivo:\n\n\t", v_v_ruta_nomarch
   
   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
   -- nombre de archivo generado

   -- Se crea el manejador de archivo
   LET v_ch_arch_ret_generico = base.Channel.create()
   CALL v_ch_arch_ret_generico.setDelimiter(NULL)

   -- Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )
   -- Escribe el encabezado del archivo de Rechazados
   LET v_s_detalle = "NSS|IMPORTE|FECHA DE RECUPERACIÓN|FOLIO DE RECUPERACION SACI"

   CALL v_ch_arch_ret_generico.write(v_s_detalle)

   LET v_query = " SELECT                                          ",
                 "        a.nss, a.importe, b.f_actualiza, b.folio ",
                 "   FROM ret_revolvente_siaff a, glo_folio b      ",
                 "  WHERE a.folio = b.folio                        ",
                 "    AND a.estado_solicitud = 302                 "

   -- se llena el arreglo 
   PREPARE s_recuperados_sin_pago FROM v_query
   DECLARE cur_recuperados_sin_pago CURSOR FOR s_recuperados_sin_pago

   -- se mandan al archivo los registros rechazados
   FOREACH cur_recuperados_sin_pago INTO p_detalle_exp.*
         LET v_s_detalle = p_detalle_exp.nss USING "&&&&&&&&&&&","|",
                           p_detalle_exp.importe USING "&&&&&&&&&&&&&&&&&&.&&", "|",
                           p_detalle_exp.fch_recupera USING "dd-mm-yyyy","|",
                           p_detalle_exp.folio
         CALL v_ch_arch_ret_generico.write(v_s_detalle)
         --LET v_contador = v_contador + 1
   END FOREACH

   -- Se cierra el archivo de Rechazados
   CALL v_ch_arch_ret_generico.close()
   DISPLAY "El archivo se generó exitosamente: ", v_v_ruta_nomarch
   LET v_mensaje_archivo = "El archivo se generó exitosamente:\n\n\t", v_v_ruta_nomarch

   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")

   RETURN v_regresa
   
END FUNCTION

---------------------------------------------------------------------
--  Funcion que exporta los registros sin Pago
---------------------------------------------------------------------

FUNCTION fn_exporta_sin_pago()
DEFINE p_detalle_exp      RECORD
         importe          DECIMAL(22,2),
         nss              CHAR(11)
      END RECORD
   DEFINE
      v_c_ruta_env            LIKE seg_modulo.ruta_envio,
      v_extension_txt         STRING,
      v_nom_archivo           STRING,
      v_archivo_txt           STRING,
      v_v_ruta_nomarch        STRING,
      v_mensaje_archivo       STRING,
      v_s_detalle             STRING,
      v_ch_arch_ret_generico  BASE.CHANNEL,  -- manejador de apuntador hacia archivo
      v_hora                  CHAR(8),
      v_regresa               SMALLINT,
      v_query                 STRING

   LET v_regresa = FALSE
   
   -- se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio
   INTO   v_c_ruta_env 
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- las extensiones del archivo son csv para el detalle
   LET v_extension_txt = ".csv"
   LET v_hora = CURRENT HOUR TO SECOND

   -- Se genera el nombre del archivo 
   LET v_nom_archivo = "Sin_Pago_", TODAY USING "yyyymmdd"
   LET v_archivo_txt = v_nom_archivo CLIPPED, v_extension_txt

   -- El archivo con ruta destino que contiene el detalle de Rechazados
   LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se muestra mensaje
   DISPLAY "Se generará el siguiente archivo: ", v_v_ruta_nomarch
   LET v_mensaje_archivo = "Se generará el siguiente archivo:\n\n\t", v_v_ruta_nomarch
   
   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
   -- nombre de archivo generado

   -- Se crea el manejador de archivo
   LET v_ch_arch_ret_generico = base.Channel.create()
   CALL v_ch_arch_ret_generico.setDelimiter(NULL)

   -- Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )
   -- Escribe el encabezado del archivo de Rechazados
   LET v_s_detalle = "NSS|SALDO DE LA SUBCUENTA TESOFE"

   CALL v_ch_arch_ret_generico.write(v_s_detalle)

   LET v_query = " SELECT                                             ",
                 "        SUM(a.acciones), b.nss                      ",
                 "   FROM tmp_movtos_47 a, afi_derechohabiente b      ",
                 "  WHERE a.id_derechohabiente = b.id_derechohabiente ",
            --     "    AND a.subcuenta = 47                          ",
                 "  GROUP BY b.nss                                    ",
                 "  HAVING SUM(a.acciones) > 0                        "

   -- se llena el arreglo 
   PREPARE s_sin_pago FROM v_query
   DECLARE cur_sin_pago CURSOR FOR s_sin_pago

   -- se mandan al archivo los registros rechazados
   FOREACH cur_sin_pago INTO p_detalle_exp.*
         LET v_s_detalle = p_detalle_exp.nss USING "&&&&&&&&&&&", "|",
                           p_detalle_exp.importe USING "&&&&&&&&&&&&&&&&&&.&&"
         CALL v_ch_arch_ret_generico.write(v_s_detalle)
         --LET v_contador = v_contador + 1
   END FOREACH

   -- Se cierra el archivo de Rechazados
   CALL v_ch_arch_ret_generico.close()
   DISPLAY "El archivo se generó exitosamente: ", v_v_ruta_nomarch
   LET v_mensaje_archivo = "El archivo se generó exitosamente:\n\n\t", v_v_ruta_nomarch

   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")

   RETURN v_regresa
   
END FUNCTION

FUNCTION f_genera_datos()

    DEFINE lc_query          STRING
    DEFINE v_query_insert    STRING   

    DEFINE v_tabla          CHAR(20)

    -- -----------------------------------------------------------------------------
   DROP TABLE IF EXISTS tmp_movtos_revolvente;   --- Borramos la tabla temporal
   SELECT * 
   FROM   cta_movimiento 
   WHERE  1 = 0 
   INTO TEMP tmp_movtos_revolvente;     --- Creamos la estructrua de la tabla
   
   LET lc_query = " SELECT tabla              \n ",
                  " FROM   cat_tab_movimiento \n ",
                  " UNION                     \n ",
                  " SELECT 'cta_movimiento'   \n ",
                  " FROM   systables          \n ",
                  " WHERE  tabid = 1             "

   PREPARE prp_tablas FROM lc_query
   DECLARE cur_tablas CURSOR FOR prp_tablas

   FOREACH cur_tablas INTO v_tabla

      LET v_query_insert = " INSERT INTO tmp_movtos_revolvente     \n",
                           " SELECT  a.*                           \n ",
                            " FROM   ", v_tabla CLIPPED , " a,     \n ",
                            "        glo_folio b                   \n ",
                            " WHERE  a.folio_liquida = b.folio     \n ",
                            " AND    b.proceso_cod IN (1569, 1586) \n "

      DISPLAY "El query de los movimientos ", v_query_insert
      PREPARE prp_movimientos FROM v_query_insert
      EXECUTE prp_movimientos 

    END FOREACH

   CREATE INDEX idxtmp_movtos_revolvente_1 ON tmp_movtos_revolvente(folio_liquida);
   CREATE INDEX idxtmp_movtos_revolvente_2 ON tmp_movtos_revolvente(id_referencia);
   CREATE INDEX idxtmp_movtos_revolvente_3 ON tmp_movtos_revolvente(subcuenta);
   
END FUNCTION

FUNCTION f_genera_movtos_47()

    DEFINE lc_query          STRING
    DEFINE v_query_insert    STRING   

    DEFINE v_tabla          CHAR(20)

    -- -----------------------------------------------------------------------------
   DROP TABLE IF EXISTS tmp_movtos_47;   --- Borramos la tabla temporal
   SELECT SUM(monto_acciones) AS acciones, id_derechohabiente
   FROM   cta_movimiento
   WHERE  subcuenta = 47
   GROUP BY id_derechohabiente
   HAVING SUM(monto_acciones) > 0
   INTO TEMP tmp_movtos_47;     --- Creamos la tabla con los movimientos de la subcuenta 47

   CREATE INDEX idxtmp_movtos_47_1 ON tmp_movtos_47(id_derechohabiente);
   
END FUNCTION













