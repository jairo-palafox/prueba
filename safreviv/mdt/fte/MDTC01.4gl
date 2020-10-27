--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 08-04-2012
--==============================================================================
################################################################################
#Modulo       => MDT                                                           #
#Programa     => MDTC01                                                        #
#Objetivo     =>                                                               #
#Fecha inicio => Febrero 07, 2012                                              #
################################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid,     # ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod, # Código de operacion
       g_ruta_bin    LIKE seg_modulo.ruta_bin,
       v_ventana           ui.Window
       
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion  INTEGER,
       p_cad_ventana    STRING
   {
    Se recuperan los parametros recibidos
    Clave de usuario
    Tipo de ejecucion (en línea o batch)
    Cadena que identifica al programa (lo que aparecería como título de la ventana)
   }
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   SELECT ruta_bin
     INTO g_ruta_bin
     FROM seg_modulo
    WHERE modulo_cod = "mdt" 

   CALL STARTLOG(p_usuario_cod CLIPPED||".MDTC01.log")

   CALL fn_consulta_instrucciones_mandato(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)
END MAIN

{ ==========================================================================
Clave:  fn_consulta_instrucciones_mandato
Nombre: fn_consulta_instrucciones_mandato
Fecha creacion: 07 de Febrero de 2012
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 función para la consulta de las instrucciones de mandatos
 Parametros de Entrada:
  -  p_usuario_cod    --> codigo de usuario logeado
     p_tpo_ejecucion  --> tipo de ejecucion 1 - en linea, 2 - batch 
     p_cad_ventana    --> cadena a mostrar en el titulo de la ventana
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_consulta_instrucciones_mandato(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)
DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion     INTEGER,
       p_cad_ventana       STRING,
       v_derechohabiente   RECORD
          v_nss            LIKE afi_derechohabiente.nss,
          v_paterno        LIKE afi_derechohabiente.ap_paterno_af,
          v_materno        LIKE afi_derechohabiente.ap_materno_af,
          v_nombres        LIKE afi_derechohabiente.nombre_af
       END RECORD,
       v_det_derechohabientes DYNAMIC ARRAY OF RECORD
          v_consecutivo    INTEGER,
          v_id_ctr_mandato LIKE mdt_ctr_mandato.id_ctr_mandato,
          v_nss            LIKE afi_derechohabiente.nss,
          v_credito        LIKE mdt_ctr_mandato.id_credito,
          v_paterno        CHAR(120),
       --   v_materno        LIKE afi_derechohabiente.ap_materno_af,
       --   v_nombres        LIKE afi_derechohabiente.nombre_af,
          v_consulta       CHAR
       END RECORD,
       v_consulta          STRING,
       v_filtro            STRING,
       v_indice            INTEGER,
       v_bnd_continua      BOOLEAN

   LET v_bnd_continua =  TRUE
   OPEN WINDOW w_consultaInstruccionesMandato WITH FORM g_ruta_bin CLIPPED||"/MDTC011"

      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_cad_ventana)
      END IF

      WHILE v_bnd_continua 
         # Se realiza la validacion de la busqueda de informacion
         INPUT v_derechohabiente.* WITHOUT DEFAULTS FROM edi_nss, edi_paterno, edi_materno, edi_nombres
                                   ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
            BEFORE INPUT
               #Se asigna el titulo de la ventana
               IF ( p_cad_ventana IS NOT NULL ) THEN
                  CALL ui.Interface.setText(p_cad_ventana)
               END IF
               INITIALIZE v_derechohabiente.*, v_filtro, v_consulta TO NULL

            ON ACTION aceptar
               # Se hacen validaciones si NSS is nulo
               IF(v_derechohabiente.v_nss IS NULL)THEN
                  # No se ha capturado ningun parámetro
                  IF(v_derechohabiente.v_paterno IS NULL AND v_derechohabiente.v_materno IS NULL AND 
                     v_derechohabiente.v_nombres IS NULL)THEN
                     ERROR "Debe indicar algún parámetro de búsqueda" ATTRIBUTE(REVERSE, BOLD, YELLOW)
                     NEXT FIELD edi_nss
                  END IF
                  # Si solo ha capturado ap paterno               
                  IF(v_derechohabiente.v_paterno IS NOT NULL AND v_derechohabiente.v_materno IS NULL AND 
                     v_derechohabiente.v_nombres IS NULL)THEN
                     ERROR "Capture algún otro parámetro de búsqueda" ATTRIBUTE(REVERSE, BOLD, YELLOW)
                     NEXT FIELD edi_materno
                  END IF
                  # Si solo ha capturado ap materno               
                  IF(v_derechohabiente.v_paterno IS NULL AND v_derechohabiente.v_materno IS NOT NULL AND 
                     v_derechohabiente.v_nombres IS NULL)THEN
                     ERROR "Capture algún otro parámetro de búsqueda" ATTRIBUTE(REVERSE, BOLD, YELLOW)
                     NEXT FIELD edi_paterno
                  END IF
                  # Si solo ha capturado nombre
                  IF(v_derechohabiente.v_paterno IS NULL AND v_derechohabiente.v_materno IS NULL AND 
                     v_derechohabiente.v_nombres IS NOT NULL)THEN
                     ERROR "Capture algún otro parámetro de búsqueda" ATTRIBUTE(REVERSE, BOLD, YELLOW)
                     NEXT FIELD edi_paterno
                  END IF
               END IF
               # Se recupera el contruct de los datos ingresados
               CALL fn_consulta_afiliado (v_derechohabiente.v_nss, v_derechohabiente.v_paterno, v_derechohabiente.v_materno, 
                                          v_derechohabiente.v_nombres) RETURNING v_filtro
               EXIT INPUT               
                                          
            ON ACTION CLOSE    -- cancelar
               LET v_bnd_continua = FALSE
               EXIT INPUT
   
         END INPUT
         IF(v_bnd_continua)THEN
         --IF(v_bnd_continua = FALSE)THEN
            
         --ELSE
            # Recupera la informacion según los datos capturados
            LET v_consulta = "\n SELECT 1, mdt.id_ctr_mandato, mdt.nss, mdt.id_credito,trim( afi.ap_paterno_af)|| ",
                             "\n        ' '||trim( afi.ap_materno_af)||' '||trim(afi.nombre_af),",
                             "\n        ' '",
                             "\n   FROM afi_derechohabiente afi RIGHT OUTER JOIN mdt_ctr_mandato mdt",
                             "\n     ON afi.id_derechohabiente = mdt.id_derechohabiente",
                             "\n  WHERE ",v_filtro,
                             "\n  ORDER BY 1,3,4,5"
            PREPARE prp_recuperaDerechohabientes FROM v_consulta
            DECLARE cur_recuperaDerechohabientes CURSOR FOR prp_recuperaDerechohabientes
            LET v_indice = 1
            FOREACH cur_recuperaDerechohabientes INTO v_det_derechohabientes[v_indice].*
               LET v_det_derechohabientes[v_indice].v_consecutivo = v_indice
               LET v_indice = v_indice + 1
            END FOREACH
            # Libera cursor
            FREE cur_recuperaDerechohabientes
            # Elimina el ultimo elemento si es nulo
            IF(v_det_derechohabientes[v_det_derechohabientes.getLength()].v_nss IS NULL)THEN
               CALL v_det_derechohabientes.deleteElement(v_det_derechohabientes.getLength())
            END IF
            # Termina si no existe informacion para mostrar
            IF(v_det_derechohabientes.getLength() < 1)THEN
               CALL fn_mensaje(p_cad_ventana,"No se encontró ningún registro con criterio dado","about")
               
            ELSE   
               INPUT ARRAY v_det_derechohabientes WITHOUT DEFAULTS  FROM tbl_instrucciones_derechohabiente.*
                                                  ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE, APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE)
                  BEFORE INPUT
                     DISPLAY v_derechohabiente.v_nss     TO edi_nss
                     DISPLAY v_derechohabiente.v_paterno TO edi_paterno
                     DISPLAY v_derechohabiente.v_materno TO edi_materno
                     DISPLAY v_derechohabiente.v_nombres TO edi_nombres

                  ON ACTION consultar
                     CALL fn_consulta_detalle_mandato(v_det_derechohabientes[ARR_CURR()].*,p_cad_ventana)
                     LET v_bnd_continua = TRUE
                     ACCEPT INPUT

                  ON ACTION cancelar
                     LET v_bnd_continua =  FALSE
                     EXIT INPUT

                  AFTER INPUT
                     CALL v_det_derechohabientes.clear()
                     INITIALIZE v_derechohabiente.* TO NULL

                END INPUT
            END IF
         END IF
      END WHILE
   CLOSE WINDOW w_consultaInstruccionesMandato   
END FUNCTION

{ ==========================================================================
Clave:  fn_consulta_detalle_mandato
Nombre: fn_consulta_detalle_mandato
Fecha creacion: 08 de Febrero de 2012
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 función para la consulta de las instrucciones de mandatos
 Parametros de Entrada:
  -  p_usuario_cod    --> codigo de usuario logeado
     p_tpo_ejecucion  --> tipo de ejecucion 1 - en linea, 2 - batch 
     p_cad_ventana    --> cadena a mostrar en el titulo de la ventana
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_consulta_detalle_mandato(v_det_derechohabientes,p_cad_ventana)
DEFINE p_cad_ventana           STRING,
       v_det_derechohabientes  RECORD   # de mdt_ctr_mandato
          v_consecutivo        INTEGER,
          v_id_ctr_mandato     LIKE mdt_ctr_mandato.id_ctr_mandato,
          v_nss                LIKE afi_derechohabiente.nss,
          v_credito            STRING,--LIKE mdt_ctr_mandato.id_credito,
          v_paterno            LIKE afi_derechohabiente.ap_paterno_af,
         -- v_materno            LIKE afi_derechohabiente.ap_materno_af,
         -- v_nombres            LIKE afi_derechohabiente.nombre_af,
          v_consulta           CHAR
       END RECORD,
       v_mandato               DYNAMIC ARRAY OF RECORD # de mdt_det_ctr_mandato
          v_numero             INTEGER,
          v_id_det_ctr_mandato LIKE mdt_det_ctr_mandato.id_det_ctr_mandato,
          v_mandato            LIKE mdt_cat_mandato.desc_mandato,
          v_fec_inicio         LIKE mdt_det_ctr_mandato.f_inicio_mandato,
          v_fec_culminacion    LIKE mdt_det_ctr_mandato.f_culmina_mandato,
          v_valor_descuento    LIKE mdt_det_ctr_mandato.valor_descuento_mandato,
          v_ref_bancaria       LIKE mdt_det_ctr_mandato.referencia,
          v_fec_presentacion   LIKE mdt_det_ctr_mandato.f_presentacion,
          v_estado             CHAR(20),
          v_usuario            LIKE mdt_ctr_mandato.usuario,
          v_detalle            CHAR
       END RECORD,
       v_consulta            STRING,
       v_indice              INTEGER

   # Recuepra los mandatos del derechohabiente
   LET v_consulta = "\n SELECT 1, det.id_det_ctr_mandato, cat.desc_mandato, det.f_inicio_mandato, ",
                    "\n           det.f_culmina_mandato, det.valor_descuento_mandato,trim(det.referencia), ",
                    "\n           det.f_presentacion,", 
                    "\n CASE WHEN det.estado = 103 THEN 'ACTIVO' " ,
                    "\n      WHEN det.estado = 106 THEN 'INACTIVO' END CASE, " ,
                    "\n           ctr.usuario ",
                    "\n   FROM mdt_ctr_mandato ctr JOIN mdt_det_ctr_mandato det",
                    "\n     ON det.id_ctr_mandato = ctr.id_ctr_mandato",
                    "\n        LEFT OUTER JOIN mdt_cat_mandato cat",
                    "\n     ON cat.id_cat_mandato = det.id_cat_mandato",
                    "\n  WHERE ctr.id_ctr_mandato = ?"
   PREPARE prp_recMandatosDeDerechohabientes FROM v_consulta
   DECLARE cur_recMandatosDeDerechohabientes CURSOR FOR prp_recMandatosDeDerechohabientes
   LET v_indice = 1
   FOREACH cur_recMandatosDeDerechohabientes USING v_det_derechohabientes.v_id_ctr_mandato
                                              INTO v_mandato[v_indice].*
     LET v_mandato[v_indice].v_ref_bancaria = v_mandato[v_indice].v_ref_bancaria CLIPPED                                               
     # se incrementa el numero continuo de los datos recuperados
     LET v_mandato[v_indice].v_numero = v_indice 
     LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recMandatosDeDerechohabientes
   # Elimina el ultimo elemento si es nulo
   IF(v_mandato[v_mandato.getLength()].v_id_det_ctr_mandato IS NULL)THEN
      CALL v_mandato.deleteElement(v_mandato.getLength())
   END IF
   # Termina si no existe informacion para mostrar
   IF(v_mandato.getLength() < 1)THEN
      CALL fn_mensaje(p_cad_ventana,"No se encontró información","about")
      RETURN
      
   END IF
            
   # Muestra el los mandatos del derechohabiente
   OPEN WINDOW w_consultaDetalleMandato WITH FORM g_ruta_bin CLIPPED||"/MDTC012"
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      INPUT ARRAY v_mandato WITHOUT DEFAULTS  FROM tbl_detalle_mandato.*
                              ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE, APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE)

         BEFORE INPUT
            #Se asigna el titulo de la ventana
            IF ( p_cad_ventana IS NOT NULL ) THEN
               CALL ui.Interface.setText(p_cad_ventana)
            END IF
            # priemero despliega los datos del derechohabiente
            DISPLAY v_det_derechohabientes.v_nss     TO flbl_nss
            DISPLAY v_det_derechohabientes.v_credito.trim() TO flbl_credito
            DISPLAY v_det_derechohabientes.v_paterno TO flbl_paterno
            --DISPLAY v_det_derechohabientes.v_materno TO flbl_materno
            ----DISPLAY v_det_derechohabientes.v_nombres TO flbl_nombres
            

         ON ACTION aceptar            
            EXIT INPUT

         ON ACTION historico            
            CALL fn_consulta_historia_mandato(v_mandato[ARR_CURR()].*,p_cad_ventana)

         AFTER INPUT
            CALL v_mandato.clear()

      END INPUT
   CLOSE WINDOW w_consultaDetalleMandato
   
END FUNCTION

{ ==========================================================================
Clave:  fn_consulta_historia_mandato
Nombre: fn_consulta_historia_mandato
Fecha creacion: 08 de Febrero de 2012
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 función para la consulta de las instrucciones de mandatos
 Parametros de Entrada:
  -  p_usuario_cod    --> codigo de usuario logeado
     p_tpo_ejecucion  --> tipo de ejecucion 1 - en linea, 2 - batch 
     p_cad_ventana    --> cadena a mostrar en el titulo de la ventana
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_consulta_historia_mandato(v_mandato,p_cad_ventana)
DEFINE p_cad_ventana           STRING, 
       v_mandato              RECORD # de mdt_det_ctr_mandato
          v_numero             INTEGER,
          v_id_det_ctr_mandato LIKE mdt_det_ctr_mandato.id_det_ctr_mandato,
          v_mandato            LIKE mdt_cat_mandato.desc_mandato,
          v_fec_inicio         LIKE mdt_det_ctr_mandato.f_inicio_mandato,
          v_fec_culminacion    LIKE mdt_det_ctr_mandato.f_culmina_mandato,
          v_valor_descuento    LIKE mdt_det_ctr_mandato.valor_descuento_mandato,
          v_ref_bancaria       LIKE mdt_det_ctr_mandato.referencia,
          v_fec_presentacion   LIKE mdt_det_ctr_mandato.f_presentacion,
          v_estado             CHAR(20),
          v_usuario            LIKE mdt_ctr_mandato.usuario,
          v_detalle            CHAR
       END RECORD,
       v_hist_mandato         DYNAMIC ARRAY OF RECORD
          v_numero             INTEGER,
          v_mandato            LIKE mdt_cat_mandato.desc_mandato,
          v_atributo           LIKE mdt_cat_dato_actualizado.etiqueta,
          v_fec_modificacion   LIKE mdt_his_mandato.f_modificacion,
          v_valor_descuento    LIKE mdt_his_mandato.valor_modificado,
          v_valor_actual       LIKE mdt_his_mandato.valor_actual,
          v_usuario            LIKE mdt_ctr_mandato.usuario
       END RECORD,
       v_consulta              STRING,
       v_indice                INTEGER
   
   # Recuepra el historico de los mandatos del derechohabiente
   LET v_consulta = "\n SELECT 1, cmdt.desc_mandato, cdato.etiqueta,his.f_modificacion,",
                    "\n CASE WHEN his.valor_modificado= '103' THEN 'ACTIVO' ",
                    "\n      WHEN his.valor_modificado= '106' THEN 'BAJA' ",
                    "\n      WHEN his.valor_modificado= ''  THEN 'NULO' ",
                    "\n      ELSE his.valor_modificado ",
                    "\n END CASE ,",
                    "\n CASE WHEN his.valor_actual = '103' THEN 'ACTIVO' ",
                    "\n      WHEN his.valor_actual = '106' THEN 'INACTIVO' ",
                    "\n      WHEN his.valor_actual = ''    THEN 'NULO' ",
                    "\n      ELSE his.valor_actual ",
                    "\n END CASE, ",
                    "\n        sol.usuario",                             
                    "\n   FROM mdt_cat_mandato cmdt JOIN mdt_det_ctr_mandato ctr",
                    "\n     ON cmdt.id_cat_mandato = ctr.id_cat_mandato",
                    "\n        RIGHT OUTER JOIN mdt_his_mandato his",
                    "\n     ON ctr.id_det_ctr_mandato = his.id_det_ctr_mandato",
                    "\n        LEFT OUTER JOIN mdt_cat_dato_actualizado cdato",
                    "\n     ON his.id_cat_dato_actualizado = cdato.id_cat_dato_actualizado",
                    "\n        LEFT OUTER JOIN mdt_solicitud_mandato sol",
                    "\n     ON sol.id_solicitud_mandato = his.id_solicitud_mandato ",
                    "\n  WHERE ctr.id_det_ctr_mandato = ?"
   PREPARE prp_recHistMandatos FROM v_consulta
   DECLARE cur_recHistMandatos CURSOR FOR prp_recHistMandatos
   LET v_indice = 1
   FOREACH cur_recHistMandatos USING v_mandato.v_id_det_ctr_mandato
                                INTO v_hist_mandato[v_indice].*
      # se incrementa el numero continuo de los datos recuperados
      LET v_hist_mandato[v_indice].v_numero = v_indice 
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recHistMandatos
   # Elimina el ultimo elemento si es nulo
   IF(v_hist_mandato[v_hist_mandato.getLength()].v_mandato IS NULL)THEN
      CALL v_hist_mandato.deleteElement(v_hist_mandato.getLength())
   END IF
   # Termina si no existe informacion para mostrar
   IF(v_hist_mandato.getLength() < 1)THEN
      CALL fn_mensaje(p_cad_ventana,"No se encontró información##","about")
      RETURN
   END IF
   
   OPEN WINDOW w_consultaHistoriaMandato WITH FORM g_ruta_bin CLIPPED||"/MDTC013"
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      DISPLAY ARRAY v_hist_mandato TO tbl_historia.*
                                        ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
         BEFORE DISPLAY
            #Se asigna el titulo de la ventana
            IF ( p_cad_ventana IS NOT NULL ) THEN
               CALL ui.Interface.setText(p_cad_ventana)
            END IF
            
         # Sale de la consulta de históricos
         ON ACTION aceptar
            EXIT DISPLAY

         AFTER DISPLAY 
            CALL v_hist_mandato.clear()

      END DISPLAY      
   CLOSE WINDOW w_consultaHistoriaMandato
          
END FUNCTION

{ ==========================================================================
Clave:  fn_consulta_afiliado
Nombre: fn_consulta_afiliado
Fecha creacion: 08 de Febrero de 2012
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 función que consulta derechohabientes
 Parametros de Entrada:
  -  
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_consulta_afiliado(v_nss, v_paterno, v_materno, v_nombres)
DEFINE v_nss          LIKE afi_derechohabiente.nss,
       v_paterno      LIKE afi_derechohabiente.ap_paterno_af,
       v_materno      LIKE afi_derechohabiente.ap_materno_af,
       v_nombres      LIKE afi_derechohabiente.nombre_af,
       v_filtro          STRING 

   CONSTRUCT v_filtro ON mdt.nss, afi.ap_paterno_af, afi.ap_materno_af, afi.nombre_af  
                      FROM edi_nss, edi_paterno, edi_materno, edi_nombres
      BEFORE CONSTRUCT
         DISPLAY v_nss     TO edi_nss
         DISPLAY v_paterno TO edi_paterno
         DISPLAY v_materno TO edi_materno
         DISPLAY v_nombres TO edi_nombres
         ACCEPT CONSTRUCT
      
      AFTER CONSTRUCT

   END CONSTRUCT
   RETURN v_filtro
END FUNCTION