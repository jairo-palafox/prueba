--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 14/06/2012
--===============================================================
################################################################################
#Módulo          => SEP                                                        #
#Programa        => SEPF23                                                     #
#Objetivo        => Programa de Carga de Dictámen                              #
#Fecha Inicio    => Junio 14, 2012                                             #
################################################################################
DATABASE safre_viv
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   SMALLINT,
       p_cad_ventana     STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ventana         ui.Window,
       v_forma           ui.Form,
       v_expediente RECORD
         v_caso_adai  LIKE sep_expediente.caso_adai,
         v_tipo_flujo LIKE sep_cat_tipo_flujo.flujo_desc,
         v_f_captura  LIKE sep_expediente.f_captura,
         v_canal      LIKE sep_cat_canal_recepcion_exp.canal_desc,
         v_id_expediente LIKE sep_expediente.id_expediente,         
         v_acreditado LIKE sep_nss_expediente.nss,
         v_trabajador LIKE sep_nss_expediente.nss
       END RECORD,
       v_ruta_docto      LIKE seg_modulo.ruta_docto

DEFINE v_diag_rechazo        CHAR(003)  ,
       v_result_operacion    CHAR(002) 

       #Este arhchivo tiene definido el tipo de variables con los cuales se trabajara el cliente de WS
GLOBALS "SEPW05.inc"

MAIN

   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   CALL fn_carga_dicatmen()
END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF21                                                   #
#Descripcion       => Solicita Numero de Caso Adai                             #
#Autor             => Jesús David Yáñez Moreno                                 #
#Fecha inicio      => 26 Julio 2012                                            #
################################################################################

FUNCTION fn_carga_dicatmen()
DEFINE v_continuar BOOLEAN,
       r_confirma  BOOLEAN,
       r_error     BOOLEAN,
       v_diag           CHAR(003)

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "sep"

   SELECT ruta_docto
     INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = 'sep'

   OPEN WINDOW vtna_carag_dictamen WITH FORM v_ruta_ejecutable CLIPPED||"/SEPW221"
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)         
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      # oculta grupos de datos exoediente y documento
      CALL v_forma.setElementHidden("gpo_expediente",1)
      CALL v_forma.setElementHidden("gpo_dicatamen",1)
      CALL v_forma.setElementHidden("btn_quitar",1)
      CALL fn_busca_expediente() RETURNING v_continuar
      IF(v_continuar)THEN
         # si recupewró infromacion, muestra detalle
         CALL v_forma.setElementHidden("gpo_expediente",0)
         CALL v_forma.setElementHidden("gpo_dicatamen",0)
         
         --INPUT ARRAY v_documento FROM sr_documento.* 
           --INPUT  v_expediente.v_caso_adai FROM  flbl_caso_expediente
           
           --INPUT  v_diag FROM  flbl_diag
           -- ATTRIBUTES(WITHOUT DEFAULTS, UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE )
           MENU " "

            BEFORE MENU
            --BEFORE INPUT
               DISPLAY v_expediente.v_caso_adai  TO flbl_caso_expediente
               DISPLAY v_expediente.v_tipo_flujo TO flbl_tipo_flujo_expediente
               DISPLAY v_expediente.v_f_captura  TO flbl_fecha_captura_expediente
               DISPLAY v_expediente.v_canal      TO flbl_origen_expediente
               DISPLAY v_expediente.v_acreditado TO flbl_acreditado_expediente
               DISPLAY v_expediente.v_trabajador TO flbl_trabajador_expediente
               --IF(v_documento[1].v_nom_documento IS NOT NULL)THEN
                  ----CALL v_forma.setElementHidden("btn_quitar",0)
               --END IF

            ON ACTION aceptar

               MESSAGE "Conectando con Adai" ATTRIBUTE (REVERSE) 
               # se omite. Aún no esta implementado en INFONAVIT
               {CALL fn_envia_expediente_adai(v_expediente.v_acreditado, 
                                            v_expediente.v_trabajador ,
                                            v_expediente.v_caso_adai )
               RETURNING v_diag_rechazo      ,
                         v_result_operacion}
               LET v_diag_rechazo     = "000"
               LET v_result_operacion = "01"

               MESSAGE "Desconectando Adai" ATTRIBUTE (REVERSE) 
               LET v_diag = v_diag_rechazo 
               IF v_diag = "000" THEN 
                  DISPLAY "RECIBIDO" TO flbl_diag
               ELSE 
                  DISPLAY v_diag TO flbl_diag
               END IF            
                         
               IF v_result_operacion = "01" THEN 
               
                  -- Avanza maquinaria de estado
                  CALL fn_actualiza_maquinaria(v_expediente.v_id_expediente)
                  RETURNING r_error
                  IF(r_error)THEN                        
                     CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al actualizar estado del Expediente","information")
                     --CONTINUE INPUT
                     CONTINUE MENU 
                  END IF   
                  CALL fn_mensaje(p_cad_ventana,"Expediente Recibido Adai","information")
                  --EXIT INPUT
                  EXIT MENU
                  
               ELSE
                     CALL fn_mensaje(p_cad_ventana,"Ocurrió un error en WS Adai, Código:"||v_result_operacion,"information")
                     --CONTINUE INPUT
                     CONTINUE MENU
               END IF               
                         
          --ON ACTION confirmar
            ON ACTION cancelar            
               IF(v_expediente.v_caso_adai IS NULL OR 
                  v_expediente.v_caso_adai = '' ) THEN
                  CALL fn_ventana_confirma(p_cad_ventana,"Desea salir sin registrar archivo?","question") 
                      RETURNING r_confirma
                  IF(r_confirma)THEN
                     --EXIT INPUT
                     EXIT MENU
                  ELSE               
                     --CONTINUE INPUT
                     CONTINUE MENU
                  END IF
               ELSE
                  --EXIT INPUT
                  EXIT MENU
               END IF
               
         --END INPUT
         END MENU
         
      END IF
   CLOSE WINDOW vtna_carag_dictamen
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF23                                                   #
#Descripcion       => Carga de archivo Dictámen                                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 14 Junio 2012                                            #
################################################################################
FUNCTION fn_busca_expediente()
DEFINE v_filtro    STRING,
       v_continuar BOOLEAN,
       v_bnd_con   BOOLEAN,
       v_caso_adai LIKE sep_expediente.caso_adai,
       v_id_expediente LIKE sep_expediente.id_expediente

   LET v_continuar = FALSE
   LET v_bnd_con = TRUE
   LET v_filtro = " 1 = 2 "
   WHILE(v_bnd_con)
      CONSTRUCT v_filtro ON exp.caso_adai, exp.id_expediente FROM edi_caso_adai,edi_id_expediente
         ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

         ON ACTION aceptar
            INITIALIZE v_caso_adai, v_id_expediente TO NULL 
            CALL GET_FLDBUF(edi_caso_adai) RETURNING v_caso_adai
            CALL GET_FLDBUF(edi_id_expediente) RETURNING v_id_expediente
            IF(v_caso_adai IS NULL AND v_id_expediente IS NULL)THEN
               CALL fn_mensaje(p_cad_ventana,"Al menos debe capturar un campo","information")
               CONTINUE CONSTRUCT
            END IF
            LET v_bnd_con = TRUE
            EXIT CONSTRUCT

         ON ACTION cancelar
            LET v_bnd_con = FALSE
            EXIT CONSTRUCT

      END CONSTRUCT
      
      IF(v_bnd_con)THEN
         CALL fn_recupera_expediente(v_filtro) RETURNING v_continuar
         # si se recuperó información termina ciclo
         IF(v_continuar)THEN
            LET v_bnd_con = FALSE
         ELSE
            CALL fn_mensaje(p_cad_ventana,"Expediente no encontrado","information")
            LET v_bnd_con = TRUE
         END IF
      END IF
   END WHILE

   RETURN v_continuar
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF23                                                   #
#Descripcion       => Carga de archivo Dictámen                                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 14 Junio 2012                                            #
################################################################################
FUNCTION fn_recupera_expediente(v_filtro)
DEFINE v_filtro   STRING,
       v_consulta STRING,
       v_expediente_aux RECORD
         v_caso_adai     LIKE sep_expediente.caso_adai,
         v_tipo_flujo    LIKE sep_cat_tipo_flujo.flujo_desc,
         v_f_captura     DATE,--LIKE sep_expediente.f_captura,
         v_canal         LIKE sep_cat_canal_recepcion_exp.canal_desc,
         v_id_expediente LIKE sep_expediente.id_expediente
       END RECORD,
       v_continua        BOOLEAN

   WHENEVER ERROR CONTINUE
   INITIALIZE v_expediente_aux.* TO NULL
   LET v_continua = FALSE 
   # Recupera el expediente
   LET v_consulta = "\n SELECT exp.caso_adai, flo.flujo_desc, ",
                    "\n        exp.f_captura, can.canal_desc, exp.id_expediente",
                    "\n   FROM sep_expediente exp LEFT OUTER JOIN sep_cat_tipo_flujo flo",
                    "\n     ON exp.flujo_cod = flo.flujo_cod",
                    "\n        LEFT OUTER JOIN sep_cat_canal_recepcion_exp can",
                    "\n     ON exp.canal_cod = can.canal_cod",
                    "\n  WHERE exp.estado = 30 ", # Caso adai solicitado
                    "\n    AND ",v_filtro
   
   PREPARE prp_recupera_expediente FROM v_consulta
   EXECUTE prp_recupera_expediente INTO v_expediente_aux.*
   IF(v_expediente_aux.v_id_expediente IS NOT NULL)THEN
      LET v_continua = TRUE
      LET v_expediente.v_caso_adai  = v_expediente_aux.v_caso_adai      
      LET v_expediente.v_canal      = v_expediente_aux.v_canal
      LET v_expediente.v_tipo_flujo = v_expediente_aux.v_tipo_flujo
      LET v_expediente.v_f_captura  = v_expediente_aux.v_f_captura USING "mm-dd-yyyy"
      LET v_expediente.v_canal      = v_expediente_aux.v_canal
      LET v_expediente.v_id_expediente      = v_expediente_aux.v_id_expediente

      # Recupera nss invadido del expediente 
      SELECT nss
        INTO v_expediente.v_acreditado
        FROM sep_nss_expediente
       WHERE id_expediente = v_expediente_aux.v_id_expediente
         AND tipo_nss = 1 # Invadido

      # Recupera nss asociado del expediente 
      SELECT nss
        INTO v_expediente.v_trabajador
        FROM sep_nss_expediente
       WHERE id_expediente = v_expediente_aux.v_id_expediente
         AND tipo_nss = 2 # Asociado
   END IF

   RETURN v_continua
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF23                                                   #
#Descripcion       => Actualiza el maquinaria                                  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 14 Junio 2012                                            #
################################################################################
FUNCTION fn_actualiza_maquinaria(p_id_expediente)
DEFINE v_consulta       STRING,
       v_error          BOOLEAN,
       v_ind            SMALLINT,       
       v_diag           CHAR(3),
       v_estado_destino SMALLINT,
       v_maquinaria     CHAR(40),
       v_senial         SMALLINT,
       p_id_expediente  LIKE sep_expediente.id_expediente,
       v_campo_id_ent   CHAR(40)

   WHENEVER ERROR CONTINUE
   LET v_error = FALSE
   LET v_maquinaria = "maq_sep_expediente"
   LET v_senial = 45  # recibir caso
   LET v_campo_id_ent = "id_expediente"
       
   LET v_consulta = "EXECUTE FUNCTION fn_maquinaria_individual(?,?,?,?,?)"
   PREPARE prp_recupera_edo_maq FROM v_consulta
   EXECUTE prp_recupera_edo_maq USING v_maquinaria,
                                      p_id_expediente,
                                      v_campo_id_ent,
                                      v_senial,
                                      p_usuario_cod
                                  INTO v_ind,v_diag,v_estado_destino 
   -- maq_sep_expediente = registro de cargas de dictamen
   IF(v_ind <> 0)THEN
      LET v_error = TRUE
   END IF

   RETURN v_error
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF23                                                   #
#Descripcion       => Recupera numero de caso                                  #
#Autor             => Jesus David Yañez Moreno                                 #
#Fecha inicio      => 25 Julio 2012                                            #
################################################################################
FUNCTION fn_envia_expediente_adai(v_nss_invadido, v_nss_asociado,v_numero_caso )

   DEFINE v_nss_invadido     CHAR(011)
   DEFINE v_nss_asociado     CHAR(011)
   DEFINE v_numero_caso      INTEGER   
   
   DEFINE v_parametros datosExpediente             #Variable para los parametros que se enviaran al WS
   DEFINE v_respuesta  respuestaDatosExpediente    #Variable para "cachar" la respuesta del WS 
   
   #Se llenan los parametros a enviar
   
   LET v_parametros.nssAsociado         = v_nss_asociado
   LET v_parametros.nssInvadido         = v_nss_invadido
   LET v_parametros.numeroCaso          = v_numero_caso
   LET v_parametros.nombreReclamante    = "NOMBRE"
   LET v_parametros.folioProcesar       = "1"
   LET v_parametros.tipoFlujo           = 1
   LET v_parametros.fRecepcion          = TODAY
   LET v_parametros.fCaptura            = TODAY

   #Se ejecuta la funcion que invoca al WS y se guarda la respuesta en la variable v_respuesta.*
   #NOTA: Esta funcion se ejecuta por cada registro
   CALL fn_envia_datos_expediente(v_parametros.*) RETURNING v_respuesta.*

   #En caso de que se presente algun problema con el WS la funcion regresara los campos:
      #  v_respuesta.diagRechazo       = "-1"
      #  v_respuesta.resultOperacion   = "-1"
   #ademas de guardar un registro en la tabla wsv_his_err_cliente
   
   RETURN v_respuesta.diagRechazo     , 
          v_respuesta.resultOperacion

END FUNCTION