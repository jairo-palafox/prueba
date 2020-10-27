--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 14/06/2012
--===============================================================
################################################################################
#Módulo          => SEP                                                        #
#Programa        => SEPW21                                                     #
#Objetivo        => Programa de Carga de Dictámen                              #
#Fecha Inicio    => Junio 14, 2012                                             #
################################################################################
DATABASE safre_viv
DEFINE g_enter char(1)
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

DEFINE v_diag_rechazo        SMALLINT  ,
       v_result_operacion    CHAR(002) ,
       v_numero_caso         INTEGER        

       #Este arhchivo tiene definido el tipo de variables con los cuales se trabajara el cliente de WS
GLOBALS "SEPW01.inc"

MAIN

   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   CALL fn_carga_dicatmen()
END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPW21                                                   #
#Descripcion       => Solicita Numero de Caso Adai                             #
#Autor             => Jesús David Yáñez Moreno                                 #
#Fecha inicio      => 26 Julio 2012                                            #
################################################################################

FUNCTION fn_carga_dicatmen()
DEFINE v_continuar BOOLEAN,
       r_confirma  BOOLEAN,
       r_error     BOOLEAN,
       v_id_expediente  CHAR, --INTEGER
       v_caso_adai DECIMAL(9,0),
       v_consulta  STRING

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "sep"

   SELECT ruta_docto
     INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = 'sep'

   OPEN WINDOW vtna_carag_dictamen WITH FORM v_ruta_ejecutable CLIPPED||"/SEPW211"
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
         INPUT  v_expediente.v_caso_adai FROM  flbl_caso_expediente       
            ATTRIBUTES(WITHOUT DEFAULTS, UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE )

            BEFORE INPUT
               LET v_consulta = "\n SELECT FIRST 1 caso_adai",                             
                                "\n   FROM sep_expediente",
                                "\n  WHERE caso_adai = ?"
               PREPARE prp_rec_caso_adai FROM v_consulta
               
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

               MESSAGE "Conectando con Adai" ATTRIBUTE (RED,BOLD)
               INITIALIZE v_caso_adai TO NULL
               EXECUTE prp_rec_caso_adai USING v_expediente.v_caso_adai            
                                          INTO v_caso_adai
               IF(v_caso_adai IS NULL)THEN
                  --CALL fn_mensaje("","es nulo","")
                  LET v_diag_rechazo     = "001"
                  LET v_result_operacion = "01"
                  LET v_numero_caso      = v_expediente.v_caso_adai
               ELSE
                  CALL fn_mensaje("Aviso","Número de caso adai, ya esta en uso","information")
                  CONTINUE INPUT
               END IF
               # no se utiliza WS, ya que no se ha implementado en INFONAVIT
               {CALL fn_recupera_numero_caso(v_expediente.v_acreditado, 
                                            v_expediente.v_trabajador )
                                            
               RETURNING v_diag_rechazo      ,
                         v_result_operacion  ,
                         v_numero_caso}

               MESSAGE "Desconectando  Adai" ATTRIBUTE (RED,BOLD) 
               IF v_result_operacion = "01" THEN 
               
                  LET v_expediente.v_caso_adai = v_numero_caso 
                  --CALL fn_mensaje("Aviso",v_numero_caso,"information")
                  --CALL fn_mensaje("Aviso",v_expediente.v_id_expediente,"information")
                  
                  UPDATE sep_expediente 
                     SET caso_adai = v_numero_caso
                   WHERE id_expediente = v_expediente.v_id_expediente
                  
                  DISPLAY v_numero_caso TO  flbl_caso_expediente
                  
                  -- Avanza maquinaria de estado
                  CALL fn_actualiza_maquinaria(v_expediente.v_id_expediente)
                  RETURNING r_error
                  IF(r_error)THEN                        
                     CALL fn_mensaje(p_cad_ventana,"Ocurrió un error","information")
                     CONTINUE INPUT
                  END IF   
                  CALL fn_mensaje(p_cad_ventana,"Numero de Caso Adai Asignado","information")
                  EXIT INPUT
                  
               ELSE
                     CALL fn_mensaje(p_cad_ventana,"Ocurrió un error en WS Adai, Código:"||v_result_operacion,"information")
                     CONTINUE INPUT
               END IF               
                         
          --ON ACTION confirmar
            ON ACTION cancelar
            
               IF(v_expediente.v_caso_adai IS NULL OR 
                  v_expediente.v_caso_adai = '' ) THEN
                  CALL fn_ventana_confirma(p_cad_ventana,"Desea salir sin registrar archivo?","question") 
                      RETURNING r_confirma
                  IF(r_confirma)THEN
                     EXIT INPUT
                  ELSE               
                     CONTINUE INPUT
                  END IF
               ELSE
                  EXIT INPUT
               END IF
         END INPUT
      END IF
   CLOSE WINDOW vtna_carag_dictamen
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPW21                                                   #
#Descripcion       => Carga de archivo Dictámen                                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 14 Junio 2012                                            #
################################################################################
FUNCTION fn_busca_expediente()
DEFINE v_filtro    STRING,
       v_continuar BOOLEAN,
       v_bnd_con   BOOLEAN,
       v_caso_adai LIKE sep_expediente.caso_adai,
       v_id_expediente LIKE sep_expediente.id_expediente ,
       v_folio_procesar LIKE sep_expediente.folio_procesar ,
       v_id_envio       LIKE sep_expediente.id_envio

   LET v_continuar = FALSE
   LET v_bnd_con = TRUE
   LET v_filtro = " 1 = 2 "
   WHILE(v_bnd_con)
      CONSTRUCT v_filtro ON exp.id_expediente,exp.folio_procesar, exp.id_envio 
      FROM edi_id_expediente,edi_folio_procesar, edi_id_envio
         ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

         ON ACTION aceptar
         
            INITIALIZE v_folio_procesar, v_id_expediente, v_id_envio TO NULL 
            
            CALL GET_FLDBUF(edi_id_expediente) RETURNING v_id_expediente
            CALL GET_FLDBUF(edi_folio_procesar) RETURNING v_folio_procesar
            CALL GET_FLDBUF(edi_id_envio) RETURNING v_id_envio
            
            IF(v_id_expediente IS NULL AND v_folio_procesar IS NULL AND v_id_envio IS NULL) THEN
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
#Programa          => SEPW21                                                   #
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
                    "\n  WHERE exp.estado = 10 ", # Caso registrado
                    "\n    AND ",v_filtro
   PREPARE prp_recupera_expediente FROM v_consulta
   EXECUTE prp_recupera_expediente INTO v_expediente_aux.*
   IF(v_expediente_aux.v_id_expediente IS NOT NULL)THEN
      LET v_continua = TRUE
      LET v_expediente.v_canal         = v_expediente_aux.v_canal
      LET v_expediente.v_tipo_flujo    = v_expediente_aux.v_tipo_flujo
      LET v_expediente.v_f_captura     = v_expediente_aux.v_f_captura USING "mm-dd-yyyy"
      LET v_expediente.v_canal         = v_expediente_aux.v_canal
      LET v_expediente.v_id_expediente = v_expediente_aux.v_id_expediente
      
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
#Programa          => SEPW21                                                   #
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
   LET v_senial = 40  # señal definida en documento maquinarias_sep
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
#Programa          => SEPW21                                                   #
#Descripcion       => Recupera numero de caso                                  #
#Autor             => Jesus David Yañez Moreno                                 #
#Fecha inicio      => 25 Julio 2012                                            #
################################################################################
FUNCTION fn_recupera_numero_caso(v_nss_invadido, v_nss_asociado )

   DEFINE v_nss_invadido     CHAR(011)
   DEFINE v_nss_asociado     CHAR(011)
   DEFINE v_parametros asignaNumeroCaso      #Variable para los parametros que se enviaran al WS
   DEFINE v_respuesta  respuestaNumeroCaso   #Variable para "cachar" la respuesta del WS
   
   #Se llenan los parametros a enviar
   LET  v_parametros.nssInvadido = v_nss_invadido
   LET  v_parametros.nssAsociado = v_nss_asociado
   
   #Se ejecuta la funcion que invoca al WS y se guarda la respuesta en la variable v_respuesta.*
   #NOTA: Esta funcion se ejecuta por cada registro
   CALL fn_asigna_numero_caso(v_parametros.*) RETURNING v_respuesta.*
   
   #En caso de que se presente algun problema con el WS la funcion regresara los campos:
      #  v_respuesta.diagRechazo       = "-1"
      #  v_respuesta.resultOperacion   = "-1"
   #ademas de guardar un registro en la tabla wsv_his_err_cliente
   
   RETURN v_respuesta.diagRechazo       ,
          v_respuesta.resultOperacion   ,
          v_respuesta.numeroCaso
          
END FUNCTION