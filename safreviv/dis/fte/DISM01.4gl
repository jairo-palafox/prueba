################################################################################
#Versión                    => 1.0.0                                           #
#Fecha última modificacion  => 21/08/2014                                      #
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISM01                                                    #
#Objetivo         => Registro manual de los registros de avance de pagos por   #
#                    Regla 27.                                                 #
#Fecha de Inicio  => 07/08/2014                                                #
################################################################################
DATABASE safre_viv
GLOBALS
  DEFINE
    f_folio                  DECIMAL(9,0),
    f_nss                    CHAR(11),
    f_nrp                    CHAR(11),
    f_numero_credito         DECIMAL(10,0),
    f_periodo_pago           CHAR(6),
    f_monto_amo              DECIMAL(12,2),
    f_tot_registro           SMALLINT,
    f_sum_amo                DECIMAL(12,2)

  DEFINE a_ava_r27           DYNAMIC ARRAY OF RECORD
    a_folio                  DECIMAL(9,0),
    a_nss                    CHAR(11),
    a_nrp                    CHAR(11),
    a_numero_credito         DECIMAL(10,0),
    a_periodo_pago           CHAR(6),
    a_monto_amo              DECIMAL(12,2),
    a_f_registro             DATE,
    a_usuario                CHAR(30),
    a_id_r27                 DECIMAL(9,0)
  END RECORD

  DEFINE pos                 INTEGER
  DEFINE v_ventana           ui.Window
  DEFINE v_forma             ui.Form
  DEFINE v_nodo              om.DomNode
  DEFINE v_tm                om.DomNode
  DEFINE v_tmg1              om.DomNode
  DEFINE v_tmg2              om.DomNode
  DEFINE v_tmc               om.DomNode

  DEFINE r_bnd               INTEGER
  DEFINE v_status_err        INTEGER
  DEFINE v_desc_err          VARCHAR(200)
  DEFINE v_respuesta         SMALLINT

  --Sección de variables del programa
  DEFINE
    p_usuario                LIKE seg_usuario.usuario_cod,
    p_nom_prog               VARCHAR(30),
    p_tipo_proceso           SMALLINT,
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --Codigo del proceso
    g_opera_cod              LIKE cat_operacion.opera_cod, --Codigo de operacion
    g_folio                  LIKE dis_det_avance_pago.folio,
    g_pid                    LIKE bat_ctr_proceso.pid      --ID del proceso

  DEFINE v_desc_edo_caso_exc CHAR(50)

  DEFINE v_bnd_periodo       SMALLINT
  DEFINE v_periodo           CHAR(06)
  DEFINE v_indice            SMALLINT
  DEFINE v_campo_error       SMALLINT
  DEFINE v_campo_str         STRING
  
END GLOBALS

MAIN 
  LET p_usuario      = ARG_VAL(1) -- Recibe la variable de usuario
  LET p_tipo_proceso = ARG_VAL(2) -- Recibe el tipo de proceso
  LET p_nom_prog     = ARG_VAL(3) -- Recibe el nombre del programa
  LET g_proceso_cod  = 919        -- Registro Avances Regla 27
  LET g_opera_cod    = 1

  --Si se obtuvo el titulo, se pone como titulo de programa
  IF (p_nom_prog IS NOT NULL ) THEN
     CALL ui.Interface.setText(p_nom_prog)
  END IF
  
  CALL fn_mantenimiento()
END MAIN 

FUNCTION fn_mantenimiento() 
  CLOSE WINDOW SCREEN
  
  OPEN WINDOW vtn_ava_reg27 WITH FORM "DISM011"
    LET v_ventana = ui.Window.getCurrent()
    LET v_forma   = v_ventana.getForm()
    LET v_nodo    = v_forma.getNode()

    CALL v_forma.setElementHidden("group1",TRUE)
    CALL v_forma.setElementHidden("group2",TRUE)
    CALL v_forma.setElementHidden("group3",TRUE)

    CALL Proceso_principal() #pp
    
  CLOSE WINDOW vtn_ava_reg27
END FUNCTION

FUNCTION Proceso_principal()
#pp-------------------------
  {LET v_tm = v_nodo.createchild("TopMenu")
      LET v_tmg1 = fn_crea_tm_grupo(v_tm,"Acción","")
      LET v_tmc = fn_crea_tm_comando(v_tmg1,"agrega","Agregar", "Agregar avances R27","")
      LET v_tmc = fn_crea_tm_comando(v_tmg1,"consulta","Consultar", "Consulta avances R27", "")
      LET v_tmc = fn_crea_tm_comando(v_tmg1,"generar_archivo","Generar Archivo", "Generar archivo avances R27", "")
      LET v_tmc = fn_crea_tm_comando(v_tmg1,"elimina","Eliminar", "Eliminar avances R27", "")
      LET v_tmc = fn_crea_tm_comando(v_tmg1,"salir","Salir", "Salir registro avances R27", "")}
      --LET v_tmg2 = fn_crea_tm_grupo(v_tmg1,"Modificar","")
          --LET v_tmc = fn_crea_tm_comando(v_tmg2,"general","General", "Modificación solicitud","")

  MENU ""
    ON ACTION agrega
       CALL fn_inicializa()
       CALL v_forma.setElementHidden("group1",FALSE)
       CALL v_forma.setElementHidden("group2",FALSE)
       CALL v_forma.setElementHidden("group3",FALSE)
       CALL fn_agrega_r27()
       CALL v_forma.setElementHidden("group1",TRUE)
       CALL v_forma.setElementHidden("group2",TRUE)
       CALL v_forma.setElementHidden("group3",TRUE)
       CALL fn_inicializa()

    ON ACTION consulta
       CALL fn_inicializa()
       CALL v_forma.setElementHidden("group1",FALSE)
       CALL v_forma.setElementHidden("group2",FALSE)
       CALL v_forma.setElementHidden("group3",FALSE)
       CALL fn_consulta_r27()
       CALL v_forma.setElementHidden("group1",TRUE)
       CALL v_forma.setElementHidden("group2",TRUE)
       CALL v_forma.setElementHidden("group3",TRUE)
       CALL fn_inicializa()

    ON ACTION generar_archivo
       CALL fn_inicializa()
       CALL v_forma.setElementHidden("group1",FALSE)
       CALL v_forma.setElementHidden("group2",FALSE)
       CALL v_forma.setElementHidden("group3",FALSE)
       CALL fn_archivo_r27()
       CALL v_forma.setElementHidden("group1",TRUE)
       CALL v_forma.setElementHidden("group2",TRUE)
       CALL v_forma.setElementHidden("group3",TRUE)
       CALL fn_inicializa()

    ON ACTION elimina
       CALL fn_inicializa()
       CALL v_forma.setElementHidden("group1",FALSE)
       CALL v_forma.setElementHidden("group2",FALSE)
       CALL v_forma.setElementHidden("group3",FALSE)
       CALL fn_elimina_r27()
       CALL v_forma.setElementHidden("group1",TRUE)
       CALL v_forma.setElementHidden("group2",TRUE)
       CALL v_forma.setElementHidden("group3",TRUE)       
       CALL fn_inicializa()

    {ON ACTION general
       CALL fn_inicializa()
       CALL Modifica()
       CALL fn_inicializa()}
       
    ON ACTION cancelar
       EXIT MENU
  END MENU
END FUNCTION

FUNCTION fn_agrega_r27()
  DIALOG ATTRIBUTES (UNBUFFERED)
    INPUT BY NAME f_folio, f_nss, f_nrp, f_numero_credito, f_periodo_pago, f_monto_amo
      BEFORE INPUT
        CALL DIALOG.setActionHidden("cancel",1)
        CALL v_forma.setFieldHidden("f_folio",TRUE)
        CALL v_forma.setElementHidden("label6",TRUE)
        CALL v_forma.setFieldHidden("f_nss",FALSE)
        CALL v_forma.setFieldHidden("f_nrp",FALSE)
        CALL v_forma.setFieldHidden("f_numero_credito",FALSE)
        CALL v_forma.setFieldHidden("f_periodo_pago",FALSE)
        CALL v_forma.setFieldHidden("f_monto_amo",FALSE)
        CALL v_forma.setElementHidden("label1",FALSE)
        CALL v_forma.setElementHidden("label2",FALSE)
        CALL v_forma.setElementHidden("label3",FALSE)
        CALL v_forma.setElementHidden("label4",FALSE)
        CALL v_forma.setElementHidden("label5",FALSE)
        CALL v_forma.setElementHidden("group2",FALSE)
        CALL v_forma.setElementHidden("group3",FALSE)

      BEFORE FIELD f_folio
        LET f_folio = 0
        DISPLAY BY NAME f_folio
        NEXT FIELD f_nss

      AFTER FIELD f_nss
        IF (f_nss IS NULL OR
            f_nss  = '')  THEN
            CALL fn_mensaje("Error","NSS nulo","information")
            NEXT FIELD f_nss
        END IF

        IF LENGTH(f_nss) <> 11 THEN
           CALL fn_mensaje("Error","NSS diferente a 11 posiciones","information")
           NEXT FIELD f_nss
        END IF

        IF f_nss = '00000000000' THEN
           CALL fn_mensaje("Error","NSS debe ser diferente de ceros","information")
           NEXT FIELD f_nss            
        END IF

        LET v_campo_str   = f_nss
        LET v_campo_error = TRUE

        FOR v_indice = 1 TO v_campo_str.getLength()
          IF (v_campo_str.getCharAt(v_indice) < "0"   OR
              v_campo_str.getCharAt(v_indice) > "9" ) THEN
              LET v_campo_str   = NULL
              LET v_campo_error = FALSE
              EXIT FOR
          END IF
        END FOR

        IF v_campo_error = FALSE THEN
           LET v_campo_error = TRUE
           CALL fn_mensaje("Error","NSS debe ser numerico","information")
           NEXT FIELD f_nss
        END IF

      AFTER FIELD f_nrp
        IF (f_nrp IS NULL OR
            f_nrp  = '')  THEN
            CALL fn_mensaje("Error","NRP nulo","information")
            NEXT FIELD f_nrp
        END IF

        IF LENGTH(f_nrp) <> 11 THEN
           CALL fn_mensaje("Error","NRP diferente a 11 posiciones","information")
           NEXT FIELD f_nrp
        END IF

        LET v_campo_str   = f_nrp
        LET v_campo_error = TRUE

        FOR v_indice = 1 TO v_campo_str.getLength()
          IF (v_campo_str.getCharAt(v_indice) < "0"   OR 
              v_campo_str.getCharAt(v_indice) > "9")  AND 
             ((v_campo_str.getCharAt(v_indice) < "A"  OR 
              v_campo_str.getCharAt(v_indice) > "Z")  AND
              --v_campo_str.getCharAt(v_indice) <> "&"  AND
              --v_campo_str.getCharAt(v_indice) <> "#"  AND
              v_campo_str.getCharAt(v_indice) <> "Ñ") THEN
              LET v_campo_str   = NULL
              LET v_campo_error = FALSE
              EXIT FOR
          END IF
        END FOR

        IF v_campo_error = FALSE THEN
           LET v_campo_error = TRUE
           CALL fn_mensaje("Error","NRP debe ser alfanumerico","information")
           NEXT FIELD f_nrp
        END IF
        
      AFTER FIELD f_numero_credito
        IF (f_numero_credito IS NULL OR
            f_numero_credito  = '')  THEN
            CALL fn_mensaje("Error","Numero de Crédito nulo","information")
            NEXT FIELD f_numero_credito
        END IF

        IF f_numero_credito = '0000000000' THEN
           CALL fn_mensaje("Error","Numero de Crédito debe ser diferente de ceros","information")
           NEXT FIELD f_numero_credito
        END IF

        LET f_numero_credito = f_numero_credito USING "&&&&&&&&&&"

        LET v_campo_str   = f_numero_credito
        LET v_campo_error = TRUE

        FOR v_indice = 1 TO v_campo_str.getLength()
          IF (v_campo_str.getCharAt(v_indice) < "0"   OR 
              v_campo_str.getCharAt(v_indice) > "9" ) THEN
              LET v_campo_str   = NULL
              LET v_campo_error = FALSE
              EXIT FOR
          END IF
        END FOR

        IF v_campo_error = FALSE THEN
           LET v_campo_error = TRUE
           CALL fn_mensaje("Error","Numero de crédito debe ser numerico","information")
           NEXT FIELD f_numero_credito
        END IF

      AFTER FIELD f_periodo_pago
        IF (f_periodo_pago        IS NULL OR
            f_periodo_pago         = ''   OR
            LENGTH(f_periodo_pago) <> 6)  THEN
            CALL fn_mensaje("Error","Periodo de Pago nulo","information")
            NEXT FIELD f_periodo_pago
        END IF

        IF (f_periodo_pago < '197301'  OR
            f_periodo_pago > '201301') THEN
            CALL fn_mensaje("Error","Periodo de Pago no valido (Periodos validos 197301 a 201301)","information")
            NEXT FIELD f_periodo_pago              
        END IF

        PREPARE prp_verifica_periodo FROM "EXECUTE FUNCTION fn_valida_formato_periodo_pago(?)"
        EXECUTE prp_verifica_periodo USING f_periodo_pago INTO v_periodo, v_bnd_periodo

        --La función "fn_valida_formato_periodo_pago" regresa 2 parámetros, uno el periodo y dos el estatus
        --Si el estatus es 0, no hay error y el periodo es válido
        --Si el estatus es 1, entonces el año del periodo es inválido, es decir, es mayor al año actual
        --Si el estatus es 2, entonces el mes es incorrecto
        IF v_bnd_periodo <> 0 THEN
           IF v_bnd_periodo = 1 THEN
              CALL fn_mensaje ("Error", "Verifique el periodo de pago. El año es incorrecto", "information")
           END IF
           
           IF v_bnd_periodo = 2 THEN
              CALL fn_mensaje ("Error", "Verifique el periodo de pago. El mes es incorrecto", "information")
           END IF

           NEXT FIELD f_periodo_pago
        END IF

      AFTER FIELD f_monto_amo
        IF (f_monto_amo <= 0 )THEN
            CALL fn_mensaje("Error","Monto amortización menor - igual a 0","information")
            NEXT FIELD f_monto_amo
        END IF

        NEXT FIELD f_nss

      ON ACTION AGREGAR
         DISPLAY BY NAME f_folio
         DISPLAY BY NAME f_nss 
         DISPLAY BY NAME f_nrp
         DISPLAY BY NAME f_numero_credito
         DISPLAY BY NAME f_periodo_pago
         DISPLAY BY NAME f_monto_amo

         CALL fn_valida_campos()
         RETURNING v_status_err, v_desc_edo_caso_exc

         IF v_status_err <> 0 THEN
            CALL fn_mensaje("Information",v_desc_edo_caso_exc,"information")
            
            IF (v_status_err = 110  OR
                v_status_err = 111) THEN
                NEXT FIELD f_nss
            END IF

            IF v_status_err = 120 THEN
               NEXT FIELD f_nrp
            END IF

            IF (v_status_err = 130  OR
                v_status_err = 131) THEN
                NEXT FIELD f_numero_credito
            END IF

            IF (v_status_err = 140  OR
                v_status_err = 141  OR
                v_status_err = 142) THEN
                NEXT FIELD f_periodo_pago
            END IF

            IF v_status_err = 150 THEN
               NEXT FIELD f_monto_amo
            END IF

            IF v_status_err = 160 THEN
               NEXT FIELD f_nss
            END IF            
         END IF
         
         WHENEVER ERROR CONTINUE
           PREPARE prp_sp_ava_r27 FROM "EXECUTE PROCEDURE safre_viv:sp_dis_ava_regla27(?,?,?,?,?,?,?)"
           EXECUTE prp_sp_ava_r27 USING f_folio,
                                        f_nss,
                                        f_nrp,
                                        f_numero_credito,
                                        f_periodo_pago,
                                        f_monto_amo,
                                        p_usuario                                          
                                  INTO  r_bnd,
                                        v_status_err,
                                        v_desc_err
         WHENEVER ERROR STOP

         IF v_status_err = 0 THEN
            CALL fn_inicializa()
            CALL fn_mensaje("Information","El registro fue almacenado.","information")
            CALL fn_llena_arreglo()
            NEXT FIELD f_nss
         ELSE
            SELECT a.cod_edo_caso_exc || " - " || a.desc_edo_caso_exc
            INTO   v_desc_edo_caso_exc
            FROM   cat_edo_caso_excepcion a
            WHERE  a.cod_edo_caso_exc = v_status_err

            CALL fn_mensaje("Information",v_desc_edo_caso_exc,"information")

            IF (v_status_err = 10  OR
                v_status_err = 20) THEN
                NEXT FIELD f_nss
            END IF

            IF v_status_err = 80 THEN
               NEXT FIELD f_nrp
            END IF

            IF v_status_err = 40 THEN
               NEXT FIELD f_numero_credito
            END IF

            IF v_status_err = 50 THEN
               NEXT FIELD f_periodo_pago
            END IF

            IF v_status_err = 100 THEN
               NEXT FIELD f_monto_amo
            END IF            
         END IF
    END INPUT

    DISPLAY ARRAY a_ava_r27 TO Record2.* 
      BEFORE ROW
        LET pos = ARR_CURR()
        NEXT FIELD f_nss
    END DISPLAY

    BEFORE DIALOG
      LET f_folio = 0
      CALL fn_llena_arreglo()

    ON ACTION CANCEL
       CALL fn_inicializa()
       EXIT DIALOG  

  END DIALOG
END FUNCTION

FUNCTION fn_consulta_r27()
  DEFINE v_tot_cons          SMALLINT

  LET  v_tot_cons = 0

  DIALOG ATTRIBUTES (UNBUFFERED)
    INPUT BY NAME f_folio
      BEFORE INPUT
        CALL DIALOG.setActionHidden("cancel",1)
        CLEAR FORM

        --Oculta las secciones de detalle, campos y botones adicionales 
        --(TRUE oculta y FALSE muestra)
        CALL v_forma.setFieldHidden("f_folio",FALSE)
        CALL v_forma.setElementHidden("label6",FALSE)
        CALL v_forma.setFieldHidden("f_nss",TRUE)
        CALL v_forma.setFieldHidden("f_nrp",TRUE)
        CALL v_forma.setFieldHidden("f_numero_credito",TRUE)
        CALL v_forma.setFieldHidden("f_periodo_pago",TRUE)
        CALL v_forma.setFieldHidden("f_monto_amo",TRUE)
        CALL v_forma.setElementHidden("label1",TRUE)
        CALL v_forma.setElementHidden("label2",TRUE)
        CALL v_forma.setElementHidden("label3",TRUE)
        CALL v_forma.setElementHidden("label4",TRUE)
        CALL v_forma.setElementHidden("label5",TRUE)
        CALL v_forma.setElementHidden("group2",TRUE)
        CALL v_forma.setElementHidden("group3",TRUE)

      BEFORE FIELD f_folio
        LET f_folio = 0
        DISPLAY BY NAME f_folio
    
      AFTER FIELD f_folio
        IF (f_folio IS NULL OR
            f_folio  = '')  THEN
            CALL fn_mensaje("Error","Folio no puede ser nulo","information")
            NEXT FIELD f_folio
        END IF

      ON ACTION CONSULTAR
         SELECT COUNT(*)
         INTO   v_tot_cons
         FROM   dis_ava_regla_27
         WHERE  folio = f_folio
         IF v_tot_cons = 0 THEN
            CALL fn_mensaje("Error","Folio no existe","information")
            NEXT FIELD f_folio
         END IF

         IF v_tot_cons <> 0 THEN
            CALL v_forma.setElementHidden("group2",FALSE)
            CALL v_forma.setElementHidden("group3",FALSE)
            CALL fn_llena_arreglo()

            DISPLAY ARRAY a_ava_r27 TO Record2.* 
              BEFORE ROW
                CALL DIALOG.setActionHidden("accept",1)
                LET pos = ARR_CURR()

                ON ACTION Reporte
                   CALL fn_reporte_ava_r27()
                   
                   ON ACTION CANCEL
                   CALL fn_inicializa()
                   CALL v_forma.setElementHidden("group2",TRUE)
                   CALL v_forma.setElementHidden("group3",TRUE)
                   EXIT DIALOG
            END DISPLAY
         ELSE
            CALL fn_mensaje("Error","Folio no existe","information")
            LET f_folio = NULL
            NEXT FIELD f_folio
         END IF 
    END INPUT

    ON ACTION CANCEL
       CALL fn_inicializa()
       CALL v_forma.setElementHidden("group2",TRUE)
       CALL v_forma.setElementHidden("group3",TRUE)
       EXIT DIALOG  

  END DIALOG
END FUNCTION

FUNCTION fn_archivo_r27()
  DIALOG ATTRIBUTES (UNBUFFERED)

    DISPLAY ARRAY a_ava_r27 TO Record2.* 
      BEFORE ROW
        LET pos = ARR_CURR()
    END DISPLAY

    BEFORE DIALOG
      CALL v_forma.setElementHidden("group1",TRUE)
      CALL DIALOG.setActionHidden("cancel",1)
      CALL fn_llena_arreglo()

      SELECT COUNT(*)
      INTO   v_respuesta
      FROM   dis_ava_regla_27 a
      WHERE  a.folio = 0
      IF v_respuesta = 0 THEN
         CALL fn_mensaje("GENERAR ARCHIVO","No existen registros para generar","information")
         EXIT DIALOG
      END IF

    ON ACTION ARCHIVO
       CALL fn_ventana_confirma("GENERAR ARCHIVO",
                                "¿Está seguro de generar el archivo?",
                                "quest")
       RETURNING v_respuesta

       IF (v_respuesta = 1) THEN
          CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario)
          RETURNING g_folio

          CALL fn_envia_operacion()
          CALL fn_inicializa()
          CALL fn_llena_arreglo()
          EXIT DIALOG        
       ELSE
          CALL fn_mensaje("GENERAR ARCHIVO","Operación Cancelada","information")
          EXIT DIALOG
       END IF
       
    ON ACTION CANCEL
       CALL fn_inicializa()
       EXIT DIALOG  

  END DIALOG
END FUNCTION

FUNCTION fn_elimina_r27()
  DIALOG ATTRIBUTES (UNBUFFERED)

    DISPLAY ARRAY a_ava_r27 TO Record2.* 
      BEFORE ROW
        LET pos = ARR_CURR()
    END DISPLAY

    BEFORE DIALOG
      CALL v_forma.setElementHidden("group1",TRUE)
      CALL DIALOG.setActionHidden("cancel",1)
      CALL fn_inicializa()
      CALL fn_llena_arreglo()

      SELECT COUNT(*)
      INTO   v_respuesta
      FROM   dis_ava_regla_27 a
      WHERE  a.folio = 0
      IF v_respuesta = 0 THEN
         CALL fn_mensaje("ELIMINAR","No existen registros para eliminar","information")
         EXIT DIALOG
      END IF
     
    ON ACTION ELIMINAR
       CALL fn_ventana_confirma("ELIMINAR REGISTRO AVANCE REGLA 27",
                                "¿Está seguro de eliminar el registro?",
                                "quest")
       RETURNING v_respuesta

       IF (v_respuesta = 1) THEN
          DISPLAY "ID:  ", a_ava_r27[pos].a_id_r27
          DISPLAY "NSS: ", a_ava_r27[pos].a_nss

          DELETE
          FROM   dis_ava_regla_27
          WHERE  id_dis_det_ava_regla27 = a_ava_r27[pos].a_id_r27

          CALL fn_inicializa()
          CALL fn_llena_arreglo()
       END IF
       
    ON ACTION CANCEL
       CALL fn_inicializa()
       EXIT DIALOG  

  END DIALOG
END FUNCTION

FUNCTION fn_llena_arreglo()
  DEFINE num                 INTEGER

  LET f_sum_amo      = 0
  LET f_tot_registro = 0
  LET num            = 1
  
  DECLARE c1 CURSOR FOR SELECT folio, 
                               nss, 
                               nrp, 
                               num_credito,
                               periodo_pago,
                               monto_amortizacion,
                               f_registro,
                               usuario,
                               id_dis_det_ava_regla27
                        FROM   dis_ava_regla_27
                        WHERE  folio = f_folio
                        ORDER BY id_dis_det_ava_regla27 DESC
  FOREACH c1 INTO a_ava_r27[num].*
    LET a_ava_r27[num].a_numero_credito = a_ava_r27[num].a_numero_credito USING "&&&&&&&&&&"
    LET f_sum_amo                       = a_ava_r27[num].a_monto_amo + f_sum_amo
    LET f_tot_registro                  = num
    LET num                             = num + 1
  END FOREACH

  CALL a_ava_r27.deleteElement(num)

  DISPLAY BY NAME f_tot_registro
  DISPLAY BY NAME f_sum_amo
  
END FUNCTION

FUNCTION fn_crea_tm_grupo(v_tm, p_texto, p_comentario)
  DEFINE v_tmg               om.DomNode
  DEFINE v_tm                om.DomNode
  DEFINE p_texto             STRING
  DEFINE p_comentario        STRING

  LET v_tmg = v_tm.createChild("TopMenuGroup")

  CALL v_tmg.setAttribute("text",p_texto)
  CALL v_tmg.setAttribute("comment",p_comentario)

  RETURN v_tmg

END FUNCTION

FUNCTION fn_crea_tm_comando(v_tmg, p_nombre, p_texto, p_comentario, p_imagen)
  DEFINE v_tmi               om.DomNode
  DEFINE v_tmg               om.DomNode
  DEFINE p_nombre            STRING
  DEFINE p_texto             STRING
  DEFINE p_imagen            STRING
  DEFINE p_comentario        STRING

  LET v_tmi = v_tmg.createChild("TopMenuCommand")

  CALL v_tmi.setAttribute("name",p_nombre)
  CALL v_tmi.setAttribute("text",p_texto)
  CALL v_tmi.setAttribute("comment",p_comentario)
  CALL v_tmi.setAttribute("image",p_imagen)

  RETURN v_tmi

END FUNCTION

FUNCTION fn_inicializa()
  CALL a_ava_r27.clear()
  
  LET f_folio          = 0
  LET f_nss            = NULL
  LET f_nrp            = NULL
  LET f_numero_credito = NULL
  LET f_periodo_pago   = NULL
  LET f_monto_amo      = NULL
  LET f_tot_registro   = 0
  LET f_sum_amo        = 0

  DISPLAY BY NAME f_folio
  DISPLAY BY NAME f_nss 
  DISPLAY BY NAME f_nrp
  DISPLAY BY NAME f_numero_credito
  DISPLAY BY NAME f_periodo_pago
  DISPLAY BY NAME f_monto_amo
  DISPLAY BY NAME f_tot_registro
  DISPLAY BY NAME f_sum_amo
 
END FUNCTION

FUNCTION fn_envia_operacion()
  DEFINE v_bandera           SMALLINT --Para verificar resultado de iniciar la operacion
  DEFINE v_comando           STRING
  DEFINE l_bat_ruta_listado  CHAR(40)
  DEFINE v_ruta_origen       CHAR(40)
  DEFINE v_desc_salida       VARCHAR(100)
  DEFINE v_mensaje           STRING
  DEFINE v_folio             LIKE glo_folio.folio
  DEFINE v_nombre_archivo    LIKE glo_ctr_archivo.nombre_archivo

  --Este proceso inicia por webservices, no tiene archivo
  LET v_nombre_archivo = "NA"

  SELECT ruta_listados
  INTO   l_bat_ruta_listado
  FROM   seg_modulo
  WHERE  modulo_cod = 'bat'

  SELECT ruta_bin
  INTO   v_ruta_origen
  FROM   seg_modulo
  WHERE  modulo_cod = 'dis'

  --Se verifica si se puede continuar con la operacion
  LET v_bandera = fn_valida_operacion(0, g_proceso_cod, g_opera_cod)

  IF ( v_bandera = 0 ) THEN
     CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario)
     RETURNING g_pid

     CALL fn_inicializa_proceso(g_pid,
                                g_proceso_cod,
                                g_opera_cod,
                                g_folio,
                                "DISM01",
                                v_nombre_archivo,
                                p_usuario)
     RETURNING v_bandera

     IF ( v_bandera = 0 ) THEN
        CALL fn_actualiza_opera_ini(g_pid,
                                    g_proceso_cod,
                                    g_opera_cod,
                                    g_folio,
                                    "DISS31",
                                    "",
                                    p_usuario)
        RETURNING v_bandera

        LET v_comando = "nohup fglrun ",v_ruta_origen CLIPPED,"/DISS31.42r ",
                        p_usuario CLIPPED, " ",
                        g_pid            , " ",
                        g_proceso_cod    , " ",
                        g_opera_cod      , " ",
                        g_folio          , " ",
                        v_folio          , " ",
                        " 1>", l_bat_ruta_listado CLIPPED ,
                        "/nohup:",g_pid  USING "&&&&&",":",
                        g_proceso_cod    USING "&&&&&",":",
                        g_opera_cod      USING "&&&&&",
                        " 2>&1 &"
        RUN v_comando

        LET v_comando = "Se ha enviado la interface con el PID: ",
                        g_pid CLIPPED,
                        "\nPuede revisar el avance del proceso en el monitor de ejecución de procesos."
        
        CALL fn_mensaje("Atención",v_comando,"information")
     ELSE
        --Se obtiene la descripcion del parametro de salida
        SELECT descripcion
        INTO   v_desc_salida
        FROM   cat_bat_parametro_salida
        WHERE  cod_salida = v_bandera

        --Se construye el mensaje de error
        LET v_comando = "No se puede iniciar la operación.\nError: ", v_desc_salida CLIPPED
        CALL fn_mensaje("Atención",v_comando,"stop")
     END IF
  ELSE
     --No se puede ejecutar la operacion
     CALL fn_recupera_inconsis_opera(v_bandera)
     RETURNING v_mensaje

     CALL fn_mensaje("Atención", v_mensaje, "stop")
  END IF
  
END FUNCTION

#Objetivo: Genera reporte de avances de pago regla 27
FUNCTION fn_reporte_ava_r27()
  DEFINE
    manejador_rpt            om.SaxDocumentHandler, --Contenedor documentos reporte
    v_rep_indice             INTEGER

  LET v_rep_indice = 1

  --Botón para generar el reporte en PDF de la consulta
  IF fgl_report_loadCurrentSettings("DISM011.4rp") THEN
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  --Inicia el reporte de avances de pago regla 27
  START REPORT rp_ava_r27 TO XML HANDLER manejador_rpt
    FOR v_rep_indice = 1 TO a_ava_r27.getLength()
        OUTPUT TO REPORT rp_ava_r27(a_ava_r27[v_rep_indice].*,
                                    f_folio,
                                    f_tot_registro,
                                    f_sum_amo,
                                    p_usuario)
    END FOR
  FINISH REPORT rp_ava_r27
END FUNCTION

#Objetivo: Estructura reporte de Avances de Pago Regla 27
REPORT rp_ava_r27(v_rep_ava_r27,
                      v_rep_folio,
                      v_rep_tot_registro,
                      v_rep_sum_amo,
                      v_usuario)

  DEFINE v_rep_ava_r27       RECORD
    v_rep_folio              DECIMAL(9,0),
    v_rep_nss                CHAR(11),
    v_rep_nrp                CHAR(11),
    v_rep_numero_credito     DECIMAL(10,0),
    v_rep_periodo_pago       CHAR(6),
    v_rep_monto_amo          DECIMAL(12,2),
    v_rep_f_registro         DATE,
    v_rep_usuario            CHAR(30),
    v_rep_id_r27             DECIMAL(9,0)
  END RECORD

  DEFINE
    v_fecha_consulta         DATE,        --Fecha de proceso
    v_usuario                VARCHAR(30), --Almacena al usuario
    v_rep_tot_registro       DECIMAL(9,0),--Total de registros
    v_rep_sum_amo            DECIMAL(12,2),
    v_rep_folio              DECIMAL(9,0)

  FORMAT
    FIRST PAGE HEADER
      --Inicializa la fecha de consulta
      LET v_fecha_consulta = TODAY

      PRINTX v_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"
      PRINTX v_rep_folio USING "&&&&&&&&&"

    ON EVERY ROW
      PRINTX v_rep_ava_r27.v_rep_folio USING "&&&&&&&&&"
      PRINTX v_rep_ava_r27.v_rep_nss
      PRINTX v_rep_ava_r27.v_rep_nrp
      PRINTX v_rep_ava_r27.v_rep_numero_credito
      PRINTX v_rep_ava_r27.v_rep_periodo_pago
      PRINTX v_rep_ava_r27.v_rep_monto_amo
      PRINTX v_rep_ava_r27.v_rep_f_registro USING "dd-mm-yyyy"
      PRINTX v_rep_ava_r27.v_rep_usuario
      PRINTX v_rep_ava_r27.v_rep_id_r27
      
    ON LAST ROW
      PRINTX v_rep_tot_registro
      PRINTX v_rep_sum_amo

END REPORT

FUNCTION fn_valida_campos()
  DEFINE v_error_val         SMALLINT
  DEFINE v_desc_err_val      CHAR(50)

  LET v_error_val    = 0
  LET v_desc_err_val = NULL
  
  --Valida NSS
  IF f_nss = '00000000000' THEN
     LET v_error_val    = 110
     LET v_desc_err_val = "NSS debe ser diferente de ceros"
  ELSE     
     LET v_campo_str   = f_nss
     LET v_campo_error = TRUE

     FOR v_indice = 1 TO v_campo_str.getLength()
       IF (v_campo_str.getCharAt(v_indice) < "0"   OR
           v_campo_str.getCharAt(v_indice) > "9" ) THEN
           LET v_campo_str   = NULL
           LET v_campo_error = FALSE
           EXIT FOR
       END IF
     END FOR

     IF v_campo_error = FALSE THEN
        LET v_campo_error  = TRUE
        LET v_error_val    = 111
        LET v_desc_err_val = "NSS debe ser numerico"
     END IF
  END IF

  --Valida NRP
  LET v_campo_str   = f_nrp
  LET v_campo_error = TRUE

  FOR v_indice = 1 TO v_campo_str.getLength()
    IF (v_campo_str.getCharAt(v_indice) < "0"   OR 
        v_campo_str.getCharAt(v_indice) > "9")  AND 
      ((v_campo_str.getCharAt(v_indice) < "A"   OR 
        v_campo_str.getCharAt(v_indice) > "Z")  AND
        v_campo_str.getCharAt(v_indice) <> "Ñ") THEN
        LET v_campo_str   = NULL
        LET v_campo_error = FALSE
        EXIT FOR
    END IF
  END FOR

  IF v_campo_error = FALSE THEN
     LET v_campo_error  = TRUE
     LET v_error_val    = 120
     LET v_desc_err_val = "NRP debe ser alfanumerico"
  END IF

  --Valida Numero de Crédito
  IF f_numero_credito = '0000000000' THEN
     LET v_error_val    = 130
     LET v_desc_err_val = "Numero de Crédito debe ser diferente de ceros"
  END IF

  LET f_numero_credito = f_numero_credito USING "&&&&&&&&&&"

  LET v_campo_str   = f_numero_credito
  LET v_campo_error = TRUE

  FOR v_indice = 1 TO v_campo_str.getLength()
    IF (v_campo_str.getCharAt(v_indice) < "0"   OR 
        v_campo_str.getCharAt(v_indice) > "9" ) THEN
        LET v_campo_str   = NULL
        LET v_campo_error = FALSE
        EXIT FOR
    END IF
  END FOR

  IF v_campo_error = FALSE THEN
     LET v_campo_error  = TRUE
     LET v_error_val    = 131
     LET v_desc_err_val = "Numero de crédito debe ser numerico"
  END IF

  --Valida Periodo de Pago
  PREPARE prp_ver_per FROM "EXECUTE FUNCTION fn_valida_formato_periodo_pago(?)"
  EXECUTE prp_ver_per USING f_periodo_pago INTO v_periodo, v_bnd_periodo

  IF v_bnd_periodo <> 0 THEN
     IF v_bnd_periodo = 1 THEN
        LET v_error_val    = 140
        LET v_desc_err_val = "Verifique el periodo de pago. El año es incorrecto"
     END IF
           
     IF v_bnd_periodo = 2 THEN
        LET v_error_val    = 141
        LET v_desc_err_val = "Verifique el periodo de pago. El mes es incorrecto"
      END IF
  END IF

  IF (f_periodo_pago < '197301'  OR
      f_periodo_pago > '201301') THEN
      LET v_error_val    = 142
      LET v_desc_err_val = "Los periodos validos son del 197301 a 201301"
  END IF

  --Valida Monto Amortización
  IF (f_monto_amo IS NULL  OR
      f_monto_amo <= 0.00) THEN
      LET v_error_val    = 150
      LET v_desc_err_val = "Monto amortización menor - igual a 0"
  END IF

  --Valida indice por Folio, Período de Pago y NSS
  SELECT a.id_dis_det_ava_regla27
  FROM   dis_ava_regla_27 a
  WHERE  a.folio        = 0
  AND    a.nss          = f_nss
  AND    a.periodo_pago = f_periodo_pago
  AND    a.nrp          = f_nrp
  IF STATUS <> NOTFOUND THEN
     LET v_error_val    = 160
     LET v_desc_err_val = "Registro ya capturado(Folio-NSS-Per Pago-NRP)"
  END IF
  
  RETURN v_error_val, v_desc_err_val
END FUNCTION