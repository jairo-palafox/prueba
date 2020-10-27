################################################################################
#Modulo           => SEP                                                       #
#Programa         => SEPM01                                                    #
#Objetivo         => Modificación det02 y det03 OP27                           #
#Fecha de Inicio  => MAYO 2015                                                 #
################################################################################
SCHEMA safre_viv

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   INTEGER,
       v_ventana         ui.Window,
       p_cad_ventana     STRING,
       v_forma           ui.Form

DEFINE g_reg_modulo        RECORD
        ruta_exp              CHAR(40),
        ruta_rescate          CHAR(40),
        ruta_listados         CHAR(40)
END RECORD

DEFINE operacion27 DYNAMIC ARRAY OF RECORD
        f_id_detalle_02 LIKE sep_det_02_op27.id_det_02_op27,
        f_f_proceso     LIKE sep_det_02_op27.f_proceso,
        f_invadido      LIKE sep_det_02_op27.invadido,
        f_asociado      LIKE sep_det_03_op27.asociado,
        f_resultado     LIKE sep_det_02_op27.resultado_operacion,
        f_diagnostico   LIKE sep_det_02_op27.diag_confronta,
        f_clasifica     LIKE sep_det_02_op27.clasifica_separacion,
        f_estado        LIKE sep_det_02_op27.estado,
        f_estado_desc   LIKE sep_estado_op27.descripcion
END RECORD

DEFINE r_box ui.ComboBox,
       c_box ui.ComboBox,
       d_box ui.ComboBox
    
MAIN

   DEFINE v_invadido        CHAR(11) 
   DEFINE v_valida_invadido BOOLEAN
   DEFINE v_err_mesg        STRING

   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

      -- se obtienen las rutas de control del modulo
   CONNECT TO "safre_viv"
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
     WHERE s.modulo_cod = 'sep'
   DISCONNECT "safre_viv"

   CALL STARTLOG(g_reg_modulo.ruta_listados CLIPPED ||"/"||p_usuario_cod CLIPPED||".SEPM01.log")

   OPEN WINDOW vtna_modificacion WITH FORM g_reg_modulo.ruta_exp CLIPPED||"/SEPM011"
   #Se asigna el titulo de la ventana y toma el control de la forma
      IF ( p_cad_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_cad_ventana)
         LET v_ventana = ui.Window.getCurrent()
         LET v_forma = v_ventana.getForm()
         CALL v_ventana.setText(p_cad_ventana)
      END IF

      DIALOG ATTRIBUTES(UNBUFFERED)

      --CAPTURANDO EL NSS INVADIDO
         INPUT BY NAME v_invadido
            ON ACTION aceptar
               CALL fn_valida_nss(v_invadido) RETURNING v_valida_invadido,v_err_mesg
               IF v_valida_invadido THEN --Despliega los datos posteriores del dialog
                  CALL v_forma.setElementHidden("f_modifica",0)
                  CALL fn_despliega_campos(v_invadido)
               ELSE --oculta los datos para evitar modificacion
                  CALL fn_mensaje(p_cad_ventana,v_err_mesg,"about")
                  CALL v_forma.setElementHidden("f_modifica",1)
               END IF
               
            ON ACTION cancelar
               EXIT DIALOG
         END INPUT

         --ACEPTA INTRODUCCION DE DATOS PARA MODIFICAR 
         INPUT ARRAY operacion27 FROM  Record1.* ATTRIBUTES(WITHOUT DEFAULTS,INSERT ROW = FALSE, APPEND ROW = FALSE, DELETE ROW = FALSE)

            ON ROW CHANGE 
               CALL fn_modifica_registro(ARR_CURR())
        
            ON ACTION aceptar
               IF FIELD_TOUCHED(Record1.*) THEN --DETECTA SI SE HA MODIFICADO ALGUN CAMPO
                  CALL fn_modifica_registro(ARR_CURR())
                  --Para no repetir el proceso en ROW CHANGE se resetea la bandera de cambio en todos los campos
                  CALL DIALOG.setFieldTouched("f_asociado",FALSE)
                  CALL DIALOG.setFieldTouched("f_resultado",FALSE)
                  CALL DIALOG.setFieldTouched("f_diagnostico",FALSE)
                  CALL DIALOG.setFieldTouched("f_clasifica",FALSE)
               END IF
        
            ON ACTION cancelar
               EXIT DIALOG 
         END INPUT 
        
         BEFORE DIALOG
            CONNECT TO "safre_viv"
            CALL fn_llena_combos()--Llenando Combobox
            CALL v_forma.setElementHidden("f_modifica",1)

         ON ACTION aceptar   
            
         ON ACTION cancelar
            EXIT DIALOG

      END DIALOG
      
   CLOSE WINDOW vtna_modificacion
   DISCONNECT ALL
END MAIN

FUNCTION fn_llena_combos()

    DEFINE p_res_op    RECORD LIKE sep_cat_resultado_operacion.*
    DEFINE p_diag_conf RECORD LIKE sep_cat_diag_confronta.*
    DEFINE p_clas_sep  RECORD LIKE sep_cat_clasificacion.*

    --LLenando combos box
    LET r_box = ui.ComboBox.forName("f_resultado")
    CALL r_box.clear()
    DECLARE cur_rb CURSOR FOR SELECT * FROM sep_cat_resultado_operacion
    FOREACH cur_rb INTO p_res_op.*
        CALL r_box.addItem(p_res_op.resultado_operacion,p_res_op.resultado_operacion_desc)
    END FOREACH
    CLOSE cur_rb
    FREE cur_rb

        --LLenando combos box
    LET d_box = ui.ComboBox.forName("f_diagnostico")
    CALL d_box.clear()
    DECLARE cur_db CURSOR FOR SELECT * FROM sep_cat_diag_confronta
    FOREACH cur_db INTO p_diag_conf.*
        CALL d_box.addItem(p_diag_conf.diag_confronta,p_diag_conf.diag_confronta_desc)
    END FOREACH
    CLOSE cur_db
    FREE cur_db

        --LLenando combos box
    LET c_box = ui.ComboBox.forName("f_clasifica")
    CALL c_box.clear()
    DECLARE cur_cb CURSOR FOR SELECT * FROM sep_cat_clasificacion
    FOREACH cur_cb INTO p_clas_sep.*
        CALL c_box.addItem(p_clas_sep.clasifica_separacion,p_clas_sep.clasificacion_desc)
    END FOREACH
    CLOSE cur_cb
    FREE cur_cb

END FUNCTION

--FUNCION QUE VALIDA SI UN NSS INVADIDO CONTIENE 11 POSICIONES 
--Y EXISTE EN LA TABLA sep_det_02_op27
FUNCTION fn_valida_nss(p_invadido)
    DEFINE p_invadido   CHAR(11)
    DEFINE f_valida     BOOLEAN
    DEFINE f_ids         DECIMAL(9,0)
    DEFINE f_message    STRING
    DEFINE i            INTEGER

    LET f_ids = 0
    LET i = LENGTH(p_invadido)
    IF i! = 11 THEN
        LET f_valida = FALSE
        LET f_message = "El dato capturado no contiene 11 posiciones"
    ELSE
        SELECT COUNT(id_det_02_op27) 
           INTO f_ids
           FROM sep_det_02_op27
           WHERE invadido = p_invadido
        IF f_ids > 0 THEN
           LET f_valida = TRUE
           LET f_message = "nss invadido OK" --Existe por lo menos 1 identificador para ese invadido
        ELSE
           LET f_valida = FALSE
           LET f_message = "El nss invadido no se encuentra en la base de datos"
        END IF
    END IF
    
    RETURN f_valida,f_message
    
END FUNCTION

## -OBJETIVO: Despliega los valores del invadido asignado
FUNCTION fn_despliega_campos(p_invadido)
    DEFINE p_invadido  CHAR(11)
    DEFINE p_qry       STRING
    DEFINE k           INTEGER

    --Llenando datos con el identificador
        --OUTER JOIN muestra valores aunque el invadido no exista en tabla det_03
    LET p_qry = "
       SELECT 
          a.id_det_02_op27,
	      a.f_proceso,
	      a.invadido,
	      b.asociado,
	      a.resultado_operacion,
	      a.diag_confronta,
	      a.clasifica_separacion,
          a.estado,
          (SELECT descripcion FROM sep_estado_op27 WHERE estado = a.estado) estado
       FROM   
          sep_det_02_op27 a,
	      OUTER sep_det_03_op27 b
       WHERE  
	      a.id_det_02_op27 = b.id_det_02_op27
       AND    
          a.invadido = 
          ",p_invadido

    CALL operacion27.clear() -- Limpiamos el arreglo     

    PREPARE prp_llena_campos FROM p_qry
    DECLARE cur CURSOR FOR prp_llena_campos 
    LET k = 1
    FOREACH cur INTO operacion27[k].*
        LET k = k+1
    END FOREACH
    CALL operacion27.deleteElement(k)
    
    DISPLAY ARRAY operacion27 TO Record1.*
        BEFORE DISPLAY --PARA SALIR AUTOMATICAMENTE DEL DISPLAY
            EXIT DISPLAY
    END DISPLAY
    
    CLOSE cur
    FREE prp_llena_campos
    
END FUNCTION

##-- OBJETIVO: REALIZA EL UPDATE EN LA TABLA sep_det_02_op27 y/o La inserción de un nuevo campo en det_03
##-- Realiza validaciones para ejecutar la tarea correspondiente 
FUNCTION fn_modifica_registro(p_pos)
    DEFINE p_pos            INTEGER
    DEFINE p_agrega_det03   BOOLEAN
    DEFINE p_id_dh          LIKE afi_derechohabiente.id_derechohabiente
    DEFINE detalle02        RECORD LIKE sep_det_02_op27.*
    DEFINE detalle03        RECORD LIKE sep_det_03_op27.*
    
    #--PRIMERA VALIDACION: si el nss Asociado existe en la tabla afi_derechohabiente
    SELECT id_derechohabiente INTO p_id_dh FROM afi_derechohabiente
       WHERE nss = operacion27[p_pos].f_asociado
    IF sqlca.sqlcode == 100 THEN
       CALL fn_mensaje(p_cad_ventana,"El nss Asociado que ha ingresado no existe en afi_derechohabiente\n
                        Favor de validar","about") 
       RETURN
    END IF
    
    #--SEGUNDA VALIDACION: Que la combinacion de campos resultado_operacion, 
    #  diag_confronta y clasifica_separación sea correcta
    IF operacion27[p_pos].f_resultado == "01" AND operacion27[p_pos].f_diagnostico == "01" THEN
        IF operacion27[p_pos].f_clasifica == 'A'
           OR operacion27[p_pos].f_clasifica == 'B'
           OR operacion27[p_pos].f_clasifica == 'C'
           OR operacion27[p_pos].f_clasifica == 'D' THEN
        ELSE
           CALL fn_mensaje(p_cad_ventana,"La combinacion de campos Resultado, Diagnostico y Clasificacion no es correcta.
              \nFavor de validar","about")
           RETURN
        END IF
    ELSE 
        CALL fn_mensaje(p_cad_ventana,"La combinacion de campos Resultado, Diagnostico y Clasificacion no es correcta.
           \nFavor de validar","about") 
        RETURN
    END IF

    ##COMPROBANDO QUE EL USUARIO REALMENTE DESEA HACER EL CAMBIO
    IF NOT fn_ventana_confirma(p_cad_ventana,"¿Realmente desea modificar este registro?","about") THEN
        RETURN        
    END IF

    --TOMANDO Un Record con la informacion del registro det_02 actual
    SELECT * INTO detalle02.* FROM sep_det_02_op27
       WHERE id_det_02_op27 = operacion27[p_pos].f_id_detalle_02
       
    #--TERCERA VALIDACION: si el id_det_02_op27 existe tambien en la tabla sep_det_03_op27
    #  Si no existe, agregar nuevo registro a la tabla sep_det_03_op27  
    SELECT * FROM sep_det_03_op27
       WHERE id_det_02_op27 = operacion27[p_pos].f_id_detalle_02
    IF sqlca.sqlcode == 100 THEN
       LET p_agrega_det03 = TRUE
    ELSE
       LET p_agrega_det03 = FALSE
    END IF

    #--CUARTA VALIDACION: si el id_expediente no tiene valor y el estado es 
    --diferente de 30 y 35 se actualiza el estado. 
    IF operacion27[p_pos].f_estado != 30 AND operacion27[p_pos].f_estado != 35 THEN
       IF detalle02.id_expediente IS NULL THEN
          LET operacion27[p_pos].f_estado = 30        
       ELSE
          LET operacion27[p_pos].f_estado = 35 
       END IF        
    END IF
    
    #--REALIZANDO UPDATE E INSERCION
    --Primero se completa el registro a insertar en tabla det_03
    IF p_agrega_det03 THEN
        --llenando campos antes de insertar
        
        SELECT seq_sep_det_03_op27.nextval INTO detalle03.id_det_03_op27 FROM systables WHERE tabname = "sep_det_03_op27"
        
        LET detalle03.id_det_02_op27 = detalle02.id_det_02_op27
        IF detalle02.folio IS NULL THEN
           LET detalle03.folio = 999
           LET detalle02.folio = 999
           --folio id = 29
           CALL fn_inserta_his_det(2,operacion27[p_pos].f_id_detalle_02,29,"",detalle02.folio)
        ELSE
           LET detalle03.folio = detalle02.folio 
        END IF
        LET detalle03.tipo_registro = "03"
        LET detalle03.contador_servicio = detalle02.contador_servicio + 1
        LET detalle03.asociado = operacion27[p_pos].f_asociado
        LET detalle03.id_derechohabiente_asociado = p_id_dh
        LET detalle03.tipo_entidad_asociado = "00"
        LET detalle03.cve_entidad_asociado = "000"
        LET detalle03.resultado_operacion = "01"
        LET detalle03.id_expediente = detalle02.id_expediente
        --SE PROCEDE A INSERTAR REGISTRO
        INSERT INTO sep_det_03_op27 VALUES detalle03.*
    ELSE --- Se obtiene un temporal de det03 y se actualiza el asociado
       SELECT * INTO detalle03.* FROM sep_det_03_op27
       WHERE id_det_02_op27 = operacion27[p_pos].f_id_detalle_02
       IF operacion27[p_pos].f_asociado != detalle03.asociado THEN --Se actualiza solo si hubo cambios
          UPDATE sep_det_03_op27
             SET asociado = operacion27[p_pos].f_asociado,
                id_derechohabiente_asociado = p_id_dh
             WHERE id_det_02_op27 = operacion27[p_pos].f_id_detalle_02
          --id 31 = asociado--
          CALL fn_inserta_his_det(3,detalle03.id_det_03_op27,31,detalle03.asociado,operacion27[p_pos].f_asociado)
       END IF
    END IF

    --Ahora se realiza el UPDATE correspondiente para la tabla sep_det_02
    UPDATE sep_det_02_op27
       SET resultado_operacion  = operacion27[p_pos].f_resultado,
           diag_confronta       = operacion27[p_pos].f_diagnostico,
           clasifica_separacion = operacion27[p_pos].f_clasifica,
           folio                = detalle02.folio,
           estado               = operacion27[p_pos].f_estado
       WHERE id_det_02_op27 = operacion27[p_pos].f_id_detalle_02
    
    #--INSERTANDO VALORES EN TABLA HISTORICA, solo si cambiaron
    --resultado Operacion id = 20
    IF detalle02.resultado_operacion != operacion27[p_pos].f_resultado THEN 
        CALL fn_inserta_his_det(2,operacion27[p_pos].f_id_detalle_02,20,detalle02.resultado_operacion,operacion27[p_pos].f_resultado)
    END IF

    --diag_confronta id = 17
    IF detalle02.diag_confronta != operacion27[p_pos].f_diagnostico THEN 
        CALL fn_inserta_his_det(2,operacion27[p_pos].f_id_detalle_02,17,detalle02.diag_confronta,operacion27[p_pos].f_diagnostico)
    END IF

    --clasifica_separacion id = 18
    IF detalle02.clasifica_separacion != operacion27[p_pos].f_clasifica THEN 
        CALL fn_inserta_his_det(2,operacion27[p_pos].f_id_detalle_02,18,detalle02.clasifica_separacion,operacion27[p_pos].f_clasifica)
    END IF

    --estado id = 28
    IF detalle02.estado != operacion27[p_pos].f_estado THEN 
        CALL fn_inserta_his_det(2,operacion27[p_pos].f_id_detalle_02,28,detalle02.estado,operacion27[p_pos].f_estado)
    END IF

    --FIN DEL PROCEDIMIENTO
    CALL fn_mensaje(p_cad_ventana,"Se ha modificado el registro","about")

END FUNCTION

FUNCTION fn_inserta_his_det(tabla_det,id_det0x,id_dato,p_modificado,p_actual)
    DEFINE tabla_det    SMALLINT
    DEFINE id_det0x     DECIMAL(9,0)
    DEFINE id_dato      SMALLINT
    DEFINE p_modificado CHAR(40)
    DEFINE p_actual     CHAR(40)

    IF tabla_det == 2 THEN
        INSERT INTO sep_his_det_02_op27 VALUES ((SELECT MAX(id_his_op27) + 1 FROM sep_his_det_02_op27),id_det0x,id_dato,TODAY,p_modificado,p_actual,p_usuario_cod)
    END IF

    IF tabla_det == 3 THEN
        INSERT INTO sep_his_det_03_op27 VALUES ((SELECT MAX(id_his_03_op27) + 1 FROM sep_his_det_03_op27),id_det0x,id_dato,TODAY,p_modificado,p_actual,p_usuario_cod)
    END IF

END FUNCTION