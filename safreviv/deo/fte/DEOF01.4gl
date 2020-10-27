--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================
#########################################################################################
#Modulo       => DEO                                                                    #
#Programa     => DEOF01                                                                 #
#Objetivo     => Programa para capturar los montos para devolucion por afore para la    #
#                devolucion por errores de operacion                                    #
#Fecha inicio => Diciembre 28, 2011                                                     #
#########################################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS
GLOBALS "DEOG01.4gl"

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana
       ,v_res_valida SMALLINT

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)


   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = g_proceso_cod_deo -- devolucion por errores de operacion

   -- se invoca la captura de saldos para devolucion por errores de operacion
   CALL fn_captura_saldos_devolucion_afores(p_usuario_cod)
END MAIN

{ ======================================================================
Clave: DEOF01
Nombre: fn_captura_saldos_devolucion_afores
Fecha creacion: Diciembre 28, 2011
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Presentan en pantalla un formulario que permite al usuario capturar una fecha
y un importe de devolucion por afore para el infonavit por concepto
de errores de operacion

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_captura_saldos_devolucion_afores(p_usuario_cod)
DEFINE p_usuario_cod           LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       v_r_cat_afore           RECORD LIKE cat_afore.*, -- registro de afore
       v_ar_saldos_capturados  DYNAMIC ARRAY OF RECORD
        afore_cod               LIKE cat_afore.afore_cod, -- codigo de la AFORE
        afore_desc              LIKE cat_afore.afore_desc, -- descripcion de la AFORE
        importe                 LIKE deo_mto_deposito.tot_pes_devolucion, -- importe capturado
        f_devolucion            LIKE deo_mto_deposito.f_valor_devol_inf -- fecha de devolucion
       END RECORD,
       v_b_continuar           SMALLINT, -- booleana para continuar ejecucion
       v_cbx_afores            ui.ComboBox, -- combo con las afores
       v_si_indice             SMALLINT, -- indice de arreglo
       v_afore_cod             LIKE cat_afore.afore_cod, -- codigo de afore capturado
       v_importe               LIKE deo_mto_deposito.tot_pes_devolucion, -- importe capturado por afore
       v_fecha                 LIKE deo_mto_deposito.f_valor_devol_inf -- fecha de devolucion
       ,v_c_fecha              CHAR(10)
       ,v_i_contador           INTEGER
       ,v_s_qryCons            STRING
       ,v_fecha_devol          LIKE deo_mto_deposito.f_valor_devol_inf -- fecha de devolucion
       

   -- se asume que se continuara el proceso
   LET v_b_continuar = TRUE

   -- se abre la ventana de captura
   OPEN WINDOW w_captura_saldos WITH FORM "DEOF010"

   -- se prepara y ejecuta la consulta
   --PREPARE sid_afores FROM v_sql
   DECLARE cur_afores CURSOR FOR 
   SELECT *
   FROM cat_afore
   ORDER BY
    afore_desc
   
   -- se le asigna el apuntado der combo a la variable
   LET v_cbx_afores = ui.ComboBox.forName("formonly.cmb_afore")
   
   -- se inicia el combobox en blanco
   CALL v_cbx_afores.clear()
   
   -- el primer elemento en el combo es la cadena que indica que se debe elegir
   -- una afore
   CALL v_cbx_afores.addItem(-1," ")
   
   -- se llena el combo con los datos del catalogo de afores
   FOREACH cur_afores INTO v_r_cat_afore.*
      -- se agrega la afore al combo
      CALL v_cbx_afores.addItem(v_r_cat_afore.afore_cod,v_r_cat_afore.afore_desc)
   END FOREACH
   
   -- se libera el cursor
   FREE cur_afores
   
   -- se inicia un DIALOG para capturar los datos y presentar en pantalla
   -- el indice del arreglo es 1
   LET v_si_indice = 1
   -- se borra el arreglo de saldos capturados
   CALL v_ar_saldos_capturados.clear()
   DIALOG 
     ATTRIBUTES ( UNBUFFERED=TRUE)
   
     -- input para la captura de los saldos por AFORE
     INPUT
      v_afore_cod,
      v_importe  ,
      v_fecha     
     FROM
      cmb_afore,
      txt_importe,
      dte_fecha
     ATTRIBUTES ( WITHOUT DEFAULTS )
   
        -- antes de comenzar la captura
        BEFORE INPUT
           CALL DIALOG.setActionHidden("close",TRUE)
           CALL DIALOG.setActionActive("close",FALSE)
           
           -- se dan los valores por omision
           LET v_afore_cod = -1 -- -1 es el valor del registro default
           LET v_importe   = 0 
           LET v_fecha     = TODAY
           LET v_s_qryCons = "EXECUTE PROCEDURE sp_obtiene_fecha_mes_siguiente_primero()"
           PREPARE Prpr_ObtDiaNaturalMesSiguiente FROM v_s_qryCons CLIPPED
           EXECUTE Prpr_ObtDiaNaturalMesSiguiente INTO v_fecha
           LET v_fecha_devol = v_fecha
         
        AFTER FIELD DTE_FECHA
           NEXT FIELD CMB_AFORE

        AFTER INPUT
           CONTINUE DIALOG
           
     END INPUT
   
     -- se despliega el arreglo de saldos capturados
     DISPLAY ARRAY v_ar_saldos_capturados TO tbl_saldos_capturados.* 
       ATTRIBUTES (KEEP CURRENT ROW =TRUE)--, ACCEPT = FALSE)

       AFTER DISPLAY
          NEXT FIELD CMB_AFORE

     END DISPLAY
           
     ON ACTION aceptar
        -- si no se tiene al menos un registro capturado
        IF ( v_ar_saldos_capturados.getLength() < 1 ) THEN
           CALL fn_mensaje("Atención","No tiene saldos capturados","stop")
           CONTINUE DIALOG
        END IF

        -- Se acepta la captura, se procede a insertar los datos capturados en base de datos
        FOR v_si_indice = 1 TO v_ar_saldos_capturados.getLength()
           -- se invoca la insercion del registro
           CALL fn_inserta_saldo_capturado(v_ar_saldos_capturados[v_si_indice].*,p_usuario_cod)
        END FOR

        CALL fn_mensaje("Atención",
                        "Los montos capturados por AFORE han sido almacenados con éxito",
                        "information")
        EXIT DIALOG
     
     ON ACTION CANCEL
        IF ( fn_ventana_confirma("Cancelar captura de saldos",
        	   "¿Desea cancelar la captura de saldos? Los datos capturados se perderán",
        	   "question" ) ) THEN
           LET v_b_continuar = FALSE
           EXIT DIALOG
        ELSE
           CONTINUE DIALOG
        END IF
       
     ON ACTION agrega
        -- el usuario debio elegir al menos una afore
        IF ( v_afore_cod = -1 ) THEN
           CALL fn_mensaje("Atención","Debe elegir una AFORE","stop")
           CONTINUE DIALOG
        END IF
        
        -- el importe debe ser mayor a cero
        IF ( v_importe <= 0 ) THEN
           CALL fn_mensaje("Atención","El importe debe ser mayor a cero","stop")
           CONTINUE DIALOG
        END IF
        
        -- la fecha no puede ser posterior a la fecha del dia
        IF ( v_fecha < '07/01/1997' OR v_fecha IS NULL) THEN
           CALL fn_mensaje("Atención",
                "La fecha no puede ser menor a 01/07/1997","stop")
           CONTINUE DIALOG
        END IF
        -- Corrige formato fecha
        LET v_c_fecha = v_fecha
        LET v_c_fecha = v_c_fecha[4,5],v_c_fecha[1,2],v_c_fecha[7,10]
        LET v_i_contador = 0

        SELECT COUNT(*)
          INTO v_i_contador
          FROM deo_mto_deposito
         WHERE cve_afore = v_afore_cod
           AND f_valor_devol_inf = v_fecha
    
        IF (v_i_contador IS NULL OR v_i_contador <=0) THEN
           -- Verificar que no este tampoco en la lista actual.
           FOR v_si_indice = 1 TO v_ar_saldos_capturados.getLength()
              IF(v_ar_saldos_capturados[v_si_indice].afore_cod = v_afore_cod
                 AND v_ar_saldos_capturados[v_si_indice].f_devolucion  = v_fecha)THEN
                 -- Indica que ya existe en la lista. Se rechaza la captura.
                 CALL fn_mensaje("Atención",
                      "Fecha y Afore no válidas \n,"||
                      " ya han sido capturadas en la lista actual"
                      ,"info")
           CONTINUE DIALOG
              END IF
           END FOR
           -- No existe, continua y agrega el registro
           LET v_si_indice = v_ar_saldos_capturados.getLength() + 1        
           -- si no hubo error en la captura se inserta el registro en el arreglo
           -- de saldos capturados
           LET v_ar_saldos_capturados[v_si_indice].afore_cod     = v_afore_cod
           -- se obtiene la descripcion de la AFORE elegida desde el combo
           LET v_ar_saldos_capturados[v_si_indice].afore_desc    = v_cbx_afores.getItemText(v_cbx_afores.getIndexOf(v_afore_cod))
           LET v_ar_saldos_capturados[v_si_indice].importe       = v_importe
           LET v_ar_saldos_capturados[v_si_indice].f_devolucion  = v_fecha
        
           -- se reinicia el formulario
           LET v_afore_cod = -1 -- -1 es el valor del registro default
           LET v_importe   = 0 
           LET v_fecha     = v_fecha_devol
           NEXT FIELD cmb_afore
        ELSE
           CALL fn_mensaje("Atención",
                "Fecha y Afore no válidas \n, ya han sido capturadas con anteriodad"
                ,"info")
           NEXT FIELD cmb_afore
        END IF
       
     ON ACTION elimina
        -- si no se tiene al menos un renglon capturado
        IF ( v_ar_saldos_capturados.getLength() < 1 ) THEN
           CALL fn_mensaje("Atención","No tiene saldos capturados","stop")
           CONTINUE DIALOG
        END IF
        
        -- se obtiene el indice del arreglo
        LET v_si_indice = ARR_CURR()
        
        -- se borra el elemento del indice
        CALL v_ar_saldos_capturados.deleteElement(v_si_indice)
     
     ON ACTION modifica
        IF ( v_ar_saldos_capturados.getLength() < 1 ) THEN
           --CALL fn_mensaje("Atención","No tiene saldos capturados","stop")
           CONTINUE DIALOG
        END IF
        LET v_si_indice = ARR_CURR()
        -- Obtiene los datos del registro seleccionado y los pone en la seccion de captura de datos.
        LET v_afore_cod = v_ar_saldos_capturados[v_si_indice].afore_cod
        LET v_importe   = v_ar_saldos_capturados[v_si_indice].importe
        LET v_fecha     = v_ar_saldos_capturados[v_si_indice].f_devolucion
        
        ## INPUT de modificacion
        INPUT v_afore_cod, v_importe, v_fecha     
         FROM cmb_afore, txt_importe, dte_fecha 
          ATTRIBUTES ( WITHOUT DEFAULTS, UNBUFFERED=TRUE, ACCEPT = FALSE)

           BEFORE INPUT
              CALL DIALOG.setActionHidden("close",TRUE)
              CALL DIALOG.setActionActive("close",FALSE)
        
           AFTER FIELD DTE_FECHA
              NEXT FIELD CMB_AFORE
           
           ON ACTION modifica
              -- Valida datos capturados
              IF ( v_afore_cod = -1 ) THEN
                 CALL fn_mensaje("Atención","Debe elegir una AFORE","stop")
                 CONTINUE INPUT
              END IF
              -- El importe debe ser mayor a cero
              IF ( v_importe <= 0 ) THEN
                 CALL fn_mensaje("Atención","El importe debe ser mayor a cero","stop")
                 CONTINUE INPUT
              END IF
              
              -- la fecha no puede ser posterior a la fecha del dia
              IF ( v_fecha < '07/01/1997' OR v_fecha IS NULL) THEN
                 CALL fn_mensaje("Atención",
                      "La fecha no puede ser menor a 01/07/1997","stop")
                 CONTINUE DIALOG
              END IF


              -- Corrige formato fecha
              LET v_c_fecha = v_fecha
              LET v_c_fecha = v_c_fecha[4,5],v_c_fecha[1,2],v_c_fecha[7,10]
              LET v_i_contador = 0
              
              SELECT COUNT(*)
                INTO v_i_contador
                FROM deo_mto_deposito
               WHERE cve_afore = v_afore_cod
                 AND f_valor_devol_inf = v_fecha
              
              IF(v_i_contador IS NULL OR v_i_contador <=0)THEN
                 -- Verificar que no este tampoco en la lista actual.
                 FOR v_i_contador = 1 TO v_ar_saldos_capturados.getLength()
                    IF(v_ar_saldos_capturados[v_i_contador].afore_cod = v_afore_cod
                       AND v_ar_saldos_capturados[v_i_contador].f_devolucion  = v_fecha
                       AND v_i_contador <> v_si_indice)THEN
                       -- Indica que ya existe en la lista. Se rechaza la captura.
                       CALL fn_mensaje("Atención",
                            "Fecha y Afore no válidas \n, ya han sido capturadas en la lista actual"
                           ,"info")
                       CONTINUE INPUT
                    END IF
                 END FOR
                 -- Realiaza cambio
                 LET v_ar_saldos_capturados[v_si_indice].afore_cod    = v_afore_cod
                 LET v_ar_saldos_capturados[v_si_indice].afore_desc   = v_cbx_afores.getItemText(v_cbx_afores.getIndexOf(v_afore_cod))
                 LET v_ar_saldos_capturados[v_si_indice].importe      = v_importe
                 LET v_ar_saldos_capturados[v_si_indice].f_devolucion = v_fecha
                 LET INT_FLAG = FALSE
                 LET v_fecha = v_fecha_devol
                 EXIT INPUT
                 
                 
              ELSE
                 CALL fn_mensaje("Atención",
                      "Fecha y Afore no válidas \n, ya han sido capturadas con anteriodad"
                      ,"info")
                 NEXT FIELD cmb_afore
              END IF         
        
        END INPUT
        -- Limpia datos de elemento modificado
        LET v_afore_cod = -1 -- -1 es el valor del registro default
        LET v_importe   = 0 
        LET v_fecha     = TODAY

   END DIALOG

   -- se cierra la ventana
   CLOSE WINDOW w_captura_saldos
   
   IF ( NOT v_b_continuar ) THEN
      RETURN
   END IF
 
END FUNCTION

{ ======================================================================
Clave: DEOF01
Nombre: fn_captura_saldos_devolucion_afores
Fecha creacion: Diciembre 28, 2011
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Presentan en pantalla un formulario que permite al usuario capturar una fecha
y un importe de devolucion por afore para el infonavit por concepto
de errores de operacion

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_inserta_saldo_capturado(p_r_saldo_capturado,p_usuario_cod)
DEFINE p_r_saldo_capturado  RECORD
        afore_cod             LIKE cat_afore.afore_cod, -- codigo de la AFORE
        afore_desc            LIKE cat_afore.afore_desc, -- descripcion de la AFORE
        importe               LIKE deo_mto_deposito.tot_pes_devolucion, -- importe capturado
        f_devolucion          LIKE deo_mto_deposito.f_valor_devol_inf -- fecha de devolucion
       END RECORD,
       p_usuario_cod           LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       --
       v_r_deo_mto_deposito RECORD LIKE deo_mto_deposito.* -- registro de monto para devolucion de afore

   -- se transfieren los datos del registro enviado al registro de insercion
   LET v_r_deo_mto_deposito.cve_afore          = p_r_saldo_capturado.afore_cod
   LET v_r_deo_mto_deposito.f_valor_devol_inf  = p_r_saldo_capturado.f_devolucion
   LET v_r_deo_mto_deposito.tot_pes_devolucion = p_r_saldo_capturado.importe
   LET v_r_deo_mto_deposito.estado_devolucion  = 1 -- capturado
   LET v_r_deo_mto_deposito.f_captura          =  TODAY
   LET v_r_deo_mto_deposito.usuario_captura    =  p_usuario_cod
   
   
   -- se inserta el registro en base
   INSERT INTO deo_mto_deposito 
   (f_valor_devol_inf, cve_afore, tot_pes_devolucion, estado_devolucion
    ,f_captura,usuario_captura)
    VALUES(v_r_deo_mto_deposito.f_valor_devol_inf, v_r_deo_mto_deposito.cve_afore,
           v_r_deo_mto_deposito.tot_pes_devolucion, v_r_deo_mto_deposito.estado_devolucion,
           v_r_deo_mto_deposito.f_captura, v_r_deo_mto_deposito.usuario_captura)

END FUNCTION
