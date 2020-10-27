--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================
#########################################################################################
#Modulo       => DEO                                                                    #
#Programa     => DEOF02                                                                 #
#Objetivo     => Programa que permite consultar y modificar los saldos capturados de    #
#                devolucion por errores de operacion por AFORE                          #
#Fecha inicio => Diciembre 28, 2011                                                     #
#########################################################################################
DATABASE safre_viv
GLOBALS "DEOG01.4gl"

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
   
   -- se invoca la captura de saldos para devolucion por errores de operacion
   CALL fn_captura_fechas_devolucion(p_usuario_cod)

END MAIN

{ ======================================================================
Clave: DEOF01
Nombre: fn_captura_fechas_devolucion
Fecha creacion: Febrero 10, 2012
Autor: Felipe Nava, EFP
Narrativa del proceso que realiza:

Rutina inicial de consultas de Saldos de Montos Depositados.
 # -> como primer paso permite capturar periodos de consulta
      para las fechas de devolucion y de captura.
 # -> como segundo paso y para continuar con la consulta, 
      realiza llamada de consulta de fechas agrupadas por si mismas
      como fecha de captura y de devolucion
 # -> el siguiente paso obtiene una de las fechas de la consulta
      realiza llamado a rutina para realizar modificaciones a los
      saldos capturados anteriormente en módulo de capturas
      de saldos.
 
 Parametros entrada: periodos de fechas de devolucion y de captura
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_captura_fechas_devolucion(p_usuario_cod)
  DEFINE
   p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   --
   ,v_c_fecha_captura_ini    DATE
   ,v_c_fecha_captura_fin    DATE
   ,v_c_fecha_devolucion_ini DATE
   ,v_c_fecha_devolucion_fin DATE
   --
   ,v_s_qryCons STRING
   
   LET INT_FLAG = FALSE
   
   OPEN WINDOW win_consulta_saldos WITH FORM "DEOF020"

   -- Se realiza captura de criterios de consulta
   INPUT v_c_fecha_captura_ini, v_c_fecha_captura_fin,
         v_c_fecha_devolucion_ini, v_c_fecha_devolucion_fin
    FROM FECHA_CAPTURA_INI, FECHA_CAPTURA_FIN, 
         FECHA_DEVOLUCION_INI, FECHA_DEVOLUCION_FIN

      BEFORE INPUT
         
         LET v_s_qryCons = "EXECUTE PROCEDURE sp_obtiene_fecha_mes_siguiente_primero()"
         PREPARE Prpr_ObtDiaNaturalMesSiguiente FROM v_s_qryCons CLIPPED
         EXECUTE Prpr_ObtDiaNaturalMesSiguiente INTO v_c_fecha_devolucion_ini
         
         LET v_c_fecha_devolucion_fin = v_c_fecha_devolucion_ini
         DISPLAY v_c_fecha_devolucion_ini, v_c_fecha_devolucion_fin
              TO FECHA_DEVOLUCION_INI, FECHA_DEVOLUCION_FIN

      AFTER INPUT
         -- Valida los datos de los periodos
         IF(v_c_fecha_captura_ini IS NOT NULL OR v_c_fecha_captura_fin IS NOT NULL)THEN
           -- Verificar que ambos campos tengan datos
           IF (v_c_fecha_captura_ini IS NULL OR v_c_fecha_captura_fin IS NULL)THEN
              -- Se deben indicar ambos campos
              CALL fn_mensaje("Atención",
                   "Se deben indicar los dos campos de Fecha Captura","info")
              CONTINUE INPUT
           END IF
         END IF
         
         IF(v_c_fecha_devolucion_ini IS NOT NULL 
            OR v_c_fecha_devolucion_fin IS NOT NULL)THEN
           -- Verificar que ambos campos tengan datos
           IF (v_c_fecha_devolucion_ini IS NULL 
              OR v_c_fecha_devolucion_fin IS NULL)THEN
              -- Se deben indicar ambos campos
              CALL fn_mensaje("Atención",
                   "Se deben indicar los dos campos de Fecha Captura","info")
              CONTINUE INPUT
           END IF
         END IF
         -- Termina captura y se continua con la consulta
         
         CALL fn_consulta_saldos_agrupados(
              v_c_fecha_captura_ini, v_c_fecha_captura_fin,
              v_c_fecha_devolucion_ini, v_c_fecha_devolucion_fin, p_usuario_cod)

         LET INT_FLAG = FALSE
         EXIT INPUT
      
      ON ACTION CANCEL
         -- Se cancela consulta
         LET INT_FLAG = TRUE
         EXIT INPUT
      
   END INPUT
   
   -- Abrir ventana con consulta de datos encontrados.
   
   CLOSE WINDOW win_consulta_saldos
   
END FUNCTION -- fn_captura_fechas_devolucion


{ ======================================================================
Clave: DEOF01
Nombre: fn_consulta_saldos_agrupados
Fecha creacion: Febrero 10, 2012
Autor: Felipe Nava, EFP
Narrativa del proceso que realiza:
Rutina que permite mostrar resultados de consulta de fechas de 
  devolucion y fechas de captura
 
 Parametros entrada: periodos de fechas de devolucion y de captura
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_saldos_agrupados(
    p_fecha_consulta_ini, p_fecha_consulta_fin, 
    p_fecha_devolucion_ini, p_fecha_devolucion_fin, p_usuario_cod  )
 DEFINE
    p_fecha_consulta_ini   CHAR(10)
    ,p_fecha_consulta_fin   CHAR(10)
    ,p_fecha_devolucion_ini CHAR(10)
    ,p_fecha_devolucion_fin CHAR(10)
    ,p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
    ,v_s_qry                STRING
    ,v_i_posicion           INTEGER
    ,v_c_fecha_captura    CHAR(10)
    ,v_c_fecha_devolucion CHAR(10)
    ,v_si_contador        SMALLINT  
    ,v_ar_fechas_rango DYNAMIC ARRAY OF RECORD
        v_c_fecha_captura    CHAR(10)
       ,v_c_fecha_devolucion CHAR(10)
       ,v_si_contador        SMALLINT  
    END RECORD
   
   # -> Armar consulta
   LET v_s_qry =
       "SELECT f_captura, f_valor_devol_inf, COUNT(*)"
       ,"\n  FROM deo_mto_deposito"
       ,"\n WHERE estado_devolucion = 1"

   IF(p_fecha_consulta_ini IS NOT NULL)THEN
      -- Consulta por fecha de captura
      LET v_s_qry = v_s_qry CLIPPED
         ,"\n   AND f_captura BETWEEN '", p_fecha_consulta_ini,"'"
         ,"\n       AND '", p_fecha_consulta_fin,"'"
   END IF
   
   IF(p_fecha_devolucion_ini IS NOT NULL)THEN
      -- Consulta por fecha de devolucion
      LET v_s_qry = v_s_qry CLIPPED
         ,"\n   AND f_valor_devol_inf BETWEEN '", p_fecha_devolucion_ini,"'"
         ,"\n       AND '", p_fecha_devolucion_fin,"'"
   END IF
   
   LET v_s_qry = v_s_qry CLIPPED
       ,"\n   GROUP BY 1,2 ORDER BY 2"

   --DISPLAY v_s_qry CLIPPED
   PREPARE Prpr_ObtDatFechas FROM v_s_qry CLIPPED
   DECLARE Curr_ObtDatFechas CURSOR FOR Prpr_ObtDatFechas
   
   LET v_i_posicion = 0
   FOREACH Curr_ObtDatFechas 
      INTO v_c_fecha_captura, v_c_fecha_devolucion, v_si_contador
    
    LET v_i_posicion = v_i_posicion + 1
    LET v_ar_fechas_rango[v_i_posicion].v_c_fecha_captura    = v_c_fecha_captura
    LET v_ar_fechas_rango[v_i_posicion].v_c_fecha_devolucion = v_c_fecha_devolucion
    LET v_ar_fechas_rango[v_i_posicion].v_si_contador        = v_si_contador   
   END FOREACH
   
   IF(v_i_posicion <= 0)THEN
      -- Indica que NO encontro datos
      CALL fn_mensaje("Atención",
           "No existen datos con los criterios de consulta utilizados",
           "info")
      RETURN
   END IF
   
   -- Si hay datos continua el proceso y muestra lista de fechas
   DISPLAY ARRAY v_ar_fechas_rango TO tbl_fechas.*
   
    AFTER DISPLAY
       -- Llama rutina para ver consulta de datos de fecha devolucion.
       --  con la fecha seleccionada
       CALL fn_consulta_saldos_devolucion_afores(
            v_ar_fechas_rango[ARR_CURR()].v_c_fecha_captura,
            v_ar_fechas_rango[ARR_CURR()].v_c_fecha_devolucion, p_usuario_cod)
       EXIT DISPLAY
    
    ON ACTION CANCEL
       EXIT DISPLAY
   
   END DISPLAY

END FUNCTION --fn_consulta_saldos_agrupados

{ ======================================================================
Clave: DEOF01
Nombre: fn_consulta_saldos_devolucion_afores
Fecha creacion: Febrero 10, 2012
Autor: Felipe Nava, EFP
Narrativa del proceso que realiza:
Rutina que permite modificar datos de saldos de devolucones
 capturadas anteriormente.
 Solo modifica datos para una fecha de devolucion o captura.

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_saldos_devolucion_afores(
         f_fecha_captura, f_fecha_devolucion, p_usuario_cod)
DEFINE f_fecha_captura         DATE,
       f_fecha_devolucion      DATE,
       p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       v_r_cat_afore           RECORD LIKE cat_afore.*, -- registro de afore
       v_ar_saldos_capturados  DYNAMIC ARRAY OF RECORD
        afore_cod               LIKE cat_afore.afore_cod, -- codigo de la AFORE
        afore_desc              LIKE cat_afore.afore_desc, -- descripcion de la AFORE
        tot_pes_devolucion      LIKE deo_mto_deposito.tot_pes_devolucion, -- importe capturado
        f_valor_devol_inf       LIKE deo_mto_deposito.f_valor_devol_inf -- fecha de devolucion
       END RECORD,
       v_r_saldos_capturados  RECORD
        afore_cod               LIKE cat_afore.afore_cod, -- codigo de la AFORE
        afore_desc              LIKE cat_afore.afore_desc, -- descripcion de la AFORE
        tot_pes_devolucion      LIKE deo_mto_deposito.tot_pes_devolucion, -- importe capturado
        f_valor_devol_inf       LIKE deo_mto_deposito.f_valor_devol_inf -- fecha de devolucion
       END RECORD,
       v_b_continuar           SMALLINT, -- booleana para continuar ejecucion
       v_cbx_afores            ui.ComboBox, -- combo con las afores
       v_si_indice             SMALLINT, -- indice de arreglo
       v_afore_cod             LIKE cat_afore.afore_cod, -- codigo de afore capturado
       v_tot_pes_devolucion    LIKE deo_mto_deposito.tot_pes_devolucion, -- importe capturado por afore
       v_fecha                 LIKE deo_mto_deposito.f_valor_devol_inf -- fecha de devolucion
       ,v_s_qry                STRING
       ,v_s_clavesFuera        STRING
       ,v_s_clavesElimina      STRING
       ,v_si_cont              SMALLINT


   -- se asume que se continuara el proceso
   LET v_b_continuar = TRUE

   -- se abre la ventana de captura
   OPEN WINDOW w_captura_saldos WITH FORM "DEOF021"

   -- se prepara y ejecuta la consulta
   --PREPARE sid_afores FROM v_sql
   DECLARE cur_afores CURSOR FOR 
   SELECT *
     FROM cat_afore
    ORDER BY afore_desc
   
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
   
   CALL v_ar_saldos_capturados.clear()
   -- Cargar saldos capturados con estatus 1 la la fecha seleccionada
   LET v_s_qry =
       "SELECT m.cve_afore, a.afore_desc, m.tot_pes_devolucion, m.f_valor_devol_inf"
       ,"\n  FROM deo_mto_deposito m, cat_afore a"
       ,"\n WHERE m.estado_devolucion = 1"
       ,"\n   AND m.cve_afore = a.afore_cod"
   
   IF(f_fecha_captura IS NOT NULL)THEN
      -- Consulta por fecha de captura
      LET v_s_qry = v_s_qry CLIPPED
          ,"\n   AND m.f_captura = '", f_fecha_captura,"' "
   END IF
   
   IF(f_fecha_devolucion IS NOT NULL)THEN
      -- Consulta por fecha de devolucion
      LET v_s_qry = v_s_qry CLIPPED
          ,"\n   AND m.f_valor_devol_inf = '", f_fecha_devolucion,"'"
   END IF
   
   LET v_s_qry = v_s_qry CLIPPED
          ,"\n   ORDER BY 1,4"
   PREPARE Prpr_OBtDatoSaldoDeps FROM v_s_qry CLIPPED
   DECLARE Curr_OBtDatoSaldoDeps CURSOR FOR Prpr_OBtDatoSaldoDeps
   
   LET v_si_indice = 0
   ## Cadena claves fuera
   LET v_s_clavesFuera = ""
   ## Cadena clave elimina
   LET v_s_clavesElimina = "1=1"
   --DISPLAY v_s_qry
   
   FOREACH Curr_OBtDatoSaldoDeps INTO v_r_saldos_capturados.*
      LET v_si_indice = v_si_indice + 1
      LET v_ar_saldos_capturados[v_si_indice].* = v_r_saldos_capturados.*
      ## Lista de clave procesadas
      LET v_s_clavesFuera = v_s_clavesFuera CLIPPED
          ,"\n AND NOT(cve_afore = ",v_r_saldos_capturados.afore_cod
          ,"\n AND f_valor_devol_inf = '",v_r_saldos_capturados.f_valor_devol_inf,"')"
      ## Arma cadena para quitar los registros anteriores.
      IF(v_s_clavesElimina CLIPPED = "1=1")THEN
         LET v_s_clavesElimina = 
             "(cve_afore = ",v_r_saldos_capturados.afore_cod
             ,"\n AND f_valor_devol_inf = '",v_r_saldos_capturados.f_valor_devol_inf,"')"
      ELSE
         LET v_s_clavesElimina = 
             v_s_clavesElimina CLIPPED, " OR "
             ,"\n (cve_afore = ",v_r_saldos_capturados.afore_cod
             ,"\n AND f_valor_devol_inf = '",v_r_saldos_capturados.f_valor_devol_inf,"')"
      END IF
   END FOREACH
   IF(v_si_indice <= 0)THEN
      CALL fn_mensaje("Atención",
           "No se encontró información con los criterios indicados","info")
      CLOSE WINDOW w_captura_saldos
      RETURN
   END IF

   DISPLAY f_fecha_captura, f_fecha_devolucion TO FECHA_CAPTURA, FECHA_DEVOLUCION
   -- se despliega el arreglo de saldos capturados
   DISPLAY ARRAY v_ar_saldos_capturados TO tbl_saldos_capturados.* 
     ATTRIBUTES (KEEP CURRENT ROW =TRUE, ACCEPT = FALSE, CANCEL = FALSE)
       
     -- Realiza todas las actualizaciones encontradas.
     --ON ACTION GuardarCambios
     --ON ACTION ACCEPT
     ON ACTION aceptar
        -- si no se tiene al menos un registro capturado
        IF ( v_ar_saldos_capturados.getLength() < 1 ) THEN
           CALL fn_mensaje("Atención","No tiene saldos capturados","stop")
           CONTINUE DISPLAY
        END IF
        
        # -> 1. Se todos los eliminan los datos anteriores.
        LET v_s_qry = 
            " DELETE"
            ,"\n  FROM deo_mto_deposito"
            ,"\n WHERE estado_devolucion = 1"
            ,"\n   AND (",v_s_clavesElimina CLIPPED,")"
        PREPARE Prpr_ElimDatAnt FROM v_s_qry CLIPPED
        EXECUTE Prpr_ElimDatAnt
        
        # -> 2. Se insertan los nuevos datos capturados recientemente.
        
        -- Se acepta la captura, se procede a insertar los datos capturados en base de datos
        FOR v_si_indice = 1 TO v_ar_saldos_capturados.getLength()
           -- se invoca la insercion del registro
           CALL fn_inserta_saldo_modificado(v_ar_saldos_capturados[v_si_indice].*,p_usuario_cod)
        END FOR
        
        # -> Los datos eliminados de la lista simplemente no se vuelven a insertar.
        
        CALL fn_mensaje("Atención",
             "Los montos capturados por AFORE han sido almacenados con éxito",
             "information")
        EXIT DISPLAY
     
     ON ACTION cancelar
        IF ( fn_ventana_confirma("Cancelar captura de saldos",
        	   "¿Desea cancelar la Modificación de saldos?\n\n"||
            "Los cambios realzados se perderán",
        	   "question" ) ) THEN
           LET v_b_continuar = FALSE
           EXIT DISPLAY
        ELSE
           CONTINUE DISPLAY
        END IF
       
     ON ACTION elimina
        -- si no se tiene al menos un renglon capturado
        IF ( v_ar_saldos_capturados.getLength() < 1 ) THEN
           CALL fn_mensaje("Atención","No tiene saldos capturados","stop")
           CONTINUE DISPLAY
        END IF
        
        -- se obtiene el indice del arreglo
        LET v_si_indice = ARR_CURR()
        
        -- se borra el elemento del indice
        CALL v_ar_saldos_capturados.deleteElement(v_si_indice)
        
        # -> Al ser eliminados de la lista ya no se vuelven a considerar
        #     al momento de realizar la inserción de los datos de la lista.
     
     ON ACTION modifica
        IF ( v_ar_saldos_capturados.getLength() < 1 ) THEN
           --CALL fn_mensaje("Atención","No tiene saldos capturados","stop")
           CONTINUE DISPLAY
        END IF
        LET v_si_indice = ARR_CURR()
        -- Obtiene los datos del registro seleccionado y los pone en la seccion de captura de datos.
        LET v_afore_cod = v_ar_saldos_capturados[v_si_indice].afore_cod
        LET v_tot_pes_devolucion   = v_ar_saldos_capturados[v_si_indice].tot_pes_devolucion
        LET v_fecha     = v_ar_saldos_capturados[v_si_indice].f_valor_devol_inf
        
        DISPLAY v_fecha TO DTE_FECHA
        ## INPUT de modificacion
        INPUT v_afore_cod, v_tot_pes_devolucion--, v_fecha     
         FROM cmb_afore, txt_importe--, dte_fecha 
          ATTRIBUTES ( WITHOUT DEFAULTS, UNBUFFERED=TRUE, ACCEPT = FALSE)

           BEFORE INPUT
              CALL DIALOG.setActionHidden("close",TRUE)
              CALL DIALOG.setActionActive("close",FALSE)
        

           AFTER FIELD TXT_IMPORTE
              NEXT FIELD CMB_AFORE
            
           ON ACTION modifica
              -- Valida datos capturados
              IF ( v_afore_cod = -1 ) THEN
                 CALL fn_mensaje("Atención","Debe elegir una AFORE","stop")
                 CONTINUE INPUT
              END IF
              -- El importe debe ser mayor a cero
              IF ( v_tot_pes_devolucion <= 0 ) THEN
                 CALL fn_mensaje("Atención","El importe debe ser mayor a cero","stop")
                 CONTINUE INPUT
              END IF
                           
              # ->  Verifica que NO exista el nuevo registro en base de datos
              IF(fn_verifica_exist_afore_fecha(v_afore_cod, v_fecha, 
                                               v_s_clavesFuera CLIPPED)=1)THEN
                 -- Indica que ya existe registro en base de datos
                 CALL fn_mensaje("Atención",
                      "Clave de [afore-fecha devolución] ya existente. "||
                      "\nClave registrada anteriormente","info")
                 CONTINUE INPUT
              END IF
              
              # -> Verificar que NO sea de los registros de la tabla.
              #     Recorrer tabla de datos.
              
              FOR v_si_cont = 1 TO v_ar_saldos_capturados.getLength()
                 IF(v_ar_saldos_capturados[v_si_cont].afore_cod = v_afore_cod
                    AND v_ar_saldos_capturados[v_si_cont].f_valor_devol_inf = v_fecha
                    AND v_si_cont <> v_si_indice)THEN
                    
                    DISPLAY "contador:",v_si_cont
                    DISPLAY "Indice:",v_si_indice
                    -- indica que ya existe esta afore en la lista
                    CALL fn_mensaje("Atención",
                      "Clave de [afore-fecha devolución] ya existente. "||
                      "\nClave existente en lista inferior","info")
                    CONTINUE INPUT
                 END IF
              END FOR
              
              # -> Cuando termina el ciclo y llega a este paso indica que 
              #     ya es una clave valida.
              
              # -> Actualizar registro.
              LET v_ar_saldos_capturados[v_si_indice].afore_cod = v_afore_cod
              LET v_ar_saldos_capturados[v_si_indice].afore_desc= 
                  v_cbx_afores.getItemText(v_cbx_afores.getIndexOf(v_afore_cod))
              LET v_ar_saldos_capturados[v_si_indice].tot_pes_devolucion   = v_tot_pes_devolucion
              LET v_ar_saldos_capturados[v_si_indice].f_valor_devol_inf = v_fecha
              LET INT_FLAG = FALSE
              EXIT INPUT
        END INPUT
        DISPLAY ARRAY v_ar_saldos_capturados TO tbl_saldos_capturados.*
           BEFORE DISPLAY 
              EXIT DISPLAY
        END DISPLAY

        CALL ui.Interface.refresh()
        -- Limpia datos de elemento modificado
        LET v_afore_cod = -1 -- -1 es el valor del registro default
        LET v_tot_pes_devolucion   = 0 
        LET v_fecha     = TODAY
        DISPLAY v_afore_cod, v_tot_pes_devolucion, v_fecha   
             TO CMB_AFORE, TXT_IMPORTE, DTE_FECHA
        
      
     --] ACTION modifica DISPLAY ARRAY

     ON ACTION agrega

        ## INPUT de captura
        LET v_afore_cod = '-1'
        LET v_tot_pes_devolucion = 0.0
        --LET v_fecha = TODAY
        -- No se modifica la fecha
        LET v_fecha = f_fecha_devolucion
        DISPLAY v_fecha TO  DTE_FECHA
        INPUT v_afore_cod, v_tot_pes_devolucion--, v_fecha     
         FROM cmb_afore, txt_importe--, dte_fecha 
          ATTRIBUTES ( WITHOUT DEFAULTS, UNBUFFERED=TRUE, ACCEPT = FALSE)

           BEFORE INPUT
              CALL DIALOG.setActionHidden("close",TRUE)
              CALL DIALOG.setActionActive("close",FALSE)
        
           AFTER FIELD TXT_IMPORTE
              NEXT FIELD CMB_AFORE
           
           ON ACTION guardar
              -- Valida datos capturados
              IF ( v_afore_cod = -1 ) THEN
                 CALL fn_mensaje("Atención","Debe elegir una AFORE","stop")
                 CONTINUE INPUT
              END IF
              -- El importe debe ser mayor a cero
              IF ( v_tot_pes_devolucion <= 0 ) THEN
                 CALL fn_mensaje("Atención","El importe debe ser mayor a cero","stop")
                 CONTINUE INPUT
              END IF
              
              # ->  Verifica que NO exista el nuevo registro en base de datos
              IF(fn_verifica_exist_afore_fecha(v_afore_cod, v_fecha, 
                                               v_s_clavesFuera CLIPPED)=1)THEN
                 -- Indica que ya existe registro en base de datos
                 CALL fn_mensaje("Atención",
                      "Clave de [afore-fecha devolución] ya existente. "||
                      "\nClave registrada anteriormente","info")
                 CONTINUE INPUT
              END IF
              
              # -> Verificar que NO sea de los registros de la tabla.
              #     Recorrer tabla de datos.
              
              FOR v_si_cont = 1 TO v_ar_saldos_capturados.getLength()
                 IF(v_ar_saldos_capturados[v_si_cont].afore_cod = v_afore_cod
                    AND v_ar_saldos_capturados[v_si_cont].f_valor_devol_inf = v_fecha)THEN
                    -- indica que ya existe esta afore en la lista
                    CALL fn_mensaje("Atención",
                      "Clave de [afore-fecha devolución] ya existente. "||
                      "\nClave existente en lista inferior","info")
                    CONTINUE INPUT
                 END IF
              END FOR
              
              # -> Cuando termina el ciclo y llega a este paso indica que 
              #     ya es una clave valida.
              
              # -> Agregar nuevo registro.
              
              LET v_si_indice = v_ar_saldos_capturados.getLength() + 1
              LET v_ar_saldos_capturados[v_si_indice].afore_cod = v_afore_cod
              LET v_ar_saldos_capturados[v_si_indice].afore_desc= 
                  v_cbx_afores.getItemText(v_cbx_afores.getIndexOf(v_afore_cod))
              LET v_ar_saldos_capturados[v_si_indice].tot_pes_devolucion   = v_tot_pes_devolucion
              LET v_ar_saldos_capturados[v_si_indice].f_valor_devol_inf = v_fecha
              LET INT_FLAG = FALSE
              EXIT INPUT
        END INPUT
        -- Limpia datos de elemento modificado
        LET v_afore_cod = -1 -- -1 es el valor del registro default
        LET v_tot_pes_devolucion   = 0 
        LET v_fecha     = TODAY
        DISPLAY v_afore_cod, v_tot_pes_devolucion, v_fecha   
             TO CMB_AFORE, TXT_IMPORTE, DTE_FECHA
        CALL ui.Interface.refresh()
      
     --] ACTION agrega DISPLAY ARRAY
     
     
   END DISPLAY

   -- se cierra la ventana
   CLOSE WINDOW w_captura_saldos
   
   IF ( NOT v_b_continuar ) THEN
      RETURN
   END IF
 
END FUNCTION -- fn_consulta_saldos_devolucion_afores


{ ======================================================================
Clave: DEOF01
Nombre: fn_verifica_exist_afore_fecha
Fecha creacion: Febrero 10, 2012
Autor: Felipe Nava, EFP
Narrativa del proceso que realiza:
Verifica si existe clave de afore-fecha devolucion para modificación de 
  de datos de Montos de Saldos Depositados

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_verifica_exist_afore_fecha(p_si_afore_cod, p_c_fecha_devol, p_s_cadena_sql)
 DEFINE
   p_si_afore_cod  SMALLINT
  ,p_c_fecha_devol CHAR(10)
  ,p_s_cadena_sql  STRING
  --
  ,v_s_qry         STRING
  ,v_si_cont       SMALLINT
 
   LET v_s_qry =
       "SELECT COUNT(*)"
       ,"\n  FROM deo_mto_deposito"
       ,"\n WHERE estado_devolucion = 1"
       ,"\n  AND cve_afore = ",p_si_afore_cod
       ,"\n  AND f_valor_devol_inf = '",p_c_fecha_devol,"'"
       ,"\n ", p_s_cadena_sql CLIPPED
       
   LET v_si_cont = 0
   PREPARE Prpr_verExiDatAfore FROM v_s_qry CLIPPED
   EXECUTE Prpr_verExiDatAfore INTO v_si_cont
   
   IF(v_si_cont<=0)THEN
      -- NO encontro registro
      RETURN 0
   END IF
 
   RETURN 1 -- ya existe la afore-fecha devolucion
END FUNCTION -- fn_verifica_exist_afore_fecha

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
FUNCTION fn_inserta_saldo_modificado(p_r_saldo_capturado, p_usuario_cod)
DEFINE p_r_saldo_capturado  RECORD
        afore_cod           LIKE cat_afore.afore_cod -- codigo de la AFORE
        ,afore_desc         LIKE cat_afore.afore_desc -- descripcion de la AFORE
        ,tot_pes_devolucion LIKE deo_mto_deposito.tot_pes_devolucion -- importe capturado
        ,f_valor_devol_inf  LIKE deo_mto_deposito.f_valor_devol_inf -- fecha de devolucion
       END RECORD
       
       ,p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       
       ,v_r_deo_mto_deposito RECORD 
          f_valor_devol_inf   LIKE deo_mto_deposito.f_valor_devol_inf
          ,cve_afore          LIKE deo_mto_deposito.cve_afore
          ,tot_pes_devolucion LIKE deo_mto_deposito.tot_pes_devolucion
          ,estado_devolucion  LIKE deo_mto_deposito.estado_devolucion
          ,f_captura          LIKE deo_mto_deposito.f_captura
          ,usuario_captura    LIKE deo_mto_deposito.usuario_captura
       END RECORD
       --,deo_mto_deposito.* -- registro de monto para devolucion de afore

   -- se transfieren los datos del registro enviado al registro de insercion
   LET v_r_deo_mto_deposito.f_valor_devol_inf  = p_r_saldo_capturado.f_valor_devol_inf
   LET v_r_deo_mto_deposito.cve_afore          = p_r_saldo_capturado.afore_cod
   LET v_r_deo_mto_deposito.tot_pes_devolucion = p_r_saldo_capturado.tot_pes_devolucion
   LET v_r_deo_mto_deposito.estado_devolucion  = 1 -- capturado
   LET v_r_deo_mto_deposito.f_captura          = TODAY
   LET v_r_deo_mto_deposito.usuario_captura    = p_usuario_cod
   DISPLAY "p_usuario_cod",p_usuario_cod
   
   -- se inserta el registro en base
   INSERT INTO deo_mto_deposito 
   (f_valor_devol_inf, cve_afore, tot_pes_devolucion, estado_devolucion, f_captura, 
    usuario_captura)
   VALUES (
           v_r_deo_mto_deposito.f_valor_devol_inf
          ,v_r_deo_mto_deposito.cve_afore        
          ,v_r_deo_mto_deposito.tot_pes_devolucion
          ,v_r_deo_mto_deposito.estado_devolucion 
          ,v_r_deo_mto_deposito.f_captura         
          ,v_r_deo_mto_deposito.usuario_captura   
       )
   
END FUNCTION --fn_inserta_saldo_modificado


