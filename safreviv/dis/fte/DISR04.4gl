################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 18/04/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISR04                                                   #
#Objetivo          => Programa de reverso de la ejecución de                   #
#                     quebranto de avances abiertos                            #
#Fecha inicio      => 18/01/2012                                               #
################################################################################
DATABASE
   safre_viv
GLOBALS
DEFINE g_sql_txt       STRING, -- Consultas
       g_usuario       VARCHAR(30), -- Almacena al usuario
       g_tipo_proceso  SMALLINT, -- Forma como ejecutara el programa
       g_nom_prog      VARCHAR(30) -- Almacena opción del menú
END GLOBALS
MAIN
DEFINE p_folio   LIKE sfr_marca_activa.folio, -- Folio marca activa
       p_fecha_marca LIKE sfr_marca_activa.f_inicio, -- Fecha de marca
       r_existe_info SMALLINT,
       p_pid         LIKE bat_ctr_proceso.pid, -- ID del proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, -- Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, -- Código de operacion
       v_des_error   CHAR(150),-- Recibe la descripción del error 
       r_bandera     SMALLINT,
       v_indice      INTEGER

DEFINE arr_Quebranto DYNAMIC ARRAY OF RECORD
       folio         LIKE sfr_marca_historica.folio,
       f_inicio      LIKE sfr_marca_historica.f_inicio,
       marca_desc    CHAR(53), 
       total_cuentas SMALLINT
END RECORD 
       
DEFINE f_ventana        ui.Window,   -- Define las propìedades de la Ventana
       f_forma          ui.Form    -- Define las propiedades de la forma

   -- Recibe valores de argumentos
   LET g_usuario = ARG_VAL(1)
   LET g_tipo_proceso = ARG_VAL(2)
   LET g_nom_prog = ARG_VAL(3)

   -- Se asigna el titulo del programa
   IF ( g_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_prog)
   END IF
LET p_proceso_cod = 903
LET p_opera_cod = 1

-- Llama la función para obtener el PID
CALL fn_max_pid(p_proceso_cod, 1) RETURNING p_pid   

-- Llama la función para validar la ejecucución del reverso
CALL fn_valida_reverso(p_pid, p_proceso_cod, p_opera_cod)
RETURNING r_bandera
             
DISPLAY "BANDERA VALIDA REVERSO", r_bandera     
--LET r_bandera = 0                  
-- Si el reverso no tiene ningún bloqueo
IF r_bandera = 0 THEN   
   
   OPEN WINDOW vtn_quebranto WITH FORM "DISR041"
      DIALOG   ATTRIBUTES(UNBUFFERED)
         INPUT p_folio,p_fecha_marca
          FROM f_folio,f_fecha_marca

            BEFORE INPUT
            LET f_ventana = ui.Window.getCurrent()
            LET f_forma = f_ventana.getForm()
            CALL f_forma.setElementHidden("btn_reverso",1)

         END INPUT

         ON ACTION ACCEPT
            -- La fecha no puede ser posterior al día de hoy
            IF p_fecha_marca > TODAY THEN
               CALL fn_mensaje("Atención","Fecha improcedente","about")
               NEXT FIELD f_fecha_marca
            END IF
            --Consulta por folio 
            CALL fn_consulta_marca_activa(p_folio,p_fecha_marca)
                 RETURNING r_existe_info
            IF r_existe_info > 0 THEN
               CALL f_forma.setElementHidden("accept",1) --Oculta
               CALL f_forma.setElementHidden("btn_reverso",0) --Muestra

               CALL fn_consulta_datos_desmarca(p_folio, p_fecha_marca)
               RETURNING arr_Quebranto, v_indice

                  -- Muestra en el detalle los registros rechazados
                  DISPLAY ARRAY arr_Quebranto TO rcd_cifras_globales.*
                  ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)               

                  --Ejecuta el reverso del quebranto
                  ON ACTION reverso
                     --CALL fn_reversa_operacion(p_pid, p_proceso_cod, 2)                     
                     --RETURNING r_bandera

                     CALL fn_reversa_operacion(p_pid, p_proceso_cod, 1)                     
                     RETURNING r_bandera

                     --Ejecuta función que llama sp de desmarca
                     CALL fn_ejecuta_reverso_quebranto(p_folio, p_fecha_marca)
                     CALL f_forma.setElementHidden("btn_reverso",1)
                     CALL f_forma.setElementHidden("accept",0)

                     CALL fn_mensaje("Reverso de Quebraton",
                                     "Se ejecuto el reverso de quebranto","about")
                     CLEAR FORM
                     
                  ON ACTION cancelar
                     CLEAR FORM
                     EXIT DISPLAY
                  END DISPLAY
            ELSE
              CALL fn_mensaje("ATENCIÓN","Información no existe","about")
            END IF
      
         ON ACTION cancelar
            EXIT DIALOG
     END DIALOG 
ELSE
   -- Si se detecta algún bloqueo
   --Se realiza consulta de descripción en base al codigo de bloqueo
   SELECT descripcion INTO v_des_error 
     FROM cat_bat_parametro_salida
    WHERE cod_salida = r_bandera
   -- Envía mensaje con la descripción del error que no permite continuar
   CALL fn_mensaje("REVERSO", v_des_error, "information")
END IF   
     
CLOSE WINDOW vtn_quebranto 

END MAIN
#Objetivo: Validar que exista información en la tabla sfr_marca_activa
FUNCTION fn_consulta_marca_activa(v_folio,v_fecha_marca)
DEFINE v_folio       LIKE sfr_marca_activa.folio, 
       v_fecha_marca LIKE sfr_marca_activa.f_inicio,
       p_existe_info SMALLINT 

   LET g_sql_txt = "\n SELECT count(*)",
                   "\n FROM sfr_marca_historica",
                   "\n WHERE 1=1 "
      IF LENGTH(v_folio) > 0 THEN
         LET g_sql_txt = g_sql_txt||"\n AND folio = ",v_folio,""
      END IF
      IF LENGTH(v_fecha_marca) > 0 THEN
         LET g_sql_txt = g_sql_txt||"\n AND f_inicio = '",v_fecha_marca,"'",
                                    "\n AND marca = 601"
      END IF
   DISPLAY "conulta ",g_sql_txt
   PREPARE prp_sql_marca_activa FROM g_sql_txt
   EXECUTE prp_sql_marca_activa INTO p_existe_info

   RETURN p_existe_info
       
END FUNCTION

FUNCTION fn_consulta_datos_desmarca(p_folio, p_fecha)
DEFINE p_folio LIKE sfr_marca_activa.folio, 
       p_fecha LIKE sfr_marca_activa.f_inicio,
       v_QryTxt STRING,
       v_indice INTEGER
       
DEFINE arr_Quebranto DYNAMIC ARRAY OF RECORD
       folio         LIKE sfr_marca_historica.folio,
       f_inicio      LIKE sfr_marca_historica.f_inicio,
       marca_desc    CHAR(53), 
       total_cuentas SMALLINT
END RECORD       

   -- Si la consulta se hace con ambos parámetros   
   LET v_QryTxt = "\n SELECT mh.folio, mh.f_inicio,",
                  "\n        mh.marca || '-' ||",
                  "\n        mr.descripcion_marca AS MARCA,",
                  "\n        count(mh.folio)",
                  "\n   FROM sfr_marca_historica mh, sfr_marca mr",
                  "\n  WHERE mh.marca = mr.marca",
                  "\n    AND mh.marca = 601"
      IF length(p_fecha) > 0 THEN
      LET v_QryTxt = v_QryTxt || "\n AND mh.f_inicio = '",p_fecha,"'"
   END IF 
   IF length(p_folio) > 0 THEN 
      LET v_QryTxt = v_QryTxt || "\n AND mh.folio = '",p_folio,"'"
   END IF                                       
   LET v_QryTxt = v_QryTxt || "\n GROUP BY 1,2,3" 

      -- Prepara la consulta para el display
   PREPARE prp_Cifras_Quebranto FROM v_QryTxt

   LET v_indice = 1
 
   -- Declara el cursor para la consulta
   DECLARE cur_Cifras_Quebranto CURSOR FOR prp_Cifras_Quebranto
   FOREACH cur_Cifras_Quebranto INTO arr_Quebranto[v_indice].folio, 
                                     arr_Quebranto[v_indice].f_inicio, 
                                     arr_Quebranto[v_indice].marca_desc,
                                     arr_Quebranto[v_indice].total_cuentas

      LET v_indice = v_indice + 1
        
   END FOREACH  

   CALL arr_Quebranto.deleteElement(v_indice)

   RETURN arr_Quebranto, v_indice

END FUNCTION


#Objetivo: Ejecutar el reverso del quebranto
FUNCTION fn_ejecuta_reverso_quebranto(p_folio, p_fecha)
DEFINE v_fn_marca_cuenta STRING,
       p_folio           DECIMAL(10,0),
       p_fecha           DATE,
       p_fecha_caus      DATE,
       p_marca_entra     SMALLINT, --Clave de la marca Quebranto Avance de Pago
       v_QryTxt          STRING, 
       v_n_referencia    LIKE sfr_marca_historica.n_referencia,
       v_estado_marca    SMALLINT, --LIKE sfr_marca_historica.estado_marca,
       v_marca_causa     LIKE sfr_marca_historica.marca_causa,
       r_bandera_marca_quebranto INTEGER,
       v_id_derechohabiente LIKE sfr_marca_historica.id_derechohabiente       
       
    LET p_marca_entra = 601
    LET p_fecha_caus = NULL 

    -- Consulta datos para enviar a función de desmarca
    LET v_QryTxt = "\n SELECT id_derechohabiente, n_referencia,",
                   "\n        estado_marca, marca_causa",
                   "\n   FROM sfr_marca_historica",
                   "\n  WHERE 1=1"
   IF length(p_fecha) > 0 THEN
      LET v_QryTxt = v_QryTxt || "\n AND f_inicio = '",p_fecha,"'"
   END IF 
   IF length(p_folio) > 0 THEN 
      LET v_QryTxt = v_QryTxt || "\n AND folio = ",p_folio,""
   END IF                                       
   LET v_QryTxt = v_QryTxt || "\n GROUP BY 1,2,3,4" 
                       
   DISPLAY v_QryTxt
   PREPARE prp_ejecuta_reverso FROM v_QryTxt
   DECLARE cur_ejecuta_reverso CURSOR FOR prp_ejecuta_reverso

      FOREACH cur_ejecuta_reverso INTO v_id_derechohabiente,
                                       v_n_referencia,
                                       v_estado_marca,
                                       v_marca_causa
             --Se ejecuta función de marca
             #Instruccion para ejecutar la funcion de reversa marca
             LET v_fn_marca_cuenta =
                "EXECUTE PROCEDURE safre_viv:sp_reversa_marca(?,?,?,?)"
             PREPARE exe_fn_marca_cuenta FROM v_fn_marca_cuenta
             EXECUTE exe_fn_marca_cuenta
               USING v_id_derechohabiente,p_marca_entra,v_n_referencia,
                     p_folio 

             DISPLAY "Derechohabiente ",v_id_derechohabiente
             DISPLAY "Marca Entra     ",p_marca_entra
             DISPLAY "Referencia      ",v_n_referencia
             DISPLAY "Folio           ",p_folio
          END FOREACH

END FUNCTION