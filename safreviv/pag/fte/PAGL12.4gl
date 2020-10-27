--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/04/2012
--===============================================================

#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGL12                                                                 #
#Objetivo     => Programa lanzador de las rutinas de carga, integración y               #
#                preliquidacion de recibos de pagos                                     #
#Fecha inicio => Enero 10, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod
GLOBALS
DEFINE g_pid                   LIKE bat_ctr_proceso.pid    #ID del proceso
       --g_proceso_cod           LIKE cat_proceso.proceso_cod, #Codigo del proceso
       --g_opera_cod_carga       LIKE cat_operacion.opera_cod, -- codigo de operacion
       --g_opera_cod_integracion LIKE cat_operacion.opera_cod -- codigo de operacion
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

   CALL STARTLOG(p_usuario_cod CLIPPED||".PAGL12.log")

   CALL fn_carga_archivo_pag(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)
END MAIN

{ ==========================================================================
Clave:  fn_carga_archivo_pag
Nombre: fn_carga_archivo_pag
Fecha creacion: 10 de Enero de 2012
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
Esta función carga un archivo del proceso de "Recaudación SAR 92"
 Parametros de Entrada:
  -  
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_carga_archivo_pag(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, #Clave de usuario
       p_tpo_ejecucion   INTEGER,   # 1 - En linea, 2 - Batch
       p_cad_ventana     STRING,    #Cadena de la ventana
       lsi_tpoProceso    SMALLINT,
       lsi_tpoOperacion  SMALLINT,
       --ls_qryIdProceso   STRING, 
       v_comando         STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_nom_archivo     STRING,
       v_cadena_pid      VARCHAR(5),
       v_cadena_proc     VARCHAR(5),
       --v_cadena_opera    VARCHAR(5),
       v_num_folio       LIKE glo_folio.folio,
       r_bnd_carga       BOOLEAN,
       r_resultado_opera SMALLINT,
       v_ruta_vacia      LIKE seg_modulo.ruta_bin
       
   #Inicializacion de variables
   INITIALIZE lsi_tpoProceso, lsi_tpoOperacion TO NULL

   #Se dan las variables de proceso y operacion
   --LET g_proceso_cod           = 1402 # Recaudación SAR 92
   --LET g_opera_cod_carga       = 1 # Recaudación SAR 92
   --LET g_opera_cod_integracion = 2 # CARGA ARCHIVO REGISTRO PAGOS
   
   OPEN WINDOW w_sar92 WITH FORM "PAGL121"
      #Se asigna el titulo de la ventana
      IF ( p_cad_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_cad_ventana)
      END IF

      #Se verifica si ya se termino el proceso anterior, si no,
      #No se permite capturar nuevos saldos
      CALL fn_valida_operacion(0,g_proceso_cod_pag_registro_pagos_sar92,g_opera_cod_pag_carga) RETURNING r_resultado_opera
      
      IF(r_resultado_opera = 0)THEN
         
         LET v_num_folio = 0
         
         
         # nombre de archivo, no es necesario, ya que en la carga se determina el archivo
         LET v_nom_archivo = "ARCHIVO.SAR92"

         #Se crean las cadenas para el nombre del archivo log
         LET v_cadena_pid   = g_pid USING "&&&&&"
         DISPLAY "@@@................ G_PID: ", g_pid
         LET v_cadena_proc  = g_proceso_cod_pag_registro_pagos_sar92 USING "&&&&&"
         --LET v_cadena_opera = g_opera_cod_integracion USING "&&&&&" 

         # Recuepra las rutas 
         CALL fn_rutas('pag') RETURNING v_ruta_ejecutable,v_ruta_vacia
         CALL fn_rutas('bat') RETURNING v_ruta_vacia,v_ruta_listados

         #Se invoca el store procedure que almacena la información en las
         #Tablas históricas
         LET v_comando = "NA"
         --LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/PAGP12.42r ",
         --                                p_usuario_cod," ",g_pid," ",g_proceso_cod," ", 
         --                                g_opera_cod_integracion," ",v_num_folio," ",
         --                                v_nom_archivo,
         --                " 1>", v_ruta_listados CLIPPED ,
         --                "/nohup:",v_cadena_pid,":", v_cadena_proc,":",
         --                          v_cadena_opera ,
         --                " 2>&1 &"
                         
          # Se invoca la función que lee el archivo y 
         # guarda la información en las tablas temporales
         # El primer parámetro pertenece al identificador del proceso
         # El segundo parámetro pertenece a la tabla "cat_proceso"
         #   1  Liquidacion  de pagos
         #   2  Recaudacion SAR 92
         #   3  Recaudación solo INFONAVIT
         #   18 Aclaraciones sin cambio 
         # El tercer parámetro pertenece a la tabla "cat_operaciones",
         # y depende del valor recibido en el primer parámetro.
         # El cuarto parámtero indica si el proceso debe ejecutarse 
         #   1 Proceso en línea
         #   2 Batch
         # El quinto parámetro pertenece al nombre del programa que realiza la operacion
         # El sexto parámmetro es el comando para invocar la integracion
         # El séptimo parámetro es el usuario logeado
         
         CALL fn_carga_archivo(g_pid, g_proceso_cod_pag_registro_pagos_sar92, g_opera_cod_pag_carga, p_tpo_ejecucion,
                               "PAGL12",v_comando, p_usuario_cod, TRUE) RETURNING r_bnd_carga
         # Si se realizó la carga se continua con el proceso
         IF NOT(r_bnd_carga)THEN
            CALL fn_mensaje("SAR 92","Se ha cancelado la carga de información","bn_about")
         END IF
      ELSE
         # Muestra el mensaje de cual es la causa de que no se puede iniciar con la operacion
         CALL fn_muestra_inc_operacion(r_resultado_opera)
         MENU
           COMMAND "Salir"
              EXIT MENU
         END MENU
      END IF
      
   CLOSE WINDOW w_sar92
END FUNCTION

{ ==========================================================================
Clave:  fn_llena_combo_proceso
Nombre: fn_llena_combo_proceso
Fecha creacion: 10 de Enero de 2012
Autor: David Miguel Garibay Rivera
Narrativa del proceso que realiza:
Esta función carga los datos del combo que muestra los procesos existentes
para el "Recaudación SAR 92"

Parametros de Entrada:
-

Parámetros de salida;
-

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_llena_combo_proceso(cb)
DEFINE 
   cb             ui.ComboBox, 
   v_contador     INTEGER,
   v_proceso_cod  LIKE cat_proceso.proceso_cod,  #Número de proceso
   v_proceso_desc LIKE cat_proceso.proceso_desc, #Descripcion del proceso
   v_qry_proceso  STRING #Esta variable permite extraer la información 
                         #de la tabla cat_proceso

   #Limpiar el combo
   CALL cb.clear()
   LET v_contador = 0

   #Verifica la existencia de procesos para el "Recaudación SAR 92"
   SELECT COUNT(*)
     INTO v_contador
     FROM cat_proceso
    WHERE modulo_cod = "pag"

   #Si existen procesos para el "Recaudación SAR 92"
   IF (v_contador > 0) THEN
      LET v_qry_proceso = "SELECT proceso_cod, proceso_desc\n",
                          "  FROM cat_proceso\n",
                          " WHERE modulo_cod = 'pag'"

      PREPARE prp_procesos FROM v_qry_proceso
      DECLARE cur_procesos CURSOR FOR prp_procesos 
      FOREACH cur_procesos INTO v_proceso_cod,v_proceso_desc 
         CALL cb.addItem(v_proceso_cod,v_proceso_desc)
      END FOREACH
   END IF

END FUNCTION

{ ==========================================================================
Clave:  fn_llena_combo_operacion
Nombre: fn_llena_combo_operacion
Fecha creacion: 10 de Enero de 2011
Autor: David Miguel Garibay Rivera
Narrativa del proceso que realiza:
Esta función carga los datos del combo que muestra las operaciones existentes
para el "Recaudación SAR 92"

Parametros de Entrada:
-

Parámetros de salida;
-

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_llena_combo_operacion(p_proceso_cod)
DEFINE 
   p_proceso_cod   SMALLINT,
   cb              ui.ComboBox,
   v_contador      INTEGER,
   v_opera_cod     LIKE cat_operacion.opera_cod,    #Número de proceso
   v_opera_desc    LIKE cat_operacion.opera_desc,#Descripcion del proceso
   v_qry_proceso   STRING      #Esta variable permite extraer la información 
                               #de la tabla cat_proceso

   #Se indica el nombre del objeto al cual hace refeencia
   #el combo en la forma(.per)
   LET cb = ui.ComboBox.forName("formonly.cmb_operacion")
   #Limpiar el combo
   CALL cb.clear()
   LET v_contador = 0
   
   #Verifica la existencia de procesos para el "Registro de pagos"
   SELECT COUNT(*)
     INTO v_contador
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod

   #Si existen procesos para el "Registro de pagos"
   IF (v_contador > 0) THEN
      #Recupera todas las operaciones que corresponden al proceso
      LET v_qry_proceso = "\n SELECT opera_cod, opera_desc",
                          "\n   FROM cat_operacion",
                          "\n  WHERE proceso_cod = ?"

      PREPARE prp_operaciones FROM v_qry_proceso
      DECLARE cur_operaciones CURSOR FOR prp_operaciones 
      FOREACH cur_operaciones USING p_proceso_cod
                              INTO v_opera_cod,v_opera_desc
         #Agrega los registros recuperados al combo de la operación
         CALL cb.addItem(v_opera_cod,v_opera_desc)
      END FOREACH
   END IF
END FUNCTION



