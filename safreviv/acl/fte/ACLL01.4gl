--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================
--hola
#########################################################################################
#Modulo       => ACL                                                                    #
#Programa     => ACLL01                                                                 #
#Objetivo     => Programa lanzador de las rutinas de carga                              #
#Fecha inicio => Febrero 07, 2012                                                       #
#Modificacion => se agrega archivo globales de aclaratorio y se sustituyen              #
#                las variables correspondientes; hilda rivas                            #
#########################################################################################
DATABASE safre_viv

GLOBALS "ACLG02.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
DEFINE g_pid                   LIKE bat_ctr_proceso.pid     # ID del proceso
--       g_proceso_cod           LIKE cat_proceso.proceso_cod, # Código del proceso
--       g_opera_cod_carga       LIKE cat_operacion.opera_cod, # Código de operación
--       g_opera_cod_integracion LIKE cat_operacion.opera_cod  # Código de operación
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

   CALL STARTLOG(p_usuario_cod CLIPPED||".ACLL01.log")

   CALL fn_carga_archivo_acl(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)
END MAIN

{===========================================================================
Clave:  fn_carga_archivo_acl
Nombre: fn_carga_archivo_acl
Fecha creacion: 07 de Febrero de 2012
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Esta función carga un archivo del proceso de "Aclaraciones sin cambio"
Parametros de Entrada:
 -  
Parámetros de salida:
 -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_carga_archivo_acl(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)
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
   --LET g_proceso_cod           = 102 # Aclaraciones sin cambio
   --LET g_opera_cod_carga       = 1  # Carga archivos Aclaraciones sin cambio
   --LET g_opera_cod_integracion = 2  # integración

   CALL fn_rutas('acl') RETURNING v_ruta_ejecutable,v_ruta_vacia 
   
   --OPEN WINDOW w_sin_cambio WITH FORM v_ruta_ejecutable CLIPPED||"/ACLL011"
      #Se asigna el titulo de la ventana
      IF ( p_cad_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_cad_ventana)
      END IF

      #Se verifica si ya se termino el proceso anterior, si no,
      #No se permite capturar nuevos saldos
      CALL fn_valida_operacion(0,g_proceso_cod_acl_reg_pag_sin_cambio,g_opera_cod_carga) RETURNING r_resultado_opera
      
      IF(r_resultado_opera = 0)THEN
         #Se invoca la carga de archivo de registro de pagos
         
         LET v_num_folio = 0
                 
         # nombre de archivo, no es necesario, ya que en la carga se determina el archivo
         LET v_nom_archivo = "N/A"

         #Se crean las cadenas para el nombre del archivo log
         LET v_cadena_pid   = g_pid USING "&&&&&"
         LET v_cadena_proc  = g_proceso_cod_acl_reg_pag_sin_cambio USING "&&&&&"
         --LET v_cadena_opera = g_opera_cod_integracion USING "&&&&&" 

         # Recuepra las rutas
         CALL fn_rutas('bat') RETURNING v_ruta_vacia,v_ruta_listados

         #Se invoca el store procedure que almacena la información en las
         #Tablas históricas
         LET v_comando = "N/A"
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
         #   18 Aclaraciones sin cambio 
         # El tercer parámetro pertenece a la tabla "cat_operaciones",
         # y depende del valor recibido en el primer parámetro.
         # El cuarto parámtero indica si el proceso debe ejecutarse 
         #   1 Proceso en línea
         #   2 Batch
         # El quinto parámetro pertenece al nombre del programa que realiza la operacion
         # El sexto parámmetro es el comando para invocar la integracion
         # El séptimo parámetro es el usuario logeado

         
         CALL fn_carga_archivo(g_pid, g_proceso_cod_acl_reg_pag_sin_cambio, g_opera_cod_carga, p_tpo_ejecucion,
                               "ACLL01",v_comando, p_usuario_cod, TRUE) RETURNING r_bnd_carga
         # Si se realizó la carga se continua con el proceso
         IF NOT(r_bnd_carga)THEN
            CALL fn_mensaje("ACLARACION SIN CAMBIO","Se ha cancelado la carga de información","about")
         END IF
      ELSE
         # Muestra el mensaje de cual es la causa de que no se puede iniciar con la operacion
         CALL fn_muestra_inc_operacion(r_resultado_opera)
         MENU
           COMMAND "Salir"
              EXIT MENU
         END MENU
      END IF
   --CLOSE WINDOW w_sin_cambio
END FUNCTION
