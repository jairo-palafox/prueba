--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 01/05/2012
--==============================================================================

################################################################################
#Modulo       => SEP                                                           #
#Programa     => SEPL40                                                        #
#Objetivo     => Programa lanzador de las rutinas de carga para la             #
#                operación 29 de separación de cuentas                         #
#Fecha inicio => Mayo 1, 2012                                                  #
################################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid             LIKE bat_ctr_proceso.pid,     # ID del proceso
       g_proceso_cod     LIKE cat_proceso.proceso_cod, # Codigo del proceso
       g_opera_cod_carga LIKE cat_operacion.opera_cod, # Código de operación
       v_ruta_lst        LIKE seg_modulo.ruta_listados,
       v_ventana         ui.Window,
       p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   INTEGER,
       p_cad_ventana     STRING
END GLOBALS

MAIN
   {
    Se recuperan los parametros recibidos
    Clave de usuario
    Tipo de ejecucion (en línea o batch)
    Cadena que identifica al programa (lo que aparecería como título de la ventana)
   }
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   SELECT ruta_listados
     INTO v_ruta_lst
     FROM seg_modulo
    WHERE modulo_cod = "sep"
   
   CALL STARTLOG(v_ruta_lst CLIPPED||"/"||p_usuario_cod CLIPPED||".SEPL05.log")
   CALL fn_carga_archivo_sepOp29()
END MAIN

{ ==========================================================================
Clave:  fn_carga_archivo_sepOp29
Nombre: fn_carga_archivo_sepOp29
Fecha creacion: 01 de Mayo de 2012
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Esta función carga un archivo de la operacion 29 de separación de cuentas
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_carga_archivo_sepOp29()
DEFINE v_comando         STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_bnd_carga       BOOLEAN,
       r_resultado_opera SMALLINT,
       v_tpo_ejecucion   INTEGER
   
   #Se dan las variables de proceso y operacion
   LET g_proceso_cod     = 2203 # Op 29 Separación de cuentas
   LET g_opera_cod_carga = 1 # Validacion Op 29 Separación de cuentas
   LET v_tpo_ejecucion   = 2

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'sep'

   
   OPEN WINDOW vtna_valicacion WITH FORM v_ruta_ejecutable CLIPPED||"/SEPL401"
      #Se asigna el titulo de la ventana
      IF ( p_cad_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_cad_ventana)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_cad_ventana)
      END IF

      #Se verifica si se puede iniciar la operacion      
      CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod_carga) RETURNING r_resultado_opera
      
      IF(r_resultado_opera = 0)THEN
         LET g_pid = 0
         LET v_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/SEPX40.42r ",
                                                              p_usuario_cod," ",
                                                              g_pid," ",
                                                              g_proceso_cod," ",
                                                              g_opera_cod_carga," ",
                                                              0," ", # folio
                                                              "NA" # archivo
         CALL fn_carga_archivo(g_pid, g_proceso_cod, g_opera_cod_carga, v_tpo_ejecucion,
                               "SEPL40",v_comando, p_usuario_cod, TRUE) RETURNING r_bnd_carga
         # Si se realizó la carga se continua con el proceso
         {IF NOT(r_bnd_carga)THEN
            CALL fn_mensaje("OP 29","Se ha cancelado la carga de información","bn_about")
         END IF}
      ELSE
         # Muestra el mensaje de cual es la causa de que no se puede iniciar con la operacion
         CALL fn_muestra_inc_operacion(r_resultado_opera)
         MENU
           COMMAND "Salir"
              EXIT MENU
         END MENU
      END IF
   CLOSE WINDOW vtna_valicacion
END FUNCTION
