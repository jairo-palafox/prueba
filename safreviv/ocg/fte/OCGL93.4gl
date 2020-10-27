--===============================================================
-- Versi�n: 1.0.0
-- Fecha �ltima modificaci�n:
--===============================================================

############################################################################
#Modulo             => OCG                                                 #
#Programa           => OCGL93                                              #
#Objetivo           => Programa que permite la carga de informaci�n  del   #
#                      archivo de migraci�n de datos de acreditados de ADS #
#                      al Sistema de Administraci�n de Cr�ditos 43 bis     #
#                      Saci SAFRE                                          #
#Autor              => Mauro Mu�iz Caballero EFP                           #
#Fecha inicio       => 19 de julio de 2016                                 #
############################################################################

DATABASE safre_viv

#Objetivo:
#  Carga de archivo de migraci�n de "acreditados" de ADS a Saci SAFRE SAC43Bis
#  Se valida la operaci�n y se invoca la funcion general que carga el archivo a la tabla temporal

MAIN

   DEFINE p_v_usuario               LIKE seg_usuario.usuario-- usuario firmado al sistema
   DEFINE p_b_tipo_carga            SMALLINT -- tipo de carga (1 - modo en linea y 2 - modo batch)
   DEFINE p_v_nom_prog              VARCHAR(30) -- nombre del programa
   DEFINE v_d_pid                   DECIMAL(9,0) -- identificador del proceso
   DEFINE v_i_proceso_cod           LIKE cat_proceso.proceso_cod -- proceso que llama las funciones
   DEFINE v_i_opera_cod             LIKE cat_operacion.opera_cod -- operaci�n que llama la funcion
   DEFINE v_v_nom_archivo           LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo en proceso
   DEFINE v_d_folio                 LIKE bat_ctr_proceso.folio -- folio del proceso
   DEFINE v_c_ruta_list_bat         LIKE seg_modulo.ruta_listados -- ruta listados de bat
   DEFINE v_c_programa_cod          LIKE bat_ctr_operacion.programa_cod -- nombre del programa
   DEFINE v_b_continua              SMALLINT -- booleana que indica si se debe continuar con el proceso o no
   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE r_c_ruta_bin              LIKE seg_modulo.ruta_bin -- ruta bin del m�dulo
   DEFINE r_c_ruta_listados         LIKE seg_modulo.ruta_listados -- ruta listados del m�dulo
   DEFINE v_r_operacion             SMALLINT
   DEFINE v_r_estado                SMALLINT
   DEFINE v_r_f_proceso             DATE
   DEFINE v_r_id_ocg_ctr_arch       DECIMAL(9,0) -- identificador de la tabla de control

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".OCGL93.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se obtiene la ruta bin y de listados del m�dulo
   CALL fn_rutas("ocg") RETURNING r_c_ruta_bin, r_c_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   -- se inicializan variables del m�dulo
   LET v_d_pid          = 0
   LET v_i_proceso_cod  = 3992
   LET v_i_opera_cod    = 1 -- valida archivo migraci�n detalles
   LET v_d_folio        = 0
   LET p_b_tipo_carga   = 2
   LET v_v_nom_archivo  = NULL
   LET v_c_programa_cod = "OCGL93"
   LET v_r_operacion    = 92
   LET v_r_estado       = 10
   LET v_r_f_proceso    = today

   DISPLAY "p_v_usuario: ",p_v_usuario
   DISPLAY "p_b_tipo_carga: ",p_b_tipo_carga
   DISPLAY "p_v_nom_prog: ",p_v_nom_prog

   -- se invoca la funcion que valida la operaci�n
   CALL fn_valida_operacion(v_d_pid,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida
DISPLAY "Valida: ", r_b_valida
   -- si la operacion es valida se ejecuta
   IF r_b_valida = 0 THEN      
      -- se desconoce el nombre del archivo por lo tanto se envia No Aplica
      LET v_v_nom_archivo = "N/A"

      -- se desconoce el nombre del archivo por lo tanto se envia No Aplica
      LET v_v_nom_archivo = "N/A"

      -- se crea el comando que ejecuta el modulo que reliza la insercion del registro de control
      LET v_s_comando = " fglrun ",r_c_ruta_bin CLIPPED,"/OCGE93 ",
                                   p_v_usuario     , " ",
                                   v_d_pid         , " ",
                                   v_i_proceso_cod , " ",
                                   v_i_opera_cod   , " ",
                                   v_d_folio       , " ",
                                   v_v_nom_archivo

DISPLAY v_d_pid, " ", v_i_proceso_cod, " ", v_i_opera_cod, " ", p_b_tipo_carga, " ", v_c_programa_cod, " ", v_s_comando, " ", p_v_usuario, " ", TRUE
      -- Se invoca la funcion que valida y carga la informacion en la tabla de paso
      CALL fn_carga_archivo(v_d_pid, v_i_proceso_cod, v_i_opera_cod, p_b_tipo_carga, v_c_programa_cod, v_s_comando, p_v_usuario, TRUE) RETURNING v_b_continua
   ELSE 
      -- en caso de error se muestra un mensaje a usuario
      CALL fn_muestra_inc_operacion(r_b_valida)
   END IF

END MAIN