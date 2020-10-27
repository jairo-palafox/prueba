################################################################################
#Proyecto     => SAFRE VIVIENDA                                                #
#Propietario  => E.F.P.                                                        #
#Modulo       => CBD                                                           #
#Programa     => CBDL42                                                        #
#Objetivo     => Lanzado para generar el archivo de fallecidos                 #
# Autor        => Antonio Gómez                                                #
#Fecha inicio => 05/Sep/2017                                                   #
################################################################################

DATABASE safre_viv

GLOBALS
DEFINE p_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS
MAIN
DEFINE p_pid            LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod      LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario        LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
       p_d_inicial      DATE, -- Fecha inicial del periodo para el archivo de salida 
       p_d_final        DATE, -- Fecha final del periodo para el archivo de salida
       p_estado         SMALLINT,
       r_bnd_fin_oper   SMALLINT

   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   LET p_usuario        = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_nombre_archivo = ARG_VAL(5)

   LET g_proceso_cod = 2121 -- Extractor Fallecidos
   LET g_opera_cod   = 1    -- Genera Archivo
       
   -- Llamado a función que genera el archivo de salida
   CALL fn_archivo_salida(p_usuario,p_pid)

   -- se invoca la finalizacion de la operacion
   CALL fn_actualiza_opera_fin(p_pid, g_proceso_cod, g_opera_cod)
        RETURNING r_bnd_fin_oper
         
   IF r_bnd_fin_oper <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      DISPLAY "ERROR en fn_actualiza_opera_fin"
   END IF

END MAIN

#OBJETIVO: Generar el archivo de salida de DAE en base al folio seleccionado
FUNCTION fn_archivo_salida(p_usuario, p_pid)
DEFINE p_usuario             LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       v_v_nom_archivo     CHAR(40), -- nombre del archivo de salida
       v_v_ruta_nomarch    VARCHAR(100), -- ruta y nombre del archivo de salida
       v_c_ruta_env_dpe    LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_solTransf BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_tot_reg                INTEGER, -- Total de registros solo registros <> 01 y 09
       v_s_registro             STRING, -- registro a insertar
       v_d_hoy                  CHAR(8),
       p_pid            LIKE bat_ctr_operacion.pid-- PID del proceso

DEFINE rec_detalles RECORD
          v_nss CHAR(11)
END RECORD 

   -- se crear el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".CBDS16.log")

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO   v_c_ruta_env_dpe
   FROM   seg_modulo
   WHERE  modulo_cod = 'cbd'

   --Generar archivo de salida 
   LET v_d_hoy  = TODAY USING "DDMMYYYY"
   LET v_v_nom_archivo = "/"||"NSS_FALLECIDOS_"||v_d_hoy||".flc"

   LET v_v_ruta_nomarch = v_c_ruta_env_dpe CLIPPED || v_v_nom_archivo

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )

   DECLARE cur_dae_det_solicitud CURSOR FOR SELECT a.nss
                                            FROM   afi_derechohabiente a
                                            INNER JOIN afi_fallecido b
                                            ON     a.id_derechohabiente = b.id_derechohabiente

         
   LET v_tot_reg = 0

   FOREACH cur_dae_det_solicitud INTO rec_detalles.v_nss
      LET v_s_registro = rec_detalles.v_nss
      LET v_tot_reg = v_tot_reg + 1
      CALL v_ch_arch_solTransf.writeline(v_s_registro)
   END FOREACH -- Detalle trabajador

   IF v_tot_reg  < 1 THEN
      DISPLAY "   No existe información para generar el archivo"
      DISPLAY "   Total de Registros  : ", v_tot_reg
   ELSE
      -- Actualiza los registros totales a 8 enviados a procesar

      DISPLAY "\n   ---- EXTRACTOR DE FALLECIDOS ---- \n"
      DISPLAY "   El archivo se creo satisfactoriamente"
      DISPLAY "   Dentro de la siguiente ruta: \n   ", v_v_ruta_nomarch

      DISPLAY "   Total de Registros  : ", v_tot_reg

      --Convierte archivo a DOS
      {LET v_v_nom_archivo = v_busca_archivo||v_reg_dia CLIPPED||".ajdae"
      LET v_convierte_archivo = "unix2dos "||" "||v_c_ruta_env_dpe CLIPPED||" "||v_v_nom_archivo
      RUN v_convierte_archivo 

      DISPLAY "\n Convierte archivo a DOS:", v_convierte_archivo

      -- Genera copia a archivo fijo
      LET v_nombre_destino = "AJAE_1.sdae"
      LET v_copia_archivo = "\n cp "||v_c_ruta_env_dpe CLIPPED||"/"||v_v_nom_archivo CLIPPED|| " " ||v_c_ruta_env_dpe CLIPPED||"/"||v_nombre_destino
      RUN v_copia_archivo 
      
      DISPLAY "\n Copia Archivo para Cartera:", v_copia_archivo, "\n"

      --LET v_ejecuta_sh = "\n sh /opt/Interpel/Scripts/DAE_1.sh"
      --RUN v_ejecuta_sh

      --DISPLAY "\n Se ejecutó la transaferencia a cartera"}
   END IF
   
   CALL v_ch_arch_solTransf.close()

END FUNCTION --fn_archivo_salida
