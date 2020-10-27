--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 22/05/2012
--===============================================================

#########################################################################################
#Modulo       => UNI                                                                    #
#Programa     => UNIL07                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la preliquidacion #
#                para la unificacion de cuentas solo INFONAVIT                          #
#Fecha inicio => 22/05/2012                                                             #
#########################################################################################
GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo   RECORD
       ruta_exp         CHAR(40),
       ruta_rescate     CHAR(40),
       ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD

END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo       STRING -- titulo de la ventana
       ,v_folio          LIKE deo_preliquida.folio_liquida
       ,v_s_cadena       STRING -- cadena de texto
       ,v_cbx_folios     ui.ComboBox -- combo de afores
       ,v_i_conArch      INTEGER
DEFINE v_r_glo_ctr_archivo RECORD
       folio           CHAR(10)
       ,nombre_archivo LIKE glo_ctr_archivo.nombre_archivo
END RECORD,
        v_sql_txt       STRING

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
   LET g_proceso_cod = g_proceso_cod_uni_INFONAVIT -- Unificacion de cuentas solo INFONAVIT
   LET g_opera_cod   = 3 -- preliquidacion

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'uni'

    SELECT b.ruta_listados
      INTO seg_modulo_bat.ruta_listados
      FROM seg_modulo b
     WHERE b.modulo_cod = 'bat'
  
   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_preliquida WITH FORM "UNIL70"

   -- se le asigna el apuntado der combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
  
   -- se inicia el combobox en blanco
   CALL v_cbx_folios.clear()
   
   CALL v_cbx_folios.addItem(-1," ")
   
   -- se captura el folio
   INPUT
    v_folio
   WITHOUT DEFAULTS
   FROM
    cmb_folio
   ATTRIBUTES (UNBUFFERED)

      BEFORE INPUT
         -- se asignan los valores por omision
         LET v_folio                  = -1       
         
         LET v_sql_txt= "\n SELECT g.folio, a.nombre_archivo",         
                        "\n   FROM glo_ctr_archivo a, glo_folio g",    
                        "\n  WHERE a.proceso_cod = ",g_proceso_cod,  
                        "\n    AND a.proceso_cod = g.proceso_cod",  
                        "\n    AND a.folio = g.folio",             
                        "\n    AND g.status = 0 "-- solo integrados
                 
         -- se llena el arreglo de folios                                 
         DECLARE cur_folio CURSOR FROM  v_sql_txt
           
 
         LET v_i_conArch = 0
         FOREACH cur_folio INTO v_r_glo_ctr_archivo.*
            LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " -", 
                v_r_glo_ctr_archivo.nombre_archivo 
                CALL v_cbx_folios.addItem(
                 v_r_glo_ctr_archivo.folio, v_s_cadena)
            -- Contador de archivos eoncontrados
            LET v_i_conArch = v_i_conArch + 1
         END FOREACH
         IF(v_i_conArch<1)THEN
            CALL fn_mensaje("Atención",
                 "No existen archivos recientemente integrados","info")
            EXIT INPUT
         END IF
         
      ON ACTION ACCEPT
         IF ( v_folio IS NULL OR v_folio = -1 ) THEN
            CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
            CONTINUE INPUT
         END IF
          
         -- se invoca la ejecucion del stored procedure
         CALL fn_uni_ejecuta_preliquidacion_INFONAVIT(v_folio, p_usuario_cod)
         EXIT INPUT
        
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_folio_preliquida
END MAIN

{
======================================================================
Clave: 
Nombre: fn_uni_ejecuta_preliquidacion_INFONAVIT
Fecha creacion: 22/05/2012
Narrativa del proceso que realiza:
Ejecuta la preliquidacion de unificacion de cuentas solo INFONAVIT
======================================================================
}
FUNCTION fn_uni_ejecuta_preliquidacion_INFONAVIT(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       r_bnd_fin_oper    SMALLINT,
       v_mensaje         STRING,
       r_bnd_valida_operacion SMALLINT,
       r_bnd_opera_ini   SMALLINT

   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "NA"
   -- se verifica si se puede continuar con la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING r_bnd_valida_operacion 
   
      IF (r_bnd_valida_operacion = 0) THEN
         CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"UNIL07","",p_usuario_cod)
         RETURNING r_bnd_opera_ini

         IF (r_bnd_opera_ini = 0) THEN 
            LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/UNIP04 ",
                                p_usuario_cod, " ",
                                g_pid  , " " ,
                                g_proceso_cod , " " ,
                                g_opera_cod ," ",
                                p_folio ," ",
                                v_nombre_archivo ," ",
                                " 1>",seg_modulo_bat.ruta_listados clipped ,
                                "/nohup:",g_pid        USING "&&&&&",":",
                                g_proceso_cod USING "&&&&&",":",
                                g_opera_cod   USING "&&&&&" ,
                                " 2>&1 &"

            DISPLAY "COMANDO :",v_s_comando
            RUN v_s_comando
            CALL fn_mensaje("Atención","Se ha enviado la preliquidación.\n"||
                            "Puede revisar el avance del proceso en el monitor de "||
                             "ejecución de procesos","information")
         ELSE
            CALL fn_recupera_inconsis_opera(r_bnd_opera_ini) RETURNING v_mensaje
            CALL fn_mensaje("Atención", v_mensaje, "stop")
         END IF
      ELSE
         CALL fn_recupera_inconsis_opera(r_bnd_valida_operacion) RETURNING v_mensaje
         CALL fn_mensaje("Atención", v_mensaje, "stop")
      END IF
END FUNCTION