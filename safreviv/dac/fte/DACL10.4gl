--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 24/04/2014
--===============================================================

################################################################################
#Modulo       => DAC                                                           #
#Programa     => DACL10                                                        #
#Objetivo     => Programa lanzador para el reverso de la generación del archivo#
#                de Devolución de Amortización MTC.                            #
#Fecha inicio => 24/04/2014                                                    #
################################################################################
--Lanzado: DACR05

DATABASE safre_viv
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
       ,v_folio          LIKE dpe_preliquida.folio_liquida
       ,v_s_cadena       STRING -- cadena de texto
       ,v_cbx_folios     ui.ComboBox -- combo de afores
       ,v_i_conArch      INTEGER
       ,v_r_glo_ctr_archivo      RECORD
          nombre_archivo LIKE glo_ctr_archivo.nombre_archivo
          ,folio         DECIMAL(9,0)
       END RECORD
       ,v_proceso_desc    LIKE cat_proceso.proceso_desc
       ,v_opera_desc      LIKE cat_operacion.opera_desc

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
   LET g_proceso_cod = 2601
   LET g_opera_cod   = 5 -- generacion de archivo

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'dac'

    SELECT b.ruta_listados
      INTO seg_modulo_bat.ruta_listados
      FROM seg_modulo b
     WHERE b.modulo_cod = 'bat'

   CALL fn_recuper_datos_proceso(g_proceso_cod, g_opera_cod)
    RETURNING v_proceso_desc, v_opera_desc
     
   -- se abre la ventana que envia el proceso de preliquidacion
   OPEN WINDOW w_folio_preliquida WITH FORM "DACL100"
   -- Muestra detalle de operación
   DISPLAY BY NAME v_proceso_desc, v_opera_desc

   -- se le asigna el apuntado del combo a la variable
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
         
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
         SELECT DISTINCT a.nombre_archivo, g.folio
           FROM glo_folio g, glo_ctr_archivo a, dac_det_solicitud d
          WHERE g.proceso_cod = g_proceso_cod
            AND g.proceso_cod = a.proceso_cod
            AND g.status = 2
            AND a.folio = d.folio_integracion
            AND g.folio = d.folio_integracion
            AND d.diagnostico = 4
            
         LET v_i_conArch = 0
         FOREACH cur_folios INTO v_r_glo_ctr_archivo.*
            LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " -", 
                v_r_glo_ctr_archivo.nombre_archivo 
            CALL v_cbx_folios.addItem(
                 v_r_glo_ctr_archivo.folio , v_s_cadena)
            -- Contador de archivos eoncontrados
            LET v_i_conArch = v_i_conArch + 1
         END FOREACH
         IF(v_i_conArch<1)THEN
            CALL fn_mensaje("Atención",
                 "No existen archivos recientemente liquidados","info")
            EXIT INPUT
         END IF
         
      ON ACTION ACCEPT
         IF ( v_folio IS NULL OR v_folio = -1) THEN
            CALL fn_mensaje("Atención","Es necesario capturar un folio","stop")
            CONTINUE INPUT
         END IF
      
         -- se invoca la ejecucion del stored procedure
         CALL fn_dpe_reverso_genera_archivo_INFONAVIT(v_folio, p_usuario_cod)
         EXIT INPUT
        
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_folio_preliquida
END MAIN

{
======================================================================
Clave: 
Nombre: fn_dpe_reverso_genera_archivo_INFONAVIT
Fecha creacion: 04/05/2012
Narrativa del proceso que realiza:
Ejecuta la reverso generacion de archivo de una devolucion por pagos 
indebidos o en exceso para un folio dado solo INFONAVIT
======================================================================
}
FUNCTION fn_dpe_reverso_genera_archivo_INFONAVIT(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
       v_si_codigo       SMALLINT

	
   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "ARCHIVOAQUI"

   -- se verifica si se puede continuar con la operacion
   CALL fn_valida_reverso(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_si_codigo
   
   IF ( v_si_codigo = 0 ) THEN
      
      -- se invoca la ejecucion del programa lanzado. los parametros se envian 
      -- en orden segun lo acordado el dia 12/Enero/2012 con equipo EFP
      -- usuario, pid, proceso_cod, opera_cod, folio y archivo
      LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/DACR05 ",
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

                         DISPLAY v_s_comando
                         
       RUN v_s_comando
       CALL fn_mensaje("Atención","Se ha enviado el reverso de la generación de archivo.\n"||
            "Puede revisar el avance del proceso en el monitor de ejecución de procesos"
            ,"information")
   ELSE
      CALL fn_recupera_inconsis_opera(v_si_codigo) RETURNING v_s_comando
      CALL fn_mensaje("Atención", v_s_comando.trim(),"information") 
   END IF
 
END FUNCTION

{
 Nombre  : fn_recuper_datos_proceso
 Fecha   : Febrero 21, 2012
 Descrip : Recupera descripciones de proceso y operación para datos de pantalla.
 Autor   : Felipe Nava.
}
FUNCTION fn_recuper_datos_proceso(p_proceso, p_operacion)
  DEFINE 
   p_proceso         LIKE cat_proceso.proceso_cod
   ,p_operacion      LIKE cat_operacion.opera_cod
   --
   ,v_proceso_desc    LIKE cat_proceso.proceso_desc
   ,v_opera_desc      LIKE cat_operacion.opera_desc
   
   -- <Recupera descripción del proceso> --
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso

    -- <Recupera descripción de operación> --
   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso
      AND opera_cod   = p_operacion
      
 --DISPLAY BY NAME v_proceso_desc, v_opera_desc
 RETURN v_proceso_desc, v_opera_desc
END FUNCTION -- fn_recuper_datos_proceso