--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================

--------------------------------------------------------------------------------------------
-- Modulo       => PAG                                                                    --
-- Programa     => PAGL81                                                                 --
-- Objetivo     => Programa lanzador consulta de extractro de pagos para precalificación  --
-- Fecha inicio => 24 Septiembre de 2014                                                  --
-- Autor        => GERARDO ALFONSO VEGA PAREDES                                           --
--------------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS "PAGG01.4gl"

GLOBALS
   DEFINE g_pid         LIKE bat_ctr_proceso.pid,     --  ID del proceso
          g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
          g_opera_cod   LIKE cat_operacion.opera_cod  -- codigo de operacion
          
   DEFINE g_reg_modulo   RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
   END RECORD
   
   DEFINE seg_modulo_bat RECORD
          ruta_listados    CHAR(40)
   END RECORD

END GLOBALS


MAIN

   DEFINE v_fecha_consulta DATE,    -- forma como ejecutara el programa
          p_s_titulo       STRING,  -- titulo de la ventana
          p_tipo_ejecucion SMALLINT,
          v_folio_temp     DECIMAL(9,0),
          v_dato           CHAR(01)

   -- se recuperan los argumentos de la linea de comandos
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se abre la ventana
   OPEN WINDOW w_consulta WITH FORM "PAGC811"

   -- se inicia el folio en null
   LET v_fecha_consulta = NULL

   INPUT v_fecha_consulta WITHOUT DEFAULTS
     FROM fecha_consulta
     ATTRIBUTES (UNBUFFERED)


      ON ACTION accept
         IF ( v_fecha_consulta IS NOT NULL ) THEN

            LET v_sql = "\n SELECT glo.folio         ",
                        "\n FROM   glo_folio glo, bat_ctr_operacion bat ",
                        "\n WHERE  date(bat.fecha_fin) = ","'",v_fecha_consulta,"'",
                        "\n AND    glo.folio  = bat.folio ",
                        "\n AND    glo.status = 2 ",
                        "\n AND    glo.proceso_cod IN (101,102,103,107,1401,1403) ",
                        "\n AND    glo.proceso_cod = bat.proceso_cod ",
                        "\n AND    bat.opera_cod   = 4 "

            PREPARE consulta_existe_fecha FROM v_sql
            DECLARE cur_consulta_fecha CURSOR FOR consulta_existe_fecha

            FOREACH cur_consulta_fecha INTO v_folio_temp
               EXIT FOREACH
            END FOREACH

            LET v_dato = NULL
            SELECT "X" dato
            INTO   v_dato
            FROM   pag_extractor_preca
            WHERE  f_consulta = v_fecha_consulta
            GROUP BY dato

            IF (v_folio_temp IS NULL OR 0) AND v_dato IS NULL THEN
              CALL fn_mensaje("Atención","La fecha capturada no existe","stop")
            ELSE
              CALL ejecuta_consulta(v_fecha_consulta)
            END IF

         ELSE

            CALL fn_mensaje("Atención","Debe capturar la Fecha de Consulta","stop")
            CONTINUE INPUT
         END IF

      ON ACTION cancel
         EXIT INPUT

   END INPUT
   CLOSE WINDOW w_consulta

END MAIN

FUNCTION ejecuta_consulta(v_fecha_consulta)
   
   DEFINE v_fecha_consulta DATE
   
   DEFINE v_comando STRING
   
   LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/PAGC81 ",
                       p_usuario_cod CLIPPED, " ",
                       g_pid  , " " ,
                       g_proceso_cod , " " ,
                       g_opera_cod ," ",
                       p_folio ," '",
                       v_nombre_archivo CLIPPED,"' ",
                       " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                       "/nohup:",g_pid        USING "&&&&&",":",
                       g_proceso_cod USING "&&&&&",":",
                       g_opera_cod   USING "&&&&&" ,
                       " 2>&1 &"

                      DISPLAY v_s_comando
                      
       CALL fn_mensaje("Atención","Se ha enviado el reverso de la Preliquidación.\n"||
            "Puede revisar el avance del proceso en el monitor de ejecución de procesos"
            ,"information")                      

    RUN v_s_comando
 
END FUNCTION

