--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 16/10/2013
--==============================================================================

################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DAE                                                      #
#Programa          => DAEL12                                                   #
#Objetivo          => Programa para capturar el monto de validación para       #
#                     la integración                                           #
#Fecha inicio      => 16/10/2013                                               #
################################################################################

DATABASE safre_viv

GLOBALS
DEFINE arr_hist_monto_valida DYNAMIC ARRAY OF RECORD
          v_hist_monto_valida DECIMAL(16,6),
          v_hist_fecha_ini    DATE, --TIME YEAR TO SECOND,
          v_hist_fecha_fin    DATE, --TIME YEAR TO SECOND,
          v_hist_usuario      CHAR(20)
END RECORD 
END GLOBALS 

MAIN

DEFINE v_folio_dictamen DECIMAL(10,0),
       v_monto_valida   DECIMAL(16,6),
       v_fecha          DATE,
       v_fecha_captura  DATETIME YEAR TO SECOND,
       p_usuario        CHAR(20),   
       p_tipo_ejecucion SMALLINT, 
       p_titulo         STRING,
       bnd_confirma     SMALLINT,
       --Campos Históricos
       v_hist_monto_valida DECIMAL(16,6), 
       v_hist_fecha_ini DATETIME YEAR TO SECOND,
       v_hist_fecha_fin DATETIME YEAR TO SECOND

   LET p_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   LET v_folio_dictamen = NULL 

   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF

   CLOSE WINDOW SCREEN 
   OPEN WINDOW vtn_captura WITH FORM "DAEL120.4fd"

   DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT v_monto_valida
      FROM ed_monto_pesos

      BEFORE INPUT 
         LET v_fecha = TODAY
         LET v_fecha_captura = CURRENT 
         DISPLAY v_fecha TO ed_fecha_captura 
         CALL fn_consulta_historico_montos()
      END INPUT 

      DISPLAY ARRAY arr_hist_monto_valida TO scr_historico_montos.*
      END DISPLAY

      
         ON ACTION insertar
            IF v_monto_valida > 0 OR v_monto_valida IS NULL THEN
               CALL fn_ventana_confirma("Atención","¿El monto capturado es correcto?","question")
               RETURNING bnd_confirma

               IF bnd_confirma = 1 THEN 
                  SELECT monto_valida,
                         fecha_captura
                  INTO   v_hist_monto_valida, 
                         v_hist_fecha_ini
                  FROM   dae_captura_monto;

                  LET v_hist_fecha_fin = CURRENT

                  INSERT INTO dae_hist_monto_valida
                  VALUES (v_hist_monto_valida,
                          v_hist_fecha_ini,
                          v_hist_fecha_fin,
                          p_usuario,
                          v_folio_dictamen);
                         
                  DELETE FROM dae_captura_monto WHERE 1=1;
                  
                  INSERT INTO dae_captura_monto
                       VALUES (v_monto_valida, 
                               v_fecha_captura,            
                               p_usuario);

                  CALL fn_mensaje("Atención", "Se ha actualizado el monto de validación", "info")
                  EXIT DIALOG
               ELSE
                  CLEAR FORM  
                  LET v_monto_valida = NULL 
                  NEXT FIELD ed_monto_pesos
               END IF
            ELSE   
               CALL fn_mensaje("Atención", "El monto de validación debe ser diferente de cero", "info")
               EXIT DIALOG
            END IF

         ON ACTION cancelar
            EXIT DIALOG
   END DIALOG

   CLOSE WINDOW vtn_captura  
END MAIN 

#OBJETIVO: Consultar el registro histórico de los montos de validación
FUNCTION fn_consulta_historico_montos()
DEFINE v_QryTxt        STRING
DEFINE v_hist          INTEGER,
       v_monto_pesos   DECIMAL (16,6),
       v_fecha_captura DATE
 
   LET v_QryTxt = " \n SELECT * ",
                  " \n FROM   dae_hist_monto_valida ",
                  " \n ORDER BY 2,3"

   PREPARE prp_historicos FROM v_QryTxt
   DECLARE cur_historicos CURSOR FOR prp_historicos

   LET v_hist = 1
   
   FOREACH cur_historicos INTO arr_hist_monto_valida[v_hist].*
      LET v_hist = v_hist + 1
   END FOREACH
   
   SELECT monto_valida,
          fecha_captura
   INTO   v_monto_pesos,
          v_fecha_captura
   FROM   dae_captura_monto;

   DISPLAY v_monto_pesos,
           v_fecha_captura 
   TO ed_monto_vigente,
      ed_fecha_vigente

   
   CALL arr_hist_monto_valida.deleteElement(v_hist)
END FUNCTION