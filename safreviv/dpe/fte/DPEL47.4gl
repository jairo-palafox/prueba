--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 24/11/2020
--==============================================================================
################################################################################
#Modulo            => DPE                                                      #
#Programa          => DPEL39                                                   #
#Descripcion       => Desmarca masiva de rechazos de NSS                       #
#Autor             => José Trinidad Soto Ortega                                #
#Fecha inicio      => 24 noviembre 2020                                        #
################################################################################
DATABASE safre_viv
GLOBALS
 CONSTANT  g_proceso_cod_dpe_procesar_gen SMALLINT = 1008 -- DPE CARGA DE ARCH DE VALIDACIÓN
 DEFINE g_pid LIKE glo_pid.pid
 DEFINE g_folio LIKE bat_ctr_operacion.folio
END GLOBALS
--GLOBALS "DPEG01.4gl"

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   INTEGER,
       p_cad_ventana     STRING,
       g_pid             LIKE glo_pid.pid,
       v_ventana         ui.Window,
       v_forma           ui.Form,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_resultado_opera SMALLINT
DEFINE f   ui.Form,           # Generico de forma
       w   ui.window          # Generico de ventana
DEFINE ArrMarcados DYNAMIC ARRAY OF RECORD
         seleccion  SMALLINT,
         nss LIKE afi_derechohabiente.nss,
         id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
         marca LIKE sfr_marca_activa.marca,
         descripcion_marca LIKE sfr_marca.descripcion_marca,
         n_referencia LIKE sfr_marca_activa.n_referencia,
         folio LIKE sfr_marca_activa.folio,
         proceso_marca LIKE sfr_marca_activa.proceso_marca,
         proceso_desc LIKE cat_proceso.proceso_desc,
         f_vigencia LIKE sfr_marca_activa.f_vigencia 
       END RECORD

MAIN
  LET p_usuario_cod   = ARG_VAL(1)
  LET p_tpo_ejecucion = ARG_VAL(2)
  LET p_cad_ventana   = ARG_VAL(3)

  # Recupera la ruta ejecutable del módulo
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'dpe'

  CALL fn_inicializa_consultas()
  
  CALL fn_recupera_nss_marcados()

END MAIN

# Descripción: Inicializa consultas sql
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

  LET v_consulta = "select '',",
                   "\n     a.nss,",
                   "\n     m.id_derechohabiente,",
                   "\n     m.marca,",
                   "\n     '',",
                   "\n     m.n_referencia,",
                   "\n     m.folio,",
                   "\n     m.proceso_marca,",
                   "\n     '',",
                   "\n     m.f_vigencia",
                   "\n from sfr_marca_activa m,",
                   "\n      afi_derechohabiente a",
                   "\n where a.id_derechohabiente = m.id_derechohabiente",
                   "\n   and m.proceso_marca in (1001)",
                   "\n    and m.marca = 401",
                   "\n    and a.nss = ?",
                   "\n order by a.id_derechohabiente DESC"

  PREPARE prp_rec_marcados_401 FROM v_consulta
  DISPLAY v_consulta

END FUNCTION

FUNCTION fn_recupera_nss_marcados()
DEFINE r_valida   SMALLINT,
       r_error    BOOLEAN,
       r_confirma BOOLEAN
DEFINE vl_nss LIKE afi_derechohabiente.nss
DEFINE bnd_seleccion SMALLINT
DEFINE v_marca, cont, contador_dpe_trabajador SMALLINT
DEFINE v_bnd_desmarca SMALLINT
DEFINE p_proceso_cod SMALLINT
DEFINE QrySql, v_executa_desmarca STRING       
       
  
   --OPEN WINDOW vtna_desmarca_nss_marcados WITH FORM v_ruta_ejecutable CLIPPED||"/DPEL391"
   OPEN WINDOW vtna_desmarca_nss_marcados WITH FORM "DPEL470"
     #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF (p_cad_ventana IS NOT NULL) THEN
        CALL ui.Interface.setText(p_cad_ventana)         
        CALL v_ventana.setText(p_cad_ventana)
      END IF
      
      DIALOG ATTRIBUTES(UNBUFFERED, FIELD ORDER FORM)
        INPUT vl_nss FROM nss
        BEFORE INPUT
        	--CALL f.setElementhidden("desmarcar",TRUE)
        	
          ON ACTION aceptar
             IF vl_nss IS NULL OR LENGTH(vl_nss CLIPPED) = 0 THEN
               CALL fn_mensaje(p_cad_ventana,"El NSS no puede estar vacío","about")
               CONTINUE DIALOG
             ELSE
               CALL fn_llenaArregloNSSMarcados(vl_nss) RETURNING ArrMarcados
               IF ArrMarcados.getLength() = 0 THEN
                 CALL fn_mensaje(p_cad_ventana,"No existen concidencias con los criterios dados.","about")
               ELSE
                 CALL f.setElementhidden("gdetalle",FALSE)	  
               END IF
             END IF
          
             
        END INPUT
        INPUT ARRAY ArrMarcados FROM sr_marcados.* ATTRIBUTES(WITHOUT DEFAULTS,
      	                                                    INSERT ROW = FALSE,
      	                                                    DELETE ROW = FALSE,
      	                                                    APPEND ROW = FALSE)
        END INPUT
        
      BEFORE DIALOG
      	LET f = DIALOG.getForm()
      	
      	ON ACTION desmarcar
            LET bnd_seleccion = 0
        	  FOR cont = 1 TO ArrMarcados.getLength()
        	    IF ArrMarcados[cont].seleccion = 1 THEN
        	      LET bnd_seleccion = 1
        	    END IF	
        	  END FOR
        	  IF NOT bnd_seleccion THEN
      	      CALL fn_mensaje(p_cad_ventana,"Debe seleccionar al menos un registro.","exclamation")
      	  	  CONTINUE DIALOG
      	    ELSE
      	      FOR cont = 1 TO ArrMarcados.getLength()
        	      LET v_marca = 401
        	      LET v_bnd_desmarca = 0
        	      LET p_proceso_cod = 1008
        	      --LET p_usuario_cod = 'IECAAJ03'
        	      IF ArrMarcados[cont].seleccion = 1 THEN
        	        TRY
        	      	  LET v_executa_desmarca = "EXECUTE FUNCTION fn_desmarca_cuenta(",
        	      	                                                                  ArrMarcados[cont].id_derechohabiente,",",
        	      	                                                                  v_marca,",",
        	      	                                                                  ArrMarcados[cont].n_referencia,",0,'','",
        	      	                                                                  p_usuario_cod CLIPPED,"',",
        	      	                                                                  p_proceso_cod,")"
        	      	    
                    PREPARE prp_executa_desmarca FROM v_executa_desmarca
        	      	  EXECUTE prp_executa_desmarca INTO v_bnd_desmarca
        	      	CATCH
                    DISPLAY "Error al desmarcar registros: ",v_bnd_desmarca
                    DISPLAY "Codigo de Falló: ", SQLCA.SQLCODE
                    DISPLAY "SQL Mensaje de error: ",SQLERRMESSAGE
                  END TRY                                     
                END IF
              END FOR
            END IF
            IF v_bnd_desmarca = 0 THEN
        	    CALL fn_mensaje(p_cad_ventana,"El proceso finalizo satisfactoriamente.","information")
        	  ELSE
        	  	CALL fn_mensaje(p_cad_ventana,"Ocurrio un error al desmarcar el registro.","information")
        	  END IF
        	  EXIT DIALOG
      	  
        ON ACTION cancelar
          EXIT DIALOG  
      
      END DIALOG
          
   CLOSE WINDOW vtna_desmarca_nss_marcados
       
END FUNCTION

FUNCTION fn_llenaArregloNSSMarcados(p_nss)
DEFINE v_indice SMALLINT
DEFINE p_nss LIKE afi_derechohabiente.nss
DEFINE rMarcados RECORD
         seleccion  SMALLINT,
         nss LIKE afi_derechohabiente.nss,
         id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
         marca LIKE sfr_marca_activa.marca,
         descripcion_marca LIKE sfr_marca.descripcion_marca,
         n_referencia LIKE sfr_marca_activa.n_referencia,
         folio LIKE sfr_marca_activa.folio,
         proceso_marca LIKE sfr_marca_activa.proceso_marca,
         proceso_desc LIKE cat_proceso.proceso_desc,
         f_vigencia LIKE sfr_marca_activa.f_vigencia 
       END RECORD
  
  CALL ArrMarcados.clear()
  LET v_indice = 1
  DECLARE cur_rec_marcados_401 CURSOR FOR prp_rec_marcados_401      
  FOREACH cur_rec_marcados_401 USING p_nss INTO rMarcados.*
  	LET ArrMarcados[v_indice].seleccion = 0
  	LET ArrMarcados[v_indice].nss = rMarcados.nss
  	LET ArrMarcados[v_indice].id_derechohabiente = rMarcados.id_derechohabiente
    LET ArrMarcados[v_indice].marca  = rMarcados.marca
    SELECT descripcion_marca
      INTO rMarcados.descripcion_marca
    FROM sfr_marca
    WHERE marca = rMarcados.marca
    LET ArrMarcados[v_indice].descripcion_marca = rMarcados.marca||'-'||rMarcados.descripcion_marca
    LET ArrMarcados[v_indice].n_referencia = rMarcados.n_referencia
    LET ArrMarcados[v_indice].folio = rMarcados.folio
    LET ArrMarcados[v_indice].proceso_marca = rMarcados.proceso_marca
    SELECT proceso_desc
      INTO rMarcados.proceso_desc
    FROM cat_proceso
    WHERE proceso_cod = rMarcados.proceso_marca
    LET ArrMarcados[v_indice].proceso_desc = rMarcados.proceso_marca||'-'||rMarcados.proceso_desc
    LET ArrMarcados[v_indice].f_vigencia = rMarcados.f_vigencia
  	LET v_indice = v_indice + 1
  END FOREACH
  FREE cur_rec_marcados_401
  
  CALL ArrMarcados.deleteelement(v_indice)
  
  RETURN ArrMarcados

END FUNCTION

{DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   INTEGER,
       p_cad_ventana     STRING,
       v_ventana         ui.Window,
       v_forma           ui.Form,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_resultado_opera SMALLINT
DEFINE ArrMarcados DYNAMIC ARRAY OF RECORD
         seleccion  SMALLINT,
         reg_patronal CHAR(11),
         nss CHAR(11),
         periodo_pago CHAR(6),
         rfc_trabajador CHAR(13),
         curp_trabajador CHAR(18),
         nombre CHAR(50),
         imp_patronal_infonavit_devolver DECIMAL(9,0),
         apli_interes_vivienda DECIMAL(15,0),
         resul_opera CHAR(2),
         diagnostico1 CHAR(3)
       END RECORD

MAIN
  LET p_usuario_cod   = ARG_VAL(1)
  LET p_tpo_ejecucion = ARG_VAL(2)
  LET p_cad_ventana   = ARG_VAL(3)

  # Recupera la ruta ejecutable del módulo
   --TRY
     SELECT ruta_bin
       INTO v_ruta_ejecutable
       FROM seg_modulo
      WHERE modulo_cod = 'dpe'
   --CATCH
   --  DISPLAY "Error al seleccionar la ruta_bin"
   --  DISPLAY "Codigo de Falló: ", SQLCA.SQLCODE
   --  DISPLAY "SQL Mensaje de error: ",SQLERRMESSAGE   
   --END TRY
   
   --TRY
   --  SELECT MAX(pid)
   --    INTO g_pid
   --    FROM bat_ctr_proceso
   --   WHERE proceso_cod = g_proceso_cod_dpe_procesar_gen
   --CATCH
   --  DISPLAY "Error al seleccionar el PID"
   --  DISPLAY "Codigo de Falló: ", SQLCA.SQLCODE
   --  DISPLAY "SQL Mensaje de error: ",SQLERRMESSAGE
   --END TRY
   --
   --TRY
   --  SELECT folio
   --    INTO g_folio
   --    FROM bat_ctr_operacion
   --   WHERE proceso_cod = g_proceso_cod_dpe_procesar_gen
   --     AND estado_cod = 4
   --     AND pid = g_pid
   --CATCH
   --  DISPLAY "Error al seleccionar folio"
   --  DISPLAY "Codigo de Falló: ", SQLCA.SQLCODE
   --  DISPLAY "SQL Mensaje de error: ",SQLERRMESSAGE
   --END TRY     

  CALL fn_inicializa_consultas()
  
  CALL fn_recupera_nss_marcados()

END MAIN

FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

  LET v_consulta = "select '',",
                   "\n     reg_patronal,",
                   "\n     nss,",
                   "\n     periodo_pago,",
                   "\n     rfc_trabajador,",
                   "\n     curp_trabajador,",
                   "\n     nombre,",
                   "\n     imp_patronal_infonavit_devolver / 100,",
                   "\n     apli_interes_vivienda / 1000000,",
                   "\n     resul_opera,",
                   "\n     diagnostico1",
                   "\n from safre_tmp:tmp_det_rch_devolucion_dpe",
                   "\n where resul_opera = 2",
                   "\n   and tpo_registro = 3"

  PREPARE prp_rec_marcados_401 FROM v_consulta
  
END FUNCTION

FUNCTION fn_recupera_nss_marcados()
DEFINE r_valida   SMALLINT,
       r_error    BOOLEAN,
       r_confirma BOOLEAN
DEFINE bnd_seleccion SMALLINT
DEFINE v_id_dpe_referencia DECIMAL(9,0)
DEFINE v_id_derechohabiente DECIMAL(9,0)
DEFINE v_marca, cont, contador_dpe_trabajador SMALLINT
DEFINE v_bnd_desmarca SMALLINT
DEFINE p_proceso_cod SMALLINT
DEFINE QrySql, v_executa_desmarca STRING
       
   --OPEN WINDOW vtna_desmarca_nss_marcados WITH FORM v_ruta_ejecutable CLIPPED||"/DPEL460"
   OPEN WINDOW vtna_desmarca_nss_marcados WITH FORM "DPEL470"
     #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF (p_cad_ventana IS NOT NULL) THEN
        CALL ui.Interface.setText(p_cad_ventana)         
        CALL v_ventana.setText(p_cad_ventana)
      END IF
      
      INPUT ARRAY ArrMarcados FROM sr_marcados.* ATTRIBUTES(WITHOUT DEFAULTS, UNBUFFERED,
      	                                                    ACCEPT = FALSE,
      	                                                    CANCEL = FALSE,
      	                                                    INSERT ROW = FALSE,
      	                                                    DELETE ROW = FALSE,
      	                                                    APPEND ROW = FALSE)
      BEFORE INPUT
        # Se recuperan los registros
        CALL fn_llenaArregloNSSMarcados() RETURNING ArrMarcados
            IF(ArrMarcados.getLength() = 0)THEN
               CALL fn_mensaje(p_cad_ventana,"No hay información para desmarcar","about")
               EXIT INPUT
            END IF
        
        ON ACTION desmarcar
          LET bnd_seleccion = 0
        	FOR cont = 1 TO ArrMarcados.getLength()
        	  IF ArrMarcados[cont].seleccion = 1 THEN
        	    LET bnd_seleccion = 1
        	  END IF	
        	END FOR
        	IF NOT bnd_seleccion THEN
      	    CALL fn_mensaje(p_cad_ventana,"Debe seleccionar al menos un registro.","exclamation")
      	  	CONTINUE INPUT
      	  ELSE
      	    FOR cont = 1 TO ArrMarcados.getLength()
        	    LET v_id_dpe_referencia = 0
        	    LET v_id_derechohabiente = NULL
        	    LET v_marca = 401
        	    LET v_bnd_desmarca = 0
        	    LET p_proceso_cod = g_proceso_cod_dpe_procesar_gen
        	    --LET p_usuario_cod = 'IECAAJ03'
        	    IF ArrMarcados[cont].seleccion = 1 THEN
        	      TRY
        	        LET QrySql = "SELECT count(*)",
                               "\n FROM dpe_sol_trabajador",
                               "\n WHERE reg_patronal_imss = '",ArrMarcados[cont].reg_patronal,"'",
                               "\n   AND nss = '",ArrMarcados[cont].nss,"'",        
                               "\n   AND periodo_pago = '",ArrMarcados[cont].periodo_pago,"'",
                               "\n   AND folio_respuesta IS NULL"
                  
                  PREPARE count_id_referencia FROM QrySql
                  EXECUTE count_id_referencia INTO contador_dpe_trabajador
                  
                  IF contador_dpe_trabajador > 0 THEN
                    TRY
        	            LET QrySql = "SELECT id_dpe_referencia,",
                                   "\n     id_derechohabiente",
                                   "\n FROM dpe_sol_trabajador",
                                   "\n WHERE reg_patronal_imss = '",ArrMarcados[cont].reg_patronal,"'",
                                   "\n   AND nss = '",ArrMarcados[cont].nss,"'",        
                                   "\n   AND periodo_pago = '",ArrMarcados[cont].periodo_pago,"'",
                                   "\n   AND folio_respuesta IS NULL"
                         
                      PREPARE prp_rec_id FROM QrySql
                      EXECUTE prp_rec_id INTO v_id_dpe_referencia, v_id_derechohabiente 
                      --LET v_id_dpe_referencia = 5426
                      --LET v_id_derechohabiente = 10423143   
                    CATCH
                      DISPLAY "Error al seleccionar dpe_sol_trabajador"
                      DISPLAY "Codigo de Falló: ", SQLCA.SQLCODE
                      DISPLAY "SQL Mensaje de error: ",SQLERRMESSAGE 
                    END TRY
                    --
                    IF v_id_dpe_referencia > 0 THEN
                      TRY
                        UPDATE dpe_sol_trabajador
                           SET resul_op = ArrMarcados[cont].resul_opera,
                               diag_procesa = ArrMarcados[cont].diagnostico1,
                               folio_respuesta = g_folio
                         WHERE id_dpe_referencia = v_id_dpe_referencia;
                      CATCH
                        DISPLAY "Error al actualizar dpe_sol_trabajador"
                        DISPLAY "Codigo de Falló: ", SQLCA.SQLCODE
                        DISPLAY "SQL Mensaje de error: ",SQLERRMESSAGE  
                      END TRY
                    END IF
        	    	    TRY
        	    	      LET v_executa_desmarca = "EXECUTE FUNCTION fn_desmarca_cuenta(",
        	    	                                                                     v_id_derechohabiente,",",
        	    	                                                                     v_marca,",",
        	    	                                                                     v_id_dpe_referencia,",0,'','",
        	    	                                                                     p_usuario_cod CLIPPED,"',",
        	    	                                                                     p_proceso_cod,")"
        	    	    
                      PREPARE prp_executa_desmarca FROM v_executa_desmarca
        	    	      EXECUTE prp_executa_desmarca INTO v_bnd_desmarca
        	    	    CATCH
                      DISPLAY "Error al desmarcar registros: ",v_bnd_desmarca
                      DISPLAY "Codigo de Falló: ", SQLCA.SQLCODE
                      DISPLAY "SQL Mensaje de error: ",SQLERRMESSAGE
                    END TRY                                     
        	    	    
        	    	    --TRY
        	    	    --  INSERT INTO dpe_resp_procesar(id_dpe_referencia,
                    --                                folio,
                    --                                reg_patronal_imss,
                    --                                nss,
                    --                                periodo_pago,
                    --                                imp_viv_dev,
                    --                                aivs_viv_dev,
                    --                                resul_op,
                    --                                f_respuesta) VALUES (v_id_dpe_referencia, 
                    --                                                     g_folio,
                    --                                                     v_reg_patronal,
                    --                                                     v_nss,
                    --                                                     v_periodo_pago,
                    --                                                     v_imp_patronal_infonavit_devolver ,
                    --                                                     v_apli_interes_vivienda,
                    --                                                     v_resultado_operacion,
                    --                                                     TODAY)
                    --CATCH
                    --  DISPLAY "Error al insertar registros en dpe_resp_procesar"
                    --  DISPLAY "Codigo de Falló: ", SQLCA.SQLCODE
                    --  DISPLAY "SQL Mensaje de error: ",SQLERRMESSAGE
                    --END TRY                                                    
                  ELSE
                    CALL fn_mensaje(p_cad_ventana,"El derechohabiente no tiene referencia con el patrón.","information")
                    CONTINUE INPUT
                  END IF
                CATCH
                  DISPLAY "Error al seleccionar contador dpe_sol_trabajador"
                  DISPLAY "Codigo de Falló: ", SQLCA.SQLCODE
                  DISPLAY "SQL Mensaje de error: ",SQLERRMESSAGE
                END TRY
              END IF  
        	  END FOR	
        	  --UPDATE STATISTICS FOR TABLE dpe_resp_procesar
        	  CALL fn_mensaje(p_cad_ventana,"El proceso finalizo satisfactoriamente.","information")
        	  EXIT INPUT
      	  END IF
        
        ON ACTION cancelar
          EXIT INPUT
      END INPUT    
   CLOSE WINDOW vtna_desmarca_nss_marcados
       
END FUNCTION

FUNCTION fn_llenaArregloNSSMarcados()
DEFINE v_indice, count_dpe SMALLINT
DEFINE rMarcados RECORD
         seleccion  SMALLINT,
         reg_patronal CHAR(11),
         nss CHAR(11),
         periodo_pago CHAR(6),
         rfc_trabajador CHAR(13),
         curp_trabajador CHAR(18),
         nombre CHAR(50),
         imp_patronal_infonavit_devolver DECIMAL(9,0),
         apli_interes_vivienda DECIMAL(15,0),
         resul_opera CHAR(2),
         diagnostico1 CHAR(3)
       END RECORD
DEFINE QrySql STRING       
  
  CALL ArrMarcados.clear()
  LET v_indice = 1
  DISPLAY "	Inicia llenado de arreglo"
  
  TRY
    DECLARE cur_rec_marcados_401 CURSOR FOR prp_rec_marcados_401      
    FOREACH cur_rec_marcados_401 INTO rMarcados.*
    	LET ArrMarcados[v_indice].seleccion = 0
    	LET ArrMarcados[v_indice].reg_patronal = rMarcados.reg_patronal
    	LET ArrMarcados[v_indice].nss = rMarcados.nss
    	LET ArrMarcados[v_indice].periodo_pago = rMarcados.periodo_pago
      LET ArrMarcados[v_indice].rfc_trabajador  = rMarcados.rfc_trabajador
      LET ArrMarcados[v_indice].curp_trabajador = rMarcados.curp_trabajador
      LET ArrMarcados[v_indice].nombre = rMarcados.nombre
      LET ArrMarcados[v_indice].imp_patronal_infonavit_devolver = rMarcados.imp_patronal_infonavit_devolver
      LET ArrMarcados[v_indice].apli_interes_vivienda = rMarcados.apli_interes_vivienda
      LET ArrMarcados[v_indice].resul_opera = rMarcados.resul_opera
      LET ArrMarcados[v_indice].diagnostico1 = rMarcados.diagnostico1
      
      --TRY
      --  LET QrySql = "SELECT count(*)",
      --               "\n FROM dpe_sol_trabajador",
      --               "\n WHERE reg_patronal_imss = '",rMarcados.reg_patronal,"'",
      --               "\n   AND nss = '",rMarcados.nss,"'",        
      --               "\n   AND periodo_pago = '",rMarcados.periodo_pago,"'",
      --               "\n   AND folio_respuesta IS NULL"
      --     
      --  PREPARE prp_rec FROM QrySql
      --  EXECUTE prp_rec INTO count_dpe
      --  
      --  IF count_dpe > 0 THEN
      --    DISPLAY "Registros: ", v_indice, "VALOR: ", count_dpe
      --  END IF
      --     
      --CATCH
      --  DISPLAY "Error al seleccionar dpe_sol_trabajador"
      --  DISPLAY "Codigo de Falló: ", SQLCA.SQLCODE
      --  DISPLAY "SQL Mensaje de error: ",SQLERRMESSAGE 
      --END TRY
      
    	LET v_indice = v_indice + 1
    END FOREACH
    DISPLAY "Finaliza llenado de arreglo"
    FREE cur_rec_marcados_401
  CATCH
    DISPLAY "Error al recuperar registros"
    DISPLAY "Codigo de Falló: ", SQLCA.SQLCODE
    DISPLAY "SQL Mensaje de error: ",SQLERRMESSAGE
  END TRY
  
  CALL ArrMarcados.deleteelement(v_indice)
  
  RETURN ArrMarcados

END FUNCTION}