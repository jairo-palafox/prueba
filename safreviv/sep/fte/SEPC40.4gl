--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 17/05/2012
--===============================================================
DATABASE safre_viv
DEFINE g_enter char(1)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   INTEGER,
       p_cad_ventana     STRING,
       p_nss_invadido    LIKE sep_det_02_op29.invadido, # NSS recibido por SEPC22(consulta de expedientes)
       p_nss_asociado    LIKE sep_det_03_op29.asociado, # NSS recibido por SEPC22(consulta de expedientes)
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ventana         ui.Window,
       v_nrp_invadidos DYNAMIC ARRAY OF STRING,
       v_nrp_asociados DYNAMIC ARRAY OF STRING,
       v_nrp_detalle_invadido DYNAMIC ARRAY OF RECORD
         v_folio         STRING,--LIKE sep_det_05_op29.folio,
         v_fecha_proceso STRING,--LIKE sep_det_02_op29.f_proceso,
         v_nrp           STRING--LIKE sep_det_05_op29.nrp
       END RECORD,
       v_nrp_detalle_asociado DYNAMIC ARRAY OF RECORD
         v_folio         STRING,
         v_fecha_proceso STRING,--DATE,
         v_nrp           STRING
       END RECORD

MAIN

   # Se recuperan parámetros del lanzador
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)
   LET p_nss_invadido  = ARG_VAL(4)
   LET p_nss_asociado  = ARG_VAL(5)
  
   CALL fn_consulta_nrp(p_nss_invadido,p_nss_asociado)

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC40                                                   #
#Descripcion       => funcion para consultar los nrp asociados                 #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 17 Mayo 2012                                             #
################################################################################
FUNCTION fn_consulta_nrp(p_nss_invadido_aux,p_nss_asociado_aux)
DEFINE r_continuar    BOOLEAN,
       p_nss_invadido_aux STRING, 
       p_nss_asociado_aux STRING,
       v_nss_invadido LIKE sep_det_02_op29.invadido,
       v_nss_asociado LIKE sep_det_03_op29.asociado

   LET r_continuar = TRUE
   
   # Recupera la ruta ejecutable
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "sep"

   OPEN WINDOW vtna_consulta_nrp WITH FORM v_ruta_ejecutable CLIPPED||"/SEPC401"
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)         
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      
      CALL fn_recupera_nrp(p_nss_invadido_aux,p_nss_asociado_aux) 
               RETURNING r_continuar,v_nss_invadido,v_nss_asociado
     
      INITIALIZE p_nss_invadido_aux,p_nss_asociado_aux TO NULL
      IF(r_continuar)THEN 

         DIALOG ATTRIBUTE(UNBUFFERED)

            DISPLAY ARRAY v_nrp_invadidos TO sr_nrp_invadido.*
               BEFORE ROW
                  CALL fn_recupera_detalle_invadido(v_nrp_invadidos[ARR_CURR()],v_nss_invadido)
                  DISPLAY v_nrp_detalle_invadido.getLength() TO flbl_total_invadidos

            END DISPLAY

            DISPLAY ARRAY v_nrp_asociados TO sr_nrp_asociado.*
               BEFORE ROW
                  CALL fn_recupera_detalle_asociado(v_nrp_asociados[ARR_CURR()],v_nss_asociado)
                  DISPLAY v_nrp_detalle_asociado.getLength() TO flbl_total_asociados

            END DISPLAY

            DISPLAY ARRAY v_nrp_detalle_invadido TO sr_nrp_detalle_invadido.*

            END DISPLAY

            DISPLAY ARRAY v_nrp_detalle_asociado TO sr_nrp_detalle_asociado.*

            END DISPLAY

            --ON ACTION aceptar

            ON ACTION cancelar
               EXIT DIALOG

            BEFORE DIALOG
               DISPLAY v_nrp_detalle_invadido.getLength() TO flbl_total_invadidos
               DISPLAY v_nrp_detalle_asociado.getLength() TO flbl_total_asociados

         END DIALOG
      END IF

   CLOSE WINDOW vtna_consulta_nrp 

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC40                                                   #
#Descripcion       => funcion que recupera los nrp asociados al nss            #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 17 Mayo 2012                                             #
################################################################################
FUNCTION fn_recupera_nrp(p_nss_invadido_aux,p_nss_asociado_aux)
DEFINE v_filtro    STRING,
       v_continuar BOOLEAN,
       --v_regresar  BOOLEAN,
       p_nss_invadido_aux LIKE sep_det_02_op29.invadido, # NSS recibido por SEPC22(consulta de expedientes)
       p_nss_asociado_aux LIKE sep_det_03_op29.asociado, # NSS recibido por SEPC22(consulta de expedientes)
       v_nss_invadido LIKE sep_det_02_op29.invadido,
       v_nss_asociado LIKE sep_det_03_op29.asociado

   # se inicializa true para la primera consulta
   --LET v_regresar  = TRUE
   --WHILE (v_regresar)
      # constructor para hacer el filtro de la consulta de nrp
      {CONSTRUCT v_filtro ON a.nss, b.nss FROM edi_nss_invadido, edi_nss_asociado
         ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)}
      INPUT v_nss_invadido,v_nss_asociado WITHOUT DEFAULTS FROM edi_nss_invadido, edi_nss_asociado
        ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)

         BEFORE INPUT
            IF(p_nss_invadido_aux IS NOT NULL AND p_nss_asociado_aux IS NOT NULL)THEN
               DISPLAY p_nss_invadido_aux TO edi_nss_invadido
               DISPLAY p_nss_asociado_aux TO edi_nss_asociado
               LET v_nss_invadido = p_nss_invadido_aux               
               LET v_nss_asociado = p_nss_asociado_aux               
               --LET v_regresar  = FALSE
               LET v_continuar = TRUE
               CALL fn_recupera_nrps(v_nss_invadido,v_nss_asociado) RETURNING v_continuar
               IF NOT(v_continuar)THEN
                   CALL fn_mensaje("","No se encontró información con criterio dado","")
                   EXIT PROGRAM
                   ELSE 
                  ACCEPT INPUT
               END IF               
            ELSE
               # despliega detalle de nrp
               LET v_continuar = TRUE
               --LET v_regresar  = FALSE
            END IF

         ON ACTION aceptar
            --LET v_regresar  = FALSE
            # despliega detalle de nrp
            {LET v_continuar = TRUE
            CALL fn_recupera_nrps(p_nss_invadido_aux,p_nss_asociado_aux) RETURNING v_regresar
            EXIT INPUT}

            CALL fn_recupera_nrps(v_nss_invadido,v_nss_asociado) RETURNING v_continuar
            IF NOT(v_continuar)THEN
               CALL fn_mensaje("","No se encontró información con criterio dado","")
               --INITIALIZE v_nss_invadido,v_nss_asociado TO NULL 
               # si no encontró información, continua con el input
               CONTINUE INPUT
            END IF
            LET v_continuar = TRUE
            ACCEPT INPUT
         
         ON ACTION cancelar
            # no despliega detalle de nrp y sale de la pantalla            
            LET v_continuar = FALSE
            INITIALIZE v_nss_invadido,v_nss_asociado TO NULL
            # no vuelve a realizar la consulta
            --LET v_regresar  = FALSE
            EXIT INPUT

      END INPUT
      # se realiza la consulta si es que se indica en el construct
      {IF(v_continuar)THEN
         CALL fn_recupera_nrps(p_nss_invadido_aux,p_nss_asociado_aux) RETURNING v_regresar
      END IF}
   --END WHILE
   
   RETURN v_continuar,v_nss_invadido,v_nss_asociado
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC40                                                   #
#Descripcion       => funcion que recupera el detalle de los nrp               #
#                     asociados al nss                                         #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 17 Mayo 2012                                             #
################################################################################
FUNCTION fn_recupera_nrps(p_nss_invadido_aux,p_nss_asociado_aux)
DEFINE p_nss_invadido_aux LIKE sep_det_02_op29.invadido, # NSS recibido por SEPC22(consulta de expedientes)
       p_nss_asociado_aux LIKE sep_det_03_op29.asociado, # NSS recibido por SEPC22(consulta de expedientes)
       v_consulta STRING,
       v_regresar BOOLEAN,
       v_indice   SMALLINT,
       v_inv_invadidos CHAR(11), --LIKE sep_det_05_op29.nrp,
       v_aso_invadidos CHAR(11)  --LIKE sep_det_06_op29.nrp

   WHENEVER ERROR CONTINUE

   # RECUPERA DATOS DE INVADIDO
   # suponiendo que no se recupera informacion
   LET v_regresar = FALSE
   --INITIALIZE v_nrp_invadidos TO NULL
   CALL v_nrp_invadidos.clear()
   
   LET v_consulta = "\n SELECT det05.nrp",
                    "\n   FROM sep_det_02_op29 det02 JOIN sep_det_05_op29 det05",
                    "\n     ON det05.id_det_02_op29 = det02.id_det_02_op29",
                    "\n  WHERE det02.invadido = ?",
                    "\n  GROUP BY det05.nrp"
   PREPARE prp_recupera_invadidos_invadido FROM v_consulta
   DECLARE cur_recupera_invadidos_invadido CURSOR FOR prp_recupera_invadidos_invadido
   LET v_indice = 1 
   FOREACH cur_recupera_invadidos_invadido USING p_nss_invadido_aux INTO v_inv_invadidos 
      LET v_nrp_invadidos[v_indice] = v_inv_invadidos CLIPPED
      LET v_regresar = TRUE
      LET v_indice = v_indice + 1 
   END FOREACH 
   FREE cur_recupera_invadidos_invadido

   # RECUPERA DATOS DE ASOCIADO
   # suponiendo que no se recupera informacion
   --LET v_regresar = FALSE
   --INITIALIZE v_nrp_asociados TO NULL 
   CALL v_nrp_asociados.clear()
   
   LET v_consulta = "\n SELECT det06.nrp",
                    "\n   FROM sep_det_03_op29 det03 JOIN sep_det_06_op29 det06",
                    "\n     ON det06.id_det_03_op29 = det03.id_det_03_op29",
                    "\n  WHERE det03.asociado = ?",
                    "\n  GROUP BY det06.nrp"
   PREPARE prp_recupera_invadidos_asociado FROM v_consulta
   DECLARE cur_recupera_invadidos_asociado CURSOR FOR prp_recupera_invadidos_asociado
   LET v_indice = 1 
   
   FOREACH cur_recupera_invadidos_asociado USING p_nss_asociado_aux INTO v_aso_invadidos
      LET v_nrp_asociados[v_indice] = v_aso_invadidos CLIPPED
      LET v_regresar = TRUE
      LET v_indice = v_indice + 1 
   END FOREACH 
   FREE cur_recupera_invadidos_asociado

   RETURN v_regresar
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC40                                                   #
#Descripcion       => funcion que recupera el detalle de los nrp               #
#                     invadidos al nss                                         #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 30 Mayo 2012                                             #
################################################################################
FUNCTION fn_recupera_detalle_invadido(v_nrp,p_nss_invadido_aux)
DEFINE v_consulta STRING,
       v_indice   SMALLINT,
       v_nrp      LIKE sep_det_05_op29.nrp,
       p_nss_invadido_aux LIKE sep_det_02_op29.invadido,
       v_nrp_det_inv_aux RECORD
         v_folio         DECIMAL(9,0), --LIKE sep_det_05_op29.folio,
         v_fecha_proceso LIKE sep_det_02_op29.f_proceso,
         v_nrp           LIKE sep_det_05_op29.nrp
       END RECORD

   WHENEVER ERROR CONTINUE

   CALL v_nrp_detalle_invadido.clear()
   LET v_consulta = "\n SELECT det02.folio, det02.f_proceso, det05.nrp",
                    "\n   FROM sep_det_02_op29 det02 JOIN sep_det_05_op29 det05",
                    "\n     ON det05.id_det_02_op29 = det02.id_det_02_op29",
                    "\n  WHERE det02.invadido = ?",
                    "\n    AND det05.nrp = ?"
   PREPARE prp_recupera_det_inv_inv FROM v_consulta
   DECLARE cur_recupera_det_inv_inv CURSOR FOR prp_recupera_det_inv_inv
   LET v_indice = 1
   FOREACH cur_recupera_det_inv_inv USING p_nss_invadido_aux,
                                          v_nrp 
                                     INTO v_nrp_det_inv_aux.* 
      LET v_nrp_detalle_invadido[v_indice].v_folio         = v_nrp_det_inv_aux.v_folio
      LET v_nrp_detalle_invadido[v_indice].v_fecha_proceso = v_nrp_det_inv_aux.v_fecha_proceso USING "dd-mm-yy"
      LET v_nrp_detalle_invadido[v_indice].v_nrp           = v_nrp_det_inv_aux.v_nrp
      LET v_indice = v_indice + 1
   END FOREACH 
   
   FREE cur_recupera_det_inv_inv

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC40                                                   #
#Descripcion       => funcion que recupera el detalle de los nrp               #
#                     asociados al nss                                         #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 31 Mayo 2012                                             #
################################################################################
FUNCTION fn_recupera_detalle_asociado(v_nrp,p_nss_asociado_aux)
DEFINE v_consulta STRING,
       v_indice   SMALLINT,
       v_nrp      LIKE sep_det_05_op29.nrp,
       p_nss_asociado_aux LIKE sep_det_02_op29.invadido,
       v_nrp_det_aso_aux RECORD
         v_folio         DECIMAL(9,0), --LIKE sep_det_06_op29.folio,
         v_fecha_proceso DATE, --LIKE sep_det_03_op29.f_proceso,
         v_nrp           LIKE sep_det_06_op29.nrp
       END RECORD

   WHENEVER ERROR CONTINUE

   CALL v_nrp_detalle_asociado.clear()
   LET v_consulta = "\n SELECT det02.folio, det02.f_proceso, det06.nrp",
                    "\n   FROM sep_det_03_op29 det03 JOIN sep_det_06_op29 det06",
                    "\n     ON det06.id_det_03_op29 = det03.id_det_03_op29",
                    "\n        JOIN sep_det_02_op29 det02",
                    "\n     ON det02.id_det_02_op29 = det03.id_det_02_op29",
                    "\n  WHERE det03.asociado = ?",
                    "\n    AND det06.nrp = ?"
   PREPARE prp_recupera_det_inv_aso FROM v_consulta
   DECLARE cur_recupera_det_inv_aso CURSOR FOR prp_recupera_det_inv_aso
   LET v_indice = 1
   FOREACH cur_recupera_det_inv_aso USING p_nss_asociado_aux,
                                          v_nrp 
                                     INTO v_nrp_det_aso_aux.* 
      LET v_nrp_detalle_asociado[v_indice].v_folio         = v_nrp_det_aso_aux.v_folio
      LET v_nrp_detalle_asociado[v_indice].v_fecha_proceso = v_nrp_det_aso_aux.v_fecha_proceso USING "dd-mm-yy"
      LET v_nrp_detalle_asociado[v_indice].v_nrp           = v_nrp_det_aso_aux.v_nrp
      LET v_indice = v_indice + 1
   END FOREACH 
   
   FREE cur_recupera_det_inv_aso

END FUNCTION