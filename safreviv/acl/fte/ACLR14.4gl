--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGR14                                                                 #
#Objetivo     => Programa que realiza el reverso total(carga/preliquidación)            #
#                para SAR 92                                                           #
#Fecha inicio => Enero 26, 2012                                                         #
#########################################################################################
DATABASE safre_viv

MAIN
DEFINE p_usuario_cod   LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion SMALLINT,
       p_cad_ventana   STRING,
       v_proceso_cod   LIKE cat_proceso.proceso_cod,
       v_opera_cod_carga          LIKE cat_operacion.opera_cod,
       v_opera_cod_integracion    LIKE cat_operacion.opera_cod,
       v_opera_cod_preliquidacion LIKE cat_operacion.opera_cod
       
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)
   LET v_proceso_cod = 106 # SAR92
   LET v_opera_cod_carga = 3 #Carga
   LET v_opera_cod_integracion = 2 ##Codigo de la integracion
   LET v_opera_cod_preliquidacion = 3 #Preliquidacion

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_cad_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_cad_ventana)
   END IF
   
   --Se invoca la funcion que efectua el reverso de la carga, integracion y preliquidación
   CALL fn_w_reverso_preliquidacion(p_usuario_cod
                                    ,p_cad_ventana
                                    ,v_proceso_cod
                                    ,v_opera_cod_preliquidacion)
       
END MAIN

{ ==========================================================================
Clave:  fn_reversa_total
Nombre: fn_reversa_total
Fecha creacion: 26 de Enero de 2012
Autor: Hugo César Ramírez García
Narrativa del proceso que realiza:
 Esta función captura/valida el folio a reversar para la carga/preliquidacion 
 de SAR 92
Parametros de Entrada:
 -
Parámetros de salida:
 -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_reversa_total(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)
DEFINE p_usuario_cod           LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion         SMALLINT,
       p_cad_ventana           STRING,
       r_ruta_bin              LIKE seg_modulo.ruta_bin,
       r_ruta_listados         LIKE seg_modulo.ruta_listados,
       v_folio                 LIKE glo_folio.folio,
       v_tmp_folio             LIKE glo_folio.folio,
       v_proceso_cod           LIKE cat_proceso.proceso_cod,
       v_opera_cod_integracion LIKE cat_operacion.opera_cod,
       r_sql_reverso           SMALLINT,
       v_bnd_continuar         SMALLINT

   CALL fn_rutas("acl") RETURNING r_ruta_bin,r_ruta_listados
   LET v_proceso_cod = 1402 #SAR 92
   LET v_opera_cod_integracion = 3 #Preliquidacion
       
   OPEN WINDOW w_reversaIntegracion WITH FORM r_ruta_bin CLIPPED||"/PAGR141"

      INPUT BY NAME v_folio ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
      
         BEFORE INPUT
            CALL ui.Interface.setText(p_cad_ventana)
            
            LET r_sql_reverso = 0

         ON ACTION reverso
            LET v_folio = GET_FLDBUF(v_folio) CLIPPED

            IF(v_folio IS NULL)THEN
               ERROR "Capture folio" ATTRIBUTE(REVERSE)
               NEXT FIELD v_folio 
            END IF
            IF(v_folio <= 0)THEN
               ERROR "Capture folio válido" ATTRIBUTE(REVERSE)
               NEXT FIELD v_folio 
            END IF
            
            LET v_tmp_folio = 0
            SELECT folio
              INTO v_tmp_folio
              FROM glo_folio
             WHERE folio = v_folio
             
            IF(v_tmp_folio = 0)THEN
               ERROR "No existe folio" ATTRIBUTE(REVERSE)
               NEXT FIELD v_folio 
            END IF
            CALL fn_ventana_confirma("Reverso Carga y Preliquidación","Se realizara el reverso para el folio "||
                                     v_folio||"\n¿Desea continuar?" ,"")
                            RETURNING v_bnd_continuar
            # v_bnd_continuar = 1 --> Aceptar
            # v_bnd_continuar = 2 --> Cancelar
            IF(v_bnd_continuar = 1)THEN
               CALL fn_reversa_integracion(v_folio,v_proceso_cod)
                              RETURNING r_sql_reverso
               IF(r_sql_reverso = 0)THEN
                  CALL fn_reversa_preliquidacion(v_folio,v_proceso_cod,v_opera_cod_integracion)
                              RETURNING r_sql_reverso
               END IF
            END IF
            
         ON ACTION salir
            EXIT INPUT

      END INPUT

   CLOSE WINDOW w_reversaIntegracion

END FUNCTION