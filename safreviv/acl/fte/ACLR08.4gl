--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#########################################################################################
#Modulo       => ACL                                                                    #
#Programa     => ACLR08                                                                 #
#Objetivo     => Programa que realiza el reverso preliquidacion enaclara                #
#Fecha inicio => Enero 26, 2012                                                         #
#Modificacion => se agrega archivo globales de aclaratorio y se sustituyen las variables#
#                correspondientes; hilda rivas                                          #
#########################################################################################
DATABASE safre_viv

GLOBALS "ACLG02.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod   LIKE seg_usuario.usuario_cod,
      -- p_tpo_ejecucion SMALLINT,
      -- p_cad_ventana   STRING,
      -- v_proceso_cod   LIKE cat_proceso.proceso_cod,
       --v_opera_cod_carga          LIKE cat_operacion.opera_cod,
      -- v_opera_cod_integracion    LIKE cat_operacion.opera_cod,
      -- v_opera_cod_preliquidacion LIKE cat_operacion.opera_cod
       p_d_folio        LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera        SMALLINT
       ,v_sql            STRING
       ,v_error_isam         INTEGER
       ,v_mensaje            VARCHAR(255)       

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- se inicia el log
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".ACLR08.log")       

   -- Se invoca rutina para reversar la preliquidación.
   CALL fn_reversa_preliquidacion(p_d_folio, g_proceso_cod, g_opera_cod)
      RETURNING r_bandera
      
   IF ( r_bandera = 0 ) THEN

      -- Reversa operación
      LET r_bandera = 0

      LET v_sql = " EXECUTE PROCEDURE sp_acl_enaclara_rev_preliquidacion(?)"
      PREPARE prp_exec_rev_prel_tran FROM v_sql
      EXECUTE prp_exec_rev_prel_tran INTO r_bandera, v_error_isam, v_mensaje
      USING p_d_folio

      DISPLAY "Resultado: ", r_bandera
      DISPLAY "Mensaje: ", v_mensaje      
      
      CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
      RETURNING r_bandera
     
      DISPLAY "El reverso se realizó con éxito"
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF   

      
END MAIN   

{       
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)
   LET v_proceso_cod = g_proceso_cod_acl_registro_pagos # enaclara
   --LET v_opera_cod_carga = 3 #Carga
   LET v_opera_cod_integracion = g_opera_cod_integracion ##Codigo de la integracion
   LET v_opera_cod_preliquidacion = g_opera_cod_preliquidacion #Preliquidacion

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_cad_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_cad_ventana)
   END IF

   
   --Se invoca la funcion que efectua el reverso de la carga, integracion y preliquidación
   CALL fn_reversa_total(p_usuario_cod
                         ,p_tpo_ejecucion
                         ,p_cad_ventana)
       
END MAIN
}

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
{
FUNCTION fn_reversa_total(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)
DEFINE p_usuario_cod           LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion         SMALLINT,
       p_cad_ventana           STRING,
       r_ruta_bin              LIKE seg_modulo.ruta_bin,
       r_ruta_listados         LIKE seg_modulo.ruta_listados,
       v_folio                 LIKE glo_folio.folio,
       v_tmp_folio             LIKE glo_folio.folio,
       v_proceso_cod           LIKE cat_proceso.proceso_cod,
       v_opera_cod_preliquidacion LIKE cat_operacion.opera_cod,
       r_sql_reverso           SMALLINT,
       v_bnd_continuar         SMALLINT,
       v_sql                   STRING,
       v_pid                   LIKE bat_ctr_proceso.pid,
       v_error_sql             INTEGER,
       v_error_isam            INTEGER,
       v_mensaje               VARCHAR(255),
       v_cbx_folios            ui.ComboBox -- combo de folios

   CALL fn_rutas("acl") RETURNING r_ruta_bin,r_ruta_listados
   LET v_proceso_cod = g_proceso_cod_acl_registro_pagos #enaclara
   LET v_opera_cod_preliquidacion = g_opera_cod_preliquidacion #Preliquidacion
       
   OPEN WINDOW w_reversaIntegracion WITH FORM r_ruta_bin CLIPPED||"/ACLR08"

      INPUT BY NAME v_folio ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
      
         BEFORE INPUT
            CALL ui.Interface.setText(p_cad_ventana)
            
            LET r_sql_reverso = 0
            
            -- se llena el combo de folios
            LET v_cbx_folios = ui.ComboBox.forName("formonly.v_folio")
  
            -- se inicia el combobox en blanco
            CALL v_cbx_folios.clear()

            -- se obtienen los folios preliquidados
            DECLARE cur_folios CURSOR FOR
            SELECT a.folio
            FROM   glo_folio a
            WHERE  a.proceso_cod = g_proceso_cod_acl_registro_pagos
            AND    a.status = 1
            ORDER BY a.folio DESC
            
            FOREACH cur_folios INTO v_folio
               CALL v_cbx_folios.addItem(v_folio, v_folio)
            END FOREACH
            
            LET v_folio = NULL

         ON ACTION reverso
            IF(v_folio IS NULL)THEN
               CALL fn_mensaje("Atención","Debe elegir un folio para ejecutar el reverso","stop")
               CONTINUE INPUT
            END IF

            CALL fn_ventana_confirma("Reverso de Preliquidación","Se realizara el reverso para el folio "||
                                     v_folio||"\n¿Desea continuar?" ,"")
                            RETURNING v_bnd_continuar
            # v_bnd_continuar = 1 --> Aceptar
            # v_bnd_continuar = 2 --> Cancelar
            IF ( v_bnd_continuar = 1 ) THEN

               -- se invoca el SP de reverso
               LET v_sql = "EXECUTE PROCEDURE sp_acl_enaclara_rev_preliquidacion(?)"
               PREPARE sid_folio FROM v_sql
               EXECUTE sid_folio USING v_folio
               INTO v_error_sql, v_error_isam, v_mensaje

               -- si el reverso se ejecuto correctamente
               IF ( v_error_sql = 0 ) THEN
               
                  -- se reversa la operacion
                  SELECT MAX(pid)
                  INTO   v_pid
                  FROM   bat_ctr_operacion
                  WHERE  proceso_cod = v_proceso_cod
                  
                  -- se reversa la operacion
                  CALL fn_reversa_operacion(v_pid, v_proceso_cod, v_opera_cod_preliquidacion) RETURNING r_sql_reverso
                  -- se verifica si se pudo realiza el reverso
                  IF ( r_sql_reverso = 0 ) THEN
                     CALL fn_mensaje("Atención","Se realizó el reverso de la preliquidación con éxito","info")
                  ELSE
                     CALL fn_mensaje("Atención","Ocurrió un error al realizar\nel reverso de la preliquidación","stop")
                  END IF
               ELSE
                  CALL fn_mensaje("Atención","Ocurrió un error al realizar\nel reverso de la preliquidación","stop")
                  DISPLAY "Error: ", v_error_sql
                  DISPLAY "Error (isam): ", v_error_isam
                  DISPLAY "Mensaje: ", v_mensaje
               END IF
            END IF
            
         ON ACTION cerrar
            EXIT INPUT

      END INPUT

   CLOSE WINDOW w_reversaIntegracion

END FUNCTION
}