################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIL05                                                        #
#Objetivo     => Invoca la carga de datos para unificación de cuentas          #
#Fecha inicio => 21/05/2012                                                    #
################################################################################

--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 15/01/2014
-- Fecha      -- Modificación                                               
-- 15-01-2014 Se agrego un parámetro de consulta en la función de unificado   AG
-- 12-11-2014 Se corrige display de crédito para que no se limpie al cambiar de registro AG
--==============================================================================


DATABASE safre_viv                                                                                                                                       
GLOBALS "UNIG01.4gl"
DEFINE g_usuario_cod    LIKE seg_usuario.usuario_cod,
       g_tipo_ejecucion SMALLINT,
       g_s_titulo       CHAR(25),
       g_proceso_cod    SMALLINT
DEFINE v_arr_unificador DYNAMIC ARRAY OF RECORD                                                                                                          
            folio_unificacion    DECIMAL (9,0),                                                                                                          
            folio_liquidacion    DECIMAL (9,0) ,
            nss                  CHAR (11) ,                                                                                                             
            nombre               CHAR (50) ,                                                                                                             
            estado_unificacion   CHAR (18) ,                                                                                                              
            diagnostico          CHAR (25) ,                                                                                                              
            tpo_credito          CHAR (18) ,                                                                                                              
            f_movimiento         DATE ,                                                                                                                  
            f_liquidacion        DATE,                                                                                                                   
            id_inf_unificador    DECIMAL(9,0) ,
            v_estado_familia     CHAR (18)           
        END RECORD                                                                                                                                       
DEFINE v_arr_unificado DYNAMIC ARRAY OF RECORD
           id_inf_unificado      DECIMAL (9,0),
           nss_unificado         CHAR (11),                                                                                                              
           nrp_unificado         CHAR (11),   
           nombre                CHAR (45),
           curp_unificado        CHAR (18),                                                                                                              
           rfc_unificado         CHAR (13), 
           ---
           v_tpo_originacion     CHAR(30),
           v_tpo_credito         CHAR(30),
           v_numero_credito      DECIMAL(10,0),
           ---
           estado_unificado      SMALLINT,--CHAR (30),  --unificador           
           diagnostico_unificado CHAR (25),--unificador  
           folio_unificacion_uni DECIMAL(9,0),  --unificador
           id_derechohabiente    DECIMAL(9,0)
       END RECORD

DEFINE v_arr_unificador_unificado DYNAMIC ARRAY OF RECORD
            --Varables del unificador
            v_folio_unificacion    DECIMAL (9,0),                                                                                                          
            v_folio_liquidacion    DECIMAL (9,0) ,                                                                                                         
            v_nss                  CHAR (11) ,                                                                                                             
            v_nombre               CHAR (50) ,                                                                                                             
            v_estado_unificacion   CHAR (18) ,                                                                                                              
            v_diagnostico          CHAR (25) ,
            v_tpo_credito          CHAR (18) ,                                                                                                              
            v_f_movimiento         DATE ,                                                                                                                  
            v_f_liquidacion        DATE,                                                                                                                   
            v_estado               CHAR (18),
           --Variables del unificado 
           v_nss_u                 CHAR(11),                                                                                                              
           v_nrp_u                 CHAR(11),                                                                                                              
           v_curp_u                CHAR(18),                                                                                                              
           v_rfc_u                 CHAR(13),                                                                                                              
           v_tpo_credito_u         CHAR(18) ,--unificador
           v_estado_u              CHAR(18),  --unificador           
           v_diagnostico_u         CHAR(25),--unificador  
           v_folio_unificacion_u   DECIMAL(9,0),  --unificador
           v_nombre_unificado      CHAR(45)
           
END RECORD 
DEFINE v_arr_total DYNAMIC ARRAY OF RECORD   
            t_id_inf_unificador      DECIMAL(9,0) ,
            t_folio_unificacion      DECIMAL(9,0) ,
            t_folio_liquidacion      DECIMAL(9,0) ,
            t_unificador            SMALLINT ,
            t_unificado              SMALLINT    
       END RECORD 

DEFINE arr_totales_unificacion DYNAMIC ARRAY OF RECORD 
       v_tot_folio_unificacion INTEGER, 
       v_tot_folio_liquidacion INTEGER,
       v_tot_unificador        INTEGER,
       v_tot_unificado         INTEGER 
END RECORD
       
DEFINE   v_arr_cmb_folio RECORD
          folio           LIKE glo_folio.folio,
          nombre_archivo LIKE glo_ctr_archivo.nombre_archivo
       END RECORD     
DEFINE   v_arr_cmb_tipo_cred RECORD
          folio          LIKE glo_folio.folio,
          nombre_archivo LIKE glo_ctr_archivo.nombre_archivo
       END RECORD
DEFINE v_arr_tpo_credito DYNAMIC  ARRAY OF  char(18) ,                                                                                                         
       v_folio_unificacion  DECIMAL(9,0),                                                                                                                
       v_folio_liquidacion  DECIMAL(9,0),
       v_comando            STRING  --Cadena para comando de ejecución 
                                                                                                                                                         
MAIN     
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana
      
   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   
   LET g_usuario_cod = p_usuario_cod
   LET g_tipo_ejecucion = p_tipo_ejecucion
   LET g_s_titulo = p_s_titulo

   LET g_proceso_cod = g_proceso_cod_uni_infonavit

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   CALL Consulta_diagnosticos(p_usuario_cod)

END MAIN
                                                                                                                                                 
FUNCTION Consulta_diagnosticos(p_usuario_cod)                                                                                                                         
DEFINE v_cadena_condiciones STRING  ,                                                                                                           
       v_nss                CHAR(11),                                                                                                                  
       v_diagnostico        SMALLINT,                                                                                                                  
       v_f_liquidacion      DATE   ,
       v_fecha_actual       DATE,
       p_usuario_cod        LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       v_s_reporte          STRING    -- Para insertar en la forma el botón de reporte 

DEFINE f_ventana ui.Window, --provee la interfaz para la ventana
       f_forma ui.Form      --provee la interfaz para la forma  

   LET v_folio_unificacion= NULL 
   LET v_diagnostico=NULL
   LET v_nss=NULL
   LET v_folio_liquidacion = NULL

   OPEN WINDOW wMain WITH FORM "UNIC08"                                                                                                                 
   CLOSE WINDOW SCREEN   
      CALL fn_oculta_campos ("gru_unificador",1)   
      CALL fn_oculta_campos ("gru_unificado",1)   
      CALL fn_oculta_campos ("gru_total",1)
      CALL fn_oculta_campos ("gr_totales",1)      
      CALL FGL_SETTITLE("Detalle de la consulta de la Unificación") 
      
      CALL fn_llena_combo_folio()

      LET v_fecha_actual=TODAY

      DISPLAY  BY NAME  v_fecha_actual

      CALL v_arr_unificador.clear()                                                                                                                    

      DIALOG ATTRIBUTES (UNBUFFERED)

         INPUT BY NAME v_folio_unificacion ,v_folio_liquidacion,
                       v_nss  ,v_diagnostico,v_f_liquidacion
 
            BEFORE INPUT
               LET f_ventana = ui.Window.getCurrent()
               LET f_forma = f_ventana.getForm()
               CALL f_forma.setElementHidden("gr_reporte",TRUE) --Oculta la sección de Reporte
               LET v_s_reporte = ""
         END INPUT

         ON ACTION  ACCEPT
            CALL fn_oculta_campos ("gru_unificador",0)
            CALL fn_oculta_campos ("gru_unificado",0)   
            CALL fn_oculta_campos ("gru_total",0)
            CALL fn_oculta_campos ("gru_total",0)
            CALL fn_oculta_campos ("gr_totales",0)
            CALL v_arr_unificador.clear()
            CALL v_arr_unificado.clear()
            CALL v_arr_total.clear()

            LET v_s_reporte = ""

            CALL arma_where( v_folio_unificacion ,v_folio_liquidacion,
                             v_nss  ,v_diagnostico,v_f_liquidacion) 
                 RETURNING v_cadena_condiciones

            CALL muestra_consulta (v_cadena_condiciones,p_usuario_cod)
         ON ACTION cancelar
            EXIT DIALOG
      END DIALOG
  CLOSE WINDOW wMain                                                                                                                                     
END FUNCTION                                                                                                                                             

#OBJETIVO: Ejecutar la consulta en base a los parámetros capturados
FUNCTION muestra_consulta (v_cadena_condiciones,p_usuario_cod)   
DEFINE v_cadena_condiciones STRING,
       id_derechohabiente   DECIMAL(9,0)  ,
       v_sql_sting          STRING,
       p_usuario_cod        LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       v_si_indice2         SMALLINT, -- indice del reporte   
       v_s_reporte          STRING,-- Para insertar en la forma el botón de reporte
       v_id_derechohabiente DECIMAL(9,0),
       indx                 INTEGER

DEFINE f_ventana ui.Window, --provee la interfaz para la ventana
       f_forma ui.Form      --provee la interfaz para la forma  

   CALL llena_consulta_unificador (v_cadena_condiciones)    
   RETURNING indx

   --Si no se encuentran datos envía mensaje
   IF indx <= 1 THEN 
      CALL fn_oculta_campos ("gru_unificador",1)   
      CALL fn_oculta_campos ("gru_unificado",1)   
      CALL fn_oculta_campos ("gru_total",1)
      CALL fn_oculta_campos ("gr_totales",1)
      CALL fn_mensaje ("Atención", "No se encontraron registros con los datos proporcionados", "stop")
   END IF

   DIALOG ATTRIBUTES (UNBUFFERED)
      DISPLAY ARRAY v_arr_unificador  TO tb_unificador.*
         BEFORE  ROW
            LET v_arr_unificador[ARR_CURR()].tpo_credito = v_arr_tpo_credito [ARR_CURR()]

            CALL llena_consulta_unificado(v_arr_unificador[ARR_CURR()].folio_unificacion,v_arr_unificador[ARR_CURR()].id_inf_unificador)

            CALL fn_llena_totales(v_cadena_condiciones,
                                  v_arr_unificador[ARR_CURR()].folio_unificacion)
         BEFORE DISPLAY 
            LET f_ventana = ui.Window.getCurrent()
            LET f_forma = f_ventana.getForm()
            CALL f_forma.setElementHidden("gr_reporte",FALSE) --Muestra la sección de Reporte
             
         ON ACTION Consulta_liquidacion             
            LET v_sql_sting="\n SELECT folio_liquidacion, ",
                            "\n        id_derechohabiente ",                     
                            "\n FROM   uni_inf_unificador ",
                            "\n WHERE  id_inf_unificador = ",v_arr_unificador[ARR_CURR()].id_inf_unificador

            PREPARE c_id_derechohabiente FROM v_sql_sting
            EXECUTE c_id_derechohabiente INTO v_folio_liquidacion, id_derechohabiente
   
            --Valida si el folio de liquidación no es nulo ejecuta consulta
            IF v_folio_liquidacion IS NOT NULL THEN 
               CALL consulta_liquidados_INFONAVIT(v_folio_liquidacion, id_derechohabiente, v_arr_unificador[ARR_CURR()].id_inf_unificador ) 
            ELSE 
               CALL fn_mensaje("Atención", "No se ha ejecutado la liquidación", "stop")
            END IF 

         ON ACTION Consulta_Unificador                                                                          
            CALL Consulta_Uni_Cuentas_Unificador_INFONAVIT(v_arr_unificador[ARR_CURR()].folio_unificacion, 
                                                              v_arr_unificador[ARR_CURR()].nss)    
                                                    
         ON ACTION Consulta_Derechohabiente
            --Se ejecuta la función general de consulta de Derechohabiente
            LET v_comando = "fglrun ../../afi/bin/AFIC01.42r " || g_usuario_cod || "1 'Consulta de Derechohabiente' " || v_arr_unificador[ARR_CURR()].nss
            RUN v_comando

         ON ACTION Consulta_Saldo       
            -- Muestra la consulta del saldo   
            LET v_sql_sting = "   SELECT id_derechohabiente",
                              "\n FROM afi_derechohabiente",
                              "\n WHERE nss = ","'",v_arr_unificador[ARR_CURR()].nss,"'"

            PREPARE Prp_consulta_dor FROM v_sql_sting
            EXECUTE Prp_consulta_dor INTO v_id_derechohabiente
            
            CALL fn_eje_consulta(1,g_usuario_cod,v_id_derechohabiente, 
                                 g_tipo_ejecucion, g_s_titulo)
      END DISPLAY                                                                                                                                 

      DISPLAY ARRAY v_arr_unificado  TO tb_unificado.*             
         ON ACTION consulta_Unificado
           CALL Consulta_Uni_Cuentas_Unificado_INFONAVIT(v_arr_unificado[ARR_CURR()].folio_unificacion_uni,v_arr_unificado[ARR_CURR()].id_inf_unificado,
                                               v_arr_unificado[ARR_CURR()].nss_unificado)  
         ON ACTION Consulta_Derechohabiente                    
            --Se ejecuta la función general de consulta de Derechohabiente
            LET v_comando = "fglrun ../../afi/bin/AFIC01.42r " || g_usuario_cod || "1 'Consulta de Derechohabiente' " || v_arr_unificado[ARR_CURR()].nss_unificado
            RUN v_comando
    
         ON ACTION Consulta_Saldo
            -- Muestra la consulta del saldo
            LET v_sql_sting = "   SELECT id_derechohabiente",
                              "\n FROM   afi_derechohabiente",
                              "\n WHERE  nss = ","'",v_arr_unificado[ARR_CURR()].nss_unificado,"'"
            
            PREPARE Prp_consulta FROM v_sql_sting
            EXECUTE Prp_consulta INTO v_id_derechohabiente

            CALL fn_eje_consulta(1,g_usuario_cod,v_id_derechohabiente, 
                                 g_tipo_ejecucion, g_s_titulo)
      END DISPLAY  
   
      DISPLAY ARRAY arr_totales_unificacion TO scr_totales.*
      END DISPLAY 
        
      ON ACTION regresar 
         CALL fn_oculta_campos ("gru_unificador",1)   
         CALL fn_oculta_campos ("gru_unificado",1)   
         CALL fn_oculta_campos ("gru_total",1)
         CALL fn_oculta_campos ("gr_totales",1)    
         CALL f_forma.setElementHidden("gr_reporte",TRUE) --Oculta la sección de Reporte
        
         EXIT DIALOG  
   
      ON ACTION reporte 
         --Genera el reporte de Unificador/Unificado
         CALL fn_genera_reporte_unificados(p_usuario_cod) RETURNING v_si_indice2,v_s_reporte
          
         IF(v_si_indice2 = 0)THEN
            CALL fn_mensaje("Atención",
                            "No se pudo generar el reporte",
                            "information")
         END IF	  
      
      DISPLAY v_s_reporte TO v_s_reporte                                                                                                                                    
   END DIALOG
END FUNCTION

#OBJETIVO: Consultar los detalles del UNIFICADOR 
FUNCTION llena_consulta_unificador (v_cadena_condiciones)
DEFINE v_sqltxt             STRING,
       indx                 INTEGER ,
       v_cadena_condiciones STRING,
       v_id_derechohabiente DECIMAL(9,0)

   LET v_sqltxt="\n SELECT folio_unificacion,",
                "\n        folio_liquidacion,",
                "\n        nss,",
                "\n        nombre,",
                "\n        estado_unificacion,",
                "\n        diagnostico,",
                "\n        tpo_credito,",
                "\n        f_movimiento,",
                "\n        f_liquidacion,",
                "\n        id_inf_unificador, ",
                "\n        estado_familia",
                "\n FROM   uni_inf_unificador b"
                , v_cadena_condiciones

   CALL v_arr_liq_unificador.clear()

   LET indx = 1
	  
   DECLARE  c_unificador CURSOR FROM   v_sqltxt
   FOREACH  c_unificador INTO v_arr_unificador[indx].* 
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   uni_inf_unificador
      WHERE  id_inf_unificador = v_arr_unificador[indx].id_inf_unificador
      
      IF v_id_derechohabiente IS NULL THEN
         LET v_arr_unificador[indx].tpo_credito = 0
      END IF
      
      SELECT id_credito
      INTO   v_arr_unificador[indx].tpo_credito
      FROM   afi_derechohabiente
      WHERE  id_derechohabiente = v_id_derechohabiente
               
      IF v_arr_unificador[indx].tpo_credito = 0 THEN          
         LET v_arr_unificador[indx].tpo_credito = "SIN CRÉDITO"
      END IF
      
      IF v_arr_unificador[indx].tpo_credito = 1 THEN
         LET v_arr_unificador[indx].tpo_credito = "CON CRÉDITO"
      END IF

      --Obtiene descripción estado unificación
      SELECT estado_desc_corta
      INTO   v_arr_unificador[indx].estado_unificacion
      FROM   uni_estado_solicitud
      WHERE  id_estado= v_arr_unificador[indx].estado_unificacion

      SELECT estado_desc_corta
      INTO   v_arr_unificador[indx].v_estado_familia
      FROM   uni_estado_solicitud
      WHERE  id_estado= v_arr_unificador[indx].v_estado_familia

      SELECT diag_desc_larga
      INTO   v_arr_unificador[indx].diagnostico
      FROM   uni_diagnostico_sol
      WHERE  id_diagnostico= v_arr_unificador[indx].diagnostico 
        
      LET indx = indx + 1
   END FOREACH

   CALL v_arr_unificador.deleteElement(v_arr_unificador.getLength())
   
   RETURN indx

END FUNCTION

#OBJETIVO: Consultar la información de DETALLE del UNIFICADO  
FUNCTION llena_consulta_unificado (v_folio,id_inf_unificador)
DEFINE v_folio           DECIMAL(9,0),
       id_inf_unificador DECIMAL (9,0),
       v_sqltxt          STRING,
       indx1             INTEGER ,
       v_sql_desc        STRING,
       v_edo_desc_corta  CHAR (30),
       v_diag_desc_larga CHAR (30)

   LET v_sqltxt= "\n SELECT a.id_inf_unificado,",
                 "\n        a.nss,",
                 "\n        b.nrp,",
                 "\n        a.nombre,",
                 "\n        b.curp,",
                 "\n        b.rfc,",
                 "\n        a.estado_unificacion,",
                 "\n        a.diagnostico,",
                 "\n        a.folio_unificacion,",
                 "\n        a.id_derechohabiente",
                 "\n FROM   uni_inf_unificado a,",
                 "\n        uni_inf_unificador b",
                 "\n WHERE  a.id_unificador     = b.id_inf_unificador",
                 "\n AND    b.folio_unificacion = a.folio_unificacion",
                 "\n AND    a.id_unificador     =",id_inf_unificador

   LET indx1 =1
   CALL v_arr_unificado.clear()   
   CALL ui.Interface.refresh()   
   DECLARE  c_unificado CURSOR FROM v_sqltxt      
   FOREACH  c_unificado INTO v_arr_unificado[indx1].id_inf_unificado,
                             v_arr_unificado[indx1].nss_unificado,
                             v_arr_unificado[indx1].nrp_unificado,
                             v_arr_unificado[indx1].nombre,
                             v_arr_unificado[indx1].curp_unificado,
                             v_arr_unificado[indx1].rfc_unificado,
                             v_arr_unificado[indx1].estado_unificado,
                             v_arr_unificado[indx1].diagnostico_unificado,
                             v_arr_unificado[indx1].folio_unificacion_uni,
                             v_arr_unificado[indx1].id_derechohabiente

      DECLARE c_creditos CURSOR  FOR                         
      --Consulta las descripciones de Originacion, Tipo de Credito y el Numero de Credito
      SELECT tr.originacion_desc,tc.desc_credito,ca.num_credito
      FROM   cre_acreditado ca, cat_cre_originacion tr, cat_tipo_credito tc 
      WHERE  tr.tpo_originacion = ca.tpo_originacion 
      AND    tc.tpo_originacion = tr.tpo_originacion 
      AND    tc.tpo_credito = ca.tpo_credito 
      AND    ca.id_derechohabiente = v_arr_unificado[indx1].id_derechohabiente
      AND    ca.estado <> 230
      GROUP BY 1,2,3

      FOREACH c_creditos INTO v_arr_unificado[indx1].v_tpo_originacion,
                              v_arr_unificado[indx1].v_tpo_credito,
                              v_arr_unificado[indx1].v_numero_credito

      SELECT estado_desc_corta
      INTO   v_edo_desc_corta 
      FROM   uni_estado_solicitud
      WHERE  id_estado= v_arr_unificado[indx1].estado_unificado
      
      SELECT diag_desc_larga
      INTO   v_diag_desc_larga
      FROM   uni_diagnostico_sol
      WHERE  id_diagnostico= v_arr_unificado[indx1].diagnostico_unificado
      END FOREACH
      LET indx1 = indx1 + 1
   END FOREACH
   
   CALL v_arr_unificado.deleteElement(v_arr_unificado.getLength())
END FUNCTION
                                                                                                                                                         
FUNCTION arma_where ( v_folio_unificacion ,v_folio_liquidacion,                                                                                          
                          v_nss  ,v_diagnostico,v_f_liquidacion)                                                                                         
DEFINE  v_folio_unificacion    DECIMAL (9,0),                                                                                                            
        v_folio_liquidacion    DECIMAL (9,0) ,                                                                                                           
        v_nss                  CHAR (11) ,                                                                                                               
        v_diagnostico          SMALLINT,                                                                                                                 
        v_f_liquidacion        DATE,                                                                                                                     
        v_cadena_condiciones             STRING                                                                                                                    
      LET v_cadena_condiciones ="\n WHERE 1=1 "                                                                                                                             

     --Concatena folio_unificación
     IF v_folio_unificacion IS NOT NULL THEN                                                                                                             
        LET v_cadena_condiciones=v_cadena_condiciones ||"\n AND b.folio_unificacion="||v_folio_unificacion CLIPPED                                                               
     END IF                             
     --Concatena folio_liquidacion
     IF v_folio_liquidacion IS NOT NULL THEN                                                                                                          
        LET v_cadena_condiciones=v_cadena_condiciones  ||"\n AND b.folio_liquidacion="||v_folio_liquidacion CLIPPED                                                                   
     END IF
     --Concatena NSS
     IF v_nss IS NOT NULL THEN                                                                                                                        
     LET v_cadena_condiciones=v_cadena_condiciones||"\n AND b.nss="||v_nss CLIPPED                                                                                                
     END IF         
     --Concatena estado_familia     
     IF v_diagnostico IS NOT NULL THEN                                                                                                                
        --LET v_cadena_condiciones=v_cadena_condiciones||" AND b.diagnostico="||v_diagnostico CLIPPED                                                                               
        --LET v_cadena_condiciones=v_cadena_condiciones||" AND b.estado_unificacion="||v_diagnostico CLIPPED
        LET v_cadena_condiciones=v_cadena_condiciones||"\n AND b.estado_familia="||v_diagnostico CLIPPED     
     END IF                                                                                           
     --Concatena fecha_liquidación
     IF v_f_liquidacion IS NOT NULL THEN                                                                                                              
        LET v_cadena_condiciones=v_cadena_condiciones||"\n AND b.f_liquidacion="||v_f_liquidacion CLIPPED                                                                           
     END IF                                                                                                                                              
                                                                                                                                                         
RETURN v_cadena_condiciones                                                                                                                                        
END FUNCTION 

FUNCTION fn_llena_totales(v_cadena_condiciones,
                          p_folio_unificacion)
DEFINE
    v_cadena_condiciones        STRING ,
    v_sqltxt2                   STRING ,
    v_sqltxt                    STRING,
    indx2                       INTEGER ,
    v_tot_unificador            INTEGER ,
    v_tot_unificado, i          INTEGER ,
    v_tot_folio_unificacion     INTEGER ,
    v_tot_folio_liquidacion     INTEGER ,
    v_cadena                    STRING,
    v_tot_folio_unificacion_liq INTEGER, 
    v_tot_folio_liquidacion_liq INTEGER,
    v_tot_unificador_liq        INTEGER ,
    v_tot_unificado_liq         INTEGER,
    v_suma_unificador           INTEGER,
    v_suma_unificado            INTEGER,
    v_i_indice                  INTEGER,
    p_folio_unificacion         DECIMAL (9,0)

DEFINE rec_tot_unificador_sliq RECORD 
       v_tot_folio_unificacion INTEGER, 
       v_tot_folio_liquidacion INTEGER,
       v_tot_unificador        INTEGER,
       v_tot_unificado         INTEGER 
END RECORD 

DEFINE rec_tot_unificador_liq RECORD 
       v_tot_folio_unificacion INTEGER, 
       v_tot_folio_liquidacion INTEGER,
       v_tot_unificador        INTEGER, 
       v_tot_unificado         INTEGER 
END RECORD 

   LET  v_tot_unificador            =0
   LET  v_tot_unificado             =0
   LET  v_tot_folio_unificacion     =0
   LET  v_tot_folio_liquidacion     =0
   LET  v_tot_folio_unificacion_liq =0
   LET  v_tot_folio_liquidacion_liq =0
   LET  v_tot_unificador_liq        =0
   LET  v_tot_unificado_liq         =0
   LET  v_suma_unificador           =0
   LET  v_suma_unificado            =0
     
   ---Obtiene  totales unificadores  no  liquidados
   LET v_sqltxt="\n SELECT b.folio_unificacion,", 
                "\n        b.folio_liquidacion,",
                "\n        count(b.folio_unificacion)",
                "\n FROM   uni_inf_unificador b",
                "\n WHERE b.folio_unificacion = ", p_folio_unificacion,
                "\n AND   b.folio_liquidacion IS NULL ",
                "\n GROUP BY 2,1"

   PREPARE c_total FROM v_sqltxt        
   DECLARE cur_total CURSOR FOR c_total 
   
   LET v_i_indice = 1
   
   FOREACH cur_total INTO arr_totales_unificacion[v_i_indice].v_tot_folio_unificacion,
                          arr_totales_unificacion[v_i_indice].v_tot_folio_liquidacion,
                          arr_totales_unificacion[v_i_indice].v_tot_unificador       
      
      ---Obtiene  totales unificados  no  liquidados
      LET v_cadena = " IN ("
      
      FOR i=1 TO v_arr_unificador.getLength()
         IF v_arr_unificador[i].f_liquidacion IS NULL THEN 
            IF i=v_arr_unificador.getLength() THEN 
               LET v_cadena =v_cadena ||v_arr_unificador[i].id_inf_unificador|| ","
            ELSE 
               LET v_cadena = v_cadena ||v_arr_unificador[i].id_inf_unificador ||","
            END IF 
         END IF 
      END FOR 

      LET v_cadena =v_cadena||"0)" 
      --Obtiene totales UNIFICADOS no liquidados      
      LET v_sqltxt2="\n SELECT COUNT (*) ",
                    "\n FROM   uni_inf_unificado",
                    "\n WHERE  folio_unificacion = ", v_arr_unificador[ARR_CURR()].folio_unificacion,
                    "\n AND    diagnostico <> 6" --No Liquidados
      --DISPLAY "--Obtiene totales UNIFICADOS no liquidados", v_sqltxt2
      PREPARE c_tot_unificado FROM v_sqltxt2
      EXECUTE c_tot_unificado INTO arr_totales_unificacion[v_i_indice].v_tot_unificado       
      
      --Obtiene totales  unificadores liquidados
      LET v_sqltxt= "\n SELECT b.folio_unificacion,",
                    "\n        b.folio_liquidacion,",
                    "\n        count (b.folio_unificacion)",
                    "\n FROM   uni_inf_unificador b",
                    "\n",{WHERE}  v_cadena_condiciones,
                    "\n AND    b.folio_liquidacion IS NOT NULL ",    
                    "\n GROUP BY folio_unificacion, folio_liquidacion"

      LET v_i_indice = v_i_indice + 1

      --DISPLAY "--Obtiene totales  UNIFICADORES liquidados: \n",v_sqltxt
      PREPARE c_total_liq FROM v_sqltxt
      DECLARE cur_folios_1 CURSOR FOR c_total_liq

      --EXECUTE c_total_liq INTO arr_totales_unificacion[v_i_indice].v_tot_folio_unificacion,
      FOREACH cur_folios_1 INTO arr_totales_unificacion[v_i_indice].v_tot_folio_unificacion,
                                arr_totales_unificacion[v_i_indice].v_tot_folio_liquidacion,
                                arr_totales_unificacion[v_i_indice].v_tot_unificador 
         ---Obtiene  totales unificados  liquidados
         LET v_cadena = " IN ("
         FOR i=1 TO v_arr_unificador.getLength()
            IF v_arr_unificador[i].f_liquidacion IS NOT NULL  THEN 
               IF i=v_arr_unificador.getLength() THEN 
                  LET v_cadena =v_cadena ||v_arr_unificador[i].id_inf_unificador||"," 
               ELSE 
                  LET v_cadena = v_cadena ||v_arr_unificador[i].id_inf_unificador ||"," 
               END IF 
            END IF 
         END FOR       
         LET v_cadena = v_cadena|| "0)"     
         LET v_sqltxt2="\n SELECT COUNT (*) ",
                       "\n FROM   uni_inf_unificado ",
                       "\n WHERE  folio_unificacion = ", v_arr_unificador[ARR_CURR()].folio_unificacion,
                       "\n AND    estado_unificacion = 1", --Aceptados
                       "\n AND    diagnostico = 6" --Liquidados

         --DISPLAY "--Obtiene totales  UNIFICADOS liquidados: \n",v_sqltxt2     
         PREPARE c_tot_unificado_liq FROM v_sqltxt2
         EXECUTE c_tot_unificado_liq INTO arr_totales_unificacion[v_i_indice].v_tot_unificado       

         IF arr_totales_unificacion[v_i_indice].v_tot_unificado = 0 THEN 
            LET arr_totales_unificacion[v_i_indice].v_tot_unificado = NULL
         END IF    
        
         LET v_i_indice = v_i_indice + 1
      END FOREACH
   END FOREACH

       LET v_suma_unificador = arr_totales_unificacion[1].v_tot_unificador +
                               arr_totales_unificacion[2].v_tot_unificador              
       
       LET v_suma_unificado = arr_totales_unificacion[1].v_tot_unificado +
                              arr_totales_unificacion[2].v_tot_unificado   


   DISPLAY BY NAME v_suma_unificado,v_suma_unificador
   
END FUNCTION

#OBJETIVO: Llenar el combo de folios que elegirá el usuario 
FUNCTION  fn_llena_combo_folio()
  DEFINE v_s_cadena      STRING ,
         v_cbx_folio     ui.ComboBox ,-- combo de folio   
         v_i_conArch     INTEGER     
         -- se llena el arreglo de folios
         LET v_cbx_folio = ui.ComboBox.forName("formonly.v_folio_unificacion")
         DECLARE cur_folios CURSOR FOR
                 SELECT DISTINCT fo.folio, ar.nombre_archivo
                   FROM glo_ctr_archivo ar, 
                        glo_folio fo
                   WHERE ar.proceso_cod = g_proceso_cod
                     AND ar.opera_cod = 1
                     AND ar.folio = fo.folio

         LET v_i_conArch = 0
         FOREACH cur_folios INTO v_arr_cmb_folio.*
            LET v_s_cadena = v_arr_cmb_folio.folio USING "##########", " - ", 
                v_arr_cmb_folio.nombre_archivo 
            CALL v_cbx_folio.addItem(v_arr_cmb_folio.folio ,v_s_cadena)
         END FOREACH 
END FUNCTION  


FUNCTION fn_oculta_campos (campo,valor)
  DEFINE valor SMALLINT
  DEFINE campo STRING
  DEFINE win ui.Window, fm ui.Form
  LET win = ui.Window.getCurrent()
  LET fm = win.getForm()
  CALL fm.setELEMENTHidden(campo, valor)
  --CALL fm.setElementHidden("lb2_18", valor)
END FUNCTION              

FUNCTION fn_genera_reporte_unificados(p_usuario_cod)
      --Define las variables para la generación del reporte
DEFINE 
         v_ind_rep1         SMALLINT, --índice para el reporte
         v_ind_rep2        SMALLINT, --índice para el reporte
         v_indx            SMALLINT, --índice para consulta
         v_manejador_rpt   om.SaxDocumentHandler,
         v_QryTxt            STRING, --Complemento de una consulta
         v_nom_reporte      VARCHAR(80), -- nombre del reporte
         r_ruta_bin         LIKE seg_modulo.ruta_bin,
         r_ruta_listados    LIKE seg_modulo.ruta_listados,
         p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
         v_s_reporte        STRING

         
         LET v_ind_rep1 = 1
         LET v_ind_rep2 = 1


         INITIALIZE v_arr_unificador_unificado TO NULL 
         
         --DISPLAY "tamaño: -- ",v_arr_unificador.getLength()
         FOR v_ind_rep1 = 1 TO  v_arr_unificador.getLength()

            --Valida si hay más unificadores o no
            --DISPLAY "nss -- ",v_arr_unificador[v_ind_rep1].nss
            IF v_arr_unificador[v_ind_rep1].nss IS NULL THEN 
               EXIT FOR 
            END IF 

            LET v_QryTxt = "\n SELECT ur.folio_unificacion, ",
                           "\n        ur.folio_liquidacion, ",
                           "\n        ur.nss, ",
                           "\n        ur.nombre, ",
                           "\n        ur.estado_unificacion, ",
                           "\n        d.diag_desc_larga, ",
                           "\n        ct.desc_credito, ",
                           "\n        ur.f_movimiento, ",
                           "\n        ur.f_liquidacion, ",
                           "\n        e.estado_desc_corta, ",
                           "\n        un.nss, ",
                           "\n        ur.nrp, ",
                           "\n        ur.curp, ",
                           "\n        ur.rfc, ",
                           "\n        ct.desc_credito, ",
                           "\n        un.estado_unificacion, ",
                           "\n        d.diag_desc_larga, ",
                           "\n        un.folio_unificacion, ",  
                           "\n        un.nombre ",                             
                           "\n   FROM uni_inf_unificador ur, ",
                           "\n        uni_inf_unificado un, ",
                           "\n        uni_estado_solicitud e, ",
                           "\n        uni_diagnostico_sol d, ",
                           "\n        cat_tipo_credito ct ",
                           "\n  WHERE ur.id_inf_unificador = un.id_unificador ",
                           "\n    AND ur.estado_familia = e.id_estado ",
                           "\n    AND d.id_diagnostico = ur.diagnostico ",
                           "\n    AND ur.tpo_credito = ct.tpo_credito "

            LET v_QryTxt = v_QryTxt ||  "\n AND ur.nss = ", 
                                 v_arr_unificador[v_ind_rep1].nss

            --DISPLAY "Consulta -------------------------------- \n", v_QryTxt
            PREPARE prp_datos_unificador_unificado FROM v_QryTxt CLIPPED
            DECLARE curr_datos_unificador_unificado CURSOR FOR prp_datos_unificador_unificado

            FOREACH curr_datos_unificador_unificado INTO v_arr_unificador_unificado[v_ind_rep2].v_folio_unificacion,
                                                         v_arr_unificador_unificado[v_ind_rep2].v_folio_liquidacion,
                                                         v_arr_unificador_unificado[v_ind_rep2].v_nss,
                                                         v_arr_unificador_unificado[v_ind_rep2].v_nombre,
                                                         v_arr_unificador_unificado[v_ind_rep2].v_estado_unificacion,
                                                         v_arr_unificador_unificado[v_ind_rep2].v_diagnostico,
                                                         v_arr_unificador_unificado[v_ind_rep2].v_tpo_credito,
                                                         v_arr_unificador_unificado[v_ind_rep2].v_f_movimiento,
                                                         v_arr_unificador_unificado[v_ind_rep2].v_f_liquidacion,
                                                         v_arr_unificador_unificado[v_ind_rep2].v_estado,

                                                         v_arr_unificador_unificado[v_ind_rep2].v_nss_u,
                                                         v_arr_unificador_unificado[v_ind_rep2].v_nrp_u,
                                                         v_arr_unificador_unificado[v_ind_rep2].v_curp_u,
                                                         v_arr_unificador_unificado[v_ind_rep2].v_rfc_u,
                                                         v_arr_unificador_unificado[v_ind_rep2].v_tpo_credito_u,
                                                         v_arr_unificador_unificado[v_ind_rep2].v_estado_u,
                                                         v_arr_unificador_unificado[v_ind_rep2].v_diagnostico_u,
                                                         v_arr_unificador_unificado[v_ind_rep2].v_folio_unificacion_u,
                                                         v_arr_unificador_unificado[v_ind_rep2].v_nombre_unificado

               --Obtiene descripción de estado_unificación                                          
               SELECT estado_desc_corta 
                 INTO v_arr_unificador_unificado[v_ind_rep2].v_estado_unificacion
                 FROM uni_estado_solicitud
                WHERE id_estado = v_arr_unificador_unificado[v_ind_rep2].v_estado_unificacion
                
               LET v_ind_rep2 = v_ind_rep2 + 1
            END FOREACH 
            
         END FOR 

         CALL v_arr_unificador_unificado.deleteElement(v_arr_unificador_unificado.getLength())

           # Recupera la ruta de listados en el que se enviara el archivo
           CALL fn_rutas("uni") RETURNING r_ruta_bin, r_ruta_listados
           # Se indica que el reporte usara la plantilla creada
           IF fgl_report_loadCurrentSettings("UNIC082.4rp") THEN
              CALL fgl_report_selectDevice("PDF")
              LET v_nom_reporte = p_usuario_cod CLIPPED || "-UNIC08-","00000","-","00000","-","00000"||".pdf"
              CALL fgl_report_selectPreview(1)
              
              CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)
              LET v_manejador_rpt = fgl_report_commitCurrentSettings()
      
           ELSE
              DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
           END IF
         
           START REPORT rp_unificados TO XML HANDLER v_manejador_rpt
            --DISPLAY "Tamaño unif ---- ", v_arr_unificador_unificado.getLength()
            FOR  v_ind_rep2 = 1 TO v_arr_unificador_unificado.getLength()
               OUTPUT TO REPORT rp_unificados(v_arr_unificador_unificado[v_ind_rep2].*,p_usuario_cod)
            END FOR
           
            FINISH REPORT rp_unificados
            FREE curr_datos_unificador_unificado

            LET v_s_reporte = "<a gwc:attributes=\"href resourceuri('",v_nom_reporte CLIPPED,"','unidocto')\" target='nueva'>",
                                                                                  v_nom_reporte CLIPPED,"</a>"
            
            RETURN v_ind_rep2,v_s_reporte
   
END FUNCTION 


--Reporte de unificadores/unificados
REPORT rp_unificados(v_arr_unificador_unificado, p_usuario_cod)
   DEFINE 
      v_arr_unificador_unificado RECORD
            --Varables del unificador
            v_folio_unificacion    DECIMAL (9,0),                                                                                                          
            v_folio_liquidacion    DECIMAL (9,0) ,                                                                                                         
            v_nss                  CHAR (11) ,                                                                                                             
            v_nombre               CHAR (50) ,                                                                                                             
            v_estado_unificacion   CHAR (18) ,                                                                                                              
            v_diagnostico          CHAR (25) ,                                                                                                              
            v_tpo_credito          CHAR (18) ,                                                                                                              
            v_f_movimiento         DATE ,                                                                                                                  
            v_f_liquidacion        DATE,                                                                                                                   
            v_estado               CHAR (18),

           --Variables del unificado 
           v_nss_u                  CHAR (11),                                                                                                              
           v_nrp_u                  CHAR (11),                                                                                                              
           v_curp_u                 CHAR (18),                                                                                                              
           v_rfc_u                  CHAR (13),                                                                                                              
           v_tpo_credito_u          CHAR (18) ,--unificador
           v_estado_u               CHAR (18),  --unificador           
           v_diagnostico_u          CHAR (25),--unificador  
           v_folio_unificacion_u    DECIMAL(9,0),  --unificador
           v_nombre_unificado       CHAR(45)
END RECORD 
      
   DEFINE 
         --Define variables del encabezado
         r_fecha_reporte      DATE,
         p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado

   FORMAT 
      FIRST PAGE HEADER
         LET r_fecha_reporte = TODAY CLIPPED
         PRINTX r_fecha_reporte USING "dd-mm-yyyy"
         PRINTX p_usuario_cod

      ON EVERY ROW
         --Información de unificadores/unificados
         PRINTX   v_arr_unificador_unificado.*
         
END REPORT 

