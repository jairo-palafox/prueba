--=============================================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--=============================================================================
################################################################################
#MODULO            =>RET                                                       #
#PROGRAMA          =>RETWC01                                                   #
#OBJETIVO          =>PROGRAMA QUE EJECUTA CONSULTA DE SOLICITDES WS FICO       #
#                    SOLO INFONAVIT                                            #
################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"    --Archivo que almacena las variables globales del modulo
--GLOBALS "RETWEL01.inc"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid,     --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod  -- codigo de operacion

DEFINE g_reg_modulo   RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD

DEFINE g_estado_solicitud SMALLINT 
       
END GLOBALS

MAIN
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion SMALLINT                     -- forma como ejecutara el programa
       ,p_s_titulo       STRING                       -- titulo de la ventana
       ,v_folio          LIKE ret_preliquida.folio_liquida
    DEFINE  ar_sol_prel  DYNAMIC ARRAY OF RECORD 
       folio             LIKE ret_preliquida.folio_liquida
      ,total_reg         DECIMAL(9,0)
      ,faltantes_reg     DECIMAL(9,0)
      END RECORD 

DEFINE v_count_ret      SMALLINT 
DEFINE v_c              SMALLINT 
DEFINE v_s_bnd          SMALLINT 
DEFINE v_bandera        SMALLINT 
DEFINE v_bandera_hub    SMALLINT 
   

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   #nicializa las variables de control del modulo
   LET g_estado_solicitud = 60
   LET v_folio            = 0
      -- se asigna proceso y operacion
   LET g_proceso_cod      = g_proceso_cod_con_ret_solo_infonavit        
   LET g_opera_cod        = g_opera_cod_ret_soloInfo_salida_tesoreria  

      -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'ret'

    SELECT b.ruta_listados
      INTO seg_modulo_bat.ruta_listados
      FROM seg_modulo b
     WHERE b.modulo_cod = 'bat' 
     
   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETWC01.log")

     #objetos necesarios con el fin de consumir el servicio 
     #no es su lugar correcto mera referencia
     --IF num_args()==1 THEN
       --LET retirosoloinfonavit_retirosoloinfonavitPortTypeLocation = arg_val(1)
     --END IF
     --CALL fn_rechazo(op1, op2,op3, op4,op5, op6,op7, op8, op10) RETURNING wsstatus, r_nss , r_result, r_remaind, r_importe

     #En el boton de CLOSE 
     #LET TERMINATE=TRUE
   
   -- se abre la ventana que envia el proceso de archivo de salida
   OPEN WINDOW w_monitoreo_fico WITH FORM "RETWC010"

   DISPLAY ARRAY  ar_sol_prel TO f_ar_soli_prel.*
    BEFORE DISPLAY    
      DECLARE cu_sol CURSOR FOR  SELECT folio,0,NVL(COUNT(id_solicitud),0)
                                   FROM ret_solo_infonavit
                                  WHERE estado_solicitud = g_estado_solicitud
                                  GROUP BY folio

       LET v_count_ret = v_count_ret + 1
       
       FOREACH cu_sol INTO ar_sol_prel[v_count_ret].*  
          SELECT NVL(COUNT(id_solicitud),0),0
            INTO ar_sol_prel[v_count_ret].total_reg
            FROM ret_solo_infonavit
           WHERE folio = ar_sol_prel[v_count_ret].folio
             AND estado_solicitud < 100             
             
            IF ar_sol_prel[v_count_ret].faltantes_reg > 0 THEN
                LET v_count_ret = v_count_ret + 1
            END IF            
       END FOREACH
       
          LET v_count_ret = v_count_ret  - 1      
             
          IF v_count_ret = 1 THEN  
             DISPLAY "Se va a generar la monitoreo de solicitudes en fico de "|| v_count_ret ||" registro. Seleccione aceptar para continuar ." TO lb_mensage
          ELSE
             IF v_count_ret > 1 THEN  
                DISPLAY "Se va a generar la monitoreo de solicitudes en fico de "|| v_count_ret ||" registros. Seleccione aceptar para continuar ." TO lb_mensage
             ELSE 
                DISPLAY "No se va a generar la monitoreo de solicitudes en fico se encontraron "|| v_count_ret ||" registros. Seleccione cancelar para Salir ." TO lb_mensage
             END IF 
          END IF 
          
      ON ACTION ACCEPT
      LET v_bandera_hub = 0
         CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod) RETURNING v_s_bnd
         IF ( v_s_bnd = 0 ) THEN
            IF v_count_ret > 0 THEN
               FOR v_c = 1 TO v_count_ret
                 CALL fn_ret_monitoreo_fico(ar_sol_prel[v_c].folio,p_usuario_cod) RETURNING v_bandera                 
                 IF v_bandera = 0 THEN
                     LET v_bandera_hub = 1
                 END IF 
               END FOR  
            ELSE 
                 CALL fn_mensaje("Atención","No existen solicitudes para \nrealizar el monitoreo","information")  
            END IF
         ELSE
            CALL fn_mensaje("Atención","No se pudo iniciar el proceso existe consulta previa","information")
         END IF

         IF v_bandera_hub = 1 THEN 
             CALL fn_mensaje("Atención","Se ha enviado la consulta a fico .\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","information")
         END IF 
         EXIT DISPLAY
            
        
      ON ACTION CANCEL
         EXIT DISPLAY     
   END DISPLAY   
   
   CLOSE WINDOW w_monitoreo_fico

END MAIN

################################################################################
#Modulo: RET
#Nombre: fn_ret_monitoreo_fico
#Fecha creacion: Junio 29, 2012
#Autor: Erick Rodriguez, EFP
#Narrativa del proceso que realiza:
#Ejecuta consulta de solicitdes ws fico de Retiro solo infonavit
#para solicitudes en estado 70
#
#Registro de modificaciones:
#Autor           Fecha                   Descrip. cambio
#
################################################################################

FUNCTION fn_ret_monitoreo_fico(p_folio, p_usuario_cod)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- usuario que ejecuta el programa
       p_folio           LIKE glo_folio.folio, -- folio para preliquidar
       v_s_comando       STRING, -- cadena con una instruccion de consola
       v_bandera         SMALLINT, -- para revisar que los procesos ejecutaron bien
       v_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
DEFINE v_program         VARCHAR(50)
      ,v_s_cadena        STRING 

   -- se obtiene el nombre del archivo
   LET v_nombre_archivo = "NA"  
   LET v_program        = "RETWC01" 
   LET v_bandera        = 0
   LET g_pid            = 0

   -- se verifica si se puede continuar con la operacion
   --DISPLAY fn_valida_operacion(0,g_proceso_cod,g_opera_cod)
   --IF ( fn_valida_operacion(0,g_proceso_cod,g_opera_cod) = 0 ) THEN
   -- fn_valida_operacion(pid, proceso_cod, opera_cod)
   -- Si resultado = 0 
      CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING g_pid

         --LET v_s_cadena =   "\n SELECT max(gl.folio)                   ",
                            --"\n   FROM glo_folio gl                        ",
                            --"\n       ,ret_solo_infonavit fh               ",
                            --"\n  WHERE gl.proceso_cod        =  1501       ",
                            --"\n    AND gl.folio              =  fh.folio   ",
                            --"\n    AND gl.status             =  2          ",
                            --"\n    AND fh.estado_solicitud   = ",g_estado_solicitud
         --DISPLAY  v_s_cadena

         --PREPARE pre_folios FROM v_s_cadena
         --EXECUTE pre_folios INTO p_folio              
      
      --CALL fn_genera_folio(g_proceso_cod, g_opera_cod,p_usuario_cod) RETURNING p_folio

      CALL fn_inicializa_proceso(g_pid             ,
                                 g_proceso_cod     ,
                                 g_opera_cod       ,
                                 p_folio           ,
                                 v_program         ,
                                 v_nombre_archivo  ,
                                 p_usuario_cod)  RETURNING v_bandera
                                  
      -- el proceso se registro correctamente
    IF ( v_bandera = 0 ) THEN
      -- se invoca la ejecucion del programa lanzado. los parametros se envian 
      -- en orden segun lo acordado el dia 12/Enero/2012 con equipo EFP
      -- usuario, pid, proceso_cod, opera_cod, folio y archivo
      LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETP06 ",
                          p_usuario_cod CLIPPED, " ",
                          g_pid  , " " ,
                          g_proceso_cod , " " ,
                          g_opera_cod ," ",
                          p_folio ," ",
                          v_nombre_archivo CLIPPED," ",
                          " 1>",seg_modulo_bat.ruta_listados CLIPPED ,
                          "/nohup:",g_pid        USING "&&&&&",":",
                          g_proceso_cod USING "&&&&&",":",
                          g_opera_cod   USING "&&&&&" ,
                          " 2>&1 &"
                          
       DISPLAY v_s_comando
       RUN v_s_comando
       --CALL fn_mensaje("Atención","Se ha enviado la consulta a fico .\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","information")
     --ELSE
        --CALL fn_mensaje("Atención","No se pudo iniciar el proceso","information")
     END IF
  --ELSE
    --CALL fn_mensaje("Atención","No se pudo iniciar el proceso existe consulta previa","information")
  --END IF
 RETURN v_bandera
END FUNCTION