################################################################################
#Modulo       => CBD                                                           #
#Programa     => CBDS04                                                        #
#Objetivo     => Programa que genera los archivos de delantos GRT              #
#Fecha inicio => 15/07/2014                                                    #
################################################################################

DATABASE safre_viv


##Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                          -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                              -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                              -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                              -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)   -- nombre dle archivo

PRIVATE DEFINE p_f_corte                  DATE                                  -- Fecha con la que se realizarán las consultas en cuestion               

PRIVATE DEFINE v_proceso_desc             CHAR(40)
PRIVATE DEFINE v_extension                CHAR(10)
PRIVATE DEFINE v_opera_desc               CHAR(40)
PRIVATE DEFINE v_detalle_monitoreo        STRING
PRIVATE DEFINE v_layout                   SMALLINT 
PRIVATE DEFINE v_usuario_proceso          CHAR(20)
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            CHAR(40)
PRIVATE DEFINE v_ruta_envio               char(40)


#Variable para el manejo del archivo
PRIVATE DEFINE v_nom_archivo              STRING
PRIVATE DEFINE v_ruta_archivo             STRING
PRIVATE DEFINE v_archivo                  BASE.CHANNEL


TYPE detalle RECORD
      nss                                    CHAR(11),
      f_liquidacion                          DATE,
      modulo                                 CHAR(3),
      subcuenta                              CHAR(1),
      aivs                                   DECIMAL(22,6),
      periodo_pago                           char(6)
END RECORD

#Variables para la generacion del archivo
PRIVATE DEFINE v_folio                    DECIMAL(10)
PRIVATE DEFINE v_detalle                  DYNAMIC ARRAY OF detalle

MAIN 
   DEFINE r_resultado_opera            INTEGER
   DEFINE v_resultado_gen              INTEGER

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_nombre_archivo = ARG_VAL(5)
   LET p_f_corte        = ARG_VAL(6)

   WHENEVER ERROR CONTINUE

    CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario_proceso 

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'cbd'

   #Encabezado para el archivo de monitoreo
   DISPLAY "*******************************************************************"
   LET v_detalle_monitoreo = " PROCESO            : ",v_proceso_desc,"\n",
                             " OPERACIÓN          : ",v_opera_desc,"\n",
                             " FECHA              : ",TODAY USING 'dd-mm-yyyy',"\n",
                             " HORA               : ",TIME(CURRENT)," "
   DISPLAY v_detalle_monitoreo;
   DISPLAY "*******************************************************************"
   -- se solicita el numero de folio asociado a la operacion
   -- parametros: proceso, operacion, usuario
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
   RETURNING v_folio

   #Se actualiza el folio del proceso               
   UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid

   UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid

    #Se manda a generar el archivo de movimientos adelantados
   CALL fn_genera_archivo() RETURNING v_resultado_gen
   
   IF v_resultado_gen = 0 THEN
      # Finaliza la operacion de generacion del archivo
      CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
      RETURNING r_resultado_opera
        DISPLAY "*******************************************************************"
        DISPLAY "Proceso finalizado correctamente "
   LET v_detalle_monitoreo = " PROCESO            : ",v_proceso_desc,"\n",
                             " OPERACIÓN          : ",v_opera_desc,"\n",
                             " FECHA FIN          : ",TODAY USING 'dd-mm-yyyy',"\n",
                             " HORA FIN           : ",TIME(CURRENT)," "
   DISPLAY v_detalle_monitoreo;
   DISPLAY "*******************************************************************"
      IF(r_resultado_opera <> 0)THEN         
         # Actualiza a estado erróneo
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
      END IF 
   ELSE
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
   END IF
   
   WHENEVER ERROR STOP
    
END MAIN 

FUNCTION fn_genera_archivo ()--Función que genera los adelantos GRT  
    DEFINE v_fn_genera_adelanto_grt_neg      STRING 
    DEFINE v_query_get_tab_mov               STRING 
    DEFINE v_query_get_detalle               STRING 
    DEFINE v_cont                            SMALLINT 
    DEFINE v_rep_fn                          VARCHAR(150)
    DEFINE v_arr_nom_tab                     DYNAMIC ARRAY OF CHAR(60)
    DEFINE v_nombre_archivo                  STRING 
    DEFINE v_detalle_archivo                 STRING 
    DEFINE v_total_registros                 INTEGER 

    --Tabla temporal donde se insertarán los adelantos
    DROP TABLE IF EXISTS tmp_adelanto_grt_neg;
    CREATE TABLE tmp_adelanto_grt_neg
    (
      nss             CHAR(11) NOT NULL ,
      f_liquidacion   DATE NOT NULL ,
      modulo          char(3),
      subcuenta       SMALLINT, 
      monto_acciones  DECIMAL(22,6),
      periodo_pago    CHAR(6)
    );

    LET v_nombre_archivo=v_ruta_envio CLIPPED ,"/ARCH_MOV_ADELANTO_",p_f_corte USING "yyyymmdd",".agr"

    DISPLAY "Inicio de la creación del archivo de adelantos GRT ...\n" 
    LET v_cont=1
    LET v_query_get_tab_mov= "Select trim(tabla) from cat_tab_movimiento"
    PREPARE prp_tab_mov FROM v_query_get_tab_mov
    DECLARE cur_tab_mov CURSOR FOR  prp_tab_mov
    TRY 
            FOREACH cur_tab_mov INTO v_arr_nom_tab[v_cont]
                LET v_cont=v_cont+1
            END FOREACH 
            
            
            LET v_arr_nom_tab[v_arr_nom_tab.getLength()]="cta_movimiento"
            LET v_fn_genera_adelanto_grt_neg="EXECUTE FUNCTION fn_cbd_adelantos_crt_neg(?,?)"
            PREPARE exe_fn_genera_adelanto_ctr_neg FROM v_fn_genera_adelanto_grt_neg

            FOR v_cont=1 TO v_arr_nom_tab.getLength()--Se ejecuta la función con todas las tablas para movimientos disponibles
                EXECUTE exe_fn_genera_adelanto_ctr_neg USING p_f_corte, v_arr_nom_tab[v_cont] INTO v_rep_fn
                DISPLAY v_rep_fn,"\n"
            END FOR 
            --Se obtiene la información que arroja la ejecución de la la función y se guarda en un array
            LET v_cont=1
            LET v_query_get_detalle="select nss\n,",               
                                           "f_liquidacion\n,", 
                                           "modulo\n,",          
                                           "subcuenta\n,",       
                                           "monto_acciones,\n",
                                           "periodo_pago\n",
                                    "FROM  tmp_adelanto_grt_neg"
           
            PREPARE prp_get_detalle FROM v_query_get_detalle
            DECLARE cur_get_detalle CURSOR FOR prp_get_detalle
            FOREACH  cur_get_detalle INTO v_detalle[v_cont].nss,
                                          v_detalle[v_cont].f_liquidacion,
                                          v_detalle[v_cont].modulo,
                                          v_detalle[v_cont].subcuenta,
                                          v_detalle[v_cont].aivs,
                                          v_detalle[v_cont].periodo_pago
                LET v_cont=v_cont+1
            END FOREACH 

            call v_detalle.deleteElement(v_detalle.getLength())

            --Se inicia con la creación de archivo de texto plano
            IF v_cont > 1 THEN   
                LET v_archivo = base.Channel.create()
                CALL v_archivo.openFile(v_nombre_archivo,"w")

                FOR v_cont=1 TO v_detalle.getLength()
                    LET v_detalle_archivo=v_detalle[v_cont].nss,"|",v_detalle[v_cont].f_liquidacion USING "yyyymmdd","|",v_detalle[v_cont].modulo,"|",v_detalle[v_cont].subcuenta,"|",v_detalle[v_cont].aivs,"|",v_detalle[v_cont].periodo_pago
                     CALL v_Archivo.writeLine(v_detalle_archivo)
                END FOR 

                CALL v_archivo.close()
                
                DISPLAY "Se ha creado el archivo ", v_nombre_archivo,"\n"
                DISPLAY "Fin de la creación del archivo de edelantos GRT\n "
                SELECT COUNT (*) 
                    INTO v_total_registros
                    FROM tmp_adelanto_grt_neg
                DISPLAY "Número de registros encontrados: ",v_total_registros
            ELSE 
         
                DISPLAY "No se encontraron registros en la base de datos\n"
                DISPLAY "No se generó el archivo ",v_nombre_archivo,"\n"
               
            END IF 
            RETURN 0
     CATCH 
        RETURN 1 
        DISPLAY SQLCA.sqlerrm
     END TRY 
END FUNCTION 



