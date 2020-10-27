######################################################################
#Modulo        => AFI                                                #
#Programa      => AFIC14                                             #
#Descripción   => Consulta totales afiliados rojos                   #
#Autor         => Jose Eduardo Ventura Bonola                        #
#Fecha         => 21 abril  2017                                     #
#Autor Mod     => Emilio Abarca Sánchez,EFP                          #
#Fecha Mod     => 01 Junio 2017                                      #
######################################################################

DATABASE safre_viv

GLOBALS

   DEFINE p_usuario                CHAR(20)      -- Obtiene dato de usuario
   DEFINE p_tipo_ejecucion         SMALLINT      -- Forma como ejecutará el programa
   DEFINE p_s_titulo               STRING        -- Título de la ventana
   DEFINE v_tot_p                  INTEGER
   DEFINE v_tot_e                  INTEGER
   DEFINE v_total                  INTEGER
   DEFINE v_monto_ini              DECIMAL(16,0)
   DEFINE v_monto_fin              DECIMAL(16,0)
   DEFINE v_bandera                SMALLINT 
   DEFINE v_query                  STRING 
   DEFINE c                        INTEGER 
   DEFINE v_mensaje                STRING 
   DEFINE w                        ui.Window
   DEFINE f                        ui.Form
   DEFINE v_c_tot_rojos            INTEGER 
   DEFINE v_c_economico            INTEGER 
   DEFINE v_c_politico             INTEGER
   DEFINE v_respuesta              SMALLINT 

   DEFINE arr_nss_rojos DYNAMIC ARRAY OF RECORD
      nss         CHAR(11),
      nombre      CHAR(150),
      curp        CHAR(18),
      rfc         CHAR(13),
      categoria   CHAR(3),
      cat_desc    CHAR(60),
      saldo       DECIMAL(16,2),
      f_actualiza DATE  
   END RECORD 
   
END GLOBALS

MAIN
   -- se recupera la clave de usuario desde parámetro
   LET p_usuario          = ""
   LET p_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   CLOSE WINDOW SCREEN 
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".AFIC14.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- Total Afiliados rojos
   SELECT COUNT(*)
     INTO v_total
     FROM afi_nss_rojo
    WHERE categoria IN (1,2)
      AND estado_rojo = 1

   -- Afiliados rojos categoría Politico   
   SELECT COUNT(*)
     INTO v_tot_p
     FROM afi_nss_rojo
    WHERE categoria = 1
      AND estado_rojo = 1

   -- Afiliados rojos categoria Economico
   SELECT COUNT(*)
     INTO v_tot_e
     FROM afi_nss_rojo
    WHERE categoria = 2
      AND estado_rojo = 1
   
   OPEN WINDOW consulta WITH FORM "AFIC141"

      LET w = ui.Window.getCurrent()
      LET f = w.getForm()

      -- Oculta arreglo 
      CALL f.setElementHidden("arr_group3",1)
      
      LET v_bandera = 0 -- Indicador no se ha calculado el saldo

      DISPLAY BY NAME v_tot_p 
      DISPLAY BY NAME v_tot_e
      DISPLAY BY NAME v_total
      
      INPUT BY NAME v_monto_ini,v_monto_fin ATTRIBUTE(UNBUFFERED,WITHOUT DEFAULTS)
         
         ON ACTION ACCEPT
         DISPLAY v_monto_ini,v_monto_fin
            IF (v_monto_ini IS NULL) AND (v_monto_fin IS NULL) THEN
               CALL fn_mensaje("","Debe ingresar datos válidos","") 
               NEXT FIELD v_monto_ini
            END IF 
           
            IF (v_monto_ini IS NOT NULL) AND (v_monto_fin IS NULL) THEN
               CALL fn_mensaje("","Debe ingresar el monto final","")
               NEXT FIELD v_monto_fin
            END IF 
          
            IF (v_monto_ini IS NULL) AND (v_monto_fin IS NOT NULL) THEN
               CALL fn_mensaje("","Debe ingresar el monto inicial","")
               NEXT FIELD v_monto_ini
            END IF 
            
            IF(v_monto_ini > v_monto_fin) THEN
               CALL fn_mensaje("","El monto inicial no puede ser mayor al monto final","")
               NEXT FIELD v_monto_ini
            END IF 

            -- Consume la tabla temporal mediante rango de montos ingresados
            LET v_query = "SELECT nss,
                                   nombre,
                                   curp,
                                   rfc,
                                   categoria,
                                   categoria_desc,
                                   saldo,
                                   f_actualiza
                              FROM safre_tmp:tmp_sdo_nss_rojo
                             WHERE saldo >= ",v_monto_ini,
                             " AND saldo <= ",v_monto_fin,
                             "ORDER BY categoria"
        

            IF (v_bandera = 0) THEN
               -- carga tmp para consulta de saldo nss rojo 
               CALL fn_extrae_saldo() RETURNING v_bandera  -- "1" Carga realizada para consulta 
            END IF 
        
    
            IF(v_bandera = 1) THEN 

               -- Inicializa variables
               LET v_c_tot_rojos = 0       
               LET v_c_economico = 0  
               LET v_c_politico  = 0 

               -- Limpia arreglo
               CALL arr_nss_rojos.clear()
          
               PREPARE prp_saldo_rojos FROM v_query
               DECLARE crs_saldo_rojos CURSOR FOR prp_saldo_rojos
          
               LET c = 1
          
               FOREACH crs_saldo_rojos INTO arr_nss_rojos[c].nss,
                                            arr_nss_rojos[c].nombre,
                                            arr_nss_rojos[c].curp,
                                            arr_nss_rojos[c].rfc,
                                            arr_nss_rojos[c].categoria,
                                            arr_nss_rojos[c].cat_desc,
                                            arr_nss_rojos[c].saldo,
                                            arr_nss_rojos[c].f_actualiza

                  --Total categoría politico
                  IF(arr_nss_rojos[c].categoria = "1") THEN
                     LET v_c_politico = v_c_politico + 1
                  END IF

                  --Total categoría económico
                  IF(arr_nss_rojos[c].categoria = "2") THEN
                     LET v_c_economico = v_c_economico + 1
                  END IF  
          
                  LET c = c + 1
                  
               END FOREACH
          
               -- Elimina fila en blanco
               IF(arr_nss_rojos[arr_nss_rojos.getLength()].nss IS NULL) AND 
                 (arr_nss_rojos[arr_nss_rojos.getLength()].nombre IS NULL) THEN
                  CALL arr_nss_rojos.deleteElement(arr_nss_rojos.getLength())
               END IF 

               IF(c = 1) THEN
                  -- Oculta arreglo 
                  CALL f.setElementHidden("arr_group3",1)
                  CALL fn_mensaje("","No se encontraron registros con el rango de parámetros ingresados","")
                  NEXT FIELD v_monto_ini
               ELSE 
                  IF(c > 1) THEN
                     -- Muestra arreglo ya con información
                     CALL f.setElementHidden("arr_group3",0)
                    
                     -- Total registros de acuerdo al rengo de montos ingresados
                     LET v_c_tot_rojos = v_c_politico + v_c_economico

                     -- Muestra  información
                     DISPLAY ARRAY arr_nss_rojos TO record1.* ATTRIBUTE(ACCEPT = FALSE)

                        ON ACTION Archivo
                           LET v_respuesta = fn_ventana_confirma("","¿Desea generar el archivo de salida?","")

                           IF(v_respuesta = 0) THEN
                              CALL fn_mensaje("","Se ha cancelado la operación","") 
                           ELSE 
                              -- Ejecuta funcion que genera archivo de salida
                              CALL fn_archivo(v_c_tot_rojos,
                                               v_c_politico,
                                               v_c_economico,
                                               v_monto_ini,
                                               v_monto_fin)
                       
                           
                              LET v_mensaje = "Se ha generado el archivo de salida con nombre : NSS_ROJOS_",TODAY USING "yyyymmdd",".txt \n y se encuentra en la ruta /safreviv_int/afi/envio"
                       
                              CALL fn_mensaje("",v_mensaje,"")
                       
                           END IF 

                        ON ACTION CANCEL 
                            -- Oculta arreglo
                            CALL f.setElementHidden("arr_group3",1)

                            -- Limpia Arreglo
                            CALL arr_nss_rojos.clear()

                            -- Limpia parámetros de entrada
                            LET v_monto_ini = NULL 
                            LET v_monto_fin  = NULL 

                            EXIT DISPLAY 
                            
                     END DISPLAY
                     
                   END IF 
               END IF 
               
            END IF 
          
          ON ACTION CANCEL 
             EXIT INPUT 

      END INPUT 

   CLOSE WINDOW consulta

END MAIN

FUNCTION fn_archivo(p_total_rojos,p_t_politicos,p_t_economicos,p_monto_ini,p_monto_fin)

   DEFINE p_total_rojos     INTEGER 
   DEFINE p_t_politicos     INTEGER 
   DEFINE p_t_economicos    INTEGER 
   DEFINE p_monto_ini       DECIMAL(12)
   DEFINE p_monto_fin       DECIMAL(12)
   DEFINE v_query_arh       STRING 
   DEFINE v_ruta_envio      CHAR(40)
   DEFINE v_arh_salida      STRING 
   DEFINE v_detalle         STRING 
   DEFINE v_rango_montos    STRING 
   DEFINE ch                base.Channel
   DEFINE cont              INTEGER 

   DEFINE r_arh_rojos  RECORD
      nss         CHAR(11),
      nombre      CHAR(150),
      cat_desc    CHAR(60),
      f_actualiza DATE ,
      saldo       CHAR(18)
   END RECORD 

   --Se obtiene la ruta donde se dejará el archivo generado
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'afi'

   -- crea nombre del archivo de salida
   LET v_arh_salida = v_ruta_envio CLIPPED,"/NSS_ROJOS_",TODAY USING "yyyymmdd",".txt"
   LET ch = base.Channel.create() #Se crea el objeto base.channel
   LET v_detalle = NULL

 
   LET v_query_arh =  "SELECT nss,
                               nombre,
                               categoria_desc,
                               f_actualiza,
                               saldo
                          FROM safre_tmp:tmp_sdo_nss_rojo
                         WHERE saldo >= ",p_monto_ini,
                         " AND saldo <= ",p_monto_fin, 
                         " ORDER BY categoria;"

   LET v_rango_montos = "c) Monto inicial : ",p_monto_ini USING "###,###,##&.&&","\n",
                         "   Monto final   : ",p_monto_fin USING "###,###,##&.&&","\n"
   

   CALL ch.openFile(v_arh_salida,"w")

   LET v_detalle = "a) Total afiliados rojos : ",p_total_rojos USING "###,###,##&","\n"

   CALL ch.writeLine(v_detalle)

   LET v_detalle = "b) Total categoría politico  : ",p_t_politicos  USING "###,###,##&","\n",
                   "   Total categoria económico  : ",p_t_economicos USING "###,###,##&","\n"

   CALL ch.writeLine(v_detalle)

   -- Escribe el inciso c)
   CALL ch.writeLine(v_rango_montos)

   PREPARE prp_arh_rojos FROM v_query_arh
   DECLARE crs_arh_rojos CURSOR FOR prp_arh_rojos

   LET cont = 1

   FOREACH crs_arh_rojos INTO r_arh_rojos.nss,
                              r_arh_rojos.nombre,
                              r_arh_rojos.cat_desc,
                              r_arh_rojos.f_actualiza,
                              r_arh_rojos.saldo

      LET v_detalle = r_arh_rojos.nss,"|",
                      r_arh_rojos.nombre CLIPPED,"|",
                      r_arh_rojos.cat_desc CLIPPED,"|",
                      r_arh_rojos.f_actualiza USING "yyyymmdd","|",
                      r_arh_rojos.saldo CLIPPED,"|"

      CALL ch.writeLine(v_detalle) 

      LET cont = cont + 1
      
   END FOREACH 

   IF(cont = 1) THEN
      LET v_detalle = "No se encontraron registros con el rango de montos ingresados"
      CALL ch.writeLine(v_detalle) 
   END IF 

   CALL ch.close()
   
END FUNCTION

FUNCTION fn_crea_temporal()

DATABASE safre_tmp

   WHENEVER ERROR CONTINUE 
      DROP TABLE tmp_sdo_nss_rojo

   WHENEVER ERROR STOP 
      CREATE TABLE tmp_sdo_nss_rojo(id_derechohabiente DECIMAL(9,0),
                                    nss               CHAR(11),
                                    nombre            CHAR(150),
                                    curp              CHAR(18),
                                    rfc               CHAR(13),
                                    categoria         CHAR(3),
                                    categoria_desc    CHAR(60),
                                    saldo             DECIMAL(16,2),
                                    f_actualiza       DATE )
DATABASE safre_viv
      
END FUNCTION 

FUNCTION fn_extrae_saldo()

   #NOTA
      #Subcuenta = 4 -->SSV 97
      #Subcuenta = 8 -->SSV 92
      
   DEFINE v_qry_global    STRING
   DEFINE v_precio_fondo  DECIMAL(19,14)
   DEFINE v_ind_carga     BOOLEAN 

   DEFINE r_inf_rojos RECORD
     id_derechohabiente DECIMAL(9,0),
     nss                CHAR(11),
     nombre             CHAR(150),
     curp               CHAR(18),
     rfc                CHAR(13),
     categoria          CHAR(3),
     categoria_desc     CHAR(60),
     saldo              DECIMAL(16,2),
     f_actualiza        DATE 
   END RECORD 

   -- Obtiene valor de fondo al día.
   SELECT precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE f_valuacion = TODAY 
      AND fondo       = 11;

   LET v_ind_carga = 0 --No se ha iniciado la carga
   
   --- crea temporal
   CALL fn_crea_temporal()
   
   LET v_qry_global = "SELECT rj.id_derechohabiente,
                              \n rj.nss,
                              \n trim(af.ap_paterno_af)||' '||trim(af.ap_materno_af)||' '||trim(af.nombre_af),
                              \n af.curp,
                              \n af.rfc,
                              \n rj.categoria,
                              \n cr.categoria_desc,
                       \n ROUND (SUM (mv.monto_acciones) * ", v_precio_fondo,",2),
                             \n  rj.f_actualiza
                         \n FROM afi_nss_rojo rj,
                             \n  cat_categoria_roja cr,
                             \n  afi_derechohabiente af,
                        \n OUTER cta_movimiento mv
                        \n WHERE rj.id_derechohabiente = af.id_derechohabiente
                          \n AND rj.id_derechohabiente = mv.id_derechohabiente
                          \n AND mv.subcuenta IN (4,8)
                          \n AND rj.estado_rojo = 1
                          \n AND rj.categoria IN ('1','2')
                          \n AND rj.categoria = cr.categoria
                          \n AND mv.fondo_inversion = 11
                          \n GROUP BY 1,2,3,4,5,6,7,9;" 
    
   PREPARE prp_nss_rojos FROM v_qry_global
   DECLARE crs_nss_rojos CURSOR FOR prp_nss_rojos

   FOREACH crs_nss_rojos INTO r_inf_rojos.id_derechohabiente,
                              r_inf_rojos.nss,
                              r_inf_rojos.nombre,
                              r_inf_rojos.curp,
                              r_inf_rojos.rfc,
                              r_inf_rojos.categoria,
                              r_inf_rojos.categoria_desc,
                              r_inf_rojos.saldo,
                              r_inf_rojos.f_actualiza

      IF(r_inf_rojos.saldo IS NULL) THEN 
         LET r_inf_rojos.saldo = 0.00 
      END IF 
      
      -- Carga temporal para su posterior consulta
      INSERT INTO safre_tmp:tmp_sdo_nss_rojo
         VALUES (r_inf_rojos.id_derechohabiente,
                 r_inf_rojos.nss,
                 r_inf_rojos.nombre,
                 r_inf_rojos.curp,
                 r_inf_rojos.rfc,
                 r_inf_rojos.categoria,
                 r_inf_rojos.categoria_desc,
                 r_inf_rojos.saldo,
                 r_inf_rojos.f_actualiza)

   END FOREACH 

   LET v_ind_carga = 1 -- Ha fializado la carga

   RETURN v_ind_carga

END FUNCTION 