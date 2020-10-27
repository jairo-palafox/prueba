#################################################################################
#Modulo            => AGR                                                       #
#Programa          => AGRP40                                                    #
#Objetivo          =>  Autoriza Actualización de Marcas uso Anualidad y Garantía#
#Autor             => Jose Edaurdo Ventura                                      #
#Fecha inicio      => 27 JUNIO 2016                                             #
#################################################################################

-------------------------------------------------------------------------------
-- Actualizado  => Gerardo Alfonso Vega Paredes.                             --
-- Fec Mod.     => 14 de Noviembre de 2017.                                  --
-- Modificación => Agregar funcionalidad desmarca y reactivación de marcas   --
-- Clave cambio => saci2017-1-11                                             --
-- Observación  => Apoyo a requerimiento de Mauro Muñiz Caballero            --
-- Autor mod.   => Emilio Abarca Sánchez                                     --
-- Modificación => Se agrega la referencia de la desmarca/marca              --
-------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS
   DEFINE g_usuario                 CHAR (20)
   DEFINE p_tpo_ejecucion           SMALLINT
   DEFINE p_s_titulo                STRING        -- Título de la ventana
   DEFINE v_s_qry                   STRING
   DEFINE v_bandera                 SMALLINT
   DEFINE v_cnt                     INTEGER
   DEFINE v_pos                     INTEGER
   DEFINE j                         INTEGER
   DEFINE i                         INTEGER
   DEFINE v_id_derechohabiente      DECIMAL(9,0)
   DEFINE v_tpo_proceso             SMALLINT
   DEFINE g_proceso_cod             INTEGER
   DEFINE v_cod_rechazo             INTEGER
   DEFINE v_cadena                  STRING

   DEFINE arr_aut_marca DYNAMIC ARRAY OF RECORD
      nss                           CHAR(11),
      num_credito                   LIKE cre_acreditado.num_credito,
      estado                        LIKE cre_acreditado.estado,
      edo_procesar                  LIKE cre_acreditado.edo_procesar,
      tpo_solicitud                 CHAR(20),
      marca                         LIKE sfr_marca_activa.marca,
      f_ini_marca                   DATE,
      f_solicitud                   DATE,
      usuario                       CHAR(40),
      v_box1                        SMALLINT,
      v_box2                        SMALLINT
   END RECORD

   DEFINE arr_id DYNAMIC ARRAY OF RECORD
      id_derechohabiente        DECIMAL(9,0),
      id_cre_uso_garantia       DECIMAL(9,0)
   END RECORD

   DEFINE v_opcion CHAR(15)
END GLOBALS

MAIN

   LET g_usuario       = ARG_VAL (1)
   LET p_tpo_ejecucion = ARG_VAL (2)
   LET p_s_titulo      = ARG_VAL (3)

   -- Creación de log
   CALL STARTLOG(g_usuario CLIPPED|| ".AGRP40.log")

   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN
   
   OPEN WINDOW AGRP401 WITH FORM "AGRP401"

      INPUT ARRAY arr_aut_marca FROM tab_aut_marca.* ATTRIBUTE( WITHOUT DEFAULTS,
                                                                APPEND ROW = FALSE,
                                                                DELETE ROW = FALSE,
                                                                INSERT ROW = FALSE,
                                                                UNBUFFERED)

         BEFORE INPUT

         LET v_s_qry = "SELECT id_derechohabiente,
                               id_cre_uso_garantia,
                               nss,
                               num_credito,
                               estado,
                               edo_procesar,
                               tpo_actualizacion,
                               marca,
                               f_ini_marca,
                               f_proceso,
                               usuario
                          FROM agr_act_marca_uso"

         PREPARE prp_aut_marca FROM v_s_qry
         DECLARE cur_aut_marca CURSOR FOR prp_aut_marca 


         LET i = 1
         FOREACH cur_aut_marca INTO arr_id[i].id_derechohabiente,
                                    arr_id[i].id_cre_uso_garantia,
                                    arr_aut_marca[i].nss,
                                    arr_aut_marca[i].num_credito,
                                    arr_aut_marca[i].estado,
                                    arr_aut_marca[i].edo_procesar,
                                    arr_aut_marca[i].tpo_solicitud,
                                    arr_aut_marca[i].marca,
                                    arr_aut_marca[i].f_ini_marca,
                                    arr_aut_marca[i].f_solicitud,
                                    arr_aut_marca[i].usuario
            --DISPLAY "arreglo : ",arr_aut_marca[i].*
         LET arr_aut_marca[i].v_box1 = 0
         LET arr_aut_marca[i].v_box2 = 0
         IF arr_aut_marca[i].tpo_solicitud = 1 THEN
            LET arr_aut_marca[i].tpo_solicitud = "MARCA"
         END IF
         IF arr_aut_marca[i].tpo_solicitud = 2 THEN
            LET arr_aut_marca[i].tpo_solicitud = "DESMARCA"
         END IF
            LET i = i + 1
         END FOREACH

         CALL arr_aut_marca.deleteElement(arr_aut_marca.getLength())
         CALL arr_id.deleteElement(arr_id.getLength())

         IF arr_aut_marca.getLength() < 1 THEN
             CALL fn_mensaje("Error","No se encontraros registros pendientes de autorizar","stop")
             EXIT INPUT
         END IF

         BEFORE ROW

         LET v_pos = arr_curr()

         ON CHANGE v_box1

            -- Se valida que solo puedan ingresar una casilla
            IF arr_aut_marca[v_pos].v_box1 = 1 THEN
                  LET arr_aut_marca[v_pos].v_box2 = 0
            END IF

         ON CHANGE v_box2

            IF arr_aut_marca[v_pos].v_box2 = 1 THEN
                  LET arr_aut_marca[v_pos].v_box1 = 0
            END IF

         ON ACTION ACCEPT

         FOR i = 1 TO arr_aut_marca.getLength()

            IF arr_aut_marca[i].marca = 223 THEN
               LET g_proceso_cod = 308 --SOLICITUD DESMARCA CRÉDITOS AG
            ELSE
               LET g_proceso_cod = 1208
            END IF

            IF(arr_aut_marca[i].marca = 221) THEN
               LET g_proceso_cod = 220 
            END IF 
            
         DISPLAY arr_aut_marca[i].v_box1
         DISPLAY arr_aut_marca[i].tpo_solicitud

            IF (arr_aut_marca[i].v_box1 = 1) AND 
               (arr_aut_marca[i].tpo_solicitud = "MARCA")  THEN
               LET v_s_qry = "EXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,?,0,0,TODAY,?,?)"

               PREPARE prp_exe_marca_int FROM v_s_qry
               EXECUTE prp_exe_marca_int USING arr_id[i].id_derechohabiente,
                                               arr_aut_marca[i].marca,
                                               arr_id[i].id_cre_uso_garantia,
                                               "0",
                                               arr_aut_marca[i].estado,
                                               g_usuario,
                                               g_proceso_cod
                                       INTO v_cod_rechazo

               DISPLAY "La funcion de desmarca regresa : ", v_cod_rechazo

               IF v_cod_rechazo <> 0 THEN
                  IF v_cod_rechazo = "-239" THEN
                     LET v_cadena = "Ya ha sido aplicada una actualización para el registro, favor de verificar"
                  ELSE
                     LET v_cadena = "Ocurrió un error al marcar\n Id Derechohabiente :" || arr_id[i].id_derechohabiente ||
                                    "\n Marca : " || arr_aut_marca[i].marca
                  END IF
                  CALL fn_mensaje("Error",v_cadena,"stop")
                  DISPLAY v_cadena
               ELSE
                  #Actualiza tablas maestras para el envío de petición de saldos a Procesar
                 { CASE 
                     WHEN arr_aut_marca[i].marca = 221

                        UPDATE cre_acreditado
                           SET estado = 20,
                               edo_procesar = 70
                         WHERE id_cre_acreditado  = arr_id[i].id_cre_uso_garantia 
                           AND id_derechohabiente = arr_id[i].id_derechohabiente
                         
                     WHEN arr_aut_marca[i].marca = 223 OR 
                          arr_aut_marca[i].marca  = 225

                        UPDATE cre_uso_garantia
                           SET estado = 20,
                               edo_procesar = 70
                         WHERE id_cre_uso_garantia = arr_id[i].id_cre_uso_garantia
                           AND id_derechohabiente  = arr_id[i].id_derechohabiente
                           
                  END CASE}
                  
                  DELETE FROM agr_act_marca_uso
                  WHERE id_derechohabiente  = arr_id[i].id_derechohabiente
                    AND id_cre_uso_garantia = arr_id[i].id_cre_uso_garantia
                    
               END IF
            END IF

            IF (arr_aut_marca[i].tpo_solicitud = "DESMARCA") AND
               (arr_aut_marca[i].v_box1 = 1 ) THEN

               --saci2017-1-11
               OPEN WINDOW AGRP402 WITH FORM "AGRP402"      --saci2017-1-11
               DIALOG ATTRIBUTES (UNBUFFERED)               --saci2017-1-11
                  INPUT v_opcion FROM radiogroup1           --saci2017-1-11
                     BEFORE INPUT                           --saci2017-1-11
                        LET v_opcion = NULL
                  END INPUT                                 --saci2017-1-11

               ON ACTION ACCEPT                             --saci2017-1-11

                  IF v_opcion IS NULL THEN                  --saci2017-1-11
                     CALL fn_mensaje("Consulta","Debe ingresar al menos un criterio","about") --saci2017-1-11
                     NEXT FIELD radiogroup1                 --saci2017-1-11
                  END IF                                    --saci2017-1-11

                  IF v_opcion = 1 THEN                       --saci2017-1-11
                     CALL Ingresa_motivo_desmarca(arr_id[i].id_cre_uso_garantia,
                                                  arr_id[i].id_derechohabiente,
                                                  arr_aut_marca[i].marca,
                                                  arr_aut_marca[i].f_ini_marca,
                                                  3,
                                                  arr_aut_marca[i].nss) --saci2017-1-11
                  END IF                                                --saci2017-1-11

                  IF v_opcion = 2 THEN                                  --saci2017-1-11
                     CALL Ingresa_motivo_desmarca(arr_id[i].id_cre_uso_garantia,
                                                  arr_id[i].id_derechohabiente,
                                                  arr_aut_marca[i].marca,
                                                  arr_aut_marca[i].f_ini_marca,
                                                  4,
                                                  arr_aut_marca[i].nss) --saci2017-1-11
                  END IF                                                --saci2017-1-11

                  EXIT DIALOG
          ON ACTION regresar
               EXIT INPUT

               END DIALOG
               CLOSE WINDOW AGRP402

            -- Se extrae el tipo de proceso

               LET v_s_qry = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,0,0,?,?)"

               PREPARE prp_exe_desmarca_interna FROM v_s_qry
               EXECUTE prp_exe_desmarca_interna USING arr_id[i].id_derechohabiente    ,
                                                      arr_aut_marca[i].marca          ,
                                                      arr_id[i].id_cre_uso_garantia   ,
                                                      g_usuario                       ,
                                                      g_proceso_cod
                                                 INTO v_cod_rechazo

               DISPLAY "La funcion de desmarca regresa : ", v_cod_rechazo

               IF v_cod_rechazo <> 0 THEN
                  LET v_cadena = "Ocurrió un error al desmarcar\n Id Derechohabiente :" || arr_id[i].id_derechohabiente ||
                                 "\n Marca : " || arr_aut_marca[i].marca

                  CALL fn_mensaje("Error",v_cadena,"stop")
                  DISPLAY v_cadena
               ELSE

                  #Actualiza tablas maestras para evitar el envío de petición de saldos a Procesar
                  CASE 
                     WHEN arr_aut_marca[i].marca = 221

                        UPDATE cre_acreditado
                           SET estado = 170,
                               edo_procesar = 120
                         WHERE id_cre_acreditado  = arr_id[i].id_cre_uso_garantia 
                           AND id_derechohabiente = arr_id[i].id_derechohabiente
                         
                     WHEN arr_aut_marca[i].marca = 223 OR 
                          arr_aut_marca[i].marca  = 225

                        UPDATE cre_uso_garantia
                           SET estado = 170,
                               edo_procesar = 120
                         WHERE id_cre_uso_garantia = arr_id[i].id_cre_uso_garantia
                           AND id_derechohabiente  = arr_id[i].id_derechohabiente
                           
                  END CASE
                  
                   DELETE FROM agr_act_marca_uso
                   WHERE id_derechohabiente  = arr_id[i].id_derechohabiente
                     AND id_cre_uso_garantia = arr_id[i].id_cre_uso_garantia
               END IF            
            END IF

             IF(arr_aut_marca[i].v_box2 = 1 ) THEN
                DELETE FROM agr_act_marca_uso
                 WHERE id_derechohabiente  = arr_id[i].id_derechohabiente
                   AND id_cre_uso_garantia = arr_id[i].id_cre_uso_garantia
            END IF

         END FOR
            CALL fn_mensaje("Atención","Se realizo proceso de marca para solicitudes aprobadas","stop")
   {         DISPLAY " ENVIA CORREO DEL REPORTE"
            -- se asigna el titulo del correo
            LET v_s_titulo_correo = "Proceso: RECEPCIÓN RECURRENTE ANUALIDADES GARANTIZADAS"

            -- se asigna el archivo a adjuntar
            LET v_s_archivo_correo = g_c_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

            -- se asigna el cuerpo del correo
            LET v_s_mens_correo =  "ID Proceso   : ",g_d_pid,"\n",
                                   "Proceso      : RECEPCIÓN RECURRENTE AGR\n",
                                   "Operacion    : INTEGRA ARCHIVO RECURRENTE\n",
                                   "Fecha Inicio : ",TODAY,"\n",
                                   "Fecha Fin    : ",TODAY

      -- se invoca la función que envía por correo el elemento generado
      CALL fn_correo_proceso(g_d_pid,
                             g_i_proceso_cod,
                             g_i_opera_cod,
                             v_s_archivo_correo,
                             v_s_titulo_correo,
                             v_s_mens_correo)}
            EXIT INPUT

         ON ACTION CANCEL
            EXIT INPUT

      END INPUT
   CLOSE WINDOW AGRP401
END MAIN

--saci2017-1-11
FUNCTION Ingresa_motivo_desmarca(p_n_referencia,v_id_derecho,v_marca,p_f_ini_marca,v_motivo,v_nss)

   -- Ingresa motivo desmarca --
   DEFINE p_n_referencia DECIMAL(9,0)
   DEFINE p_f_ini_marca  DATE
   DEFINE v_id_derecho DECIMAL(9,0),
          v_marca      SMALLINT,
          v_motivo     SMALLINT,
          v_solic      SMALLINT,
          v_fecha      DATE,
          v_nss        CHAR(11)

   LET v_solic = 2;      --tipo solic 1=marca 2=desmarca
   LET v_fecha = TODAY;
  
   INSERT INTO cre_sol_marca_esp VALUES 
     (
      p_n_referencia,
      v_id_derecho, 
      v_marca,
      p_f_ini_marca,
      v_solic,
      v_motivo,
      v_fecha,
      g_usuario
      );

   -- Cerrar partida en conciliación --

   INSERT INTO safre_tmp:tmp_cre_confirmado VALUES 
      (v_nss,
       v_id_derecho,
       TODAY,
       "agr");
   
END FUNCTION