################################################################################
#Modulo              => OCG                                                    #
#Programa            => OCGC01                                                 #
#Objetivo            => Programa que realiza la consulta general               #
#                       de ubicación de registros Garantía 43 bis              #
#Autor               => Héctor F. Jiménez Lara                                 #
#Fecha inicio        => 23 Octubre 2015                                        #
################################################################################

DATABASE safre_viv

   DEFINE p_bnd_sp                  CHAR(3)
   
   DEFINE p_usuario_cod             LIKE seg_usuario.usuario_cod
   DEFINE v_cnt                     INTEGER
   DEFINE v_s_qry                   STRING
   
   DEFINE v_estado_desc             CHAR(40)
   DEFINE v_genero                  CHAR(1)
   
   DEFINE v_cve_nss_asociado        CHAR(11)
   DEFINE v_clave_nss               CHAR(1)
   DEFINE v_tpo_credito_combo       CHAR(2)
   DEFINE v_producto                CHAR(1)
   DEFINE v_edo_transaccion         SMALLINT
   DEFINE v_f_formaliza_ins         DATE
   DEFINE v_edo_uso                 CHAR(20)
   DEFINE v_situacion               CHAR(20)
   DEFINE v_id_formaliza            DECIMAL(9,0)
   DEFINE v_formaliza               DECIMAL(9,0)
   DEFINE v_id_dere                 DECIMAL(9,0)
   
   DEFINE v_cuenta                  INTEGER
   DEFINE v_f_proceso               CHAR(8)
   DEFINE v_f_liq_cofi              DATE
   DEFINE v_ax_situacion            SMALLINT
   DEFINE ch                        base.Channel
   DEFINE v_nom_arh                 STRING
   DEFINE v_ruta_envio              LIKE seg_modulo.ruta_envio 
   DEFINE v_cadena_sit              STRING
   DEFINE v_tab_forma               STRING 

   DEFINE v_arr_inconsistencia      DYNAMIC ARRAY OF RECORD
      inconsistencia                CHAR(2),
      descripcion                   CHAR(80)
   END RECORD

   DEFINE v_desc_nss_1              CHAR(15)
   DEFINE v_desc_nss_2              CHAR(15)
   DEFINE v_nss_unificacion         CHAR(11)
   DEFINE v_cnt_unificador          SMALLINT
   DEFINE v_cnt_unificado           SMALLINT
   DEFINE v_s_qry_dor               STRING
   DEFINE v_s_qry_do                STRING

MAIN

   DEFINE p_tipo_ejecucion          SMALLINT -- forma como ejecutara el programa
   DEFINE p_s_titulo                STRING   -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   LET p_bnd_sp         = ARG_VAL(4)

   --DISPLAY "BANDERA SP : ",p_bnd_sp

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".OCGC01.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se invoca la función de consulta
   CALL fn_cons_ubicacion_registros_nss(p_bnd_sp)
END MAIN



FUNCTION fn_cons_ubicacion_registros_nss(v_bnd_sp)
   DEFINE v_bnd_correcto            BOOLEAN
   
   DEFINE v_bnd_sp                  CHAR(3)
   DEFINE v_subproceso              SMALLINT
   DEFINE v_nss                     CHAR(11)
   DEFINE v_f_proceso               DATE
   DEFINE v_f_carga                 DATE
   DEFINE v_cve_ent_financiera      SMALLINT  
   DEFINE v_situacion               SMALLINT
   DEFINE v_f_formaliza             DATE
   DEFINE v_forma                   ui.Form
   DEFINE v_window                  ui.Window
   DEFINE cb                        ui.ComboBox

   DEFINE arr_tpo_credito DYNAMIC ARRAY OF RECORD
      tpo_credito_cod     CHAR(2),
      tpo_credito_desc    CHAR(40)
   END RECORD

   OPEN WINDOW w1 WITH FORM "OCGC011"

      -- Sí la ejecución proviene de un SP se oculta el campo subproceso
      IF v_bnd_sp IS NOT NULL THEN 
         LET v_window = ui.Window.getCurrent()
         LET v_forma = v_window.getForm()

         CALL v_forma.setFieldHidden("v_subproceso",1)
         CALL v_forma.setElementHidden("lb_subproceso",1)
      END IF

      LET cb = ui.ComboBox.forName("v_tpo_credito_combo")

      INPUT BY NAME v_nss,
                    v_subproceso,
                    v_cve_ent_financiera,
                    v_f_proceso,
                    v_f_carga,
                    v_situacion,
                    --v_tpo_credito,
                    v_tpo_credito_combo,
                    v_f_formaliza ATTRIBUTES (UNBUFFERED)

      BEFORE INPUT
 

   LET arr_tpo_credito[1].tpo_credito_cod   = "A"
   LET arr_tpo_credito[1].tpo_credito_desc  = "A  - APOYO INFONAVIT"
   CALL cb.addItem(arr_tpo_credito[1].tpo_credito_cod, arr_tpo_credito[1].tpo_credito_desc)
   
   LET arr_tpo_credito[2].tpo_credito_cod   = "C"
   LET arr_tpo_credito[2].tpo_credito_desc  = "C  - CONVENIDO"
   CALL cb.addItem(arr_tpo_credito[2].tpo_credito_cod, arr_tpo_credito[2].tpo_credito_desc)

   LET arr_tpo_credito[3].tpo_credito_cod   = "7"
   LET arr_tpo_credito[3].tpo_credito_desc  = "7  - COFINANCIADO TIPO 7"
   CALL cb.addItem(arr_tpo_credito[3].tpo_credito_cod, arr_tpo_credito[3].tpo_credito_desc)

   LET arr_tpo_credito[4].tpo_credito_cod   = "8"
   LET arr_tpo_credito[4].tpo_credito_desc  = "8  - COFINANCIADO TIPO 8"
   CALL cb.addItem(arr_tpo_credito[4].tpo_credito_cod, arr_tpo_credito[4].tpo_credito_desc)

   LET arr_tpo_credito[5].tpo_credito_cod   = "AI"
   LET arr_tpo_credito[5].tpo_credito_desc  = "AI - APOYOS"
   CALL cb.addItem(arr_tpo_credito[5].tpo_credito_cod, arr_tpo_credito[5].tpo_credito_desc)

   LET arr_tpo_credito[6].tpo_credito_cod   = "CO"
   LET arr_tpo_credito[6].tpo_credito_desc  = "CO - COFINANCIADOS"
   CALL cb.addItem(arr_tpo_credito[6].tpo_credito_cod, arr_tpo_credito[6].tpo_credito_desc)
 
    

         ON ACTION ACCEPT
            --DISPLAY v_nss,v_subproceso,v_cve_ent_financiera,v_f_proceso,v_f_carga,v_situacion,v_tpo_credito_combo

            IF v_nss IS NOT NULL THEN
               CALL fn_valida_nss(v_nss) RETURNING v_bnd_correcto
               IF v_bnd_correcto = TRUE THEN 
                  CALL fn_cons_ubicacion_registros(v_nss,
                                                   v_subproceso,
                                                   v_cve_ent_financiera,
                                                   v_f_proceso,
                                                   v_f_carga,
                                                   v_situacion,
                                                   v_tpo_credito_combo,
                                                   v_bnd_sp,
                                                   v_f_formaliza)
               END IF
            ELSE
               CALL fn_cons_ubicacion_registros(v_nss,
                                                v_subproceso,
                                                v_cve_ent_financiera,
                                                v_f_proceso,
                                                v_f_carga,
                                                v_situacion,
                                                v_tpo_credito_combo,
                                                v_bnd_sp,
                                                v_f_formaliza)
            END IF
      END INPUT

   CLOSE WINDOW w1
END FUNCTION



FUNCTION fn_cons_ubicacion_registros( p_nss,
                                      p_subproceso,
                                      p_cve_ent_financiera,
                                      p_f_proceso,
                                      p_f_carga,
                                      p_situacion,
                                      p_tpo_credito,
                                      p_bnd_sp,
                                      p_f_formalizacion )

   DEFINE p_tpo_credito             CHAR(9)
   DEFINE p_subproceso              CHAR(3)
   DEFINE p_bnd_sp                  CHAR(3)
   DEFINE p_nss                     CHAR(11)
   DEFINE v_ax_fecha                CHAR(15)
   DEFINE p_f_proceso               DATE
   DEFINE p_f_carga                 DATE
   DEFINE p_cve_ent_financiera      SMALLINT  
   DEFINE p_situacion               SMALLINT 
   DEFINE v_ax_cnt                  SMALLINT 
   DEFINE v_bnd_nulos               SMALLINT
   DEFINE v_pos                     SMALLINT
  
   DEFINE v_s_condicion             STRING
   DEFINE v_s_condicion_aux         STRING 
   DEFINE v_cadena_aux              STRING
   DEFINE p_f_formalizacion         DATE
   DEFINE v_tabla                   STRING
   
   DEFINE v_cadena_situacion        STRING
   DEFINE v_cadena_credito          STRING
   DEFINE v_cadena_diagnostico      STRING
   DEFINE v_tabla_formalizacion     STRING
   DEFINE v_busca_id_ocg_fz         STRING
   DEFINE v_tpo_producto_aux        STRING
   DEFINE v_s_condicion_sp4         STRING 
   DEFINE a                         INTEGER
   DEFINE v_detalle                 STRING
   DEFINE v_mensaje                 STRING

   DEFINE v_d_sit                   STRING
   DEFINE v_d_sub                   STRING
   DEFINE v_d_cve_ent               STRING
   DEFINE v_d_diag                  STRING
   
   DEFINE v_arr_consulta DYNAMIC ARRAY OF RECORD
      id_detalle                    DECIMAL(9,0),
      id_derechohabiente            DECIMAL(9,0),
      id                            DECIMAL(9,0),
      situacion                     SMALLINT, -- Anteriormente estado
      subproceso                    SMALLINT, --CHAR(3),
      tpo_credito                   CHAR(3),
      cve_ent_financiera            SMALLINT,
      diagnostico                   SMALLINT, --CHAR(2),
      f_proceso                     DATE,
      nss                           CHAR(11),
      rfc                           CHAR(13),
      nombre                        VARCHAR(80)
   END RECORD
   DEFINE v_arr_cons_tabla DYNAMIC ARRAY OF RECORD
      nom_tabla                     LIKE cat_tabla_subproceso.nom_tabla,
      subproceso                    LIKE cat_tabla_subproceso.subproceso,
      alias                         LIKE cat_tabla_subproceso.alias,
      identificador                 LIKE cat_tabla_subproceso.identificador
   END RECORD

   LET v_cnt        = 1
   LET v_ax_cnt     = 1
   LET v_cadena_aux = ""
   LET v_bnd_nulos  = 0

   CALL v_arr_consulta.clear()

   LET v_tabla = " "

   LET v_s_qry = " SELECT nom_tabla,             \n
                          subproceso,            \n
                          alias,                 \n
                          identificador          \n
                     FROM cat_tabla_subproceso   \n
                    WHERE 1=1                    \n "

   CASE p_bnd_sp 
      WHEN 1 
         LET v_s_qry = v_s_qry || ' AND subproceso = "001" \n'
         LET p_subproceso = "001"
      WHEN 2 
         LET v_s_qry = v_s_qry || ' AND subproceso = "002" \n'
         LET p_subproceso = "002"
      WHEN 3 
         LET v_s_qry = v_s_qry || ' AND subproceso = "003" \n'
         LET p_subproceso = "003"
      WHEN 4
         LET v_s_qry = v_s_qry || ' AND subproceso = "004" \n'
         LET p_subproceso = "004"
      WHEN 5
         LET v_s_qry = v_s_qry || ' AND subproceso = "005" \n'
         LET p_subproceso = "005"
   END CASE  

   --DISPLAY "SUBPROCESO CASE : ",p_subproceso

   --DISPLAY "QUERY DE TABLA : \n",v_s_qry
   PREPARE prp_cons_tabla FROM v_s_qry
   DECLARE cur_tabla CURSOR FOR prp_cons_tabla

   LET v_cadena_sit       = " "
   LET v_tab_forma        = " "

   IF p_nss IS NOT NULL THEN
      LET v_bnd_nulos = 1 
      LET v_cadena_aux = "\n AND gd.nss = '" || p_nss||"'"
      LET v_s_condicion = v_s_condicion , v_cadena_aux
   END IF

   IF p_subproceso IS NOT NULL THEN
      LET v_bnd_nulos = 1 
      LET v_cadena_aux =  "\n AND subproceso = " || p_subproceso 
      LET v_s_condicion =  v_s_condicion, v_cadena_aux
   END IF 

   IF p_cve_ent_financiera IS NOT NULL THEN
      LET v_bnd_nulos = 1 
     -- DISPLAY "alias: ", v_arr_cons_tabla[v_ax_cnt].alias
      LET v_cadena_aux = "\n AND gd.cve_ent_financiera = "   , p_cve_ent_financiera  
      LET v_s_condicion = v_s_condicion , v_cadena_aux
   END IF 

   IF p_f_proceso IS NOT NULL THEN
      LET v_ax_fecha = p_f_proceso
      LET v_ax_fecha = "'", v_ax_fecha CLIPPED , "'"
      LET v_bnd_nulos = 1 
      LET v_cadena_aux = "\n AND gd.f_proceso = " || v_ax_fecha
      LET v_s_condicion = v_s_condicion , v_cadena_aux
   END IF

   IF p_f_carga IS NOT NULL THEN
      LET v_ax_fecha = p_f_carga
      LET v_ax_fecha = "'", v_ax_fecha CLIPPED , "'"
      LET v_bnd_nulos = 1 
      LET v_cadena_aux = "\n AND gd.f_proceso = " || v_ax_fecha
      LET v_s_condicion = v_s_condicion , v_cadena_aux
   END IF  

   --DISPLAY "p_tpo_credito",p_tpo_credito

   IF p_tpo_credito IS NOT NULL THEN

      IF (p_tpo_credito = "A") OR
         (p_tpo_credito = "C") OR
         (p_tpo_credito = "7") OR
         (p_tpo_credito = "8") THEN

         LET v_bnd_nulos = 1 
         --LET v_cadena_aux = "\n AND tpo_credito = '" || p_tpo_credito || "'"
         LET v_s_condicion = v_s_condicion , "\n AND tpo_credito = '" || p_tpo_credito || "'"

         LET v_tpo_producto_aux = " = ",'"',p_tpo_credito CLIPPED,'"' -- Cadena para bùsqueda de producto por SP004
      ELSE
         IF p_tpo_credito = "AI" THEN
            LET p_tpo_credito = "('A','C')"
            LET v_tpo_producto_aux = "IN ('A','C')" CLIPPED 
         END IF 

         IF p_tpo_credito = "CO" THEN
            LET p_tpo_credito = "(7,8)"
            LET v_tpo_producto_aux = "IN (7,8)" CLIPPED 
         END IF

         LET v_bnd_nulos = 1 
         --LET v_cadena_aux = "\n AND tpo_credito in '" || p_tpo_credito || "'"

            LET v_s_condicion = v_s_condicion , "\n AND tpo_credito in " || p_tpo_credito --v_cadena_aux
      END IF
   END IF

   IF (p_situacion IS NOT NULL) THEN 
      LET v_bnd_nulos = 1 
      LET v_cadena_aux = "\n AND situacion = " || p_situacion
      --LET v_cadena_aux = "\n AND estado = " || p_situacion
      LET v_s_condicion = v_s_condicion , v_cadena_aux
   END IF

   IF (p_f_formalizacion IS NOT NULL) THEN
      IF p_situacion IS NOT NULL THEN
         LET v_bnd_nulos = 1 
         LET v_cadena_aux = "\n AND f.situacion = " || p_situacion
         --LET v_cadena_aux = "\n AND estado = " || p_situacion
         LET v_s_condicion = v_s_condicion , v_cadena_aux
      END IF
      LET v_tabla = " , ocg_formalizacion f \n" CLIPPED
      LET v_ax_fecha = p_f_formalizacion
      LET v_ax_fecha = "'", v_ax_fecha CLIPPED , "'"
      LET v_bnd_nulos = 1 
      LET v_cadena_aux = "\n AND f.f_registro_carta = " || v_ax_fecha
      LET v_s_condicion = v_s_condicion , v_cadena_aux
   END IF

--******************************************************************************
   --DISPLAY "CONDICIÓN : \n", v_s_condicion
   LET v_s_condicion_aux = v_s_condicion

   -- todos son nulos 
   IF v_bnd_nulos = 0 THEN
      -- Si no ingresan paámetros de búsuqeda se notifica con un mensaje
      CALL fn_mensaje("Alterta", "Debe ingresar al menos un criterio de búsqueda","stop")
   ELSE 
      LET v_cuenta = 0
      FOREACH cur_tabla INTO v_arr_cons_tabla[v_ax_cnt].*

         LET v_s_condicion = v_s_condicion_aux
         
         LET v_busca_id_ocg_fz     = "\n"
         LET v_tabla_formalizacion = "\n"
         LET v_s_condicion_sp4     = "\n"
    
         IF v_arr_cons_tabla[v_ax_cnt].subproceso = 004 THEN
         
            LET v_s_condicion = NULL 
            LET v_s_condicion_sp4 = "\n AND gp.concepto NOT IN (108,118,128,308,318,328,407,417,408,418,428,508,608,808,818,828)"

            IF (p_subproceso IS NOT NULL) THEN
               LET v_cadena_situacion   =  "'', \n"
               LET v_cadena_credito     =  "'', \n"
               LET v_cadena_diagnostico =  "'', \n"

               LET v_s_condicion = "\n AND subproceso = " || p_subproceso
            END IF 
            
            IF(p_nss IS NOT NULL) THEN 
               LET v_cadena_situacion   =  "'', \n"
               LET v_cadena_credito     =  "'', \n"
               LET v_cadena_diagnostico =  "'', \n"

               LET v_s_condicion = v_s_condicion,"\n AND gd.nss = '"|| p_nss ||"'"
            END IF 
            
            IF(p_cve_ent_financiera IS NOT NULL) THEN
               
               LET v_cadena_situacion   =  "'', \n"
               LET v_cadena_credito     =  "'', \n"
               LET v_cadena_diagnostico =  "'', \n"
               
               LET v_s_condicion = v_s_condicion,"\n AND gd.cve_ent_financiera = ", p_cve_ent_financiera
            END IF

            IF (p_f_proceso IS NOT NULL) THEN

               LET v_cadena_situacion   =  "'', \n"
               LET v_cadena_credito     =  "'', \n"
               LET v_cadena_diagnostico =  "'', \n"
               
               LET v_s_condicion =  v_s_condicion,"\n AND gd.f_proceso = " || '"',p_f_proceso CLIPPED,'"'
            END IF

            IF(p_f_carga IS NOT NULL) THEN 

               LET v_cadena_situacion   = "'',\n"
               LET v_cadena_credito     = "'',\n"
               LET v_cadena_diagnostico = "'',\n"

               LET v_s_condicion = v_s_condicion," AND gd.f_proceso = "||'"',p_f_carga CLIPPED,'"'
               
            END IF 

            IF (p_f_formalizacion IS NOT NULL) THEN

               LET v_cadena_situacion   =  "'', \n"
               LET v_cadena_credito     =  "'', \n"
               LET v_cadena_diagnostico =  "'', \n"
               
               LET v_busca_id_ocg_fz     = " \n AND f.id_ocg_formalizacion = gp.id_ocg_formalizacion "
               LET v_s_condicion = v_s_condicion, "\n AND f.f_registro_carta = " ,'"',p_f_formalizacion,'"'
               
            END IF

            IF (p_situacion IS NOT NULL)  THEN

               LET v_cadena_situacion   =  v_arr_cons_tabla[v_ax_cnt].alias CLIPPED || ".estado, \n"
               LET v_cadena_credito     =  "'', \n"
               LET v_cadena_diagnostico =  "'', \n"

               LET v_s_condicion = v_s_condicion, "\n AND gp.estado = " || p_situacion

            END IF

            IF (p_tpo_credito IS NOT NULL) THEN
               
               LET v_cadena_situacion   =  "'', \n"
               LET v_cadena_credito     =  "gf.tpo_credito, \n"
               LET v_cadena_diagnostico =  "'', \n"

               LET v_tabla_formalizacion = " ocg_formalizacion gf ,\n"
               LET v_busca_id_ocg_fz     = " \n AND gf.id_ocg_formalizacion = gp.id_ocg_formalizacion "
               
               LET v_s_condicion = v_s_condicion," \n AND gf.tpo_credito " || v_tpo_producto_aux
            END IF           
         ELSE
            LET v_cadena_situacion   =  v_arr_cons_tabla[v_ax_cnt].alias CLIPPED || ".situacion, \n"
            LET v_cadena_credito     =  v_arr_cons_tabla[v_ax_cnt].alias CLIPPED || ".tpo_credito, \n"
            LET v_cadena_diagnostico =  v_arr_cons_tabla[v_ax_cnt].alias CLIPPED || ".diagnostico, \n"

            IF v_arr_cons_tabla[v_ax_cnt].subproceso = 005 THEN
               LET v_cadena_sit       = " AND form.id_ocg_formalizacion = ",v_arr_cons_tabla[v_ax_cnt].alias CLIPPED || ".id_ocg_formalizacion \n"
               LET v_cadena_credito = v_arr_cons_tabla[v_ax_cnt].alias CLIPPED||".tpo_credito, "
               LET v_tab_forma        = " ,OUTER ocg_formalizacion form"
         
               LET v_s_condicion = NULL 
               IF p_nss IS NOT NULL THEN
                  LET v_bnd_nulos = 1 
                  LET v_cadena_aux = "\n AND gd.nss = '"|| p_nss||"'"
                  LET v_s_condicion = v_s_condicion , v_cadena_aux
               END IF

               IF p_subproceso IS NOT NULL THEN
                  LET v_bnd_nulos = 1 
                  LET v_cadena_aux =  "\n AND subproceso = " || p_subproceso 
                  LET v_s_condicion =  v_s_condicion, v_cadena_aux
               END IF 

               IF p_cve_ent_financiera IS NOT NULL THEN
                  LET v_bnd_nulos = 1 
                  -- DISPLAY "alias: ", v_arr_cons_tabla[v_ax_cnt].alias
                  LET v_cadena_aux = "\n AND gd.cve_ent_financiera = "   , p_cve_ent_financiera  
                  LET v_s_condicion = v_s_condicion , v_cadena_aux
               END IF 

               IF p_f_proceso IS NOT NULL THEN
                  LET v_ax_fecha = p_f_proceso
                  LET v_ax_fecha = "'", v_ax_fecha CLIPPED , "'"
                  LET v_bnd_nulos = 1 
                  LET v_cadena_aux = "\n AND gd.f_proceso = " || v_ax_fecha
                  LET v_s_condicion = v_s_condicion , v_cadena_aux
               END IF

               IF p_f_carga IS NOT NULL THEN
                  LET v_ax_fecha = p_f_carga
                  LET v_ax_fecha = "'", v_ax_fecha CLIPPED , "'"
                  LET v_bnd_nulos = 1 
                  LET v_cadena_aux = "\n AND gd.f_proceso = " || v_ax_fecha
                  LET v_s_condicion = v_s_condicion , v_cadena_aux
               END IF  

   --DISPLAY "p_tpo_credito",p_tpo_credito

               IF p_tpo_credito IS NOT NULL THEN

               IF (p_tpo_credito = "A") OR
                  (p_tpo_credito = "C") OR
                  (p_tpo_credito = "7") OR
                  (p_tpo_credito = "8") THEN

                  LET v_bnd_nulos = 1 
                   --LET v_cadena_aux = "\n AND tpo_credito = '" || p_tpo_credito || "'"
                  LET v_s_condicion = v_s_condicion , "\n AND form.tpo_credito = '" || p_tpo_credito || "'"

                  LET v_tpo_producto_aux = " = ",'"',p_tpo_credito CLIPPED,'"' -- Cadena para bùsqueda de producto por SP004
               ELSE
                  IF p_tpo_credito = "AI" THEN
                  LET p_tpo_credito = "('A','C')"
                  LET v_tpo_producto_aux = "IN ('A','C')" CLIPPED 
               END IF 

               IF p_tpo_credito = "CO" THEN
                  LET p_tpo_credito = "(7,8)"
                  LET v_tpo_producto_aux = "IN (7,8)" CLIPPED 
               END IF

                  LET v_bnd_nulos = 1 
                --LET v_cadena_aux = "\n AND tpo_credito in '" || p_tpo_credito || "'"

                  LET v_s_condicion = v_s_condicion , "\n AND form.tpo_credito in " || p_tpo_credito --v_cadena_aux
         END IF
         END IF

         IF (p_situacion IS NOT NULL) THEN 
            LET v_bnd_nulos = 1 
            LET v_cadena_aux = "\n AND gl.situacion = " || p_situacion
            --LET v_cadena_aux = "\n AND estado = " || p_situacion
            LET v_s_condicion = v_s_condicion , v_cadena_aux
         END IF

         IF (p_f_formalizacion IS NOT NULL) THEN
            IF p_situacion IS NOT NULL THEN
               LET v_bnd_nulos = 1 
               LET v_cadena_aux = "\n AND f.situacion = " || p_situacion
             --LET v_cadena_aux = "\n AND estado = " || p_situacion
               LET v_s_condicion = v_s_condicion , v_cadena_aux
            END IF
            LET v_tabla = " , ocg_formalizacion f \n" CLIPPED
            LET v_ax_fecha = p_f_formalizacion
            LET v_ax_fecha = "'", v_ax_fecha CLIPPED , "'"
            LET v_bnd_nulos = 1 
            LET v_cadena_aux = "\n AND f.f_registro_carta = " || v_ax_fecha
            LET v_s_condicion = v_s_condicion , v_cadena_aux
         END IF
      END IF

         END IF

         LET v_s_qry = "SELECT " || "gd.id_ocg_detalle,\n"
                                 || "gd.id_derechohabiente,\n"
                                 || v_arr_cons_tabla[v_ax_cnt].alias CLIPPED || "." || v_arr_cons_tabla[v_ax_cnt].identificador || ",\n"
                                 || v_cadena_situacion
                                 || "gd.subproceso, \n" 
                                 || v_cadena_credito
                                 || "gd.cve_ent_financiera, \n"
                                 || v_cadena_diagnostico
                                 || "gd.f_proceso, \n"
                                 || "gd.nss, \n"
                                 || "'', \n"        
                                 || "''\n" ||
                         "FROM " || "ocg_detalle gd, \n"
                                 || v_arr_cons_tabla[v_ax_cnt].nom_tabla CLIPPED || " " || v_arr_cons_tabla[v_ax_cnt].alias || ", \n"
                                 , v_tabla_formalizacion CLIPPED 
                                 || " ocg_ctr_archivo gar \n " || v_tabla ||v_tab_forma||
                        " WHERE "|| v_arr_cons_tabla[v_ax_cnt].alias CLIPPED || ".id_ocg_detalle = gd.id_ocg_detalle \n " 
                                 || v_busca_id_ocg_fz ,"\n"||   
                          "AND " || v_arr_cons_tabla[v_ax_cnt].alias CLIPPED || ".id_derechohabiente = gd.id_derechohabiente \n" ||
                          " AND      gar.id_ocg_ctr_archivo = gd.id_ocg_ctr_archivo \n" ||
                          " AND "|| "gd.subproceso = "||v_arr_cons_tabla[v_ax_cnt].subproceso
                                 || v_s_condicion
                                 || v_s_condicion_sp4||v_cadena_sit||
                          "\n ORDER BY gd.id_ocg_detalle desc,gd. subproceso asc \n"
                        --"\nORDER BY gd.cve_ent_financiera \n"

         
         DISPLAY "Query detalle global : ",v_s_qry
         
         PREPARE prp_consulta FROM v_s_qry
         DECLARE cur_consulta CURSOR FOR prp_consulta

         FOREACH cur_consulta INTO v_arr_consulta[v_cnt].*
           
         --LET v_cuenta = v_cuenta +1

            IF v_arr_consulta[v_cnt].subproceso = 1 THEN 
               -- Se consulta el nombre 
               LET v_s_qry = " SELECT rfc,
                                      TRIM(ap_paterno)||' '|| TRIM(NVL(ap_materno,' ')) || ' ' || TRIM(nombre)\n
                                 FROM ocg_tramite           \n
                                WHERE id_ocg_tramite = (    \n
                                      SELECT id_ocg_tramite \n 
                                        FROM " || v_arr_cons_tabla[v_ax_cnt].nom_tabla || "\n" ||
                                     " WHERE " || v_arr_cons_tabla[v_ax_cnt].identificador || " = " || v_arr_consulta[v_cnt].id || ")\n"

               --DISPLAY "CONSULTA DE RFC y NOMBRE : \n",v_s_qry

               PREPARE prp_cons_ax_edo FROM v_s_qry
               EXECUTE prp_cons_ax_edo INTO v_arr_consulta[v_cnt].rfc,
                                            v_arr_consulta[v_cnt].nombre
            END IF 

            IF v_arr_consulta[v_cnt].subproceso = 2 THEN

               LET v_s_qry = " SELECT rfc,
                                      TRIM(ap_paterno)||' '|| TRIM(NVL(ap_materno,' ')) || ' ' || TRIM(nombre)\n
                                 FROM ocg_formalizacion           \n
                                WHERE id_ocg_formalizacion = ?    \n"

               --DISPLAY "CONSULTA DE RFC y NOMBRE : \n",v_s_qry

               PREPARE prp_cons_ax_dp FROM v_s_qry
               EXECUTE prp_cons_ax_dp INTO v_arr_consulta[v_cnt].rfc,
                                           v_arr_consulta[v_cnt].nombre
                                     USING v_arr_consulta[v_cnt].id

            END IF

            IF v_arr_consulta[v_cnt].subproceso = 3 THEN

               SELECT id_ocg_formalizacion
                 INTO v_id_formaliza
                 FROM ocg_solicitud_uso_garantia
                WHERE id_ocg_solicitud_ug = v_arr_consulta[v_cnt].id

               LET v_s_qry = " SELECT rfc,
                                      TRIM(ap_paterno)||' '|| TRIM(NVL(ap_materno,' ')) || ' ' || TRIM(nombre)\n
                                 FROM ocg_formalizacion           \n
                                WHERE id_ocg_formalizacion = ?    \n"

               --DISPLAY "CONSULTA DE RFC y NOMBRE : \n",v_s_qry

               PREPARE prp_cons_uso FROM v_s_qry
               EXECUTE prp_cons_uso INTO v_arr_consulta[v_cnt].rfc,
                                           v_arr_consulta[v_cnt].nombre
                                     USING v_id_formaliza

            END IF

            IF v_arr_consulta[v_cnt].subproceso = 4 THEN

               LET v_s_qry = " SELECT f.rfc,
                                      f.tpo_credito,
                                      t.estado,
                                      TRIM(ap_paterno)||' '|| TRIM(NVL(ap_materno,' ')) || ' ' || TRIM(nombre)\n
                                 FROM ocg_formalizacion f,ocg_ctr_transaccion t           \n
                                WHERE t.id_ocg_formalizacion = f.id_ocg_formalizacion
                                  AND t.id_ocg_ctr_transaccion = ?    \n"

               --DISPLAY "CONSULTA DE RFC y NOMBRE : \n",v_s_qry
               --DISPLAY "id consulta : ",v_arr_consulta[v_cnt].id
               PREPARE prp_cons_ax FROM v_s_qry
               EXECUTE prp_cons_ax INTO v_arr_consulta[v_cnt].rfc,
                                        v_arr_consulta[v_cnt].tpo_credito,   --v_producto,
                                        v_arr_consulta[v_cnt].situacion,
                                        v_arr_consulta[v_cnt].nombre
                                  USING v_arr_consulta[v_cnt].id
               
               LET v_arr_consulta[v_cnt].diagnostico = "00"
               LET v_ax_situacion = v_arr_consulta[v_cnt].situacion
            END IF

            IF v_arr_consulta[v_cnt].subproceso = 5 THEN

               SELECT id_ocg_formalizacion,id_derechohabiente
                 INTO v_formaliza, v_id_dere
                 FROM ocg_liquidacion 
                WHERE id_ocg_liquidacion = v_arr_consulta[v_cnt].id

               IF (v_formaliza IS NULL) OR
                  (v_formaliza = "" )   OR
                  (v_formaliza = " ") THEN

                  SELECT rfc,
                         TRIM(ap_paterno_af)||' '|| TRIM(NVL(ap_materno_af,' ')) || ' ' || TRIM(nombre_af)
                    INTO v_arr_consulta[v_cnt].rfc,
                         v_arr_consulta[v_cnt].nombre
                    FROM afi_derechohabiente
                   WHERE id_derechohabiente = v_id_dere

               ELSE

               LET v_s_qry = " SELECT f.rfc,
                                      TRIM(ap_paterno)||' '|| TRIM(NVL(ap_materno,' ')) || ' ' || TRIM(nombre)\n
                                 FROM ocg_formalizacion f,ocg_liquidacion l           \n
                                WHERE f.id_ocg_formalizacion = l.id_ocg_formalizacion
                                  AND l.id_ocg_liquidacion = ?    \n"

              -- DISPLAY "CONSULTA DE RFC y NOMBRE : \n",v_s_qry
               
               PREPARE prp_cons_axl FROM v_s_qry
               EXECUTE prp_cons_axl INTO v_arr_consulta[v_cnt].rfc,
                                           v_arr_consulta[v_cnt].nombre
                                     USING v_arr_consulta[v_cnt].id

               END IF
            END IF 

            LET v_f_proceso = v_arr_consulta[v_cnt].f_proceso USING "mmddyyyy"

            LET v_cnt = v_cnt + 1
         END FOREACH
         CLOSE cur_consulta
         FREE cur_consulta
         CALL v_arr_consulta.deleteElement(v_arr_consulta.getLength())

         LET v_ax_cnt = v_ax_cnt + 1
      END FOREACH
      CLOSE cur_tabla
      FREE cur_tabla
 

      IF v_arr_consulta.getLength() <= 0 THEN
         CALL fn_mensaje("Alerta","No se encontraron registros","stop")
      ELSE


         LET v_cve_nss_asociado = NULL
         LET v_clave_nss        = NULL

         SELECT ruta_envio
           INTO v_ruta_envio
           FROM seg_modulo
          WHERE modulo_cod = 'ocg'

         LET v_nom_arh = v_ruta_envio CLIPPED ,"/consulta_detalles",".det"

         OPEN WINDOW w_consulta WITH FORM "OCGC012"
            --DISPLAY BY NAME v_cuenta
            DISPLAY ARRAY v_arr_consulta TO r_consulta.*

               ON ACTION archivo

                  LET ch = base.Channel.create()
                  CALL ch.openFile(v_nom_arh,"w" )
                  CALL ch.setDelimiter("|")

          FOR a = 1 TO v_arr_consulta.getLength()

            LET v_d_sit = v_arr_consulta[a].situacion
            LET v_d_sub = v_arr_consulta[a].subproceso
            LET v_d_cve_ent = v_arr_consulta[a].cve_ent_financiera
            LET v_d_diag = v_arr_consulta[a].diagnostico

            LET v_detalle = v_d_sit.trim()                                ,"|",
                            v_d_sub.trim()                                ,"|",
                            v_arr_consulta[a].tpo_credito          CLIPPED,"|",
                            v_d_cve_ent.trim()                            ,"|",
                            v_d_diag.trim()                               ,"|",
                            v_arr_consulta[a].f_proceso            USING"yyyymmdd","|",
                            v_arr_consulta[a].nss                  CLIPPED,"|",
                            v_arr_consulta[a].rfc                  CLIPPED,"|",
                            v_arr_consulta[a].nombre               CLIPPED

            CALL ch.writeLine([v_detalle])

         END FOR

         CALL ch.close()

         LET v_mensaje = "Archivo generado de forma correcta en : ",v_nom_arh
         CALL fn_mensaje ("Archivo",v_mensaje,"information")

               ON ACTION ACCEPT
                  LET v_pos = ARR_CURR()
                  --DISPLAY "CASE : ",v_arr_consulta[v_pos].subproceso
                  LET v_cve_nss_asociado = NULL
                  LET v_clave_nss        = NULL
                  CALL fn_consulta_detalle(v_arr_consulta[v_pos].*)
            END DISPLAY
         CLOSE WINDOW w_consulta
         LET v_tpo_credito_combo = NULL
      END IF
   END IF

END FUNCTION

FUNCTION fn_consulta_detalle(v_rec_detalle)
   DEFINE v_rec_detalle         RECORD
      id_detalle                    DECIMAL(9,0),
      id_derechohabiente            DECIMAL(9,0),   
      id                            DECIMAL(9,0),
      situacion                     SMALLINT,
      subproceso                    SMALLINT, --CHAR(3),
      tpo_credito                   CHAR(3),
      cve_ent_financiera            SMALLINT,      
      diagnostico                   SMALLINT, --CHAR(2),
      f_proceso                     DATE,
      nss                           CHAR(11),
      rfc                           CHAR(13),
      nombre                        VARCHAR(80)
   END RECORD
   
   DEFINE v_desc_ent_financiera     CHAR(60)
   
   DEFINE v_curp                    CHAR(18)
   DEFINE v_control_ef              CHAR(18)
   DEFINE v_ap_paterno              CHAR(40)
   DEFINE v_ap_materno              CHAR(40)
   DEFINE v_nombre                  CHAR(40)
   DEFINE v_f_envio                 DATE 
   
   DEFINE v_sdo_97                  DECIMAL(12,2)
   DEFINE v_id_ocg_tramite          DECIMAL(9,0)
   DEFINE v_f_subcta                DATE
   DEFINE v_carga                   DATE   -- consultar 
   DEFINE v_respuesta               DATE   -- consultar 
   DEFINE v_f_vigencia              DATE 
   DEFINE v_estado                  SMALLINT
   DEFINE v_ax_id_dh                DECIMAL(9,0)
   
   -- variables SP002
   DEFINE v_num_escritura          DECIMAL(8,0)
   DEFINE v_notario                DECIMAL(4,0)
   DEFINE v_ent_fed_notario        SMALLINT
   DEFINE v_mcpio_notario          SMALLINT
   DEFINE v_num_rpp                CHAR(15)
   DEFINE v_folio_real             DECIMAL(8,0)
   DEFINE v_partida                DECIMAL(6,0)
   DEFINE v_foja                   DECIMAL(8,0)
   DEFINE v_volumen                DECIMAL(6,0)
   DEFINE v_libro                  DECIMAL(6,0)
   DEFINE v_tomo                   DECIMAL(6,0)
   DEFINE v_seccion                DECIMAL(6,0)
   DEFINE v_ent_fed_inmueble       SMALLINT
   DEFINE v_domicilio_inmueble     CHAR(30)
   DEFINE v_valor_avaluo           DECIMAL(15,2)
   DEFINE v_valor_avaluo1          CHAR(17)
   DEFINE v_monto_credito          DECIMAL(15,2)
   DEFINE v_plazo_credito          DECIMAL(5,0)
   DEFINE v_tpo_moneda             SMALLINT
   DEFINE v_tasa_base              CHAR(20)
   DEFINE v_margen                 CHAR(20)
   DEFINE v_f_otorga_ent_fin       DATE
   DEFINE v_f_reg_carta            DATE
   DEFINE v_usuario_carta          CHAR(20)
   
   -- Variables SP3 
   DEFINE v_importe_solicitado     DECIMAL(12,2)
   DEFINE v_f_vencimiento          DATE
   DEFINE v_importe_utilizado      DECIMAL(12,2)
   
   DEFINE v_ax_f_otorga_ent_fin    SMALLINT 
   DEFINE v_ax_f_reg_carta         DATE
   DEFINE v_ax_f_vencimiento       SMALLINT
   
   DEFINE v_ax_f_envio             SMALLINT
   DEFINE v_ax_f_subcta            SMALLINT
   DEFINE v_ax_carga,v_ax_respuesta,v_ax_vigencia SMALLINT 
   DEFINE v_f_liberacion           DATE
   DEFINE v_imp_devuelto           DECIMAL(13,2)
   DEFINE v_causa_liquida          SMALLINT
   DEFINE v_f_deposito             DATE
   DEFINE v_f_liquida_cofi         DATE
   DEFINE v_f_liquida              DATE
   
   DEFINE v_ent_financiera         CHAR(50)
   DEFINE v_periodo_pago           CHAR(6)
   DEFINE v_f_proceso              DATE
   DEFINE v_f_transaccion          DATE
   DEFINE v_f_pago                 DATE
   DEFINE v_concepto               SMALLINT
   DEFINE v_viv_97                 DECIMAL(12,2)
   DEFINE v_bim_ap                 CHAR(8)
   DEFINE v_imp_ap_subsec          DECIMAL(13,2)
   DEFINE v_cnt_mig                INTEGER
   DEFINE v_cadena                  STRING

   CALL fn_llena_inconsistencias(v_rec_detalle.id_derechohabiente,v_rec_detalle.id,v_rec_detalle.subproceso)
     -- DISPLAY "inconsistencias ",v_rec_detalle.id_derechohabiente,v_rec_detalle.id,v_rec_detalle.subproceso

   OPEN WINDOW w_det_1 WITH FORM "OCGC013"

      IF v_rec_detalle.cve_ent_financiera IS NOT NULL THEN
         LET v_cadena = " AND cve_ent_financiera = ",v_rec_detalle.cve_ent_financiera
      ELSE
         LET v_cadena = " "
      END IF

      IF v_rec_detalle.tpo_credito = "7" OR
         v_rec_detalle.tpo_credito = "8" THEN
         LET v_s_qry = "SELECT first 1 nss_conyuge,
                               marca_conyuge,
                               f_liquidacion
                          FROM ocg_liquidacion_cofi
                         WHERE nss = ","'",v_rec_detalle.nss,"'",
                        " AND f_liquidacion  < '",v_f_proceso,"'",v_cadena,
                           --" AND cve_ent_financiera = ",v_arr_consulta[v_cnt].cve_ent_financiera,
                           " order by f_proceso desc "

           --DISPLAY "qry conyuge : ",v_s_qry
         PREPARE prp_cons_conyuge FROM v_s_qry
         EXECUTE prp_cons_conyuge INTO v_cve_nss_asociado,
                                       v_clave_nss,
                                       v_f_liq_cofi

         IF v_cve_nss_asociado IS NULL AND
            v_clave_nss IS NULL THEN

            SELECT count(*)
              INTO v_cnt_mig
              FROM tmp_tramite_migra
             WHERE nss = v_rec_detalle.nss

            IF (v_cnt_mig > 0) AND
                  (v_rec_detalle.subproceso = 1 )THEN

                 SELECT FIRST 1 nss_conyuge,
                        cve_nss_asociado
                   INTO v_cve_nss_asociado,
                        v_clave_nss
                   FROM tmp_tramite_migra
                  WHERE nss = v_rec_detalle.nss
                 --AND tpo_credtio = v_arr_consulta[v_cnt].tpo_credito

                --DISPLAY v_clave_nss
                --DISPLAY v_cve_nss_asociado
            END IF
         END IF
      END IF

      CASE v_rec_detalle.subproceso
         WHEN 1         
            LET v_s_qry = "SELECT ap_paterno,         \n
                                  ap_materno,         \n
                                  nombre,             \n
                                  curp,               \n
                                  tpo_credito,        \n
                                  viv97,              \n
                                  f_saldo,            \n
                                  f_vigencia,         \n
                                  id_ocg_tramite,     \n
                                  estado,             \n
                                  id_derechohabiente, \n
                                  f_respuesta,        \n
                                  situacion          \n
                             FROM ocg_tramite         \n
                            WHERE id_ocg_tramite = ? "
   
           -- DISPLAY "Qry 1 a ejecutar : \n" ,v_s_qry
   
            PREPARE prp_cons_dat_tram FROM v_s_qry
            EXECUTE prp_cons_dat_tram USING v_rec_detalle.id
                                       INTO v_ap_paterno,
                                            v_ap_materno,
                                            v_nombre,
                                            v_curp,
                                            v_producto,
                                            v_sdo_97,
                                            v_f_subcta,
                                            v_f_vigencia,
                                            v_id_ocg_tramite,
                                            v_estado,
                                            v_ax_id_dh,
                                            v_respuesta,
                                            v_ax_situacion

            LET v_s_qry = "SELECT tm.f_envio  ,                                   \n
                                  tm.f_carga  ,                                   \n
                                  tm.f_liquida_cofi,                              \n
                                  tm.f_respuesta                                  \n
                             FROM ocg_tramite g,                                  \n
                                  ocg_fecha_mig tm                                \n
                            WHERE tm.id_derechohabiente = g.id_derechohabiente    \n
                              AND tm.id_ocg_detalle     = g.id_ocg_detalle        \n
                              AND tm.id_ocg_referencia  = g.id_ocg_tramite        \n
                              AND subproceso = 1                                  \n
                              AND g.id_ocg_tramite      = ? "

            --DISPLAY "Qry 2 a ejecutar \n: " ,v_s_qry

            PREPARE prp_cons_dat_migra FROM v_s_qry
            EXECUTE prp_cons_dat_migra USING v_rec_detalle.id
                                        INTO v_f_envio,
                                             v_carga,
                                             v_f_liquida_cofi,
                                             v_respuesta

            LET v_s_qry = "SELECT LPAD(c.cve_ent_financiera,3,'0') || ' - ' || c.ent_financiera_desc \n
                             FROM cat_entidad_financiera c,                         \n
                                  ocg_tramite g                                 \n
                            WHERE g.cve_ent_financiera  = c.cve_ent_financiera  \n
                              AND g.id_ocg_tramite = ? "
   
            --DISPLAY "Qry 3 a ejecutar : \n" ,v_s_qry
   
            PREPARE prp_cons_dat_ef FROM v_s_qry
            EXECUTE prp_cons_dat_ef USING v_rec_detalle.id
                                     INTO v_desc_ent_financiera
   
         WHEN 2

            LET v_s_qry = "SELECT fm.f_envio  ,                                   \n
                                  fm.f_carga  ,                                   \n
                                  fm.f_liquida_cofi,                              \n
                                  fm.f_respuesta
                             FROM ocg_formalizacion f,                            \n
                                  ocg_fecha_mig fm                                \n
                            WHERE fm.id_derechohabiente = f.id_derechohabiente    \n
                              AND fm.id_ocg_detalle     = f.id_ocg_detalle        \n
                              AND fm.id_ocg_referencia  = f.id_ocg_formalizacion  \n
                              AND subproceso = 2                                  \n
                              AND f.id_ocg_formalizacion = ? "
   
            --DISPLAY "Qry 2 a ejecutar \n: " ,v_s_qry
            --DISPLAY " id ",v_rec_detalle.id
   
            PREPARE prp_cons_form_migra  FROM v_s_qry
            EXECUTE prp_cons_form_migra USING v_rec_detalle.id
                                         INTO v_f_envio,
                                              v_carga,
                                              v_f_liquida_cofi,
                                              v_respuesta

            IF (v_f_liquida_cofi IS NULL) OR
               (v_f_liquida_cofi = "12/31/1899") THEN

               LET v_s_qry = "SELECT tm.f_liquida_cofi
                                FROM ocg_formalizacion f,                            \n
                                     ocg_fecha_mig tm                                \n
                               WHERE tm.id_derechohabiente = f.id_derechohabiente    \n
                                 AND tm.id_ocg_referencia  = f.id_ocg_tramite        \n
                                 AND tm.subproceso =  1                              \n
                                 AND f.id_ocg_formalizacion= ? "

               PREPARE prp_cons_migra FROM v_s_qry
               EXECUTE prp_cons_migra USING v_rec_detalle.id
                                       INTO v_f_liquida_cofi
            END IF

            LET v_s_qry = "SELECT LPAD(c.cve_ent_financiera,3,'0') || ' - ' || c.ent_financiera_desc \n
                             FROM ocg_formalizacion g,                         \n
                                  cat_entidad_financiera c                           \n
                            WHERE g.id_ocg_formalizacion = ?  \n
                              AND g.cve_ent_financiera  = c.cve_ent_financiera "
   
       --     DISPLAY "Qry 3 a ejecutar : \n" ,v_s_qry
   
            PREPARE prp_cons_dat_ef_f FROM v_s_qry
            EXECUTE prp_cons_dat_ef_f USING v_rec_detalle.id
                                       INTO v_desc_ent_financiera

            LET v_s_qry = "SELECT f.ap_paterno,
                                  f.ap_materno,
                                  f.nombre,
                                  f.curp,
                                  f.tpo_credito,
                                  f.num_escritura ,
                                  f.notario,
                                  f.ent_fed_notario,
                                  f.mcpio_notario ,
                                  f.num_rpp ,
                                  f.folio_real,
                                  f.partida ,
                                  f.foja ,
                                  f.volumen ,
                                  f.libro ,
                                  f.tomo ,
                                  f.seccion ,
                                  f.ent_fed_inmueble ,
                                  f.domicilio_inmueble ,
                                  f.valor_avaluo ,
                                  f.monto_credito ,
                                  f.plazo_credito ,
                                  f.tpo_moneda ,
                                  f.tasa_base ,
                                  f.margen ,
                                  f.f_otorga_ent_fin,
                                  f.f_registro_carta,
                                  f.usuario_reg_carta,
                                  f.estado           ,
                                  f.f_vigencia, 
                                  f.situacion,
                                  f.f_respuesta,
                                  f.f_saldo,
                                  f.viv97,
                                  f.genero,
                                  f.num_ctr_int_ef
                             FROM ocg_formalizacion f
                            WHERE f.id_ocg_formalizacion = ?"

           -- DISPLAY "id detalle ",v_rec_detalle.id
         --   DISPLAY "qry sp002",v_s_qry

            PREPARE prp_cons_formalizacion FROM v_s_qry
            EXECUTE prp_cons_formalizacion USING v_rec_detalle.id
                                            INTO v_ap_paterno        ,
                                                 v_ap_materno        ,
                                                 v_nombre            ,
                                                 v_curp              ,
                                                 v_producto          ,
                                                 v_num_escritura     ,
                                                 v_notario           ,
                                                 v_ent_fed_notario   ,
                                                 v_mcpio_notario     ,
                                                 v_num_rpp           ,
                                                 v_folio_real        ,
                                                 v_partida           ,
                                                 v_foja              ,
                                                 v_volumen           ,
                                                 v_libro             ,
                                                 v_tomo              ,
                                                 v_seccion           ,
                                                 v_ent_fed_inmueble  ,
                                                 v_domicilio_inmueble,
                                                 v_valor_avaluo      ,
                                                 v_monto_credito     ,
                                                 v_plazo_credito     ,
                                                 v_tpo_moneda        ,
                                                 v_tasa_base         ,
                                                 v_margen            ,
                                                 v_f_otorga_ent_fin  ,
                                                 v_f_reg_carta       ,
                                                 v_usuario_carta     ,
                                                 v_estado            ,
                                                 v_f_vigencia        ,
                                                 v_ax_situacion      ,
                                                 v_respuesta,
                                                 v_f_subcta,
                                                 v_sdo_97,
                                                 v_genero,
                                                 v_control_ef
        --DISPLAY "Num ef :",v_control_ef

         --DISPLAY "Qry 4 a ejecutar datos completos : \n" ,v_s_qry
        -- DISPLAY "id",v_rec_detalle.id
         WHEN 3              --antes ga.f_lote

            LET v_s_qry = "SELECT tm.f_envio  ,                                   \n
                                  tm.f_carga  ,                                   \n
                                  tm.f_liquida_cofi,                              \n
                                  tm.f_respuesta                                  \n
                             FROM ocg_solicitud_uso_garantia s,                   \n
                                  ocg_fecha_mig tm                                \n
                            WHERE tm.id_derechohabiente = s.id_derechohabiente    \n
                              AND tm.id_ocg_detalle     = s.id_ocg_detalle        \n
                              AND tm.id_ocg_referencia  = s.id_ocg_solicitud_ug   \n
                              AND subproceso = 3                                  \n
                              AND s.id_ocg_solicitud_ug = ? "
   
            --DISPLAY "Qry 2 a ejecutar \n: " ,v_s_qry
   
            PREPARE prp_cons_3_migra  FROM v_s_qry
            EXECUTE prp_cons_3_migra USING v_rec_detalle.id
                                      INTO v_f_envio,
                                           v_carga,
                                           v_f_liquida_cofi,
                                           v_respuesta

            LET v_s_qry = "SELECT LPAD(c.cve_ent_financiera,3,'0') || ' - ' || c.ent_financiera_desc \n
                             FROM ocg_solicitud_uso_garantia g,                         \n
                                  cat_entidad_financiera c                           \n
                            WHERE g.id_ocg_solicitud_ug = ?  \n
                              AND g.cve_ent_financiera  = c.cve_ent_financiera "
   
            PREPARE prp_cons_dat_ef_ug FROM v_s_qry
            EXECUTE prp_cons_dat_ef_ug USING v_rec_detalle.id
                                        INTO v_desc_ent_financiera

            LET v_s_qry = " SELECT a.importe_solicitado,
                                   a.f_vencimiento,
                                   a.importe_utilizado,
                                   a.estado,
                                   a.situacion,
                                   c.situacion_desc,
                                   a.tpo_credito,
                                   a.num_ctr_int_ef,
                                   b.cve_ent_financiera||'-'||b.ent_financiera_desc,
                                   a.id_ocg_formalizacion
                              FROM ocg_solicitud_uso_garantia a, 
                                   cat_entidad_financiera b,
                                   cat_ocg_situacion c
                             WHERE a.id_ocg_solicitud_ug = ? 
                               AND a.cve_ent_financiera = b.cve_ent_financiera
                               AND a.situacion = c.situacion"

            --DISPLAY "ID SOLIC UG, ",v_rec_detalle.id
            PREPARE prp_cons_ug FROM v_s_qry
            EXECUTE prp_cons_ug USING v_rec_detalle.id
                                 INTO v_importe_solicitado,
                                      v_f_vencimiento,
                                      v_importe_utilizado,
                                      v_estado,
                                      v_ax_situacion,
                                      v_situacion,
                                      v_producto,
                                      v_control_ef,
                                      v_ent_financiera,
                                      v_id_formaliza

      LET v_edo_uso = v_situacion
      LET v_rec_detalle.rfc = NULL

      DISPLAY v_rec_detalle.id
   --   DISPLAY v_f_envio
    -- DISPLAY v_carga
     -- DISPLAY v_respuesta

      IF v_ax_situacion = 110 OR 
         v_ax_situacion = 20 OR
         v_ax_situacion = 40 THEN

         LET v_respuesta = v_respuesta

      ELSE
         LET v_respuesta = NULL
      END IF
--******************************************************************************

 WHEN 4
 --DISPLAY "SP004 : "

            LET v_s_qry = "SELECT tm.f_envio  ,                                   \n
                                  tm.f_carga  ,                                   \n
                                  tm.f_liquida_cofi,                              \n
                                  tm.f_respuesta                                  \n
                             FROM ocg_ctr_transaccion t,                          \n
                                  ocg_fecha_mig tm                                \n
                            WHERE tm.id_derechohabiente = t.id_derechohabiente    \n
                              AND tm.id_ocg_detalle     = t.id_ocg_detalle        \n
                              AND tm.id_ocg_referencia  = t.id_ocg_ctr_transaccion \n
                              AND subproceso = 4                                  \n
                              AND t.id_ocg_ctr_transaccion = ? "
   
            --DISPLAY "Qry 4 a ejecutar \n: " ,v_s_qry
            --DISPLAY v_rec_detalle.id
   
            PREPARE prp_cons_4_migra  FROM v_s_qry
            EXECUTE prp_cons_4_migra USING v_rec_detalle.id
                                      INTO v_f_envio,
                                           v_carga,
                                           v_f_liquida_cofi,
                                           v_respuesta

            LET v_s_qry = "SELECT c.cve_ent_financiera||'-'||c.ent_financiera_desc, \n
                                  f.ap_paterno,                                     \n
                                  f.ap_materno,                                     \n
                                  f.nombre,                                         \n
                                  tr.num_ctr_int_ef,                                \n
                                  tr.periodo_pago,                                  \n
                                  gd.f_proceso,                                     \n
                                  tr.f_transaccion,                                 \n
                                  tr.f_pago,                                        \n
                                  tr.concepto,                                      \n
                                  tr.vivienda_97,                                   \n
                                  tr.estado,                                        \n
                                  f.tpo_credito                                     \n
                             FROM ocg_detalle gd,                                   \n
                                  ocg_ctr_transaccion tr,                           \n
                                  OUTER ocg_formalizacion f,                        \n
                                  cat_entidad_financiera c                          \n
                            WHERE gd.nss = ?             \n
                              AND tr.id_ocg_ctr_transaccion = ?     \n
                              AND gd.id_ocg_detalle = tr.id_ocg_detalle  \n
                              AND tr.cve_ent_financiera  = c.cve_ent_financiera  \n
                              AND f.id_ocg_formalizacion = tr.id_ocg_formalizacion "

     --DISPLAY "id transaccion : ",v_rec_detalle.id
    -- DISPLAY " nss           : ",v_rec_detalle.nss

            PREPARE prp_cons_dat_tr FROM v_s_qry
            EXECUTE prp_cons_dat_tr USING v_rec_detalle.nss,
                                          v_rec_detalle.id
                                     INTO v_desc_ent_financiera,
                                          v_ap_paterno,
                                          v_ap_materno,
                                          v_nombre,
                                          v_control_ef,
                                          v_periodo_pago,
                                          v_f_proceso,
                                          v_f_transaccion,
                                          v_f_pago,
                                          v_concepto,
                                          v_viv_97,
                                          v_edo_transaccion,
                                          v_producto

         LET v_nombre      = NULL
         LET v_ap_paterno  = NULL
         LET v_ap_materno  = NULL

--******************************************************************************

      WHEN 5
            LET v_id_formaliza= NULL

            LET v_s_qry = "SELECT tm.f_envio  ,                                   \n
                                  tm.f_carga  ,                                   \n
                                  tm.f_liquida_cofi,                              \n
                                  tm.f_respuesta                                  \n
                             FROM ocg_liquidacion l,                              \n
                                  ocg_fecha_mig tm                                \n
                            WHERE tm.id_derechohabiente = l.id_derechohabiente    \n
                              AND tm.id_ocg_detalle     = l.id_ocg_detalle        \n
                              AND tm.id_ocg_referencia  = l.id_ocg_liquidacion    \n
                              AND subproceso = 5                                  \n
                              AND l.id_ocg_liquidacion = ? "
   
            --DISPLAY "Qry 2 a ejecutar \n: " ,v_s_qry
   
            PREPARE prp_cons_5_migra  FROM v_s_qry
            EXECUTE prp_cons_5_migra USING v_rec_detalle.id
                                      INTO v_f_envio,
                                           v_carga,
                                           v_f_liquida_cofi,
                                           v_respuesta

                                           

            LET v_s_qry = "SELECT liq.situacion,                                \n
                                  liq.f_liberacion_gtia,                        \n
                                  liq.importe_devuelto,                         \n
                                  liq.id_causa_liquida,                         \n
                                  liq.f_deposito,                               \n
                                  liq.bimestre_ap_subsec,
                                  liq.importe_ap_subsec,
                                  liq.id_ocg_formalizacion,
                                  liq.id_causa_liquida,
                                  acre.f_liquida_credito,
                                  liq.tpo_credito,
                                  liq.num_ctr_int_ef
                             FROM ocg_liquidacion liq,                          \n
                                  ocg_detalle det, OUTER ocg_acreditado acre           \n
                            WHERE liq.id_ocg_liquidacion = ?                    \n
                              AND det.id_ocg_detalle = liq.id_ocg_detalle       \n
                              AND acre.id_ocg_formalizacion = liq.id_ocg_formalizacion"

         --DISPLAY "qry sp00.. : ",v_s_qry
            PREPARE prp_cons_dat_liq FROM v_s_qry
            EXECUTE prp_cons_dat_liq USING v_rec_detalle.id
                                          INTO v_ax_situacion,
                                               v_f_liberacion,
                                               v_imp_devuelto,
                                               v_causa_liquida,
                                               v_f_deposito,
                                               v_bim_ap,
                                               v_imp_ap_subsec,
                                               v_id_formaliza,
                                               v_causa_liquida,
                                               v_f_liquida,
                                               v_producto,
                                               v_control_ef
                                               
          SELECT estado
            INTO v_estado
            FROM ocg_acreditado
          WHERE id_ocg_formalizacion = (SELECT id_ocg_formalizacion
                                          FROM ocg_liquidacion
                                         WHERE id_ocg_liquidacion =  v_rec_detalle.id)

           LET v_s_qry = "SELECT LPAD(c.cve_ent_financiera,3,'0') || ' - ' || c.ent_financiera_desc \n
                             FROM ocg_liquidacion liq,                         \n
                                  cat_entidad_financiera c                           \n
                            WHERE liq.id_ocg_liquidacion = ?  \n
                              AND liq.cve_ent_financiera  = c.cve_ent_financiera "
   
            PREPARE prp_cons_dat_ef_liq FROM v_s_qry
            EXECUTE prp_cons_dat_ef_liq USING v_rec_detalle.id
                                        INTO v_desc_ent_financiera

      END CASE

--******************************************************************************

      IF v_f_liquida_cofi IS NULL OR
         v_f_liquida_cofi = "12/31/1899" THEN
         LET v_f_liquida_cofi = NULL
      END IF

      IF (v_producto = 7 ) OR
         (v_producto = 8 ) THEN
         LET v_f_subcta = NULL
         LET v_sdo_97   = NULL

         IF v_f_liquida_cofi IS NULL OR
            v_f_liquida_cofi = "12/31/1899" THEN
            LET v_f_liquida_cofi = NULL
         END IF

         LET v_f_vigencia  = NULL
         LET v_ax_vigencia = NULL
      ELSE
         LET v_f_liq_cofi = NULL
      END IF

      IF v_producto IS NULL THEN
         SELECT tpo_credito
           INTO v_producto
           FROM ocg_tramite
          WHERE id_ocg_tramite = (SELECT id_ocg_tramite
                                    FROM ocg_liquidacion
                                   WHERE id_ocg_liquidacion  = v_rec_detalle.id)
      END IF
--******************************************************************************

      IF v_f_formaliza_ins IS NULL OR
         v_f_formaliza_ins = "12/31/1899" THEN
         LET v_f_formaliza_ins = NULL
      END IF

      DISPLAY v_ax_situacion            TO v_situacion
      DISPLAY v_desc_ent_financiera     TO v_cve_ent_financiera
      DISPLAY v_rec_detalle.diagnostico TO v_diagnostico
      DISPLAY v_rec_detalle.nss         TO v_nss
--***************************************************************

      SELECT count(*)
        INTO v_cnt_unificador
        FROM ocg_unificacion
       WHERE nss_unificador = v_rec_detalle.nss

      IF v_cnt_unificador >= 1 THEN
         LET v_desc_nss_1 = "NSS UNIFICADOR"

         LET v_s_qry_do = " SELECT first 1 (nss_unificado)
                           FROM ocg_unificacion
                          WHERE nss_unificador = ?"

         PREPARE prp_do FROM v_s_qry_do
         EXECUTE prp_do USING v_rec_detalle.nss
                        INTO v_nss_unificacion

         IF v_nss_unificacion IS NOT NULL THEN
            LET v_desc_nss_2 = "NSS UNIFICADO"
         END IF

      ELSE
         LET v_desc_nss_1      = ""
         LET v_nss_unificacion = ""
      END IF

      IF v_cnt_unificador = 0 THEN

         SELECT count(*)
           INTO v_cnt_unificado
           FROM ocg_unificacion
          WHERE nss_unificado = v_rec_detalle.nss

         IF v_cnt_unificado >= 1 THEN
            LET v_desc_nss_1 = "NSS UNIFICADO"

            LET v_s_qry_dor = " SELECT first 1 (nss_unificador)
                                 FROM ocg_unificacion
                                WHERE nss_unificado = ?"
            PREPARE prp_dor FROM v_s_qry_dor
            EXECUTE prp_dor USING v_rec_detalle.nss
                            INTO v_nss_unificacion

            IF v_nss_unificacion IS NOT NULL THEN
               LET v_desc_nss_2 = "NSS UNIFICADOR"
            END IF

         ELSE
            LET v_desc_nss_2  = ""
            LET v_nss_unificacion = ""
         END IF
      END IF

      DISPLAY BY NAME v_desc_nss_1
      DISPLAY BY NAME v_desc_nss_2
      DISPLAY BY NAME v_nss_unificacion

--***************************************************************
      DISPLAY v_rec_detalle.rfc         TO v_rfc
      DISPLAY BY NAME v_cve_nss_asociado
      DISPLAY BY NAME v_clave_nss
      DISPLAY BY NAME v_sdo_97
      DISPLAY BY NAME v_ap_paterno
      DISPLAY BY NAME v_ap_materno
      DISPLAY BY NAME v_nombre
      DISPLAY BY NAME v_producto
      DISPLAY BY NAME v_curp
      DISPLAY BY NAME v_genero
      DISPLAY BY NAME v_f_formaliza_ins

      LET v_control_ef = v_control_ef --USING "&&&&&&&&&&&&&&&&&&"

      DISPLAY BY NAME v_control_ef

      IF v_f_proceso IS NULL OR v_f_proceso = "12/31/1899" THEN
         LET v_f_proceso = NULL
      END IF

     -- DISPLAY "f_proceso_cofi :",v_f_proceso
      
      IF v_f_liquida = "12/31/1899" THEN
         LET v_f_liquida = NULL
      END IF

      DISPLAY BY NAME v_f_liquida
      DISPLAY v_rec_detalle.f_proceso TO v_f_proceso_1

      --DISPLAY "fecha liquidación cofi : ",v_f_liquida_cofi
       IF v_f_liq_cofi IS NULL OR
            v_f_liq_cofi = "12/31/1899" THEN
            LET v_f_liq_cofi = NULL
       END IF

       IF v_f_liquida_cofi IS NOT NULL THEN
          DISPLAY v_f_liquida_cofi TO v_f_liquida_cofi
       ELSE
          DISPLAY v_f_liq_cofi TO v_f_liquida_cofi
       END IF

      DISPLAY BY NAME v_respuesta

      DISPLAY v_rec_detalle.subproceso  TO v_subproceso

           IF v_f_reg_carta IS NULL OR v_f_reg_carta = "12/31/1899" THEN
              LET v_f_reg_carta = NULL 
           END IF

         --  DISPLAY v_f_formaliza_ins USING 'dd-mm-yyyy' TO v_f_formaliza_ins 
        --END IF

      LET v_estado_desc = ""

      SELECT edo_credito_desc
        INTO v_estado_desc
       FROM cat_ocg_estado
       WHERE edo_credito = v_estado

      DISPLAY BY NAME v_estado_desc

      IF v_f_subcta IS NULL OR v_f_subcta = "12/31/1899" THEN
         LET v_ax_f_subcta = NULL
         DISPLAY v_ax_f_subcta TO v_f_subcta
      ELSE 
         DISPLAY v_f_subcta USING 'dd-mm-yyyy' TO v_f_envio
         DISPLAY v_f_subcta USING 'dd-mm-yyyy' TO v_f_subcta
      END IF
      IF v_f_envio IS NULL OR v_f_envio = "12/31/1899" THEN
         LET v_ax_f_envio = NULL
         DISPLAY v_ax_f_envio TO v_f_envio
      ELSE 
         DISPLAY v_f_envio USING 'dd-mm-yyyy' TO v_f_envio
      END IF
      IF v_respuesta IS NULL OR v_respuesta = "12/31/1899" THEN
         LET v_ax_respuesta = NULL
         DISPLAY v_ax_respuesta TO v_respuesta
      ELSE 
         DISPLAY v_respuesta USING 'dd-mm-yyyy' TO v_respuesta
      END IF
      --DISPLAY BY NAME v_respuesta
      IF v_f_vigencia IS NULL OR v_f_vigencia = "12/31/1899" THEN
         LET v_ax_vigencia = NULL
         DISPLAY v_ax_vigencia TO v_f_vigencia
      ELSE 
         DISPLAY v_f_vigencia USING 'dd-mm-yyyy' TO v_f_vigencia
      END IF
      --DISPLAY BY NAME v_f_vigencia
      --DISPLAY BY NAME v_carga
      IF v_carga IS NULL OR v_carga = "12/31/1899" THEN
         LET v_ax_carga = NULL
         DISPLAY v_ax_carga TO v_carga
      ELSE 
         DISPLAY v_carga USING 'dd-mm-yyyy' TO v_carga
      END IF


      --MENU
      DISPLAY ARRAY v_arr_inconsistencia TO r_inconsistencia.* ATTRIBUTES(ACCEPT=FALSE,CANCEL=FALSE)
         ON ACTION siguiente
            OPEN WINDOW w_det_2 WITH FORM "OCGC014"
               --Display de segunda pantalla
               DISPLAY v_rec_detalle.nss TO v_nss
               DISPLAY v_ax_situacion    TO v_situacion
               --DISPLAY BY NAME v_estado
               DISPLAY BY NAME v_num_escritura
               DISPLAY BY NAME v_notario
               DISPLAY BY NAME v_num_rpp
               DISPLAY BY NAME v_folio_real
               DISPLAY BY NAME v_partida
               DISPLAY BY NAME v_foja
               DISPLAY BY NAME v_volumen
               DISPLAY BY NAME v_libro
               DISPLAY BY NAME v_tomo
               DISPLAY BY NAME v_seccion
               DISPLAY BY NAME v_domicilio_inmueble
               LET v_valor_avaluo1  = v_valor_avaluo
               DISPLAY BY NAME v_valor_avaluo1
               DISPLAY BY NAME v_monto_credito
               DISPLAY BY NAME v_plazo_credito
               DISPLAY BY NAME v_tasa_base
               DISPLAY BY NAME v_margen
               DISPLAY BY NAME v_usuario_carta

               IF v_ent_fed_notario <> 0 THEN
                  DISPLAY v_ent_fed_notario USING '&&' TO v_ent_fed_notario
               END IF
               IF v_mcpio_notario <> 0THEN 
                  DISPLAY v_mcpio_notario USING '&&&' TO v_mcpio_notario
               END IF
               IF v_f_reg_carta IS NULL OR v_f_reg_carta = "12/31/1899" THEN
                  LET v_ax_f_reg_carta = NULL
                  DISPLAY v_ax_f_reg_carta TO v_f_reg_carta
               ELSE 
                  DISPLAY v_f_reg_carta USING 'dd-mm-yyyy' TO v_f_reg_carta
               END IF
               IF v_f_otorga_ent_fin IS NULL OR v_f_otorga_ent_fin = "12/31/1899" THEN
                  LET v_ax_f_otorga_ent_fin = NULL 
                  DISPLAY v_ax_f_otorga_ent_fin TO v_f_otorga_ent_fin
               ELSE
                  DISPLAY v_f_otorga_ent_fin USING 'dd-mm-yyyy' TO v_f_otorga_ent_fin
               END IF
               IF v_tpo_moneda <> 0 THEN 
                  DISPLAY v_tpo_moneda USING '&&' TO v_tpo_moneda
               END IF
               IF v_ent_fed_inmueble <> 0 THEN
                  DISPLAY v_ent_fed_inmueble USING '&&' TO v_ent_fed_inmueble
               END IF

               MENU
                  ON ACTION siguiente
                     OPEN WINDOW w_det_3 WITH FORM "OCGC016"
                        -- Display de variables terecera pantalla
                        DISPLAY v_rec_detalle.nss TO v_nss


                        IF v_ent_financiera IS NULL THEN
                           LET v_ent_financiera = v_desc_ent_financiera
                        END IF

                        IF (v_rec_detalle.subproceso = 3)THEN

                           IF v_f_vencimiento IS NULL OR v_f_vencimiento = "12/31/1899" THEN
                              LET v_ax_f_vencimiento = NULL
                              DISPLAY v_ax_f_vencimiento USING '##########' TO v_f_vencimiento
                           ELSE
                              DISPLAY v_f_vencimiento USING 'dd-mm-yyyy' TO v_f_vencimiento
                           END IF

                           DISPLAY BY NAME v_importe_solicitado
                           DISPLAY BY NAME v_importe_utilizado
                           DISPLAY BY NAME v_producto
                           DISPLAY BY NAME v_ent_financiera
                           DISPLAY BY NAME v_control_ef
                           DISPLAY BY NAME v_edo_uso
                           --DISPLAY BY NAME v_f_envio
                           --DISPLAY BY NAME v_carga
                           --DISPLAY BY NAME v_respuesta

                        END IF

                        MENU
                           ON ACTION siguiente
                              OPEN WINDOW w_det_4 WITH FORM "OCGC015"

                        IF v_f_proceso IS NULL OR v_f_proceso = "12/31/1899" THEN
                           LET v_f_proceso = NULL
                        END IF

                        IF v_f_transaccion IS NULL OR v_f_transaccion = "12/31/1899" THEN
                           LET v_f_transaccion = NULL
                        END IF

                        IF v_f_pago IS NULL OR v_f_pago = "12/31/1899" THEN
                           LET v_f_pago = NULL
                        END IF

                        IF v_concepto = 0 THEN
                           LET v_concepto = NULL
                        END IF

                        DISPLAY v_rec_detalle.nss TO v_nss

                        IF (v_rec_detalle.subproceso = 4)THEN

                           LET v_desc_ent_financiera = v_ent_financiera
                              DISPLAY v_ent_financiera 
                           
                           DISPLAY v_rec_detalle.nss TO v_nss
                           DISPLAY BY NAME v_periodo_pago
                           DISPLAY BY NAME v_f_proceso
                           DISPLAY BY NAME v_f_transaccion                        
                           DISPLAY BY NAME v_f_pago 
                           DISPLAY BY NAME v_concepto
                           DISPLAY BY NAME v_producto
                           DISPLAY BY NAME v_ent_financiera
                           DISPLAY BY NAME v_control_ef
                           DISPLAY BY NAME v_edo_transaccion
                           DISPLAY v_viv_97 TO v_imp_pago

                        END IF

                              MENU
                                 ON ACTION siguiente
                                    OPEN WINDOW w_det_5 WITH FORM "OCGC017"

                                       IF v_f_liberacion IS NULL OR v_f_liberacion = "12/31/1899" THEN
                                          LET v_f_liberacion = NULL
                                       END IF

                                       IF v_causa_liquida IS NULL OR v_causa_liquida = 0 THEN
                                          LET v_causa_liquida = NULL
                                       END IF
                                       
                                       IF v_f_deposito IS NULL OR v_f_deposito = "12/31/1899" THEN
                                          LET v_f_deposito = NULL
                                       END IF

                        IF v_ent_financiera IS NULL THEN
                           LET v_ent_financiera = v_desc_ent_financiera
                        END IF

                        DISPLAY v_rec_detalle.nss TO v_nss

                        IF (v_rec_detalle.subproceso = 5)THEN

                            --DISPLAY v_rec_detalle.nss TO v_nss
                            DISPLAY v_rec_detalle.situacion TO v_situacion
                            DISPLAY BY NAME v_imp_devuelto
                            DISPLAY BY NAME v_f_deposito
                            DISPLAY BY NAME v_bim_ap
                            DISPLAY BY NAME v_imp_ap_subsec
                            DISPLAY BY NAME v_ent_financiera
                            DISPLAY BY NAME v_control_ef
                            DISPLAY BY NAME v_f_liberacion
                            DISPLAY BY NAME v_causa_liquida
                         END IF
                                    MENU
                                       ON ACTION anterior
                                    EXIT MENU
                                    END MENU
                                    CLOSE WINDOW w_det_5

                                 ON ACTION anterior
                                    EXIT MENU
                                    END MENU
                                    CLOSE WINDOW w_det_4

                           ON ACTION anterior
                              EXIT MENU
                        END MENU

                     CLOSE WINDOW w_det_3

                  ON ACTION anterior
                     EXIT MENU 
               END MENU 
            CLOSE WINDOW w_det_2

         ON ACTION salir 
            EXIT DISPLAY
      --END MENU
      END DISPLAY 

   CLOSE WINDOW w_det_1
END FUNCTION

FUNCTION fn_llena_inconsistencias(p_id_dh,p_id_proceso,p_subproceso)
   DEFINE p_id_dh                   DECIMAL(9,0)
   DEFINE p_id_proceso              DECIMAL(9,0)
   DEFINE p_subproceso              SMALLINT
   DEFINE v_nom_tabla               CHAR(40)
   DEFINE v_alias                   CHAR(2)
   DEFINE v_identificador           CHAR(25)

   LET v_cnt = 1      -- Se inicializa el contador
   
   -- se limpia el arreglo
   CALL v_arr_inconsistencia.clear()

   LET v_s_qry = "SELECT nom_tabla,
                         alias,
                         identificador
                    FROM cat_tabla_subproceso
                   WHERE subproceso = ?"

   PREPARE prp_c_ax_tabla FROM v_s_qry
   EXECUTE prp_c_ax_tabla INTO v_nom_tabla,
                               v_alias,
                               v_identificador
                         USING p_subproceso

   LET v_s_qry = " SELECT gci.inconsistencia,   \n"
               ||"        gci.incons_desc       \n"
               ||" FROM "|| v_nom_tabla || " " || v_alias ||",\n" 
               ||"        ocg_inconsistencia gi, \n "
               ||"        cat_inconsistencia gci \n" 
               ||"  WHERE gci.inconsistencia = gi.inconsistencia \n "
               ||"    AND " || v_alias || "."|| v_identificador || " = gi.id_ocg_referencia \n"
               ||"    AND " || v_alias || ".id_derechohabiente = " || p_id_dh || "\n"
               ||"    AND " || v_alias || "." || v_identificador ||" = " || p_id_proceso|| " \n "
               ||"    AND gi.subproceso = " || p_subproceso 

 --  DISPLAY "qry incons :\n",v_s_qry
   
   PREPARE prp_inconsis FROM v_s_qry
   DECLARE cur_inc CURSOR FOR prp_inconsis

   --FOREACH cur_inc USING p_id_dh, p_id_proceso, p_subproceso INTO v_arr_inconsistencia[v_cnt].*
   FOREACH cur_inc INTO v_arr_inconsistencia[v_cnt].*
      --DISPLAY "Arreglo :",v_arr_inconsistencia[v_cnt].*
      LET v_cnt = v_cnt + 1 
   END FOREACH 
   CLOSE cur_inc
   FREE cur_inc
   CALL v_arr_inconsistencia.deleteElement(v_arr_inconsistencia.getLength())

END FUNCTION


-- Función para validar nss
FUNCTION fn_valida_nss(p_nss_modificado)
   DEFINE p_nss_modificado          LIKE afi_derechohabiente.nss
   DEFINE v_nss                     STRING       -- cadena con el NSS,
   DEFINE v_mensaje                 STRING       -- mensaje para el usuario
   DEFINE v_indice                  SMALLINT     -- indice pivote
   DEFINE v_nss_es_correcto         SMALLINT     -- booleana que indica si un NSS esta correctamente construido
   

   LET v_nss = p_nss_modificado CLIPPED

   -- se asume que el NSS esta correcto
   LET v_nss_es_correcto = TRUE

   -- NSS debe ser de 11 digitos
   IF ( v_nss.getLength() <> 11 ) THEN
      LET v_mensaje = "La longitud del NSS debe ser de 11 dígitos"
      LET v_nss_es_correcto = FALSE
   ELSE
      -- se verifica que todos los caracteres sean numericos
      FOR v_indice = 1 TO v_nss.getLength()
         IF ( v_nss.getCharAt(v_indice) < "0" OR v_nss.getCharAt(v_indice) > "9" ) THEN
            LET v_mensaje = "El NSS contiene caracteres no numéricos."
            LET v_nss_es_correcto = FALSE
            EXIT FOR
         END IF
      END FOR
   END IF
 
   -- si hubo algun error, se envia mensaje en pantalla indicando cual es
   IF ( NOT v_nss_es_correcto ) THEN
      CALL fn_mensaje("Error",v_mensaje,"stop")
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_nss_es_correcto
END FUNCTION
