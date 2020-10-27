#########################################################################
#Proyecto          => INFONAVIT (MEXICO)                                #
#Propietario       => E.F.P.                                            #
#Programa AFIC03   => Programa de consulta de rechazo de afiliatorios   #
#Sistema           => AfI                                               #
#Fecha             => 30 de junio de 2012                               #
#########################################################################

DATABASE safre_viv

GLOBALS
   DEFINE cb             ui.ComboBox

   DEFINE v_d_pid                DECIMAL(9,0) -- identificador del proceso
   DEFINE v_i_opera_cod          LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE v_i_proceso_cod        LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_usuario_cod          LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   DEFINE v_folio                DECIMAL (9,0)
   DEFINE p_v_nom_prog           VARCHAR(30) -- nombre del programa

   DEFINE arr_afi_rch DYNAMIC ARRAY OF RECORD 
      tpo_movimiento             LIKE afi_rch_afiliatorio.tpo_movimiento,
      nrp                        LIKE afi_rch_afiliatorio.nrp,
      f_movimiento               LIKE afi_rch_afiliatorio.f_movimiento,
      curp_rfc                   LIKE afi_rch_afiliatorio.curp_rfc,
      t_trabajador               LIKE afi_rch_afiliatorio.t_trabajador,
      nss                        LIKE afi_rch_afiliatorio.nss,
      nombre                     LIKE afi_rch_afiliatorio.nombre,
      jornada_semana             LIKE afi_rch_afiliatorio.jornada_semana,
      sdi                        LIKE afi_rch_afiliatorio.sdi,
      sexo                       LIKE afi_rch_afiliatorio.sexo,
      nss_correcto               LIKE afi_rch_afiliatorio.nss_correcto,
      nombre_correcto            LIKE afi_rch_afiliatorio.nombre_correcto,
      riss_imss                  CHAR(40),
      cod_rechazo                CHAR(40),
      f_actualiza                DATE
   END RECORD 

END GLOBALS

MAIN

-- Sección de variables UI
   DEFINE v_query                STRING
   DEFINE v_index                INTEGER
   DEFINE f_nss                  LIKE afi_rch_afiliatorio.nss
   DEFINE f_rfc                  LIKE afi_rch_afiliatorio.curp_rfc
   DEFINE f_cod_rechazo          LIKE afi_rch_afiliatorio.cod_rechazo
   DEFINE f_cmb_mvto             LIKE afi_rch_afiliatorio.tpo_movimiento
   DEFINE v_folio                LIKE glo_folio.folio -- folio del proceso

   LET p_usuario_cod   = ARG_VAL(1) -- Recibe la variable de usuario
   LET v_i_proceso_cod = ARG_VAL(2) -- Recibe el tipo de proceso
   LET p_v_nom_prog    = ARG_VAL(3) -- Recibe el nombre del programa

   LET v_index = 1
   LET v_query = " "
   LET v_folio = 0

   CLOSE WINDOW SCREEN

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   OPEN WINDOW v_afic030 WITH FORM "AFIC030"
      DIALOG ATTRIBUTE(UNBUFFERED)

      INPUT BY NAME f_nss, f_rfc,
                    f_cod_rechazo,f_cmb_mvto
                    , v_folio

         BEFORE INPUT 
            -- se llena el combo con los tipos de movimiento
            CALL fn_llena_combo_tipo_mov()

         ON ACTION ACCEPT 
            --Valida que se haya capturado al menos un parámetro
            IF ( f_nss IS NULL AND
                 f_rfc IS NULL AND
                 f_cod_rechazo IS NULL AND 
                 f_cmb_mvto IS NULL AND
                 v_folio IS NULL) THEN

               CALL fn_mensaje("Atención","Ingrese al menos un criterio de búsqueda","info")
            END IF

            --Armamos el query dinámico
            IF f_nss IS NOT NULL THEN
               LET v_query = v_query || "\n AND afi.nss = '",f_nss,"'"
            END IF 

            IF f_rfc IS NOT NULL THEN
               LET v_query = v_query || "\n AND afi.curp_rfc = '",f_rfc,"'"
            END IF 

            IF f_cod_rechazo IS NOT NULL THEN
               LET v_query = v_query || "\n AND afi.cod_rechazo = ",f_cod_rechazo
            END IF 

            IF f_cmb_mvto IS NOT NULL THEN
               LET v_query = v_query || "\n AND afi.tpo_movimiento = '",f_cmb_mvto,"'"
            END IF 

            IF v_folio IS NOT NULL THEN
               LET v_query = v_query || "\n AND afi.folio_lote = ", v_folio
            END IF 

            DISPLAY "v_query -- ",v_query
            CALL arr_afi_rch.clear()
            CALL fn_muestra_consulta(v_query)
            LET v_query = " "

      ON ACTION cancelar
         EXIT DIALOG

      END INPUT 

      END DIALOG

   CLOSE WINDOW v_afic030

END MAIN 

#Objetivo: Realiza la consulta y muestra en pantalla 
FUNCTION fn_muestra_consulta(v_comp)

    --Sección de variables UI
   DEFINE v_query   STRING
   DEFINE v_comp    STRING
   DEFINE v_index   INTEGER

   LET v_index = 1

         LET v_query =  "\n SELECT afi.tpo_movimiento,",
                        "\n afi.nrp, ",                
                        "\n afi.f_movimiento, ",       
                        "\n afi.curp_rfc, ",           
                        "\n afi.t_trabajador,  ",      
                        "\n afi.nss,   ",              
                        "\n afi.nombre, ",               
                        "\n afi.jornada_semana, ",     
                        "\n afi.sdi, ",             
                        "\n afi.sexo,  ",              
                        "\n afi.nss_correcto, ",       
                        "\n afi.nombre_correcto,  ",
                        "\n cr.desc_riss, ",
                        "\n afi.cod_rechazo || '-' ||  ct.des_rechazo descripcion, ",
                        "\n g.f_actualiza ",
                        "\n FROM afi_rch_afiliatorio afi, afi_cat_rch ct, glo_folio g,  ",
                        "\n OUTER cat_riss cr  ",
                        "\n WHERE afi.cod_rechazo = ct.cod_rechazo ",
                        "\n AND afi.folio_lote = g.folio ",
                        "\n AND afi.riss_imss = cr.id_riss"

         LET v_query = v_query || v_comp
         LET v_query = v_query || "\n ORDER BY f_actualiza DESC"

         DISPLAY "consulta -- ",v_query

         PREPARE prp_afi_rch FROM v_query
         DECLARE cur_afi_rch CURSOR FOR prp_afi_rch

         FOREACH cur_afi_rch INTO arr_afi_rch[v_index].*

               LET v_index = v_index + 1
         END FOREACH 

         -- se borra el ultimo registro
         CALL arr_afi_rch.deleteElement(v_index)
         LET v_index = v_index - 1

         --DISPLAY "v_index -- ",v_index
         IF v_index <=0 THEN 

            CALL fn_mensaje("Información","No existe información con estos parámetros","info")

         ELSE
            OPEN WINDOW v_afi WITH FORM "AFIC031"

               DISPLAY ARRAY arr_afi_rch TO scr_afiliatorios.*
               LET v_query = " "

            CLOSE WINDOW v_afi
         END IF 

END FUNCTION 

##Función que llena el combo para seleccionar un tipo de movimiento
FUNCTION fn_llena_combo_tipo_mov()

   DEFINE arr_tipos_mov DYNAMIC ARRAY OF RECORD 
      cve_mov     LIKE  afi_rch_afiliatorio.tpo_movimiento,
      desc_mov    VARCHAR (22)
   END RECORD 

   DEFINE v_ind_cmb  SMALLINT 


   LET cb = ui.ComboBox.forName("f_cmb_mvto") --Asignación del combo a la forma
   LET v_ind_cmb = 1

   -- Validación si el combo es nulo 
   IF cb IS NULL THEN
      ERROR "Form field not found in current form"
      EXIT PROGRAM
   END IF

   -- se reinicia el cmbo
   CALL cb.clear()

   --Llena el combo
   LET arr_tipos_mov[1].cve_mov = "01"
   LET arr_tipos_mov[1].desc_mov = "01-Alta"

   LET arr_tipos_mov[2].cve_mov = "02"
   LET arr_tipos_mov[2].desc_mov = "02-Baja"

   LET arr_tipos_mov[3].cve_mov = "05"
   LET arr_tipos_mov[3].desc_mov = "05-Cambio de número SS"

   LET arr_tipos_mov[4].cve_mov = "06"
   LET arr_tipos_mov[4].desc_mov = "06-Cambio de nombre"

   LET arr_tipos_mov[5].cve_mov = "08"
   LET arr_tipos_mov[5].desc_mov = "08-Reingreso"

   LET arr_tipos_mov[6].cve_mov = "75"
   LET arr_tipos_mov[6].desc_mov = "75-Operación 75"

   LET arr_tipos_mov[6].cve_mov = "21"
   LET arr_tipos_mov[6].desc_mov = "21-RISS"

    -- Limpia el combo
   CALL cb.clear()

   FOR v_ind_cmb = 1 TO 6
      CALL cb.addItem(arr_tipos_mov[v_ind_cmb].cve_mov,arr_tipos_mov[v_ind_cmb].desc_mov)
   END FOR 

END FUNCTION 