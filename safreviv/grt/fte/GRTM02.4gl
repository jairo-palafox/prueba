--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: Noviembre 12, 2018.
--==============================================================================
################################################################################
#Modulo       => GRT                                                           #
#Programa     => GRTM02                                                        #
#Objetivo     => Admninistración de Usuario de Entidades Financieras.          #
#Fecha inicio => Noviembre 13, 2018                                            #
#Elaboró      => José Antonio Gómez Contreras                                  #
#Modifica     => Emilio Abarca,EFP.                                            #
#Fecha        => 27/Diciembre/2018.                                            #
################################################################################

DATABASE safre_viv

DEFINE g_usuario      CHAR(20)
DEFINE g_tipo_proceso SMALLINT
DEFINE g_nom_ventana  STRING

PRIVATE DEFINE arr_detalles DYNAMIC ARRAY OF RECORD
                  v_id_usuario_ef       DECIMAL(9,0),
                  v_rfc                 CHAR(13),
                  v_ent_financiera_desc CHAR(65),
                  v_nombre              CHAR(50),
                  v_primer_apellido     CHAR(50),
                  v_segundo_apellido    CHAR(50), 
                  v_curp                CHAR(18),
                  v_email               CHAR(50),
                  v_f_registro          DATE,
                  v_f_baja              DATE,
                  v_estado              SMALLINT,
                  v_estado_desc         CHAR(15)
END RECORD

   DEFINE g_arr_det_rpt DYNAMIC ARRAY OF RECORD
      entidad_financiera CHAR(70),
      rfc    CHAR(13),
      curp   CHAR(18),
      nombre CHAR(50),
      primer_apellido  CHAR(50),
      segundo_apellido CHAR(50),
      f_registro       DATE,
      f_baja           DATE,
      estatus          CHAR(8)
   END RECORD

MAIN
DEFINE v_ruta_bitacora  CHAR(40)
DEFINE v_archivo_log    STRING
DEFINE v_programa       STRING
DEFINE v_front          STRING
DEFINE lc_error   CHAR(1), -- Regresa estatus de error 
       ls_qry     STRING

   -- se incorpora como parametros enviados desde el menu el proceso y codigo de operaciòn
   LET g_usuario      = ARG_VAL(1) 
   LET g_tipo_proceso = ARG_VAL(2) 
   LET g_nom_ventana  = ARG_VAL(3) 

   LET v_programa     = "GRTM02" 

   IF ( g_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_ventana)
   END IF

    SELECT ruta_bitacora
     INTO v_ruta_bitacora
     FROM seg_modulo
    WHERE modulo_cod = "grt"

    LET v_archivo_log = v_ruta_bitacora CLIPPED ,"/",g_usuario CLIPPED, ".",v_programa,".log"
    CALL STARTLOG(v_archivo_log)

   CLOSE WINDOW SCREEN

   OPEN WINDOW vtn_princial WITH FORM "GRTM022"
         CALL fn_consulta_usuarios(g_usuario)
   CLOSE WINDOW vtn_princial 
END MAIN

#OBJETIVO: Consulta modulos
FUNCTION fn_consulta_usuarios(v_usuario) 
DEFINE v_usuario CHAR(20),
       i         INTEGER,
       j         INTEGER,
       x         INTEGER,
       v_nuevo   SMALLINT,
       v_rfc     CHAR(13),
       v_confirma SMALLINT

DEFINE arr_detalles_hist DYNAMIC ARRAY OF RECORD
          v_id_usuario_ef DECIMAL(9,0), 
          v_rfc CHAR(13),
          v_cve_ent_financiera_desc CHAR(60),
          v_nombre CHAR(50),
          v_primer_apellido CHAR(50),
          v_segundo_apellido CHAR(50),
          v_curp CHAR(18),
          v_email CHAR(50),
          v_f_modifica DATE,
          v_estado SMALLINT,
          v_estado_desc CHAR(15),
          v_usuario_modifica CHAR(20)
END RECORD

   CALL arr_detalles.clear()
   
   DECLARE cur_usuarios CURSOR FOR SELECT a.id_usuario_ef,
                                          a.rfc,
                                          b.ent_financiera_desc,
                                          a.nombre,
                                          a.primer_apellido,
                                          a.segundo_apellido, 
                                          a.curp,
                                          a.email,
                                          a.f_registro,
                                          a.f_baja,
                                          a.estado
                                   FROM   cat_usuario_ef a,
                                          cat_entidad_financiera b
                                   WHERE  b.cve_ent_financiera = a.cve_ent_financiera
                                   AND    a.estado = 10

   LET i = 1

   FOREACH cur_usuarios INTO arr_detalles[i].v_id_usuario_ef,
                             arr_detalles[i].v_rfc,
                             arr_detalles[i].v_ent_financiera_desc,
                             arr_detalles[i].v_nombre,
                             arr_detalles[i].v_primer_apellido,
                             arr_detalles[i].v_segundo_apellido, 
                             arr_detalles[i].v_curp,
                             arr_detalles[i].v_email,
                             arr_detalles[i].v_f_registro,
                             arr_detalles[i].v_f_baja,
                             arr_detalles[i].v_estado

      IF arr_detalles[i].v_estado = 10 THEN
         LET arr_detalles[i].v_estado_desc = "10 - ACTIVO"
      ELSE
         LET arr_detalles[i].v_estado_desc = "20 - INACTIVO"
      END IF
 
      LET i = i + 1
   END FOREACH

   FREE cur_usuarios

   --IF ( arr_detalles[arr_detalles.getLength()].v_modulo_cod IS NULL ) THEN
      --CALL arr_detalles.deleteElement(arr_detalles.getLength())
   --END IF


   DIALOG ATTRIBUTES (UNBUFFERED)
      DISPLAY ARRAY arr_detalles TO scr_detalles.*
         BEFORE ROW
         CALL arr_detalles_hist.clear()

         DECLARE cur_historico CURSOR FOR SELECT a.id_usuario_ef,
                                                 a.rfc,
                                                 b.ent_financiera_desc,
                                                 a.nombre,
                                                 a.primer_apellido,
                                                 a.segundo_apellido,
                                                 a.curp,
                                                 a.email,
                                                 a.f_modifica,
                                                 a.estado,
                                                 "",
                                                 a.usuario_modifica
                                          FROM   cat_his_usuario_ef a,
                                                 cat_entidad_financiera b
                                          WHERE  b.cve_ent_financiera = a.cve_ent_financiera
                                          AND    a.id_usuario_ef = arr_detalles[ARR_CURR()].v_id_usuario_ef
                                          AND    a.estado <> 10

         LET j = 1

         FOREACH cur_historico INTO arr_detalles_hist[j].*
            IF arr_detalles_hist[j].v_estado = 10 THEN
               LET arr_detalles_hist[j].v_estado_desc = "10 - ACTIVO"
            ELSE
               LET arr_detalles_hist[j].v_estado_desc = "20 - INACTIVO"
            END IF
       
            LET j = j + 1
         END FOREACH

         FREE cur_historico

         {IF ( arr_detalles_hist[arr_detalles_hist.getLength()].v_modulo_cod IS NULL ) THEN
            CALL arr_detalles_hist.deleteElement(arr_detalles_hist.getLength())
         END IF}
         
         ON ACTION nuevo
            CALL fn_reactiva_rfc(v_usuario)
            RETURNING v_nuevo, v_rfc

            IF v_nuevo = 1 THEN
               CALL fn_ventana_confirma ("Atención", "El RFC " || v_rfc || " capturado no existe " || "\n ¿Desea darlo de alta? ", "question")
               RETURNING v_confirma -- 1-confirma, 0-Cancela

               IF v_confirma = 1 THEN
                  CALL fn_crear_usuarios(v_usuario, v_rfc) --v_usuario,p_usuario_alta, v_usuario_cod,v_correo_e, i)
                  CALL ui.Interface.refresh()
               END IF
            END IF

         ON ACTION modificar
            CALL fn_modificar_usuario(arr_detalles[ARR_CURR()].v_id_usuario_ef, 
                                      arr_detalles[ARR_CURR()].v_rfc,
                                      v_usuario)
            CALL ui.Interface.refresh()

         ON ACTION eliminar
            CALL fn_eliminar_usuario(arr_detalles[ARR_CURR()].v_id_usuario_ef, 
                                     arr_detalles[ARR_CURR()].v_rfc,
                                     v_usuario)
            CALL ui.Interface.refresh()

         ON ACTION reporte
            CALL fn_genera_reporte_pdf()

         ON ACTION CLOSE
            EXIT DIALOG

      END DISPLAY

      DISPLAY ARRAY arr_detalles_hist TO scr_his_detalles.*
      END DISPLAY 
   END DIALOG
END FUNCTION

#OBJETIVO: Dar de alta un usuario nuevo 
FUNCTION fn_crear_usuarios(p_usuario_cod, v_rfc) --v_usuario,p_usuario_alta, v_usuario_cod,v_correo_e,i)
DEFINE v_existe_rfc      CHAR(13),
       v_existe_edo      SMALLINT,
       v_existe_id_usr   DECIMAL(9,0),
       f_ventana         ui.Window,
       f_forma           ui.Form,
       v_l_curp          STRING,
       v_l_rfc           STRING,
       v_confirma        SMALLINT,
       p_usuario_cod     CHAR(20),

       v_id_usuario_ef    DECIMAL(9,0),
       v_rfc              CHAR(13),
       v_nombre           CHAR(50),
       v_primer_apellido  CHAR(50),
       v_segundo_apellido CHAR(13),
       v_curp             CHAR(18),
       v_correo_electronico CHAR(50),
       v_ent_financiera   SMALLINT,
       v_fecha_registro   DATE,
       v_estado           SMALLINT,
       v_usuario_externo  CHAR(20),
       v_bnd_campo        SMALLINT

      LET f_ventana = ui.Window.getCurrent()
      LET f_forma = f_ventana.getForm()

   OPEN WINDOW vtn_crear WITH FORM "GRTM021"

      LET v_fecha_registro = TODAY
      DISPLAY BY NAME v_fecha_registro, v_rfc
    
      INPUT BY NAME --v_rfc,
                    v_nombre,
                    v_primer_apellido,
                    v_segundo_apellido,
                    v_curp,
                    v_correo_electronico,
                    v_ent_financiera,
                    v_usuario_externo,
                    v_fecha_registro
      WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED) 

      BEFORE INPUT 
         CALL fn_llena_combo_entidad_financiera()

      ON ACTION ACCEPT 
         IF (v_rfc IS NULL) 
            OR (v_nombre IS NULL) 
            OR (v_primer_apellido IS NULL) 
            OR (v_curp IS NULL) 
            OR (v_correo_electronico IS NULL) 
            OR (v_ent_financiera IS NULL)
            OR (v_usuario_externo IS NULL)
            OR (v_fecha_registro IS NULL) THEN
            CALL fn_mensaje("Atención", "Es necesario llenar todos los campos", "cancel2")
         ELSE
            INITIALIZE v_existe_rfc TO NULL
            SELECT rfc, estado, id_usuario_ef
            INTO   v_existe_rfc,
                   v_existe_edo,
                   v_existe_id_usr
            FROM   cat_usuario_ef
            WHERE  rfc = v_rfc

            -- Valida campo Usuario externo
            LET v_bnd_campo = fn_valida_campo(v_usuario_externo)
            IF(v_bnd_campo = 1) THEN
               -- No continúa con el alta
               CALL fn_mensaje("","El usuario externo no debe contener caracteres especiales","")
               NEXT FIELD v_usuario_externo 
            END IF

            IF v_existe_rfc IS NULL THEN
               LET v_l_curp = v_curp CLIPPED
               LET v_l_rfc = v_rfc CLIPPED

               IF v_l_curp.getLength() = 18 AND v_l_rfc.getLength() = 13 THEN
                  SELECT seq_cat_usuario_ef.NEXTVAL
                  INTO   v_id_usuario_ef
                  FROM   systables
                  WHERE  tabname = "cat_usuario_ef"

                  LET v_estado = 10

                  CALL fn_valida_usuario_cpb(v_rfc,
                                             v_nombre,
                                             v_primer_apellido,
                                             v_segundo_apellido,
                                             v_curp,
                                             v_correo_electronico,
                                             p_usuario_cod)

                  INSERT INTO cat_usuario_ef (id_usuario_ef,
                                              rfc,
                                              cve_ent_financiera,
                                              nombre,
                                              primer_apellido,
                                              segundo_apellido,
                                              curp,
                                              email,
                                              f_registro,
                                              f_baja,
                                              estado,
                                              usuario,
                                              usuario_cod)
                                      VALUES (v_id_usuario_ef,
                                              v_rfc,
                                              v_ent_financiera,
                                              v_nombre,
                                              v_primer_apellido,
                                              v_segundo_apellido,
                                              v_curp,
                                              v_correo_electronico,
                                              v_fecha_registro,
                                              NULL,
                                              v_estado,
                                              p_usuario_cod,
                                              v_usuario_externo)

                  CALL fn_mensaje("Atención", "El usuario se ha registrado exitosamente", "ok2")
                  EXIT INPUT               
               ELSE 
                  CALL fn_mensaje("Atención", "CURP y/o RFC no validos", "about")
                  NEXT FIELD v_curp
               END IF 
            ELSE
               {IF v_existe_edo = 20 THEN
                  CALL fn_ventana_confirma ("Atención", "El RFC " || v_rfc || "capturado ya existe con estado inactivo " || "\n ¿Desea reactivarlo? ", "question")
                  RETURNING v_confirma -- 1-confirma, 0-Cancela

                  IF v_confirma = 1 THEN

                     UPDATE cat_usuario_ef 
                     SET    estado = 10,
                            f_baja = TODAY
                     WHERE id_usuario_ef = v_existe_id_usr
                     
                     CALL fn_modificar_usuario(v_existe_id_usr,
                                               v_existe_rfc,
                                               p_usuario_cod)

                     CALL fn_mensaje ("Atención", "El usuario se ha reactivado exitosamente "|| v_rfc, "atention")
                  END IF
               ELSE
                  CALL fn_mensaje("Atención", "El RFC capturada ya existe y se encuentra activo \n Verifique la información.", "about")
               END IF
               NEXT FIELD v_rfc}
            END IF
         END IF
      ON ACTION CANCEL
         EXIT INPUT
      END INPUT
   CLOSE WINDOW vtn_crear
END FUNCTION

#OBJETIVO: Llenar el combo de las entidades financieras
FUNCTION fn_llena_combo_entidad_financiera()
DEFINE v_cmbx       ui.ComboBox,
       i            SMALLINT
DEFINE arr_ent_financiera DYNAMIC ARRAY OF RECORD
         cve_ent_financiera  SMALLINT,
         ent_financiera_desc CHAR(50)
END RECORD 

   LET v_cmbx = ui.ComboBox.forName("v_ent_financiera")
   
   LET i = 1

   CALL v_cmbx.clear()

   DECLARE cur_ent_fin CURSOR FOR SELECT cve_ent_financiera,
                                         ent_financiera_desc
                                  FROM   cat_entidad_financiera
                                  ORDER BY cve_ent_financiera
   FOREACH cur_ent_fin INTO arr_ent_financiera[i].*

         CALL v_cmbx.addItem(arr_ent_financiera[i].cve_ent_financiera,
                             arr_ent_financiera[i].cve_ent_financiera||'-'|| arr_ent_financiera[i].ent_financiera_desc)

         LET i = i + 1
      END FOREACH 

   CALL arr_ent_financiera.deleteElement(i)

END FUNCTION

#OBJETIVO: Hacer la baja lógica del usuario 
FUNCTION fn_eliminar_usuario(p_id_usuario_ef, p_rfc, p_usuario_cod)
DEFINE p_id_usuario_ef DECIMAL(9,0)
DEFINE p_rfc CHAR(13)
DEFINE p_usuario_cod CHAR(20)
DEFINE v_confirma SMALLINT
DEFINE v_id_usuario_ef DECIMAL(9,0), 
       v_rfc CHAR(13),
       v_ent_financiera SMALLINT,
       v_nombre CHAR(50),
       v_primer_apellido CHAR(50),
       v_segundo_apellido CHAR(50),
       v_curp CHAR(18),
       v_email CHAR(50),
       v_estado SMALLINT

DISPLAY "ELIMINAR USUARIO : ", p_id_usuario_ef, " - ", p_rfc
   CALL fn_ventana_confirma ("Atención", "Se eliminará la información del usuario "|| p_rfc || "\n ¿Desea continuar? ", "question")
   RETURNING v_confirma -- 1-confirma, 0-Cancela

   IF v_confirma = 1 THEN
      SELECT a.id_usuario_ef,
             a.rfc,
             a.cve_ent_financiera,
             a.nombre,
             a.primer_apellido,
             a.segundo_apellido, 
             a.curp,
             a.email,
             a.estado
      INTO   v_id_usuario_ef, 
             v_rfc,
             v_ent_financiera,
             v_nombre,
             v_primer_apellido,
             v_segundo_apellido,
             v_curp,
             v_email,
             v_estado
      FROM   cat_usuario_ef a
      WHERE  a.id_usuario_ef = p_id_usuario_ef
      AND    a.estado = 10

      --Se almacena registro en tabla histórica
      INSERT INTO cat_his_usuario_ef (id_usuario_ef,
                                      rfc,
                                      cve_ent_financiera,
                                      nombre,
                                      primer_apellido,
                                      segundo_apellido,
                                      curp,
                                      email,
                                      f_modifica,
                                      estado,
                                      usuario_modifica)
                              VALUES (p_id_usuario_ef,
                                      v_rfc,
                                      v_ent_financiera,
                                      v_nombre,
                                      v_primer_apellido,
                                      v_segundo_apellido,
                                      v_curp,
                                      v_email,
                                      TODAY,
                                      v_estado,
                                      p_usuario_cod)

      --Se hace baja lógica estado 20
      UPDATE cat_usuario_ef 
      SET    estado = 20,
             f_baja = TODAY
      WHERE id_usuario_ef = p_id_usuario_ef
      
      CALL fn_mensaje ("Atención", "Se ha eliminado la información del usuario "|| p_rfc, "atention")
   END IF

END FUNCTION

#OBJETIVO: Modificar la información de algún usuario.
FUNCTION fn_modificar_usuario(p_id_usuario_ef, p_rfc, p_usuario_cod)
DEFINE p_id_usuario_ef DECIMAL(9,0)
DEFINE p_rfc CHAR(13)
DEFINE p_usuario_cod CHAR(20)
DEFINE v_confirma SMALLINT
DEFINE v_existe_rfc      CHAR(13),
       f_ventana         ui.Window,
       f_forma           ui.Form,
       v_l_curp          STRING,
       v_l_rfc           STRING,

       v_id_usuario_ef    DECIMAL(9,0),
       v_m_rfc              CHAR(13),
       v_m_nombre           CHAR(50),
       v_m_primer_apellido  CHAR(50),
       v_m_segundo_apellido CHAR(13),
       v_m_curp             CHAR(18),
       v_m_correo_electronico CHAR(50),
       v_m_ent_financiera   SMALLINT,
       v_m_usuario_externo  CHAR(20),

       v_rfc              CHAR(13),
       v_nombre           CHAR(50),
       v_primer_apellido  CHAR(50),
       v_segundo_apellido CHAR(50),
       v_curp             CHAR(18),
       v_correo_electronico CHAR(50),
       v_ent_financiera   INTEGER,
       v_fecha_registro   DATE,
       v_estado           SMALLINT       
      LET f_ventana = ui.Window.getCurrent()
      LET f_forma = f_ventana.getForm()
DISPLAY "MODIFICAR USUARIO : ", p_id_usuario_ef, " - ", p_rfc
   
   CALL fn_ventana_confirma ("Atención", "Se modificará la información del usuario "|| p_rfc || "\n ¿Desea continuar? ", "question")
   RETURNING v_confirma -- 1-confirma, 0-Cancela

   IF v_confirma = 1 THEN
      OPEN WINDOW vtn_modifica WITH FORM "GRTM021"
       --Nombre, primer apellido, segundo apellido, CURP, Entidad Financiera
         INPUT  v_m_nombre,
                v_m_primer_apellido,
                v_m_segundo_apellido,
                v_m_curp,
                v_m_correo_electronico,
                v_m_ent_financiera
         WITHOUT DEFAULTS
         FROM   v_nombre,
                v_primer_apellido,
                v_segundo_apellido,
                v_curp,
                v_correo_electronico,
                v_ent_financiera
         ATTRIBUTES (UNBUFFERED)

         BEFORE INPUT 
            CALL fn_llena_combo_entidad_financiera()
            CALL fn_consulta_datos_rfc(p_id_usuario_ef, 10)
            RETURNING v_rfc,  
                      v_nombre,
                      v_primer_apellido,
                      v_segundo_apellido,
                      v_curp,
                      v_correo_electronico,
                      v_ent_financiera,
                      v_fecha_registro,
                      v_estado,
                      v_m_usuario_externo

            LET v_m_nombre = v_nombre
            LET v_m_primer_apellido = v_primer_apellido
            LET v_m_segundo_apellido = v_segundo_apellido
            LET v_m_curp = v_curp
            LET v_m_correo_electronico = v_correo_electronico
            LET v_m_ent_financiera = v_ent_financiera

            DISPLAY v_rfc TO v_rfc
            DISPLAY v_fecha_registro TO v_fecha_registro
            DISPLAY v_m_nombre TO v_nombre
            DISPLAY v_m_primer_apellido TO v_primer_apellido
            DISPLAY v_m_segundo_apellido TO v_segundo_apellido
            DISPLAY v_m_curp TO v_curp
            DISPLAY v_m_correo_electronico TO v_correo_electronico
            DISPLAY v_m_ent_financiera TO v_ent_financiera
            DISPLAY v_m_usuario_externo TO v_usuario_externo

         ON ACTION ACCEPT
            IF (v_m_nombre IS NULL) 
               OR (v_m_primer_apellido IS NULL) 
               OR (v_m_curp IS NULL) 
               OR (v_m_correo_electronico IS NULL) 
               OR (v_m_ent_financiera IS NULL) THEN
               CALL fn_mensaje("Atención", "Es necesario llenar todos los campos", "cancel2")

            ELSE
               LET v_l_curp = v_m_curp CLIPPED
                  IF v_l_curp.getLength() = 18 THEN

                     LET v_estado = 30

                     INSERT INTO cat_his_usuario_ef (id_usuario_ef,
                                                     rfc,
                                                     cve_ent_financiera,
                                                     nombre,
                                                     primer_apellido,
                                                     segundo_apellido,
                                                     curp,
                                                     email,
                                                     f_modifica,
                                                     estado,
                                                     usuario_modifica)
                                             VALUES (p_id_usuario_ef,
                                                     v_rfc,
                                                     v_ent_financiera,
                                                     v_nombre,
                                                     v_primer_apellido,
                                                     v_segundo_apellido,
                                                     v_curp,
                                                     v_correo_electronico,
                                                     TODAY,
                                                     v_estado,
                                                     p_usuario_cod
                                                     )

                     UPDATE cat_usuario_ef
                     SET cve_ent_financiera = v_m_ent_financiera,
                         nombre = v_m_nombre,
                         primer_apellido = v_m_primer_apellido,
                         segundo_apellido = v_m_segundo_apellido,
                         curp =  v_m_curp,
                         email = v_m_correo_electronico
                     WHERE id_usuario_ef = p_id_usuario_ef

                     CALL fn_mensaje("Atención", "El usuario se ha modificado exitosamente", "ok2")
                     EXIT INPUT               
                  ELSE 
                     CALL fn_mensaje("Atención", "CURP no valida", "about")
                     NEXT FIELD v_curp
                  END IF 
            END IF
         ON ACTION CANCEL
            EXIT INPUT
         END INPUT
      
      END IF
   CLOSE WINDOW vtn_modifica

END FUNCTION

#OBJETIVO:  Consultar la información del RFC capturado
FUNCTION fn_consulta_datos_rfc(p_id_usuario_ef, p_estado)
DEFINE p_id_usuario_ef    DECIMAL(9,0),
       p_estado           SMALLINT,
       v_rfc              CHAR(13),  
       v_nombre           CHAR(50),
       v_primer_apellido  CHAR(50),
       v_segundo_apellido CHAR(50),
       v_curp             CHAR(18),
       v_correo_electronico CHAR(50),
       v_ent_financiera   SMALLINT,
       v_fecha_registro   DATE,
       v_fecha_baja       DATE,
       v_estado           SMALLINT,
       v_r_usuario_cod    CHAR(20)
DISPLAY "CONSULTA DATOS : ", p_id_usuario_ef
   SELECT a.rfc,
          a.cve_ent_financiera,
          a.nombre,
          a.primer_apellido,
          a.segundo_apellido,
          a.curp,
          a.email,
          a.f_registro,
          a.f_baja,
          a.estado,
          a.usuario_cod
   INTO   v_rfc,
          v_ent_financiera,
          v_nombre,
          v_primer_apellido,
          v_segundo_apellido, 
          v_curp,
          v_correo_electronico,
          v_fecha_registro,
          v_fecha_baja,
          v_estado,
          v_r_usuario_cod
   FROM   cat_usuario_ef a
   WHERE  a.id_usuario_ef = p_id_usuario_ef
   AND    a.estado = p_estado

   RETURN v_rfc,
          v_nombre,
          v_primer_apellido,
          v_segundo_apellido,
          v_curp,
          v_correo_electronico,
          v_ent_financiera,
          v_fecha_registro,
          v_estado,
          v_r_usuario_cod
END FUNCTION 

{
FUNCTION fn_genera_reporte(p_usuario)
DEFINE p_usuario CHAR(20)
DEFINE manejador_rpt om.SaxDocumentHandler
DEFINE v_ini SMALLINT
DEFINE i     INTEGER
DEFINE j     INTEGER
DEFINE k     INTEGER

DEFINE arr_det_rpt_acept DYNAMIC ARRAY OF RECORD
          v_a_id_usuario_ef       DECIMAL(9,0),
          v_a_rfc                 CHAR(13),
          v_a_ent_financiera_desc CHAR(65),
          v_a_nombre              CHAR(50),
          v_a_primer_apellido     CHAR(50),
          v_a_segundo_apellido    CHAR(50), 
          v_a_curp                CHAR(18),
          v_a_email               CHAR(50),
          v_a_f_registro          DATE,
          v_a_f_baja              DATE,
          v_a_estado              SMALLINT
END RECORD
DEFINE arr_det_rpt_rch DYNAMIC ARRAY OF RECORD
          v_r_id_usuario_ef       DECIMAL(9,0),
          v_r_rfc                 CHAR(13),
          v_r_ent_financiera_desc CHAR(65),
          v_r_nombre              CHAR(50),
          v_r_primer_apellido     CHAR(50),
          v_r_segundo_apellido    CHAR(50), 
          v_r_curp                CHAR(18),
          v_r_email               CHAR(50),
          v_r_f_registro          DATE,
          v_r_f_baja              DATE,
          v_r_estado              SMALLINT
END RECORD

   DECLARE cur_aceptados CURSOR FOR SELECT a.id_usuario_ef,
                                                 a.rfc,
                                                 b.ent_financiera_desc,
                                                 a.nombre,
                                                 a.primer_apellido,
                                                 a.segundo_apellido,
                                                 a.curp,
                                                 a.email,
                                                 a.f_registro,
                                                 "",
                                                 a.estado
                                          FROM   cat_usuario_ef a,
                                                 cat_entidad_financiera b
                                          WHERE  b.cve_ent_financiera = a.cve_ent_financiera
                                          AND    a.estado = 10
   LET i = 1

   FOREACH cur_aceptados INTO arr_det_rpt_acept[i].*   
      LET i = i + 1
   END FOREACH

   DECLARE cur_rechazados CURSOR FOR SELECT a.id_usuario_ef,
                                                 a.rfc,
                                                 b.ent_financiera_desc,
                                                 a.nombre,
                                                 a.primer_apellido,
                                                 a.segundo_apellido,
                                                 a.curp,
                                                 a.email,
                                                 "",
                                                 a.f_baja,
                                                 a.estado
                                          FROM   cat_usuario_ef a,
                                                 cat_entidad_financiera b
                                          WHERE  b.cve_ent_financiera = a.cve_ent_financiera
                                          AND    a.estado = 20

   FOREACH cur_rechazados INTO arr_det_rpt_rch[i].*   
      LET i = i + 1
   END FOREACH

         IF fgl_report_loadCurrentSettings("GRTM023.4rp") THEN
            CALL fgl_report_selectDevice ("PDF")
            CALL fgl_report_selectPreview(1)

            LET manejador_rpt = fgl_report_commitCurrentSettings()
         END IF

         --Inicia el reporte de detalles
         START REPORT rpt_detalles TO XML HANDLER manejador_rpt
            FOR v_ini = 1 TO i
               OUTPUT TO REPORT rpt_detalles (p_usuario,
                                              arr_det_rpt_acept[v_ini].*,
                                              arr_det_rpt_rch[v_ini].*
                                              )
            END FOR
         FINISH REPORT rpt_detalles
END FUNCTION

REPORT rpt_detalles(p_usuario, arr_det_rpt_act, arr_det_rpt_inact)
DEFINE p_usuario CHAR(20),
       v_fecha_reporte DATE,
       v_a_estado_desc CHAR(15),
       v_r_estado_desc CHAR(15)

DEFINE arr_det_rpt_act RECORD
          v_a_id_usuario_ef       DECIMAL(9,0),
          v_a_rfc                 CHAR(13),
          v_a_ent_financiera_desc CHAR(65),
          v_a_nombre              CHAR(50),
          v_a_primer_apellido     CHAR(50),
          v_a_segundo_apellido    CHAR(50), 
          v_a_curp                CHAR(18),
          v_a_email               CHAR(50),
          v_a_f_registro          DATE,
          v_a_f_baja              DATE,
          v_a_estado              SMALLINT
END RECORD
DEFINE arr_det_rpt_inact RECORD
          v_r_id_usuario_ef       DECIMAL(9,0),
          v_r_rfc                 CHAR(13),
          v_r_ent_financiera_desc CHAR(65),
          v_r_nombre              CHAR(50),
          v_r_primer_apellido     CHAR(50),
          v_r_segundo_apellido    CHAR(50), 
          v_r_curp                CHAR(18),
          v_r_email               CHAR(50),
          v_r_f_registro          DATE,
          v_r_f_baja              DATE,
          v_r_estado              SMALLINT
END RECORD

FORMAT
   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY CLIPPED

      PRINTX v_fecha_reporte USING "DD-MM-YYYY"
      PRINTX p_usuario


   ON EVERY ROW
      IF arr_det_rpt_act.v_a_estado = 10 THEN
         LET v_a_estado_desc = "ACTIVO"
      END IF

      IF arr_det_rpt_inact.v_r_estado = 20 THEN
         LET v_a_estado_desc = "INACTIVO"
      END IF

      PRINTX arr_det_rpt_act.v_a_rfc
      PRINTX arr_det_rpt_act.v_a_ent_financiera_desc
      PRINTX arr_det_rpt_act.v_a_nombre
      PRINTX arr_det_rpt_act.v_a_primer_apellido
      PRINTX arr_det_rpt_act.v_a_segundo_apellido
      PRINTX arr_det_rpt_act.v_a_curp
      PRINTX v_a_estado_desc
      PRINTX arr_det_rpt_act.v_a_f_registro  USING "DD-MM-YYYY"

      PRINTX arr_det_rpt_inact.v_r_rfc
      PRINTX arr_det_rpt_inact.v_r_ent_financiera_desc
      PRINTX arr_det_rpt_inact.v_r_nombre
      PRINTX arr_det_rpt_inact.v_r_primer_apellido
      PRINTX arr_det_rpt_inact.v_r_segundo_apellido
      PRINTX arr_det_rpt_inact.v_r_curp
      PRINTX v_r_estado_desc
      PRINTX arr_det_rpt_inact.v_r_f_baja  USING "DD-MM-YYYY"

END REPORT
}
FUNCTION fn_genera_reporte_pdf()

   DEFINE v_k        INTEGER
   DEFINE object_rpt om.SaxDocumentHandler
   DEFINE v_aux_cve_ent_financiera  SMALLINT
   DEFINE v_aux_ent_financiera_desc CHAR(60)
   DEFINE v_aux_estado     SMALLINT

   DECLARE crs_usuarios_ef CURSOR FOR
   SELECT us.cve_ent_financiera,
          ef.ent_financiera_desc,
          us.rfc,
          us.curp,
          us.nombre,
          us.primer_apellido,
          us.segundo_apellido,
          us.f_registro,
          us.f_baja,
          us.estado
     FROM cat_usuario_ef us,
          cat_entidad_financiera ef
    WHERE us.cve_ent_financiera = ef.cve_ent_financiera
      AND us.estado IN (10,20)
      ORDER BY 1,9

   CALL g_arr_det_rpt.clear()

   LET v_aux_cve_ent_financiera  = NULL
   LET v_aux_ent_financiera_desc = NULL
   LET v_aux_estado     = NULL
   
   LET v_k = 1

   FOREACH crs_usuarios_ef INTO v_aux_cve_ent_financiera,
                                v_aux_ent_financiera_desc,
                                g_arr_det_rpt[v_k].rfc,
                                g_arr_det_rpt[v_k].curp,
                                g_arr_det_rpt[v_k].nombre,
                                g_arr_det_rpt[v_k].primer_apellido,
                                g_arr_det_rpt[v_k].segundo_apellido,
                                g_arr_det_rpt[v_k].f_registro,
                                g_arr_det_rpt[v_k].f_baja,
                                v_aux_estado

      -- Concatena EF
      LET g_arr_det_rpt[v_k].entidad_financiera = v_aux_cve_ent_financiera CLIPPED,"-",v_aux_ent_financiera_desc CLIPPED

      IF(v_aux_estado = 10) THEN
         LET g_arr_det_rpt[v_k].f_baja  = NULL
         LET g_arr_det_rpt[v_k].estatus = "ACTIVO  "
      ELSE
         LET g_arr_det_rpt[v_k].estatus   = "INACTIVO"
      END IF 
   
      LET v_k = v_k + 1

   END FOREACH

   --> CONFIGURACIÓN DE LA SALIDA REPORTE PDF <--
   IF fgl_report_loadCurrentSettings("GRTM023.4rp") THEN
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(1)
      LET object_rpt = fgl_report_commitCurrentSettings()
   END IF

   IF (object_rpt IS NOT NULL) THEN
      START REPORT reporte_pdf TO XML HANDLER object_rpt
         OUTPUT TO REPORT reporte_pdf()
      FINISH REPORT reporte_pdf
   END IF
   
END FUNCTION

REPORT reporte_pdf()

   DEFINE v_f         INTEGER
   DEFINE v_f_proceso DATE
   
   FORMAT
      FIRST PAGE HEADER
         LET v_f_proceso = TODAY

         PRINTX v_f_proceso USING "dd/mm/yyyy"
         PRINTX g_usuario

      ON EVERY ROW
         FOR v_f = 1 TO g_arr_det_rpt.getLength()
            PRINTX g_arr_det_rpt[v_f].entidad_financiera
            PRINTX g_arr_det_rpt[v_f].rfc
            PRINTX g_arr_det_rpt[v_f].curp
            PRINTX g_arr_det_rpt[v_f].nombre
            PRINTX g_arr_det_rpt[v_f].primer_apellido
            PRINTX g_arr_det_rpt[v_f].segundo_apellido
            PRINTX g_arr_det_rpt[v_f].f_registro USING "dd/mm/yyyy"
            PRINTX g_arr_det_rpt[v_f].f_baja     USING "dd/mm/yyyy"
            PRINTX g_arr_det_rpt[v_f].estatus
         END FOR 

END REPORT 

FUNCTION fn_valida_campo(p_cadena)

   DEFINE p_cadena   STRING
   DEFINE v_idx      INTEGER
   DEFINE v_r_ind    BOOLEAN

   LET p_cadena = p_cadena CLIPPED
   LET v_r_ind  = 0 -- Procedente

   FOR v_idx = 1 TO p_cadena.getLength()
      IF(p_cadena.subString(v_idx,v_idx) MATCHES '[0-9]') OR 
        (p_cadena.subString(v_idx,v_idx) MATCHES '[A-Z]') THEN
         LET v_r_ind = 0
      ELSE
         LET v_r_ind = 1
         EXIT FOR
      END IF
   END FOR

   RETURN v_r_ind

END FUNCTION

#OBJETIVO: Validar la existencia del RFC en la base de datos, si existe reactivarlo
FUNCTION fn_reactiva_rfc(p_usuario_cod)
DEFINE p_usuario_cod  CHAR(20)
DEFINE v_rfc_consulta CHAR(13)
DEFINE v_existe_rfc    CHAR(13),
       v_existe_edo    SMALLINT,
       v_existe_id_usr DECIMAL(9,0),
       f_ventana       ui.Window,
       f_forma         ui.Form,
       v_confirma      SMALLINT,
       v_nuevo         SMALLINT,
       v_id_usuario_ef    DECIMAL(9,0),
       v_m_rfc              CHAR(13),
       v_m_nombre           CHAR(50),
       v_m_primer_apellido  CHAR(50),
       v_m_segundo_apellido CHAR(13),
       v_m_curp             CHAR(18),
       v_m_correo_electronico CHAR(50),
       v_m_ent_financiera   SMALLINT,
       v_m_usuario_externo  CHAR(20),

       v_rfc              CHAR(13),
       v_nombre           CHAR(50),
       v_primer_apellido  CHAR(50),
       v_segundo_apellido CHAR(50),
       v_curp             CHAR(18),
       v_correo_electronico CHAR(50),
       v_ent_financiera   INTEGER,
       v_fecha_registro   DATE,
       v_estado           SMALLINT,
       v_l_curp           STRING

   LET f_ventana = ui.Window.getCurrent()
   LET f_forma = f_ventana.getForm()
       
   OPEN WINDOW vtn_consulta_rfc WITH FORM "GRTM024"
      INPUT BY NAME v_rfc_consulta
      WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED)  

         ON ACTION ACCEPT 
            IF (v_rfc_consulta IS NULL) THEN
               CALL fn_mensaje("Atención", "Es necesario capturar el RFC", "cancel")
            ELSE
               INITIALIZE v_existe_rfc TO NULL
               SELECT rfc, estado, id_usuario_ef
               INTO   v_existe_rfc,
                      v_existe_edo,
                      v_existe_id_usr
               FROM   cat_usuario_ef
               WHERE  rfc = v_rfc_consulta

               IF v_existe_rfc IS NULL THEN
                  LET v_nuevo = 1
                  EXIT INPUT
               ELSE
                  LET v_nuevo = 0

                  IF v_existe_edo = 20 THEN
                     CALL fn_ventana_confirma ("Atención", "El RFC " || v_existe_rfc || " capturado ya existe con estado inactivo " || "\n ¿Desea reactivarlo? ", "question")
                     RETURNING v_confirma -- 1-confirma, 0-Cancela

                     IF v_confirma = 1 THEN
                        OPEN WINDOW vtn_modifica WITH FORM "GRTM021"
                           --Nombre, primer apellido, segundo apellido, CURP, Entidad Financiera
                           INPUT  v_m_nombre,
                                  v_m_primer_apellido,
                                  v_m_segundo_apellido,
                                  v_m_curp,
                                  v_m_correo_electronico,
                                  v_m_ent_financiera
                           WITHOUT DEFAULTS
                           FROM   v_nombre,
                                  v_primer_apellido,
                                  v_segundo_apellido,
                                  v_curp,
                                  v_correo_electronico,
                                  v_ent_financiera
                           ATTRIBUTES (UNBUFFERED)

                           BEFORE INPUT 
                              CALL fn_llena_combo_entidad_financiera()
                              CALL fn_consulta_datos_rfc(v_existe_id_usr, 20)
                              RETURNING v_rfc,  
                                        v_nombre,
                                        v_primer_apellido,
                                        v_segundo_apellido,
                                        v_curp,
                                        v_correo_electronico,
                                        v_ent_financiera,
                                        v_fecha_registro,
                                        v_estado,
                                        v_m_usuario_externo

                              LET v_m_nombre = v_nombre
                              LET v_m_primer_apellido = v_primer_apellido
                              LET v_m_segundo_apellido = v_segundo_apellido
                              LET v_m_curp = v_curp
                              LET v_m_correo_electronico = v_correo_electronico
                              LET v_m_ent_financiera = v_ent_financiera

                              DISPLAY v_rfc TO v_rfc
                              DISPLAY v_fecha_registro TO v_fecha_registro
                              DISPLAY v_m_nombre TO v_nombre
                              DISPLAY v_m_primer_apellido TO v_primer_apellido
                              DISPLAY v_m_segundo_apellido TO v_segundo_apellido
                              DISPLAY v_m_curp TO v_curp
                              DISPLAY v_m_correo_electronico TO v_correo_electronico
                              DISPLAY v_m_ent_financiera TO v_ent_financiera
                              DISPLAY v_m_usuario_externo TO v_usuario_externo

                           ON ACTION ACCEPT
                              IF (v_m_nombre IS NULL) 
                                 OR (v_m_primer_apellido IS NULL) 
                                 OR (v_m_curp IS NULL) 
                                 OR (v_m_correo_electronico IS NULL) 
                                 OR (v_m_ent_financiera IS NULL) THEN
                                 CALL fn_mensaje("Atención", "Es necesario llenar todos los campos", "cancel2")

                              ELSE
                                 LET v_l_curp = v_m_curp CLIPPED
                                    IF v_l_curp.getLength() = 18 THEN

                                       LET v_estado = 30

                                       INSERT INTO cat_his_usuario_ef (id_usuario_ef,
                                                                       rfc,
                                                                       cve_ent_financiera,
                                                                       nombre,
                                                                       primer_apellido,
                                                                       segundo_apellido,
                                                                       curp,
                                                                       email,
                                                                       f_modifica,
                                                                       estado,
                                                                       usuario_modifica)
                                                               VALUES (v_existe_id_usr,
                                                                       v_rfc,
                                                                       v_ent_financiera,
                                                                       v_nombre,
                                                                       v_primer_apellido,
                                                                       v_segundo_apellido,
                                                                       v_curp,
                                                                       v_correo_electronico,
                                                                       TODAY,
                                                                       v_estado,
                                                                       p_usuario_cod
                                                                       )

                                       UPDATE cat_usuario_ef
                                       SET cve_ent_financiera = v_m_ent_financiera,
                                           nombre = v_m_nombre,
                                           primer_apellido = v_m_primer_apellido,
                                           segundo_apellido = v_m_segundo_apellido,
                                           curp =  v_m_curp,
                                           email = v_m_correo_electronico,
                                           estado = 10
                                       WHERE id_usuario_ef = v_existe_id_usr

                                       CALL fn_mensaje ("Atención", "El usuario se ha reactivado exitosamente "|| v_existe_rfc, "atention")
                                       EXIT INPUT
                                    ELSE 
                                       CALL fn_mensaje("Atención", "CURP no valida", "about")
                                       NEXT FIELD v_curp
                                    END IF 
                              END IF
                           ON ACTION CANCEL
                              EXIT INPUT
                           END INPUT
                        CLOSE WINDOW vtn_modifica
                     END IF
                  ELSE
                     IF v_existe_edo = 10 THEN
                        CALL fn_mensaje("Atención", "El RFC capturada ya existe y se encuentra activo \n Verifique la información.", "about")
                        EXIT INPUT
                     END IF
                  END IF
               END IF
            END IF
            EXIT INPUT
         ON ACTION CANCEL
            EXIT INPUT
      END INPUT
   CLOSE WINDOW vtn_consulta_rfc 

   RETURN v_nuevo, 
          v_rfc_consulta
END FUNCTION

#OBJETIVO:  Validar si existe el usuario en la base de datos safre_cpb
FUNCTION fn_valida_usuario_cpb(p_rfc,p_nombre,p_primer_apellido,p_segundo_apellido,
                               p_curp,p_correo_electronico,p_usuario_cod)
DEFINE v_usuario_rfc CHAR(13)
DEFINE p_rfc              CHAR(13),
       p_nombre           CHAR(50),
       p_primer_apellido  CHAR(50),
       p_segundo_apellido CHAR(13),
       p_curp             CHAR(18),
       p_correo_electronico CHAR(50),
       p_usuario_cod        CHAR(20)

DEFINE v_nombre_completo CHAR(120)

   IF p_segundo_apellido IS NOT NULL THEN
      LET v_nombre_completo = p_nombre CLIPPED|| " " ||p_primer_apellido CLIPPED|| " " ||p_segundo_apellido CLIPPED
   ELSE
      LET v_nombre_completo = p_nombre CLIPPED|| " " ||p_primer_apellido CLIPPED
   END IF

DATABASE safre_cpb

   SELECT usuario_cod
   INTO   v_usuario_rfc
   FROM   seg_usuario
   WHERE  usuario_cod = p_rfc

   IF v_usuario_rfc IS NULL THEN
      INSERT INTO seg_usuario (usuario_cod,
                               usuario_desc,
                               correo_e,
                               curp,
                               ind_activo,
                               f_crea,
                               f_alta,
                               usuario
                              )
                      VALUES  (p_rfc,
                               v_nombre_completo,
                               p_correo_electronico,
                               p_curp,
                               1,
                               TODAY,
                               TODAY,
                               p_usuario_cod);

      INSERT INTO seg_usuario_perfil (perfil_cod,
                                      usuario_cod,
                                      f_actualiza,
                                      usuario
                                      )
                              VALUES (0,
                                      p_rfc, 
                                      TODAY,
                                      p_usuario_cod);
   END IF
CLOSE DATABASE

DATABASE safre_viv

END FUNCTION