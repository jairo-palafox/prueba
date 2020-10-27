--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10-04-2012
--===============================================================

####################################################################
#Modulo            =>MDT                                           #
#Programa          =>MDTM03                                        #
#Objetivo          =>Eliminacion de mandatos registrados           #
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>14 Febrero 2012                               #
####################################################################

DATABASE safre_viv

DEFINE v_s_qryTxt        STRING
DEFINE p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
       p_b_tipo_carga    SMALLINT,                -- tipo de carga (1 - modo en linea y 2 - modo batch)
       p_v_nom_prog      VARCHAR(30)              -- nombre del programa
DEFINE cb_mdt            ui.combobox  -- Control de combo dinamico de mandatos
DEFINE cb_tpomdt         ui.combobox  -- Control de combo dinamico de tipos de mandato
DEFINE cmbmdt            LIKE mdt_cat_mandato.id_cat_mandato
DEFINE cmbtpomdt         LIKE mdt_tpo_mandato.tpo_mandato
DEFINE v_arr_mandato     DYNAMIC ARRAY OF RECORD -- arreglo que contiene los mandatos
          etiqueta          LIKE mdt_cat_gpo_etiqueta.etiqueta,
          valor_etiqueta    LIKE mdt_cat_instancia_mandato.valor_etiqueta
       END RECORD
DEFINE v_arr_imagen      DYNAMIC ARRAY OF RECORD -- arreglo que contiene los mandatos
          nombre_imagen     CHAR(200),
          desc_imagen       CHAR(40)
       END RECORD
DEFINE r_not_mdt RECORD --LIKE mdt_notifica_mandato.*
         id_mdt_notifica   LIKE mdt_notifica_mandato.id_cat_mandato,
         id_tpo_mandato    LIKE mdt_notifica_mandato.id_tpo_mandato,
         id_cat_mandato    LIKE mdt_notifica_mandato.id_cat_mandato,
         cve_mandato       LIKE mdt_notifica_mandato.cve_mandato,
         tipo_operacion    LIKE mdt_notifica_mandato.tipo_operacion,
         descripcion1      LIKE mdt_notifica_mandato.descripcion1,
         descripcion2      LIKE mdt_notifica_mandato.descripcion2,
         descripcion3      LIKE mdt_notifica_mandato.descripcion3,
         n_cuenta_bancaria LIKE mdt_notifica_mandato.n_cuenta_bancaria,
         n_convenio        LIKE mdt_notifica_mandato.n_convenio,
         n_referencia      LIKE mdt_notifica_mandato.n_referencia,
         cta_clabe         LIKE mdt_notifica_mandato.cta_clabe,
         estado            LIKE mdt_notifica_mandato.estado,
         resultado_operacion LIKE mdt_notifica_mandato.resultado_operacion,
         diagnostico         LIKE mdt_notifica_mandato.diagnostico,
         f_creacion          LIKE mdt_notifica_mandato.f_creacion
       END RECORD
DEFINE r_mandato RECORD --LIKE mdt_cat_mandato.*
         id_cat_mandato LIKE mdt_cat_mandato.id_cat_mandato,
         cve_mandato    LIKE mdt_notifica_mandato.cve_mandato,
         desc_mandato   LIKE mdt_cat_mandato.desc_mandato,
         desc_larga_mandato LIKE mdt_cat_mandato.desc_larga_mandato,
         usuario            LIKE mdt_cat_mandato.usuario,
         tpo_mandato        LIKE mdt_cat_mandato.tpo_mandato,
         f_creacion         LIKE mdt_cat_mandato.f_creacion,
         estado             LIKE mdt_cat_mandato.estado
       END RECORD
          
DEFINE v_gpo_mandato     LIKE mdt_gpo_mandato.id_gpo_mandato
DEFINE v_s_nota          STRING
DEFINE v_sp_estado       INTEGER,
       v_ventana         ui.Window

MAIN
DEFINE v_consulta       STRING,
       v_bnd_notifica   BOOLEAN,
       v_etiqueta       LIKE mdt_cat_gpo_etiqueta.etiqueta,
       v_valor_etiqueta LIKE mdt_cat_instancia_mandato.valor_etiqueta,
       v_error_sql      BOOLEAN
       
   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog = ARG_VAL(3)
   LET cmbtpomdt = ARG_VAL(4)
   LET cmbmdt = ARG_VAL(5)

   --CALL ui.Interface.loadStyles("mdtstyle")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   IF NOT fn_verifica_privilegios("MDTM03",p_v_usuario) THEN
      CALL fn_mensaje("Advertencia","No cuenta con privilegios para esta opción","error")
      EXIT PROGRAM
   END IF
   
   -- se abre la ventana de consulta
   OPEN WINDOW w_eli_mandato WITH FORM "PRTM121"

   IF(p_v_nom_prog IS NOT NULL)THEN
      CALL ui.Interface.setText(p_v_nom_prog)
      LET v_ventana = ui.Window.getCurrent()
      CALL v_ventana.setText(p_v_nom_prog)
   END IF

   -- Ajuste 20120229 AHM INPUT BY NAME cmbtpomdt, cmbmdt WITHOUT DEFAULTS ATTRIBUTES(UNBUFFERED)

   DIALOG ATTRIBUTES (UNBUFFERED)

      DISPLAY ARRAY v_arr_mandato TO r_atributos_mdt.*
      END DISPLAY

      DISPLAY ARRAY v_arr_imagen TO tbl_imagen.*
      --   ON ACTION imagen
            -- Despliegue de imagen
            
      END DISPLAY

      BEFORE DIALOG
         DISPLAY BY NAME cmbtpomdt, cmbmdt
         -- Inicializa el combo de mandatos
         CALL inicializa_mandatos()
         
         -- Carga el arrerglo de mandatos-detalle por clave seleccionada
         CALL fn_carga_arr_mdt(cmbtpomdt, cmbmdt)
         CALL fn_carga_arr_img()
         -- Despliega arreglo de mandatos-detalle
      
      -- Ajuste 20120229 AHM ON CHANGE cmbtpomdt
      -- Ajuste 20120229 AHM    CALL init_combo_mandato(cmbtpomdt)
      -- Ajuste 20120229 AHM    CALL v_arr_mandato.clear()
      -- Ajuste 20120229 AHM    DISPLAY ARRAY v_arr_mandato TO r_atributos_mdt.* 
      -- Ajuste 20120229 AHM       BEFORE DISPLAY
      -- Ajuste 20120229 AHM          EXIT DISPLAY
      -- Ajuste 20120229 AHM    END DISPLAY
         
      ON ACTION CLOSE -- cancelar
         EXIT DIALOG
      
      ON ACTION eliminar
         IF fn_existe_instruccion_mdt(cmbmdt) THEN
            LET v_s_nota = "\nSe eliminarán las solicitudes relacionadas a este,\ny ."
         ELSE
            LET v_s_nota = " "
         END IF
         
         -- Eliminar el mandato
         
         SELECT desc_entidad,desc_larga_entidad
           INTO r_mandato.desc_mandato,r_mandato.desc_larga_mandato
           FROM prt_cat_entidad
          WHERE id_cat_entidad = cmbmdt
          
         IF fn_ventana_confirma("Confimar","Confirmar eliminación de la entidad: "||r_mandato.desc_mandato||v_s_nota,"info") = 1 THEN
         --IF fgl_winquestion("Confirmación","Confirmar eliminación del mandato: "||r_mandato.desc_mandato||v_s_nota,"no","yes|no", "question", 0) = 'yes' THEN
            LET v_error_sql = FALSE
            INITIALIZE r_not_mdt TO NULL
            -- insert into mdt_notifica_mandato
            SELECT NVL(MAX(id_prt_notifica),0) + 1 INTO r_not_mdt.id_mdt_notifica
              FROM prt_notifica_entidad
            LET r_not_mdt.descripcion1   = r_mandato.desc_larga_mandato
            LET r_not_mdt.tipo_operacion = 'B'
            LET r_not_mdt.estado         = 100

            # Recuepra todas las etiquetas asignadas al mandato
            LET v_consulta = "\n SELECT etq.etiqueta, ins.valor_etiqueta",
                             "\n   FROM prt_cat_atributo_nivel atr JOIN prt_cat_instancia_entidad ins",
                             "\n     ON ins.id_atr_nivel = atr.id_atr_nivel",
                             "\n        JOIN mdt_cat_gpo_etiqueta etq",
                             "\n     ON etq.id_gpo_etiqueta = atr.id_gpo_etiqueta",
                             "\n  WHERE id_cat_entidad = ?"
            PREPARE prp_rec_atributos FROM v_consulta
            DECLARE cur_rec_atributos CURSOR FOR prp_rec_atributos
            FOREACH cur_rec_atributos USING cmbmdt INTO v_etiqueta,
                                                        v_valor_etiqueta
               # Se obtienen solo los campos que nos interesa, para insertar en mdt_notifica_mandato
               CASE v_etiqueta CLIPPED
                  WHEN "NUMERO DE CUENTA"
                    LET r_not_mdt.n_cuenta_bancaria = v_valor_etiqueta
                  WHEN "CONVENIO"
                    LET r_not_mdt.n_convenio = v_valor_etiqueta
                  WHEN "REFERENCIA"
                    LET r_not_mdt.n_referencia = v_valor_etiqueta
                  WHEN "CLABE"
                    LET r_not_mdt.cta_clabe = v_valor_etiqueta
               END CASE
            END FOREACH 
            

            LET v_bnd_notifica = FALSE
            # Por cada paquete se agrega un registro en notificacion, para eliminar todos los registros de mandato 
            LET v_consulta = "\n SELECT cat.id_cat_entidad,",
                             "\n        paq.cve_entidad,",
                             "\n        cat.desc_entidad,",
                             "\n        cat.desc_larga_entidad,",
                             "\n        cat.usuario,",
                             "\n        cat.tpo_entidad,",
                             "\n        cat.f_creacion,",
                             "\n        cat.estado", 
                             "\n   FROM prt_cat_entidad cat JOIN prt_cat_entidad_paquete paq",
                             "\n     ON paq.id_cat_entidad = cat.id_cat_entidad",
                             "\n  WHERE cat.id_cat_entidad = ?"
            PREPARE prp_rec_mandatos FROM v_consulta 
            DECLARE cur_rec_mandatos CURSOR FOR prp_rec_mandatos
            --CALL Fn_RecMandato(cmbmdt) RETURNING r_mandato.*
            FOREACH cur_rec_mandatos USING cmbmdt INTO r_mandato.*
               # indica que si se encontró relacion de paquete para el mandato
               LET v_bnd_notifica = TRUE
               
               # se recuperan para insertar en mdt_notifica_mandato
               LET r_not_mdt.cve_mandato    = r_mandato.cve_mandato
               LET r_not_mdt.id_tpo_mandato = r_mandato.tpo_mandato
               LET r_not_mdt.id_cat_mandato = r_mandato.id_cat_mandato
               LET r_not_mdt.f_creacion     = r_mandato.f_creacion
               
               INSERT INTO prt_notifica_entidad
               (id_prt_notifica,
                id_tpo_entidad,
                id_cat_entidad,
                cve_entidad,
                tipo_operacion,
                descripcion1,
                descripcion2,
                descripcion3,
                n_cuenta_bancaria,
                n_convenio,
                n_referencia,
                cta_clabe,
                estado,
                resultado_operacion,
                diagnostico,
                f_creacion)
               VALUES(
                r_not_mdt.id_mdt_notifica,
                r_not_mdt.id_tpo_mandato,
                r_not_mdt.id_cat_mandato,
                r_not_mdt.cve_mandato,
                r_not_mdt.tipo_operacion,
                r_not_mdt.descripcion1,
                r_not_mdt.descripcion2,
                r_not_mdt.descripcion3,
                r_not_mdt.n_cuenta_bancaria,
                r_not_mdt.n_convenio,
                r_not_mdt.n_referencia,
                r_not_mdt.cta_clabe,
                r_not_mdt.estado,
                r_not_mdt.resultado_operacion,
                r_not_mdt.diagnostico,
                r_not_mdt.f_creacion
               )
            END FOREACH
            # en caso de que no se encontró relación con alguna clave de paquete
            # se notfica la baja del mandato sin clave
            IF NOT(v_bnd_notifica)THEN
               LET r_not_mdt.cve_mandato = "                  "
               INSERT INTO prt_notifica_entidad
               (id_prt_notifica,
                id_tpo_entidad,
                id_cat_entidad,
                cve_entidad,
                tipo_operacion,
                descripcion1,
                descripcion2,
                descripcion3,
                n_cuenta_bancaria,
                n_convenio,
                n_referencia,
                cta_clabe,
                estado,
                resultado_operacion,
                diagnostico,
                f_creacion)
               VALUES(
                r_not_mdt.id_mdt_notifica,
                r_not_mdt.id_tpo_mandato,
                r_not_mdt.id_cat_mandato,
                r_not_mdt.cve_mandato,
                r_not_mdt.tipo_operacion,
                r_not_mdt.descripcion1,
                r_not_mdt.descripcion2,
                r_not_mdt.descripcion3,
                r_not_mdt.n_cuenta_bancaria,
                r_not_mdt.n_convenio,
                r_not_mdt.n_referencia,
                r_not_mdt.cta_clabe,
                r_not_mdt.estado,
                r_not_mdt.resultado_operacion,
                r_not_mdt.diagnostico,
                r_not_mdt.f_creacion
               )
            END IF
            -- update estado en mdt_cat_mandato a 106
            UPDATE prt_cat_entidad
               SET estado = 106
             WHERE id_cat_entidad = cmbmdt
            IF(SQLCA.SQLCODE <> 0)THEN
               LET v_error_sql = TRUE
            END IF

            DISPLAY "cmbmdt:",cmbmdt

            DELETE 
              FROM prt_cat_entidad_paquete
             WHERE id_cat_entidad = cmbmdt
            
            IF LENGTH(v_s_nota CLIPPED) > 0 THEN
              
               LET v_s_qryTxt = "EXECUTE PROCEDURE safre_viv:sp_baja_instrucciones_por_mandato(?)"
            
               # Se prepara la ejecucion del stored procedure para la baja de instrucciones de mandatos por id_cat_mandato
               PREPARE prp_baja_inst_mdt FROM v_s_qryTxt
               EXECUTE prp_baja_inst_mdt USING cmbmdt INTO v_sp_estado
               IF(SQLCA.SQLCODE <> 0)THEN
                  LET v_error_sql = TRUE
               END IF
               
               IF v_sp_estado = 1 THEN
                  DISPLAY "Se registraron bajas de instrucciones de mandatos"
               ELSE
                  DISPLAY "No se registraron bajas de instrucciones de mandatos"
               END IF
               
            END IF
            IF(v_error_sql)THEN
               CALL fn_mensaje("Aviso","Ocurrió un error al eliminar el mandato","information")
            ELSE
               CALL fn_mensaje("Aviso","Operación realizada correctamente","information")
               EXIT DIALOG
            END IF
            
            CALL init_combo_mandato(cmbtpomdt)
            CALL v_arr_mandato.clear()
            CALL v_arr_imagen.clear()
            
         END IF
                  
         CONTINUE DIALOG
      
   END DIALOG
   
   CLOSE WINDOW w_eli_mandato 

END MAIN

#############################################################################
# Funcion           => fn_carga_arr_mdt - Carga detalle del mandato         #
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 14 Febrero 2012                                      #
#############################################################################
FUNCTION fn_carga_arr_mdt(p_tpomandato, p_mandato)
DEFINE p_mandato     LIKE mdt_cat_mandato.id_cat_mandato
DEFINE p_tpomandato  LIKE mdt_cat_mandato.tpo_mandato
DEFINE v_i_count     INTEGER

   IF p_tpomandato IS NULL OR LENGTH(p_tpomandato CLIPPED) = 0 THEN
      RETURN
   END IF

   IF p_mandato IS NULL OR LENGTH(p_mandato CLIPPED) = 0 THEN
      RETURN
   END IF
   
   LET v_s_qryTxt = "SELECT etiqueta, valor_etiqueta",
                    "  FROM prt_cat_entidad m,",
                    "       prt_cat_atributo_nivel n,",
                    "       prt_cat_gpo_etiqueta g,",
                    "       prt_cat_instancia_entidad i",
                    " WHERE m.id_cat_entidad = n.id_cat_entidad",
                    "   AND n.id_atr_nivel = i.id_atr_nivel",
                    "   AND n.id_gpo_etiqueta = g.id_gpo_etiqueta",
                    "   AND m.id_cat_entidad = ", p_mandato,
                    "   AND m.tpo_entidad = ", p_tpomandato

   PREPARE EnuDetMandato FROM v_s_qryTxt
   DECLARE CurDetMandato CURSOR FOR EnuDetMandato
   
   CALL v_arr_mandato.clear()
   
   LET v_i_count = 1
   FOREACH CurDetMandato INTO v_arr_mandato[v_i_count].*
      LET v_i_count = v_i_count + 1
   END FOREACH
   
END FUNCTION 

#############################################################################
# Funcion           => fn_carga_arr_img - Carga imagenes del mandato        #
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 14 Febrero 2012                                      #
#############################################################################
FUNCTION fn_carga_arr_img()
DEFINE v_i_count     INTEGER
DEFINE p_gpo_mdt     LIKE mdt_imagen_docto.id_gpo_mandato
DEFINE v_ruta_docto  LIKE seg_modulo.ruta_docto

   SELECT ruta_docto INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = 'prt'

   LET v_s_qryTxt = "SELECT nombre_imagen, desc_imagen",
                    "  FROM prt_imagen_docto",
                    " WHERE id_gpo_entidad = ?"
   
   PREPARE EnuImgMandato FROM v_s_qryTxt
   DECLARE CurImgMandato CURSOR FOR EnuImgMandato

   LET v_s_qryTxt = "SELECT id_gpo_entidad ",
                    "  FROM prt_cat_entidad m,",
                    "       prt_gpo_entidad n",
                    " WHERE m.id_cat_entidad = n.id_cat_entidad",
                    "   AND m.id_cat_entidad = ",cmbmdt,
                    "   AND m.tpo_entidad = ",cmbtpomdt
                             
   PREPARE EnuGpoImg FROM v_s_qryTxt
   DECLARE CurGpoImg CURSOR FOR EnuGpoImg
   
   CALL v_arr_imagen.clear()
   LET v_i_count = 1

   FOREACH CurGpoImg INTO p_gpo_mdt
      
      FOREACH CurImgMandato USING p_gpo_mdt INTO v_arr_imagen[v_i_count].*
         --LET v_arr_imagen[v_i_count].nombre_imagen = "<a href='",v_ruta_docto CLIPPED,v_arr_imagen[v_i_count].nombre_imagen CLIPPED,"' target='nueva'>",v_arr_imagen[v_i_count].nombre_imagen CLIPPED,"</a>"
         LET v_arr_imagen[v_i_count].nombre_imagen = "<a gwc:attributes=\"href resourceuri('",v_arr_imagen[v_i_count].nombre_imagen CLIPPED,"','prtdocto')\" target='nueva'>",v_arr_imagen[v_i_count].nombre_imagen CLIPPED,"</a>"
         DISPLAY v_arr_imagen[v_i_count].nombre_imagen
         LET v_i_count = v_i_count + 1
      END FOREACH
   END FOREACH
   
END FUNCTION 

#############################################################################
# Funcion           => inicializa_mandatos - Definicion e inicializacion    #
#                      de los combos dinamicos del modulo de mandatos       #
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 14 Febrero 2012                                      #
#############################################################################
FUNCTION inicializa_mandatos()
 
   -- Inicializa combo de bancos en funcion a su tabla de base de datos
   CALL init_combo_mandato(NULL)
   CALL init_combo_tipomandato()

END FUNCTION

#############################################################################
# Funcion           => init_combo_mandato - Inicializa el combo de mandatos #
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => p_tpo_mdt - tipo de mandato para el filtro           #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 14 Febrero 2012                                      #
#############################################################################
FUNCTION init_combo_mandato(p_tpo_mdt)

   DEFINE p_tpo_mdt        SMALLINT
   DEFINE v_s_qry          VARCHAR(50)
   DEFINE cb ui.ComboBox
   DEFINE mandato           LIKE mdt_cat_mandato.id_cat_mandato
   DEFINE descripcion       LIKE mdt_cat_mandato.desc_mandato
   DEFINE desc_combo        CHAR(50)

   LET cb      = ui.combobox.forname("cmbmdt")
   CALL cb.clear()
   
   IF p_tpo_mdt IS NULL THEN
      LET v_s_qry = ""
   ELSE
      LET v_s_qry = " AND tpo_mandato = ", p_tpo_mdt
   END IF

   LET v_s_qryTxt = "SELECT tb.id_cat_entidad, tb.desc_entidad",
                    "  FROM prt_cat_entidad tb",
                    " WHERE estado = 100 ",
                    v_s_qry,
                    " ORDER  BY 1 "

   PREPARE enu_mandato FROM v_s_qryTxt
   DECLARE cur_mandato CURSOR FOR enu_mandato
   
   FOREACH cur_mandato INTO mandato, descripcion
       LET desc_combo= mandato       USING "&&&", " ",
                       descripcion CLIPPED
       CALL cb.additem( mandato, desc_combo )
   END FOREACH

END FUNCTION

#############################################################################
# Funcion           => init_combo_tipomandato - Inicializa el combo tpo mdt.#
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 14 Febrero 2012                                      #
#############################################################################
FUNCTION init_combo_tipomandato()

   DEFINE cb ui.ComboBox
   DEFINE mandato           LIKE mdt_tpo_mandato.tpo_mandato
   DEFINE descripcion       LIKE mdt_tpo_mandato.desc_tpo_mandato
   DEFINE desc_combo        CHAR(50)

   LET cb   = ui.combobox.forname("cmbtpomdt")
   CALL cb.clear()
   
   DECLARE cur_tpomandato CURSOR FOR
    SELECT tb.tpo_entidad, tb.desc_tpo_entidad
      FROM prt_tpo_entidad tb
     ORDER  BY 1
   
   FOREACH cur_tpomandato INTO mandato, descripcion
       LET desc_combo= mandato       USING "&&&", " ",
                       descripcion CLIPPED
       CALL cb.additem( mandato, desc_combo )
   END FOREACH

END FUNCTION

#############################################################################
# Funcion           => Fn_RecMandato - Recupera registro del catalogo de mdt#
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => p_mandato - clave del mandato a recuperar            #
# Salida:           => r_mandato - regsitro del mandato recuperado          #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 14 Febrero 2012                                      #
#############################################################################
FUNCTION Fn_RecMandato(p_mandato)
DEFINE p_mandato    LIKE mdt_cat_mandato.id_cat_mandato
DEFINE v_r_mandato  RECORD --LIKE mdt_cat_mandato.*
         id_cat_mandato     LIKE mdt_cat_mandato.id_cat_mandato,
         cve_mandato        LIKE mdt_cat_mandato_paquete.cve_mandato,
         desc_mandato       LIKE mdt_cat_mandato.desc_mandato,
         desc_larga_mandato LIKE mdt_cat_mandato.desc_larga_mandato,
         usuario            LIKE mdt_cat_mandato.usuario,
         tpo_mandato        LIKE mdt_cat_mandato.tpo_mandato,
         f_creacion         LIKE mdt_cat_mandato.f_creacion,
         estado             LIKE mdt_cat_mandato.estado
        END RECORD 
    
   SELECT cat.id_cat_entidad,
          --paq.cve_mandato,
          cat.cve_entidad,
          cat.desc_entidad,
          cat.desc_larga_entidad,
          cat.usuario,
          cat.tpo_entidad,
          cat.f_creacion,
          cat.estado 
     INTO v_r_mandato.*
     FROM prt_cat_entidad cat --JOIN mdt_cat_mandato_paquete paq
       --ON paq.id_cat_mandato = cat.id_cat_mandato
    WHERE cat.id_cat_entidad = p_mandato
    
   RETURN v_r_mandato.*
   
END FUNCTION

#############################################################################
# Funcion           => fn_existe_instruccion_mdt - Verifica la existencia   #
#                      de instrucciones de mandato                          #
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => p_id_cat_mandato - Mandato a validar si cuenta con   #
#                      instrucciones de mandato                             #
# Salida:           => Verdadero si tiene asociadas instrucciones y falso en#
#                      caso contrario                                       #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 01 Marzo 2012                                        #
#############################################################################
FUNCTION fn_existe_instruccion_mdt(p_id_cat_entidad)
   DEFINE p_id_cat_entidad LIKE mdt_cat_mandato.id_cat_mandato
   DEFINE v_existe         INTEGER
   
   SELECT NVL(COUNT(*),0) INTO v_existe
     --FROM prt_det_ctr_entidad   -- antes mdt_det_ctr_mandato
     FROM prt_expediente_convenios_iss
    WHERE id_cat_entidad = p_id_cat_entidad       
      --AND estado in (103,104)
     
   IF v_existe > 0 THEN
      RETURN TRUE
   ELSE
      RETURN FALSE
   END IF
   
END FUNCTION
