--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10-04-2012
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => MDT                                                     #
#Programa          => MDTM07                                                  #
#Objetivo          => CATÁLOGO DE GRUPOS                                      #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 29 Febrero 2012                                         #
###############################################################################
DATABASE safre_viv

# VARIABLES GLOBALES
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod,
       p_tipo_ejecucion SMALLINT,
       p_texto_ventana  VARCHAR(50),
       v_arbol_grupos   DYNAMIC ARRAY OF RECORD
        v_descripcion   LIKE mdt_cat_gpo_etiqueta.etiqueta,
        v_grupo         LIKE mdt_cat_gpo.id_cat_gpo,
        v_etiqueta      LIKE mdt_cat_gpo_etiqueta.id_gpo_etiqueta,
        v_expandido     BOOLEAN
       END RECORD,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       w ui.Window,
       f ui.Form

MAIN   
DEFINE v_bnd_continua BOOLEAN   
   # Se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_texto_ventana  = ARG_VAL(3)

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "mdt"

    LET v_bnd_continua = TRUE

   OPEN WINDOW vtna_arbol_grupos WITH FORM v_ruta_ejecutable CLIPPED||"/MDTM071"
      WHILE v_bnd_continua 
         DISPLAY ARRAY v_arbol_grupos TO sr_arbol.* 
                                      ATTRIBUTE(UNBUFFERED, ACCEPT = FALSE,CANCEL = FALSE)
            BEFORE DISPLAY

               IF(p_texto_ventana IS NOT NULL)THEN
                  CALL ui.Interface.setText(p_texto_ventana)
                  LET w = ui.Window.getCurrent()
                  CALL w.setText(p_texto_ventana)
               END IF

               CALL f_recupera_arbol()

            ON ACTION Agregar
               CALL f_actualiza_arbol(v_arbol_grupos[ARR_CURR()].*,'A')
               ACCEPT DISPLAY               
 
            ON ACTION modificar
               IF(v_arbol_grupos[ARR_CURR()].v_grupo IS NULL)THEN
                  CALL fn_mensaje(p_texto_ventana,"Seleccione un grupo o atributo","about")
                  CONTINUE DISPLAY
               END IF
               CALL f_actualiza_arbol(v_arbol_grupos[ARR_CURR()].*,'M')
               ACCEPT DISPLAY

            ON ACTION Eliminar
               IF(v_arbol_grupos[ARR_CURR()].v_grupo IS NULL)THEN
                  CALL fn_mensaje(p_texto_ventana,"Seleccione un grupo o atributo","about")
                  CONTINUE DISPLAY
               END IF                
               CALL f_elimina_grupo(v_arbol_grupos[ARR_CURR( )].*)
               ACCEPT DISPLAY

            ON ACTION CLOSE
               LET v_bnd_continua = FALSE
               EXIT DISPLAY
            
         END DISPLAY
      END WHILE
   CLOSE WINDOW vtna_arbol_grupos
   
END MAIN

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM07                                                  #
#Objetivo          => Recupera el arbol de grupo de mandatos                  #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 29 Febrero 2012                                         #
###############################################################################
FUNCTION f_recupera_arbol()
DEFINE v_indice           INTEGER,
       v_grupos           RECORD   # Grupos de mandatos
        v_id_cat_gpo      LIKE mdt_cat_gpo.id_cat_gpo,
        v_descripcion     LIKE mdt_cat_gpo.descripcion
       END RECORD,
       v_etiquetas        RECORD   # Etiquetas de mandatos
        v_id_gpo_etiqueta LIKE mdt_cat_gpo_etiqueta.id_gpo_etiqueta,
        v_etiqueta        LIKE mdt_cat_gpo_etiqueta.etiqueta
       END RECORD,
       v_consulta         STRING  

   WHENEVER ERROR CONTINUE

   CALL v_arbol_grupos.clear()
   # Recupera los grupos/padres
   DECLARE cur_grupos CURSOR FOR
   SELECT id_cat_gpo, descripcion
     FROM mdt_cat_gpo
    WHERE 1=1
    ORDER BY id_cat_gpo

   # Recupera las etiquetas
   LET v_consulta = "\n SELECT id_gpo_etiqueta, etiqueta",
                    "\n   FROM mdt_cat_gpo_etiqueta",
                    "\n  WHERE id_cat_gpo = ?", 
                    "\n  ORDER BY id_gpo_etiqueta"
   PREPARE prp_etiquetas FROM v_consulta 
   DECLARE cur_etiquetas CURSOR FOR prp_etiquetas
   INITIALIZE v_arbol_grupos[1].v_grupo TO NULL 
   LET v_arbol_grupos[1].v_etiqueta    = 0
   LET v_arbol_grupos[1].v_descripcion = "GRUPOS"
   LET v_arbol_grupos[1].v_expandido   = TRUE
   # Se recupera cada grupo y por cada grupo se recuperan las etiquetas
   LET v_indice = 2   
   FOREACH cur_grupos INTO v_grupos.*
      LET v_arbol_grupos[v_indice].v_grupo       = 0 
      LET v_arbol_grupos[v_indice].v_etiqueta    = v_grupos.v_id_cat_gpo
      LET v_arbol_grupos[v_indice].v_descripcion = v_grupos.v_descripcion CLIPPED
      LET v_arbol_grupos[v_indice].v_expandido   = TRUE
      # Solo se agrega v_grupo para indicar a que nodo pertenece
      FOREACH cur_etiquetas USING v_grupos.v_id_cat_gpo INTO v_etiquetas.*
         LET v_indice = v_indice + 1
         LET v_arbol_grupos[v_indice].v_grupo       = v_grupos.v_id_cat_gpo 
         LET v_arbol_grupos[v_indice].v_etiqueta    = v_etiquetas.v_id_gpo_etiqueta
         LET v_arbol_grupos[v_indice].v_descripcion = v_etiquetas.v_etiqueta
         LET v_arbol_grupos[v_indice].v_expandido   = TRUE
      END FOREACH
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_grupos
   FREE cur_etiquetas

END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM07                                                  #
#Objetivo          => Actualiza(agrega, modifica, elimina) los grupos de      #
#                     mandatos                                                #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 29 Febrero 2012                                         #
###############################################################################
FUNCTION f_actualiza_arbol(v_registro_gpo,v_accion)
DEFINE v_registro_gpo   RECORD
        v_descripcion   LIKE mdt_cat_gpo_etiqueta.etiqueta,
        v_grupo         LIKE mdt_cat_gpo.id_cat_gpo,
        v_etiqueta      LIKE mdt_cat_gpo_etiqueta.id_gpo_etiqueta,
        v_expandido     BOOLEAN
       END RECORD,
       v_accion         CHAR(1),
       v_grupo          STRING,
       v_atributo       STRING,
       v_bandera        BOOLEAN,
       v_descrip_gpo    LIKE mdt_cat_gpo.descripcion

   OPEN WINDOW vtna_detalle_mandato WITH FORM v_ruta_ejecutable CLIPPED||"/MDTM072"
      # v_registro_gpo.v_grupo IS NULL --> para agregar un grupo
      # v_accion = 'M' AND v_registro_gpo.v_grupo = 0 --> para modificar un grupo
      IF(v_registro_gpo.v_grupo IS NULL OR (v_accion = 'M' AND v_registro_gpo.v_grupo = 0))THEN
      
         INPUT v_grupo,v_atributo WITHOUT DEFAULTS FROM edi_grupo,edi_atributo
                        ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
            BEFORE INPUT
               INITIALIZE v_grupo,v_atributo TO NULL
               LET w = ui.Window.getCurrent()
               IF(p_texto_ventana IS NOT NULL)THEN
                  CALL w.setText(p_texto_ventana)
               END IF            
               LET f = w.getForm()
               # Se deshabilita la captura de atributo para capturar el grupo
               CALL f.setFieldHidden("edi_atributo",1)
               CALL f.setElementHidden("lbl_atributo",1)
               # Tipo de accion que desea hacer el usuario (agregar, modificar, eliminar)
               CASE v_accion
                  WHEN 'A'
                     --CALL DIALOG.setActionActive("modificar",0)
                     CALL DIALOG.setActionHidden("modificar",1)
                     CALL f.setFieldHidden("edi_atributo",0)
                     CALL f.setElementHidden("lbl_atributo",0)
               
                  WHEN 'M'
                     INITIALIZE v_descrip_gpo TO NULL 
                     SELECT descripcion
                       INTO v_descrip_gpo
                       FROM mdt_cat_gpo
                      WHERE id_cat_gpo = v_registro_gpo.v_grupo
                         --OR id_cat_gpo = v_registro_gpo.v_etiqueta

                     IF(v_descrip_gpo IS NULL)THEN
                        SELECT descripcion
                          INTO v_descrip_gpo
                          FROM mdt_cat_gpo
                         WHERE id_cat_gpo = v_registro_gpo.v_etiqueta
                     END IF
                     LET v_grupo = v_descrip_gpo
                     DISPLAY v_grupo TO edi_grupo
                     --CALL DIALOG.setActionActive("agregar",0)
                     CALL DIALOG.setActionHidden("agregar",1)
                     --CALL DIALOG.setFieldActive("edi_grupo",0)
                  
               END CASE

            AFTER FIELD edi_grupo
               IF(v_grupo IS NULL OR v_grupo = "")THEN
                  ERROR "Capture Grupo" ATTRIBUTE(REVERSE, BOLD, YELLOW)
                  NEXT FIELD edi_grupo
               END IF

            AFTER FIELD edi_atributo
               IF(v_atributo IS NULL OR v_atributo = "")THEN
                  ERROR "Capture Atributo" ATTRIBUTE(REVERSE, BOLD, YELLOW)
                  NEXT FIELD edi_atributo 
               END IF
 
            ON ACTION Agregar
               IF(v_grupo IS NULL OR v_grupo = "")THEN
                  ERROR "Capture Grupo" ATTRIBUTE(REVERSE, BOLD, YELLOW)
                  NEXT FIELD edi_grupo
               END IF
               IF(v_atributo IS NULL OR v_atributo = "")THEN
                  ERROR "Capture Atributo" ATTRIBUTE(REVERSE, BOLD, YELLOW)
                  NEXT FIELD edi_atributo 
               END IF
               # revisa que no existe otro grupo como el que se almacenara
               CALL f_verifica_grupo(v_grupo) RETURNING v_bandera
               IF(v_bandera)THEN
                  CALL fn_mensaje(p_texto_ventana,"Ya existe un grupo con misma descripción","about")
               ELSE
                  # revisa que no existe otra etiqueta como la que se almacenara
                  CALL f_verifica_etiqueta(v_registro_gpo.v_grupo,v_registro_gpo.v_etiqueta,v_atributo) RETURNING v_bandera
                  IF(v_bandera)THEN
                     CALL fn_mensaje(p_texto_ventana,"Ya existe una etiqueta con misma descripción","about")
                  ELSE
                     # Agrega grupo
                     CALL f_agrega_grupo(v_grupo)RETURNING v_registro_gpo.v_grupo
                     # Agrega grupo
                     CALL f_agrega_etiqueta_grupo(v_registro_gpo.v_grupo,v_registro_gpo.v_etiqueta,v_atributo)
                  END IF
                  
               END IF
               ACCEPT INPUT

            ON ACTION modificar
               IF(v_grupo IS NULL OR v_grupo = "")THEN
                  ERROR "Capture Grupo" ATTRIBUTE(REVERSE, BOLD, YELLOW)
                  NEXT FIELD edi_grupo
               END IF
               # revisa que no existe otro grupo como el que se almacenara
               CALL f_verifica_grupo(v_grupo) RETURNING v_bandera
               IF(v_bandera)THEN
                  CALL fn_mensaje(p_texto_ventana,"Ya existe un grupo con misma descripción","about")
               ELSE
                  # Modifica el grupo
                  CALL f_modificar_grupo(v_registro_gpo.v_grupo,v_registro_gpo.v_etiqueta,v_grupo)
               END IF
               ACCEPT INPUT

            ON ACTION regresa
               EXIT INPUT

         END INPUT
      ELSE
         INPUT v_atributo WITHOUT DEFAULTS FROM edi_atributo
                                  ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
            BEFORE INPUT
               INITIALIZE v_grupo,v_atributo TO NULL
               LET w = ui.Window.getCurrent()
               IF(p_texto_ventana IS NOT NULL)THEN
                  CALL w.setText(p_texto_ventana)
               END IF            
               LET f = w.getForm()

               INITIALIZE v_descrip_gpo TO NULL
               # recupera la descripcion del grupo
               SELECT descripcion
                 INTO v_descrip_gpo
                 FROM mdt_cat_gpo
                WHERE id_cat_gpo = v_registro_gpo.v_grupo
               --OR id_cat_gpo = v_registro_gpo.v_etiqueta
               IF(v_descrip_gpo IS NULL)THEN
                  SELECT descripcion
                    INTO v_descrip_gpo
                    FROM mdt_cat_gpo
                   WHERE id_cat_gpo = v_registro_gpo.v_etiqueta
               END IF
               LET v_grupo = v_descrip_gpo
               DISPLAY v_grupo TO edi_grupo
               # dependiendo de lo que se haya resionado en la pantalla anterior, se 
               # agregara o modificara un nuevo registro
               CASE v_accion
                  WHEN 'A'
                     --CALL DIALOG.setActionActive("modificar",0)
                     CALL DIALOG.setActionHidden("modificar",1)
                     
                  WHEN 'M'
                     SELECT etiqueta
                       INTO v_descrip_gpo
                       FROM mdt_cat_gpo_etiqueta
                      WHERE id_gpo_etiqueta = v_registro_gpo.v_etiqueta
                     LET v_atributo = v_descrip_gpo
                     DISPLAY v_atributo TO edi_atributo
                     --CALL DIALOG.setActionActive("agregar",0)
                     CALL DIALOG.setActionHidden("agregar",1)
                  
               END CASE

            AFTER FIELD edi_atributo
               IF(v_atributo IS NULL OR v_atributo = "")THEN
                  ERROR "Capture Atributo" ATTRIBUTE(REVERSE, BOLD, YELLOW)
                  NEXT FIELD edi_atributo 
               END IF

            ON ACTION Agregar
                IF(v_atributo IS NULL OR v_atributo = "")THEN
                  ERROR "Capture Atributo" ATTRIBUTE(REVERSE, BOLD, YELLOW)
                 NEXT FIELD edi_atributo 
               END IF
               # revisa que no existe otra etiqueta como la que se almacenara
               # se usa v_registro_gpo.v_etiqueta ya que es el grupo al que se le agregara la etiqueta
               DISPLAY "grupo: "||v_registro_gpo.v_etiqueta||" valor agregado: "||v_atributo 
               CALL f_verifica_etiqueta(v_registro_gpo.v_grupo,v_registro_gpo.v_etiqueta,v_atributo) RETURNING v_bandera
               IF(v_bandera)THEN
                  CALL fn_mensaje(p_texto_ventana,"Ya existe una etiqueta con misma descripción","about")
               ELSE
                  # Agrega grupo
                  # se usa v_registro_gpo.v_etiqueta ya que es el grupo al que se le agregara la etiqueta
                  CALL f_agrega_etiqueta_grupo(v_registro_gpo.v_grupo,v_registro_gpo.v_etiqueta,v_atributo)
               END IF
               ACCEPT INPUT

            ON ACTION modificar
                IF(v_atributo IS NULL OR v_atributo = "")THEN
                  ERROR "Capture Atributo" ATTRIBUTE(REVERSE, BOLD, YELLOW)
                 NEXT FIELD edi_atributo 
               END IF
               # revisa que no existe otra etiqueta como la que se almacenara
               # se usa v_registro_gpo.v_etiqueta ya que es el grupo actual y al que se le modificará la etiqueta
               CALL f_verifica_etiqueta(v_registro_gpo.v_grupo,v_registro_gpo.v_etiqueta,v_atributo) RETURNING v_bandera
               IF(v_bandera)THEN
                  CALL fn_mensaje(p_texto_ventana,"Ya existe una etiqueta con misma descripción","about")
               ELSE
                  # Modifica grupo
                  CALL f_modifica_etiqueta_grupo(v_registro_gpo.v_grupo,v_registro_gpo.v_etiqueta,v_atributo)
               END IF
               ACCEPT INPUT

            ON ACTION regresa
               EXIT INPUT

         END INPUT
      END IF
   CLOSE WINDOW vtna_detalle_mandato

END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM07                                                  #
#Objetivo          => Elimina los grupos de mandatos                          #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 30 Marzo 2012                                           #
###############################################################################
FUNCTION f_elimina_grupo(v_registro_gpo)
DEFINE v_registro_gpo   RECORD
        v_descripcion   LIKE mdt_cat_gpo_etiqueta.etiqueta,
        v_grupo         LIKE mdt_cat_gpo.id_cat_gpo,
        v_etiqueta      LIKE mdt_cat_gpo_etiqueta.id_gpo_etiqueta,
        v_expandido     BOOLEAN
       END RECORD,
       v_grupo          STRING,
       v_atributo       STRING,
       v_descrip_gpo    LIKE mdt_cat_gpo.descripcion,
       r_respuesta      BOOLEAN,
       r_bnd_sql        BOOLEAN,
       r_cto_mandatos   SMALLINT,
       r_conteo         SMALLINT

   OPEN WINDOW vtna_detalle_mandato WITH FORM v_ruta_ejecutable CLIPPED||"/MDTM072"
      IF(v_registro_gpo.v_grupo = 0)THEN
         MENU  
            BEFORE MENU
               LET w = ui.Window.getCurrent()
               IF(p_texto_ventana IS NOT NULL)THEN
                  CALL w.setText(p_texto_ventana)
               END IF            
               LET f = w.getForm()
               # Se deshabilita la captura de atributo para capturar el grupo
               CALL f.setFieldHidden("edi_atributo",1)
               CALL f.setElementHidden("lbl_atributo",1)
               # Tipo de accion que desea hacer el usuario (agregar, modificar, eliminar)
               SELECT descripcion
                 INTO v_descrip_gpo
                 FROM mdt_cat_gpo
                WHERE id_cat_gpo = v_registro_gpo.v_grupo
                   OR id_cat_gpo = v_registro_gpo.v_etiqueta
               LET v_grupo = v_descrip_gpo
               DISPLAY v_grupo TO edi_grupo
               
            ON ACTION Eliminar
               # Verifica si se puede eliminar el grupo
               CALL f_verifica_eliminar_grupo(v_registro_gpo.v_etiqueta)RETURNING r_bnd_sql, r_cto_mandatos
               # no hace nada si ocurrió un error sql 
               IF(r_bnd_sql)THEN
                  EXIT MENU
               END IF
               IF(r_cto_mandatos > 0)THEN
                  # no se puede eliminar el grupo, ya que hay mandatos relacionados al grupo
                  CALL fn_mensaje(p_texto_ventana,"No se puede Eliminar grupo/atributo, mandato con grupo/atributo activo","about")
                  CONTINUE MENU
               END IF
               
               # confirma si se desea eliminar el grupo
               CALL fn_ventana_confirma(p_texto_ventana,"Se eliminará el grupo "||v_grupo CLIPPED||"\n¿Desea continuar?","about")RETURNING r_respuesta
               IF(r_respuesta)THEN
                  CALL f_eliminar_grupo(v_registro_gpo.v_etiqueta)
                  EXIT MENU
               END IF
               CONTINUE MENU

            ON ACTION regresa
               EXIT MENU

         END MENU
      ELSE
         MENU 
            BEFORE MENU
               LET w = ui.Window.getCurrent()
               IF(p_texto_ventana IS NOT NULL)THEN
                  CALL w.setText(p_texto_ventana)
               END IF            
               LET f = w.getForm()
               # recupera la descripcion del grupo
               SELECT descripcion
                 INTO v_descrip_gpo
                 FROM mdt_cat_gpo
                WHERE id_cat_gpo = v_registro_gpo.v_grupo
                   OR id_cat_gpo = v_registro_gpo.v_etiqueta
               LET v_grupo = v_descrip_gpo
               DISPLAY v_grupo TO edi_grupo
               # recupera la descripcion de la etiqueta
               SELECT etiqueta
                 INTO v_descrip_gpo
                 FROM mdt_cat_gpo_etiqueta
                WHERE id_gpo_etiqueta = v_registro_gpo.v_etiqueta
               LET v_atributo = v_descrip_gpo
               DISPLAY v_atributo TO edi_atributo

            ON ACTION Eliminar
              # verifica si se puede eliminar la etiqueta
              CALL f_verifica_eliminar_etiqueta(v_registro_gpo.v_etiqueta)RETURNING r_bnd_sql, r_cto_mandatos
               # no hace nada si ocurrió un error sql 
               IF(r_bnd_sql)THEN
                  EXIT MENU
               END IF
               IF(r_cto_mandatos > 0)THEN
                  # no se puede eliminar el grupo, ya que hay mandatos relacionados al grupo
                  CALL fn_mensaje(p_texto_ventana,"No se puede Eliminar grupo/atributo, mandato con grupo/atributo activo","about")
                  CONTINUE MENU
               END IF
               CALL f_verifica_etiqueta_grupo(v_registro_gpo.v_grupo)RETURNING r_conteo
               # si solo queda un atributo para el grupo se elimina tambien el grupo
               IF(r_conteo = 1)THEN
                  # se eliminara el grupo junto con el atributo
                  CALL fn_ventana_confirma(p_texto_ventana,"Se eliminará el grupo "||v_grupo CLIPPED||" junto con el atributo "||v_atributo CLIPPED||"\n¿Desea continuar?","about")RETURNING r_respuesta
                  IF(r_respuesta)THEN
                     CALL f_eliminar_etiqueta(v_registro_gpo.v_grupo,v_registro_gpo.v_etiqueta,r_conteo)
                     EXIT MENU
                  END IF
               ELSE
                  # solo se elimina el atributo
                  CALL fn_ventana_confirma(p_texto_ventana,"Se eliminará el atributo "||v_atributo CLIPPED||"\n¿Desea continuar?","about")RETURNING r_respuesta
                  IF(r_respuesta)THEN
                     CALL f_eliminar_etiqueta(v_registro_gpo.v_grupo,v_registro_gpo.v_etiqueta,r_conteo)
                     EXIT MENU
                  END IF
               END IF
               CONTINUE MENU

            ON ACTION regresa
               EXIT MENU

         END MENU
      END IF
   CLOSE WINDOW vtna_detalle_mandato

END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM07                                                  #
#Objetivo          => Aagrega grupo                                           #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 29 Febrero 2012                                         #
###############################################################################
FUNCTION f_agrega_grupo(v_descripcion)
DEFINE v_id_cat_gpo  LIKE mdt_cat_gpo.id_cat_gpo,
       v_descripcion LIKE mdt_cat_gpo.descripcion

   WHENEVER ERROR CONTINUE
   
   SELECT MAX(id_cat_gpo)+1
     INTO v_id_cat_gpo
     FROM mdt_cat_gpo

   LET v_descripcion =  v_descripcion CLIPPED
     
   INSERT INTO mdt_cat_gpo(id_cat_gpo, descripcion, usuario) 
     VALUES(v_id_cat_gpo,v_descripcion,p_usuario_cod)

   IF(SQLCA.SQLCODE <> 0)THEN
      CALL fn_mensaje(p_texto_ventana,"Ocurrió un error","about")
   END IF
   RETURN v_id_cat_gpo
END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM07                                                  #
#Objetivo          => Modificar grupo                                         #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 29 Febrero 2012                                         #
###############################################################################
FUNCTION f_modificar_grupo(v_id_cat_gpo,v_etiqueta,v_descripcion)
DEFINE v_id_cat_gpo  LIKE mdt_cat_gpo.id_cat_gpo,
       v_etiqueta    LIKE mdt_cat_gpo.id_cat_gpo,
       v_descripcion LIKE mdt_cat_gpo.descripcion

   WHENEVER ERROR CONTINUE
   
   SELECT MAX(id_cat_gpo)+1
     INTO v_id_cat_gpo
     FROM mdt_cat_gpo

   UPDATE mdt_cat_gpo SET descripcion = v_descripcion
    WHERE id_cat_gpo = v_id_cat_gpo
       OR id_cat_gpo = v_etiqueta
   
   IF(SQLCA.SQLCODE <> 0)THEN
      CALL fn_mensaje(p_texto_ventana,"Ocurrió un error","about")
   ELSE
      CALL fn_mensaje(p_texto_ventana,"Se ha modificado correctamente el grupo","about")
   END IF

END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM07                                                  #
#Objetivo          => Elimina grupo                                           #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 30 Marzo 2012                                           #
###############################################################################
FUNCTION f_eliminar_grupo(v_etiqueta)
DEFINE v_etiqueta     LIKE mdt_cat_gpo.id_cat_gpo

   WHENEVER ERROR CONTINUE
   
   DELETE
     FROM mdt_cat_gpo
    WHERE id_cat_gpo = v_etiqueta

   DELETE
     FROM mdt_cat_gpo_etiqueta
    WHERE id_cat_gpo = v_etiqueta
   
   IF(SQLCA.SQLCODE <> 0)THEN
      CALL fn_mensaje(p_texto_ventana,"Ocurrió un error","about")
   ELSE
      CALL fn_mensaje(p_texto_ventana,"Se ha eliminado correctamente el grupo","about")
   END IF
   
END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM07                                                  #
#Objetivo          => Elimina etiqueta                                        #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 30 Marzo 2012                                           #
###############################################################################
FUNCTION f_eliminar_etiqueta(v_grupo,v_etiqueta,p_conteo_etiquetas)
DEFINE v_etiqueta         LIKE mdt_cat_gpo_etiqueta.etiqueta,
       v_grupo            LIKE mdt_cat_gpo_etiqueta.id_cat_gpo,
       p_conteo_etiquetas SMALLINT

   WHENEVER ERROR CONTINUE

   IF(p_conteo_etiquetas = 1)THEN
      # se ha aceptado eliminar el atributo junto con el grupo
      DELETE
        FROM mdt_cat_gpo_etiqueta
       WHERE id_cat_gpo = v_grupo
         AND id_gpo_etiqueta = v_etiqueta

      DELETE
        FROM mdt_cat_gpo
       WHERE id_cat_gpo = v_grupo
   ELSE
      # solo se elimina el atributo
      DELETE
        FROM mdt_cat_gpo_etiqueta
       WHERE id_cat_gpo = v_grupo
         AND id_gpo_etiqueta = v_etiqueta
   END IF

   IF(SQLCA.SQLCODE <> 0)THEN
      CALL fn_mensaje(p_texto_ventana,"Ocurrió un error","about")
   ELSE
      CALL fn_mensaje(p_texto_ventana,"Se ha eliminado correctamente el atributo","about")
   END IF
   
END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM07                                                  #
#Objetivo          => Aagrega etiqueta a grupo                                #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 29 Febrero 2012                                         #
###############################################################################
FUNCTION f_agrega_etiqueta_grupo(v_id_cat_gpo,v_etiqueta,v_descripcion)
DEFINE v_id_cat_gpo      LIKE mdt_cat_gpo_etiqueta.id_cat_gpo,
       v_id_gpo_etiqueta LIKE mdt_cat_gpo_etiqueta.id_gpo_etiqueta,
       v_etiqueta        LIKE mdt_cat_gpo_etiqueta.id_cat_gpo,
       v_descripcion     LIKE mdt_cat_gpo.descripcion

   WHENEVER ERROR CONTINUE
   
   
   SELECT MAX(id_gpo_etiqueta)+1
     INTO v_id_gpo_etiqueta
     FROM mdt_cat_gpo_etiqueta
    --WHERE id_cat_gpo = v_id_cat_gpo

   LET v_descripcion = v_descripcion CLIPPED      
   IF(v_id_cat_gpo = 0)THEN
      INSERT INTO mdt_cat_gpo_etiqueta(id_gpo_etiqueta,id_cat_gpo, etiqueta,
                                       usuario,id_modifica) 
          VALUES(v_id_gpo_etiqueta,v_etiqueta,v_descripcion,p_usuario_cod,0)
   ELSE
      INSERT INTO mdt_cat_gpo_etiqueta(id_gpo_etiqueta,id_cat_gpo, etiqueta,
                                       usuario,id_modifica) 
          VALUES(v_id_gpo_etiqueta,v_id_cat_gpo,v_descripcion,p_usuario_cod,0)
   END IF

   
   

   IF(SQLCA.SQLCODE <> 0)THEN
      CALL fn_mensaje(p_texto_ventana,"Ocurrió un error","about")
   ELSE
      CALL fn_mensaje(p_texto_ventana,"Se ha agregado correctamente el registro","about")
   END IF

END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM07                                                  #
#Objetivo          => Aagrega etiqueta a grupo                                #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 01 Marzo 2012                                           #
###############################################################################
FUNCTION f_modifica_etiqueta_grupo(v_id_cat_gpo,v_id_gpo_etiqueta,v_descripcion)
DEFINE v_id_cat_gpo      LIKE mdt_cat_gpo_etiqueta.id_cat_gpo,
       v_id_gpo_etiqueta LIKE mdt_cat_gpo_etiqueta.id_gpo_etiqueta,
       v_descripcion     LIKE mdt_cat_gpo.descripcion

   WHENEVER ERROR CONTINUE

   UPDATE mdt_cat_gpo_etiqueta SET etiqueta = v_descripcion,
                                   usuario = p_usuario_cod
    WHERE id_gpo_etiqueta = v_id_gpo_etiqueta
      AND id_cat_gpo = v_id_cat_gpo
      
   IF(SQLCA.SQLCODE <> 0)THEN
      CALL fn_mensaje(p_texto_ventana,"Ocurrió un error","about")
   ELSE
      CALL fn_mensaje(p_texto_ventana,"Se ha modificado correctamente el atributo","about")
   END IF

END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM07                                                  #
#Objetivo          => Aagrega grupo                                           #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 29 Febrero 2012                                         #
###############################################################################
FUNCTION f_verifica_grupo(v_descripcion)
DEFINE v_descripcion LIKE mdt_cat_gpo.descripcion,
       v_bandera     BOOLEAN

   WHENEVER ERROR CONTINUE
   LET v_bandera = 0
   
   SELECT NVL(1,0)
     INTO v_bandera
     FROM mdt_cat_gpo
    WHERE descripcion = v_descripcion
    
   IF(SQLCA.SQLCODE < 0)THEN
      CALL fn_mensaje(p_texto_ventana,"Ocurrió un error","about")
   END IF
   DISPLAY v_bandera||" desc: "||v_descripcion||" f_verifica_grupo"

   RETURN v_bandera

END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM07                                                  #
#Objetivo          => verifica si no hay un mandato relacionado al grupo para #
#                     poder eliminar el grupo                                 #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 30 Marzo 2012                                           #
###############################################################################
FUNCTION f_verifica_eliminar_grupo(v_id_cat_gpo)
DEFINE v_id_cat_gpo LIKE mdt_gpo_mandato.id_cat_gpo,
       v_conteo     SMALLINT,
       v_bandera    BOOLEAN

   WHENEVER ERROR CONTINUE
   LET v_conteo = 0
   LET v_bandera = FALSE
   
   SELECT COUNT(id_cat_gpo)
     INTO v_conteo
     FROM mdt_gpo_mandato
    WHERE id_cat_gpo = v_id_cat_gpo
    
   IF(SQLCA.SQLCODE < 0)THEN
      CALL fn_mensaje(p_texto_ventana,"Ocurrió un error","about")
      LET v_bandera = TRUE
   ELSE
      LET v_bandera = FALSE
   END IF
   DISPLAY "v_bandera: "||v_bandera||" v_conteo: "||v_conteo

   RETURN v_bandera,v_conteo
END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM07                                                  #
#Objetivo          => verifica si no hay un mandato relacionado al grupo para #
#                     poder eliminar la etiqueta                              #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 30 Marzo 2012                                           #
###############################################################################
--FUNCTION f_verifica_mandato_grupo(v_id_cat_gpo)
FUNCTION f_verifica_eliminar_etiqueta(v_id_gpo_etiqueta)
DEFINE v_id_gpo_etiqueta LIKE mdt_cat_gpo_etiqueta.id_gpo_etiqueta,
       v_conteo          SMALLINT,
       v_bandera         BOOLEAN

   WHENEVER ERROR CONTINUE
   LET v_conteo = 0
   LET v_bandera = FALSE
   
   SELECT COUNT(*)
     INTO v_conteo
     FROM mdt_cat_atributo_nivel
    WHERE id_gpo_etiqueta =  v_id_gpo_etiqueta
    
   IF(SQLCA.SQLCODE < 0)THEN
      CALL fn_mensaje(p_texto_ventana,"Ocurrió un error","about")
      LET v_bandera = TRUE
   ELSE
      LET v_bandera = FALSE
   END IF
   DISPLAY "v_bandera: "||v_bandera||" v_conteo: "||v_conteo

   RETURN v_bandera,v_conteo
END FUNCTION


###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM07                                                  #
#Objetivo          => verifica si solo queda una etiqueta relacionada al      #
#                     grupo                                                   #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 30 Marzo 2012                                           #
###############################################################################
FUNCTION f_verifica_etiqueta_grupo(v_grupo)
DEFINE v_grupo  LIKE mdt_gpo_mandato.id_cat_gpo,
       v_conteo SMALLINT       

   WHENEVER ERROR CONTINUE

   SELECT COUNT(id_gpo_etiqueta)
     INTO v_conteo
     FROM mdt_cat_gpo_etiqueta
    WHERE id_cat_gpo = v_grupo
    

   IF(SQLCA.SQLCODE < 0)THEN
      CALL fn_mensaje(p_texto_ventana,"Ocurrió un error","about")
   END IF
   RETURN v_conteo
END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM07                                                  #
#Objetivo          => Aagrega grupo                                           #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 29 Febrero 2012                                         #
###############################################################################
FUNCTION f_verifica_etiqueta(v_id_cat_gpo,v_id_gpo_etiqueta,v_etiqueta)
DEFINE v_etiqueta        LIKE mdt_cat_gpo_etiqueta.etiqueta,
       v_id_cat_gpo      LIKE mdt_cat_gpo.id_cat_gpo,
       v_id_gpo_etiqueta LIKE mdt_cat_gpo_etiqueta.id_gpo_etiqueta,
       v_bandera         BOOLEAN

   WHENEVER ERROR CONTINUE
   LET v_bandera = 0
   # se revisa si es un grupo o es un atributo
   IF(v_id_cat_gpo = 0)THEN
      SELECT NVL(1,0)
        INTO v_bandera
        FROM mdt_cat_gpo_etiqueta
       WHERE etiqueta = v_etiqueta
         AND id_cat_gpo = v_id_gpo_etiqueta
   ELSE
      SELECT NVL(1,0)
        INTO v_bandera
        FROM mdt_cat_gpo_etiqueta
       WHERE etiqueta = v_etiqueta
         AND id_cat_gpo = v_id_cat_gpo
   END IF
          
   IF(SQLCA.SQLCODE < 0)THEN
      CALL fn_mensaje(p_texto_ventana,"Ocurrió un error","about")
   END IF
   DISPLAY v_bandera||" desc: "||v_etiqueta||" gpo: "||v_id_cat_gpo||" f_verifica_etiqueta"

   RETURN v_bandera
END FUNCTION