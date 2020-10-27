--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#############################################################################
#Módulo          => ACR                                                     #
#Programa        => ACRC18                                                  #
#Objetivo        => Programa que cambia el orden de prioridad de las marcas #
#Autor:          => Mauricio Sanchez, EFP                                   #
#Fecha Inicio    => 19 Junio 2012                                           #
#############################################################################
DATABASE safre_viv

MAIN
 DEFINE v_ar_marcas DYNAMIC ARRAY OF RECORD
          tpo_originacion   LIKE cat_tipo_credito.tpo_originacion,
          originacion_desc  LIKE cat_cre_originacion.originacion_desc,
          modulo_desc       LIKE cat_cre_originacion.modulo_desc,
          prioridad_marca   LIKE cat_tipo_credito.prioridad_marca
        END RECORD,
        v_ar_marcas_aux DYNAMIC ARRAY OF RECORD
          tpo_originacion   LIKE cat_tipo_credito.tpo_originacion,
          originacion_desc  LIKE cat_cre_originacion.originacion_desc,
          modulo_desc       LIKE cat_cre_originacion.modulo_desc,
          prioridad_marca   LIKE cat_tipo_credito.prioridad_marca
        END RECORD,
        v_indice            SMALLINT,
        v_s_qryTxt          STRING,
        v_cta_marcas        SMALLINT,
        p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
        p_tipo_ejecucion    SMALLINT, -- forma como ejecutara el programa
        p_s_titulo          STRING -- titulo de la ventana
        
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".ACRC18.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   --se crea tabla temporal 
   CALL fn_crea_tbl_tmp_prioridad_marca()
   
   OPEN WINDOW w_marcas WITH FORM "ACRC181"

      CALL v_ar_marcas.clear()
      --se consulta si existen marcas
      SELECT COUNT (*)
      INTO v_cta_marcas
      FROM cat_cre_originacion

      LET v_indice = 1
      --se consultan los registros de las marcas
      LET v_s_qryTxt = " SELECT UNIQUE a.tpo_originacion, ",
                       "               b.originacion_desc,",
                       "               b.modulo_desc, ",
                       "               a.prioridad_marca ",
                       " FROM cat_tipo_credito a, cat_cre_originacion b ",
                       " WHERE a.tpo_originacion = b.tpo_originacion ",
                       " ORDER BY a.prioridad_marca "

      PREPARE prp_marca FROM v_s_qryTxt
      DECLARE cur_marca CURSOR FOR prp_marca
      FOREACH cur_marca INTO v_ar_marcas[v_indice].*
         LET v_indice = v_indice + 1
      END FOREACH

      FOR v_indice = 1 TO v_ar_marcas.getLength() - 1
         INSERT INTO safre_tmp:tmp_prioridad_marca VALUES (v_ar_marcas[v_indice].*)
      END FOR

      LET v_indice = 1
      LET v_s_qryTxt = " SELECT * ",       
                       "   FROM safre_tmp:tmp_prioridad_marca ",
                       "  ORDER BY prioridad_marca "
                       
      PREPARE prp_marca2 FROM v_s_qryTxt
      DECLARE cur_marca2 CURSOR FOR prp_marca2
      FOREACH cur_marca2 INTO v_ar_marcas_aux[v_indice].*
         LET v_indice = v_indice + 1
      END FOREACH
       
     
     DISPLAY ARRAY v_ar_marcas_aux TO tabla_marcas.* ATTRIBUTES (UNBUFFERED)
        BEFORE DISPLAY
           --si no se encontraton marcas
           IF v_cta_marcas = 0 THEN 
              CALL fn_mensaje("Aviso","No existen marcas a modificar","stop")
              EXIT DISPLAY
           END IF

        ON ACTION subir
           --el registro seleccionado sube y el de arriba baja, si es el primero no cambia
          { IF v_ar_marcas[arr_curr()].prioridad_marca < 1 THEN
              LET v_ar_marcas[arr_curr()].prioridad_marca = v_ar_marcas[arr_curr()].prioridad_marca - 1 
              LET v_ar_marcas[arr_curr() - 1].prioridad_marca = v_ar_marcas[arr_curr() - 1].prioridad_marca + 1
           END IF
           }
           IF v_ar_marcas_aux[arr_curr()].prioridad_marca = 2 THEN 
              LET v_ar_marcas_aux[arr_curr()].prioridad_marca = 1
              LET v_ar_marcas_aux[arr_curr() - 1].prioridad_marca = 2
           ELSE 
              IF v_ar_marcas_aux[arr_curr()].prioridad_marca = 3 THEN
                 LET v_ar_marcas_aux[arr_curr()].prioridad_marca = 2
                 LET v_ar_marcas_aux[arr_curr() - 1].prioridad_marca = 3
              END IF
           END IF    

           --se actualiza la prioridad de las marcas
           FOR v_indice = 1 TO v_ar_marcas.getLength()
              UPDATE safre_tmp:tmp_prioridad_marca
                 SET prioridad_marca = v_ar_marcas_aux[v_indice].prioridad_marca
               WHERE tpo_originacion = v_ar_marcas_aux[v_indice].tpo_originacion
           END FOR 
                      
           --se recarga el arreglo
           LET v_indice = 1
           CALL v_ar_marcas_aux.clear()
           FOREACH cur_marca2 INTO v_ar_marcas_aux[v_indice].*
              LET v_indice = v_indice + 1
           END FOREACH
           CONTINUE DISPLAY
        
        ON ACTION bajar
           --el registro seleccionado baja y el de abajo sube, si es el ultimo no cambia
{           IF v_ar_marcas[arr_curr()].prioridad_marca < v_ar_marcas.getLength() THEN
              LET v_ar_marcas[arr_curr()].prioridad_marca = v_ar_marcas[arr_curr()].prioridad_marca + 1 
              LET v_ar_marcas[arr_curr() + 1].prioridad_marca = v_ar_marcas[arr_curr() + 1].prioridad_marca - 1
           END IF
           }
           IF v_ar_marcas_aux[arr_curr()].prioridad_marca = 1 THEN 
              LET v_ar_marcas_aux[arr_curr()].prioridad_marca = 2
              LET v_ar_marcas_aux[arr_curr() + 1].prioridad_marca = 1
           ELSE 
              IF v_ar_marcas_aux[arr_curr()].prioridad_marca = 2 THEN
                 LET v_ar_marcas_aux[arr_curr()].prioridad_marca = 3
                 LET v_ar_marcas_aux[arr_curr() + 1].prioridad_marca = 2
              END IF
           END IF    

           --se actualiza la prioridad de las marcas
           FOR v_indice = 1 TO v_ar_marcas_aux.getLength()
              UPDATE safre_tmp:tmp_prioridad_marca
                 SET prioridad_marca = v_ar_marcas_aux[v_indice].prioridad_marca
               WHERE tpo_originacion = v_ar_marcas_aux[v_indice].tpo_originacion
           END FOR 
           
           --se recarga el arreglo
           LET v_indice = 1
           CALL v_ar_marcas_aux.clear()
           FOREACH cur_marca2 INTO v_ar_marcas_aux[v_indice].*
              LET v_indice = v_indice + 1
           END FOREACH
           CONTINUE DISPLAY
           
        ON ACTION ACCEPT
           FOR v_indice = 1 TO v_ar_marcas_aux.getLength()
              UPDATE cat_tipo_credito
                 SET prioridad_marca = v_ar_marcas_aux[v_indice].prioridad_marca,
                     f_actualiza = TODAY
               WHERE tpo_originacion = v_ar_marcas_aux[v_indice].tpo_originacion
           END FOR
           CALL fn_mensaje("Aviso","Se actualizó la prioridad de marcas","info") 
           EXIT DISPLAY 

        ON ACTION CANCEL
            EXIT DISPLAY
     END DISPLAY  
   CLOSE WINDOW w_marcas
             
END MAIN 

# Objetivo: Función que crea la tabla temporal de deudor
FUNCTION fn_crea_tbl_tmp_prioridad_marca()
   -- se declara la base de datos temporal
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

   DROP TABLE tmp_prioridad_marca 
   CREATE TABLE tmp_prioridad_marca(tpo_originacion SMALLINT,
                                    originacion_desc CHAR(40),
                                    modulo_desc CHAR(40),
                                    prioridad_marca SMALLINT)

   WHENEVER ERROR STOP

   -- regresa a la base de datos safre viv
   DATABASE safre_viv
END FUNCTION
