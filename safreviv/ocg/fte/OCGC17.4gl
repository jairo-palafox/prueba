#################################################################
#Modulo        => OCG                                           #
#Programa      => OCGC17                                        #
#Descripción   => Consulta histórica PROCESAR                   #
#Autor         => Emilio Abarca,EFP.                            #
#Fecha         => 07 SEPTIEMBRE 2017                            #
#################################################################

DATABASE safre_viv

GLOBALS

   DEFINE p_usuario               CHAR(20)
   DEFINE p_tipo_ejecucion        SMALLINT
   DEFINE p_s_titulo              STRING
   
END GLOBALS 

MAIN

   -- Recibe parámetros que envía el menú SACI-SAFRE.
   LET p_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   CLOSE WINDOW SCREEN 
   
   -- Crea log en caso de error.
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGC17.log")

   -- Si se obtuvo el titulo, se pone como titulo de programa.
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- Función principal
   CALL fn_consulta_historica()
   
END MAIN 

FUNCTION fn_consulta_historica()

   DEFINE v_nss              CHAR(11)
   DEFINE v_f_proceso        CHAR(10)
   DEFINE v_aux_f_proceso    CHAR(8)
   DEFINE bnd_nss            SMALLINT 
   DEFINE v_arma_cadena      STRING 
   DEFINE v_query            STRING 
   DEFINE i                  INTEGER
   DEFINE w                  ui.Window
   DEFINE f                  ui.Form
   
   DEFINE arr_historico DYNAMIC ARRAY OF RECORD
      nss             CHAR(11),
      f_proceso       DATE,
      operacion       CHAR(2) ,
      transaccion     CHAR(2) ,
      cod_operacion   CHAR(2) ,
      diagnostico1    CHAR(3) ,
      diagnostico2    CHAR(3) ,
      diagnostico3    CHAR(3) ,
      diagnostico4    CHAR(3) ,
      diagnostico5    CHAR(3) ,
      diagnostico6    CHAR(3)  
   END RECORD 

OPEN WINDOW vtn WITH FORM "OCGC171"

   LET w = ui.Window.getCurrent()
   LET f = w.getForm()

   # Oculta arreglo 
   CALL f.setElementHidden("group2",1)

   INPUT BY NAME v_nss,v_f_proceso ATTRIBUTE(UNBUFFERED,WITHOUT DEFAULTS)
      
      ON ACTION ACCEPT 
         # Valida campos
         IF(v_nss IS NULL) AND (v_f_proceso IS NULL) THEN 
            CALL fn_mensaje("","Debe ingresar por lo menos un parámetro de búsqueda","")
            NEXT FIELD v_nss
         END IF
         
         IF(v_nss IS NOT NULL) THEN 
            # Evalua longitud
            IF(LENGTH(v_nss) < 11) THEN
               CALL fn_mensaje("","El nss debe ser de 11 dígitos","")
               NEXT FIELD v_nss
            ELSE 
               # Evalúa que sea numérico
               LET bnd_nss = fn_es_numerico(v_nss)
               IF(bnd_nss = 1) THEN
                  CALL fn_mensaje("","El nss debe ser numérico","")
                  NEXT FIELD v_nss
               END IF 
            END IF 
         END IF 
   
         # Arma f_proceso (MESDIAAÑO)
         IF(v_f_proceso IS NOT NULL) THEN
            LET v_aux_f_proceso = v_f_proceso[4,5],v_f_proceso[1,2],v_f_proceso[7,10]
         END IF 
         
         # Al pasar el filtro de validaciones se arma cadena de búsqueda
         CASE
            WHEN (v_nss IS NOT NULL) AND (v_f_proceso IS NULL)
            LET v_arma_cadena = "nss = '"||v_nss||"'"
   
            WHEN (v_nss IS NULL) AND (v_f_proceso IS NOT NULL)
            LET v_arma_cadena = "f_proceso = '"||v_aux_f_proceso||"'"
   
            WHEN (v_nss IS NOT NULL) AND (v_f_proceso IS NOT NULL)
            LET v_arma_cadena = "nss = '"||v_nss||"' \n AND f_proceso = '"||v_aux_f_proceso||"'"
           
         END CASE 
   
         #Arma Query que extrae información
         LET v_query = "SELECT nss,\n
                               f_proceso,\n
                               operacion,\n
                               transaccion,\n
                               cod_operacion,\n
                               diagnostico1,\n
                               diagnostico2,\n
                               diagnostico3,\n
                               diagnostico4,\n
                               diagnostico5,\n
                               diagnostico6\n
                          FROM ocg_ctr_procesar
                         WHERE ",v_arma_cadena
                         
   
         PREPARE prp_cons_historica FROM v_query
         DECLARE crs_cons_historica CURSOR FOR prp_cons_historica
   
         LET i = 1
         CALL arr_historico.clear()
         
         FOREACH crs_cons_historica INTO arr_historico[i].nss,
                                          arr_historico[i].f_proceso,
                                          arr_historico[i].operacion,
                                          arr_historico[i].transaccion,
                                          arr_historico[i].cod_operacion,
                                          arr_historico[i].diagnostico1,
                                          arr_historico[i].diagnostico2,
                                          arr_historico[i].diagnostico3,
                                          arr_historico[i].diagnostico4,
                                          arr_historico[i].diagnostico5,
                                          arr_historico[i].diagnostico6
            LET i = i + 1
            
         END FOREACH 

         # Elimina la fila en blanco
         IF(arr_historico[arr_historico.getLength()].nss IS NULL) AND 
           (arr_historico[arr_historico.getLength()].f_proceso IS NULL) THEN
           CALL arr_historico.deleteElement(arr_historico.getLength()) 
         END IF 

         IF(i = 1) THEN
            CALL fn_mensaje("","No se encontraron registros con los parámetros ingresados","") 
            CONTINUE INPUT 
         ELSE 
            # Oculta parámetros de búsqueda
            CALL f.setElementHidden("group1",1)
            
            # Muestra arreglo Histórico Procesar
            CALL f.setElementHidden("group2",0)

            DISPLAY ARRAY arr_historico TO record1.* ATTRIBUTE(ACCEPT = FALSE)
            
               ON ACTION CANCEL 
                  # Oculta arreglo Histórico procesar
                  CALL f.setElementHidden("group2",1)

                  # Limpia el arreglo
                  CALL arr_historico.clear()

                  # Muestra parámetros de búsqueda
                  CALL f.setElementHidden("group1",0)
                  
                  # Limpia parámetros de entrada 
                  LET v_nss       = NULL
                  LET v_f_proceso = NULL

                  EXIT DISPLAY 
                  
            END DISPLAY 
            
         END IF 

         ON ACTION CANCEL 
            EXIT INPUT 
            
   END INPUT 
 
CLOSE WINDOW vtn

END FUNCTION 

FUNCTION fn_es_numerico(p_cadena)

   DEFINE p_cadena   STRING
   DEFINE v_idx      INTEGER
   DEFINE indicador  BOOLEAN

   FOR v_idx = 1 TO p_cadena.getLength()
      IF(p_cadena.subString(v_idx,v_idx) MATCHES '[0-9]') THEN
         LET indicador = 0
      ELSE
         LET indicador = 1
         IF(indicador == 1) THEN
            EXIT FOR
         END IF
      END IF
   END FOR

   RETURN indicador
   
END FUNCTION 