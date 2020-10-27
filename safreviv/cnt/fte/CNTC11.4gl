################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 23/01/2015                                      #
--------------------------------------------------------------------------------
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => CNT                                                       #
#Programa         => CNTC11                                                    #
#Objetivo         => Programa de consulta para la actualización del estado de  #
#                    la póliza contable para reenvío a confirmación por el     #
#                    sistema SAP - FICO).                                      #
#Fecha de Inicio  => 23/01/2015                                                #
################################################################################
DATABASE safre_viv
GLOBALS "CNTG01.4gl"

--Definición de variables globales
GLOBALS
  DEFINE f_fecha_inicial     DATE
  DEFINE f_fecha_final       DATE

  DEFINE a_valida_poliza     DYNAMIC ARRAY OF RECORD
    f_liquida                DATE,
    folio_liquida            DECIMAL(9,0),
    folio_cnt                DECIMAL(9,0),
    proceso                  CHAR(40),
    estado                   CHAR(35),
    marca                    SMALLINT,
    sh_folio_cnt             DECIMAL(9,0),
    sh_cod_proceso_cnt       SMALLINT,
    sh_cod_proceso           SMALLINT,
    sh_f_emision             DATE,
    sh_estado                SMALLINT
  END RECORD

  DEFINE f_ventana           ui.Window
  DEFINE f_forma             ui.Form
  DEFINE f_fecha             DATE
  DEFINE v_registros         SMALLINT
  
  DEFINE v_nodo              om.DomNode
  DEFINE v_tm                om.DomNode
  DEFINE v_tmg1              om.DomNode
  DEFINE v_tmc               om.DomNode
  DEFINE v_tmg2              om.DomNode
  DEFINE v_bandera_accion    CHAR(1)
  DEFINE v_respuesta         SMALLINT
  DEFINE v_tipo_reverso      SMALLINT
  DEFINE v_hora              DATETIME HOUR TO SECOND --Hora de registro
  
END GLOBALS

MAIN 
  CALL fn_despliega_polizas()
END MAIN

--Realiza consulta para desplegar las pólizas
FUNCTION fn_despliega_polizas()
  DEFINE i                   SMALLINT

  --Definición de variables de Reporte
  DEFINE v_err               SMALLINT
  DEFINE v_band_ent          SMALLINT
  
  --Se asignan los parametros que vienen del fglrun
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)

  --Se asigna el titulo del programa
  IF (g_nom_prog IS NOT NULL) THEN
     CALL ui.Interface.setText(g_nom_prog)
  END IF

  CLOSE WINDOW SCREEN
  
  OPEN WINDOW ventana_111 WITH FORM "CNTC111"
    DIALOG ATTRIBUTES (UNBUFFERED)
      INPUT BY NAME f_fecha_inicial, f_fecha_final
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()
          CALL f_forma.setElementHidden("grp_valida_poliza",TRUE)

          ON ACTION aceptar
             IF (f_fecha_inicial IS NULL) AND
                (f_fecha_final   IS NULL) THEN
                CALL fn_mensaje("Error","Ingrese al menos un criterio de búsqueda","information")
                NEXT FIELD f_fecha_inicial
             ELSE
                IF ((f_fecha_inicial IS NULL)      AND
                    (f_fecha_final   IS NOT NULL)) OR
                   ((f_fecha_inicial IS NOT NULL)  AND
                    (f_fecha_final   IS NULL))     THEN
                   CALL fn_mensaje("Error","Debe ingresar fecha inicial y final para realizar la búsqueda","information")
                   NEXT FIELD f_fecha_inicial
                END IF
             END IF

             --Valida que la fecha inicial no sea mayor a la final
             IF f_fecha_inicial > f_fecha_final THEN 
                CALL fn_mensaje("Error", "La fecha inicial no debe ser mayor a la fecha final.", "information")
                NEXT FIELD f_fecha_inicial
             END IF 

             --Valida que la fecha final no sea mayor a la actual
             IF f_fecha_final > TODAY THEN 
                CALL fn_mensaje("Error", "La fecha final no debe ser mayor a la fecha de hoy.", "information")
                NEXT FIELD f_fecha_final
             END IF 

             CALL f_forma.setElementHidden("grp_valida_poliza",FALSE)
             CALL a_valida_poliza.clear()

             --Realiza la consulta por un rango de periodo de fechas
             CALL fn_valida_polizas() 
             RETURNING v_registros

             IF v_registros <= 1 THEN
                CALL fn_mensaje("INFORMACIÓN","No existe información de pólizas","info")
                CALL a_valida_poliza.clear()
                CALL f_forma.setElementHidden("grp_valida_poliza",TRUE)
                NEXT FIELD f_fecha_inicial
             ELSE
                INPUT ARRAY a_valida_poliza WITHOUT DEFAULTS FROM record1.*
                ATTRIBUTES (APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE, UNBUFFERED, CANCEL = FALSE, ACCEPT = FALSE)

                  ON ACTION actualizar
                     LET v_band_ent = 0 
                     LET v_err      = 0

                     CALL fn_ventana_confirma("ACTUALIZA", 
                     "¿Está seguro que desea realizar la actualización del estado de la(s) póliza(s)?", 
                     "quest")
                     RETURNING v_respuesta

                     --Si el usuario confirma la ejecución de la actualización
                     IF v_respuesta = 1 THEN
                        LET v_tipo_reverso = 9 --Reenvío Póliza Contable
                     
                        FOR i = 1 TO a_valida_poliza.getLength()
                            IF a_valida_poliza[i].marca = 1 THEN
                               LET v_band_ent = 1 
                               DISPLAY "Póliza seleccionada: ", a_valida_poliza[i].folio_liquida

                               UPDATE cnt_transaccion 
                               SET    estado        = 10
                               WHERE  folio_liquida = a_valida_poliza[i].folio_liquida
                               AND    f_liquida     = a_valida_poliza[i].f_liquida
                               AND    folio_cnt     = a_valida_poliza[i].folio_cnt
                               AND    estado       IN (20,60)
                               IF SQLCA.sqlcode < 0 THEN
                                  LET v_err = 1
                                  CALL fn_mensaje("Aviso","Ocurrió un error al actualizar la(s) póliza(s)","stop") 
                                  EXIT INPUT
                               END IF

                               LET v_hora = CURRENT HOUR TO SECOND

                               --Inserta en la bitácora de reversos
                               INSERT INTO cnt_ctr_reverso VALUES(v_tipo_reverso,
                                                                  a_valida_poliza[i].folio_cnt,
                                                                  a_valida_poliza[i].folio_liquida,
                                                                  a_valida_poliza[i].sh_cod_proceso_cnt,
                                                                  a_valida_poliza[i].sh_cod_proceso,
                                                                  a_valida_poliza[i].f_liquida,
                                                                  a_valida_poliza[i].sh_f_emision,
                                                                  a_valida_poliza[i].sh_estado,
                                                                  g_usuario,
                                                                  TODAY,
                                                                  v_hora);
                            END IF
                        END FOR

                        IF v_band_ent = 0 THEN 
                           CALL fn_mensaje("Aviso","No se ha seleccionado ninguna póliza","")
                        ELSE
                           IF v_err = 0 THEN
                              CALL fn_mensaje("Aviso","Se han actualizado las pólizas seleccionadas","")
                              INITIALIZE a_valida_poliza TO NULL
                              EXIT PROGRAM
                           END IF
                        END IF
                     ELSE
                        CALL fn_mensaje("Aviso","Operación cancelada","stop") 
                        EXIT PROGRAM
                     END IF

                  ON ACTION CANCEL 
                     INITIALIZE a_valida_poliza TO NULL
                     EXIT PROGRAM
                     
                END INPUT
             END IF              
      END INPUT

      ON ACTION Cancelar
         EXIT DIALOG

    END DIALOG
  CLOSE WINDOW ventana_111

END FUNCTION

--Función para obtener las cuentas contables, folios y documentos contables y desplegarla en la tabla
FUNCTION fn_valida_polizas()
  DEFINE
    v_consulta               STRING,
    v_indx                   INT

  LET v_consulta = "\n SELECT UNIQUE T.f_liquida, ",
                   "\n        T.folio_liquida, ",
                   "\n        T.folio_cnt, ",
                   "\n        T.cod_proceso_cnt || '-' || P.desc_proceso_cnt, ",
                   "\n        T.estado || '-' || E.desc_estado_cnt, ",
                   "\n        0, ",
                   "\n        T.folio_cnt, ",
                   "\n        T.cod_proceso_cnt, ",
                   "\n        T.cod_proceso, ",
                   "\n        T.f_emision, ",
                   "\n        T.estado ",
                   "\n FROM   cnt_transaccion T ",
                   "\n INNER  JOIN cat_estado_cnt E ",
                   "\n ON     E.cod_estado_cnt  = T.estado ",
                   "\n INNER  JOIN cat_proceso_cnt P ",
                   "\n ON     T.cod_proceso_cnt = P.cod_proceso_cnt",
                   "\n WHERE  T.folio_liquida  IS NOT NULL ",
                   "\n AND    T.folio_cnt      <> 0        ",
                   "\n AND    T.estado         IN (20,60)  "

  --Se arma el query dinámico
  IF (f_fecha_inicial IS NOT NULL) AND (f_fecha_final IS NOT NULL) THEN
     LET v_consulta = v_consulta || "\n AND T.f_liquida BETWEEN '", f_fecha_inicial, "' AND '",f_fecha_final, "'"
  END IF

  LET v_consulta = v_consulta, "\n ORDER BY 1 DESC, 2 DESC, 3 DESC, 4 DESC, 5"

  DISPLAY "La consulta:  \n ", v_consulta

  LET v_indx = 1

  DECLARE valida_poliza CURSOR FROM v_consulta
  FOREACH valida_poliza INTO a_valida_poliza[v_indx].f_liquida,
                             a_valida_poliza[v_indx].folio_liquida,
                             a_valida_poliza[v_indx].folio_cnt,
                             a_valida_poliza[v_indx].proceso,
                             a_valida_poliza[v_indx].estado,
                             a_valida_poliza[v_indx].marca,
                             a_valida_poliza[v_indx].sh_folio_cnt,
                             a_valida_poliza[v_indx].sh_cod_proceso_cnt,
                             a_valida_poliza[v_indx].sh_cod_proceso,
                             a_valida_poliza[v_indx].sh_f_emision,
                             a_valida_poliza[v_indx].sh_estado
    IF a_valida_poliza[v_indx].folio_cnt IS NULL THEN
       LET a_valida_poliza[v_indx].folio_cnt = 0
    END IF
    
    LET a_valida_poliza[v_indx].marca = 0
    
    LET v_indx = v_indx + 1
  END FOREACH

  CALL a_valida_poliza.deleteElement(a_valida_poliza.getLength())

  DISPLAY "El index de la consulta -- ", v_indx

  RETURN v_indx

END FUNCTION