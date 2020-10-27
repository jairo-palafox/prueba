################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => LEY 72                                                   #
#Programa          => L72C01                                                   #
#Objetivo          => Consultar información de Ley 72 para que el usuario      #
#                     seleccione la operación a ejecutar                       #
#Fecha Inicio      => 23/05/2012                                               #
################################################################################
DATABASE safre_viv

GLOBALS "CTAC06.inc"
GLOBALS
DEFINE v_ind_estado_cuenta SMALLINT,
       v_desc_edo_cuenta   CHAR(19),
       v_f_estado_cuenta   DATE
END GLOBALS

#Variables para capturar los parametros que recibe la consulta
PRIVATE DEFINE p_usuario            CHAR(10)
PRIVATE DEFINE p_tipo_proc          CHAR(1)
PRIVATE DEFINE p_nombre_menu        CHAR(50)
PRIVATE DEFINE p_id_fondo           DECIMAL(9,0)

PRIVATE DEFINE v_saldo_total        DECIMAL(22,2)

#Variables para el filtro de busqueda
PRIVATE DEFINE v_datos                       datos_generales

#Lista para los movimientos de decreto
PRIVATE DEFINE v_lista_fondo DYNAMIC ARRAY OF movimientos_fondo

#Variables para el manejo de la pantalla
PRIVATE DEFINE ventana                       ui.Window
PRIVATE DEFINE forma                         ui.Form

MAIN
   DEFINE v_ciclo          SMALLINT

   LET v_ciclo = 1
   
   LET p_usuario            = ARG_VAL(1)
   LET p_tipo_proc          = ARG_VAL(2) -- Recibe el tipo de proceso
   LET p_nombre_menu        = ARG_VAL(3) -- Recibe el nombre del programa
   LET p_id_fondo           = ARG_VAL(4)



   IF p_id_fondo IS NOT NULL THEN
      LET v_datos.id_afi_fondo72 = p_id_fondo
   END IF

   CALL STARTLOG(p_usuario CLIPPED ||".CTAC06.log")

   CLOSE WINDOW SCREEN

   -- se asigna el titulo del programa
   LET p_nombre_menu = "Consulta de Fondo 72"
   CALL ui.Interface.setText(p_nombre_menu)

   OPEN WINDOW vtn_L72C011 WITH FORM "CTAC061"

      LET ventana = ui.Window.getCurrent()
      LET forma   = ventana.getForm()

      WHILE v_ciclo = 1
         IF v_datos.id_afi_fondo72 IS NULL THEN
            CALL fn_nueva_busqueda() RETURNING v_ciclo 
         END IF
         IF v_datos.id_afi_fondo72 IS NOT NULL THEN
            CALL fn_presenta_datos() RETURNING v_ciclo
         END IF
      END WHILE

   CLOSE WINDOW vtn_L72C011
END MAIN

PRIVATE FUNCTION fn_nueva_busqueda()
   DEFINE v_consulta_cliente              STRING
   DEFINE v_consulta_rfc                  STRING
   DEFINE v_condicion                     STRING
   DEFINE v_rfc_filtro                    STRING
   DEFINE v_rfc10                         CHAR(10)

   DEFINE v_respuesta                     INTEGER
   DEFINE i                               INTEGER
   DEFINE v_ind_seleccion                 INTEGER
   DEFINE v_ciclo                         INTEGER
   
   DEFINE v_lista_clientes         DYNAMIC ARRAY OF RECORD
      id_afi_fondo72               DECIMAL(9,0),
      nss                          CHAR(11),
      rfc                          CHAR(13),
      nombre_completo              VARCHAR(60),
      elige_registro               SMALLINT
   END RECORD

  DEFINE v_lista_clientes_1         DYNAMIC ARRAY OF RECORD
      id_afi_fondo72               DECIMAL(9,0),
      nss                          CHAR(11),
      rfc                          CHAR(13),
      nombre_completo              VARCHAR(60),
      elige_registro               SMALLINT
   END RECORD
   
   #Se inicializan las valiables del filtro
   INITIALIZE v_datos               TO NULL
   INITIALIZE v_lista_fondo         TO NULL
   INITIALIZE v_saldo_total         TO NULL

   #Ocultamos las secciones de las listas porque no tienen datos
   CALL forma.setElementHidden("gr_resumen_saldo",1)
   CALL forma.setElementHidden("gr_busqueda",     1)
   CALL forma.setElementHidden("gr_movimientos",  1)

   CONSTRUCT v_condicion ON nss, rfc, nombre
                         FROM nss, rfc, nombre
      BEFORE CONSTRUCT
         CLEAR FORM

      ON ACTION ACCEPT
         LET v_datos.nss    = GET_FLDBUF(nss)
         LET v_datos.rfc    = GET_FLDBUF(rfc)
         LET v_datos.nombre_completo    = GET_FLDBUF(nombre)
         
         LET INT_FLAG = FALSE

         IF v_datos.nss IS NULL AND
            v_datos.rfc IS NULL AND 
            v_datos.nombre_completo IS NULL THEN
            CALL fn_mensaje("Consulta Fondo 72",
                            "Debe de ingresar algún campo de búsqueda.",
                            "about")
            RETURN 1
         END IF
         ACCEPT CONSTRUCT
         
      ON ACTION CANCEL
         LET INT_FLAG = 1
         EXIT CONSTRUCT
         
   END CONSTRUCT

   #Si en la seccion de parametros de busqueda se selecciono aceptar pinta las siguientes secciones
   IF NOT INT_FLAG THEN
   
      #Se buscan los datos del cliente
      LET v_consulta_cliente =   "SELECT FIRST 51 ",
                                    "id_afi_fondo72, ",
                                    "nss, ",
                                    "rfc, ",
                                    "nombre ",
                                 "FROM afi_fondo72 ",
                                 "WHERE ", v_condicion

                                 DISPLAY v_consulta_cliente
                                 
      PREPARE exe_consulta_cliente FROM v_consulta_cliente
      DECLARE cur_consulta_cliente CURSOR FOR exe_consulta_cliente

      LET i = 1
      FOREACH cur_consulta_cliente INTO v_lista_clientes[i].*     
         LET i = i + 1
         IF i > MAX_REGISTROS THEN
            CALL fn_mensaje("Consulta Fondo 72",
                            "Acotar mas el criterio de búsqueda. \n"||
                            "Se muestran solo los primeros " || MAX_REGISTROS || " registros",
                            "about")
            EXIT FOREACH
         END IF
      END FOREACH
      CALL v_lista_clientes.deleteElement(v_lista_clientes.getLength())
      CLOSE cur_consulta_cliente
      FREE cur_consulta_cliente

      IF v_lista_clientes.getLength() = 0 THEN  #No se encontraron registros con el filtro original
         #Se valida que el campo RFC13 no sea nulo
         IF v_datos.rfc IS NOT NULL THEN
            LET v_rfc_filtro = v_datos.rfc CLIPPED
            IF v_rfc_filtro.getLength() > 10 THEN  #RFC > 10
               CALL fn_ventana_confirma("Atención",
                    "No existen registros con el criterio de búsqueda. \n"||
                         "Se ejecutara  la consulta con el RFC a 10 posiciones",
                     "quest") RETURNING v_respuesta
               IF v_respuesta = 1 THEN #Se ejecuta la consulta con RFC10
                  LET v_rfc10 = v_rfc_filtro.subString(1,10)
                  LET v_consulta_rfc = "SELECT FIRST 51 ",
                                    "id_afi_fondo72, ",
                                    "nss, ",
                                    "rfc, ",
                                    "nombre ",
                                 "FROM afi_fondo72 ",
                                 "WHERE rfc[1,10] = ?" 
                  PREPARE exe_consulta_rfc FROM v_consulta_rfc
                  DECLARE cur_consulta_rfc CURSOR FOR exe_consulta_rfc
                  LET i = 1
                  FOREACH cur_consulta_rfc USING v_rfc10 INTO v_lista_clientes[i].*

                     LET i = i + 1
                     IF i > MAX_REGISTROS THEN
                        CALL fn_mensaje("Consulta Fondo 72",
                                        "Acotar mas el criterio de búsqueda. \n"||
                                        "Se muestran solo los primeros " || MAX_REGISTROS || " registros",
                                        "about")                                        
                        EXIT FOREACH
                     END IF
                  END FOREACH
                  CALL v_lista_clientes.deleteElement(v_lista_clientes.getLength())
                  CLOSE cur_consulta_rfc
                  FREE cur_consulta_rfc
               END IF   #FIN consulta con RFC10
            END IF   #FIN VAlidacion de tamaño para RFC
         END IF   #FIN RFC no nulo
      END IF   #FIN no se encontraron registros con el filtro original

      IF v_lista_clientes.getLength() > 0 THEN
         IF v_lista_clientes.getLength() < 1 THEN
            CALL fn_mensaje("Consulta Ley 72", "No se encontro información con los parámetros proporcionados","about");
         ELSE
            #Si se encotro mas de un cliente con el filtro de busqueda se muestra la lista para que el usuario seleccione a un cliente
            CALL forma.setElementHidden("gr_busqueda",       0)
            DIALOG ATTRIBUTES(UNBUFFERED)
               DISPLAY ARRAY v_lista_clientes TO scr_busqueda.*
   
               BEFORE ROW 
                  LET INT_FLAG = FALSE
                  LET v_datos.id_afi_fondo72 = v_lista_clientes[ARR_CURR()].id_afi_fondo72
                  CALL fn_presenta_datos() RETURNING v_ciclo
                  
               BEFORE DISPLAY 
                  CALL forma.setElementHidden("gr_resumen_saldo",  0)
                  CALL forma.setElementHidden("gr_movimientos",    0)
         
                  ON ACTION ACCEPT 
                     LET INT_FLAG = FALSE
                     LET v_datos.id_afi_fondo72 = v_lista_clientes[ARR_CURR()].id_afi_fondo72
                     CALL fn_presenta_datos() RETURNING v_ciclo

                  ON ACTION cancelar 
                     INITIALIZE v_datos       TO NULL
                     EXIT DIALOG 
                  --###  Botón que ejecuta la función de unificación
                  --ON ACTION unificacion 
                     --CALL fn_unifica_ley72(v_condicion, p_usuario)
                  --###  Botón que ejecuta la función de separación
                  --ON ACTION separacion 
                     --CALL fn_mensaje ("Atencion", "Se ejecutará la separación", "About")
                  --###  Botón que ejecuta la función de separación
                  --ON ACTION modificacion 
                     --CALL fn_mensaje ("Atencion", "Se ejecutará la modificación", "About")

                    AFTER DISPLAY 
                    DISPLAY "abc"
                    
               END DISPLAY 

               
                  DISPLAY ARRAY v_lista_fondo TO scr_det_movimientos.*
                  --DISPLAY v_lista_fondo 
                     ON ACTION CANCEL 
                        EXIT DIALOG 
                  END DISPLAY               

            END DIALOG         
            CALL v_lista_clientes.deleteElement(v_lista_clientes.getLength())  
         END IF
                     
      ELSE
         CALL fn_mensaje("Consulta Fondo 72",
                         "No existen registros con el criterio de búsqueda. \n",
                         "about")
      END IF
      RETURN 1
   ELSE
      RETURN 0
   END IF
END FUNCTION

PRIVATE FUNCTION fn_presenta_datos()
   #primero se buscan los datos generales del cliente
   CALL fn_datos_generales()

   #Se ejecuta la funcion que consulta los movimientos de fondo 72
   CALL fn_consulta_fondo()

   DISPLAY v_datos.nss              TO nss
   DISPLAY v_datos.rfc              TO rfc
   DISPLAY v_datos.nombre_completo  TO nombre
   DISPLAY v_saldo_total            TO saldo
   DISPLAY v_desc_edo_cuenta        TO ed_estado_cta
   --DISPLAY v_f_estado_cuenta        TO ed_fecha_edo
   DISPLAY v_f_estado_cuenta        USING "dd-mm-yyyy" TO ed_fecha_edo  

   RETURN 1
END FUNCTION

PRIVATE FUNCTION fn_datos_generales()
   SELECT 
          id_afi_fondo72,
          nss,
          rfc,
          nombre
     INTO v_datos.*
     FROM afi_fondo72
    WHERE id_afi_fondo72 = v_datos.id_afi_fondo72
END FUNCTION

#OBJETIVO: Consultar información del detalle de movimientos
PRIVATE FUNCTION fn_consulta_fondo()
   DEFINE v_consulta_fondo       STRING
   DEFINE i                      SMALLINT

   LET v_consulta_fondo =   "\n SELECT mov.id_afi_fondo72, ",
                            "\n        mov.f_liquida, ",
                            "\n        mov.folio_liquida, ",
                            "\n        mov.movimiento || ' - ' || TRIM(cat.movimiento_desc), ",
                            "\n        mov.origen, ",
                            "\n        mov.importe ",
                            "\n FROM   cta_fondo72 mov ",
                            "\n LEFT JOIN cat_movimiento cat ",
                            "\n ON cat.movimiento = mov.movimiento ",
                            "\n WHERE mov.id_afi_fondo72 = ", v_datos.id_afi_fondo72 ,
                            "\n ORDER BY mov.f_liquida DESC"
--DISPLAY v_consulta_fondo
   PREPARE exe_consulta_fondo FROM v_consulta_fondo
   DECLARE cur_consulta_fondo CURSOR FOR exe_consulta_fondo
   
   LET i = 1
   FOREACH cur_consulta_fondo INTO v_lista_fondo[i].*
      LET i = i + 1
      IF i > MAX_REGISTROS THEN
         CALL fn_mensaje("Consulta Fondo 72",
                         "Solo se mostrara los ultimos " || MAX_REGISTROS || " movimientos de Fondo 72.",
                         "about")
      END IF
   END FOREACH
   CALL v_lista_fondo.deleteElement(v_lista_fondo.getLength())
   CLOSE cur_consulta_fondo
   FREE cur_consulta_fondo

   SELECT SUM(mov.importe)
   INTO v_saldo_total
   FROM cta_fondo72 mov
   WHERE mov.id_afi_fondo72 = v_datos.id_afi_fondo72
   AND mov.movimiento <> 422  #Omitimos el CARGO RETIRO FONDO 72-92, TANTO ADICIONAL

   SELECT ind_estado_cuenta,
          f_estado_cuenta
   INTO   v_ind_estado_cuenta,
          v_f_estado_cuenta
   FROM   afi_fondo72
   WHERE  id_afi_fondo72 = v_datos.id_afi_fondo72

   IF v_ind_estado_cuenta = 0 THEN 
      LET v_desc_edo_cuenta = "0 - CUENTA ACTIVA"
   END IF 
   IF v_ind_estado_cuenta = 1 THEN 
      LET v_desc_edo_cuenta = "1 - CUENTA INACTIVA"
   END IF
   

END FUNCTION