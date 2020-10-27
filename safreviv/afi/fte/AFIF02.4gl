DATABASE safre_viv

{
======================================================================
Nombre: fn_modificar_datos
Fecha creacion: Mayo 24, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Abre una ventana para realizar la modificacion de datos de
un derechohabiente en Fondo 72

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     12 Ago 2013             - si el campo ind_estado_cuenta de afi_fondo72
                                        es igual a 1, esta cuenta no se puede modificar
Ivan Vega     18 Sep 2013             - si hay cambio de RFC y el nuevo ya existe en base
                                        de datos, no se permite crear la nueva cuenta
                                      - se debe poder generar mas de un proceso a la vez
                                        porque es captura en linea
======================================================================
}

FUNCTION fn_modificar_datos(v_id_afi_fondo72, v_nss_original, v_rfc_original, v_nombre_original)

   DEFINE v_id_afi_fondo72        LIKE afi_fondo72.id_afi_fondo72
   DEFINE v_nss_original          LIKE afi_fondo72.nss -- NSS original
   DEFINE v_rfc_original          LIKE afi_fondo72.rfc -- RFC original
   DEFINE v_nombre_original       LIKE afi_fondo72.nombre -- Nombre original
   DEFINE v_nss_modificado        LIKE afi_fondo72.nss -- NSS modificado por usuario
   DEFINE v_rfc_modificado        LIKE afi_fondo72.rfc -- RFC modificado por usuario
   DEFINE v_id_afi_fondo72_nuevo  LIKE afi_fondo72.id_afi_fondo72 -- id afi de la cuenta nueva
   DEFINE v_nombre_modificado     LIKE afi_fondo72.nombre -- Nombre modificado por usuario
   DEFINE v_nombre_concatenado    LIKE afi_fondo72.nombre -- Nombre concatenado de los datos del RFC
   DEFINE v_apellido_paterno      STRING -- apellido paterno modificado
   DEFINE v_apellido_materno      STRING -- apellido materno modificado
   DEFINE v_nombres               STRING -- nombre modificado
   DEFINE v_id_derechohabiente    LIKE afi_derechohabiente.id_derechohabiente -- id derechohabiente encontrado
   DEFINE v_mensaje               STRING
   DEFINE v_ruta                  STRING
   DEFINE v_ruta_formulario       VARCHAR(40) -- ruta de afi/bin 
   DEFINE v_ruta_bin              VARCHAR(40) -- ruta de glo/bin
   DEFINE v_ruta_listados         VARCHAR(40) -- ruta de safreviv_lst/glo/
   DEFINE v_nss_es_correcto       SMALLINT -- booleana que indica que un NSS es correcto
   DEFINE v_rfc_es_correcto       SMALLINT -- booleana que indica que un RFC es correcto
   DEFINE v_nombre_es_correcto    SMALLINT -- booleana que indica que un NOMBRE es correcto
   DEFINE v_confirma_cambio       SMALLINT -- booleana que confirma si se cambiaron datos
   DEFINE v_clave_cambio          SMALLINT -- clave del campo cambiado
   DEFINE v_cambio_nombre         SMALLINT
   DEFINE v_cambio_nss            SMALLINT
   DEFINE v_cambio_rfc            SMALLINT
   DEFINE v_ind_estado_cuenta     LIKE afi_fondo72.ind_estado_cuenta

   DEFINE v_r_movimientos RECORD
      f_movimiento                LIKE cta_fondo72.f_liquida,
      folio                       LIKE cta_fondo72.folio_liquida,
      des_movimiento              VARCHAR(200),
      origen                      LIKE cta_fondo72.origen,
      monto_pesos                 LIKE cta_fondo72.importe,
      subcuenta                   LIKE cta_fondo72.subcuenta,
      movimiento                  LIKE cta_fondo72.movimiento
   END RECORD

   DEFINE v_arr_mov_original DYNAMIC ARRAY OF RECORD
      f_movimiento                LIKE cta_fondo72.f_liquida,
      folio                       LIKE cta_fondo72.folio_liquida,
      des_movimiento              VARCHAR(200),
      origen                      LIKE cta_fondo72.origen,
      monto_pesos                 LIKE cta_fondo72.importe,
      subcuenta                   LIKE cta_fondo72.subcuenta,
      movimiento                  LIKE cta_fondo72.movimiento
   END RECORD

   DEFINE v_arr_movs_nuevo DYNAMIC ARRAY OF RECORD
      f_movimiento                LIKE cta_fondo72.f_liquida,
      folio                       LIKE cta_fondo72.folio_liquida,
      des_movimiento              VARCHAR(200),
      origen                      LIKE cta_fondo72.origen,
      monto_pesos                 LIKE cta_fondo72.importe,
      subcuenta                   LIKE cta_fondo72.subcuenta,
      movimiento                  LIKE cta_fondo72.movimiento
   END RECORD

   DEFINE li_i                    SMALLINT -- contador para movimientos a cuenta nueva
   DEFINE li_j                    SMALLINT -- contador para movimientos de cuenta original
   DEFINE v_sql                   STRING
   DEFINE v_saldo_original        DECIMAL(22,2) 
   DEFINE v_saldo_modificado      DECIMAL(22,2)
   DEFINE v_saldo_positivo_or     DECIMAL(22,2)
   DEFINE v_saldo_negativo_or     DECIMAL(22,2)
   DEFINE v_ind_estado_cuenta_or  SMALLINT
   DEFINE v_ind_estado_cuenta_nu  SMALLINT
   DEFINE v_estado_cta_original   STRING
   DEFINE v_estado_cta_modificado STRING
   DEFINE v_comando               STRING -- cadena con comando para ejecutar de consola
   DEFINE v_proceso_cod           SMALLINT -- codigo de proceso
   DEFINE v_opera_cod             SMALLINT -- numero de operacion
   DEFINE v_pid                   DECIMAL(9,0) -- PID del proceso
   DEFINE v_folio                 LIKE glo_folio.folio -- folio del proceso
   DEFINE v_resultado             SMALLINT -- para monitorear control de procesos
   DEFINE v_r_cta_his_fondo72     RECORD LIKE cta_his_fondo72.* -- registro de historico cta fondo72 en aclaracion
   DEFINE v_r_cta_aclara_fondo72  RECORD LIKE cta_aclara_fondo72.* -- registro cta_fondo72 en aclaracion
   DEFINE v_rfc_buscado           LIKE afi_fondo72.rfc -- para validar existencia de rfc

   -- se asigna proceso y operacion
   LET v_proceso_cod = 1810 -- aclaracion de cuentas de fondo 72
   LET v_opera_cod   = 1 -- liquidacion
   LET v_pid         = 0
   LET v_folio       = 0
   LET v_resultado   = 0

   -- se obtiene la ruta bin de afi
   SELECT ruta_bin
     INTO v_ruta_formulario
     FROM seg_modulo
    WHERE modulo_cod = "afi"

   SELECT ruta_bin
     INTO v_ruta_bin
     FROM seg_modulo
    WHERE modulo_cod = "glo"

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = "bat"

   LET v_ruta = v_ruta_formulario CLIPPED, "/AFIF021"

   -- se duplican los datos originales en los espacios para modificacion
   LET v_nss_modificado    = v_nss_original   
   LET v_rfc_modificado    = v_rfc_original   
   LET v_nombre_modificado = v_nombre_original

   -- se verifica el estatus de la cuenta que se desea modificar
   SELECT ind_estado_cuenta
     INTO v_ind_estado_cuenta
     FROM afi_fondo72
    WHERE id_afi_fondo72 = v_id_afi_fondo72

   -- si el estado de la cuenta es 1, esta inactiva y no puede ser modificada
   IF ( v_ind_estado_cuenta = 1 ) THEN
      CALL fn_mensaje("Atención", "La cuenta se encuentra inactiva y no puede ser modificada","stop")
      RETURN v_nss_modificado, v_rfc_modificado, v_nombre_modificado
   END IF

   -- se abre la ventana para modificar los datos
   OPEN WINDOW w_modificar WITH FORM v_ruta

   DISPLAY BY NAME v_id_afi_fondo72, v_nss_original, v_rfc_original, v_nombre_original

   INPUT BY NAME v_nss_modificado, v_rfc_modificado, v_nombre_modificado,
                 v_apellido_paterno, v_apellido_materno, v_nombres

   WITHOUT DEFAULTS 
   ATTRIBUTES ( UNBUFFERED )

      AFTER FIELD v_nombres
         NEXT FIELD v_nss_modificado

      ON ACTION accept

         -- se obtiene el nombre concatenado
         LET v_nombre_concatenado = v_apellido_paterno.trim(), " ", v_apellido_materno.trim(), " ", v_nombres.trim()
         DISPLAY "Nombre concatenado al iniciar validaciones: ", v_nombre_concatenado

         -- se valida si el NSS cambio
         IF ( v_nss_original <> v_nss_modificado ) THEN
            -- se verifica que este bien construido
            CALL fn_valida_nss(v_nss_modificado) RETURNING v_nss_es_correcto, v_nss_modificado
            DISPLAY BY NAME v_nss_modificado
            CALL ui.interface.refresh()

            IF ( NOT v_nss_es_correcto ) THEN
               CALL fn_mensaje("Error","El NSS no cumple con las validaciones de estructura","stop")
               CONTINUE INPUT
            END IF
         END IF

         -- se verifica si el RFC cambio
         -- el RFC debe ser de 10 o 13 caracteres de longitud
         IF ( LENGTH(v_rfc_modificado) <> 10 AND LENGTH(v_rfc_modificado) <> 13 ) THEN
            CALL fn_mensaje("Error","El RFC debe tener una longitud de 10 o 13 caracteres","stop")
            CONTINUE INPUT
         ELSE
            -- si cambio el RFC
            IF ( v_rfc_original <> v_rfc_modificado ) THEN

               -- si modifico el RFC debe capturar al menos el apellido paterno y el nombre
               IF ( v_apellido_paterno IS NULL OR v_nombres IS NULL ) THEN
                  CALL fn_mensaje("Error","Para poder validar el RFC es necesario que capture al menos el apellido paterno\ny el nombre del derechohabiente","stop")
                  CONTINUE INPUT
               END IF

               -- se verifica que este bien construido
               CALL fn_valida_rfc(v_rfc_modificado, v_apellido_paterno, v_apellido_materno, v_nombres)
                    RETURNING v_rfc_es_correcto, v_rfc_modificado
                    DISPLAY BY NAME v_rfc_modificado
                    CALL ui.interface.refresh()

               IF ( NOT v_rfc_es_correcto ) THEN
                  CALL fn_mensaje("Error","El RFC no cumple con las validaciones de estructura","stop")
                  CONTINUE INPUT
               END IF
            END IF
         END IF

         -- se verifica si el nombre cambio
         IF ( LENGTH(v_nombre_concatenado CLIPPED) > 0 ) THEN
            IF ( v_nombre_original <> v_nombre_concatenado OR v_apellido_paterno IS NOT NULL OR
                 v_apellido_materno IS NOT NULL OR v_nombres IS NOT NULL ) THEN

               -- verifica si el nombre se capturo por separado para validar el rfc
               IF ( v_apellido_paterno IS NULL OR v_nombres IS NULL ) THEN
                  CALL fn_mensaje("Error","El nombre ha sido modificado, es necesario\nque lo escriba por separado en apellidos y nombre","stop")
                  CONTINUE INPUT
               ELSE
                  -- se verifica que este bien construido
                  CALL fn_valida_nombre(v_nombre_concatenado) RETURNING v_nombre_es_correcto, v_nombre_concatenado
                  --DISPLAY BY NAME v_nombre_modificado
                  --CALL ui.interface.refresh()

                  IF ( NOT v_nombre_es_correcto ) THEN
                     CALL fn_mensaje("Error","El nombre no cumple con las validaciones de estructura","stop")
                     CONTINUE INPUT
                  END IF

                  -- se verifica que el RFC corresponda al nombre              
                  CALL fn_valida_rfc(v_rfc_modificado, v_apellido_paterno, v_apellido_materno, v_nombres)
                       RETURNING v_rfc_es_correcto, v_rfc_modificado
                       DISPLAY BY NAME v_rfc_modificado
                       CALL ui.interface.refresh()

                  IF ( NOT v_rfc_es_correcto ) THEN
                     CALL fn_mensaje("Error","El RFC no cumple con las validaciones de estructura","stop")
                     CONTINUE INPUT
                  END IF

                  -- se valida que la homoclave sea correcta
                  CALL fn_valida_homoclave_rfc(v_rfc_modificado, v_apellido_paterno, v_apellido_materno, v_nombres)
                       RETURNING v_rfc_es_correcto, v_rfc_modificado
                       DISPLAY BY NAME v_rfc_modificado
                       CALL ui.interface.refresh()

                  IF ( NOT v_rfc_es_correcto ) THEN
                     CALL fn_mensaje("Error","El RFC no cumple con las validaciones de estructura","stop")
                     CONTINUE INPUT
                  END IF

                  -- se verifica el digito verificador del RFC
                  CALL fn_valida_digitoverificador_rfc(v_rfc_modificado)
                       RETURNING v_rfc_es_correcto, v_rfc_modificado
                       DISPLAY BY NAME v_rfc_modificado
                       CALL ui.interface.refresh()
   
                  IF ( NOT v_rfc_es_correcto ) THEN
                     CALL fn_mensaje("Error","El RFC no cumple con las validaciones de estructura","stop")
                     CONTINUE INPUT
                  END IF
               END IF
            END IF
         ELSE
            -- pudo haber cambio de rfc/nss, el nombre quedo igual. El nombre concatenado se toma igual
            -- al original
            LET v_nombre_concatenado = v_nombre_original
         END IF

         -- ===========================================================================
         -- si las validaciones son correctas se modifica el registro en base de datos

         -- se solicita confirmacion al usuario para efectuar los cambios en el registro en base de datos
         LET v_mensaje = "La validación de los datos modificados ha concluido exitosamente,",
                         "\npara continuar y realizar el cambio en base de datos es necesario",
                         "\nconfirmar la operación.",
                         "\n\n¿Desea realizar los cambios en la base de datos?"
                         
         MENU "Confirmar modificación"
         ATTRIBUTES ( STYLE = "dialog", COMMENT = v_mensaje, IMAGE = "question" )
            COMMAND "Aceptar"

               -- se verifica que se cambio
               IF ( v_nss_original <> v_nss_modificado ) THEN
                  LET v_cambio_nss = TRUE
               ELSE
                  LET v_cambio_nss = FALSE
               END IF

               IF ( v_rfc_original <> v_rfc_modificado ) THEN
                  LET v_cambio_rfc = TRUE
               ELSE
                  LET v_cambio_rfc = FALSE
               END IF

               IF ( v_nombre_original <> v_nombre_concatenado ) THEN
                  LET v_cambio_nombre = TRUE
               ELSE
                  LET v_cambio_nombre = FALSE
               END IF

               -- si hubo cambio de RFC y ya existe un NSS/RFC igual al que se acaba de capturar, se impide el cambio
               IF ( v_cambio_rfc OR v_cambio_nss ) THEN
                  SELECT MAX(rfc)
                  INTO   v_rfc_buscado
                  FROM   afi_fondo72
                  WHERE  rfc = v_rfc_modificado
                  AND    nss = v_nss_modificado

                  -- si existe, se impide
                  IF ( v_rfc_buscado IS NOT NULL ) THEN
                     LET v_mensaje = "Ya existe una cuenta con el mismo NSS-RFC en base de datos.\nNo es posible continuar con la opreación."

                     CALL fn_mensaje("Aclaraciones", v_mensaje, "stop")
                     EXIT MENU
                  END IF
               END IF

               display "NSS, RFC, NOMBRE: ", v_cambio_nss, v_cambio_rfc, v_cambio_nombre

               -- se cambiaron los tres
               IF ( v_cambio_nss AND v_cambio_rfc AND v_cambio_nombre ) THEN
                  LET v_clave_cambio = 13 -- 13 - CAMBIO RFC NSS NOMB
               ELSE
                  -- cambio nss y rfc
                  IF ( v_cambio_nss AND v_cambio_rfc AND (NOT v_cambio_nombre)) THEN
                     LET v_clave_cambio = 10 -- NSS y RFC
                     LET v_nombre_concatenado = v_nombre_original
                  ELSE
                     -- cambio nss y nombre
                     IF ( v_cambio_nss AND (NOT v_cambio_rfc) AND v_cambio_nombre) THEN
                        LET v_clave_cambio = 12 -- NSS y NOMBRE
                     ELSE
                        -- cambio rfc y nombre
                        IF ( (NOT v_cambio_nss) AND v_cambio_rfc AND v_cambio_nombre) THEN
                           LET v_clave_cambio = 11 -- RFC y NOMBRE
                        ELSE
                           -- cambio nss
                           IF ( v_cambio_nss AND (NOT v_cambio_rfc) AND (NOT v_cambio_nombre)) THEN
                              LET v_clave_cambio = 8 -- nss
                              LET v_nombre_concatenado = v_nombre_original
                           ELSE
                              -- cambio rfc
                              IF ( (NOT v_cambio_nss) AND v_cambio_rfc AND (NOT v_cambio_nombre)) THEN
                                 LET v_clave_cambio = 2 -- RFC
                                 LET v_nombre_concatenado = v_nombre_original
                              ELSE
                                 -- cambio el nombre
                                 LET v_clave_cambio = 9 -- NOMBRE
                              END IF
                           END IF
                        END IF
                     END IF
                  END IF
               END IF

               -- si cambio el NSS o RFC, se debe generar un registro nuevo
               IF ( v_cambio_nss OR v_cambio_rfc ) THEN
                  -- se crea un registro nuevo de afi fondo72
                  LET v_mensaje = "ATENCION\n\n",
                                  "El ajuste realizado a la cuenta afectó el NSS o el RFC,\n",
                                  "por lo que será necesario crear una cuenta nueva con estos\n",
                                  "datos e inhabilitar la original."

                  -- se muestra el mensaje
                  CALL fn_mensaje("Modificación de datos", v_mensaje, "question")

                  -- se abre la ventana para mostrar como quedaria
                  LET v_ruta = v_ruta_formulario CLIPPED, "/AFIF022"
                  OPEN WINDOW w_vistaprevia WITH FORM v_ruta

                  -- consulta de movimientos
                  LET v_sql =   "\n SELECT mov.f_liquida, ",
                                "\n        mov.folio_liquida, ",
                                "\n        mov.movimiento || ' - ' || TRIM(cat.movimiento_desc), ",
                                "\n        mov.origen,    ",
                                "\n        mov.importe,   ",
                                "\n        mov.subcuenta, ",
                                "\n        mov.movimiento ",
                                "\n FROM   cta_fondo72 mov ",
                                "\n LEFT JOIN cat_movimiento cat ",
                                "\n ON cat.movimiento = mov.movimiento ",
                                "\n WHERE mov.id_afi_fondo72 = ", v_id_afi_fondo72,
                                "\n ORDER BY mov.f_liquida DESC"

                  PREPARE sid_consmovs FROM v_sql

                  DECLARE cur_consmovs CURSOR FOR sid_consmovs

                  -- se obtienen los movimientos de la cuenta original
                  LET li_i = 1
                  LET li_j = 1
                  LET v_saldo_original = 0
                  LET v_saldo_modificado = 0
                  LET v_saldo_positivo_or = 0
                  LET v_saldo_negativo_or = 0

                  CALL v_arr_mov_original.clear()
                  CALL v_arr_movs_nuevo.clear()

                  -- se pasan los movimientos al arreglo de despligue de la cuenta original
                  FOREACH cur_consmovs INTO v_r_movimientos.*
                     -- se transfieren los datos al arreglo de despliegue de movimientos originales
                     LET v_arr_mov_original[li_j].* = v_r_movimientos.*

                     LET li_j = li_j + 1
                  END FOREACH

                  -- el saldo de la cuenta se muestra como un cargo/abono segun sea
                  -- se obtienen los movimientos de la cuenta nueva
                  SELECT NVL(SUM(mov.importe),0)
                  INTO   v_saldo_original
                  FROM   cta_fondo72 mov
                  WHERE  mov.id_afi_fondo72 = v_id_afi_fondo72
                  AND    mov.movimiento <> 422  #Omitimos el CARGO RETIRO FONDO 72-92, TANTO ADICIONAL

                  -- el saldo de la cuenta destino es el mismo de la original
                  LET v_saldo_modificado = v_saldo_original

                  IF ( v_saldo_original > 0 ) THEN
                     -- cargo en cuenta original
                     DISPLAY "se agrega movimiento de cargo a cuenta original"

                     LET v_arr_mov_original[li_j].f_movimiento   = TODAY
                     LET v_arr_mov_original[li_j].folio          = 0
                     LET v_arr_mov_original[li_j].des_movimiento = "1412-CARGO ACLARACIÓN DATOS FONDO 72"
                     LET v_arr_mov_original[li_j].origen         = "CTAC04"
                     LET v_arr_mov_original[li_j].monto_pesos    = v_saldo_original * (-1)
                     LET v_arr_mov_original[li_j].subcuenta      = v_r_movimientos.subcuenta
                     LET v_arr_mov_original[li_j].movimiento     = 1412

                     -- abono en cuenta destino
                     LET v_arr_movs_nuevo[li_i].f_movimiento    = TODAY
                     LET v_arr_movs_nuevo[li_i].folio           = 0
                     LET v_arr_movs_nuevo[li_i].des_movimiento  = "241-ABONO ACLARACIÓN DATOS FONDO 72"
                     LET v_arr_movs_nuevo[li_i].origen          = "CTAC04"
                     LET v_arr_movs_nuevo[li_i].monto_pesos     = v_saldo_original
                     LET v_arr_movs_nuevo[li_i].subcuenta       = v_r_movimientos.subcuenta
                     LET v_arr_movs_nuevo[li_i].movimiento      = 241
                  ELSE
                     -- abono en cuenta original
                     DISPLAY "se agrega movimiento de abono a cuenta original"

                     LET v_arr_mov_original[li_j].f_movimiento   = TODAY
                     LET v_arr_mov_original[li_j].folio          = 0
                     LET v_arr_mov_original[li_j].des_movimiento = "241-ABONO ACLARACIÓN DATOS FONDO 72"
                     LET v_arr_mov_original[li_j].origen         = "CTAC04"
                     LET v_arr_mov_original[li_j].monto_pesos    = v_saldo_original * (-1)
                     LET v_arr_mov_original[li_j].subcuenta      = v_r_movimientos.subcuenta
                     LET v_arr_mov_original[li_j].movimiento     = 241

                     -- cargo en cuenta destino
                     LET v_arr_movs_nuevo[li_i].f_movimiento   = TODAY
                     LET v_arr_movs_nuevo[li_i].folio          = 0
                     LET v_arr_movs_nuevo[li_i].des_movimiento = "1412-CARGO ACLARACIÓN DATOS FONDO 72"
                     LET v_arr_movs_nuevo[li_i].origen         = "CTAC04"
                     LET v_arr_movs_nuevo[li_i].monto_pesos    = v_saldo_original
                     LET v_arr_movs_nuevo[li_i].subcuenta      = v_r_movimientos.subcuenta
                     LET v_arr_movs_nuevo[li_i].movimiento     = 1412
                  END IF

                  -- una vez balanceado, el saldo de la cuenta original debe ser cero
                  LET v_saldo_original = 0

                  -- la cuenta original se inhabilita
                  LET v_ind_estado_cuenta_or = 1
                  LET v_estado_cta_original = "1 - CUENTA INACTIVA"

                  -- la cuenta nueva se queda activa
                  LET v_ind_estado_cuenta_nu = 0
                  LET v_estado_cta_modificado = "0 - CUENTA ACTIVA"

                  LET v_nombre_modificado = v_nombre_concatenado

                  DISPLAY BY NAME v_nss_original, v_rfc_original, v_nombre_original,
                                  v_nss_modificado, v_rfc_modificado, v_nombre_modificado,
                                  v_saldo_original, v_estado_cta_original,
                                  v_saldo_modificado, v_estado_cta_modificado

                  DIALOG
                  ATTRIBUTES ( UNBUFFERED )

                     DISPLAY ARRAY v_arr_mov_original TO scr_det_movimientos_originales.*
                     END DISPLAY

                     DISPLAY ARRAY v_arr_movs_nuevo TO scr_det_movimientos_modificados.*
                     END DISPLAY

                     ON ACTION accept
                     --v_id_afi_fondo72_nuevo
                        -- se le indica al usuario que confirme la operacion ya que no hay manera de cancelaro
                        LET v_mensaje = "Por favor confirme la inhabilitación de la cuenta original y la creación\n",
                                        "de la cuenta nueva. Esta operación no puede cancelarse mediante sistema\n",
                                        "una vez efectuada."
                                        
                        MENU "Confirmación de cambio"
                        ATTRIBUTES ( STYLE="dialog",COMMENT = v_mensaje, IMAGE="question")
                           COMMAND "Confirmar"
                              LET v_confirma_cambio = TRUE
                              EXIT MENU

                           COMMAND "Cancelar"
                              -- no se realizara el cambio
                              LET v_confirma_cambio = FALSE
                              EXIT MENU
                        END MENU

                        -- si se confirmo el cambio
                        IF ( v_confirma_cambio ) THEN

                           -- se inicia el proceso
                           --LET v_resultado = fn_valida_operacion(0, v_proceso_cod, v_opera_cod)

                           -- NO SE VALIDA PARA QUE SE PUEDAN LIQUIDAR VARIOS AL MISMO TIEMPO
                           LET v_resultado = 0

                           IF ( v_resultado = 0 ) THEN
                              -- se genera el pid 
                              CALL fn_genera_pid(v_proceso_cod, v_opera_cod, "safreviv") RETURNING v_pid
                              DISPLAY "PID: ", v_pid

                              -- se obtiene el folio
                              CALL fn_genera_folio(v_proceso_cod, v_opera_cod, "safreviv")
                                   RETURNING v_folio
DISPLAY "Folio: ", v_folio

                              CALL fn_inicializa_proceso(v_pid             ,
                                                         v_proceso_cod     ,
                                                         v_opera_cod       ,
                                                         v_folio           ,
                                                         "AFIF02"          ,
                                                         "NA"  ,
                                                         "safreviv")  RETURNING v_resultado

                              -- el proceso se registro correctamente
                              IF ( v_resultado = 0 ) THEN
                                 -- Inicio operacion.
                                 CALL fn_actualiza_opera_ini(v_pid,
                                                             v_proceso_cod,
                                                             v_opera_cod  ,
                                                             v_folio      ,
                                                             "AFIF02"     ,
                                                             "NA"         ,
                                                             "safreviv"   )
                                      RETURNING v_resultado

                                 -- =================================================================
                                 -- creación de la cuenta nueva

                                 -- solicitud de id_afi_fondo72
                                 --LET v_id_afi_fondo72_nuevo = -2525
                                 SELECT seq_afi_fondo72.NEXTVAL
                                 INTO   v_id_afi_fondo72_nuevo
                                 FROM   systables
                                 WHERE  tabid = 1

                                 DISPLAY "ID AFI FONDO72 de la cuenta nueva: ", v_id_afi_fondo72_nuevo

                                 -- se busca si existe el NSS en afi_derechohabiente
                                 SELECT id_derechohabiente
                                 INTO   v_id_derechohabiente
                                 FROM   afi_derechohabiente
                                 WHERE  nss = v_nss_modificado

                                 DISPLAY "Insertando derechohabiente"
                                 -- creacion en la tabla de afiliados
                                 INSERT INTO afi_fondo72 (
                                    id_afi_fondo72,
                                    rfc,
                                    nss,
                                    id_derechohabiente,
                                    nombre,
                                    f_apertura,
                                    ind_estado_cuenta,
                                    f_estado_cuenta )
                                 VALUES (
                                    v_id_afi_fondo72_nuevo,
                                    v_rfc_modificado      ,
                                    v_nss_modificado      ,
                                    v_id_derechohabiente  ,
                                    v_nombre_concatenado   ,
                                    TODAY                 ,
                                    v_ind_estado_cuenta_nu,
                                    TODAY)

                                DISPLAY "insertando en histórico de afiliación"
                                 -- se inserta en la tabla de historicos. Cuenta nueva generada por aclaracion de cuenta
                                 INSERT INTO afi_his_fondo72 (
                                    folio_aclaracion,
                                    id_afi_fondo72  , 
                                    f_modifica      ,
                                    rfc             ,
                                    nss             ,
                                    nombre          ,
                                    ind_modifica    ,
                                    id_origen       )
                                 VALUES (
                                    v_folio               ,
                                    v_id_afi_fondo72_nuevo,
                                    TODAY                 ,
                                    v_rfc_modificado      ,
                                    v_nss_modificado      ,
                                    v_nombre_concatenado   ,
                                    v_clave_cambio        ,
                                    v_id_afi_fondo72)

                         DISPLAY "Insertando movimientos preliquidados de la cuenta nueva"

                                 -- si se tienen movimientos, se insertan
                                 IF ( v_arr_movs_nuevo.getLength() > 0 ) THEN

                                    -- insercion de movimientos de cargo y abono en la tabla de preliquidacion
                                    FOR li_i = 1 TO v_arr_movs_nuevo.getLength()
                                       LET v_sql = "\n INSERT INTO afi_preliquida72 (",
                                                   "\n id_afi_fondo72 ,",
                                                   "\n f_liquida      ,",
                                                   "\n subcuenta      ,",
                                                   "\n movimiento     ,",
                                                   "\n folio_liquida  ,",
                                                   "\n id_referencia  ,",
                                                   "\n importe        ,",
                                                   "\n estado_pago    ,",
                                                   "\n f_registro     ,",
                                                   "\n h_registro     ,",
                                                   "\n origen         )",
                                                   "\n VALUES         (",
                                                   "\n ", v_id_afi_fondo72_nuevo ,",",
                                                   "\n TODAY          ,",
                                                   "\n ", v_arr_movs_nuevo[li_i].subcuenta, ",",
                                                   "\n ", v_arr_movs_nuevo[li_i].movimiento, ",",
                                                   "\n ", v_folio ,",",
                                                   "\n ", v_id_afi_fondo72_nuevo, ",", 
                                                   "\n ", v_arr_movs_nuevo[li_i].monto_pesos, ",", 
                                                   "\n NULL,",
                                                   "\n TODAY ,",
                                                   "\n CURRENT HOUR TO SECOND,",
                                                   "\n 'AJUSTEF72'",
                                                   "\n)"

                                       DISPLAY "Insertando cuenta nueva: ", v_sql

                                       -- se inserta
                                       EXECUTE IMMEDIATE v_sql
                                    END FOR
                                 END IF

                                 -- ====================================================================
                                 -- inhabilitacion de la cuenta original y generacion de movimientos de balance

display "Actualización fondo72 cuenta original"

                                 -- se actualiza la cuenta original a inactiva
                                 UPDATE afi_fondo72
                                 SET    ind_estado_cuenta = v_ind_estado_cuenta_or
                                 WHERE  id_afi_fondo72 = v_id_afi_fondo72

                                 DISPLAY "\nActualizando cuenta original",
                                         "\nUPDATE afi_fondo72",
                                         "\nSET    ind_estado_cuenta = ", v_ind_estado_cuenta_or,
                                         "\nWHERE  id_afi_fondo72 = ", v_id_afi_fondo72

{
d. Registrar en la tabla cta_aclara_fondo72 todos los registros que tengan el NSS y RFC original
 de la tabla cta_his_fondo72 con el folio que se le asignó a la solicitud de modificación, 
 id_cta_his_fondo72, nss, rfc y nombre, una vez registrada esta información se debe actualizar 
 el NSS y/o RFC o nombre inclusive, en la tabla cta_his_fondo72 con la nueva información.
}

                                 -- se insertan los registros de cta_his_fondo72 de la cuenta origen
                                 -- en la tabla cta_aclara_fondo72
                                 DECLARE cur_ctahisf72 CURSOR FOR
                                 SELECT *
                                 FROM   cta_his_fondo72
                                 WHERE  nss          = v_nss_original
                                 AND    rfc          = v_rfc_original
                                 AND    ind_verifica = 0

                                 DISPLAY "Insertando movimientos de histórico de movimientos de la cuenta original"

                                 FOREACH cur_ctahisf72 INTO v_r_cta_his_fondo72.*
                                    -- se transfieren los datos a la tabla de aclaracion con el folio del cambio
                                    LET v_r_cta_aclara_fondo72.folio_aclaracion   = v_folio
                                    LET v_r_cta_aclara_fondo72.id_cta_his_fondo72 = v_r_cta_his_fondo72.id_cta_his_fondo72
                                    LET v_r_cta_aclara_fondo72.nss                = v_nss_original
                                    LET v_r_cta_aclara_fondo72.rfc                = v_rfc_original
                                    LET v_r_cta_aclara_fondo72.nombre             = v_nombre_original

                                    -- se inserta en la tabla
                                    INSERT INTO cta_aclara_fondo72 VALUES ( v_r_cta_aclara_fondo72.* )
                                 END FOREACH

                                 DISPLAY "Actualizando datos de la cuenta original con los modificados en histórico de movimientos"
                                 -- se actualizan los datos en la tabla cta_his_fondo72
                                 UPDATE cta_his_fondo72
                                 SET    nss          = v_nss_modificado   ,
                                        rfc          = v_rfc_modificado   ,
                                        nombre       = v_nombre_concatenado
                                 WHERE  nss          = v_nss_original
                                 AND    rfc          = v_rfc_original
                                 AND    ind_verifica = 0

DISPLAY "Insertando en preliquidación los movimientos de la cuenta original"
                                 -- si se tienen movientos se insertan
                                 IF ( v_arr_mov_original.getLength() > 0 ) THEN
                                    -- se realiza la transferencia de los movimientos
                                    FOR li_i = 1 TO v_arr_mov_original.getLength()
                                       -- solo se insertan los movimientos NUEVOS de balance
                                       IF ( (v_arr_mov_original[li_i].movimiento = 241 OR v_arr_mov_original[li_i].movimiento = 1412) AND v_arr_mov_original[li_i].folio = 0 ) THEN

                                          LET v_sql = "\n INSERT INTO afi_preliquida72 (",
                                                      "\n id_afi_fondo72 ,",
                                                      "\n f_liquida      ,",
                                                      "\n subcuenta      ,",
                                                      "\n movimiento     ,",
                                                      "\n folio_liquida  ,",
                                                      "\n id_referencia  ,",
                                                      "\n importe        ,",
                                                      "\n estado_pago    ,",
                                                      "\n f_registro     ,",
                                                      "\n h_registro     ,",
                                                      "\n origen         )",
                                                      "\n VALUES         (",
                                                      "\n ", v_id_afi_fondo72,",",
                                                      "\n TODAY          ,",
                                                      "\n ", v_arr_mov_original[li_i].subcuenta, ",",
                                                      "\n ", v_arr_mov_original[li_i].movimiento, ",",
                                                      "\n ", v_folio ,",",
                                                      "\n ", v_id_afi_fondo72, ",", 
                                                      "\n ", v_arr_mov_original[li_i].monto_pesos, ",", 
                                                      "\n NULL,",
                                                      "\n TODAY ,",
                                                      "\n CURRENT HOUR TO SECOND,",
                                                      "\n 'AJUSTEF72'",
                                                      "\n)"

                                          DISPLAY "Insert para cuenta nueva: ", v_sql

                                          -- se inserta
                                          EXECUTE IMMEDIATE v_sql
                                       END IF
                                    END FOR
                                 END IF

                                 -- se actualiza el folio a liquidado
                                 LET v_sql = "UPDATE glo_folio SET status = 1 WHERE folio = ", v_folio
                                 DISPLAY v_sql
                                 EXECUTE IMMEDIATE v_sql

                                 -- se asigna el folio al proceso y operacion
                                 LET v_sql = "UPDATE bat_ctr_proceso SET folio = ", v_folio, " WHERE pid = ", v_pid
                                 DISPLAY v_sql
                                 EXECUTE IMMEDIATE v_sql

                                 LET v_sql = "UPDATE bat_ctr_operacion SET folio = ", v_folio, " WHERE pid = ", v_pid
                                 DISPLAY v_sql
                                 EXECUTE IMMEDIATE v_sql

                                 -- se ejecuta la liquidacion de los movimientos
                                 DISPLAY "ejecutar GLOG03 aqui"

                                 -- se preliquidan (y liquidan) los movimientos realizados a la cuenta

                                 LET v_comando = "fglrun ", v_ruta_bin CLIPPED,"/GLOG03 ",
                                                 "safreviv" , " ",
                                                 v_pid                , " ",
                                                 v_proceso_cod        , " ",
                                                 v_opera_cod          , " ",
                                                 v_folio              , " ",
                                                 "NA" , " ",
                                                 " 1>", v_ruta_listados CLIPPED,
                                                 "/nohup:", v_pid USING "&&&&&",":",
                                                 v_proceso_cod   USING "&&&&&",":",
                                                 v_opera_cod     USING "&&&&&" ,
                                                 " 2>&1"

                                 DISPLAY "Comando para liquidar:", v_comando
                                 RUN v_comando

                              END IF -- se pudo iniciar el proceso
                           END IF -- se pudo validar la operacion
                        END IF -- se confirmo le cambio

                        EXIT DIALOG

                     ON ACTION cancel
                        EXIT DIALOG
                  END DIALOG

                  CLOSE WINDOW w_vistaprevia
               ELSE

                  -- se obtiene el folio
                  CALL fn_genera_folio(v_proceso_cod, v_opera_cod, "safreviv")
                       RETURNING v_folio

                  -- se insertan los registros de cta_his_fondo72 de la cuenta origen
                  -- en la tabla cta_aclara_fondo72
                  DECLARE cur_ctahisf72b CURSOR FOR
                  SELECT *
                  FROM   cta_his_fondo72
                  WHERE  nss          = v_nss_original
                  AND    rfc          = v_rfc_original
                  AND    ind_verifica = 0

                  FOREACH cur_ctahisf72b INTO v_r_cta_his_fondo72.*
                     -- se transfieren los datos a la tabla de aclaracion con el folio del cambio
                     LET v_r_cta_aclara_fondo72.folio_aclaracion   = v_folio
                     LET v_r_cta_aclara_fondo72.id_cta_his_fondo72 = v_r_cta_his_fondo72.id_cta_his_fondo72
                     LET v_r_cta_aclara_fondo72.nss                = v_nss_original
                     LET v_r_cta_aclara_fondo72.rfc                = v_rfc_original
                     LET v_r_cta_aclara_fondo72.nombre             = v_nombre_original

                     -- se inserta en la tabla
                     INSERT INTO cta_aclara_fondo72 VALUES ( v_r_cta_aclara_fondo72.* )
                  END FOREACH

                  -- se actualizan los datos en la tabla cta_his_fondo72
                  UPDATE cta_his_fondo72
                  SET    nombre       = v_nombre_concatenado
                  WHERE  nss          = v_nss_original
                  AND    rfc          = v_rfc_original
                  AND    ind_verifica = 0

                  -- se actualiza el registro en base de datos
                  UPDATE afi_fondo72
                  SET    nombre = v_nombre_concatenado
                  WHERE  id_afi_fondo72 = v_id_afi_fondo72

                  -- se registra el cambio en la tabla de historicos
                  INSERT INTO afi_his_fondo72 (
                     folio_aclaracion,
                     id_afi_fondo72  , 
                     f_modifica      ,
                     rfc             ,
                     nss             ,
                     nombre          ,
                     ind_modifica    ,
                     id_origen       )
                  VALUES (
                     v_folio            ,
                     v_id_afi_fondo72   ,
                     TODAY              ,
                     v_rfc_modificado   ,
                     v_nss_modificado   ,
                     v_nombre_concatenado,
                     v_clave_cambio     ,
                     NULL)

                  LET v_confirma_cambio = TRUE
               END IF

               -- si se confirmo el cambio
               IF ( v_confirma_cambio ) THEN
                  CALL fn_mensaje("Atención","Los datos del derechohabiente han sido modificados exitosamente.","information")
                  LET v_confirma_cambio = TRUE
               END IF
               EXIT MENU

            COMMAND "Cancelar"
               CALL fn_mensaje("Atención","No se realizaron cambios al registro","exclamation")
               LET v_confirma_cambio = FALSE
               EXIT MENU
         END MENU

         -- si se confirmo la afectacion en base de datos, se finaliza el input
         IF ( v_confirma_cambio ) THEN
            EXIT INPUT
         ELSE
            -- se continua la captura
            CONTINUE INPUT
         END IF

      ON ACTION cancel
         -- se regresan los datos originales
         LET v_nss_modificado    = v_nss_original
         LET v_rfc_modificado    = v_rfc_original
         LET v_nombre_modificado = v_nombre_original
         EXIT INPUT

   END INPUT

   CLOSE WINDOW w_modificar

   -- se devuelven los datos
   RETURN v_nss_modificado, v_rfc_modificado, v_nombre_modificado

END FUNCTION

{
======================================================================
Nombre: fn_valida_nss
Fecha creacion: Mayo 24, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Valida la estructura de un NSS modificado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}

FUNCTION fn_valida_nss(p_nss_modificado)

   DEFINE p_nss_modificado            LIKE afi_fondo72.nss
   DEFINE v_nss                       STRING -- cadena con el NSS,
   DEFINE v_mensaje                   STRING -- mensaje para el usuario
   DEFINE v_indice                    SMALLINT -- indice pivote
   DEFINE v_nss_es_correcto           SMALLINT -- booleana que indica si un NSS esta correctamente construido
   DEFINE v_digito_verificador        SMALLINT

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

      -- si se paso la validacion de caracteres numericos
      IF ( v_nss_es_correcto ) THEN
         -- se verifica si el numero verificador es correcto con respecto a los otros
         CALL fn_valida_nss_digito_verificador(p_nss_modificado) RETURNING v_digito_verificador

         -- si el digito verificador no es igual al obtenido se indica al usuario
         IF ( v_nss.getCharAt(11) <> v_digito_verificador ) THEN
            LET v_mensaje = "Se encontró una inconsistencia en el dígito verificador:\n",
                            "\nDígito verificador original: ", v_nss.getCharAt(11),
                            "\nDígito verificador calculado: ", v_digito_verificador USING "&",
                            "\n\n¿Desea realizar la corrección al NSS? El NSS corregido quedaría como sigue:\n",
                            p_nss_modificado[1,10], v_digito_verificador USING "#"

            MENU "Confirmar corrección"
            ATTRIBUTES ( STYLE = "dialog", COMMENT = v_mensaje, IMAGE="question" )
               -- aceptar la correccion
               COMMAND "Corregir"
                  -- se corrige el NSS
                  LET v_nss = p_nss_modificado[1,10], v_digito_verificador USING "&"

                  -- se asigna la NSS modificado
                  LET p_nss_modificado = v_nss
                  -- se indica que ya es correcto el NSS
                  LET v_nss_es_correcto = TRUE

                  EXIT MENU

               -- no acepta la correccion
               COMMAND "Cancelar"
                  LET v_mensaje = "Es necesario que corrija el dígito verificador o el NSS"
                  LET v_nss_es_correcto = FALSE
                  EXIT MENU

            END MENU

         END IF
      END IF
   END IF

   -- si hubo algun error, se envia mensaje en pantalla indicando cual es
   IF ( NOT v_nss_es_correcto ) THEN
      CALL fn_mensaje("Error",v_mensaje,"stop")
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_nss_es_correcto, p_nss_modificado

END FUNCTION

{
======================================================================
Nombre: fn_valida_nss_digito_verificador
Fecha creacion: Mayo 24, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}

FUNCTION fn_valida_nss_digito_verificador(p_nss_modificado)

   DEFINE p_nss_modificado            LIKE afi_fondo72.nss -- NSS para verificar digito
   DEFINE v_digito_verificador        SMALLINT
   DEFINE v_monto_2_digitos           CHAR(2)
   DEFINE v_monto_duplicado           SMALLINT
   DEFINE v_suma                      SMALLINT
   DEFINE v_indice                    SMALLINT

   -- para obtener el digito verificador se suman los primeros 10 digitos del NSS
   -- posición impar: se suma el valor del digito
   -- posición par: se obtiene el doble del valor del digito. si es mayor que 10, ambos digitos se suman y el resultado se acumula
   LET v_suma = 0
   FOR v_indice = 1 TO 10
      -- posicion par
      IF ( v_indice MOD 2 = 0 ) THEN
         -- se multiplica el digito por 2
         LET v_monto_duplicado = p_nss_modificado[v_indice, v_indice] * 2

         -- si excede 9, se suman los digitos
         IF ( v_monto_duplicado > 9 ) THEN
            LET v_monto_2_digitos = v_monto_duplicado
            LET v_monto_duplicado = v_monto_2_digitos[1,1] + v_monto_2_digitos[2,2]
         END IF

         -- se acumula con la suma
         LET v_suma = v_suma + v_monto_duplicado

      ELSE
         -- posicion impar. se suma el digito
         LET v_suma = v_suma + p_nss_modificado[v_indice,v_indice]
      END IF
   END FOR

   -- si la suma excede 9, se toma la unidad
   IF ( v_suma > 9 ) THEN
      DISPLAY "suma excede 9: ", v_suma
      LET v_monto_2_digitos = v_suma
      LET v_suma = v_monto_2_digitos[2,2]
      DISPLAY "suma queda: ", v_suma
   END IF

   -- el digito verificador se obtiene de restar la cifra final de la suma a 10
   IF ( v_suma = 0 ) THEN
      -- en caso de ser cero, asi se queda
      LET v_digito_verificador = v_suma
   ELSE
      LET v_digito_verificador = 10 - v_suma
   END IF

   -- se devuelve el digito verificador obtenido
   DISPLAY "Suma: ", v_suma
   DISPLAY "Digito verificador obtenido: ", v_digito_verificador
   RETURN v_digito_verificador
END FUNCTION

{
======================================================================
Nombre: fn_valida_rfc
Fecha creacion: Mayo 24, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Valida la estructura de un RFC modificado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}

FUNCTION fn_valida_rfc(p_rfc_modificado, p_apellido_paterno, p_apellido_materno, p_nombres)

   DEFINE p_rfc_modificado          LIKE afi_fondo72.rfc -- RFC modificado
   DEFINE p_apellido_paterno        STRING -- apellido paterno modificado
   DEFINE p_apellido_materno        STRING -- apellido materno modificado
   DEFINE p_nombres                 STRING -- nombre modificado
   DEFINE v_rfc_es_correcto         SMALLINT -- booleana que indica si un RFC esta correctamente construido
   DEFINE v_rfc_analisis            STRING -- RFC que se analizara
   DEFINE v_rfc_aux                 STRING -- RFC auxiliar para sugerir correccion
   DEFINE v_tokenizer               base.StringTokenizer -- tokenizer para separar particulas y evaluar conformacion de RFC
   DEFINE v_indice                  SMALLINT
   DEFINE v_cadena                  STRING
   DEFINE v_total_tokens            SMALLINT -- total de tokens encontrados
   DEFINE v_fecha_nacimiento        VARCHAR(6) -- seccion de la fecha de nacimiento del RFC
   DEFINE v_mensaje                 STRING -- cadena con mensaje para usuario
   DEFINE v_altisonante             CHAR(4) -- busqueda de palabra altisonante
   DEFINE v_rfc_sustituto           CHAR(4)

   -- se asume que el RFC esta correcto
   LET v_rfc_es_correcto = TRUE

   -- se asigna el RFC para analisis
   LET v_rfc_analisis = p_rfc_modificado CLIPPED

   -- ==========================================================
   -- se busca que el RFC este bien conformado
   -- validacion de particula obtenida de apellido paterno
   LET v_tokenizer = base.StringTokenizer.create(p_apellido_paterno," ")

   -- se verifica si el apellido paterno tiene mas de una particula
   LET v_total_tokens = v_tokenizer.countTokens()
   LET v_indice = 1
   IF ( v_total_tokens > 1 ) THEN
      -- se obtiene la primera particula que no sea una preposicion o particula invalida     
      WHILE ( v_tokenizer.hasMoreTokens() )
         LET v_cadena = v_tokenizer.nextToken()

         -- si es diferente a una palabra invalida, se toma esa palabra
         IF ( v_cadena <> "DA"  AND
              v_cadena <> "DAS" AND
              v_cadena <> "DE"  AND
              v_cadena <> "DEL" AND
              v_cadena <> "DER" AND
              v_cadena <> "DI"  AND
              v_cadena <> "DIE" AND
              v_cadena <> "DD"  AND
              v_cadena <> "EL"  AND
              v_cadena <> "LA"  AND
              v_cadena <> "LOS" AND
              v_cadena <> "LAS" AND
              v_cadena <> "LE"  AND
              v_cadena <> "LES" AND
              v_cadena <> "MAC" AND
              v_cadena <> "MC"  AND
              v_cadena <> "VAN" AND
              v_cadena <> "VON" AND
              v_cadena <> "Y"   ) THEN
            EXIT WHILE
         ELSE
            -- se incrementa el contador de palabra
            LET v_indice = v_indice + 1
         END IF
      END WHILE
   ELSE
      -- solo es una palabra
      -- si es diferente a una palabra invalida, se toma esa palabra
      IF ( p_apellido_paterno <> "DA"  AND
           p_apellido_paterno <> "DAS" AND
           p_apellido_paterno <> "DE"  AND
           p_apellido_paterno <> "DEL" AND
           p_apellido_paterno <> "DER" AND
           p_apellido_paterno <> "DI"  AND
           p_apellido_paterno <> "DIE" AND
           p_apellido_paterno <> "DD"  AND
           p_apellido_paterno <> "EL"  AND
           p_apellido_paterno <> "LA"  AND
           p_apellido_paterno <> "LOS" AND
           p_apellido_paterno <> "LAS" AND
           p_apellido_paterno <> "LE"  AND
           p_apellido_paterno <> "LES" AND
           p_apellido_paterno <> "MAC" AND
           p_apellido_paterno <> "MC"  AND
           p_apellido_paterno <> "VAN" AND
           p_apellido_paterno <> "VON" AND
           p_apellido_paterno <> "Y"   ) THEN
         LET v_cadena = p_apellido_paterno
      ELSE
         -- se le pone una X
         LET v_cadena = "X"
      END IF
   END IF

   -- si no se encontro una palabra valida
   IF ( v_indice > v_total_tokens ) THEN
      DISPLAY "v_indice: ", v_indice
      DISPLAY "tokens: ", v_total_tokens
      -- se ponen 2 X
      LET v_rfc_aux = "XX"
   ELSE
      -- se obtiene la primera letra
      IF ( v_cadena.getCharAt(1) = "Ñ" ) THEN
         -- si es Ñ se cambia por X
         LET v_rfc_aux = "X"
      ELSE
         LET v_rfc_aux = v_cadena.getCharAt(1)
      END IF

      -- se busca la primera vocal seguida de la primera letra
      FOR v_indice = 2 TO v_cadena.getLength()
         IF ( v_cadena.getCharAt(v_indice) = "A" OR
              v_cadena.getCharAt(v_indice) = "E" OR
              v_cadena.getCharAt(v_indice) = "I" OR
              v_cadena.getCharAt(v_indice) = "O" OR
              v_cadena.getCharAt(v_indice) = "U" ) THEN
            -- se sale del ciclo para quedarse con el indice
            EXIT FOR
         END IF
      END FOR

      -- se verifica si se encontro alguna vocal. Esto ocurriria si el indice esta entre 2 y la longitud de la palabra
      IF ( v_indice > 1 AND v_indice <= v_cadena.getLength() ) THEN
         -- se agrega la vocal encontrada
         LET v_rfc_aux = v_rfc_aux || v_cadena.getCharAt(v_indice)
      ELSE
         -- no se encontro vocal, se agrega una X
         LET v_rfc_aux = v_rfc_aux || "X"
      END IF
   END IF

   -- =========================================
   -- particula obtenida del apellido materno
   IF ( p_apellido_materno IS NULL ) THEN
      -- se asigna X cuando no se tiene apellido materno
      LET v_rfc_aux = v_rfc_aux || "X"
   ELSE
      LET v_tokenizer = base.StringTokenizer.create(p_apellido_materno," ")

      -- se verifica si el apellido paterno tiene mas de una particula
      LET v_total_tokens = v_tokenizer.countTokens()
      LET v_indice = 1
      IF ( v_total_tokens > 1 ) THEN
         -- se obtiene la primera particula que no sea una preposicion o particula invalida
         WHILE ( v_tokenizer.hasMoreTokens() )
            LET v_cadena = v_tokenizer.nextToken()

            -- si es diferente a una palabra invalida, se toma esa palabra
            IF ( v_cadena <> "DA"  AND
                 v_cadena <> "DAS" AND
                 v_cadena <> "DE"  AND
                 v_cadena <> "DEL" AND
                 v_cadena <> "DER" AND
                 v_cadena <> "DI"  AND
                 v_cadena <> "DIE" AND
                 v_cadena <> "DD"  AND
                 v_cadena <> "EL"  AND
                 v_cadena <> "LA"  AND
                 v_cadena <> "LOS" AND
                 v_cadena <> "LAS" AND
                 v_cadena <> "LE"  AND
                 v_cadena <> "LES" AND
                 v_cadena <> "MAC" AND
                 v_cadena <> "MC"  AND
                 v_cadena <> "VAN" AND
                 v_cadena <> "VON" AND
                 v_cadena <> "Y"   ) THEN
               EXIT WHILE
            END IF
            -- se incrementa el contador de palabras
            LET v_indice = v_indice + 1
         END WHILE
      ELSE
         -- solo es una palabra
         -- si es diferente a una palabra invalida, se toma esa palabra
         IF ( p_apellido_materno <> "DA"  AND
              p_apellido_materno <> "DAS" AND
              p_apellido_materno <> "DE"  AND
              p_apellido_materno <> "DEL" AND
              p_apellido_materno <> "DER" AND
              p_apellido_materno <> "DI"  AND
              p_apellido_materno <> "DIE" AND
              p_apellido_materno <> "DD"  AND
              p_apellido_materno <> "EL"  AND
              p_apellido_materno <> "LA"  AND
              p_apellido_materno <> "LOS" AND
              p_apellido_materno <> "LAS" AND
              p_apellido_materno <> "LE"  AND
              p_apellido_materno <> "LES" AND
              p_apellido_materno <> "MAC" AND
              p_apellido_materno <> "MC"  AND
              p_apellido_materno <> "VAN" AND
              p_apellido_materno <> "VON" AND
              p_apellido_materno <> "Y"   ) THEN
            LET v_cadena = p_apellido_materno
         ELSE
            -- se le pone una X
            LET v_cadena = "X"
         END IF
      END IF

      -- si no se encontro una palabra valida
      IF ( v_indice > v_total_tokens ) THEN
         -- se concatena una X
         LET v_rfc_aux = v_rfc_aux || "X"
      ELSE
         -- se obtiene la primera letra del apellido
         IF ( v_cadena.getCharAt(1) = "Ñ" ) THEN
            -- si es Ñ se cambia por X
            LET v_rfc_aux = v_rfc_aux || "X"
         ELSE
            -- se concatena la primera inicial
            LET v_rfc_aux = v_rfc_aux || v_cadena.getCharAt(1)
         END IF
      END IF
   END IF

   -- ===========================================
   -- particula del nombre. se copia la primera inicial
   LET v_tokenizer = base.StringTokenizer.create(p_nombres," ")

   -- se verifica si el apellido paterno tiene mas de una particula
   LET v_total_tokens = v_tokenizer.countTokens()
   IF ( v_total_tokens > 1 ) THEN
      -- se obtiene la primera particula que no sea una preposicion o particula invalida
      LET v_indice = 1
      WHILE ( v_tokenizer.hasMoreTokens() )
         LET v_cadena = v_tokenizer.nextToken()

         -- si es diferente a una palabra invalida, se toma esa palabra
         IF ( v_cadena = "MARIA" OR
              v_cadena = "MA."   OR
              v_cadena = "MA"    OR
              v_cadena = "JOSE"  OR
              v_cadena = "J"     OR
              v_cadena = "J."      ) THEN
            -- si es el primer nombre, no se puede tomar
            IF ( v_indice = 1 ) THEN
               LET v_indice = v_indice + 1
               CONTINUE WHILE
            ELSE
               -- se sale del while para tomar el nombre en turno
               EXIT WHILE
            END IF
         ELSE
            -- si se trata de una palabra invalida, se salta esa palabra
            IF ( v_cadena = "DA"  OR
                 v_cadena = "DAS" OR
                 v_cadena = "DE"  OR
                 v_cadena = "DEL" OR
                 v_cadena = "DER" OR
                 v_cadena = "DI"  OR
                 v_cadena = "DIE" OR
                 v_cadena = "DD"  OR
                 v_cadena = "EL"  OR
                 v_cadena = "LA"  OR
                 v_cadena = "LOS" OR
                 v_cadena = "LAS" OR
                 v_cadena = "LE"  OR
                 v_cadena = "LES" OR
                 v_cadena = "MAC" OR
                 v_cadena = "MC"  OR
                 v_cadena = "VAN" OR
                 v_cadena = "VON" OR
                 v_cadena = "Y"   ) THEN
               LET v_indice = v_indice + 1
               CONTINUE WHILE
            ELSE
               -- se toma la palabra en turno
               EXIT WHILE
            END IF
         END IF
      END WHILE

      -- si no se encontro una palabra valida
      IF ( v_indice > v_total_tokens ) THEN
         LET v_cadena = "X"
      END IF
   ELSE
      -- solo es una palabra
      -- si es diferente a una palabra invalida, se toma esa palabra
      IF ( p_nombres <> "DA"  AND
           p_nombres <> "DAS" AND
           p_nombres <> "DE"  AND
           p_nombres <> "DEL" AND
           p_nombres <> "DER" AND
           p_nombres <> "DI"  AND
           p_nombres <> "DIE" AND
           p_nombres <> "DD"  AND
           p_nombres <> "EL"  AND
           p_nombres <> "LA"  AND
           p_nombres <> "LOS" AND
           p_nombres <> "LAS" AND
           p_nombres <> "LE"  AND
           p_nombres <> "LES" AND
           p_nombres <> "MAC" AND
           p_nombres <> "MC"  AND
           p_nombres <> "VAN" AND
           p_nombres <> "VON" AND
           p_nombres <> "Y"   ) THEN
         LET v_cadena = p_nombres
      ELSE
         -- se le pone una X
         LET v_cadena = "X"
      END IF
   END IF

   -- se obtiene la primera letra del nombre
   IF ( v_cadena.getCharAt(1) = "Ñ" ) THEN
      -- si es Ñ se cambia por X
      LET v_rfc_aux = v_rfc_aux || "X"
   ELSE
      -- se concatena la primera inicial
      LET v_rfc_aux = v_rfc_aux || v_cadena.getCharAt(1)
   END IF

   -- se verifica que la parte del RFC sacada de los nombres no sea una palabra invalida altisonante
   LET v_altisonante = v_rfc_aux

   SELECT palabra_sust
   INTO   v_rfc_sustituto
   FROM   cat_rfc_inconv
   WHERE  palabra_inconv = v_altisonante

   -- si se encontro, se usa la palabra sustituto
   IF ( v_rfc_sustituto IS NOT NULL ) THEN
      LET v_rfc_aux = v_rfc_sustituto
   END IF

   -- ===================================================
   -- verificacion de la seccion de la fecha de nacimiento
   -- se obtiene la fecha de nacimiento
   LET v_fecha_nacimiento = p_rfc_modificado[5,10]

   -- el mes no puede ser menor a 1 ni mayor a 12
   IF ( v_fecha_nacimiento[1,2] < "00" OR v_fecha_nacimiento[1,2] > "99" ) THEN
      LET v_mensaje = "El AÑO en la fecha del RFC es inválido"
      LET v_rfc_es_correcto = FALSE
   END IF

   -- el mes no puede ser menor a 1 ni mayor a 12
   IF ( v_fecha_nacimiento[3,4] < "01" OR v_fecha_nacimiento[3,4] > "12" ) THEN
      LET v_mensaje = "El MES en la fecha del RFC es inválido"
      LET v_rfc_es_correcto = FALSE
   ELSE
      -- se verifica que el dia sea valido
      IF ( v_fecha_nacimiento[5,6] < "01" OR v_fecha_nacimiento[5,6] > "31" ) THEN
         LET v_mensaje = "El DIA en la fecha del RFC es inválido"
         LET v_rfc_es_correcto = FALSE
      ELSE
         -- la fecha es correcta, se concatena al rfc auxiliar
         LET v_rfc_aux = v_rfc_aux || v_fecha_nacimiento

         -- si el rfc modificado tiene homoclave, se concatena al rfc_auxiliar
         IF ( LENGTH(p_rfc_modificado) = 13 ) THEN
            LET v_rfc_aux = v_rfc_aux || p_rfc_modificado[11,13]
         END IF

         -- se verifica si el RFC calculado es igual al original modificado
         IF ( p_rfc_modificado <> v_rfc_aux ) THEN
            LET v_mensaje = "El RFC obtenido a partir de los datos proporcionados no coincide",
                            "\ncon el RFC que se capturó:",
                            "\n\nCapturado: ", p_rfc_modificado,
                            "\nCalculado: ", v_rfc_aux,
                            "\n\n¿Desea corregir el RFC?"

            MENU "Cambio de RFC"
            ATTRIBUTES ( STYLE = "dialog", COMMENT = v_mensaje, IMAGE = "question" )
               COMMAND "Aceptar"
                  LET p_rfc_modificado = v_rfc_aux
                  LET v_rfc_es_correcto = TRUE
                  EXIT MENU

               COMMAND "Cancelar"
                  LET v_mensaje = "Es necesario que verifique los datos proporcionados que conforman el RFC"
                  LET v_rfc_es_correcto = FALSE
                  EXIT MENU
            END MENU
         END IF
      END IF
   END IF

   -- si hubo error se muestra el mensaje al usuario
   IF ( NOT v_rfc_es_correcto ) THEN
      CALL fn_mensaje("Error", v_mensaje, "stop")
   END IF

   DISPLAY "RFC AUX QUEDA: ", v_rfc_aux

   -- se devuelve el resultado de la consulta
   RETURN v_rfc_es_correcto, p_rfc_modificado
END FUNCTION

{
======================================================================
Nombre: fn_valida_homoclave_rfc
Fecha creacion: Septiembre 13, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica que la homoclave del RFC este correcta y si no existe, se calcula

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}

FUNCTION fn_valida_homoclave_rfc(p_rfc_modificado, p_apellido_paterno, p_apellido_materno, p_nombres)

   DEFINE p_rfc_modificado              LIKE afi_fondo72.rfc -- RFC modificado
   DEFINE p_apellido_paterno            STRING -- apellido paterno modificado
   DEFINE p_apellido_materno            STRING -- apellido materno modificado
   DEFINE p_nombres                     STRING -- nombre modificado
   DEFINE v_nombre_compuesto            STRING -- nombre compuesto
   DEFINE v_rfc_aux                     LIKE afi_fondo72.rfc -- RFC auxiliar
   DEFINE v_mensaje                     STRING -- mensaje para el usuario
   DEFINE v_indice                      SMALLINT -- indice pivote
   DEFINE v_indice_aux                  SMALLINT -- indice auxiliar
   DEFINE v_homoclave_es_correcta       SMALLINT -- booleana que indica si el digito verificador del RFC es correcto
   DEFINE v_homoclave1                  VARCHAR(1)
   DEFINE v_homoclave2                  VARCHAR(1)
   DEFINE v_homoclave_calculada         VARCHAR(2)

   DEFINE v_arreglo_val_nombre DYNAMIC ARRAY OF RECORD -- valor de cada caracter encontrado en el nombre
      alfanumerico                      VARCHAR(1),
      valor                             SMALLINT
   END RECORD

   DEFINE v_arreglo_val_char_homonimia DYNAMIC ARRAY OF RECORD
      alfanumerico                      VARCHAR(1),
      valor                             SMALLINT
   END RECORD

   DEFINE v_arr_valores_nombre          DYNAMIC ARRAY OF SMALLINT -- para obtener el valor de cada caracter
   DEFINE v_arr_valores_calculo         DYNAMIC ARRAY OF SMALLINT -- para calcular la suma
   DEFINE v_concatenacion_val           STRING
   DEFINE v_cadena                      STRING
   DEFINE v_suma                        INTEGER -- suma calculada para el digito verificador
   DEFINE v_residuo                     SMALLINT -- residuo de una division

   -- se llena el arreglo de valores de caracteres del nombre
   -- el primer valor es el espacio
   LET v_arreglo_val_nombre[1].alfanumerico = " "
   LET v_arreglo_val_nombre[1].VALOR        = 0

   -- los numeros tienen su mismo valor
   FOR v_indice = 2 TO 11 -- para que quede del cero al 9
      LET v_arreglo_val_nombre[v_indice].alfanumerico = v_indice - 2
      LET v_arreglo_val_nombre[v_indice].VALOR        = v_indice - 2
   END FOR

   -- el resto de los caracteres se pone a manu
   LET v_arreglo_val_nombre[12].alfanumerico = "&"
   LET v_arreglo_val_nombre[12].valor        = 10

   LET v_arreglo_val_nombre[13].alfanumerico = "A"
   LET v_arreglo_val_nombre[13].valor        = 11 

   LET v_arreglo_val_nombre[14].alfanumerico = "B"
   LET v_arreglo_val_nombre[14].valor        = 12

   LET v_arreglo_val_nombre[15].alfanumerico = "C"
   LET v_arreglo_val_nombre[15].valor        = 13

   LET v_arreglo_val_nombre[16].alfanumerico = "D"
   LET v_arreglo_val_nombre[16].valor        = 14

   LET v_arreglo_val_nombre[17].alfanumerico = "E"
   LET v_arreglo_val_nombre[17].valor        = 15

   LET v_arreglo_val_nombre[18].alfanumerico = "F"
   LET v_arreglo_val_nombre[18].valor        = 16

   LET v_arreglo_val_nombre[19].alfanumerico = "G"
   LET v_arreglo_val_nombre[19].valor        = 17

   LET v_arreglo_val_nombre[20].alfanumerico = "H"
   LET v_arreglo_val_nombre[20].valor        = 18

   LET v_arreglo_val_nombre[21].alfanumerico = "I"
   LET v_arreglo_val_nombre[21].valor        = 19

   LET v_arreglo_val_nombre[22].alfanumerico = "J"
   LET v_arreglo_val_nombre[22].valor        = 21

   LET v_arreglo_val_nombre[23].alfanumerico = "K"
   LET v_arreglo_val_nombre[23].valor        = 22

   LET v_arreglo_val_nombre[24].alfanumerico = "L"
   LET v_arreglo_val_nombre[24].valor        = 23

   LET v_arreglo_val_nombre[25].alfanumerico = "M"
   LET v_arreglo_val_nombre[25].valor        = 24

   LET v_arreglo_val_nombre[26].alfanumerico = "N"
   LET v_arreglo_val_nombre[26].valor        = 25

   LET v_arreglo_val_nombre[27].alfanumerico = "O"
   LET v_arreglo_val_nombre[27].valor        = 26

   LET v_arreglo_val_nombre[28].alfanumerico = "P"
   LET v_arreglo_val_nombre[28].valor        = 27

   LET v_arreglo_val_nombre[29].alfanumerico = "Q"
   LET v_arreglo_val_nombre[29].valor        = 28

   LET v_arreglo_val_nombre[30].alfanumerico = "R"
   LET v_arreglo_val_nombre[30].valor        = 29

   LET v_arreglo_val_nombre[31].alfanumerico = "S"
   LET v_arreglo_val_nombre[31].valor        = 32

   LET v_arreglo_val_nombre[32].alfanumerico = "T"
   LET v_arreglo_val_nombre[32].valor        = 33

   LET v_arreglo_val_nombre[33].alfanumerico = "U"
   LET v_arreglo_val_nombre[33].valor        = 34

   LET v_arreglo_val_nombre[34].alfanumerico = "V"
   LET v_arreglo_val_nombre[34].valor        = 35

   LET v_arreglo_val_nombre[35].alfanumerico = "W"
   LET v_arreglo_val_nombre[35].valor        = 36

   LET v_arreglo_val_nombre[36].alfanumerico = "X"
   LET v_arreglo_val_nombre[36].valor        = 37

   LET v_arreglo_val_nombre[37].alfanumerico = "Y"
   LET v_arreglo_val_nombre[37].valor        = 38

   LET v_arreglo_val_nombre[38].alfanumerico = "Z"
   LET v_arreglo_val_nombre[38].valor        = 39

   LET v_arreglo_val_nombre[39].alfanumerico = "Ñ"
   LET v_arreglo_val_nombre[39].valor        = 10

   -- arreglo de caracteres para homonimia
   FOR v_indice = 1 TO 9
      LET v_arreglo_val_char_homonimia[v_indice].valor        = v_indice - 1
      LET v_arreglo_val_char_homonimia[v_indice].alfanumerico = v_indice
   END FOR

   -- de la posicion 10 en adelante van las letras
   LET v_arreglo_val_char_homonimia[10].valor        = 9
   LET v_arreglo_val_char_homonimia[10].alfanumerico = "A"

   LET v_arreglo_val_char_homonimia[11].valor        = 10
   LET v_arreglo_val_char_homonimia[11].alfanumerico = "B"

   LET v_arreglo_val_char_homonimia[12].valor        = 11
   LET v_arreglo_val_char_homonimia[12].alfanumerico = "C"

   LET v_arreglo_val_char_homonimia[13].valor        = 12
   LET v_arreglo_val_char_homonimia[13].alfanumerico = "D"

   LET v_arreglo_val_char_homonimia[14].valor        = 13
   LET v_arreglo_val_char_homonimia[14].alfanumerico = "E"

   LET v_arreglo_val_char_homonimia[15].valor        = 14
   LET v_arreglo_val_char_homonimia[15].alfanumerico = "F"
   
   LET v_arreglo_val_char_homonimia[16].valor        = 15
   LET v_arreglo_val_char_homonimia[16].alfanumerico = "G"

   LET v_arreglo_val_char_homonimia[17].valor        = 16
   LET v_arreglo_val_char_homonimia[17].alfanumerico = "H"

   LET v_arreglo_val_char_homonimia[18].valor        = 17
   LET v_arreglo_val_char_homonimia[18].alfanumerico = "I"

   LET v_arreglo_val_char_homonimia[19].valor        = 18
   LET v_arreglo_val_char_homonimia[19].alfanumerico = "J"

   LET v_arreglo_val_char_homonimia[20].valor        = 19
   LET v_arreglo_val_char_homonimia[20].alfanumerico = "K"

   LET v_arreglo_val_char_homonimia[21].valor        = 20
   LET v_arreglo_val_char_homonimia[21].alfanumerico = "L"

   LET v_arreglo_val_char_homonimia[22].valor        = 21
   LET v_arreglo_val_char_homonimia[22].alfanumerico = "M"

   LET v_arreglo_val_char_homonimia[23].valor        = 22
   LET v_arreglo_val_char_homonimia[23].alfanumerico = "N"

   LET v_arreglo_val_char_homonimia[24].valor        = 23
   LET v_arreglo_val_char_homonimia[24].alfanumerico = "P"

   LET v_arreglo_val_char_homonimia[25].valor        = 24
   LET v_arreglo_val_char_homonimia[25].alfanumerico = "Q"

   LET v_arreglo_val_char_homonimia[26].valor        = 25
   LET v_arreglo_val_char_homonimia[26].alfanumerico = "R"

   LET v_arreglo_val_char_homonimia[27].valor        = 26
   LET v_arreglo_val_char_homonimia[27].alfanumerico = "S"
   
   LET v_arreglo_val_char_homonimia[28].valor        = 27
   LET v_arreglo_val_char_homonimia[28].alfanumerico = "T"

   LET v_arreglo_val_char_homonimia[29].valor        = 28
   LET v_arreglo_val_char_homonimia[29].alfanumerico = "U"

   LET v_arreglo_val_char_homonimia[30].valor        = 29
   LET v_arreglo_val_char_homonimia[30].alfanumerico = "V"

   LET v_arreglo_val_char_homonimia[31].valor        = 30
   LET v_arreglo_val_char_homonimia[31].alfanumerico = "W"

   LET v_arreglo_val_char_homonimia[32].valor        = 31
   LET v_arreglo_val_char_homonimia[32].alfanumerico = "X"

   LET v_arreglo_val_char_homonimia[33].valor        = 32
   LET v_arreglo_val_char_homonimia[33].alfanumerico = "Y"

   LET v_arreglo_val_char_homonimia[34].valor        = 33
   LET v_arreglo_val_char_homonimia[34].alfanumerico = "Z"

   -- se asume que la homoclave es correcta
   LET v_homoclave_es_correcta = TRUE
   LET v_suma = 0
   LET v_rfc_aux = p_rfc_modificado

   -- rfc debe ser de 13 digitos
   IF ( LENGTH(v_rfc_aux) < 10 ) THEN
      LET v_mensaje = "La longitud del RFC debe ser de 10 dígitos para poder comprobar la homoclave"
      LET v_homoclave_es_correcta = FALSE      
   ELSE
      -- se forma el nombre compuesto
      LET v_nombre_compuesto = p_apellido_paterno.trim(), " ", p_apellido_materno.trim(), " ", p_nombres.trim()

      -- la primera posicion del arreglo de valores del nombre siempre es cero
      LET v_arr_valores_nombre[1] = 0

      -- se procede a calcular la homoclave. Se busca el valor de cada caracter del nombre con respecto a su valor
      FOR v_indice = 1 TO v_nombre_compuesto.getLength()

         -- se busca el caracter en turno en el arreglo de valores de cartacter del nombre
         FOR v_indice_aux = 1 TO v_arreglo_val_nombre.getLength()
            IF ( v_nombre_compuesto.getCharAt(v_indice) = v_arreglo_val_nombre[v_indice_aux].alfanumerico ) THEN
               -- se toma el valor y se guarda
               LET v_arr_valores_nombre[v_indice + 1] = v_arreglo_val_nombre[v_indice_aux].valor
               EXIT FOR
            END IF
         END FOR
      END FOR

      LET v_indice_aux = 1

      LET v_concatenacion_val = "0"

      -- se concatena todo el arreglo de valores para calculo
      FOR v_indice = 2 TO v_arr_valores_nombre.getLength()
         LET v_cadena = v_arr_valores_nombre[v_indice] USING "&&"
         LET v_concatenacion_val = v_concatenacion_val, v_cadena
         DISPLAY "concatenando: ", v_cadena
         DISPLAY "acumulado: ", v_concatenacion_val
      END FOR

      -- se crea un nuevo arreglo para calculo final tomando pares de la cadena
      CALL v_arr_valores_nombre.clear()

      FOR v_indice = 1 TO v_concatenacion_val.getLength() - 1
         LET v_arr_valores_nombre[v_indice] = v_concatenacion_val.subString(v_indice, v_indice + 1)
      END FOR

      -- se calcula la suma
      LET v_suma = 0

      FOR v_indice = 1 TO v_arr_valores_nombre.getLength()
         DISPLAY "nuevos valores para calculo: ", v_arr_valores_nombre[v_indice]
         -- se multiplica el valor por su unidad
         LET v_cadena = v_arr_valores_nombre[v_indice]
         IF ( v_cadena.getLength() > 1 ) THEN
            LET v_indice_aux = v_cadena.getCharAt(2)
         ELSE
            LET v_indice_aux = v_cadena
         END IF
         LET v_suma = v_suma + (v_arr_valores_nombre[v_indice] * v_indice_aux )
      END FOR

      display "Suma calculada: ", v_suma

      -- de la suma se obtienen los ultimos 3 digitos
      LET v_cadena = v_suma
      LET v_indice = v_cadena.getLength() - 2
      LET v_indice_aux = v_cadena.getLength()
      DISPLAY "indice suma: ", v_indice
      DISPLAY "indice_aux suma: ", v_indice_aux
      LET v_suma = v_cadena.subString(v_indice, v_indice_aux)

      -- el indice del primer caracter de la homoclave es el cociente
      LET v_indice = v_suma / 34
      -- el segundo caracter es el residuo de la division
      LET v_indice_aux = v_suma MOD 34

      DISPLAY "Indice 1: ", v_indice
      DISPLAY "Indice 2: ", v_indice_aux

      -- se obtienen los caracteres que corresponden a la homoclave
      -- [los indices son + 1 porque los indices comienzan en 1]
      LET v_homoclave1 = v_arreglo_val_char_homonimia[v_indice + 1].alfanumerico
      LET v_homoclave2 = v_arreglo_val_char_homonimia[v_indice_aux + 1 ].alfanumerico

      LET v_homoclave_calculada = v_homoclave1, v_homoclave2
   END IF

   -- si el rfc esta completo
   IF ( LENGTH(v_rfc_aux) = 13 ) THEN
      -- si la homoclave no coincide
      IF ( v_rfc_aux[11,12] <> v_homoclave_calculada ) THEN
         -- se construye el mensaje de error      
         LET v_mensaje = "La homoclave del RFC capturada no coincide con la calculada.\nPor favor verifique los datos:\n",
                         "\nCapturado: ", v_rfc_aux[11,12],
                         "\nCalculado: ", v_homoclave_calculada

         MENU "Atención"
         ATTRIBUTES ( STYLE="dialog", COMMENT = v_mensaje, IMAGE="question" )

            COMMAND "Aceptar"
               LET p_rfc_modificado[11,12] = v_homoclave_calculada
               LET v_homoclave_es_correcta = TRUE
               EXIT MENU

            COMMAND "Cancelar"
               LET v_homoclave_es_correcta = FALSE
               LET v_mensaje = "Es necesario corroborar la homoclave del RFC."
               EXIT MENU
         END MENU

      END IF
   ELSE
      -- se indica cual fue la homoclave calculada
      LET v_rfc_aux = v_rfc_aux CLIPPED, v_homoclave_calculada

      -- se construye el mensaje de error      
      LET v_cadena = "No se capturó homoclave para el RFC.\n\n¿Desea utilizar la homoclave calculada?\n",
                      "\nRFC Calculado: ", v_rfc_aux

      MENU "Atención"
      ATTRIBUTES ( STYLE="dialog", COMMENT=v_cadena, IMAGE="question" )

         COMMAND "Aceptar"
            LET p_rfc_modificado[11,12] = v_rfc_aux[11,12]
            LET v_homoclave_es_correcta = TRUE
            EXIT MENU

         COMMAND "Cancelar"
            LET v_homoclave_es_correcta = FALSE
            LET v_mensaje = "Es necesario completar el RFC con la homoclave"
            EXIT MENU
      END MENU
   END IF

   -- si hubo algun error, se envia mensaje en pantalla indicando cual es
   IF ( NOT v_homoclave_es_correcta ) THEN
      CALL fn_mensaje("Error",v_mensaje,"stop")
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_homoclave_es_correcta, p_rfc_modificado
END FUNCTION

{
======================================================================
Nombre: fn_valida_digitoverificador_rfc
Fecha creacion: Septiembre 13, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Valida que el digito verificador del RFC capturado sea correcto con respecto
al calculo de digito verificador

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}

FUNCTION fn_valida_digitoverificador_rfc(p_rfc_modificado)

DEFINE p_rfc_modificado     LIKE afi_fondo72.rfc, -- RFC modificado
       v_rfc_aux            LIKE afi_fondo72.rfc, -- RFC auxiliar
       v_mensaje            STRING, -- mensaje para el usuario
       v_indice             SMALLINT, -- indice pivote
       v_indice_aux         SMALLINT, -- indice auxiliar
       v_digito_es_correcto SMALLINT, -- booleana que indica si el digito verificador del RFC es correcto
       v_digito_verificador SMALLINT, -- digito calculado
       v_dv_caracter        VARCHAR(1), -- caracter verificador
       v_arreglo_valores    DYNAMIC ARRAY OF RECORD
         alfanumerico        VARCHAR(1),
         valor               SMALLINT
       END RECORD,
       v_suma               INTEGER, -- suma calculada para el digito verificador
       v_residuo            SMALLINT -- residuo de una division

   -- se llena el arreglo de valores
   FOR v_indice = 1 TO 10 
      LET v_arreglo_valores[v_indice].alfanumerico = v_indice - 1
      LET v_arreglo_valores[v_indice].valor        = v_indice - 1
   END FOR

   -- los siguientes son los caracteres del abecedeario
   LET v_arreglo_valores[11].alfanumerico = "A"
   LET v_arreglo_valores[11].valor        = 10

   LET v_arreglo_valores[12].alfanumerico = "B"
   LET v_arreglo_valores[12].valor        = 11

   LET v_arreglo_valores[13].alfanumerico = "C"
   LET v_arreglo_valores[13].valor        = 12

   LET v_arreglo_valores[14].alfanumerico = "D"
   LET v_arreglo_valores[14].valor        = 13

   LET v_arreglo_valores[15].alfanumerico = "E"
   LET v_arreglo_valores[15].valor        = 14

   LET v_arreglo_valores[16].alfanumerico = "F"
   LET v_arreglo_valores[16].valor        = 15

   LET v_arreglo_valores[17].alfanumerico = "G"
   LET v_arreglo_valores[17].valor        = 16
 
   LET v_arreglo_valores[18].alfanumerico = "H"
   LET v_arreglo_valores[18].valor        = 17

   LET v_arreglo_valores[19].alfanumerico = "I"
   LET v_arreglo_valores[19].valor        = 18

   LET v_arreglo_valores[20].alfanumerico = "J"
   LET v_arreglo_valores[20].valor        = 19

   LET v_arreglo_valores[21].alfanumerico = "K"
   LET v_arreglo_valores[21].valor        = 20

   LET v_arreglo_valores[22].alfanumerico = "L"
   LET v_arreglo_valores[22].valor        = 21

   LET v_arreglo_valores[23].alfanumerico = "M"
   LET v_arreglo_valores[23].valor        = 22

   LET v_arreglo_valores[24].alfanumerico = "N"
   LET v_arreglo_valores[24].valor        = 23

   LET v_arreglo_valores[25].alfanumerico = "&"
   LET v_arreglo_valores[25].valor        = 24

   LET v_arreglo_valores[26].alfanumerico = "O"
   LET v_arreglo_valores[26].valor        = 25

   LET v_arreglo_valores[27].alfanumerico = "P"
   LET v_arreglo_valores[27].valor        = 26

   LET v_arreglo_valores[28].alfanumerico = "Q"
   LET v_arreglo_valores[28].valor        = 27

   LET v_arreglo_valores[29].alfanumerico = "R"
   LET v_arreglo_valores[29].valor        = 28

   LET v_arreglo_valores[30].alfanumerico = "S"
   LET v_arreglo_valores[30].valor        = 29

   LET v_arreglo_valores[31].alfanumerico = "T"
   LET v_arreglo_valores[31].valor        = 30

   LET v_arreglo_valores[32].alfanumerico = "U"
   LET v_arreglo_valores[32].valor        = 31

   LET v_arreglo_valores[33].alfanumerico = "V"
   LET v_arreglo_valores[33].valor        = 32

   LET v_arreglo_valores[34].alfanumerico = "W"
   LET v_arreglo_valores[34].valor        = 33

   LET v_arreglo_valores[35].alfanumerico = "X"
   LET v_arreglo_valores[35].valor        = 34

   LET v_arreglo_valores[36].alfanumerico = "Y"
   LET v_arreglo_valores[36].valor        = 35

   LET v_arreglo_valores[37].alfanumerico = "Z"
   LET v_arreglo_valores[37].valor        = 36

   LET v_arreglo_valores[38].alfanumerico = " "
   LET v_arreglo_valores[38].valor        = 37

   LET v_arreglo_valores[39].alfanumerico = "#"
   LET v_arreglo_valores[39].valor        = 24

   -- se asume que el digito es correcto
   LET v_digito_es_correcto = TRUE
   LET v_suma = 0
   LET v_rfc_aux = p_rfc_modificado

   -- rfc debe ser de 13 digitos
   IF ( LENGTH(v_rfc_aux) < 12 ) THEN
      LET v_mensaje = "La longitud del RFC debe ser de al menos 12 caracteres para poder comprobar el dígito verificador"
      LET v_digito_es_correcto = FALSE      
   ELSE
      -- se procede a calcular el digito verificador, recorriendo cada caracter del RFC
      FOR v_indice = 1 TO 12

         -- se busca el caracter en el arreglo de valores
         FOR v_indice_aux = 1 TO v_arreglo_valores.getLength()
            IF ( v_rfc_aux[v_indice,v_indice] = v_arreglo_valores[v_indice_aux].alfanumerico ) THEN
               -- se toma este indice
               EXIT FOR
            END IF
         END FOR

         -- se toma el valor del caracter, se multiplica por 14 menos el valor de su indice y se acumula
         LET v_suma = v_suma + (v_arreglo_valores[v_indice_aux].valor * (14 - v_indice))
      END FOR
   END IF

   -- para obtener el digito, la suma se divide por 11 y se recobra el residuo
   LET v_residuo = v_suma MOD 11

   -- el digito es el resultado de la resta 11 menos el residuo
   LET v_digito_verificador = 11 - v_residuo

   -- si el digito verificador quedo 10
   IF ( v_digito_verificador = "10" ) THEN
      LET v_dv_caracter = "A"
   ELSE
      -- si es 11
      IF ( v_digito_verificador = 11 ) THEN
         LET v_dv_caracter = "0"
      ELSE
         -- en otro caso se queda el numero calculado
         LET v_dv_caracter = v_digito_verificador
      END IF
   END IF

   -- si el digito calculado no es igual, se indica
   IF ( v_dv_caracter <> v_rfc_aux[13] ) THEN
      LET v_digito_es_correcto = FALSE
      LET v_mensaje = "El dígito verificador del RFC calculado no coincide con el capturado.\n\n¿Desea utilizar el dígito calculado?\n",
                      "\nCapturado: ", v_rfc_aux[13],
                      "\nCalculado: ", v_dv_caracter

      MENU "Validación de RFC"
      ATTRIBUTES ( STYLE="dialog", COMMENT=v_mensaje, IMAGE="question" )
 
         COMMAND "Aceptar"

            -- se cambia el digito verificador
            LET p_rfc_modificado[13] = v_dv_caracter
            LET v_digito_es_correcto = TRUE
            EXIT MENU

         COMMAND "Cancelar"
            LET v_digito_es_correcto = FALSE
            LET v_mensaje = "Es necesario corroborar el dígito verificador.\nPor favor verifique los datos.\n"
            EXIT MENU
      END MENU
   END IF

   -- si hubo algun error, se envia mensaje en pantalla indicando cual es
   IF ( NOT v_digito_es_correcto ) THEN
      CALL fn_mensaje("Error",v_mensaje,"stop")
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_digito_es_correcto, p_rfc_modificado
END FUNCTION

{
======================================================================
Nombre: fn_valida_nombre
Fecha creacion: Mayo 24, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Valida la estructura de un nombre modificado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}

FUNCTION fn_valida_nombre(p_nombre_modificado)

DEFINE p_nombre_modificado  LIKE afi_fondo72.nombre,
       v_nombre_es_correcto SMALLINT, -- booleana que indica si un NOMBRE esta correctamente construido
       v_cadena             STRING, -- cadena para analizar el nombre
       v_mensaje            STRING, -- mensaje para el usuario en caso de error
       v_nombre_aux         STRING, -- nombre auxiliar
       v_tokenizer          base.StringTokenizer, -- para analizar nombre y verificar espacios
       v_indice             SMALLINT

   -- se asume que el NOMBRE esta correcto
   LET v_nombre_es_correcto = TRUE

   -- se asigna el nombre a la cadena
   LET v_cadena = p_nombre_modificado CLIPPED

   -- se revisa que no se tengan caracteres especiales
   FOR v_indice = 1 TO v_cadena.getLength()
      IF ( (v_cadena.getCharAt(v_indice) < "A" OR v_cadena.getCharAt(v_indice) > "Z") AND
           v_cadena.getCharAt(v_indice) <> " " AND
           v_cadena.getCharAt(v_indice) <> "&" AND
           v_cadena.getCharAt(v_indice) <> "#" AND
           v_cadena.getCharAt(v_indice) <> "Ñ" ) THEN
          LET v_mensaje = "El nombre modificado contiene caracteres especiales inválidos."
          LET v_nombre_es_correcto = FALSE
          EXIT FOR
       END IF 
   END FOR

   -- si paso la validacion de caracteres especiales
   IF ( v_nombre_es_correcto  ) THEN
      -- se verifica que el nombre no tenga mas de un espacio entre sus componentes
      LET v_tokenizer = base.StringTokenizer.create(v_cadena," ")

      -- se construye el nombre auxiliar quitando espacios extra que pudieran haber sido quitados por el tokenizer
      LET v_indice = 1
      WHILE ( v_tokenizer.hasMoreTokens() )
         IF ( v_indice = 1 ) THEN
            -- se concatena el nombre
            LET v_nombre_aux = v_tokenizer.nextToken()
         ELSE
            LET v_nombre_aux = v_nombre_aux || " " || v_tokenizer.nextToken()
         END IF

         LET v_indice = v_indice + 1
      END WHILE

      -- se verifica si el nombre auxiliar es igual al modificado, si no, es porque habia espacios en medio
      IF ( v_cadena <> v_nombre_aux ) THEN
         LET v_mensaje = "El nombre modificado contiene más de un espacio entre sus componentes",
                         "\nModificado: ", v_cadena,
                         "\n\nSe sugiera la siguiente corrección:",
                         "\n", v_nombre_aux,
                         "\n\n¿Desea aplicar la corrección sugerida?"

         MENU "Cambio de nombre"
         ATTRIBUTES ( Style="dialog", COMMENT = v_mensaje, IMAGE = "question" )
            COMMAND "Aceptar"
               LET p_nombre_modificado = v_nombre_aux
               LET v_nombre_es_correcto = TRUE
               EXIT MENU

            COMMAND "Cancelar"
               LET v_mensaje = "El nombre modificado contiene más de un espacio entre sus componentes"
               LET v_nombre_es_correcto = FALSE
               EXIT MENU
         END MENU
      END IF
   END IF

   -- si hubo algun error, se envia mensaje en pantalla indicando cual es
   IF ( NOT v_nombre_es_correcto ) THEN
      CALL fn_mensaje("Error",v_mensaje,"stop")
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_nombre_es_correcto, p_nombre_modificado

END FUNCTION