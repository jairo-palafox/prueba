
MAIN
DEFINE v_resultado   SMALLINT,
       v_nss         CHAR(11),
       v_indice      INTEGER 

   DEFINE arr_casos_crm DYNAMIC ARRAY OF RECORD
         casos           STRING, 
         fecha_creacion  CHAR(10),
         status          CHAR(5),
         fecha_modifica  CHAR(10),
         clase_operacion STRING,
         tipificacion    CHAR(4),
         texto_status    STRING,
         permite_adoc    CHAR(05),
         marca_origen    STRING
   END RECORD 

   LET v_nss = ARG_VAL(1)
   LET v_indice = 0
   LET v_resultado = 0

   -- se inicializan las variables de retorno del saldo
   CALL arr_casos_crm.clear()

   DISPLAY "Prueba de comunicacion con CRM, Busca Casos"
   DISPLAY "Valida casos para el NSS:", v_nss, "----"
   DISPLAY "Llama a la funcion fn_busca_caso_crm"
   
   CALL fn_busca_caso_crm(v_nss) RETURNING v_resultado, arr_casos_crm
   
   DISPLAY "Regresa de la funcion con resultado:", v_resultado, "----"

   IF v_resultado = 0 AND arr_casos_crm.getLength() > 0 THEN 
      FOR v_indice = 1 TO arr_casos_crm.getLength()
         DISPLAY "Los valores regresados son:"
         DISPLAY "arr_casos_crm[v_indice].casos           ---", arr_casos_crm[v_indice].casos, "---"
         DISPLAY "arr_casos_crm[v_indice].clase_operacion ---", arr_casos_crm[v_indice].clase_operacion, "---"
         DISPLAY "arr_casos_crm[v_indice].fecha_creacion  ---", arr_casos_crm[v_indice].fecha_creacion, "---"
         DISPLAY "arr_casos_crm[v_indice].fecha_modifica  ---", arr_casos_crm[v_indice].fecha_modifica, "---"
         DISPLAY "arr_casos_crm[v_indice].marca_origen    ---", arr_casos_crm[v_indice].marca_origen, "---"
         DISPLAY "arr_casos_crm[v_indice].permite_adoc    ---", arr_casos_crm[v_indice].permite_adoc, "---"
         DISPLAY "arr_casos_crm[v_indice].status          ---", arr_casos_crm[v_indice].status, "---"
         DISPLAY "arr_casos_crm[v_indice].texto_status    ---", arr_casos_crm[v_indice].texto_status, "---"
         DISPLAY "arr_casos_crm[v_indice].tipificacion    ---", arr_casos_crm[v_indice].tipificacion, "---"
      END FOR
   ELSE 
      IF v_resultado = 0 THEN 
         DISPLAY " Resultado al llamado exitoso, sin casos abiertos"
      ELSE
         DISPLAY " Problemas en el llamado al servicio"
      END IF 
   END IF 

END MAIN