####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAW14.inc                                    #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>09 JUNIO 2012                                 #
####################################################################

GLOBALS
   CONSTANT WS_MARCA          CHAR(5) = 'cre_1'
   CONSTANT WS_DESMARCA       CHAR(5) = 'cre_2'

   TYPE solicita_marca RECORD
      apeMaterno              STRING,
      apePaterno              STRING,
      fechaPresentacion       STRING,        --Tiene que tener el formato AAAAMMDD
      nombres                 STRING,
      nss                     STRING,
      numCreditoInfonavit     STRING,
      rfc                     STRING,
      sitCredito              STRING,        --Tiene que ser '2' para solicitud de marca
      tipoCredito             STRING         --solo acepta los siguientes valores '01', '02', '04'
   END RECORD

   TYPE respuesta_marca RECORD
      apeMaternoBD            STRING,
      apeMaternoInfo          STRING,
      apePaternoBD            STRING,
      apePaternoInfo          STRING,
      diagProceso             STRING,
      nombresBD               STRING,
      nombresInfo             STRING,
      nss                     STRING,
      resultOperacion         STRING,
      rfc                     STRING,
      rfcBD                   STRING,
      tipTrabajador           STRING
   END RECORD

   TYPE solicita_desmarca RECORD
      apeMaterno              STRING,
      apePaterno              STRING,
      fechaPresentacion       STRING,
      nombres                 STRING,
      nss                     STRING,
      numCreditoInfonavit     STRING,
      rfc                     STRING
   END RECORD

   TYPE respuesta_desmarca RECORD
      apeMaternoBD            STRING,
      apeMaternoInfo          STRING,
      apePaternoBD            STRING,
      apePaternoInfo          STRING,
      diagProceso             STRING,
      nombresBD               STRING,
      nombresInfo             STRING,
      nss                     STRING,
      resultOperacion         STRING,
      rfc                     STRING,
      rfcBD                   STRING,
      tipCreditDesm           STRING
   END RECORD

END GLOBALS