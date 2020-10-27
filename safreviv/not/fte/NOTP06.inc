GLOBALS

   TYPE pendientes RECORD 
      folio_not           decimal(9,0),
      folio_operativo     decimal(9,0),
      id_detalle_archivo  decimal(9,0),
      nombre_archivo      varchar(100),
      código_respuesta    INTEGER,
      desc_respuesta      STRING 
   END RECORD
   
END GLOBALS