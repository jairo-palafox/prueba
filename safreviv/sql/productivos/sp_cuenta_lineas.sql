






create procedure "safreviv".sp_cuenta_lineas( p_ruta_rescate char(250),
                                   p_archivo_ent  char(60),
                                   p_archivo_sal  char(300))

define v_comando char(700);
define v_ruta_ejecutable char(30);

select ruta_rescate 
  into v_ruta_ejecutable
  from seg_modulo
 where modulo_cod = "glo";

let v_comando = " cd "||trim(v_ruta_ejecutable)||"; ./cuenta_lineas.sh "||
                trim(p_ruta_rescate)||" "||
                trim(p_archivo_ent)||" "||
                trim(p_archivo_sal);

system v_comando;
                
end procedure;


