






create procedure "safreviv".sp_arma_archivos( p_registro     char(2),
                                   p_ruta_rescate char(250),
                                   p_archivo      char(60),
                                   p_usuario      char(20),
                                   p_archivo_out  char(60))

define v_comando char(700);
define v_ruta_ejecutable char(30);

select ruta_rescate 
  into v_ruta_ejecutable
  from seg_modulo
 where modulo_cod = "glo";

let v_comando = " cd "||trim(v_ruta_ejecutable)||"; ./arma_archivos.sh "||
                trim(p_registro)||" "||
                trim(p_ruta_rescate)||" "||
                trim(p_archivo)||" "||
                trim(p_usuario)||" "||
                trim(p_archivo_out);

system v_comando;
                
end procedure;


