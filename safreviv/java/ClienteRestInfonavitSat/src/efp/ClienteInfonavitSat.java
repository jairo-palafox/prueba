package efp;

import java.io.File; 

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.glassfish.jersey.media.multipart.FormDataMultiPart;
import org.glassfish.jersey.media.multipart.MultiPartFeature;
import org.glassfish.jersey.media.multipart.file.FileDataBodyPart;
import org.json.JSONArray;
import org.json.JSONObject;

public class ClienteInfonavitSat {

	public static void main(String[] args) {
		String arrRespuesta;
		ClienteInfonavitSat prueba=new ClienteInfonavitSat();
		System.out.println("Parametros entrada:");
		System.out.println("Ruta .cer:"+args[0]);
		System.out.println("Ruta .key:"+args[1]);
		System.out.println("Password:"+args[2]);
		arrRespuesta=prueba.ClienteInfonavitSat(args[0],args[1],args[2]);
		System.out.println("Respuesta WS");
		System.out.println("Cod ejecucion:"+arrRespuesta);
	}
	
	public ClienteInfonavitSat() {
		
	}
	
	
	public String ClienteInfonavitSat(String rutaCer,String rutaKey,String password){
		String respuesta;
		String arrRespuesta;
		String estadoEjecucion;
		String rfc="";

		try {
		
		System.out.println("Generando cliente...");
		Client cliente = ClientBuilder.newBuilder().register(MultiPartFeature.class).build();
	//	cliente.register(new LoggingFilter());
		//QA
	//	WebTarget webTarget=cliente.target("http://10.82.0.39:9081/firmasatprod/protect/auth");
		//PROD
		WebTarget webTarget=cliente.target("http://firmaelectronica.infonavit.org.mx:9080/firmasat/protect/auth");
		
		System.out.println("Generando archivos a enviar...");
		//se crean los archivos
		FileDataBodyPart filePartCer=new FileDataBodyPart("cert",new File(rutaCer));	
		FileDataBodyPart filePartKey=new FileDataBodyPart("key",new File(rutaKey));
		
		FormDataMultiPart multiPart=new FormDataMultiPart();
		multiPart.setMediaType(MediaType.MULTIPART_FORM_DATA_TYPE);

		System.out.println("Insertando partes del request...");
		multiPart.bodyPart(filePartCer);
		multiPart.bodyPart(filePartKey);
		multiPart.field("pass",password);
		
		System.out.println("Creando response...");
		Response response=webTarget.request().post(Entity.entity(multiPart, multiPart.getMediaType()));
		response.bufferEntity();
		respuesta=response.readEntity(String.class);
		
		//sa pasa el json a la clase
	//	System.out.println("respose String....:"+respuesta.toString());

		JSONObject jsonObject=new JSONObject(respuesta);
		
	//	System.out.println("respose String....:"+jsonObject.toString());
			
		//se mapea del objeto JSON a la clase
			
		ClienteInfonavitRespuestaSat clienteInfonavitRespuestaSat=new ClienteInfonavitRespuestaSat(
																	   jsonObject.get("expDate").toString(),
																	   jsonObject.get("name").toString(),
																	   jsonObject.get("response").toString(),
																	   jsonObject.get("success").toString());
		
		
		System.out.println("respose JSON....:");
		System.out.println("expDate:"+clienteInfonavitRespuestaSat.getExpDate());
		System.out.println("name:"+clienteInfonavitRespuestaSat.getName());			System.out.println("response:"+clienteInfonavitRespuestaSat.getResponse());
		System.out.println("success:"+clienteInfonavitRespuestaSat.getSuccess());
		
		if (clienteInfonavitRespuestaSat.getSuccess()=="true") {
			//el certificado es valido
			System.out.println("Certificado Valido");
			//se descompone el nombre

			System.out.println("Se descompone NAME....:");
			JSONArray jsonArrayName=new JSONArray("[{"+jsonObject.get("name").toString()+"}]");
			JSONObject jsonObjectName = jsonArrayName.getJSONObject(0);
			System.out.println("Descompuesto:"+jsonObjectName.get("OID.2.5.4.45"));
			rfc=jsonObjectName.get("OID.2.5.4.45").toString();
			
			estadoEjecucion="00";
		}else {
			System.out.println("Certificado Invalido");	
			estadoEjecucion="01";
		}

		
		} catch (Exception e) {
			System.out.println("Ocurrio un error al ejecutar el cliente, cod:"+e.getMessage());
			estadoEjecucion="10";
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		arrRespuesta=estadoEjecucion+rfc;
		//arrRespuesta="";
		
		return arrRespuesta;
					
	}
}
