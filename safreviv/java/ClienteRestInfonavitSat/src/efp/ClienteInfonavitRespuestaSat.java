package efp;

public class ClienteInfonavitRespuestaSat {
	String expDate;
	String name;
	String response;
	String success;
	public String getExpDate() {
		return expDate;
	}
	public void setExpDate(String expDate) {
		this.expDate = expDate;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
// TODO Auto-generated constructor stub
	public ClienteInfonavitRespuestaSat(String expDate, String name, String response, String success) {
		super();
		this.expDate = expDate;
		this.name = name;
		this.response = response;
		this.success = success;
	}
	public ClienteInfonavitRespuestaSat() {
		
	}

	public String getResponse() {
		return response;
	}
	public void setResponse(String response) {
		this.response = response;
	}
	public String getSuccess() {
		return success;
	}
	public void setSuccess(String success) {
		this.success = success;
	}

	
	
}
