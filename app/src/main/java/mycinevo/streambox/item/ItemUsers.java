package mycinevo.streambox.item;

public class ItemUsers {

	private final String id;
	private final String user_type;
	private final String user_name;
	private final String user_password;
	private final String dns_base;
	private final String device_id;

	public ItemUsers(String id, String user_type, String user_name, String user_password, String dns_base, String device_id) {
		this.id = id;
		this.user_type = user_type;
		this.user_name = user_name;
		this.user_password = user_password;
		this.dns_base = dns_base;
		this.device_id = device_id;
	}

	public String getId() {
		return id;
	}

	public String getUserType() {
		return user_type;
	}

	public String getUserName() {
		return user_name;
	}

	public String getUserPassword() {
		return user_password;
	}

	public String getDnsBase() {
		return dns_base;
	}

	public String getDeviceID() {
		return device_id;
	}
}