package mycinevo.streambox.item;

import java.io.Serializable;

public class ItemCast implements Serializable {

	private final String id;
	private final String name;
	private final String profile;

	public ItemCast(String id, String name, String profile) {
		this.id = id;
		this.name = name;
		this.profile = profile;
	}

	public String getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	public String getProfile() {
		return profile;
	}
}
