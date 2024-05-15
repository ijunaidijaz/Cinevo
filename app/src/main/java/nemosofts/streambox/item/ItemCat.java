package mycinevo.streambox.item;

import java.io.Serializable;

public class ItemCat implements Serializable {
	
	private final String id;
	private final String name;
	private final String page;

	public ItemCat(String id, String name, String page) {
		this.id = id;
		this.name = name;
		this.page = page;
	}

	public String getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	public String getPage() {
		return page;
	}
}
