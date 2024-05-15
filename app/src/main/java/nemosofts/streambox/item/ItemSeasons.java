package mycinevo.streambox.item;

import java.io.Serializable;

public class ItemSeasons implements Serializable {

	private final String name;
	private final String season_number;

	public ItemSeasons(String name, String season_number) {
		this.name = name;
		this.season_number = season_number;
	}

	public String getName() {
		return name;
	}

	public String getSeasonNumber() {
		return season_number;
	}
}
