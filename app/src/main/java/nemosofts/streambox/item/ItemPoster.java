package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemPoster implements Serializable {

	private final String poster;

	public ItemPoster(String poster) {
		this.poster = poster;
	}

	public String getPoster() {
		return poster;
	}
}
