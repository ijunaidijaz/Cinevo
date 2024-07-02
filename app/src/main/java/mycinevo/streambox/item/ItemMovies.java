package mycinevo.streambox.item;

import java.io.Serializable;

public class ItemMovies implements Serializable {

	private final String name;
	private final String stream_id;
	private final String stream_icon;
	private final String rating;
	private final String cat_name;

	public ItemMovies(String name, String stream_id, String stream_icon, String rating, String cat_name) {
		this.name = name;
		this.stream_id = stream_id;
		this.stream_icon = stream_icon;
		this.rating = rating;
		this.cat_name = cat_name;
	}

	public String getName() {
		return name;
	}

	public String getStreamID() {
		return stream_id;
	}

	public String getStreamIcon() {
		return stream_icon;
	}

	public String getRating() {
		return rating;
	}

	public String getCatName() {
		return cat_name;
	}
}
