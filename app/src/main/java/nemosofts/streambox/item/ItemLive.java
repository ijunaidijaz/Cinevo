package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemLive implements Serializable {

	private final String name;
	private final String stream_id;
	private final String stream_icon;
	private final String cat_name;

	public ItemLive(String name, String stream_id, String stream_icon, String cat_name) {
		this.name = name;
		this.stream_id = stream_id;
		this.stream_icon = stream_icon;
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

	public String getCatName() {
		return cat_name;
	}
}
