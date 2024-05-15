package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemMediaData implements Serializable {

	private final String title;
	private final String videoType;
	private final String frameWidth;
	private final String frameHeight;

	public ItemMediaData(String title, String videoType, String frameWidth, String frameHeight) {
		this.title = title;
		this.videoType = videoType;
		this.frameWidth = frameWidth;
		this.frameHeight = frameHeight;
	}

	public String getTitle() {
		return title;
	}

	public String getVideoType() {
		return videoType;
	}

	public String getFrameWidth() {
		return frameWidth;
	}

	public String getFrameHeight() {
		return frameHeight;
	}
}
