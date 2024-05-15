package mycinevo.streambox.item;

import java.io.Serializable;

public class ItemVideoDownload implements Serializable {

	private final String name;
	private final String stream_id;
	private String stream_icon;
	private String video_url;
	private final String container_extension;
	private String tempName;
	private int progress = 0;

	public ItemVideoDownload(String name, String stream_id, String stream_icon, String video_url, String container_extension) {
		this.name = name;
		this.stream_id = stream_id;
		this.stream_icon = stream_icon;
		this.video_url = video_url;
		this.container_extension = container_extension;
	}

	public String getName() {
		return name;
	}

	public String getStreamID() {
		return stream_id;
	}

	public String getContainerExtension() {
		return container_extension;
	}

	public String getStreamIcon() {
		return stream_icon;
	}
	public void setStreamIcon(String streamIcon) {
		this.stream_icon = streamIcon;
	}

	public void setTempName(String tempName) {
		this.tempName = tempName;
	}
	public String getTempName() {
		return tempName;
	}

	public String getVideoURL() {
		return video_url;
	}
	public void setVideoURL(String video_url) {
		this.video_url = video_url;
	}

	public int getProgress() {
		return progress;
	}
	public void setProgress(int progress) {
		this.progress = progress;
	}
}