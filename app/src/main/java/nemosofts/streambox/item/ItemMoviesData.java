package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemMoviesData implements Serializable {

	private final String stream_id;
	private final String name;
	private final String container_extension;
	Boolean is_download = false;

	public ItemMoviesData(String stream_id, String name, String container_extension) {
		this.stream_id = stream_id;
		this.name = name;
		this.container_extension = container_extension;
	}

	public String getStreamID() {
		return stream_id;
	}

	public String getName() {
		return name;
	}

	public String getContainerExtension() {
		return container_extension;
	}

	public void setDownload(Boolean isDownload) {
		is_download = isDownload;
	}
	public Boolean IsDownload() {
		return is_download;
	}
}
