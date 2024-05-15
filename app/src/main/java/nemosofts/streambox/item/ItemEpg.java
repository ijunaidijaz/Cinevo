package mycinevo.streambox.item;

import java.io.Serializable;

public class ItemEpg implements Serializable {

	private final String start;
	private final String end;
	private final String title;
	private final String start_timestamp;
	private final String stop_timestamp;

	public ItemEpg(String start, String end, String title, String start_timestamp, String stop_timestamp) {
		this.start = start;
		this.end = end;
		this.title = title;
		this.start_timestamp = start_timestamp;
		this.stop_timestamp = stop_timestamp;
	}

	public String getStart() {
		return start;
	}

	public String getEnd() {
		return end;
	}

	public String getTitle() {
		return title;
	}

	public String getStartTimestamp() {
		return start_timestamp;
	}

	public String getStopTimestamp() {
		return stop_timestamp;
	}
}
