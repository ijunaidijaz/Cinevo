package nemosofts.streambox.adapter.epg;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import nemosofts.streambox.item.ItemEpg;
import nemosofts.streambox.item.ItemLive;

public class ItemPost implements Serializable{

	private final String id;
	private final String type;
	private List<ItemLive> arrayListLive = new ArrayList<>();
	private List<ItemEpg> arrayListEpg = new ArrayList<>();

	public ItemPost(String id, String type) {
		this.id = id;
		this.type = type;
	}

	public String getId() {
		return id;
	}

	public String getType() {
		return type;
	}

	public List<ItemLive> getArrayListLive() {
		return arrayListLive;
	}
	public void setArrayListLive(List<ItemLive> arrayListLive) {
		this.arrayListLive = arrayListLive;
	}

	public List<ItemEpg> getArrayListEpg() {
		return arrayListEpg;
	}
	public void setArrayListEpg(List<ItemEpg> arrayListEpg) {
		this.arrayListEpg = arrayListEpg;
	}
}