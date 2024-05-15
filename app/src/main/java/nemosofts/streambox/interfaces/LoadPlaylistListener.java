package nemosofts.streambox.interfaces;

import java.util.ArrayList;

import nemosofts.streambox.item.ItemPlaylist;

public interface LoadPlaylistListener {
    void onStart();
    void onEnd(String success, ArrayList<ItemPlaylist> arrayListPlaylist);
}