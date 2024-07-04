package mycinevo.streambox.interfaces;

import java.util.ArrayList;

import mycinevo.streambox.item.ItemPlaylist;

public interface LoadPlaylistListener {
    void onStart();
    void onEnd(String success, String msg, ArrayList<ItemPlaylist> arrayListPlaylist);
}