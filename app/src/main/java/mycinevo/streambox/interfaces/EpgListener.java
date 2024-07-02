package mycinevo.streambox.interfaces;

import java.util.ArrayList;

import mycinevo.streambox.item.ItemEpg;

public interface EpgListener {
    void onStart();
    void onEnd(String success, ArrayList<ItemEpg> epgArrayList);
}