package mycinevo.streambox.interfaces;

import java.util.ArrayList;

import mycinevo.streambox.item.ItemLive;

public interface GetLiveListener {
    void onStart();
    void onEnd(String success, ArrayList<ItemLive> arrayListLive);
}