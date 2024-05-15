package mycinevo.streambox.interfaces;

import java.util.ArrayList;

import mycinevo.streambox.item.ItemPoster;

public interface PosterListener {
    void onStart();
    void onEnd(String success, String verifyStatus, String message, ArrayList<ItemPoster> arrayList);
}