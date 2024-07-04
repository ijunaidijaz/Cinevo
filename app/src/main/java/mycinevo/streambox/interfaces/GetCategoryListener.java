package mycinevo.streambox.interfaces;

import java.util.ArrayList;

import mycinevo.streambox.item.ItemCat;

public interface GetCategoryListener {
    void onStart();
    void onEnd(boolean success, ArrayList<ItemCat> arrayListCat);
}