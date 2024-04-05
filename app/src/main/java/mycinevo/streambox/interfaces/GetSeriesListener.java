package mycinevo.streambox.interfaces;

import java.util.ArrayList;

import mycinevo.streambox.item.ItemSeries;

public interface GetSeriesListener {
    void onStart();
    void onEnd(String success, ArrayList<ItemSeries> arrayListSeries);
}