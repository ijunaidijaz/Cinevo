package mycinevo.streambox.interfaces;

import java.util.ArrayList;

import mycinevo.streambox.item.ItemEpisodes;
import mycinevo.streambox.item.ItemInfoSeasons;
import mycinevo.streambox.item.ItemSeasons;

public interface SeriesIDListener {
    void onStart();
    void onEnd(String success, ArrayList<ItemInfoSeasons> arrayListInfo, ArrayList<ItemSeasons> arrayListSeasons, ArrayList<ItemEpisodes> arrayListEpisodes);
}