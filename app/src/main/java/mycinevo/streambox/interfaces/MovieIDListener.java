package mycinevo.streambox.interfaces;

import java.util.ArrayList;

import mycinevo.streambox.item.ItemInfoMovies;
import mycinevo.streambox.item.ItemMoviesData;

public interface MovieIDListener {
    void onStart();
    void onEnd(String success, ArrayList<ItemInfoMovies> arrayListInfo , ArrayList<ItemMoviesData> arrayListMoviesData);
}