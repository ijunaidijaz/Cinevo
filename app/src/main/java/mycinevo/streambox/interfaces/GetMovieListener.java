package mycinevo.streambox.interfaces;

import java.util.ArrayList;

import mycinevo.streambox.item.ItemMovies;

public interface GetMovieListener {
    void onStart();
    void onEnd(String success, ArrayList<ItemMovies> arrayListMovies);
}