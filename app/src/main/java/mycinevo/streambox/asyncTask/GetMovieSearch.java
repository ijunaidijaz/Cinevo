package mycinevo.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import java.util.ArrayList;

import mycinevo.streambox.interfaces.GetMovieListener;
import mycinevo.streambox.item.ItemMovies;
import mycinevo.streambox.util.helper.JSHelper;

public class GetMovieSearch extends AsyncTask<String, String, String> {

    private final JSHelper jsHelper;
    private final GetMovieListener listener;
    private final ArrayList<ItemMovies> itemMovies = new ArrayList<>();
    private final String searchText;
    private final Boolean isPlaylist;

    public GetMovieSearch(Context ctx, Boolean isPlaylist, String searchText, GetMovieListener listener) {
        this.listener = listener;
        this.isPlaylist = isPlaylist;
        this.searchText = searchText;
        jsHelper = new JSHelper(ctx);
    }

    @Override
    protected void onPreExecute() {
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String... strings) {
        try {
            if (Boolean.TRUE.equals(isPlaylist)){
                final ArrayList<ItemMovies> arrayListAll = new ArrayList<>(jsHelper.getMoviesPlaylist());
                for (int i = 0; i < arrayListAll.size(); i++) {
                    addOrUpdateItem(itemMovies, arrayListAll.get(i));
                }
            } else {
                itemMovies.addAll(jsHelper.getMoviesSearch(searchText));
            }
            return "1";
        } catch (Exception e) {
            e.printStackTrace();
            return "0";
        }
    }

    private void addOrUpdateItem(ArrayList<ItemMovies> arrayList, ItemMovies itemMovies) {
        if (itemMovies != null){
            boolean idExists = itemMovies.getName().toLowerCase().contains(searchText);
            if (idExists) {
                arrayList.add(itemMovies);
            }
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s,itemMovies);
        super.onPostExecute(s);
    }
}