package mycinevo.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import java.util.ArrayList;
import java.util.Collections;

import mycinevo.streambox.interfaces.GetMovieListener;
import mycinevo.streambox.item.ItemMovies;
import mycinevo.streambox.util.helper.JSHelper;

public class GetMoviesPlaylist extends AsyncTask<String, String, String> {

    private final JSHelper jsHelper;
    private final GetMovieListener listener;
    private final ArrayList<ItemMovies> itemMovies = new ArrayList<>();
    private final String cat_name;
    private final int page;
    int itemsPerPage = 10;

    public GetMoviesPlaylist(Context ctx, int page, String cat_name, GetMovieListener listener) {
        this.listener = listener;
        this.cat_name = cat_name;
        this.page = page;
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
            final ArrayList<ItemMovies> arrayList = new ArrayList<>();
            final ArrayList<ItemMovies> arrayListAll = new ArrayList<>(jsHelper.getMoviesPlaylist());

            for (int i = 0; i < arrayListAll.size(); i++) {
                addOrUpdateItem(arrayList, cat_name, arrayListAll.get(i));
            }
            if (Boolean.TRUE.equals(jsHelper.getIsMovieOrder())){
                Collections.reverse(arrayList);
            }
            if (!arrayList.isEmpty()){
                int startIndex = (page - 1) * itemsPerPage;
                int endIndex = Math.min(startIndex + itemsPerPage, arrayList.size());
                for (int i = startIndex; i < endIndex; i++) {
                    itemMovies.add(arrayList.get(i));
                }
            }
            return "1";
        } catch (Exception e) {
            e.printStackTrace();
            return "0";
        }
    }

    private void addOrUpdateItem(ArrayList<ItemMovies> arrayList, String catName, ItemMovies itemMovies) {
        if (itemMovies != null){
            boolean idExists = itemMovies.getCatName().equals(catName);
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