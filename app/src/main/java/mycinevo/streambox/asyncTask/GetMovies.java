package mycinevo.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import mycinevo.streambox.interfaces.GetMovieListener;
import mycinevo.streambox.item.ItemMovies;
import mycinevo.streambox.util.helper.DBHelper;
import mycinevo.streambox.util.helper.JSHelper;

public class GetMovies extends AsyncTask<String, String, String> {

    private final DBHelper dbHelper;
    private final JSHelper jsHelper;
    private final GetMovieListener listener;
    private final ArrayList<ItemMovies> itemMovies = new ArrayList<>();
    private final int is_page;
    private final String cat_id;
    private final int page;
    int itemsPerPage = 15;

    public GetMovies(Context ctx, int page, String cat_id, int is_page, GetMovieListener listener) {
        this.listener = listener;
        this.is_page = is_page;
        this.cat_id = cat_id;
        this.page = page;
        jsHelper = new JSHelper(ctx);
        dbHelper = new DBHelper(ctx);
    }

    @Override
    protected void onPreExecute() {
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String... strings) {
        try {
            if (Boolean.TRUE.equals(is_page == 1)){
                itemMovies.addAll(dbHelper.getMovies(DBHelper.TABLE_FAV_MOVIE, jsHelper.getIsLiveOrder()));
            } else  if (Boolean.TRUE.equals(is_page == 2)){
                itemMovies.addAll(dbHelper.getMovies(DBHelper.TABLE_RECENT_MOVIE, jsHelper.getIsLiveOrder()));
            } else if (Boolean.TRUE.equals(is_page == 3)){
                final ArrayList<ItemMovies> arrayList = new ArrayList<>(jsHelper.getMoviesRe());
                if (!arrayList.isEmpty()){
                    Collections.sort(arrayList, new Comparator<ItemMovies>() {
                        @Override
                        public int compare(ItemMovies o1, ItemMovies o2) {
                            return Integer.compare(Integer.parseInt(o1.getStreamID()), Integer.parseInt(o2.getStreamID()));
                        }
                    });
                    Collections.reverse(arrayList);
                    for (int i = 0; i < arrayList.size(); i++) {
                        itemMovies.add(arrayList.get(i));
                        if (i == 49){
                            break;
                        }
                    }
                    if (Boolean.TRUE.equals(jsHelper.getIsMovieOrder()) && !itemMovies.isEmpty()){
                        Collections.reverse(itemMovies);
                    }
                }
            } else {
                final ArrayList<ItemMovies> arrayList = new ArrayList<>(jsHelper.getMovies(cat_id));
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
            }
            return "1";
        } catch (Exception e) {
            e.printStackTrace();
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s,itemMovies);
        super.onPostExecute(s);
    }
}