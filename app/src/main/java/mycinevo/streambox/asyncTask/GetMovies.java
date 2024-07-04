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

    private static final int PAGE_TYPE_FAV = 1;
    private static final int PAGE_TYPE_RECENT = 2;
    private static final int PAGE_TYPE_RECENT_ADD = 3;

    private final DBHelper dbHelper;
    private final JSHelper jsHelper;
    private final GetMovieListener listener;
    private final ArrayList<ItemMovies> itemMovies = new ArrayList<>();
    private final int isPage;
    private final String catId;
    private final int page;
    private static final int ITEMS_PER_PAGE = 15;

    public GetMovies(Context ctx, int page, String catId , int isPage, GetMovieListener listener) {
        this.listener = listener;
        this.isPage = isPage;
        this.catId  = catId ;
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
            switch (isPage) {
                case PAGE_TYPE_FAV:
                    // Fetch favorite movies
                    itemMovies.addAll(dbHelper.getMovies(DBHelper.TABLE_FAV_MOVIE, jsHelper.getIsLiveOrder()));
                    break;
                case PAGE_TYPE_RECENT:
                    // Fetch recent movies
                    itemMovies.addAll(dbHelper.getMovies(DBHelper.TABLE_RECENT_MOVIE, jsHelper.getIsLiveOrder()));
                    break;
                case PAGE_TYPE_RECENT_ADD:
                    // Fetch and sort recommended movies
                    ArrayList<ItemMovies> recommendedMovies = new ArrayList<>(jsHelper.getMoviesRe());
                    if (!recommendedMovies.isEmpty()) {
                        Collections.sort(recommendedMovies, new Comparator<ItemMovies>() {
                            @Override
                            public int compare(ItemMovies o1, ItemMovies o2) {
                                return Integer.compare(Integer.parseInt(o1.getStreamID()), Integer.parseInt(o2.getStreamID()));
                            }
                        });
                        Collections.reverse(recommendedMovies);
                        for (int i = 0; i < Math.min(50, recommendedMovies.size()); i++) {
                            itemMovies.add(recommendedMovies.get(i));
                        }
                        if (Boolean.TRUE.equals(jsHelper.getIsMovieOrder())) {
                            Collections.reverse(itemMovies);
                        }
                    }
                    break;
                default:
                    // Fetch movies by category with pagination
                    ArrayList<ItemMovies> categoryMovies = new ArrayList<>(jsHelper.getMovies(catId));
                    if (Boolean.TRUE.equals(jsHelper.getIsMovieOrder())) {
                        Collections.reverse(categoryMovies);
                    }
                    if (!categoryMovies.isEmpty()) {
                        int startIndex = (page - 1) * ITEMS_PER_PAGE;
                        int endIndex = Math.min(startIndex + ITEMS_PER_PAGE, categoryMovies.size());
                        for (int i = startIndex; i < endIndex; i++) {
                            itemMovies.add(categoryMovies.get(i));
                        }
                    }
                    break;
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