package nemosofts.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import org.json.JSONObject;

import java.util.ArrayList;

import nemosofts.streambox.interfaces.MovieIDListener;
import nemosofts.streambox.item.ItemInfoMovies;
import nemosofts.streambox.item.ItemMoviesData;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.helper.SPHelper;
import okhttp3.RequestBody;

public class LoadMovieID extends AsyncTask<String, String, String> {

    private final SPHelper spHelper;
    private final MovieIDListener listener;
    private final RequestBody requestBody;
    private final ArrayList<ItemInfoMovies> arrayListInfo = new ArrayList<>();
    private final ArrayList<ItemMoviesData> arrayListData = new ArrayList<>();

    public LoadMovieID(Context ctx, MovieIDListener listener, RequestBody requestBody) {
        this.listener = listener;
        this.requestBody = requestBody;
        spHelper = new SPHelper(ctx);
    }

    @Override
    protected void onPreExecute() {
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String... strings) {
        try {
            String json = ApplicationUtil.responsePost(spHelper.getAPI(), requestBody);
            JSONObject jsonObject = new JSONObject(json);

            if (jsonObject.has("info")) {

                JSONObject c =  jsonObject.getJSONObject("info");

                String tmdb_id = "";
                if (c.has("tmdb_id")){
                    tmdb_id = c.getString("tmdb_id");
                }

                String name = "";
                if (c.has("name")){
                    name = c.getString("name");
                }

                String movie_image = "";
                if (c.has("movie_image")){
                    movie_image = c.getString("movie_image");
                }

                String release_date = "";
                if (c.has("release_date")){
                    release_date = c.getString("release_date");
                }

                String episode_run_time = "";
                if (c.has("episode_run_time")){
                    episode_run_time = c.getString("episode_run_time");
                }

                String youtube_trailer = "";
                if (c.has("youtube_trailer")){
                    youtube_trailer = c.getString("youtube_trailer");
                }

                String director = "";
                if (c.has("director")){
                    director = c.getString("director");
                }

                String cast = "";
                if (c.has("cast")){
                    cast = c.getString("cast");
                }

                String plot = "";
                if (c.has("plot")){
                    plot = c.getString("plot");
                }

                String genre = "";
                if (c.has("genre")){
                    genre = c.getString("genre");
                }

                String rating = "";
                if (c.has("rating")){
                    rating = c.getString("rating");
                }

                ItemInfoMovies objItem1 = new ItemInfoMovies(tmdb_id, name, movie_image, release_date, episode_run_time, youtube_trailer, director, cast, plot, genre, rating);
                arrayListInfo.add(objItem1);
            }

            if (jsonObject.has("movie_data")) {

                JSONObject c =  jsonObject.getJSONObject("movie_data");

                String stream_id = c.getString("stream_id");
                String name = c.getString("name");
                String container = c.getString("container_extension");

                ItemMoviesData objItem2 = new ItemMoviesData(stream_id, name, container);
                arrayListData.add(objItem2);
            }
            return "1";
        } catch (Exception e) {
            e.printStackTrace();
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s, arrayListInfo, arrayListData);
        super.onPostExecute(s);
    }
}