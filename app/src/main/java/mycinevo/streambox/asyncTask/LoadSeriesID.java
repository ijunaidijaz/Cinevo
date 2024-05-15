package mycinevo.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;

import mycinevo.streambox.interfaces.SeriesIDListener;
import mycinevo.streambox.item.ItemEpisodes;
import mycinevo.streambox.item.ItemInfoSeasons;
import mycinevo.streambox.item.ItemSeasons;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.helper.SPHelper;
import okhttp3.RequestBody;

public class LoadSeriesID extends AsyncTask<String, String, String> {

    private final SPHelper spHelper;
    private final SeriesIDListener listener;
    private final RequestBody requestBody;
    private final ArrayList<ItemInfoSeasons> arrayListInfo = new ArrayList<>();
    private final ArrayList<ItemSeasons> arrayListSeries = new ArrayList<>();
    private final ArrayList<ItemEpisodes> arrayListEpisodes = new ArrayList<>();

    public LoadSeriesID(Context ctx, SeriesIDListener listener, RequestBody requestBody) {
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

                String name="";
                if (c.has("name")){
                    name = c.getString("name");
                }
                String cover="";
                if (c.has("cover")){
                    cover = c.getString("cover");
                }
                String plot="";
                if (c.has("plot")){
                    plot = c.getString("plot");
                }
                String director="";
                if (c.has("director")){
                    director = c.getString("director");
                }
                String genre="";
                if (c.has("genre")){
                    genre = c.getString("genre");
                }
                String releaseDate="";
                if (c.has("releaseDate")){
                    releaseDate = c.getString("releaseDate");
                }
                String rating="";
                if (c.has("rating")){
                    rating = c.getString("rating");
                }
                String rating_5based="";
                if (c.has("rating_5based")){
                    rating_5based = c.getString("rating_5based");
                }
                String youtube_trailer="";
                if (c.has("youtube_trailer")){
                    youtube_trailer = c.getString("youtube_trailer");
                }

                ItemInfoSeasons objItem = new ItemInfoSeasons(name, cover, plot, director, genre, releaseDate, rating, rating_5based, youtube_trailer);
                arrayListInfo.add(objItem);
            }

            if (jsonObject.has("seasons")) {

                JSONArray c =  jsonObject.getJSONArray("seasons");

                for (int i = 0; i < c.length(); i++) {
                    JSONObject objectCategory = c.getJSONObject(i);

                    String name = objectCategory.getString("name");
                    String season_number = objectCategory.getString("season_number");

                    ItemSeasons objItem = new ItemSeasons(name,season_number);
                    arrayListSeries.add(objItem);
                }
            }

            if (jsonObject.has("episodes")) {

                JSONObject cx =  jsonObject.getJSONObject("episodes");

                for (int h = 0; h < 20; h++) {

                    if (cx.has(String.valueOf(h))) {

                        JSONArray cm = cx.getJSONArray(String.valueOf(h));
                        for (int i = 0; i < cm.length(); i++) {
                            JSONObject object = cm.getJSONObject(i);

                            String id = object.getString("id");
                            String title = object.getString("title");
                            String container_extension = object.getString("container_extension");
                            String season = object.getString("season");

                            // info
                            JSONObject object2 = object.getJSONObject("info");

                            String plot="";
                            if (object2.has("plot")){
                                plot = object2.getString("plot");
                            }
                            String duration="0";
                            if (object2.has("duration")){
                                duration = object2.getString("duration");
                            }
                            String movie_image="";
                            if (object2.has("movie_image")){
                                movie_image = object2.getString("movie_image");
                            }
                            String rating="0";
                            if (object2.has("rating")){
                                rating = object2.getString("rating");
                            }

                            ItemEpisodes episodes = new ItemEpisodes(id, title, container_extension, season, plot, duration, rating, movie_image);
                            arrayListEpisodes.add(episodes);
                        }
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
        listener.onEnd(s, arrayListInfo, arrayListSeries, arrayListEpisodes);
        super.onPostExecute(s);
    }
}