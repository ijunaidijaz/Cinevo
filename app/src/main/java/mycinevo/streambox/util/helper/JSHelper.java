package mycinevo.streambox.util.helper;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.SharedPreferences;

import androidx.annotation.NonNull;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import org.json.JSONArray;
import org.json.JSONObject;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import mycinevo.streambox.item.ItemCat;
import mycinevo.streambox.item.ItemLive;
import mycinevo.streambox.item.ItemMovies;
import mycinevo.streambox.item.ItemPlaylist;
import mycinevo.streambox.item.ItemSeries;
import mycinevo.streambox.util.ApplicationUtil;

public class JSHelper {

    private final SharedPreferences sp;
    private final SharedPreferences.Editor ed;

    private static final String TAG_JSON_LIVE = "json_live";
    private static final String TAG_JSON_MOVIE = "json_movie";
    private static final String TAG_JSON_SERIES = "json_series";
    private static final String TAG_JSON_LIVE_CAT = "json_live_cat";
    private static final String TAG_JSON_MOVIE_CAT = "json_movie_cat";
    private static final String TAG_JSON_SERIES_CAT = "json_series_cat";
    private static final String TAG_ORDER_LIVE = "live_order";
    private static final String TAG_ORDER_MOVIE = "movie_order";
    private static final String TAG_ORDER_SERIES = "series_order";
    private static final String TAG_UPDATE_DATE = "update_date";
    private static final String TAG_JSON_PLAYLIST = "json_playlist";
    private static final String TAG_SIZE_LIVE = "live_size_all";
    private static final String TAG_SIZE_MOVIE = "movie_size_all";
    private static final String TAG_SIZE_SERIES = "series_size_all";

    private static final String TAG_CAT_ID = "category_id";
    private static final String TAG_CAT_NAME = "category_name";
    private static final String TAG_NAME = "name";
    private static final String TAG_STREAM_ID = "stream_id";
    private static final String TAG_STREAM_ICON = "stream_icon";
    private static final String TAG_STREAM_TYPE = "stream_type";
    private static final String TAG_RATING = "rating";
    private static final String TAG_SERIES_ID = "series_id";
    private static final String TAG_COVER = "cover";
    private static final String TAG_EMPTY = "";

    private static final String TAG_GROUP = "group";
    private static final String TAG_GROUP_TITLE = "group-title";

    private static final String TAG_LIVE = "live";
    private static final String TAG_CREATED_LIVE = "created_live";
    private static final String TAG_RADIO_STREAMS = "radio_streams";
    private static final String TAG_LOGO = "logo";
    private static final String TAG_URL = "url";

    public JSHelper(@NonNull Context ctx) {
        sp = ctx.getSharedPreferences("json_streambox", Context.MODE_PRIVATE);
        ed = sp.edit();
    }

    // Live ----------------------------------------------------------------------------------------
    public List<ItemLive> getLive(String catId, Boolean isRadio) {
        ArrayList<ItemLive> arrayList = new ArrayList<>();
        try {
            String json = sp.getString(TAG_JSON_LIVE, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                String categoryID = jsonobject.getString(TAG_CAT_ID);
                String name = jsonobject.getString(TAG_NAME);
                String streamID = jsonobject.getString(TAG_STREAM_ID);
                String streamIcon = jsonobject.getString(TAG_STREAM_ICON);
                String streamType = jsonobject.getString(TAG_STREAM_TYPE);
                ItemLive objItem = new ItemLive(name, streamID, streamIcon, TAG_EMPTY);
                if (!catId.isEmpty()){
                    if (categoryID.equals(catId) && streamType.equals(TAG_LIVE) || streamType.equals(TAG_CREATED_LIVE)){
                        arrayList.add(objItem);
                    }
                } else {
                    if (Boolean.TRUE.equals(isRadio) && (streamType.equals(TAG_RADIO_STREAMS))){
                        arrayList.add(objItem);
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return arrayList;
    }

    public List<ItemLive> getLiveRe() {
        ArrayList<ItemLive> arrayList = new ArrayList<>();
        try {
            String json = sp.getString(TAG_JSON_LIVE, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                if (!ApplicationUtil.geIsAdultsCount(jsonobject.getString(TAG_NAME))){
                    String name = jsonobject.getString(TAG_NAME);
                    String streamID = jsonobject.getString(TAG_STREAM_ID);
                    String streamIcon = jsonobject.getString(TAG_STREAM_ICON);
                    String streamType = jsonobject.getString(TAG_STREAM_TYPE);
                    ItemLive objItem = new ItemLive(name,streamID,streamIcon,TAG_EMPTY);
                    if (streamType.equals(TAG_LIVE) || streamType.equals(TAG_CREATED_LIVE)){
                        arrayList.add(objItem);
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return arrayList;
    }

    public List<ItemLive> getLivesSearch(String searchText) {
        ArrayList<ItemLive> arrayList = new ArrayList<>();
        try {
            if (searchText.isEmpty() || searchText.equals(" ") || searchText.length() == 1){
                return arrayList;
            }
            String json = sp.getString(TAG_JSON_LIVE, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                String name = jsonobject.getString(TAG_NAME);
                String streamID = jsonobject.getString(TAG_STREAM_ID);
                String streamIcon = jsonobject.getString(TAG_STREAM_ICON);
                String streamType = jsonobject.getString(TAG_STREAM_TYPE);
                ItemLive objItem = new ItemLive(name,streamID,streamIcon,TAG_EMPTY);
                if (name.toLowerCase().contains(searchText.toLowerCase()) || name.toUpperCase().contains(searchText.toUpperCase()) && streamType.equals(TAG_LIVE)){
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return arrayList;
    }
    public void addToLiveData(String json) {
        if (json != null) {
            ed.putString(TAG_JSON_LIVE, json);
            ed.apply();
        }
    }

    public List<ItemCat> getCategoryLive() {
        ArrayList<ItemCat> arrayList = new ArrayList<>();
        try {
            String json = sp.getString(TAG_JSON_LIVE_CAT, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray arrayCategory = new JSONArray(json);
            for (int i = 0; i < arrayCategory.length(); i++) {
                JSONObject objectCategory = arrayCategory.getJSONObject(i);
                String id = objectCategory.getString(TAG_CAT_ID);
                String name = objectCategory.getString(TAG_CAT_NAME);
                ItemCat objItem = new ItemCat(id, name, TAG_EMPTY);
                arrayList.add(objItem);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return arrayList;
    }

    public void addToCatLiveList(String json) {
        if (json != null) {
            ed.putString(TAG_JSON_LIVE_CAT, json);
            ed.apply();
        }
    }

    // Movies --------------------------------------------------------------------------------------
    public List<ItemMovies> getMovies(String catId) {
        ArrayList<ItemMovies> arrayList = new ArrayList<>();
        try {
            String json = sp.getString(TAG_JSON_MOVIE, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);

                String categoryId= jsonobject.getString(TAG_CAT_ID);
                String name = jsonobject.getString(TAG_NAME);
                String streamId = jsonobject.getString(TAG_STREAM_ID);
                String streamIcon = jsonobject.getString(TAG_STREAM_ICON);
                String rating = jsonobject.getString(TAG_RATING);
                ItemMovies objItem = new ItemMovies(name, streamId, streamIcon, rating, TAG_EMPTY);

                if (!catId.isEmpty()){
                    if (categoryId.equals(catId)){
                        arrayList.add(objItem);
                    }
                } else {
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return arrayList;
    }

    public List<ItemMovies> getMoviesRe() {
        ArrayList<ItemMovies> arrayList = new ArrayList<>();
        try {
            String json = sp.getString(TAG_JSON_MOVIE, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                if (!ApplicationUtil.geIsAdultsCount(jsonobject.getString(TAG_NAME))){

                    String name = jsonobject.getString(TAG_NAME);
                    String streamId = jsonobject.getString(TAG_STREAM_ID);
                    String streamIcon = jsonobject.getString(TAG_STREAM_ICON);
                    String rating = jsonobject.getString(TAG_RATING);

                    ItemMovies objItem = new ItemMovies(name, streamId, streamIcon, rating, TAG_EMPTY);
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return arrayList;
    }

    public List<ItemMovies> getMoviesSearch(String searchText) {
        ArrayList<ItemMovies> arrayList = new ArrayList<>();
        try {
            if (searchText.isEmpty() || searchText.equals(" ") || searchText.length() == 1){
                return arrayList;
            }
            String json = sp.getString(TAG_JSON_MOVIE, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                if (jsonobject.getString(TAG_NAME).toLowerCase().contains(searchText.toLowerCase()) || jsonobject.getString(TAG_NAME).toUpperCase().contains(searchText.toUpperCase())){
                    String name = jsonobject.getString(TAG_NAME);
                    String streamId = jsonobject.getString(TAG_STREAM_ID);
                    String streamIcon = jsonobject.getString(TAG_STREAM_ICON);
                    String rating = jsonobject.getString(TAG_RATING);

                    ItemMovies objItem = new ItemMovies(name, streamId, streamIcon, rating, TAG_EMPTY);
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return arrayList;
    }

    public void addToMovieData(String json) {
        if (json != null) {
            ed.putString(TAG_JSON_MOVIE, json);
            ed.apply();
        }
    }

    public List<ItemCat> getCategoryMovie() {
        ArrayList<ItemCat> arrayList = new ArrayList<>();
        try {
            String json = sp.getString(TAG_JSON_MOVIE_CAT, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray arrayCategory = new JSONArray(json);
            for (int i = 0; i < arrayCategory.length(); i++) {
                JSONObject objectCategory = arrayCategory.getJSONObject(i);

                String id = objectCategory.getString(TAG_CAT_ID);
                String name = objectCategory.getString(TAG_CAT_NAME);

                ItemCat objItem = new ItemCat(id, name,"");
                arrayList.add(objItem);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return arrayList;
    }

    public void addToMovieCatData(String json) {
        if (json != null) {
            ed.putString(TAG_JSON_MOVIE_CAT, json);
            ed.apply();
        }
    }

    // Series --------------------------------------------------------------------------------------
    public List<ItemSeries> getSeries(String catId) {
        ArrayList<ItemSeries> arrayList = new ArrayList<>();
        try {
            String json = sp.getString(TAG_JSON_SERIES, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);

                String categoryId= jsonobject.getString(TAG_CAT_ID);
                String name= jsonobject.getString(TAG_NAME);

                String seriesId= jsonobject.getString(TAG_SERIES_ID);
                String cover= jsonobject.getString(TAG_COVER);
                String rating= jsonobject.getString(TAG_RATING);
                ItemSeries objItem = new ItemSeries(name, seriesId, cover, rating);

                if (!catId.isEmpty()){
                    if (categoryId.equals(catId)){
                        arrayList.add(objItem);
                    }
                } else {
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return arrayList;
    }

    public List<ItemSeries> getSeriesRe() {
        ArrayList<ItemSeries> arrayList = new ArrayList<>();
        try {
            String json = sp.getString(TAG_JSON_SERIES, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                if (!ApplicationUtil.geIsAdultsCount(jsonobject.getString(TAG_NAME))){
                    String name = jsonobject.getString(TAG_NAME);
                    String seriesId= jsonobject.getString(TAG_SERIES_ID);
                    String cover= jsonobject.getString(TAG_COVER);
                    String rating= jsonobject.getString(TAG_RATING);

                    ItemSeries objItem = new ItemSeries(name, seriesId, cover, rating);
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return arrayList;
    }

    public List<ItemSeries> getSeriesSearch(String searchText) {
        ArrayList<ItemSeries> arrayList = new ArrayList<>();
        try {
            if (searchText.isEmpty() || searchText.equals(" ") || searchText.length() == 1){
                return arrayList;
            }
            String json = sp.getString(TAG_JSON_SERIES, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                if (jsonobject.getString(TAG_NAME).toLowerCase().contains(searchText.toLowerCase()) || jsonobject.getString(TAG_NAME).toUpperCase().contains(searchText.toUpperCase())){
                    String name = jsonobject.getString(TAG_NAME);
                    String seriesId = jsonobject.getString(TAG_SERIES_ID);
                    String cover = jsonobject.getString(TAG_COVER);
                    String rating = jsonobject.getString(TAG_RATING);

                    ItemSeries objItem = new ItemSeries(name, seriesId, cover, rating);
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return arrayList;
    }

    public void addToSeriesData(String json) {
        if (json != null) {
            ed.putString(TAG_JSON_SERIES, json);
            ed.apply();
        }
    }

    public List<ItemCat> getCategorySeries() {
        ArrayList<ItemCat> arrayList = new ArrayList<>();
        try {
            String json = sp.getString(TAG_JSON_SERIES_CAT, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray arrayCategory = new JSONArray(json);
            for (int i = 0; i < arrayCategory.length(); i++) {
                JSONObject objectCategory = arrayCategory.getJSONObject(i);

                String id = objectCategory.getString(TAG_CAT_ID);
                String name = objectCategory.getString(TAG_CAT_NAME);

                ItemCat objItem = new ItemCat(id, name, TAG_EMPTY);
                arrayList.add(objItem);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return arrayList;
    }

    public void addToSeriesCatData(String json) {
        if (json != null) {
            ed.putString(TAG_JSON_SERIES_CAT, json);
            ed.apply();
        }
    }

    // Playlist ------------------------------------------------------------------------------------
    public void addToPlaylistData(List<ItemPlaylist> arrayListPlaylist) {
        if (arrayListPlaylist == null) {
            return;
        }
        Gson gson = new GsonBuilder().setDateFormat("yyyy-MM-dd' 'HH:mm:ss").create();
        String jsonString = gson.toJson(arrayListPlaylist);
        ed.putString(TAG_JSON_PLAYLIST, jsonString);
        ed.apply();
    }

    public List<ItemCat> getCategoryPlaylist(int pageType) {
        ArrayList<ItemCat> arrayList = new ArrayList<>();
        try {
            String json = sp.getString(TAG_JSON_PLAYLIST, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray arrayCategory = new JSONArray(json);
            for (int i = 0; i < arrayCategory.length(); i++) {
                JSONObject objectCategory = arrayCategory.getJSONObject(i);

                String group = "";
                if (objectCategory.has(TAG_GROUP)){
                    group = objectCategory.getString(TAG_GROUP);
                } else if (objectCategory.has(TAG_GROUP_TITLE)){
                    group = objectCategory.getString(TAG_GROUP_TITLE);
                }

                String url = objectCategory.getString(TAG_URL);
                if (pageType == 4){
                    if (url.equalsIgnoreCase(".ts") || url.contains("/ts") || url.contains(".m3u8") || url.contains("/m3u8")){
                        ItemCat objItem = new ItemCat(TAG_EMPTY ,group, TAG_EMPTY);
                        arrayList.add(objItem);
                    }
                } else {
                    if (url.contains(".mp4") || url.contains(".mkv") || url.contains(".avi") || url.contains(".webm") || url.contains(".mov") || url.contains(".flv")){
                        ItemCat objItem = new ItemCat(TAG_EMPTY ,group, TAG_EMPTY);
                        arrayList.add(objItem);
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return arrayList;
    }

    public List<ItemLive> getLivePlaylist() {
        ArrayList<ItemLive> arrayList = new ArrayList<>();
        try {
            String json = sp.getString(TAG_JSON_PLAYLIST, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);

                String url = jsonobject.getString(TAG_URL);
                if (url.equalsIgnoreCase(".ts") || url.contains("/ts") || url.contains(".m3u8") || url.contains("/m3u8")){
                    String name = jsonobject.getString(TAG_NAME);
                    String logo = jsonobject.getString(TAG_LOGO);
                    String group = jsonobject.getString(TAG_GROUP);

                    ItemLive objItem = new ItemLive(name, url, logo, group);
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return arrayList;
    }

    public List<ItemMovies> getMoviesPlaylist() {
        ArrayList<ItemMovies> arrayList = new ArrayList<>();
        try {
            String json = sp.getString(TAG_JSON_PLAYLIST, null);
            if (json == null) {
                return arrayList;
            }
            JSONArray jsonarray = new JSONArray(json);
            for (int i = 0; i < jsonarray.length(); i++) {
                JSONObject jsonobject = jsonarray.getJSONObject(i);
                String url = jsonobject.getString(TAG_URL);
                if (url.contains(".mp4") || url.contains(".mkv") || url.contains(".avi") || url.contains(".webm") || url.contains(".mov") || url.contains(".flv")){
                    String name = jsonobject.getString(TAG_NAME);
                    String logo = jsonobject.getString(TAG_LOGO);
                    String group = jsonobject.getString(TAG_GROUP);

                    ItemMovies objItem = new ItemMovies(name, url, logo, TAG_EMPTY,group);
                    arrayList.add(objItem);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return arrayList;
    }

    // Remove --------------------------------------------------------------------------------------
    public void removeAllPlaylist() {
        ed.putString(TAG_JSON_PLAYLIST, null);
        ed.apply();
    }
    
    public void removeAllData() {
        ed.putInt(TAG_SIZE_LIVE, 0);
        ed.putString(TAG_JSON_LIVE, null);
        ed.putString(TAG_JSON_LIVE_CAT, null);

        ed.putInt(TAG_SIZE_MOVIE, 0);
        ed.putString(TAG_JSON_MOVIE, null);
        ed.putString(TAG_JSON_MOVIE_CAT, null);

        ed.putInt(TAG_SIZE_SERIES, 0);
        ed.putString(TAG_JSON_SERIES, null);
        ed.putString(TAG_JSON_SERIES_CAT, null);

        ed.apply();
    }

    public void removeAllSeries() {
        ed.putInt(TAG_SIZE_SERIES, 0);
        ed.putString(TAG_JSON_SERIES, null);
        ed.putString(TAG_JSON_SERIES_CAT, null);
        ed.apply();
    }

    public void removeAllMovies() {
        ed.putInt(TAG_SIZE_MOVIE, 0);
        ed.putString(TAG_JSON_MOVIE, null);
        ed.putString(TAG_JSON_MOVIE_CAT, null);
        ed.apply();
    }

    public void removeAllLive() {
        ed.putInt(TAG_SIZE_LIVE, 0);
        ed.putString(TAG_JSON_LIVE, null);
        ed.putString(TAG_JSON_LIVE_CAT, null);
        ed.apply();
    }

    // ---------------------------------------------------------------------------------------------
    public Boolean getIsLiveOrder() {
        return sp.getBoolean(TAG_ORDER_LIVE, false);
    }
    public void setIsLiveOrder(Boolean flag) {
        ed.putBoolean(TAG_ORDER_LIVE, flag);
        ed.apply();
    }

    public Boolean getIsMovieOrder() {
        return sp.getBoolean(TAG_ORDER_MOVIE, false);
    }
    public void setIsMovieOrder(Boolean flag) {
        ed.putBoolean(TAG_ORDER_MOVIE, flag);
        ed.apply();
    }

    public Boolean getIsSeriesOrder() {
        return sp.getBoolean(TAG_ORDER_SERIES, false);
    }
    public void setIsSeriesOrder(Boolean flag) {
        ed.putBoolean(TAG_ORDER_SERIES, flag);
        ed.apply();
    }

    //Size------------------------------------------------------------------------------------------
    public int getLiveSize() {
        return sp.getInt(TAG_SIZE_LIVE, 0);
    }
    public void setLiveSize(int size) {
        ed.putInt(TAG_SIZE_LIVE, size);
        ed.apply();
    }

    public int getMoviesSize() {
        return sp.getInt(TAG_SIZE_MOVIE, 0);
    }
    public void setMovieSize(int size) {
        ed.putInt(TAG_SIZE_MOVIE, size);
        ed.apply();
    }

    public int getSeriesSize() {
        return sp.getInt(TAG_SIZE_SERIES, 0);
    }
    public void setSeriesSize(int size) {
        ed.putInt(TAG_SIZE_SERIES, size);
        ed.apply();
    }

    //----------------------------------------------------------------------------------------------
    @SuppressLint("SimpleDateFormat")
    public void setUpdateDate(){
        Calendar calendar = Calendar.getInstance();
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
        String currentDateTime = simpleDateFormat.format(calendar.getTime());
        ed.putString(TAG_UPDATE_DATE, currentDateTime);
        ed.apply();
    }

    public String getUpdateDate() {
        return sp.getString(TAG_UPDATE_DATE, TAG_EMPTY);
    }
}