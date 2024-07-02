package mycinevo.streambox.util.helper;

import android.annotation.SuppressLint;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.net.Uri;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import mycinevo.streambox.item.ItemDns;
import mycinevo.streambox.item.ItemLive;
import mycinevo.streambox.item.ItemMovies;
import mycinevo.streambox.item.ItemSeries;
import mycinevo.streambox.item.ItemSingleURL;
import mycinevo.streambox.item.ItemUsersDB;
import mycinevo.streambox.item.ItemVideoDownload;
import mycinevo.streambox.util.Encrypter.EncryptData;

public class DBHelper extends SQLiteOpenHelper {

    static String DB_NAME = "streambox.db";

    SQLiteDatabase db;
    final Context context;
    private final EncryptData encryptData;

    private static final String TAG_ID = "id";

    // Table Name ----------------------------------------------------------------------------------
    private static final String TABLE_USERS = "users";
    private static final String TABLE_MOVIE_SEEK = "movie_seek";
    private static final String TABLE_SINGLE = "single";
    // DNS
    public static final String TABLE_DNS_XUI = "tbl_dns_xui";
    public static final String TABLE_DNS_STREAM = "tbl_dns_stream";
    // Fav
    public static final String TABLE_FAV_LIVE = "fav_live";
    public static final String TABLE_FAV_MOVIE = "fav_movie";
    public static final String TABLE_FAV_SERIES = "fav_series";
    // RECENT
    public static final String TABLE_RECENT_LIVE = "recent_live";
    public static final String TABLE_RECENT_MOVIE = "recent_movie";
    public static final String TABLE_RECENT_SERIES = "recent_series";
    // DOWNLOAD
    public static final String TABLE_DOWNLOAD_MOVIES = "download_movie";

    // TAG -----------------------------------------------------------------------------------------
    private static final String TAG_DNS_TITLE = "dns_title", TAG_DNS_BASE = "dns_base";
    private static final String TAG_SINGLE_ANY_NAME = "any_name", TAG_SINGLE_URL = "single_url";
    private static final String TAG_USERS_ANY_NAME = "any_name", TAG_USERS_NAME = "user_name", TAG_USERS_PASSWORD = "user_pass",
            TAG_USERS_URL = "user_url", TAG_USERS_TYPE = "user_type";
    private static final String TAG_MOVIE_STREAM_ID = "stream_id", TAG_MOVIE_TITLE = "title", TAG_MOVIE_SEEK = "seek";
    // FAV AND RECENT ------------------------------------------------------------------------------
    private static final String TAG_LIVE_NAME = "name", TAG_LIVE_ID = "stream_id", TAG_LIVE_ICON = "stream_icon";
    private static final String TAG_MOVIE_NAME = "name", TAG_MOVIE_ID = "stream_id", TAG_MOVIE_ICON = "stream_icon", TAG_MOVIE_RATING = "rating";
    private static final String TAG_SERIES_NAME = "name", TAG_SERIES_ID = "series_id", TAG_SERIES_COVER = "cover", TAG_SERIES_RATING = "rating";
    // DOWNLOAD ------------------------------------------------------------------------------------
    private static final String TAG_DOWNLOAD_NAME = "name", TAG_DOWNLOAD_ID = "stream_id", TAG_DOWNLOAD_ICON = "stream_icon",
            TAG_DOWNLOAD_URL = "video_url", TAG_DOWNLOAD_CONTAINER = "container", TAG_DOWNLOAD_TEMP_NAME = "temp_name";

    // Columns -------------------------------------------------------------------------------------
    private final String[] columns_live = new String[]{TAG_ID, TAG_LIVE_NAME, TAG_LIVE_ID, TAG_LIVE_ICON};
    private final String[] columns_movie = new String[]{TAG_ID, TAG_MOVIE_NAME, TAG_MOVIE_ID, TAG_MOVIE_ICON, TAG_MOVIE_RATING};
    private final String[] columns_series = new String[]{TAG_ID, TAG_SERIES_NAME, TAG_SERIES_ID, TAG_SERIES_COVER, TAG_SERIES_RATING};
    private final String[] columns_single = new String[]{TAG_ID, TAG_SINGLE_ANY_NAME, TAG_SINGLE_URL};
    private final String[] columns_movie_seek = new String[]{TAG_ID, TAG_MOVIE_STREAM_ID, TAG_MOVIE_TITLE, TAG_MOVIE_SEEK};
    private final String[] columns_users = new String[]{TAG_ID, TAG_USERS_ANY_NAME, TAG_USERS_NAME, TAG_USERS_PASSWORD, TAG_USERS_URL, TAG_USERS_TYPE};
    private final String[] columns_dns = new String[]{TAG_ID, TAG_DNS_TITLE, TAG_DNS_BASE};
    private final String[] columns_download = new String[]{TAG_ID, TAG_DOWNLOAD_NAME, TAG_DOWNLOAD_ID, TAG_DOWNLOAD_ICON,
            TAG_DOWNLOAD_URL, TAG_DOWNLOAD_CONTAINER, TAG_DOWNLOAD_TEMP_NAME};

    // Creating Table Query DOWNLOAD ---------------------------------------------------------------
    private static final String CREATE_TABLE_DOWNLOAD_MOVIES = "create table " + TABLE_DOWNLOAD_MOVIES + "(" +
            TAG_ID + " integer PRIMARY KEY AUTOINCREMENT," +
            TAG_DOWNLOAD_NAME + " TEXT," +
            TAG_DOWNLOAD_ID + " TEXT," +
            TAG_DOWNLOAD_ICON + " TEXT," +
            TAG_DOWNLOAD_URL + " TEXT," +
            TAG_DOWNLOAD_CONTAINER + " TEXT," +
            TAG_DOWNLOAD_TEMP_NAME + " TEXT);";

    // Creating Table Query FAV --------------------------------------------------------------------
    private static final String CREATE_TABLE_FAV_SERIES = "create table " + TABLE_FAV_SERIES + "(" +
            TAG_ID + " integer PRIMARY KEY AUTOINCREMENT," +
            TAG_SERIES_NAME + " TEXT," +
            TAG_SERIES_ID + " TEXT," +
            TAG_SERIES_COVER + " TEXT," +
            TAG_SERIES_RATING + " TEXT);";
    private static final String CREATE_TABLE_FAV_MOVIE = "create table " + TABLE_FAV_MOVIE + "(" +
            TAG_ID + " integer PRIMARY KEY AUTOINCREMENT," +
            TAG_MOVIE_NAME + " TEXT," +
            TAG_MOVIE_ID + " TEXT," +
            TAG_MOVIE_ICON + " TEXT," +
            TAG_MOVIE_RATING + " TEXT);";
    private static final String CREATE_TABLE_FAV_LIVE = "create table " + TABLE_FAV_LIVE + "(" +
            TAG_ID + " integer PRIMARY KEY AUTOINCREMENT," +
            TAG_LIVE_NAME + " TEXT," +
            TAG_LIVE_ID + " TEXT," +
            TAG_LIVE_ICON + " TEXT);";

    // Creating Table Query RECENT -----------------------------------------------------------------
    private static final String CREATE_TABLE_RECENT_SERIES = "create table " + TABLE_RECENT_SERIES + "(" +
            TAG_ID + " integer PRIMARY KEY AUTOINCREMENT," +
            TAG_SERIES_NAME + " TEXT," +
            TAG_SERIES_ID + " TEXT," +
            TAG_SERIES_COVER + " TEXT," +
            TAG_SERIES_RATING + " TEXT);";
    private static final String CREATE_TABLE_RECENT_MOVIE = "create table " + TABLE_RECENT_MOVIE + "(" +
            TAG_ID + " integer PRIMARY KEY AUTOINCREMENT," +
            TAG_MOVIE_NAME + " TEXT," +
            TAG_MOVIE_ID + " TEXT," +
            TAG_MOVIE_ICON + " TEXT," +
            TAG_MOVIE_RATING + " TEXT);";
    private static final String CREATE_TABLE_RECENT_LIVE = "create table " + TABLE_RECENT_LIVE + "(" +
            TAG_ID + " integer PRIMARY KEY AUTOINCREMENT," +
            TAG_LIVE_NAME + " TEXT," +
            TAG_LIVE_ID + " TEXT," +
            TAG_LIVE_ICON + " TEXT);";

    // Creating Table Query ------------------------------------------------------------------------
    private static final String CREATE_TABLE_DNS_XUI = "create table " + TABLE_DNS_XUI + "(" +
            TAG_ID + " integer PRIMARY KEY AUTOINCREMENT," +
            TAG_DNS_TITLE + " TEXT," +
            TAG_DNS_BASE + " TEXT);";
    private static final String CREATE_TABLE_DNS_STREAM = "create table " + TABLE_DNS_STREAM + "(" +
            TAG_ID + " integer PRIMARY KEY AUTOINCREMENT," +
            TAG_DNS_TITLE + " TEXT," +
            TAG_DNS_BASE + " TEXT);";

    // Creating table query ------------------------------------------------------------------------
    private static final String CREATE_TABLE_SINGLE = "create table " + TABLE_SINGLE + "(" +
            TAG_ID + " integer PRIMARY KEY AUTOINCREMENT," +
            TAG_SINGLE_ANY_NAME + " TEXT," +
            TAG_SINGLE_URL + " TEXT);";

    // Creating table query ------------------------------------------------------------------------
    private static final String CREATE_TABLE_USERS = "create table " + TABLE_USERS + "(" +
            TAG_ID + " integer PRIMARY KEY AUTOINCREMENT," +
            TAG_USERS_ANY_NAME + " TEXT," +
            TAG_USERS_NAME + " TEXT," +
            TAG_USERS_PASSWORD + " TEXT," +
            TAG_USERS_URL + " TEXT," +
            TAG_USERS_TYPE + " TEXT);";

    // Creating table query ------------------------------------------------------------------------
    private static final String CREATE_TABLE_MOVIE_SEEK = "create table " + TABLE_MOVIE_SEEK + "(" +
            TAG_ID + " integer PRIMARY KEY AUTOINCREMENT," +
            TAG_MOVIE_STREAM_ID + " TEXT," +
            TAG_MOVIE_TITLE + " TEXT," +
            TAG_MOVIE_SEEK + " TEXT);";

    public DBHelper(Context context) {
        super(context, DB_NAME, null, 2);
        encryptData = new EncryptData(context);
        this.context = context;
        db = getWritableDatabase();
    }

    @Override
    public void onCreate(SQLiteDatabase db) {
        try {
            // SINGLE URl
            db.execSQL(CREATE_TABLE_SINGLE);

            // LIVE
            db.execSQL(CREATE_TABLE_FAV_LIVE);
            db.execSQL(CREATE_TABLE_RECENT_LIVE);

            // MOVIES
            db.execSQL(CREATE_TABLE_MOVIE_SEEK);
            db.execSQL(CREATE_TABLE_FAV_MOVIE);
            db.execSQL(CREATE_TABLE_RECENT_MOVIE);

            // SERIES
            db.execSQL(CREATE_TABLE_FAV_SERIES);
            db.execSQL(CREATE_TABLE_RECENT_SERIES);

            // DNS
            db.execSQL(CREATE_TABLE_DNS_XUI);
            db.execSQL(CREATE_TABLE_DNS_STREAM);

            // USER
            db.execSQL(CREATE_TABLE_USERS);

            // DOWNLOAD
            db.execSQL(CREATE_TABLE_DOWNLOAD_MOVIES);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // FAV AND RECENT ------------------------------------------------------------------------------
    @SuppressLint("Range")
    public List<ItemLive> getLive(String table, boolean IsOrder) {
        ArrayList<ItemLive> arrayList = new ArrayList<>();
        String order = " DESC";
        if (IsOrder){
            order = " ASC";
        }
        try {
            Cursor cursor = db.query(table, columns_live, null, null, null, null, TAG_ID + order);
            if (cursor != null && cursor.moveToFirst()) {
                for (int i = 0; i < cursor.getCount(); i++) {

                    String streamIcon = encryptData.decrypt(cursor.getString(cursor.getColumnIndex(TAG_LIVE_ICON)));

                    String name = cursor.getString(cursor.getColumnIndex(TAG_LIVE_NAME));
                    String streamID = cursor.getString(cursor.getColumnIndex(TAG_LIVE_ID));

                    ItemLive objItem = new ItemLive(name,streamID,streamIcon,"");
                    arrayList.add(objItem);

                    cursor.moveToNext();
                }
                cursor.close();
            }
            return arrayList;
        } catch (Exception e) {
            return arrayList;
        }
    }

    @SuppressLint("Range")
    public void addToLive(String table, ItemLive itemLive, int limit) {
        try {
            if (table != null){
                if (table.equals(TABLE_RECENT_LIVE)){
                    Cursor cursor_delete = db.query(TABLE_RECENT_LIVE, columns_live, null, null, null, null, null);
                    if (cursor_delete != null && cursor_delete.getCount() > limit) {
                        cursor_delete.moveToFirst();
                        db.delete(TABLE_RECENT_LIVE, TAG_ID + "=" + cursor_delete.getString(cursor_delete.getColumnIndex(TAG_ID)), null);
                    }
                    if (cursor_delete != null) {
                        cursor_delete.close();
                    }

                    if (Boolean.TRUE.equals(checkLive(TABLE_RECENT_LIVE, itemLive.getStreamID()))) {
                        db.delete(TABLE_RECENT_LIVE, TAG_LIVE_ID + "=" + itemLive.getStreamID(), null);
                    }
                }

                String image = encryptData.encrypt(itemLive.getStreamIcon().replace(" ", "%20"));
                ContentValues contentValues = new ContentValues();
                contentValues.put(TAG_LIVE_NAME, itemLive.getName());
                contentValues.put(TAG_LIVE_ID, itemLive.getStreamID());
                contentValues.put(TAG_LIVE_ICON, image);
                db.insert(table, null, contentValues);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    public void removeLive(String table, String stream_id) {
        try {
            if (Boolean.TRUE.equals(checkLive(table, stream_id))){
                db.delete(table, TAG_LIVE_ID + "=" + stream_id, null);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    public Boolean checkLive(String table, String streamID) {
        try {
            Cursor cursor = db.query(table, columns_live, TAG_LIVE_ID + "=" + streamID, null, null, null, null);
            boolean isFav = cursor != null && cursor.getCount() > 0;
            if (cursor != null) {
                cursor.close();
            }
            return isFav;
        } catch (Exception e) {
            return false;
        }
    }

    @SuppressLint("Range")
    public List<ItemMovies> getMovies(String table, boolean IsOrder) {
        ArrayList<ItemMovies> arrayList = new ArrayList<>();
        String order = " DESC";
        if (IsOrder){
            order = " ASC";
        }
        try {
            Cursor cursor = db.query(table, columns_movie, null, null, null, null, TAG_ID + order);
            if (cursor != null && cursor.moveToFirst()) {
                for (int i = 0; i < cursor.getCount(); i++) {

                    String streamIcon = encryptData.decrypt(cursor.getString(cursor.getColumnIndex(TAG_MOVIE_ICON)));
                    String name = cursor.getString(cursor.getColumnIndex(TAG_MOVIE_NAME));
                    String streamID = cursor.getString(cursor.getColumnIndex(TAG_MOVIE_ID));
                    String rating = cursor.getString(cursor.getColumnIndex(TAG_MOVIE_RATING));

                    ItemMovies objItem = new ItemMovies(name, streamID, streamIcon, rating,"");
                    arrayList.add(objItem);

                    cursor.moveToNext();
                }
                cursor.close();
            }
            return arrayList;
        } catch (Exception e) {
            return arrayList;
        }
    }
    @SuppressLint("Range")
    public void addToMovie(String table, ItemMovies itemMovies, int limit) {
        try {
            if (itemMovies != null && table != null){
                if (table.equals(TABLE_RECENT_MOVIE)){
                    Cursor cursor_delete = db.query(TABLE_RECENT_MOVIE, columns_movie, null, null, null, null, null);
                    if (cursor_delete != null && cursor_delete.getCount() > limit) {
                        cursor_delete.moveToFirst();
                        db.delete(TABLE_RECENT_MOVIE, TAG_ID + "=" + cursor_delete.getString(cursor_delete.getColumnIndex(TAG_ID)), null);
                    }
                    if (cursor_delete != null) {
                        cursor_delete.close();
                    }
                    if (Boolean.TRUE.equals(checkMovie(TABLE_RECENT_MOVIE, itemMovies.getStreamID()))) {
                        db.delete(TABLE_RECENT_MOVIE, TAG_MOVIE_ID + "=" + itemMovies.getStreamID(), null);
                    }
                }
                String image = encryptData.encrypt(itemMovies.getStreamIcon().replace(" ", "%20"));
                ContentValues contentValues = new ContentValues();
                contentValues.put(TAG_MOVIE_NAME, itemMovies.getName());
                contentValues.put(TAG_MOVIE_ID, itemMovies.getStreamID());
                contentValues.put(TAG_MOVIE_ICON, image);
                contentValues.put(TAG_MOVIE_RATING, itemMovies.getRating());
                db.insert(table, null, contentValues);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    public void removeMovie(String table, String stream_id) {
        try {
            if (Boolean.TRUE.equals(checkMovie(table, stream_id))){
                db.delete(table, TAG_MOVIE_ID + "=" + stream_id, null);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    public Boolean checkMovie(String table, String streamID) {
        try {
            Cursor cursor = db.query(table, columns_movie, TAG_MOVIE_ID + "=" + streamID, null, null, null, null);
            boolean isFav = cursor != null && cursor.getCount() > 0;
            if (cursor != null) {
                cursor.close();
            }
            return isFav;
        } catch (Exception e) {
            return false;
        }
    }

    @SuppressLint("Range")
    public List<ItemSeries> getSeries(String table, boolean IsOrder) {
        ArrayList<ItemSeries> arrayList = new ArrayList<>();
        String order = " DESC";
        if (IsOrder){
            order = " ASC";
        }
        try {
            Cursor cursor = db.query(table, columns_series, null, null, null, null, TAG_ID + order);
            if (cursor != null && cursor.moveToFirst()) {
                for (int i = 0; i < cursor.getCount(); i++) {

                    String cover = encryptData.decrypt(cursor.getString(cursor.getColumnIndex(TAG_SERIES_COVER)));
                    String name = cursor.getString(cursor.getColumnIndex(TAG_SERIES_NAME));
                    String seriesID = cursor.getString(cursor.getColumnIndex(TAG_SERIES_ID));
                    String rating = cursor.getString(cursor.getColumnIndex(TAG_SERIES_RATING));

                    ItemSeries objItem = new ItemSeries(name, seriesID, cover, rating);
                    arrayList.add(objItem);

                    cursor.moveToNext();
                }
                cursor.close();
            }
            return arrayList;
        } catch (Exception e) {
            return arrayList;
        }
    }
    @SuppressLint("Range")
    public void addToSeries(String table, ItemSeries itemSeries, int limit) {
        try {
            if (itemSeries != null && table != null){
                if (table.equals(TABLE_RECENT_SERIES)){
                    Cursor cursor_delete = db.query(TABLE_RECENT_SERIES, columns_series, null, null, null, null, null);
                    if (cursor_delete != null && cursor_delete.getCount() > limit) {
                        cursor_delete.moveToFirst();
                        db.delete(TABLE_RECENT_SERIES, TAG_ID + "=" + cursor_delete.getString(cursor_delete.getColumnIndex(TAG_ID)), null);
                    }
                    if (cursor_delete != null) {
                        cursor_delete.close();
                    }
                    if (Boolean.TRUE.equals(checkSeries(TABLE_RECENT_SERIES, itemSeries.getSeriesID()))) {
                        db.delete(TABLE_RECENT_SERIES, TAG_SERIES_ID + "=" + itemSeries.getSeriesID(), null);
                    }
                }
                String image = encryptData.encrypt(itemSeries.getCover().replace(" ", "%20"));
                ContentValues contentValues = new ContentValues();
                contentValues.put(TAG_SERIES_NAME, itemSeries.getName());
                contentValues.put(TAG_SERIES_ID, itemSeries.getSeriesID());
                contentValues.put(TAG_SERIES_COVER, image);
                contentValues.put(TAG_SERIES_RATING, itemSeries.getRating());
                db.insert(table, null, contentValues);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    public void removeFavSeries(String table, String seriesID) {
        try {
            if (Boolean.TRUE.equals(checkSeries(table,seriesID))){
                db.delete(table, TAG_SERIES_ID + "=" + seriesID, null);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    public Boolean checkSeries(String table, String seriesID) {
        try {
            Cursor cursor = db.query(table, columns_series, TAG_SERIES_ID + "=" + seriesID, null, null, null, null);
            boolean isFav = cursor != null && cursor.getCount() > 0;
            if (cursor != null) {
                cursor.close();
            }
            return isFav;
        } catch (Exception e) {
            return false;
        }
    }

    // DNS -----------------------------------------------------------------------------------------
    @SuppressLint("Range")
    public List<ItemDns> loadDNS(String table) {
        ArrayList<ItemDns> arrayList = new ArrayList<>();
        try {
            Cursor cursor = db.query(table, columns_dns, null, null, null, null, TAG_ID + " ASC");
            if (cursor != null && cursor.moveToFirst()) {
                for (int i = 0; i < cursor.getCount(); i++) {

                    String name = encryptData.decrypt(cursor.getString(cursor.getColumnIndex(TAG_DNS_TITLE)));
                    String url = encryptData.decrypt(cursor.getString(cursor.getColumnIndex(TAG_DNS_BASE)));

                    ItemDns objItem = new ItemDns(name, url);
                    arrayList.add(objItem);

                    cursor.moveToNext();
                }
                cursor.close();
            }
            return arrayList;
        } catch (Exception e) {
            return arrayList;
        }
    }
    public void addToDNS(String table, ItemDns itemDns) {
        try {
            if (itemDns != null) {
                String name = encryptData.encrypt(itemDns.getTitle());
                String url = encryptData.encrypt(itemDns.getBase().replace(" ", "%20"));
                ContentValues contentValues = new ContentValues();
                contentValues.put(TAG_DNS_TITLE , name);
                contentValues.put(TAG_DNS_BASE, url);
                db.insert(table, null, contentValues);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    public void removeAllDNS(String table) {
        try {
            db.delete(table, null, null);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // Single --------------------------------------------------------------------------------------
    @SuppressLint("Range")
    public List<ItemSingleURL> loadSingleURL() {
        ArrayList<ItemSingleURL> arrayList = new ArrayList<>();
        try {
            Cursor cursor = db.query(TABLE_SINGLE, columns_single, null, null, null, null, TAG_ID + " ASC");
            if (cursor != null && cursor.moveToFirst()) {
                for (int i = 0; i < cursor.getCount(); i++) {

                    String id = cursor.getString(cursor.getColumnIndex(TAG_ID));
                    String any_name = encryptData.decrypt(cursor.getString(cursor.getColumnIndex(TAG_SINGLE_ANY_NAME)));
                    String url = encryptData.decrypt(cursor.getString(cursor.getColumnIndex(TAG_SINGLE_URL)));

                    ItemSingleURL objItem = new ItemSingleURL(id, any_name, url);
                    arrayList.add(objItem);

                    cursor.moveToNext();
                }
                cursor.close();
            }
            return arrayList;
        } catch (Exception e) {
            return arrayList;
        }
    }
    public void addToSingleURL(ItemSingleURL itemSingle) {
        try {
            if (itemSingle != null) {
                String any_name = encryptData.encrypt(itemSingle.getAnyName());
                String url = encryptData.encrypt(itemSingle.getSingleURL().replace(" ", "%20"));
                ContentValues contentValues = new ContentValues();
                contentValues.put(TAG_SINGLE_ANY_NAME , any_name);
                contentValues.put(TAG_SINGLE_URL, url);
                db.insert(TABLE_SINGLE, null, contentValues);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    public void removeFromSingleURL(String SingleID) {
        try {
            db.delete(TABLE_SINGLE, TAG_ID + "=" + SingleID, null);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // Users ---------------------------------------------------------------------------------------
    @SuppressLint("Range")
    public List<ItemUsersDB> loadUsersDB() {
        ArrayList<ItemUsersDB> arrayList = new ArrayList<>();
        try {
            Cursor cursor = db.query(TABLE_USERS, columns_users, null, null, null, null, TAG_ID + " ASC");
            if (cursor != null && cursor.moveToFirst()) {
                for (int i = 0; i < cursor.getCount(); i++) {

                    String id = cursor.getString(cursor.getColumnIndex(TAG_ID));
                    String any_name = encryptData.decrypt(cursor.getString(cursor.getColumnIndex(TAG_USERS_ANY_NAME)));
                    String user_name = encryptData.decrypt(cursor.getString(cursor.getColumnIndex(TAG_USERS_NAME)));
                    String user_pass = encryptData.decrypt(cursor.getString(cursor.getColumnIndex(TAG_USERS_PASSWORD)));
                    String user_url = encryptData.decrypt(cursor.getString(cursor.getColumnIndex(TAG_USERS_URL)));
                    String user_type = cursor.getString(cursor.getColumnIndex(TAG_USERS_TYPE));

                    ItemUsersDB objItem = new ItemUsersDB(id, any_name, user_name, user_pass,user_url, user_type);
                    arrayList.add(objItem);

                    cursor.moveToNext();
                }
                cursor.close();
            }
            return arrayList;
        } catch (Exception e) {
            return arrayList;
        }
    }
    public void addToUserDB(ItemUsersDB itemUsersDB) {
        try {
            String any_name = encryptData.encrypt(itemUsersDB.getAnyName());
            String user_name = encryptData.encrypt(itemUsersDB.getUseName());
            String user_pass = encryptData.encrypt(itemUsersDB.getUserPass());
            String user_url = encryptData.encrypt(itemUsersDB.getUserURL().replace(" ", "%20"));

            ContentValues contentValues = new ContentValues();
            contentValues.put(TAG_USERS_ANY_NAME , any_name);
            contentValues.put(TAG_USERS_NAME, user_name);
            contentValues.put(TAG_USERS_PASSWORD, user_pass);
            contentValues.put(TAG_USERS_URL, user_url);
            contentValues.put(TAG_USERS_TYPE, itemUsersDB.getUserType());

            db.insert(TABLE_USERS, null, contentValues);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    public void removeFromUser(String userID) {
        try {
            db.delete(TABLE_USERS, TAG_ID + "=" + userID, null);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // Seek Movie ----------------------------------------------------------------------------------
    @SuppressLint("Range")
    public int getSeekMovie(String streamId, String stream_name) {
        String seekTo = "0";
        try {
            String where = TAG_MOVIE_STREAM_ID + "=? AND " + TAG_MOVIE_TITLE + "=?";
            String[] args = new String[]{streamId, stream_name.replace("'", "%27")};
            Cursor cursor = db.query(TABLE_MOVIE_SEEK, columns_movie_seek, where, args, null, null,  null,null);
            if (cursor != null && cursor.getCount() > 0) {
                cursor.moveToFirst();
                if (!cursor.getString(cursor.getColumnIndex(TAG_MOVIE_SEEK)).isEmpty()){
                    seekTo = cursor.getString(cursor.getColumnIndex(TAG_MOVIE_SEEK));
                }
                cursor.close();
            }
            return Integer.parseInt(seekTo);
        } catch (Exception e) {
            return Integer.parseInt("0");
        }
    }
    public void addToSeekMovie(String currentPosition, String streamId, String stream_name) {
        try {
            String where = TAG_MOVIE_STREAM_ID + "=? AND " + TAG_MOVIE_TITLE + "=?";
            String[] args = new String[]{streamId, stream_name.replace("'", "%27")};

            if (Boolean.TRUE.equals(checkSeekMovie(streamId, stream_name))) {
                db.delete(TABLE_MOVIE_SEEK, where, args);
            }

            ContentValues contentValues = new ContentValues();
            contentValues.put(TAG_MOVIE_STREAM_ID, streamId);
            contentValues.put(TAG_MOVIE_TITLE, stream_name);
            contentValues.put(TAG_MOVIE_SEEK, currentPosition);
            db.insert(TABLE_MOVIE_SEEK, null, contentValues);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    public Boolean checkSeekMovie(String streamId, String stream_name) {
        boolean isSeekMovie = false;
        try {
            String where = TAG_MOVIE_STREAM_ID + "=? AND " + TAG_MOVIE_TITLE + "=?";
            String[] args = new String[]{streamId, stream_name.replace("'", "%27")};
            Cursor cursor = db.query(TABLE_MOVIE_SEEK, columns_movie_seek, where, args, null, null, null);
            isSeekMovie = cursor != null && cursor.getCount() > 0;
            if (cursor != null) {
                cursor.close();
            }
            return isSeekMovie;
        } catch (Exception e) {
            return isSeekMovie;
        }
    }
    public void removeSeekMovieID(String streamId, String stream_name) {
        try {
            if (Boolean.TRUE.equals(checkSeekMovie(streamId, stream_name))) {
                String where = TAG_MOVIE_STREAM_ID + "=? AND " + TAG_MOVIE_TITLE + "=?";
                String[] args = new String[]{streamId, stream_name.replace("'", "%27")};
                db.delete(TABLE_MOVIE_SEEK, where, args);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // Download ------------------------------------------------------------------------------------
    @SuppressLint("Range")
    public List<ItemVideoDownload> loadDataDownload(String table) {
        ArrayList<ItemVideoDownload> arrayList = new ArrayList<>();
        try {
            Cursor cursor = db.query(table, columns_download, null, null, null, null, "");
            if (cursor != null && cursor.getCount() > 0) {
                cursor.moveToFirst();
                for (int i = 0; i < cursor.getCount(); i++) {

                    String id = cursor.getString(cursor.getColumnIndex(TAG_DOWNLOAD_ID));
                    String name = cursor.getString(cursor.getColumnIndex(TAG_DOWNLOAD_NAME)).replace("%27", "'");
                    String imageBig = Uri.fromFile(new File(encryptData.decrypt(cursor.getString(cursor.getColumnIndex(TAG_DOWNLOAD_ICON))))).toString();
                    String container = cursor.getString(cursor.getColumnIndex(TAG_DOWNLOAD_CONTAINER));
                    String tempName = cursor.getString(cursor.getColumnIndex(TAG_DOWNLOAD_TEMP_NAME));
                    String url = context.getExternalFilesDir("").getAbsolutePath() + File.separator + "temp/" + tempName;

                    ItemVideoDownload objItem = new ItemVideoDownload(name ,id ,imageBig ,url ,container);
                    objItem.setTempName(tempName);
                    arrayList.add(objItem);

                    cursor.moveToNext();
                }
            }
            if (cursor != null) {
                cursor.close();
            }
            return arrayList;
        } catch (Exception e) {
            return arrayList;
        }
    }
    public void addToDownloads(String table, ItemVideoDownload itemDownload) {
        try {
            if (itemDownload != null){
                String name = itemDownload.getName().replace("'", "%27");
                String imageBig = encryptData.encrypt(itemDownload.getStreamIcon());
                String url = encryptData.encrypt(itemDownload.getVideoURL());

                ContentValues contentValues = new ContentValues();
                contentValues.put(TAG_DOWNLOAD_ID, itemDownload.getStreamID());
                contentValues.put(TAG_DOWNLOAD_NAME, name);
                contentValues.put(TAG_DOWNLOAD_ICON, imageBig);
                contentValues.put(TAG_DOWNLOAD_URL, url);
                contentValues.put(TAG_DOWNLOAD_CONTAINER, itemDownload.getContainerExtension());
                contentValues.put(TAG_DOWNLOAD_TEMP_NAME, itemDownload.getTempName());

                db.insert(table, null, contentValues);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    public Boolean checkDownload(String table, String id, String container) {
        boolean isDownloaded;
        try {
            File root = new File(context.getExternalFilesDir("").getAbsolutePath() + "/temp");
            Cursor cursor = db.query(table, columns_download, TAG_DOWNLOAD_ID + "=" + id, null, null, null, null);
            if (cursor != null && cursor.getCount() > 0) {
                cursor.moveToFirst();
                @SuppressLint("Range") String filename = cursor.getString(cursor.getColumnIndex(TAG_DOWNLOAD_TEMP_NAME));
                File file = new File(root, filename + container);
                isDownloaded = file.exists();
                cursor.close();
            } else {
                isDownloaded = false;
            }
        } catch (Exception e) {
            isDownloaded = false;
        }
        return isDownloaded;
    }
    public void removeFromDownload(String table, String streamID) {
        try {
            db.delete(table, TAG_DOWNLOAD_ID + "=" + streamID, null);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // Remove All Data -----------------------------------------------------------------------------
    public void removeAllData() {
        try {
            db.delete(TABLE_MOVIE_SEEK, null, null);

            db.delete(TABLE_FAV_LIVE, null, null);
            db.delete(TABLE_FAV_MOVIE, null, null);
            db.delete(TABLE_FAV_SERIES, null, null);

            db.delete(TABLE_RECENT_LIVE, null, null);
            db.delete(TABLE_RECENT_MOVIE, null, null);
            db.delete(TABLE_RECENT_SERIES, null, null);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // Upgrade -------------------------------------------------------------------------------------
    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        // document why this method is empty
    }

    @Override
    public synchronized void close () {
        if (db != null) {
            db.close();
            super.close();
        }
    }
}