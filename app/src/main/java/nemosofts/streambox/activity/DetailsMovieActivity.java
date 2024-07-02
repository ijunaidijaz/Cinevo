package nemosofts.streambox.activity;

import static android.Manifest.permission.POST_NOTIFICATIONS;
import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.READ_MEDIA_VIDEO;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.graphics.Bitmap;
import android.graphics.drawable.Drawable;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.view.BlurImage;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;
import com.squareup.picasso.Target;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterCast;
import nemosofts.streambox.asyncTask.LoadMovieID;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.FeedBackDialog;
import nemosofts.streambox.dialog.Toasty;
import nemosofts.streambox.interfaces.MovieIDListener;
import nemosofts.streambox.item.ItemCast;
import nemosofts.streambox.item.ItemInfoMovies;
import nemosofts.streambox.item.ItemMovies;
import nemosofts.streambox.item.ItemMoviesData;
import nemosofts.streambox.item.ItemVideoDownload;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.helper.SPHelper;
import nemosofts.streambox.view.NSoftsProgressDialog;


@UnstableApi
public class DetailsMovieActivity extends AppCompatActivity {

    private int playback = 0;
    private Helper helper;
    private DBHelper dbHelper;
    private SPHelper spHelper;
    private ItemInfoMovies itemMovies;
    private ItemMoviesData itemData;
    private ImageView iv_poster;
    private ImageView iv_star_1, iv_star_2, iv_star_3, iv_star_4, iv_star_5;
    private TextView tv_page_title, tv_directed, tv_release, tv_duration, tv_genre, tv_cast, tv_plot;
    private String stream_id, stream_name, stream_icon, stream_rating;
    private ImageView iv_fav, iv_download, iv_download_close;
    private int theme_bg;
    private final Handler seekHandler = new Handler();
    private ProgressBar pb_download;
    private NSoftsProgressDialog progressDialog;
    private LinearLayout ll_page;
    private FrameLayout shimmer;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (Boolean.TRUE.equals(Callback.isLandscape)){
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        }
        IfSupported.IsRTL(this);
        IfSupported.IsScreenshot(this);
        IfSupported.hideStatusBar(this);

        theme_bg = ApplicationUtil.openThemeBg(this);

        ImageView iv_bg_blur = findViewById(R.id.iv_bg_blur);
        iv_bg_blur.setImageResource(theme_bg);

        ImageView iv_alpha = findViewById(R.id.iv_alpha);
        iv_alpha.setImageResource(theme_bg);

        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        progressDialog = new NSoftsProgressDialog(DetailsMovieActivity.this);

        stream_id = getIntent().getStringExtra("stream_id");
        stream_name = getIntent().getStringExtra("stream_name");
        stream_icon = getIntent().getStringExtra("stream_icon");
        stream_rating = getIntent().getStringExtra("stream_rating");

        helper = new Helper(this);
        dbHelper = new DBHelper(this);
        spHelper = new SPHelper(this);

        ll_page = findViewById(R.id.ll_page);
        shimmer = findViewById(R.id.fl_shimmer);
        iv_poster = findViewById(R.id.iv_poster);
        tv_page_title = findViewById(R.id.tv_page_title);
        iv_fav = findViewById(R.id.iv_fav);
        iv_download = findViewById(R.id.iv_download);
        pb_download = findViewById(R.id.pb_download);
        iv_download_close = findViewById(R.id.iv_download_close);

        tv_directed = findViewById(R.id.tv_directed);
        tv_release = findViewById(R.id.tv_release);
        tv_duration = findViewById(R.id.tv_duration);
        tv_genre = findViewById(R.id.tv_genre);
        tv_cast = findViewById(R.id.tv_cast);
        tv_plot = findViewById(R.id.tv_plot);

        iv_star_1 = findViewById(R.id.iv_star_1);
        iv_star_2 = findViewById(R.id.iv_star_2);
        iv_star_3 = findViewById(R.id.iv_star_3);
        iv_star_4 = findViewById(R.id.iv_star_4);
        iv_star_5 = findViewById(R.id.iv_star_5);

        iv_fav.setOnClickListener(v -> {
            if (Boolean.TRUE.equals(dbHelper.checkMovie(DBHelper.TABLE_FAV_MOVIE, stream_id))){
                dbHelper.removeMovie(DBHelper.TABLE_FAV_MOVIE, stream_id);
                iv_fav.setImageResource(R.drawable.ic_favorite_border);
                Toast.makeText(DetailsMovieActivity.this, getString(R.string.fav_remove_success), Toast.LENGTH_SHORT).show();
            } else {
                ItemMovies itemMovies = new ItemMovies(stream_name, stream_id, stream_icon, stream_rating,"");
                dbHelper.addToMovie(DBHelper.TABLE_FAV_MOVIE, itemMovies, 0);
                iv_fav.setImageResource(R.drawable.ic_favorite);
                Toast.makeText(DetailsMovieActivity.this, getString(R.string.fav_success), Toast.LENGTH_SHORT).show();
            }
        });

        TextView play_movie = findViewById(R.id.tv_play_movie);
        if (Boolean.TRUE.equals(dbHelper.checkSeek(DBHelper.TABLE_SEEK_MOVIE, stream_id, stream_name))){
            play_movie.setText(R.string.resume);
        } else {
            play_movie.setText(R.string.play);
        }

        findViewById(R.id.ll_play_movie).setOnClickListener(v -> play());
        findViewById(R.id.ll_play_trailer).setOnClickListener(v -> {
            if (findViewById(R.id.pb_trailer).getVisibility() == View.GONE){
                if (itemMovies != null && !itemMovies.getYoutubeTrailer().isEmpty()) {
                    String videoId = itemMovies.getYoutubeTrailer();
                    Intent intent = new Intent(DetailsMovieActivity.this, YouTubePlayerActivity.class);
                    intent.putExtra("stream_id", videoId);
                    startActivity(intent);
                }
            }
        });
        findViewById(R.id.iv_feedback).setOnClickListener(v -> {
            if (itemMovies != null){
                new FeedBackDialog(this).showDialog("Movies - "+itemMovies.getName());
            }
        });

        getData();

        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.ll_play_movie).requestFocus();
        }

        iv_download_close.setOnClickListener(v -> {
            try {
                if (DownloadService.arrayListVideo != null && !DownloadService.arrayListVideo.isEmpty()){
                    Intent serviceIntent = new Intent(this, DownloadService.class);
                    serviceIntent.setAction(DownloadService.ACTION_STOP);
                    startService(serviceIntent);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_details_movie;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
    }

    private void getData() {
        if (NetworkUtils.isConnected(this)){
            LoadMovieID loadSeriesID = new LoadMovieID(this, new MovieIDListener() {
                @Override
                public void onStart() {
                    addShimmer();
                }

                @Override
                public void onEnd(String success, ArrayList<ItemInfoMovies> arrayListInfo, ArrayList<ItemMoviesData> arrayListMoviesData) {
                    if (!isFinishing()){
                        if (success.equals("1")) {
                            if (!arrayListInfo.isEmpty()){
                                itemMovies = arrayListInfo.get(0);
                            } else {
                                itemMovies = new ItemInfoMovies("",stream_name,stream_icon,"N/A", "0","","N/A","N/A","N/A","N/A",stream_rating);
                            }
                            if (!arrayListMoviesData.isEmpty()){
                                itemData =  arrayListMoviesData.get(0);
                            }
                            ll_page.setVisibility(View.VISIBLE);
                            removeShimmer();
                            setInfo();
                        }  else {
                            if (playback < 3){
                                playback = playback + 1;
                                Toast.makeText(DetailsMovieActivity.this, "Server Error - "+ String.valueOf(playback)+"/3", Toast.LENGTH_SHORT).show();
                                getData();
                            } else {
                                playback = 1;
                                Toasty.makeText(DetailsMovieActivity.this, getString(R.string.err_server_not_connected), Toasty.ERROR);
                                removeShimmer();
                            }
                        }
                    }

                }
            }, helper.getAPIRequestID("get_vod_info","vod_id" ,stream_id, spHelper.getUserName(), spHelper.getPassword()));
            loadSeriesID.execute();
        } else {
            Toasty.makeText(DetailsMovieActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
    }

    private void removeShimmer() {
        if (Boolean.TRUE.equals(spHelper.getIsShimmeringDetails())){
            shimmer.setVisibility(View.GONE);
            shimmer.removeAllViews();
        } else {
            if (progressDialog.isShowing()){
                progressDialog.dismiss();
            }
        }
    }

    private void addShimmer() {
        if (Boolean.TRUE.equals(spHelper.getIsShimmeringDetails())){
            ll_page.setVisibility(View.GONE);
            shimmer.setVisibility(View.VISIBLE);
            shimmer.removeAllViews();
            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.shimmer_details_movie, null);
            shimmer.addView(myView);
        } else {
            ll_page.setVisibility(View.VISIBLE);
            if (!progressDialog.isShowing()){
                progressDialog.show();
            }
        }
    }

    private void setInfo() {
        if (itemData != null){
            if (Boolean.FALSE.equals(dbHelper.checkDownload(DBHelper.TABLE_DOWNLOAD_MOVIES, stream_id, ApplicationUtil.containerExtension(itemData.getContainerExtension())))) {
                iv_download.setImageResource(R.drawable.iv_downloading);
            } else {
                iv_download.setImageResource(R.drawable.ic_check);
            }
        } else {
            findViewById(R.id.ll_download).setVisibility(View.GONE);
        }
        if (!spHelper.getIsDownload()){
            findViewById(R.id.ll_download).setVisibility(View.GONE);
        } else {
            if (!spHelper.getIsDownloadUser()){
                findViewById(R.id.ll_download).setVisibility(View.GONE);
            }
        }
        seekUpdating();

        Picasso.get()
                .load(itemMovies.getMovieImage().isEmpty() ? "null" : itemMovies.getMovieImage())
                .placeholder(R.drawable.material_design_default)
                .into(iv_poster);

        ApplicationUtil.setRating(itemMovies.getRating(), iv_star_1, iv_star_2, iv_star_3, iv_star_4, iv_star_5);

        iv_fav.setImageResource(Boolean.TRUE.equals(dbHelper.checkMovie(DBHelper.TABLE_FAV_MOVIE, stream_id)) ? R.drawable.ic_favorite : R.drawable.ic_favorite_border);

        tv_page_title.setText(itemMovies.getName());
        tv_directed.setText(itemMovies.getDirector().isEmpty() || itemMovies.getDirector().equals("null") ? "N/A" : itemMovies.getDirector());
        tv_release.setText(itemMovies.getReleaseDate());
        tv_genre.setText(itemMovies.getGenre());
        tv_cast.setText(itemMovies.getCast());
        tv_duration.setText(ApplicationUtil.TimeFormat(itemMovies.getEpisodeRunTime()));
        tv_plot.setText(itemMovies.getPlot());

        findViewById(R.id.ll_play_trailer).setVisibility(itemMovies.getYoutubeTrailer().isEmpty() ? View.GONE : View.VISIBLE);
        findViewById(R.id.ll_download).setOnClickListener(v -> {
            if (Boolean.TRUE.equals(checkPer())) {
                if (itemData != null){
                    if (itemData.IsDownload()){
                        Toast.makeText(this, getResources().getString(R.string.already_download), Toast.LENGTH_SHORT).show();
                    } else {
                        if(Boolean.TRUE.equals(checkPerNotification())) {
                            if (Boolean.FALSE.equals(dbHelper.checkDownload(DBHelper.TABLE_DOWNLOAD_MOVIES, stream_id, ApplicationUtil.containerExtension(itemData.getContainerExtension())))) {
                                try{
                                    itemData.setDownload(true);
                                    String channelUrl = spHelper.getServerURL()+"movie/"+ spHelper.getUserName()+"/"+ spHelper.getPassword()+"/"+stream_id+"."+itemData.getContainerExtension();
                                    ItemVideoDownload download  = new ItemVideoDownload(stream_name, stream_id, stream_icon, channelUrl, ApplicationUtil.containerExtension(itemData.getContainerExtension()));
                                    helper.download(download, DBHelper.TABLE_DOWNLOAD_MOVIES);
                                    new Handler().postDelayed(this::seekUpdating, 0);
                                } catch (Exception e) {
                                    e.printStackTrace();
                                }
                            } else {
                                Toast.makeText(this, getResources().getString(R.string.downloading), Toast.LENGTH_SHORT).show();
                            }
                        }
                    }
                }
            } else {
                checkPer();
            }
        });
        setBlur();

        if (spHelper.getIsCast()){
            if (itemMovies.getTmdbID().isEmpty()){
                findViewById(R.id.tv_top_cast).setVisibility(View.GONE);
                findViewById(R.id.rv_cast).setVisibility(View.GONE);
                findViewById(R.id.pb_cast).setVisibility(View.GONE);
            } else {
                getTmdb(itemMovies.getTmdbID());
            }
        } else {
            findViewById(R.id.tv_top_cast).setVisibility(View.GONE);
            findViewById(R.id.rv_cast).setVisibility(View.GONE);
            findViewById(R.id.pb_cast).setVisibility(View.GONE);
        }

    }

    @SuppressLint("StaticFieldLeak")
    private void getTmdb(String movie_id) {
        final ArrayList<ItemCast> arrayListCast = new ArrayList<>();
        new AsyncTask<String, String, String>(){

            @Override
            protected void onPreExecute() {
                super.onPreExecute();
                findViewById(R.id.tv_top_cast).setVisibility(View.VISIBLE);
                findViewById(R.id.pb_cast).setVisibility(View.VISIBLE);
                findViewById(R.id.rv_cast).setVisibility(View.GONE);
            }

            @Override
            protected String doInBackground(String... strings) {
                try {
                    String json = ApplicationUtil.okhttpGet(movie_id, spHelper.getTmdbKEY());
                    JSONObject jsonObject = new JSONObject(json);

                    JSONArray c =  jsonObject.getJSONArray("cast");
                    for (int i = 0; i < c.length(); i++) {
                        JSONObject objectCategory = c.getJSONObject(i);

                        String id = objectCategory.getString("id");
                        String name = objectCategory.getString("name");
                        String profile = objectCategory.getString("profile_path");

                        ItemCast objItem = new ItemCast(id,name,profile);
                        arrayListCast.add(objItem);
                    }
                    return "1";
                } catch (Exception ee) {
                    ee.printStackTrace();
                    return "0";
                }
            }

            @Override
            protected void onPostExecute(String s) {
                if (!isFinishing()){
                    findViewById(R.id.pb_cast).setVisibility(View.GONE);
                    if (s.equals("1")){
                        setAdapterCast(arrayListCast);
                    }
                }
                super.onPostExecute(s);
            }
        }.execute();
    }

    private void setAdapterCast(ArrayList<ItemCast> arrayListCast) {
        if (arrayListCast != null && !arrayListCast.isEmpty()){
            RecyclerView rv_cast = findViewById(R.id.rv_cast);
            LinearLayoutManager linearLayoutManager = new LinearLayoutManager(this, LinearLayoutManager.HORIZONTAL, false);
            rv_cast.setLayoutManager(linearLayoutManager);
            rv_cast.setItemAnimator(new DefaultItemAnimator());

            AdapterCast adapterCast = new AdapterCast(arrayListCast, (itemCast, position) -> Toast.makeText(DetailsMovieActivity.this, arrayListCast.get(position).getName(), Toast.LENGTH_SHORT).show());
            rv_cast.setAdapter(adapterCast);

            findViewById(R.id.tv_top_cast).setVisibility(View.VISIBLE);
            findViewById(R.id.rv_cast).setVisibility(View.VISIBLE);
        } else {
            findViewById(R.id.tv_top_cast).setVisibility(View.GONE);
        }
    }

    @NonNull
    private Boolean checkPerNotification() {
        if (!ApplicationUtil.isTvBox(this) && (android.os.Build.VERSION.SDK_INT >= 33)) {
            if ((ContextCompat.checkSelfPermission(DetailsMovieActivity.this, POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED)) {
                requestPermissions(new String[]{POST_NOTIFICATIONS}, 103);
                return false;
            } else {
                return true;
            }
        }
        return true;
    }

    private void setBlur() {
        ImageView blur = findViewById(R.id.iv_bg_blur);
        if (itemMovies.getMovieImage() != null && !itemMovies.getMovieImage().isEmpty()){
            try {
                Target target = new Target() {
                    @Override
                    public void onBitmapLoaded(Bitmap bitmap, Picasso.LoadedFrom from) {
                        try {
                            int blur_amount = 80;
                            blur.setImageBitmap(BlurImage.fastBlur(bitmap, 1f, blur_amount));
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }

                    @Override
                    public void onBitmapFailed(Exception e, Drawable errorDrawable) {
                        blur.setImageResource(theme_bg);
                    }
                    @Override
                    public void onPrepareLoad(Drawable placeHolderDrawable) {

                    }
                };
                blur.setTag(target);
                Picasso.get()
                        .load(itemMovies.getMovieImage().isEmpty() ? "null" : itemMovies.getMovieImage())
                        .placeholder(theme_bg)
                        .into(target);

            } catch (Exception e) {
                e.printStackTrace();
                blur.setImageResource(theme_bg);
            }
        } else {
            blur.setImageResource(theme_bg);
        }
    }

    private void play() {
        if (itemData != null){
            Intent intent = new Intent(DetailsMovieActivity.this, PlayerMovieActivity.class);
            intent.putExtra("stream_id", itemData.getStreamID());
            intent.putExtra("movie_name", itemData.getName());
            intent.putExtra("container", itemData.getContainerExtension());
            intent.putExtra("stream_rating", stream_rating);
            intent.putExtra("stream_icon", stream_icon);
            startActivity(intent);
        }
    }

    public Boolean checkPer() {
        if (Build.VERSION.SDK_INT >= 33){
            if ((ContextCompat.checkSelfPermission(DetailsMovieActivity.this, READ_MEDIA_VIDEO) != PackageManager.PERMISSION_GRANTED)) {
                requestPermissions(new String[]{READ_MEDIA_VIDEO}, 1);
                return false;
            } else {
                return true;
            }
        } else if (Build.VERSION.SDK_INT >= 29) {
            if ((ContextCompat.checkSelfPermission(DetailsMovieActivity.this, READ_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED)) {
                requestPermissions(new String[]{READ_EXTERNAL_STORAGE}, 1);
                return false;
            } else {
                return true;
            }
        } else {
            if (ContextCompat.checkSelfPermission(DetailsMovieActivity.this, WRITE_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED) {
                requestPermissions(new String[]{WRITE_EXTERNAL_STORAGE}, 1);
                return false;
            }
            return true;
        }
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        boolean canUseExternalStorage = false;
        boolean canUseNot = false;
        if (requestCode == 1) {
            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                canUseExternalStorage = true;
            }
            if (!canUseExternalStorage) {
                Toast.makeText(DetailsMovieActivity.this, getResources().getString(R.string.err_cannot_use_features), Toast.LENGTH_SHORT).show();
            }
        } else if (requestCode == 103) {
            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                canUseNot = true;
            }
            if (!canUseNot) {
                Toast.makeText(DetailsMovieActivity.this, getResources().getString(R.string.err_cannot_use_features), Toast.LENGTH_SHORT).show();
            }
        }
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
    }

    private final Runnable run = this::seekUpdating;

    public void seekUpdating() {
        try {
            if (DownloadService.arrayListVideo != null && !DownloadService.arrayListVideo.isEmpty()){
                boolean found = false;
                int pos = 0;

                // Find the position of the stream_id in arrayListVideo
                for (int i = 0; i < DownloadService.arrayListVideo.size(); i++) {
                    if (stream_id.equals(DownloadService.arrayListVideo.get(i).getStreamID())) {
                        pos = i;
                        found = true;
                        break;
                    }
                }

                // If stream_id is found in arrayListVideo
                if (found) {
                    // Update itemData if not null
                    if (itemData != null) {
                        itemData.setDownload(true);
                    }

                    // Show progress bar and close button
                    pb_download.setVisibility(View.VISIBLE);
                    iv_download_close.setVisibility(View.VISIBLE);

                    // Set progress to progress bar
                    pb_download.setProgress(DownloadService.arrayListVideo.get(pos).getProgress());
                } else {
                    // Hide progress bar and close button if stream_id is not found
                    hideDownloadViews();
                }
            } else {
                // Hide progress bar and close button if arrayListVideo is null or empty
                hideDownloadViews();
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            // Schedule next update after 1 second (1000 milliseconds)
            seekHandler.removeCallbacks(run);
            seekHandler.postDelayed(run, 1000);
        }
    }

    private void hideDownloadViews() {
        pb_download.setVisibility(View.GONE);
        iv_download_close.setVisibility(View.GONE);
    }

    @Override
    protected void onPause() {
        try {
            seekHandler.removeCallbacks(run);
        } catch (Exception e) {
            e.printStackTrace();
        }
        super.onPause();
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN) {
            if (keyCode == KeyEvent.KEYCODE_BACK){
                finish();
                return true;
            } else if (keyCode == KeyEvent.KEYCODE_HOME){
                ApplicationUtil.openHomeActivity(this);
                return true;
            }
        }
        return super.onKeyDown(keyCode, event);
    }

}