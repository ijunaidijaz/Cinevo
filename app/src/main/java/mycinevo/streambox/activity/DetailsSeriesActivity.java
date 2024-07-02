package mycinevo.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.graphics.Bitmap;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.os.Handler;
import android.util.SparseArray;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.view.BlurImage;
import androidx.nemosofts.view.youtubeExtractor.VideoMeta;
import androidx.nemosofts.view.youtubeExtractor.YouTubeExtractor;
import androidx.nemosofts.view.youtubeExtractor.YtFile;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;
import com.squareup.picasso.Target;

import java.util.ArrayList;

import mycinevo.streambox.R;
import mycinevo.streambox.adapter.AdapterEpisodes;
import mycinevo.streambox.adapter.AdapterSeason;
import mycinevo.streambox.asyncTask.LoadSeriesID;
import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.dialog.FeedBackDialog;
import mycinevo.streambox.dialog.Toasty;
import mycinevo.streambox.interfaces.SeriesIDListener;
import mycinevo.streambox.item.ItemEpisodes;
import mycinevo.streambox.item.ItemInfoSeasons;
import mycinevo.streambox.item.ItemSeasons;
import mycinevo.streambox.item.ItemSeries;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.NetworkUtils;
import mycinevo.streambox.util.helper.SPHelper;
import mycinevo.streambox.util.helper.DBHelper;
import mycinevo.streambox.util.helper.Helper;
import mycinevo.streambox.view.NSoftsProgressDialog;

public class DetailsSeriesActivity extends AppCompatActivity {

    private int playback = 0;
    private Helper helper;
    private DBHelper dbHelper;
    private SPHelper spHelper;
    private String series_id = "0", series_name="", series_rating="", series_cover="";
    private TextView tv_page_title, tv_directed, tv_release, tv_genre, tv_plot;
    private ImageView iv_series;
    private ImageView iv_star_1, iv_star_2, iv_star_3, iv_star_4, iv_star_5;
    private ArrayList<ItemSeasons> arraySeasons;
    private ArrayList<ItemEpisodes> arrayAllEpisodes;
    private ArrayList<ItemEpisodes> arrayEpisodes;
    private RecyclerView rv_episodes;
    private AdapterEpisodes adapterEpisodes;
    private String season_id="0";
    private String youtube ="https://www.youtube.com/watch?v=",youtube_title="" ;
    private ImageView iv_fav;
    private int theme_bg;
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

        series_id = getIntent().getStringExtra("series_id");
        series_name = getIntent().getStringExtra("series_name");
        series_rating = getIntent().getStringExtra("series_rating");
        series_cover = getIntent().getStringExtra("series_cover");

        helper = new Helper(this);
        dbHelper = new DBHelper(this);
        spHelper = new SPHelper(this);

        helper = new Helper(this, (position, type) -> {
            @SuppressLint("UnsafeOptInUsageError") Intent intent = new Intent(DetailsSeriesActivity.this, PlayerEpisodesActivity.class);
            Callback.playPosEpisodes = position;
            if (!Callback.arrayListEpisodes.isEmpty()) {
                Callback.arrayListEpisodes.clear();
            }
            Callback.arrayListEpisodes.addAll(arrayEpisodes);
            startActivity(intent);
        });

        arraySeasons = new ArrayList<>();
        arrayAllEpisodes = new ArrayList<>();
        arrayEpisodes = new ArrayList<>();

        progressDialog = new NSoftsProgressDialog(DetailsSeriesActivity.this);

        ll_page = findViewById(R.id.ll_page);
        shimmer = findViewById(R.id.fl_shimmer);
        tv_page_title = findViewById(R.id.tv_page_title);
        iv_series = findViewById(R.id.iv_series);
        tv_directed = findViewById(R.id.tv_directed);
        tv_release = findViewById(R.id.tv_release);
        tv_genre = findViewById(R.id.tv_genre);
        tv_plot = findViewById(R.id.tv_plot);
        iv_fav = findViewById(R.id.iv_fav);

        iv_star_1 = findViewById(R.id.iv_star_1);
        iv_star_2 = findViewById(R.id.iv_star_2);
        iv_star_3 = findViewById(R.id.iv_star_3);
        iv_star_4 = findViewById(R.id.iv_star_4);
        iv_star_5 = findViewById(R.id.iv_star_5);

        rv_episodes = findViewById(R.id.rv_episodes);
        rv_episodes.setHasFixedSize(true);
        LinearLayoutManager llm = new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
        rv_episodes.setLayoutManager(llm);
        rv_episodes.setNestedScrollingEnabled(false);

        iv_fav.setOnClickListener(v -> {
            if (Boolean.TRUE.equals(dbHelper.checkSeries(DBHelper.TABLE_FAV_SERIES, series_id))){
                dbHelper.removeFavSeries(DBHelper.TABLE_FAV_SERIES, series_id);
                iv_fav.setImageResource(R.drawable.ic_favorite_border);
                Toast.makeText(this, getString(R.string.fav_remove_success), Toast.LENGTH_SHORT).show();
            } else {
                ItemSeries itemSeries = new ItemSeries(series_name,series_id,series_cover,series_rating);
                dbHelper.addToSeries(DBHelper.TABLE_FAV_SERIES, itemSeries, 0);
                iv_fav.setImageResource(R.drawable.ic_favorite);
                Toast.makeText(this, getString(R.string.fav_success), Toast.LENGTH_SHORT).show();
            }
        });

        findViewById(R.id.ll_play_trailer).setOnClickListener(v -> {
            if (findViewById(R.id.pb_trailer).getVisibility() == View.GONE){
                showYouTubeExtractor();
            }
        });

        getData();
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_details_series;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
    }

    private void getData() {
        if (NetworkUtils.isConnected(this)){
            LoadSeriesID loadSeriesID = new LoadSeriesID(this, new SeriesIDListener() {
                @Override
                public void onStart() {
                    addShimmer();
                }

                @Override
                public void onEnd(String success, ArrayList<ItemInfoSeasons> arrayListInfo, ArrayList<ItemSeasons> arrayListSeasons, ArrayList<ItemEpisodes> arrayListEpisodes) {
                    if (!isFinishing()){
                        if (success.equals("1")) {
                            if (!arrayListInfo.isEmpty()){
                                setInfo(arrayListInfo.get(0));
                            }
                            if (!arrayListEpisodes.isEmpty()){
                                arrayAllEpisodes.addAll(arrayListEpisodes);
                            }
                            if (!arrayListSeasons.isEmpty()){
                                arraySeasons.addAll(arrayListSeasons);
                            } else {
                                if (!arrayListEpisodes.isEmpty()){
                                    arraySeasons.add(new ItemSeasons("Episodes ("+arrayListEpisodes.size()+")","0"));
                                }
                            }
                            ll_page.setVisibility(View.VISIBLE);
                            removeShimmer();
                            setSeasonsAdapter();
                        }  else {
                            if (playback < 2){
                                playback = playback + 1;
                                Toast.makeText(DetailsSeriesActivity.this, "Checking error - "+ String.valueOf(playback)+"/2", Toast.LENGTH_SHORT).show();
                                getData();
                            } else {
                                removeShimmer();
                                playback = 1;
                                Toasty.makeText(DetailsSeriesActivity.this, getString(R.string.err_server_not_connected), Toasty.ERROR);
                            }
                        }
                    }
                }
            }, helper.getAPIRequestID("get_series_info","series_id", series_id, spHelper.getUserName(), spHelper.getPassword()));
            loadSeriesID.execute();
        } else {
            Toasty.makeText(DetailsSeriesActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
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
            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.shimmer_details_series, null);
            shimmer.addView(myView);
        } else {
            ll_page.setVisibility(View.VISIBLE);
            if (!progressDialog.isShowing()){
                progressDialog.show();
            }
        }
    }

    private void setSeasonsAdapter() {
        RecyclerView rv_seasons = findViewById(R.id.rv_seasons);
        rv_seasons.setHasFixedSize(true);
        LinearLayoutManager llm = new LinearLayoutManager(this, LinearLayoutManager.HORIZONTAL, false);
        rv_seasons.setLayoutManager(llm);
        rv_seasons.setNestedScrollingEnabled(false);

        if (!arraySeasons.isEmpty()) {
            AdapterSeason adapterColors = new AdapterSeason(this, arraySeasons, (itemSeasons, position) -> {
                season_id = arraySeasons.get(position).getSeasonNumber();
                setSeasonAdapter();
            });
            rv_seasons.setAdapter(adapterColors);
            season_id = arraySeasons.get(0).getSeasonNumber();
            setSeasonAdapter();
            if (ApplicationUtil.isTvBox(this)){
                rv_seasons.requestFocus();
            }
        } else {
            Toasty.makeText(DetailsSeriesActivity.this, getString(R.string.err_no_data_found), Toasty.ERROR);
        }
    }

    @SuppressLint("NotifyDataSetChanged")
    private void setSeasonAdapter() {
        if (!arrayAllEpisodes.isEmpty()){

            if (!arrayEpisodes.isEmpty()){
                arrayEpisodes.clear();
            }

            if (!season_id.equals("0")){
                for (int i = 0; i < arrayAllEpisodes.size(); i++) {
                    if (arrayAllEpisodes.get(i).getSeason().equals(season_id)){
                        arrayEpisodes.add(arrayAllEpisodes.get(i));
                    }
                }
            } else {
                arrayEpisodes.addAll(arrayAllEpisodes);
            }

            if (!arrayEpisodes.isEmpty()){
                adapterEpisodes = new AdapterEpisodes(arrayEpisodes, series_cover, (itemEpisodes, position) -> helper.showInterAd(position,""));
                rv_episodes.setAdapter(adapterEpisodes);
            } else if (adapterEpisodes != null){
                adapterEpisodes.notifyDataSetChanged();
            }
            findViewById(R.id.tv_empty_msg).setVisibility(arrayEpisodes.isEmpty()? View.VISIBLE : View.GONE);
        } else {
            findViewById(R.id.tv_empty_msg).setVisibility(View.VISIBLE);
        }
    }

    private void setInfo(@NonNull ItemInfoSeasons itemInfoSeasons) {

        findViewById(R.id.iv_feedback).setOnClickListener(v -> new FeedBackDialog(this).showDialog("Series - "+itemInfoSeasons.getName()));

        tv_page_title.setText(itemInfoSeasons.getName());
        tv_directed.setText(itemInfoSeasons.getDirector().isEmpty() || itemInfoSeasons.getDirector().equals("null") ? "N/A" : itemInfoSeasons.getDirector());
        tv_release.setText(itemInfoSeasons.getReleaseDate());
        tv_genre.setText(itemInfoSeasons.getGenre().isEmpty() || itemInfoSeasons.getGenre().equals("null") ? "N/A" : itemInfoSeasons.getGenre());
        tv_plot.setText(itemInfoSeasons.getPlot());

        iv_fav.setImageResource(Boolean.TRUE.equals(dbHelper.checkSeries(DBHelper.TABLE_FAV_SERIES, series_id)) ? R.drawable.ic_favorite : R.drawable.ic_favorite_border);

        try {
            Picasso.get()
                    .load(itemInfoSeasons.getCover().isEmpty() ? "null" : itemInfoSeasons.getCover())
                    .placeholder(R.drawable.material_design_default)
                    .into(iv_series);
        } catch (Exception e) {
            e.printStackTrace();
        }

        ApplicationUtil.setRating(itemInfoSeasons.getRating5based(), iv_star_1, iv_star_2, iv_star_3, iv_star_4, iv_star_5);

        setBlur(series_cover);

        if (itemInfoSeasons.getYoutubeTrailer().isEmpty()){
            findViewById(R.id.ll_play_trailer).setVisibility(View.GONE);
        } else {
            findViewById(R.id.ll_play_trailer).setVisibility(View.VISIBLE);
            youtube = "https://www.youtube.com/watch?v="+itemInfoSeasons.getYoutubeTrailer();
            youtube_title = itemInfoSeasons.getName();
        }
        findViewById(R.id.ll_play_trailer).setVisibility(View.GONE);
        try {
            ItemSeries itemSeries = new ItemSeries(series_name,series_id,series_cover,series_rating);
            dbHelper.addToSeries(DBHelper.TABLE_RECENT_SERIES, itemSeries, spHelper.getMovieLimit());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void setBlur(String cover) {
        ImageView imageViewBackground = findViewById(R.id.iv_bg_blur);
        try {
            Target target = new Target() {
                @Override
                public void onBitmapLoaded(Bitmap bitmap, Picasso.LoadedFrom from) {
                    try {
                        int blur_amount = 80;
                        imageViewBackground.setImageBitmap(BlurImage.fastBlur(bitmap, 1f, blur_amount));
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }

                @Override
                public void onBitmapFailed(Exception e, Drawable errorDrawable) {
                    imageViewBackground.setImageResource(theme_bg);
                }
                @Override
                public void onPrepareLoad(Drawable placeHolderDrawable) {
                    // document why this method is empty
                }
            };
            imageViewBackground.setTag(target);
            Picasso.get()
                    .load(cover)
                    .placeholder(theme_bg)
                    .into(target);

        } catch (Exception e) {
            e.printStackTrace();
            imageViewBackground.setImageResource(theme_bg);
        }
    }

    @SuppressLint("StaticFieldLeak")
    private void showYouTubeExtractor() {
        progressDialog.show();
        findViewById(R.id.tv_trailer).setVisibility(View.GONE);
        findViewById(R.id.pb_trailer).setVisibility(View.VISIBLE);
        if (youtube != null && !youtube.isEmpty()) {
            new YouTubeExtractor(this) {

                @Override
                public void onExtractionComplete(SparseArray<YtFile> ytFiles, VideoMeta vMeta) {
                    if (!isFinishing()){
                        progressDialog.dismiss();
                        findViewById(R.id.tv_trailer).setVisibility(View.VISIBLE);
                        findViewById(R.id.pb_trailer).setVisibility(View.GONE);
                        new Handler().postDelayed(() -> {
                            if (ytFiles != null) {
                                try {
                                    String downloadUrl = ytFiles.get(22).getUrl();
                                    @SuppressLint("UnsafeOptInUsageError") Intent intent = new Intent(DetailsSeriesActivity.this, PlayerSingleURLActivity.class);
                                    intent.putExtra("channel_title", youtube_title);
                                    intent.putExtra("channel_url", downloadUrl);
                                    startActivity(intent);
                                } catch (Exception e1) {
                                    e1.printStackTrace();
                                }
                            }
                        }, 10);
                    }
                }

                @Override
                protected void onCancelled(SparseArray<YtFile> ytFileSparseArray) {
                    super.onCancelled(ytFileSparseArray);
                    if (!isFinishing()){
                        findViewById(R.id.tv_trailer).setVisibility(View.VISIBLE);
                        findViewById(R.id.pb_trailer).setVisibility(View.GONE);
                    }
                }

            }.extract(youtube, true, true);
        } else {
            findViewById(R.id.tv_trailer).setVisibility(View.VISIBLE);
            findViewById(R.id.pb_trailer).setVisibility(View.GONE);
            Toasty.makeText(DetailsSeriesActivity.this, "No YouTube trailer available", Toasty.ERROR);
        }
    }

    @Override
    public void onDestroy() {
        if (progressDialog != null && progressDialog.isShowing()){
            progressDialog.cancel();
        }
        super.onDestroy();
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