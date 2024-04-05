package mycinevo.streambox.activity.UI;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.res.ColorStateList;
import android.graphics.Bitmap;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.KeyEvent;
import android.view.View;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;
import androidx.palette.graphics.Palette;

import com.squareup.picasso.Picasso;
import com.squareup.picasso.Target;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;

import mycinevo.streambox.R;
import mycinevo.streambox.activity.CatchUpActivity;
import mycinevo.streambox.activity.CategoriesActivity;
import mycinevo.streambox.activity.DownloadActivity;
import mycinevo.streambox.activity.LiveTvActivity;
import mycinevo.streambox.activity.MovieActivity;
import mycinevo.streambox.activity.MultipleScreenActivity;
import mycinevo.streambox.activity.NotificationsActivity;
import mycinevo.streambox.activity.ProfileActivity;
import mycinevo.streambox.activity.RadioActivity;
import mycinevo.streambox.activity.SeriesActivity;
import mycinevo.streambox.activity.Setting.SettingActivity;
import mycinevo.streambox.activity.UsersListActivity;
import mycinevo.streambox.asyncTask.LoadLive;
import mycinevo.streambox.asyncTask.LoadLogin;
import mycinevo.streambox.asyncTask.LoadMovies;
import mycinevo.streambox.asyncTask.LoadPoster;
import mycinevo.streambox.asyncTask.LoadSeries;
import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.dialog.ExitDialog;
import mycinevo.streambox.dialog.LiveDownloadDialog;
import mycinevo.streambox.dialog.Toasty;
import mycinevo.streambox.interfaces.LiveListener;
import mycinevo.streambox.interfaces.LoginListener;
import mycinevo.streambox.interfaces.PosterListener;
import mycinevo.streambox.interfaces.SuccessListener;
import mycinevo.streambox.item.ItemPoster;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.NetworkUtils;
import mycinevo.streambox.util.SharedPref;
import mycinevo.streambox.util.helper.DBHelper;
import mycinevo.streambox.util.helper.Helper;
import mycinevo.streambox.util.helper.JSHelper;
import mycinevo.streambox.view.NSoftsProgressDialog;

public class MovieUIActivity extends AppCompatActivity implements View.OnClickListener {

    private DBHelper dbHelper;
    private Helper helper;
    private SharedPref sharedPref;
    private NSoftsProgressDialog progressDialog;
    private ProgressBar pb_live, pb_movie, pb_serials;
    private final Handler handlerLive = new Handler();
    private final Handler handlerMovie = new Handler();
    private final Handler handlerSeries = new Handler();
    private int progressStatusLive = 0, progressStatusMovie = 0, progressStatusSeries = 0;

    private int theme_bg;
    private View vw_live_bg, vw_movies_bg, vw_series_bg, vw_epg_bg, vw_multiple_bg, vw_catch_up_bg;
    private Boolean vibrant = true;

    private final Handler handler = new Handler(Looper.getMainLooper());
    private Runnable runnableCode;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (Boolean.TRUE.equals(Callback.isLandscape)){
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        }
        IfSupported.IsRTL(this);
        IfSupported.IsScreenshot(this);
        IfSupported.hideStatusBar(this);

        Callback.isAppOpen = true;

        theme_bg = ApplicationUtil.openThemeBg(this);

        helper = new Helper(this);
        sharedPref = new SharedPref(this);
        dbHelper = new DBHelper(this);

        progressDialog = new NSoftsProgressDialog(MovieUIActivity.this);

        ImageView iv_bg_blur = findViewById(R.id.iv_bg_blur);
        iv_bg_blur.setImageResource(theme_bg);

        getPoster();

        vw_live_bg = findViewById(R.id.vw_live_bg);
        vw_movies_bg = findViewById(R.id.vw_movies_bg);
        vw_series_bg = findViewById(R.id.vw_series_bg);
        vw_epg_bg = findViewById(R.id.vw_epg_bg);
        vw_multiple_bg = findViewById(R.id.vw_multiple_bg);
        vw_catch_up_bg = findViewById(R.id.vw_catch_up_bg);

        pb_live = findViewById(R.id.pb_live_tv);
        pb_movie = findViewById(R.id.pb_movie);
        pb_serials = findViewById(R.id.pb_serials);

        getInfo();
        setListenerHome();

        changeIcon(sharedPref.getCurrent(Callback.TAG_TV).isEmpty(), Callback.TAG_TV, true);
        changeIcon(sharedPref.getCurrent(Callback.TAG_MOVIE).isEmpty(), Callback.TAG_MOVIE, true);
        changeIcon(sharedPref.getCurrent(Callback.TAG_SERIES).isEmpty(), Callback.TAG_SERIES, true);

        loadLogin();
        chalkedData();

        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.select_live).requestFocus();
        }

        runnableCode = () -> {
            if (!Callback.arrayListPoster.isEmpty()){
                if (Callback.posterPos < (Callback.arrayListPoster.size() - 1)) {
                    Callback.posterPos = Callback.posterPos + 1;
                } else {
                    Callback.posterPos = 0;
                }
                LoadPoster();
                handler.postDelayed(runnableCode, (long) 20 * 1000);
            } else {
                handler.removeCallbacks(runnableCode);
            }
        };
    }

    private void getPoster() {
        if (NetworkUtils.isConnected(this)){
            LoadPoster loadPoster = new LoadPoster(new PosterListener() {
                @Override
                public void onStart() {
                    findViewById(R.id.pb).setVisibility(View.VISIBLE);
                }

                @Override
                public void onEnd(String success, String verifyStatus, String message, ArrayList<ItemPoster> arrayList) {
                    findViewById(R.id.pb).setVisibility(View.GONE);
                    if (!isFinishing()){
                        if (success.equals("1") && (!arrayList.isEmpty())){
                            if (!Callback.arrayListPoster.isEmpty()){
                                Callback.arrayListPoster.clear();
                            }
                            Callback.arrayListPoster.addAll(arrayList);
                            handler.postDelayed(runnableCode, (long) 20 * 1000);
                            LoadPoster();
                        }
                    }
                }
            }, helper.getAPIRequestNSofts(Callback.METHOD_POSTER, "", "", "", ""));
            loadPoster.execute();
        }
    }

    private void LoadPoster() {
        ImageView iv_bg_poster = findViewById(R.id.iv_bg_poster);
        findViewById(R.id.pb).setVisibility(View.VISIBLE);
        Picasso.get()
                .load(Callback.arrayListPoster.get(Callback.posterPos).getPoster())
                .placeholder(theme_bg)
                .into(iv_bg_poster, new com.squareup.picasso.Callback() {
                    @Override
                    public void onSuccess() {
                        loadVibrantColor();
                        findViewById(R.id.pb).setVisibility(View.GONE);
                    }

                    @Override
                    public void onError(Exception e) {
                        findViewById(R.id.pb).setVisibility(View.GONE);
                    }
                });
    }

    private void getInfo() {
        ImageView iv_wifi = findViewById(R.id.iv_wifi);
        if (NetworkUtils.isConnected(this)) {
            if (NetworkUtils.isConnectedMobile(this)){
                iv_wifi.setImageResource(R.drawable.bar_selector_none);
            } else if (NetworkUtils.isConnectedWifi(this)){
                iv_wifi.setImageResource(R.drawable.ic_wifi);
            } else if (NetworkUtils.isConnectedEthernet(this)){
                iv_wifi.setImageResource(R.drawable.ic_ethernet);
            }
        } else {
            iv_wifi.setImageResource(R.drawable.ic_wifi_off);
        }

        try {
            TextView iv_app_date = findViewById(R.id.iv_app_date);
            @SuppressLint("SimpleDateFormat") DateFormat df = new SimpleDateFormat("EEE, d MMM yyyy");
            iv_app_date.setText(df.format(Calendar.getInstance().getTime()));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void setListenerHome() {
        findViewById(R.id.iv_notifications).setOnClickListener(this);
        findViewById(R.id.iv_radio).setOnClickListener(this);
        findViewById(R.id.iv_file_download).setOnClickListener(this);
        findViewById(R.id.iv_profile).setOnClickListener(this);
        findViewById(R.id.iv_profile_re).setOnClickListener(this);
        findViewById(R.id.iv_settings).setOnClickListener(this);
        findViewById(R.id.select_live).setOnClickListener(this);
        findViewById(R.id.select_movie).setOnClickListener(this);
        findViewById(R.id.select_serials).setOnClickListener(this);
        findViewById(R.id.select_epg).setOnClickListener(this);
        findViewById(R.id.select_multiple_screen).setOnClickListener(this);
        findViewById(R.id.select_catch_up).setOnClickListener(this);

        if (!sharedPref.getIsDownload()){
            findViewById(R.id.iv_file_download).setVisibility(View.GONE);
        }
        if (Boolean.FALSE.equals(sharedPref.getIsRadio())){
            findViewById(R.id.iv_radio).setVisibility(View.GONE);
        }
    }

    @SuppressLint({"NonConstantResourceId", "UnsafeOptInUsageError"})
    @Override
    public void onClick(@NonNull View id) {
        switch (id.getId()) {
            case R.id.iv_notifications ->
                    startActivity(new Intent(MovieUIActivity.this, NotificationsActivity.class));
            case R.id.iv_file_download ->
                    startActivity(new Intent(MovieUIActivity.this, DownloadActivity.class));
            case R.id.iv_radio -> {
                if (isDownloadLive()) {
                    startActivity(new Intent(MovieUIActivity.this, RadioActivity.class));
                }
            }
            case R.id.iv_profile ->
                    startActivity(new Intent(MovieUIActivity.this, ProfileActivity.class));
            case R.id.iv_profile_re -> sign_out();
            case R.id.iv_settings ->
                    startActivity(new Intent(MovieUIActivity.this, SettingActivity.class));
            case R.id.select_live -> {
                if (sharedPref.getCurrent(Callback.TAG_TV).isEmpty()) {
                    getLive();
                } else {
                    startActivity(new Intent(MovieUIActivity.this, LiveTvActivity.class));
                }
            }
            case R.id.select_movie -> {
                if (sharedPref.getCurrent(Callback.TAG_MOVIE).isEmpty()) {
                    getMovies();
                } else {
                    startActivity(new Intent(MovieUIActivity.this, MovieActivity.class));
                }
            }
            case R.id.select_serials -> {
                if (sharedPref.getCurrent(Callback.TAG_SERIES).isEmpty()) {
                    getSeries();
                } else {
                    startActivity(new Intent(MovieUIActivity.this, SeriesActivity.class));
                }
            }
            case R.id.select_epg -> {
                if (isDownloadLive()) {
                    startActivity(new Intent(MovieUIActivity.this, CategoriesActivity.class));
                }
            }
            case R.id.select_multiple_screen -> {
                if (isDownloadLive()) {
                    startActivity(new Intent(MovieUIActivity.this, MultipleScreenActivity.class));
                }
            }
            case R.id.select_catch_up -> {
                if (isDownloadLive()) {
                    startActivity(new Intent(MovieUIActivity.this, CatchUpActivity.class));
                }
            }
            case R.id.ll_tv_auto_renew -> getLive();
            case R.id.ll_movie_auto_renew -> getMovies();
            case R.id.ll_series_auto_renew -> getSeries();
            default -> {
            }
        }
    }

    private boolean isDownloadLive() {
        if (!sharedPref.getCurrent(Callback.TAG_TV).isEmpty()){
            return true;
        } else {
            new LiveDownloadDialog(this, this::getLive);
            return false;
        }
    }

    private void loadLogin() {
        if (NetworkUtils.isConnected(this)){
            LoadLogin login = new LoadLogin(new LoginListener() {
                @Override
                public void onStart() {
                    // document why this method is empty
                }

                @Override
                public void onEnd(String success, String username, String password, String message, int auth, String status, String exp_date, String is_trial,
                                  String active_cons, String created_at, String max_connections, String allowed_output_formats, boolean xui, String version,
                                  int revision, String url, String port, String https_port, String server_protocol, String rtmp_port, int timestamp_now,
                                  String time_now, String timezone) {
                    if (!isFinishing() && (success.equals("1"))) {
                        sharedPref.setLoginDetails(username,password,message,auth,status, exp_date, is_trial, active_cons,created_at,max_connections,
                                xui,version,revision,url,port,https_port,server_protocol,rtmp_port,timestamp_now,time_now,timezone
                        );
                        sharedPref.setIsLogged(true);
                    }
                }
            },sharedPref.getServerURL(), helper.getAPIRequestLogin(sharedPref.getUserName(),sharedPref.getPassword()));
            login.execute();
        }
    }

    private void chalkedData() {
        if (Boolean.TRUE.equals(Callback.successLive.equals("1"))){
            try {
                Callback.successLive = "0";
                pb_live.setVisibility(View.VISIBLE);
                progressStatusLive = 0;
                pb_live.setProgress(progressStatusLive);
                findViewById(R.id.vw_live_tv).setVisibility(View.VISIBLE);
                findViewById(R.id.vw_live_epg).setVisibility(View.VISIBLE);
                findViewById(R.id.vw_catch_up).setVisibility(View.VISIBLE);
                findViewById(R.id.vw_multiple_screen).setVisibility(View.VISIBLE);
                handlerLive.postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        if (progressStatusLive < 100) {
                            progressStatusLive++;
                            pb_live.setProgress(progressStatusLive);
                            if (progressStatusLive == 99){
                                findViewById(R.id.vw_live_tv).setVisibility(View.GONE);
                                findViewById(R.id.vw_live_epg).setVisibility(View.GONE);
                                findViewById(R.id.vw_catch_up).setVisibility(View.GONE);
                                findViewById(R.id.vw_multiple_screen).setVisibility(View.GONE);
                                pb_live.setVisibility(View.GONE);
                            }
                            sharedPref.setCurrentDate(Callback.TAG_TV);
                            changeIcon(sharedPref.getCurrent(Callback.TAG_TV).isEmpty(), Callback.TAG_TV, false);
                            handlerLive.postDelayed(this, 10);
                        }
                    }
                }, 10);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        if (Boolean.TRUE.equals(Callback.successMovies.equals("1"))){
            try {
                Callback.successMovies = "0";
                pb_movie.setVisibility(View.VISIBLE);
                progressStatusMovie = 0;
                pb_movie.setProgress(progressStatusMovie);
                findViewById(R.id.vw_movie).setVisibility(View.VISIBLE);
                handlerMovie.postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        if (progressStatusMovie < 100) {
                            progressStatusMovie++;
                            pb_movie.setProgress(progressStatusMovie);
                            if (progressStatusMovie == 99){
                                findViewById(R.id.vw_movie).setVisibility(View.GONE);
                                pb_movie.setVisibility(View.GONE);
                            }
                            sharedPref.setCurrentDate(Callback.TAG_MOVIE);
                            changeIcon(sharedPref.getCurrent(Callback.TAG_MOVIE).isEmpty(), Callback.TAG_MOVIE, false);
                            handlerMovie.postDelayed(this, 10);
                        }
                    }
                }, 10);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        if (Boolean.TRUE.equals(Callback.successSeries.equals("1"))){
            try {
                Callback.successSeries = "0";
                pb_serials.setVisibility(View.VISIBLE);
                progressStatusSeries = 0;
                pb_serials.setProgress(progressStatusSeries);
                findViewById(R.id.vw_serials).setVisibility(View.VISIBLE);
                handlerSeries.postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        if (progressStatusSeries < 100) {
                            progressStatusSeries++;
                            pb_serials.setProgress(progressStatusSeries);
                            if (progressStatusSeries == 99){
                                findViewById(R.id.vw_serials).setVisibility(View.GONE);
                                pb_serials.setVisibility(View.GONE);
                            }
                            sharedPref.setCurrentDate(Callback.TAG_SERIES);
                            changeIcon(sharedPref.getCurrent(Callback.TAG_SERIES).isEmpty(), Callback.TAG_SERIES, false);
                            handlerSeries.postDelayed(this, 10);
                        }
                    }
                }, 10);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    @SuppressLint("UseCompatLoadingForDrawables")
    public void changeIcon(Boolean isDownload, String type, boolean is_view) {
        if (type != null){
            int visibility = Boolean.TRUE.equals(isDownload) ? View.VISIBLE : View.GONE;
            switch (type) {
                case "date_tv" -> {
                    if (is_view) {
                        findViewById(R.id.vw_live_tv).setVisibility(visibility);
                        findViewById(R.id.vw_live_epg).setVisibility(visibility);
                        findViewById(R.id.vw_catch_up).setVisibility(visibility);
                        findViewById(R.id.vw_multiple_screen).setVisibility(visibility);
                    }
                }
                case "date_movies" -> {
                    if (is_view) {
                        findViewById(R.id.vw_movie).setVisibility(visibility);
                    }
                }
                case "date_series" -> {
                    if (is_view) {
                        findViewById(R.id.vw_serials).setVisibility(visibility);
                    }
                }
                default -> {
                }
            }
        }
    }

    private void sign_out() {
        Intent intent = new Intent(MovieUIActivity.this, UsersListActivity.class);
        if (sharedPref.isLogged()) {
            new JSHelper(this).removeAllData();
            dbHelper.removeAllData();
            sharedPref.removeSignOut();
            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
            intent.putExtra("from", "");
            Toast.makeText(MovieUIActivity.this, getString(R.string.logout_success), Toast.LENGTH_SHORT).show();
        } else {
            intent.putExtra("from", "app");
        }
        startActivity(intent);
        finish();
    }

    private void loadVibrantColor() {
        try {
            Picasso.get()
                    .load(Callback.arrayListPoster.get(Callback.posterPos).getPoster())
                    .centerCrop()
                    .resize(200,200)
                    .into(new Target() {
                        @Override
                        public void onBitmapLoaded(Bitmap bitmap, Picasso.LoadedFrom from) {
                            try {
                                Palette.from(bitmap).generate(palette -> {
                                    if (palette != null){
                                        try{
                                            int defaultValue = 0x000000;
                                            int vibrant = palette.getDarkVibrantColor(defaultValue);
                                            vw_live_bg.setBackgroundTintList(ColorStateList.valueOf(vibrant));
                                            vw_movies_bg.setBackgroundTintList(ColorStateList.valueOf(vibrant));
                                            vw_series_bg.setBackgroundTintList(ColorStateList.valueOf(vibrant));
                                            vw_epg_bg.setBackgroundTintList(ColorStateList.valueOf(vibrant));
                                            vw_multiple_bg.setBackgroundTintList(ColorStateList.valueOf(vibrant));
                                            vw_catch_up_bg.setBackgroundTintList(ColorStateList.valueOf(vibrant));
                                        } catch (Exception e) {
                                            e.printStackTrace();
                                        }
                                    }
                                });
                            } catch (Exception e) {
                                e.printStackTrace();
                            }
                        }

                        @Override
                        public void onBitmapFailed(Exception e, Drawable errorDrawable) {
                            if (Boolean.TRUE.equals(vibrant)){
                                vibrant = false;
                                loadVibrantColor();
                            }
                        }

                        @Override
                        public void onPrepareLoad(Drawable placeHolderDrawable) {
                            // document why this method is empty
                        }
                    });
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void getSeries() {
        if (NetworkUtils.isConnected(this)){
            LoadSeries loadSeries = new LoadSeries(this, new SuccessListener() {
                @Override
                public void onStart() {
                    progressDialog.show();
                    findViewById(R.id.vw_serials).setVisibility(View.VISIBLE);
                    pb_serials.setVisibility(View.VISIBLE);
                    progressStatusSeries = 0;
                    pb_serials.setProgress(progressStatusSeries);
                    handlerSeries.postDelayed(new Runnable() {
                        @Override
                        public void run() {
                            if (progressStatusSeries < 50) {
                                progressStatusSeries++;
                                pb_serials.setProgress(progressStatusSeries);
                                handlerSeries.postDelayed(this, 20);
                            }
                        }
                    }, 20);
                }

                @Override
                public void onEnd(String success) {
                    progressDialog.dismiss();
                    if (!isFinishing()){
                        if (success.equals("1")) {
                            handlerSeries.postDelayed(new Runnable() {
                                @Override
                                public void run() {
                                    if (progressStatusSeries < 100) {
                                        progressStatusSeries++;
                                        pb_serials.setProgress(progressStatusSeries);
                                        if (progressStatusSeries == 99){
                                            findViewById(R.id.vw_serials).setVisibility(View.GONE);
                                            pb_serials.setVisibility(View.GONE);
                                        }
                                        handlerSeries.postDelayed(this, 10);
                                    }
                                }
                            }, 10);
                            sharedPref.setCurrentDate(Callback.TAG_SERIES);
                            changeIcon(sharedPref.getCurrent(Callback.TAG_SERIES).isEmpty(), Callback.TAG_SERIES,false);
                            Toast.makeText(MovieUIActivity.this, getString(R.string.added_success), Toast.LENGTH_SHORT).show();
                        }  else {
                            sharedPref.setCurrentDateEmpty(Callback.TAG_SERIES);
                            changeIcon(sharedPref.getCurrent(Callback.TAG_SERIES).isEmpty(), Callback.TAG_SERIES,true);
                            pb_serials.setVisibility(View.GONE);
                            Toasty.makeText(MovieUIActivity.this, getString(R.string.err_server_not_connected), Toasty.ERROR);
                        }
                    }
                }
            });
            loadSeries.execute();
        } else {
            pb_serials.setVisibility(View.GONE);
            Toasty.makeText(MovieUIActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
    }

    private void getMovies() {
        if (NetworkUtils.isConnected(this)){
            LoadMovies loadMovies = new LoadMovies(this,  new SuccessListener() {
                @Override
                public void onStart() {
                    progressDialog.show();
                    findViewById(R.id.vw_movie).setVisibility(View.VISIBLE);
                    pb_movie.setVisibility(View.VISIBLE);
                    progressStatusMovie = 0;
                    pb_movie.setProgress(progressStatusMovie);
                    handlerMovie.postDelayed(new Runnable() {
                        @Override
                        public void run() {
                            if (progressStatusMovie < 50) {
                                progressStatusMovie++;
                                pb_movie.setProgress(progressStatusMovie);
                                handlerMovie.postDelayed(this, 20);
                            }
                        }
                    }, 20);
                }

                @Override
                public void onEnd(String success) {
                    progressDialog.dismiss();
                    if (!isFinishing()){
                        if (success.equals("1")) {
                            handlerMovie.postDelayed(new Runnable() {
                                @Override
                                public void run() {
                                    if (progressStatusMovie < 100) {
                                        progressStatusMovie++;
                                        pb_movie.setProgress(progressStatusMovie);
                                        if (progressStatusMovie == 99){
                                            findViewById(R.id.vw_movie).setVisibility(View.GONE);
                                            pb_movie.setVisibility(View.GONE);
                                        }
                                        handlerMovie.postDelayed(this, 10);
                                    }
                                }
                            }, 10);
                            sharedPref.setCurrentDate(Callback.TAG_MOVIE);
                            changeIcon(sharedPref.getCurrent(Callback.TAG_MOVIE).isEmpty(), Callback.TAG_MOVIE,false);
                            Toast.makeText(MovieUIActivity.this, getString(R.string.added_success), Toast.LENGTH_SHORT).show();
                        }  else {
                            sharedPref.setCurrentDateEmpty(Callback.TAG_MOVIE);
                            changeIcon(sharedPref.getCurrent(Callback.TAG_MOVIE).isEmpty(), Callback.TAG_MOVIE,true);
                            pb_movie.setVisibility(View.GONE);
                            Toasty.makeText(MovieUIActivity.this, getString(R.string.err_server_not_connected), Toasty.ERROR);
                        }
                    }
                }
            });
            loadMovies.execute();
        } else {
            pb_movie.setVisibility(View.GONE);
            Toasty.makeText(MovieUIActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
    }

    private void getLive() {
        if (NetworkUtils.isConnected(this)){
            LoadLive loadLive = new LoadLive(this, new LiveListener() {
                @Override
                public void onStart() {
                    progressDialog.show();
                    findViewById(R.id.vw_live_tv).setVisibility(View.VISIBLE);
                    findViewById(R.id.vw_live_epg).setVisibility(View.VISIBLE);
                    findViewById(R.id.vw_catch_up).setVisibility(View.VISIBLE);
                    findViewById(R.id.vw_multiple_screen).setVisibility(View.VISIBLE);
                    pb_live.setVisibility(View.VISIBLE);
                    progressStatusLive = 0;
                    pb_live.setProgress(progressStatusLive);
                    handlerLive.postDelayed(new Runnable() {
                        @Override
                        public void run() {
                            if (progressStatusLive < 50) {
                                progressStatusLive++;
                                pb_live.setProgress(progressStatusLive);
                                handlerLive.postDelayed(this, 20);
                            }
                        }
                    }, 20);
                }

                @Override
                public void onEnd(String success) {
                    progressDialog.dismiss();
                    if (!isFinishing()){
                        if (success.equals("1")) {
                            pb_live.setProgress(progressStatusLive);
                            handlerLive.postDelayed(new Runnable() {
                                @Override
                                public void run() {
                                    if (progressStatusLive < 100) {
                                        progressStatusLive++;
                                        pb_live.setProgress(progressStatusLive);
                                        if (progressStatusLive == 99){
                                            findViewById(R.id.vw_live_tv).setVisibility(View.GONE);
                                            findViewById(R.id.vw_live_epg).setVisibility(View.GONE);
                                            findViewById(R.id.vw_catch_up).setVisibility(View.GONE);
                                            findViewById(R.id.vw_multiple_screen).setVisibility(View.GONE);
                                            pb_live.setVisibility(View.GONE);
                                        }
                                        handlerLive.postDelayed(this, 10);
                                    }
                                }
                            }, 10);
                            sharedPref.setCurrentDate(Callback.TAG_TV);
                            changeIcon(sharedPref.getCurrent(Callback.TAG_TV).isEmpty(), Callback.TAG_TV, false);
                            Toast.makeText(MovieUIActivity.this, getString(R.string.added_success), Toast.LENGTH_SHORT).show();
                        }  else {
                            sharedPref.setCurrentDateEmpty(Callback.TAG_TV);
                            changeIcon(sharedPref.getCurrent(Callback.TAG_TV).isEmpty(), Callback.TAG_TV, true);
                            pb_live.setVisibility(View.GONE);
                            Toasty.makeText(MovieUIActivity.this, getString(R.string.err_server_not_connected), Toasty.ERROR);
                        }
                    }
                }

                @Override
                public void onCancel(String message) {
                    if (!isFinishing()){
                        sharedPref.setCurrentDateEmpty(Callback.TAG_TV);
                        changeIcon(sharedPref.getCurrent(Callback.TAG_TV).isEmpty(), Callback.TAG_TV, true);
                        Toast.makeText(MovieUIActivity.this, message.isEmpty() ? "" : message, Toast.LENGTH_SHORT).show();
                        pb_live.setVisibility(View.GONE);
                    }
                }
            });
            loadLive.execute();
        } else {
            pb_live.setVisibility(View.GONE);
            Toasty.makeText(MovieUIActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_ui_movie;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
    }

    @Override
    protected void onResume() {
        super.onResume();
        handler.post(runnableCode);
    }

    @Override
    protected void onPause() {
        super.onPause();
        handler.removeCallbacks(runnableCode);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        handler.removeCallbacks(runnableCode);
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)) {
            onBackPressed();
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public void onBackPressed() {
        if (ApplicationUtil.isTvBox(MovieUIActivity.this)) {
            super.onBackPressed();
        } else {
            new ExitDialog(MovieUIActivity.this);
        }
    }
}