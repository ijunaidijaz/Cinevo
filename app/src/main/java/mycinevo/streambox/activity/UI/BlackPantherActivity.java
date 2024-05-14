package mycinevo.streambox.activity.UI;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.view.KeyEvent;
import android.view.View;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
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
import mycinevo.streambox.activity.SettingActivity;
import mycinevo.streambox.activity.UsersListActivity;
import mycinevo.streambox.asyncTask.LoadLive;
import mycinevo.streambox.asyncTask.LoadLogin;
import mycinevo.streambox.asyncTask.LoadMovies;
import mycinevo.streambox.asyncTask.LoadSeries;
import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.dialog.DialogUtil;
import mycinevo.streambox.dialog.PopupAdsDialog;
import mycinevo.streambox.dialog.Toasty;
import mycinevo.streambox.interfaces.LiveListener;
import mycinevo.streambox.interfaces.LoginListener;
import mycinevo.streambox.interfaces.SuccessListener;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.NetworkUtils;
import mycinevo.streambox.util.helper.SPHelper;
import mycinevo.streambox.util.helper.DBHelper;
import mycinevo.streambox.util.helper.Helper;
import mycinevo.streambox.util.helper.JSHelper;
import mycinevo.streambox.view.NSoftsProgressDialog;

public class BlackPantherActivity extends AppCompatActivity implements View.OnClickListener {

    private DBHelper dbHelper;
    private Helper helper;
    private SPHelper spHelper;
    private NSoftsProgressDialog progressDialog;
    private TextView tv_tv_auto_renew, tv_movie_auto_renew, tv_series_auto_renew;
    private ImageView iv_tv_auto_renew, iv_movie_auto_renew, iv_series_auto_renew;
    private ProgressBar pb_live, pb_movie, pb_serials;
    private final Handler handlerLive = new Handler();
    private final Handler handlerMovie = new Handler();
    private final Handler handlerSeries = new Handler();
    private int progressStatusLive = 0, progressStatusMovie = 0, progressStatusSeries = 0;

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
        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        helper = new Helper(this);
        spHelper = new SPHelper(this);
        dbHelper = new DBHelper(this);

        progressDialog = new NSoftsProgressDialog(BlackPantherActivity.this);

        tv_tv_auto_renew = findViewById(R.id.tv_tv_auto_renew);
        tv_movie_auto_renew = findViewById(R.id.tv_movie_auto_renew);
        tv_series_auto_renew = findViewById(R.id.tv_series_auto_renew);

        iv_tv_auto_renew = findViewById(R.id.iv_tv_auto_renew);
        iv_movie_auto_renew = findViewById(R.id.iv_movie_auto_renew);
        iv_series_auto_renew = findViewById(R.id.iv_series_auto_renew);

        pb_live = findViewById(R.id.pb_live_tv);
        pb_movie = findViewById(R.id.pb_movie);
        pb_serials = findViewById(R.id.pb_serials);

        getInfo();
        setListenerHome();

        changeIcon(spHelper.getCurrent(Callback.TAG_TV).isEmpty(), Callback.TAG_TV, true);
        changeIcon(spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty(), Callback.TAG_MOVIE, true);
        changeIcon(spHelper.getCurrent(Callback.TAG_SERIES).isEmpty(), Callback.TAG_SERIES, true);

        if (spHelper.isLogged()){
            TextView tv_user_name = findViewById(R.id.tv_user_name);
            String user_name = getString(R.string.user_list_user_name)+" "+ spHelper.getAnyName();
            tv_user_name.setText(user_name);

            String exp_date = getString(R.string.expiration)+" "+ ApplicationUtil.convertIntToDate(spHelper.getExpDate(), "MMMM dd, yyyy");
            TextView tv_exp_date = findViewById(R.id.tv_exp_date);
            tv_exp_date.setText(exp_date);
        }

        loadLogin();
        chalkedData();
        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.select_live).requestFocus();
        }

        new PopupAdsDialog(this);
    }

    private void setListenerHome() {
        findViewById(R.id.iv_notifications).setOnClickListener(this);
        findViewById(R.id.iv_file_download).setOnClickListener(this);
        findViewById(R.id.iv_radio).setOnClickListener(this);
        findViewById(R.id.iv_profile).setOnClickListener(this);
        findViewById(R.id.iv_profile_re).setOnClickListener(this);
        findViewById(R.id.iv_settings).setOnClickListener(this);
        findViewById(R.id.select_live).setOnClickListener(this);
        findViewById(R.id.select_movie).setOnClickListener(this);
        findViewById(R.id.select_serials).setOnClickListener(this);
        findViewById(R.id.select_epg).setOnClickListener(this);
        findViewById(R.id.select_multiple_screen).setOnClickListener(this);
        findViewById(R.id.select_catch_up).setOnClickListener(this);

        findViewById(R.id.ll_tv_auto_renew).setOnClickListener(this);
        findViewById(R.id.ll_movie_auto_renew).setOnClickListener(this);
        findViewById(R.id.ll_series_auto_renew).setOnClickListener(this);

        if (!spHelper.getIsDownload()){
            findViewById(R.id.iv_file_download).setVisibility(View.GONE);
        }
        if (Boolean.FALSE.equals(spHelper.getIsRadio())){
            findViewById(R.id.iv_radio).setVisibility(View.GONE);
        }

        findViewById(R.id.select_live).setOnLongClickListener(v -> {
            if (!spHelper.getCurrent(Callback.TAG_TV).isEmpty()) {
                DialogUtil.DownloadDataDialog(BlackPantherActivity.this, Callback.TAG_TV, type -> getLive());
            }
            return false;
        });
        findViewById(R.id.select_movie).setOnLongClickListener(v -> {
            if (!spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty()) {
                DialogUtil.DownloadDataDialog(BlackPantherActivity.this, Callback.TAG_TV, type -> getMovies());
            }
            return false;
        });
        findViewById(R.id.select_serials).setOnLongClickListener(v -> {
            if (!spHelper.getCurrent(Callback.TAG_SERIES).isEmpty()) {
                DialogUtil.DownloadDataDialog(BlackPantherActivity.this, Callback.TAG_TV, type -> getSeries());
            }
            return false;
        });
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
                            spHelper.setCurrentDate(Callback.TAG_TV);
                            changeIcon(spHelper.getCurrent(Callback.TAG_TV).isEmpty(), Callback.TAG_TV, false);
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
                            spHelper.setCurrentDate(Callback.TAG_MOVIE);
                            changeIcon(spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty(), Callback.TAG_MOVIE, false);
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
                            spHelper.setCurrentDate(Callback.TAG_SERIES);
                            changeIcon(spHelper.getCurrent(Callback.TAG_SERIES).isEmpty(), Callback.TAG_SERIES, false);
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
            int id = Boolean.TRUE.equals(isDownload) ? R.drawable.ic_file_download : R.drawable.ic_repeate;
            int visibility = Boolean.TRUE.equals(isDownload) ? View.VISIBLE : View.GONE;
            switch (type) {
                case "date_tv" -> {
                    iv_tv_auto_renew.setImageDrawable(getResources().getDrawable(id));
                    tv_tv_auto_renew.setText(Boolean.TRUE.equals(isDownload) ? "" : "Last updated: " + ApplicationUtil.calculateTimeSpan(spHelper.getCurrent(Callback.TAG_TV)));
                    if (is_view) {
                        findViewById(R.id.vw_live_tv).setVisibility(visibility);
                        findViewById(R.id.vw_live_epg).setVisibility(visibility);
                        findViewById(R.id.vw_catch_up).setVisibility(visibility);
                        findViewById(R.id.vw_multiple_screen).setVisibility(visibility);
                    }
                }
                case "date_movies" -> {
                    iv_movie_auto_renew.setImageDrawable(getResources().getDrawable(id));
                    tv_movie_auto_renew.setText(Boolean.TRUE.equals(isDownload) ? "" : "Last updated: " + ApplicationUtil.calculateTimeSpan(spHelper.getCurrent(Callback.TAG_MOVIE)));
                    if (is_view) {
                        findViewById(R.id.vw_movie).setVisibility(visibility);
                    }
                }
                case "date_series" -> {
                    iv_series_auto_renew.setImageDrawable(getResources().getDrawable(id));
                    tv_series_auto_renew.setText(Boolean.TRUE.equals(isDownload) ? "" : "Last updated: " + ApplicationUtil.calculateTimeSpan(spHelper.getCurrent(Callback.TAG_SERIES)));
                    if (is_view) {
                        findViewById(R.id.vw_serials).setVisibility(visibility);
                    }
                }
                default -> {
                }
            }
        }
    }

    @SuppressLint({"NonConstantResourceId", "UnsafeOptInUsageError"})
    @Override
    public void onClick(@NonNull View id) {
        switch (id.getId()) {
            case R.id.iv_notifications ->
                    startActivity(new Intent(BlackPantherActivity.this, NotificationsActivity.class));
            case R.id.iv_file_download ->
                    startActivity(new Intent(BlackPantherActivity.this, DownloadActivity.class));
            case R.id.iv_radio -> {
                if (isDownloadLive()) {
                    startActivity(new Intent(BlackPantherActivity.this, RadioActivity.class));
                }
            }
            case R.id.iv_profile ->
                    startActivity(new Intent(BlackPantherActivity.this, ProfileActivity.class));
            case R.id.iv_profile_re -> sign_out();
            case R.id.iv_settings ->
                    startActivity(new Intent(BlackPantherActivity.this, SettingActivity.class));
            case R.id.select_live -> {
                if (spHelper.getCurrent(Callback.TAG_TV).isEmpty()) {
                    getLive();
                } else {
                    startActivity(new Intent(BlackPantherActivity.this, LiveTvActivity.class));
                }
            }
            case R.id.select_movie -> {
                if (spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty()) {
                    getMovies();
                } else {
                    startActivity(new Intent(BlackPantherActivity.this, MovieActivity.class));
                }
            }
            case R.id.select_serials -> {
                if (spHelper.getCurrent(Callback.TAG_SERIES).isEmpty()) {
                    getSeries();
                } else {
                    startActivity(new Intent(BlackPantherActivity.this, SeriesActivity.class));
                }
            }
            case R.id.select_epg -> {
                if (isDownloadLive()) {
                    startActivity(new Intent(BlackPantherActivity.this, CategoriesActivity.class));
                }
            }
            case R.id.select_multiple_screen -> {
                if (isDownloadLive()) {
                    startActivity(new Intent(BlackPantherActivity.this, MultipleScreenActivity.class));
                }
            }
            case R.id.select_catch_up -> {
                if (isDownloadLive()) {
                    startActivity(new Intent(BlackPantherActivity.this, CatchUpActivity.class));
                }
            }
            case R.id.ll_tv_auto_renew -> getLive();
            case R.id.ll_movie_auto_renew -> getMovies();
            case R.id.ll_series_auto_renew -> getSeries();
            default -> {
            }
        }
    }

    private void sign_out() {
        DialogUtil.LogoutDialog(BlackPantherActivity.this, () -> {
            Intent intent = new Intent(BlackPantherActivity.this, UsersListActivity.class);
            if (spHelper.isLogged()) {
                new JSHelper(this).removeAllData();
                dbHelper.removeAllData();
                spHelper.removeSignOut();
                intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
                intent.putExtra("from", "");
                Toast.makeText(BlackPantherActivity.this, getString(R.string.logout_success), Toast.LENGTH_SHORT).show();
            } else {
                intent.putExtra("from", "app");
            }
            startActivity(intent);
            finish();
        });
    }

    private boolean isDownloadLive() {
        if (!spHelper.getCurrent(Callback.TAG_TV).isEmpty()){
            return true;
        } else {
            DialogUtil.LiveDownloadDialog(this, this::getLive);
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
                public void onEnd(String success, String username, String password, String message, int auth, String status, String exp_date, String is_trial, String active_cons, String created_at, String max_connections, String allowed_output_formats, boolean xui, String version, int revision, String url, String port, String https_port, String server_protocol, String rtmp_port, int timestamp_now, String time_now, String timezone) {
                    if (!isFinishing() && (success.equals("1"))) {
                        spHelper.setLoginDetails(username,password,message,auth,status, exp_date, is_trial, active_cons,created_at,max_connections,
                                xui,version,revision,url,port,https_port,server_protocol,rtmp_port,timestamp_now,time_now,timezone
                        );
                        spHelper.setIsLogged(true);
                    }
                }
            }, spHelper.getServerURL(), helper.getAPIRequestLogin(spHelper.getUserName(), spHelper.getPassword()));
            login.execute();
        }
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
                            spHelper.setCurrentDate(Callback.TAG_SERIES);
                            changeIcon(spHelper.getCurrent(Callback.TAG_SERIES).isEmpty(), Callback.TAG_SERIES,false);
                            Toast.makeText(BlackPantherActivity.this, getString(R.string.added_success), Toast.LENGTH_SHORT).show();
                        }  else {
                            spHelper.setCurrentDateEmpty(Callback.TAG_SERIES);
                            changeIcon(spHelper.getCurrent(Callback.TAG_SERIES).isEmpty(), Callback.TAG_SERIES,true);
                            pb_serials.setVisibility(View.GONE);
                            Toast.makeText(BlackPantherActivity.this, getString(R.string.err_server_not_connected), Toast.LENGTH_SHORT).show();
                        }
                    }
                }
            });
            loadSeries.execute();
        } else {
            pb_serials.setVisibility(View.GONE);
            Toasty.makeText(BlackPantherActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
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
                            spHelper.setCurrentDate(Callback.TAG_MOVIE);
                            changeIcon(spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty(), Callback.TAG_MOVIE,false);
                            Toast.makeText(BlackPantherActivity.this, getString(R.string.added_success), Toast.LENGTH_SHORT).show();
                        }  else {
                            spHelper.setCurrentDateEmpty(Callback.TAG_MOVIE);
                            changeIcon(spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty(), Callback.TAG_MOVIE,true);
                            pb_movie.setVisibility(View.GONE);
                            Toast.makeText(BlackPantherActivity.this, getString(R.string.err_server_not_connected), Toast.LENGTH_SHORT).show();
                        }
                    }
                }
            });
            loadMovies.execute();
        } else {
            pb_movie.setVisibility(View.GONE);
            Toasty.makeText(BlackPantherActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
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
                            spHelper.setCurrentDate(Callback.TAG_TV);
                            changeIcon(spHelper.getCurrent(Callback.TAG_TV).isEmpty(), Callback.TAG_TV, false);
                            Toast.makeText(BlackPantherActivity.this, getString(R.string.added_success), Toast.LENGTH_SHORT).show();
                        }  else {
                            spHelper.setCurrentDateEmpty(Callback.TAG_TV);
                            changeIcon(spHelper.getCurrent(Callback.TAG_TV).isEmpty(), Callback.TAG_TV, true);
                            pb_live.setVisibility(View.GONE);
                            Toast.makeText(BlackPantherActivity.this, getString(R.string.err_server_not_connected), Toast.LENGTH_SHORT).show();
                        }
                    }
                }

                @Override
                public void onCancel(String message) {
                    if (!isFinishing()){
                        spHelper.setCurrentDateEmpty(Callback.TAG_TV);
                        changeIcon(spHelper.getCurrent(Callback.TAG_TV).isEmpty(), Callback.TAG_TV, true);
                        Toast.makeText(BlackPantherActivity.this, message.isEmpty() ? "" : message, Toast.LENGTH_SHORT).show();
                        pb_live.setVisibility(View.GONE);
                    }
                }
            });
            loadLive.execute();
        } else {
            pb_live.setVisibility(View.GONE);
            Toasty.makeText(BlackPantherActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_ui_black_panther;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
    }

    @Override
    public void onDestroy() {
        try {
            dbHelper.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
        super.onDestroy();
    }

    @Override
    public void onResume() {
        if (Boolean.TRUE.equals(Callback.is_recreate)) {
            Callback.is_recreate = false;
            recreate();
        }
        super.onResume();
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
        if (ApplicationUtil.isTvBox(BlackPantherActivity.this)) {
            super.onBackPressed();
        } else {
            DialogUtil.ExitDialog(BlackPantherActivity.this);
        }
    }
}