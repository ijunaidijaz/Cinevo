package mycinevo.streambox.activity;

import static mycinevo.streambox.util.helper.Helper.hideNavigationKeys;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.view.KeyEvent;
import android.view.View;
import android.view.Window;
import android.widget.ProgressBar;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.media3.common.MediaItem;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.common.util.Util;
import androidx.media3.datasource.DataSource;
import androidx.media3.datasource.DefaultDataSourceFactory;
import androidx.media3.datasource.RawResourceDataSource;
import androidx.media3.exoplayer.ExoPlayer;
import androidx.media3.exoplayer.source.MediaSource;
import androidx.media3.exoplayer.source.ProgressiveMediaSource;
import androidx.nemosofts.envato.EnvatoProduct;
import androidx.nemosofts.envato.interfaces.EnvatoListener;

import com.facebook.shimmer.ShimmerFrameLayout;

import org.json.JSONArray;

import mycinevo.streambox.BuildConfig;
import mycinevo.streambox.R;
import mycinevo.streambox.activity.UI.PlaylistActivity;
import mycinevo.streambox.activity.UI.SingleStreamActivity;
import mycinevo.streambox.asyncTask.LoadAbout;
import mycinevo.streambox.asyncTask.LoadData;
import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.interfaces.AboutListener;
import mycinevo.streambox.interfaces.DataListener;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.NetworkUtils;
import mycinevo.streambox.util.SharedPref;
import mycinevo.streambox.util.helper.Helper;

@SuppressLint("CustomSplashScreen")
public class SplashActivity extends AppCompatActivity implements EnvatoListener {

    Helper helper;
    SharedPref sharedPref;
    private ProgressBar pb;
    private ExoPlayer exoPlayer = null;
    private int delayMillis = 3500;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_splash);
        if (Boolean.TRUE.equals(Callback.isLandscape)){
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        }
        hideNavigationKeys(getWindow());
        IfSupported.IsRTL(this);
        IfSupported.IsScreenshot(this);
        IfSupported.hideStatusBar(this);
        IfSupported.keepScreenOn(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        helper = new Helper(this);
        sharedPref = new SharedPref(this);

        if (Boolean.FALSE.equals(sharedPref.getIsShimmeringSplash())){
            ShimmerFrameLayout shimmer = findViewById(R.id.shimmer_view);
            shimmer.hideShimmer();
        }

        pb = findViewById(R.id.pb_splash);

        prepareAudio();
        loadAboutData();
    }

    private void loadAboutData() {
        if (NetworkUtils.isConnected(this)){
            LoadAbout loadAbout = new LoadAbout(SplashActivity.this, new AboutListener() {
                @Override
                public void onStart() {
                    pb.setVisibility(View.VISIBLE);
                }

                @Override
                public void onEnd(String success, String verifyStatus, String message){
                    pb.setVisibility(View.GONE);
                    if (success.equals("1")){
                        setSaveData();
                    } else {
                        if (Boolean.TRUE.equals(sharedPref.getIsAboutDetails())){
                            setSaveData();
                        } else {
                            errorDialog(getString(R.string.err_server_error), getString(R.string.err_server_not_connected));
                        }
                    }
                }
            });
            loadAbout.execute();
        } else {
            if (Boolean.TRUE.equals(sharedPref.getIsAboutDetails())){
                setSaveData();
            } else {
                errorDialog(getString(R.string.err_internet_not_connected), getString(R.string.err_connect_net_try));
            }
        }
    }

    @OptIn(markerClass = UnstableApi.class)
    private void prepareAudio() {
        if (Boolean.TRUE.equals(sharedPref.getIsSplashAudio())){
            exoPlayer = new ExoPlayer.Builder(this).build();
            DataSource.Factory dataSourceFactory = new DefaultDataSourceFactory(this, Util.getUserAgent(this, "nemosofts_rc"));
            Uri fileUri = RawResourceDataSource.buildRawResourceUri(R.raw.opener_logo);
            MediaSource mediaSource = new ProgressiveMediaSource.Factory(dataSourceFactory).createMediaSource(MediaItem.fromUri(fileUri));
            exoPlayer.setMediaSource(mediaSource);
            exoPlayer.prepare();
            exoPlayer.setPlayWhenReady(false);
        } else {
            delayMillis = 2000;
        }
    }

    private void playAudio() {
        if (Boolean.TRUE.equals(sharedPref.getIsSplashAudio()) && exoPlayer != null){
            exoPlayer.play();
        }
    }

    private void loadSettings() {
        if (Boolean.FALSE.equals(sharedPref.getIsAboutDetails())){
            sharedPref.setAboutDetails(true);
        }
        if (Boolean.TRUE.equals(Callback.isAppUpdate) && Callback.app_new_version != BuildConfig.VERSION_CODE){
            openDialogActivity(Callback.DIALOG_TYPE_UPDATE);
        } else if(Boolean.TRUE.equals(sharedPref.getIsMaintenance())){
            openDialogActivity(Callback.DIALOG_TYPE_MAINTENANCE);
        } else {
            if (sharedPref.getLoginType().equals(Callback.TAG_LOGIN_SINGLE_STREAM)){
                playAudio();
                new Handler().postDelayed(this::openSingleStream, delayMillis);
            }
            else if (sharedPref.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
                playAudio();
                new Handler().postDelayed(this::openPlaylistActivity, delayMillis);
            }
            else if (sharedPref.getLoginType().equals(Callback.TAG_LOGIN_ONE_UI) || sharedPref.getLoginType().equals(Callback.TAG_LOGIN_STREAM)){
                if (Boolean.TRUE.equals(sharedPref.getIsFirst())) {
                    playAudio();
                    new Handler().postDelayed(this::openSelectPlayer, delayMillis);
                } else {
                    if (Boolean.FALSE.equals(sharedPref.getIsAutoLogin())) {
                        playAudio();
                        new Handler().postDelayed(this::openSelectPlayer, delayMillis);
                    } else {
                        get_data();
                    }
                }
            } else {
                playAudio();
                new Handler().postDelayed(this::openSelectPlayer, delayMillis);
            }
        }
    }

    private void get_data() {
        if (NetworkUtils.isConnected(this)){
            LoadData loadData = new LoadData(this, new DataListener() {
                @Override
                public void onStart() {
                    pb.setVisibility(View.VISIBLE);
                }

                @Override
                public void onEnd(String success, JSONArray arrayLive, JSONArray arraySeries, JSONArray arrayMovies) {
                    pb.setVisibility(View.GONE);
                    if (Boolean.TRUE.equals(sharedPref.getIsSplashAudio())){
                        playAudio();
                        new Handler().postDelayed(()-> ApplicationUtil.openThemeActivity(SplashActivity.this), delayMillis);
                    } else {
                        ApplicationUtil.openThemeActivity(SplashActivity.this);
                    }
                }
            });
            loadData.execute();
        } else {
            if (Boolean.TRUE.equals(sharedPref.getIsSplashAudio())){
                playAudio();
                new Handler().postDelayed(()-> ApplicationUtil.openThemeActivity(SplashActivity.this), delayMillis);
            } else {
                ApplicationUtil.openThemeActivity(SplashActivity.this);
            }
        }
    }

    @SuppressLint("UnsafeOptInUsageError")
    private void openSelectPlayer() {
        Intent intent = new Intent(SplashActivity.this, SelectPlayerActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        intent.putExtra("from", "");
        startActivity(intent);
        finish();
    }

    @SuppressLint("UnsafeOptInUsageError")
    private void openSingleStream() {
        Intent intent = new Intent(SplashActivity.this, SingleStreamActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        startActivity(intent);
        finish();
    }

    private void openPlaylistActivity() {
        Intent intent = new Intent(SplashActivity.this, PlaylistActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        startActivity(intent);
        finish();
    }

    private void openDialogActivity(String type) {
        Intent intent = new Intent(SplashActivity.this, DialogActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        intent.putExtra("from", type);
        startActivity(intent);
        finish();
    }

    @Override
    public void onStartPairing() {
        pb.setVisibility(View.VISIBLE);
    }

    @Override
    public void onConnected() {
        pb.setVisibility(View.GONE);
        loadSettings();
    }

    @Override
    public void onUnauthorized(String message) {
        pb.setVisibility(View.GONE);
        errorDialog(getString(R.string.err_unauthorized_access), message);
    }

    @Override
    public void onReconnect() {
        Toast.makeText(SplashActivity.this, "Please wait a minute", Toast.LENGTH_SHORT).show();
    }

    @Override
    public void onError() {
        pb.setVisibility(View.GONE);
        errorDialog(getString(R.string.err_server_error), getString(R.string.err_server_not_connected));
    }

    private void errorDialog(String title, String message) {
        final AlertDialog.Builder alertDialog = new AlertDialog.Builder(SplashActivity.this, R.style.ThemeDialog);
        alertDialog.setTitle(title);
        alertDialog.setMessage(message);
        alertDialog.setCancelable(false);
        if (title.equals(getString(R.string.err_internet_not_connected))) {
            alertDialog.setNegativeButton(getString(R.string.retry), (dialog, which) -> loadSettings());
        }
        alertDialog.setOnKeyListener((dialog1, keyCode, event) -> {
            if (event.getAction() == KeyEvent.ACTION_DOWN) {
                if (keyCode == KeyEvent.KEYCODE_BACK){
                    finish();
                } else if (keyCode == KeyEvent.KEYCODE_DPAD_CENTER || keyCode == KeyEvent.KEYCODE_ENTER){
                    if (title.equals(getString(R.string.err_internet_not_connected))) {
                        loadSettings();
                    } else {
                        finish();
                    }
                }
                return false;
            }
            return false;
        });
        alertDialog.setPositiveButton(getString(R.string.exit), (dialog, which) -> finish());
        alertDialog.show();
    }

    private void setSaveData() {
        new EnvatoProduct(this, this).execute();
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)){
            onBackPressed();
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }

}