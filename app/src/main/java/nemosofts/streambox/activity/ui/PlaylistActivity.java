package nemosofts.streambox.activity.ui;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.view.KeyEvent;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.view.ShimmerView;

import com.onesignal.Continue;
import com.onesignal.OneSignal;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;

import nemosofts.streambox.R;
import nemosofts.streambox.activity.DownloadActivity;
import nemosofts.streambox.activity.MultipleScreenActivity;
import nemosofts.streambox.activity.NotificationsActivity;
import nemosofts.streambox.activity.PlaylistLiveTvActivity;
import nemosofts.streambox.activity.PlaylistMovieActivity;
import nemosofts.streambox.activity.UsersListActivity;
import nemosofts.streambox.activity.setting.SettingActivity;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.util.helper.SPHelper;

public class PlaylistActivity extends AppCompatActivity implements View.OnClickListener {

    private SPHelper spHelper;
    private ShimmerView shimmer_live, shimmer_movie, shimmer_serials;

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

        spHelper = new SPHelper(this);

        getInfo();
        setListenerHome();

        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.select_live).requestFocus();
        }

        shimmer_live = findViewById(R.id.shimmer_view_live);
        shimmer_movie = findViewById(R.id.shimmer_view_movie);
        shimmer_serials = findViewById(R.id.shimmer_view_serials);
        changeIcon();

        // requestPermission will show the native Android notification permission prompt.
        // NOTE: It's recommended to use a OneSignal In-App Message to prompt instead.
        OneSignal.getNotifications().requestPermission(false, Continue.none());
    }

    private void changeIcon() {
        if (Boolean.FALSE.equals(spHelper.getIsShimmeringHome())){
            shimmer_live.setVisibility(View.GONE);
            shimmer_movie.setVisibility(View.GONE);
            shimmer_serials.setVisibility(View.GONE);
        } else {
            shimmer_live.setVisibility(View.VISIBLE);
            shimmer_movie.setVisibility(View.VISIBLE);
            shimmer_serials.setVisibility(View.VISIBLE);
        }
    }

    private void setListenerHome() {
        findViewById(R.id.iv_notifications).setOnClickListener(this);
        findViewById(R.id.iv_file_download).setOnClickListener(this);
        findViewById(R.id.iv_profile_re).setOnClickListener(this);
        findViewById(R.id.iv_settings).setOnClickListener(this);
        findViewById(R.id.select_live).setOnClickListener(this);
        findViewById(R.id.select_movie).setOnClickListener(this);
        findViewById(R.id.select_multiple_screen).setOnClickListener(this);
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

        try {
            TextView tv_user_name = findViewById(R.id.tv_user_name);
            String user_name = getString(R.string.user_list_user_name)+" "+ spHelper.getAnyName();
            tv_user_name.setText(user_name);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @SuppressLint({"NonConstantResourceId", "UnsafeOptInUsageError"})
    @Override
    public void onClick(@NonNull View id) {
        switch (id.getId()) {
            case R.id.iv_notifications ->
                    startActivity(new Intent(PlaylistActivity.this, NotificationsActivity.class));
            case R.id.iv_file_download ->
                    startActivity(new Intent(PlaylistActivity.this, DownloadActivity.class));
            case R.id.iv_profile_re -> sign_out();
            case R.id.select_live ->
                    new Handler().postDelayed(() -> startActivity(new Intent(PlaylistActivity.this, PlaylistLiveTvActivity.class)), 0);
            case R.id.select_movie ->
                    new Handler().postDelayed(() -> startActivity(new Intent(PlaylistActivity.this, PlaylistMovieActivity.class)), 0);
            case R.id.select_multiple_screen ->
                    new Handler().postDelayed(() -> startActivity(new Intent(PlaylistActivity.this, MultipleScreenActivity.class)), 0);
            case R.id.iv_settings ->
                    startActivity(new Intent(PlaylistActivity.this, SettingActivity.class));
            default -> {
            }
        }
    }

    private void sign_out() {
        DialogUtil.LogoutDialog(PlaylistActivity.this, () -> {
            spHelper.setLoginType(Callback.TAG_LOGIN);
            Intent intent = new Intent(PlaylistActivity.this, UsersListActivity.class);
            new JSHelper(this).removeAllPlaylist();
            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
            intent.putExtra("from", "");
            Toast.makeText(PlaylistActivity.this, getString(R.string.logout_success), Toast.LENGTH_SHORT).show();
            startActivity(intent);
            finish();
        });


    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_ui_playlist;
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
    public void onResume() {
        if (Boolean.TRUE.equals(Callback.isDataUpdate)) {
            Callback.isDataUpdate = false;
            changeIcon();
        }
        if (Boolean.TRUE.equals(Callback.is_recreate)) {
            Callback.is_recreate = false;
            recreate();
        }
        super.onResume();
    }

    @Override
    public void onBackPressed() {
        if (ApplicationUtil.isTvBox(PlaylistActivity.this)) {
            super.onBackPressed();
        } else {
            DialogUtil.ExitDialog(PlaylistActivity.this);
        }
    }
}