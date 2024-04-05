package mycinevo.streambox.activity.Setting;

import android.annotation.SuppressLint;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.view.KeyEvent;
import android.view.View;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;

import mycinevo.streambox.R;
import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.dialog.HoursDialog;
import mycinevo.streambox.dialog.LimitDialog;
import mycinevo.streambox.dialog.Toasty;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.SharedPref;

public class SettingGeneralActivity extends AppCompatActivity {

    private SharedPref sharedPref;
    private TextView tv_recently_movie, tv_recently_live, tv_auto_update;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (Boolean.TRUE.equals(Callback.isLandscape)){
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        }
        IfSupported.IsRTL(this);
        IfSupported.IsScreenshot(this);
        IfSupported.hideStatusBar(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        Boolean isTvBox  = ApplicationUtil.isTvBox(this);

        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (Boolean.TRUE.equals(isTvBox)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        sharedPref = new SharedPref(this);

        tv_recently_movie = findViewById(R.id.tv_add_recently_movie);
        tv_recently_live = findViewById(R.id.tv_add_recently_live);
        tv_auto_update = findViewById(R.id.tv_auto_update);

        CheckBox autoplay = findViewById(R.id.cbox_autoplay_episode);
        autoplay.setChecked(sharedPref.getIsAutoplayEpisode());

        CheckBox splash_audio = findViewById(R.id.cbox_splash_audio);
        splash_audio.setChecked(sharedPref.getIsSplashAudio());

        EditText et_agent = findViewById(R.id.et_agent);
        et_agent.setText(sharedPref.getAgentName());

        findViewById(R.id.ll_recently_movie).setOnClickListener(v -> new LimitDialog(this, "movie", limit -> {
            sharedPref.setMovieLimit(limit);
            getRecently();
        }));
        findViewById(R.id.ll_recently_live).setOnClickListener(v -> new LimitDialog(this, "live", limit -> {
            sharedPref.setLiveLimit(limit);
            getRecently();
        }));
        findViewById(R.id.ll_auto_update).setOnClickListener(v -> new HoursDialog(this, hours -> {
            sharedPref.setAutoUpdate(hours);
            getRecently();
        }));

        findViewById(R.id.ll_btn_save).setOnClickListener(v -> {
            sharedPref.setAgentName(et_agent.getText().toString());
            if (!sharedPref.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
                sharedPref.setIsAutoplayEpisode(autoplay.isChecked());
            }
            sharedPref.setIsAudio(splash_audio.isChecked());
            findViewById(R.id.tv_save).setVisibility(View.GONE);
            findViewById(R.id.pb_save).setVisibility(View.VISIBLE);
            new Handler().postDelayed(() -> {
                findViewById(R.id.tv_save).setVisibility(View.VISIBLE);
                findViewById(R.id.pb_save).setVisibility(View.GONE);
                Toasty.makeText(SettingGeneralActivity.this, "Save Data", Toasty.SUCCESS);
            }, 500);
        });

        getRecently();

        if (sharedPref.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
            findViewById(R.id.ll_recently_movie).setVisibility(View.GONE);
            findViewById(R.id.ll_recently_live).setVisibility(View.GONE);
            findViewById(R.id.ll_auto_update).setVisibility(View.GONE);
            autoplay.setVisibility(View.GONE);
        }

        if (Boolean.TRUE.equals(isTvBox)){
            findViewById(R.id.cbox_autoplay_episode).requestFocus();
        }
    }

    @SuppressLint("SetTextI18n")
    private void getRecently() {
        tv_recently_movie.setText(String.valueOf(sharedPref.getMovieLimit()));
        tv_recently_live.setText(String.valueOf(sharedPref.getLiveLimit()));
        tv_auto_update.setText(String.valueOf(sharedPref.getAutoUpdate())+" Hours");
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_setting_general;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
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