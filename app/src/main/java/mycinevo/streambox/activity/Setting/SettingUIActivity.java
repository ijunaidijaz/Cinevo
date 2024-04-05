package mycinevo.streambox.activity.Setting;

import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.view.View;
import android.widget.CheckBox;

import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;

import mycinevo.streambox.R;
import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.dialog.Toasty;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.SharedPref;

public class SettingUIActivity extends AppCompatActivity {

    private SharedPref sharedPref;

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

        CheckBox shimmer_home = findViewById(R.id.cbox_shimmering_home);
        CheckBox shimmer_details = findViewById(R.id.cbox_shimmering_details);
        CheckBox shimmer_splash = findViewById(R.id.cbox_shimmering_splash);

        shimmer_home.setChecked(sharedPref.getIsShimmeringHome());
        shimmer_details.setChecked(sharedPref.getIsShimmeringDetails());
        shimmer_splash.setChecked(sharedPref.getIsShimmeringSplash());

        CheckBox card_title = findViewById(R.id.cbox_card_title);
        card_title.setChecked(sharedPref.getUICardTitle());

        CheckBox cast = findViewById(R.id.cbox_cast);
        cast.setChecked(sharedPref.getIsCast());

        CheckBox download = findViewById(R.id.cbox_download);
        download.setChecked(sharedPref.getIsDownloadUser());
        if (!sharedPref.getIsDownload()){
            download.setVisibility(View.GONE);
        }

        CheckBox player_subtitle = findViewById(R.id.cbox_subtitle);
        CheckBox player_vr = findViewById(R.id.cbox_vr);

        player_subtitle.setChecked(sharedPref.getIsSubtitle());
        player_vr.setChecked(sharedPref.getIsVR());

        findViewById(R.id.ll_btn_save).setOnClickListener(v -> {
            findViewById(R.id.tv_save).setVisibility(View.GONE);
            findViewById(R.id.pb_save).setVisibility(View.VISIBLE);
            sharedPref.setIsUI(card_title.isChecked(), download.isChecked(), cast.isChecked());
            sharedPref.setIsShimmering(shimmer_home.isChecked(), shimmer_details.isChecked(), shimmer_splash.isChecked());
            sharedPref.setIsPlayerUI(player_subtitle.isChecked(),player_vr.isChecked());
            Callback.isDataUpdate = true;
            new Handler().postDelayed(() -> {
                findViewById(R.id.tv_save).setVisibility(View.VISIBLE);
                findViewById(R.id.pb_save).setVisibility(View.GONE);
                Toasty.makeText(SettingUIActivity.this, "Save Data", Toasty.SUCCESS);
            }, 500);
        });

        if (sharedPref.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
            shimmer_details.setVisibility(View.GONE);
        }

        if (Boolean.TRUE.equals(isTvBox)){
            findViewById(R.id.cbox_card_title).requestFocus();
        }
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_setting_ui;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
    }
}