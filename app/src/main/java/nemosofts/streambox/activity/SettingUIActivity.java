package nemosofts.streambox.activity;

import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.view.View;
import android.widget.CheckBox;
import android.widget.TextView;

import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.theme.ColorUtils;
import androidx.nemosofts.theme.ThemeEngine;

import nemosofts.streambox.R;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.Toasty;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.SPHelper;

public class SettingUIActivity extends AppCompatActivity {

    private ThemeEngine themeEngine;
    private SPHelper spHelper;
    private TextView tv_classic, tv_dark_grey, tv_dark, tv_dark_blue;

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
        themeEngine = new ThemeEngine(this);

        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (Boolean.TRUE.equals(isTvBox)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        spHelper = new SPHelper(this);

        CheckBox shimmer_home = findViewById(R.id.cbox_shimmering_home);
        CheckBox shimmer_details = findViewById(R.id.cbox_shimmering_details);

        shimmer_home.setChecked(spHelper.getIsShimmeringHome());
        shimmer_details.setChecked(spHelper.getIsShimmeringDetails());

        CheckBox card_title = findViewById(R.id.cbox_card_title);
        card_title.setChecked(spHelper.getUICardTitle());

        CheckBox cast = findViewById(R.id.cbox_cast);
        cast.setChecked(spHelper.getIsCast());

        CheckBox download = findViewById(R.id.cbox_download);
        download.setChecked(spHelper.getIsDownloadUser());
        if (!spHelper.getIsDownload()){
            download.setVisibility(View.GONE);
        }

        CheckBox player_subtitle = findViewById(R.id.cbox_subtitle);
        CheckBox player_vr = findViewById(R.id.cbox_vr);

        player_subtitle.setChecked(spHelper.getIsSubtitle());
        player_vr.setChecked(spHelper.getIsVR());

        findViewById(R.id.ll_btn_save).setOnClickListener(v -> {
            findViewById(R.id.tv_save).setVisibility(View.GONE);
            findViewById(R.id.pb_save).setVisibility(View.VISIBLE);
            spHelper.setIsUI(card_title.isChecked(), download.isChecked(), cast.isChecked());
            spHelper.setIsShimmering(shimmer_home.isChecked(), shimmer_details.isChecked());
            spHelper.setIsPlayerUI(player_subtitle.isChecked(),player_vr.isChecked());
            Callback.isDataUpdate = true;
            new Handler().postDelayed(() -> {
                findViewById(R.id.tv_save).setVisibility(View.VISIBLE);
                findViewById(R.id.pb_save).setVisibility(View.GONE);
                Toasty.makeText(SettingUIActivity.this, "Save Data", Toasty.SUCCESS);
            }, 500);
        });

        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
            shimmer_details.setVisibility(View.GONE);
        }

        int theme = spHelper.getIsTheme();
        if (theme == 1 || theme == 4){
            findViewById(R.id.ll_theme).setVisibility(View.VISIBLE);
            findViewById(R.id.ll_theme).requestFocus();
        } else {
            findViewById(R.id.ll_theme).setVisibility(View.GONE);
            findViewById(R.id.cbox_card_title).requestFocus();
        }

        tv_classic = findViewById(R.id.tv_classic);
        tv_dark_grey = findViewById(R.id.tv_dark_grey);
        tv_dark = findViewById(R.id.tv_dark);
        tv_dark_blue = findViewById(R.id.tv_dark_blue);

        tv_classic.setOnClickListener(view -> {
            if (themeEngine.getThemePage() != 0){
                setThemeMode(false, 0);
            }
        });
        tv_dark_grey.setOnClickListener(view -> {
            if (themeEngine.getThemePage() != 2){
                setThemeMode(true, 2);
            }
        });
        tv_dark_blue.setOnClickListener(view -> {
            if (themeEngine.getThemePage() != 3){
                setThemeMode(true, 3);
            }
        });
        tv_dark.setOnClickListener(view -> {
            if (themeEngine.getThemePage() != 1){
                setThemeMode(true, 1);
            }
        });

        getThemeData();
    }

    private void setThemeMode(Boolean isChecked, int isTheme) {
        themeEngine.setThemeMode(isChecked);
        themeEngine.setThemePage(isTheme);
        Callback.is_recreate_ui = true;
        recreate();
    }

    private void getThemeData() {
        int theme = themeEngine.getThemePage();
        if (theme == 0){
            tv_classic.setBackgroundResource(R.drawable.focused_btn_accent);
            tv_dark_grey.setBackgroundResource(R.drawable.focused_save_btn);
            tv_dark_blue.setBackgroundResource(R.drawable.focused_save_btn);
            tv_dark.setBackgroundResource(R.drawable.focused_save_btn);

            tv_classic.setTextColor(ColorUtils.colorWhite(this));
            tv_dark_grey.setTextColor(ColorUtils.colorTitle(this));
            tv_dark_blue.setTextColor(ColorUtils.colorTitle(this));
            tv_dark.setTextColor(ColorUtils.colorTitle(this));

        } else if (theme == 1){
            tv_classic.setBackgroundResource(R.drawable.focused_save_btn);
            tv_dark_grey.setBackgroundResource(R.drawable.focused_save_btn);
            tv_dark_blue.setBackgroundResource(R.drawable.focused_save_btn);
            tv_dark.setBackgroundResource(R.drawable.focused_btn_accent);

            tv_classic.setTextColor(ColorUtils.colorTitle(this));
            tv_dark_grey.setTextColor(ColorUtils.colorTitle(this));
            tv_dark_blue.setTextColor(ColorUtils.colorTitle(this));
            tv_dark.setTextColor(ColorUtils.colorWhite(this));

        } else if (theme == 2){
            tv_classic.setBackgroundResource(R.drawable.focused_save_btn);
            tv_dark_grey.setBackgroundResource(R.drawable.focused_btn_accent);
            tv_dark_blue.setBackgroundResource(R.drawable.focused_save_btn);
            tv_dark.setBackgroundResource(R.drawable.focused_save_btn);

            tv_classic.setTextColor(ColorUtils.colorTitle(this));
            tv_dark_grey.setTextColor(ColorUtils.colorWhite(this));
            tv_dark_blue.setTextColor(ColorUtils.colorTitle(this));
            tv_dark.setTextColor(ColorUtils.colorTitle(this));

        } else if (theme == 3){
            tv_classic.setBackgroundResource(R.drawable.focused_save_btn);
            tv_dark_grey.setBackgroundResource(R.drawable.focused_save_btn);
            tv_dark_blue.setBackgroundResource(R.drawable.focused_btn_accent);
            tv_dark.setBackgroundResource(R.drawable.focused_save_btn);

            tv_classic.setTextColor(ColorUtils.colorTitle(this));
            tv_dark_grey.setTextColor(ColorUtils.colorTitle(this));
            tv_dark_blue.setTextColor(ColorUtils.colorWhite(this));
            tv_dark.setTextColor(ColorUtils.colorTitle(this));

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