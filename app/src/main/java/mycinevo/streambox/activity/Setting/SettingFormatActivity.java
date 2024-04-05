package mycinevo.streambox.activity.Setting;

import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.view.KeyEvent;
import android.view.View;
import android.widget.RadioGroup;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;

import mycinevo.streambox.R;
import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.dialog.Toasty;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.SharedPref;

public class SettingFormatActivity extends AppCompatActivity {

    int format = 0;

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

        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        SharedPref sharedPref = new SharedPref(this);

        RadioGroup rg = findViewById(R.id.rg);
        format = sharedPref.getLiveFormat();
        if (format == 1){
            rg.check(R.id.rd_2);
        } else if (format == 2){
            rg.check(R.id.rd_3);
        } else {
            rg.check(R.id.rd_1);
        }

        findViewById(R.id.rd_1).setOnClickListener(view -> format = 0);
        findViewById(R.id.rd_2).setOnClickListener(view -> format = 1);
        findViewById(R.id.rd_3).setOnClickListener(view -> format = 2);
        findViewById(R.id.ll_btn_save).setOnClickListener(v -> {
            sharedPref.setLiveFormat(format);
            findViewById(R.id.tv_save).setVisibility(View.GONE);
            findViewById(R.id.pb_save).setVisibility(View.VISIBLE);
            new Handler().postDelayed(() -> {
                findViewById(R.id.tv_save).setVisibility(View.VISIBLE);
                findViewById(R.id.pb_save).setVisibility(View.GONE);
                Toasty.makeText(SettingFormatActivity.this, "Save Data", Toasty.SUCCESS);
            }, 500);
        });
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_setting_format;
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