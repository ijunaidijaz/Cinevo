package mycinevo.streambox.activity;

import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.view.KeyEvent;
import android.view.View;
import android.widget.CheckBox;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;

import mycinevo.streambox.R;
import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.dialog.DialogUtil;
import mycinevo.streambox.dialog.Toasty;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.helper.SPHelper;

public class SettingMultiScreenActivity extends AppCompatActivity {

    private SPHelper spHelper;
    private ImageView iv_screen;

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

        findViewById(R.id.iv_back_page).setOnClickListener(view -> onBackPressed());
        if (Boolean.TRUE.equals(isTvBox)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        spHelper = new SPHelper(this);

        iv_screen = findViewById(R.id.iv_screen);

        CheckBox cbox_screen = findViewById(R.id.cbox_screen);
        cbox_screen.setChecked(spHelper.getIsScreen());

        findViewById(R.id.btn_select).setOnClickListener(v -> DialogUtil.ScreenDialog(SettingMultiScreenActivity.this, new DialogUtil.ScreenDialogListener() {
            @Override
            public void onSubmit(int screen) {
                spHelper.setScreen(screen);
                findViewById(R.id.tv_select).setVisibility(View.GONE);
                findViewById(R.id.pb_select).setVisibility(View.VISIBLE);
                new Handler().postDelayed(() -> {
                    findViewById(R.id.tv_select).setVisibility(View.VISIBLE);
                    findViewById(R.id.pb_select).setVisibility(View.GONE);
                    setScreen();
                }, 500);
            }

            @Override
            public void onCancel() {
                findViewById(R.id.tv_select).setVisibility(View.VISIBLE);
                findViewById(R.id.pb_select).setVisibility(View.GONE);
                setScreen();
            }
        }));

        findViewById(R.id.ll_btn_save).setOnClickListener(v -> {
            spHelper.setIsScreen(cbox_screen.isChecked());
            findViewById(R.id.tv_save).setVisibility(View.GONE);
            findViewById(R.id.pb_save).setVisibility(View.VISIBLE);
            new Handler().postDelayed(() -> {
                findViewById(R.id.tv_save).setVisibility(View.VISIBLE);
                findViewById(R.id.pb_save).setVisibility(View.GONE);
                Toasty.makeText(SettingMultiScreenActivity.this, "Save Data", Toasty.SUCCESS);
            }, 500);
        });

        setScreen();

        if (Boolean.TRUE.equals(isTvBox)){
            cbox_screen.requestFocus();
        }
    }

    private void setScreen() {
        int screen = spHelper.getScreen();
        if (screen == 1){
            iv_screen.setImageResource(R.drawable.screen_one);
        } else if (screen == 2){
            iv_screen.setImageResource(R.drawable.screen_two);
        } else if (screen == 3){
            iv_screen.setImageResource(R.drawable.screen_three);
        } else if (screen == 4){
            iv_screen.setImageResource(R.drawable.screen_four);
        } else if (screen == 5){
            iv_screen.setImageResource(R.drawable.screen_five);
        }
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_setting_multi_screen;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN) {
            if (keyCode == KeyEvent.KEYCODE_BACK){
                onBackPressed();
                return true;
            } else if (keyCode == KeyEvent.KEYCODE_HOME){
                ApplicationUtil.openHomeActivity(this);
                return true;
            }
        }
        return super.onKeyDown(keyCode, event);
    }

}