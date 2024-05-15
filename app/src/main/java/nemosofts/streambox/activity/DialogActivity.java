package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.view.KeyEvent;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;

import java.util.Objects;

import nemosofts.streambox.R;
import nemosofts.streambox.activity.UI.SingleStreamActivity;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.SPHelper;

public class DialogActivity extends AppCompatActivity {

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

        String from = getIntent().getStringExtra("from");
        switch (Objects.requireNonNull(from)) {
            case Callback.DIALOG_TYPE_UPDATE -> DialogUtil.UpgradeDialog(this, this::openMainActivity);
            case Callback.DIALOG_TYPE_MAINTENANCE -> DialogUtil.MaintenanceDialog(this);
            case Callback.DIALOG_TYPE_DEVELOPER -> DialogUtil.DModeDialog(this);
            case Callback.DIALOG_TYPE_VPN -> DialogUtil.VpnDialog(this);
            default -> openMainActivity();
        }
    }

    private void openMainActivity() {
        SPHelper spHelper = new SPHelper(this);
        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_SINGLE_STREAM)){
            new Handler().postDelayed(this::openSingleStream, 2000);
        } else if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_ONE_UI) || spHelper.getLoginType().equals(Callback.TAG_LOGIN_STREAM)){
            if (Boolean.TRUE.equals(spHelper.getIsFirst())) {
                new Handler().postDelayed(this::openSelectPlayer, 2000);
            } else {
                if (Boolean.FALSE.equals(spHelper.getIsAutoLogin())) {
                    new Handler().postDelayed(this::openSelectPlayer, 2000);
                } else {
                    ApplicationUtil.openThemeActivity(DialogActivity.this);
                }
            }
        } else {
            new Handler().postDelayed(this::openSelectPlayer, 2000);
        }
    }

    @SuppressLint("UnsafeOptInUsageError")
    private void openSelectPlayer() {
        Intent intent = new Intent(DialogActivity.this, SelectPlayerActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        intent.putExtra("from", "");
        startActivity(intent);
        finish();
    }

    @SuppressLint("UnsafeOptInUsageError")
    private void openSingleStream() {
        Intent intent = new Intent(DialogActivity.this, SingleStreamActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        startActivity(intent);
        finish();
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_splash;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)) {
            finish();
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }
}