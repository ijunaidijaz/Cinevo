package mycinevo.streambox.activity;

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

import mycinevo.streambox.R;
import mycinevo.streambox.activity.UI.SingleStreamActivity;
import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.dialog.DModeDialog;
import mycinevo.streambox.dialog.MaintenanceDialog;
import mycinevo.streambox.dialog.UpgradeDialog;
import mycinevo.streambox.dialog.VpnDialog;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.SharedPref;

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
            case Callback.DIALOG_TYPE_UPDATE ->
                    new UpgradeDialog(this, new UpgradeDialog.UpgradeListener() {
                        @Override
                        public void onCancel() {
                            openMainActivity();
                        }

                        @Override
                        public void onDo() {
                            // document why this method is empty
                        }
                    });
            case Callback.DIALOG_TYPE_MAINTENANCE -> new MaintenanceDialog(this);
            case Callback.DIALOG_TYPE_DEVELOPER -> new DModeDialog(this);
            case Callback.DIALOG_TYPE_VPN -> new VpnDialog(this);
            default -> openMainActivity();
        }
    }

    private void openMainActivity() {
        SharedPref sharedPref = new SharedPref(this);
        if (sharedPref.getLoginType().equals(Callback.TAG_LOGIN_SINGLE_STREAM)){
            new Handler().postDelayed(this::openSingleStream, 2000);
        } else if (sharedPref.getLoginType().equals(Callback.TAG_LOGIN_ONE_UI) || sharedPref.getLoginType().equals(Callback.TAG_LOGIN_STREAM)){
            if (Boolean.TRUE.equals(sharedPref.getIsFirst())) {
                new Handler().postDelayed(this::openSelectPlayer, 2000);
            } else {
                if (Boolean.FALSE.equals(sharedPref.getIsAutoLogin())) {
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