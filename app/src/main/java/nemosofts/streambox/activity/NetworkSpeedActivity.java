package mycinevo.streambox.activity;

import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;

import mycinevo.streambox.R;
import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.dialog.Toasty;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.NetworkUtils;
import mycinevo.streambox.util.SpeedTest;

public class NetworkSpeedActivity extends AppCompatActivity {

    private LinearLayout ll_btn_speed;
    private TextView tv_time;

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

        tv_time = findViewById(R.id.tv_time);
        ll_btn_speed = findViewById(R.id.ll_btn_speed);

        ll_btn_speed.setOnClickListener(v -> runSpeedTest());

        if (ApplicationUtil.isTvBox(this)){
            ll_btn_speed.requestFocus();
        }
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_network_speed;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
    }

    private void runSpeedTest() {
        if (NetworkUtils.isConnected(this)){
            SpeedTest speedTest = new SpeedTest(new SpeedTest.OnSpeedCheckListener() {
                @Override
                public void onStart() {
                    tv_time.setText("");
                    tv_time.setVisibility(View.GONE);
                    findViewById(R.id.tv_speed).setVisibility(View.GONE);
                    findViewById(R.id.pb_speed).setVisibility(View.VISIBLE);
                }

                @Override
                public void onEnd(String success, String speedMbps) {
                    if (!isFinishing()){
                        if (success.equals("1")){
                            tv_time.setText(speedMbps);
                            tv_time.setVisibility(View.VISIBLE);
                            ll_btn_speed.setVisibility(View.GONE);
                        } else {
                            tv_time.setVisibility(View.GONE);

                            findViewById(R.id.tv_speed).setVisibility(View.VISIBLE);
                            findViewById(R.id.pb_speed).setVisibility(View.GONE);
                            ll_btn_speed.setVisibility(View.VISIBLE);
                            Toasty.makeText(NetworkSpeedActivity.this, getString(R.string.err_server_not_connected), Toasty.ERROR);
                        }
                    }
                }
            });
            speedTest.execute();
        } else {
            Toasty.makeText(NetworkSpeedActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
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