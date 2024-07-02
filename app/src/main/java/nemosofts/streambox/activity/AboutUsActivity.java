package nemosofts.streambox.activity;

import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;

import nemosofts.streambox.BuildConfig;
import nemosofts.streambox.R;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.SPHelper;


public class AboutUsActivity extends AppCompatActivity {

    private SPHelper spHelper;
    private TextView tv_author, tv_email, tv_website, tv_contact, tv_description, tv_version;

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

        spHelper = new SPHelper(this);

        tv_author = findViewById(R.id.tv_company);
        tv_email = findViewById(R.id.tv_email);
        tv_website = findViewById(R.id.tv_website);
        tv_contact = findViewById(R.id.tv_contact);
        tv_description = findViewById(R.id.tv_app_des);
        tv_version = findViewById(R.id.tv_version);

        setAboutUs();
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_about_us;
    }

    private void setAboutUs() {
        tv_author.setText(!spHelper.getAppAuthor().trim().isEmpty() ? spHelper.getAppAuthor() : "");
        tv_email.setText(!spHelper.getAppEmail().trim().isEmpty() ? spHelper.getAppEmail() : "");
        tv_website.setText(!spHelper.getAppWebsite().trim().isEmpty() ? spHelper.getAppWebsite() : "");
        tv_contact.setText(!spHelper.getAppContact().trim().isEmpty() ? spHelper.getAppContact() : "");
        tv_description.setText(!spHelper.getAppDescription().trim().isEmpty() ? spHelper.getAppDescription() : "");
        tv_version.setText(BuildConfig.VERSION_NAME);
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