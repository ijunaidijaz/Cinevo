package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.pm.ActivityInfo;
import android.graphics.Color;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.webkit.WebView;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompatActivity;

import nemosofts.streambox.R;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.SPHelper;

public class NotificationsWebActivity extends AppCompatActivity {

    @SuppressLint("SetJavaScriptEnabled")
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

        WebView audio_lyrics = findViewById(R.id.web);
        audio_lyrics.getSettings().setJavaScriptEnabled(true);

        String htmlString = getHtmlString();
        audio_lyrics.setScrollbarFadingEnabled(true);
        audio_lyrics.setBackgroundColor(Color.TRANSPARENT);
        audio_lyrics.loadDataWithBaseURL("", htmlString, "text/html", "utf-8", null);

        if (ApplicationUtil.isTvBox(this)){
            audio_lyrics.requestFocus();
        }
    }

    @NonNull
    private String getHtmlString() {
        String htmlText = "";
        try {
            htmlText = Callback.arrayListNotify.get(Callback.posNotify).getDescription();
        } catch (Exception e) {
            e.printStackTrace();
        }

        String textSize = "body{font-size:15px;}";

        String myCustomStyleString = "<style> body{color:#fff !important;}</style>"
                + "<style type=\"text/css\">"+ textSize + "</style>";
        String htmlString;
        if(Boolean.FALSE.equals(new SPHelper(this).getIsRTL())) {
            htmlString = myCustomStyleString + "<div>" + htmlText + "</div>";
        } else {
            htmlString = "<html dir=\"rtl\" lang=\"\"><body>" + myCustomStyleString + "<div>" + htmlText + "</div>" + "</body></html>";
        }
        return htmlString;
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_notifications_web;
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