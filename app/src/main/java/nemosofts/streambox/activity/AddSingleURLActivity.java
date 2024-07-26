package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.Handler;
import android.text.TextUtils;
import android.view.KeyEvent;
import android.view.View;
import android.widget.EditText;

import androidx.annotation.NonNull;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;

import nemosofts.streambox.R;
import nemosofts.streambox.activity.ui.SingleStreamActivity;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.dialog.Toasty;
import nemosofts.streambox.item.ItemSingleURL;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.SPHelper;
import nemosofts.streambox.view.NSoftsProgressDialog;

@UnstableApi
public class AddSingleURLActivity extends AppCompatActivity {

    private DBHelper dbHelper;
    private SPHelper spHelper;
    private EditText etAnyName;
    private EditText etUrl;
    private NSoftsProgressDialog progressDialog;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (Boolean.TRUE.equals(Callback.isLandscape)){
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        }
        IfSupported.IsRTL(this);
        IfSupported.IsScreenshot(this);
        IfSupported.hideStatusBar(this);
        IfSupported.keepScreenOn(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        progressDialog = new NSoftsProgressDialog(AddSingleURLActivity.this);

        spHelper = new SPHelper(this);
        dbHelper = new DBHelper(this);

        etAnyName = findViewById(R.id.et_any_name);
        etUrl = findViewById(R.id.et_url);

        findViewById(R.id.ll_btn_add).setOnClickListener(v -> addURL());
        findViewById(R.id.rl_list_single).setOnClickListener(view -> {
            Intent intent = new Intent(AddSingleURLActivity.this, SingleStreamActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            startActivity(intent);
            finish();
        });

        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.rl_list_single).setFocusableInTouchMode(false);
            etAnyName.requestFocus();
        }
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_add_single_url;
    }

    private void addURL() {
        etAnyName.setError(null);
        etUrl.setError(null);

        // Store values at the time of the login attempt.
        String anyName = etAnyName.getText().toString();
        String videoUrl = etUrl.getText().toString();

        boolean cancel = false;
        View focusView = null;

        // Check for a valid any name.
        if (TextUtils.isEmpty(anyName)) {
            etAnyName.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_cannot_empty)));
            focusView = etAnyName;
            cancel = true;
        }

        // Check for a valid url.
        if (TextUtils.isEmpty(videoUrl)) {
            etUrl.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_cannot_empty)));
            focusView = etUrl;
            cancel = true;
        }

        if (cancel) {
            focusView.requestFocus();
        } else {
            playVideo();
        }
    }

    @SuppressLint("StaticFieldLeak")
    private void playVideo() {
        new AsyncTask<String, String, String>() {

            @Override
            protected void onPreExecute() {
                setEnabled(false);
                progressDialog.show();
                super.onPreExecute();
            }

            @Override
            protected String doInBackground(String... strings) {
                try {
                    dbHelper.addToSingleURL(new ItemSingleURL("", etAnyName.getText().toString(), etUrl.getText().toString()));
                    return "1";
                } catch (Exception e) {
                    e.printStackTrace();
                    return "0";
                }
            }

            @Override
            protected void onPostExecute(String s) {
                progressDialog.dismiss();
                if (!isFinishing()){
                    if (s.equals("1")){
                        setData();
                    } else {
                        setEnabled(true);
                        Toasty.makeText(AddSingleURLActivity.this, getString(R.string.err_file_invalid), Toasty.ERROR);
                    }
                }
                super.onPostExecute(s);
            }
        }.execute();
    }

    private void setData() {
        spHelper.setLoginType(Callback.TAG_LOGIN_SINGLE_STREAM);
        new Handler().postDelayed(() -> {
            Intent intent = new Intent(AddSingleURLActivity.this, SingleStreamActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            startActivity(intent);
            finish();
        }, 500);
    }

    private void setEnabled(boolean is_enabled) {
        if (is_enabled){
            new Handler().postDelayed(() -> {
                findViewById(R.id.iv_add).setVisibility(View.VISIBLE);
                findViewById(R.id.pb_add).setVisibility(View.GONE);
                if (ApplicationUtil.isTvBox(this)){
                    findViewById(R.id.ll_btn_add).requestFocus();
                }
            }, 400);
        } else {
            findViewById(R.id.iv_add).setVisibility(View.GONE);
            findViewById(R.id.pb_add).setVisibility(View.VISIBLE);
        }
    }

    @Override
    public void onDestroy() {
        try {
            dbHelper.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
        super.onDestroy();
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)){
            DialogUtil.ExitDialog(AddSingleURLActivity.this);
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }

    @SuppressLint("MissingSuperCall")
    @Override
    public void onBackPressed() {
        DialogUtil.ExitDialog(AddSingleURLActivity.this);
    }
}