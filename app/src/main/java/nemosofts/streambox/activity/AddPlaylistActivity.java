package nemosofts.streambox.activity;

import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.READ_MEDIA_AUDIO;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.text.TextUtils;
import android.view.KeyEvent;
import android.view.View;
import android.widget.EditText;
import android.widget.RadioGroup;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.activity.UI.PlaylistActivity;
import nemosofts.streambox.asyncTask.LoadPlaylist;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.dialog.Toasty;
import nemosofts.streambox.interfaces.LoadPlaylistListener;
import nemosofts.streambox.item.ItemPlaylist;
import nemosofts.streambox.item.ItemUsersDB;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.util.helper.SPHelper;
import nemosofts.streambox.view.NSoftsProgressDialog;

public class AddPlaylistActivity extends AppCompatActivity {

    private SPHelper spHelper;
    private DBHelper dbHelper;
    private JSHelper jsHelper;
    private EditText et_any_name;
    private EditText et_url;
    private Boolean isFile = true;
    private String filePath = "";
    private TextView btn_browse;
    private TextView tv_browse;
    private static final int PICK_REQUEST = 1;
    static final int MY_PERMISSIONS_REQUEST_WRITE_EXTERNAL_STORAGE = 102;
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

        progressDialog = new NSoftsProgressDialog(AddPlaylistActivity.this);

        jsHelper = new JSHelper(this);
        dbHelper = new DBHelper(this);
        spHelper = new SPHelper(this);

        et_any_name = findViewById(R.id.et_any_name);
        et_url = findViewById(R.id.et_url);
        btn_browse = findViewById(R.id.btn_browse);
        tv_browse = findViewById(R.id.tv_browse);

        RadioGroup rg =  findViewById(R.id.rg);
        rg.check(R.id.rd_1);
        isFile = true;

        findViewById(R.id.rd_1).setOnClickListener(view -> {
            isFile = true;
            findViewById(R.id.ll_browse).setVisibility(View.VISIBLE);
            findViewById(R.id.ll_url).setVisibility(View.GONE);
        });
        findViewById(R.id.rd_2).setOnClickListener(view -> {
            isFile = false;
            findViewById(R.id.ll_browse).setVisibility(View.GONE);
            findViewById(R.id.ll_url).setVisibility(View.VISIBLE);
        });
        findViewById(R.id.ll_btn_add).setOnClickListener(v -> attemptLogin());
        findViewById(R.id.rl_list_users).setOnClickListener(view -> {
            Intent intent = new Intent(AddPlaylistActivity.this, UsersListActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "");
            startActivity(intent);
            finish();
        });

        btn_browse.setOnClickListener(view -> {
            if (checkPer()) {
                btn_browse.setBackgroundResource(R.drawable.focused_btn_primary);
                pickPlaylist();
            }
        });

        if (ApplicationUtil.isTvBox(this)){
            et_any_name.requestFocus();
        }
    }

    private void openPlaylistActivity() {
        Toast.makeText(AddPlaylistActivity.this, "Login successfully.", Toast.LENGTH_SHORT).show();
        spHelper.setLoginType(Callback.TAG_LOGIN_PLAYLIST);
        spHelper.setAnyName(et_any_name.getText().toString());
        Intent intent = new Intent(AddPlaylistActivity.this, PlaylistActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        startActivity(intent);
        finish();
    }

    private void pickPlaylist() {
        tv_browse.setText("");
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setType("audio/*");
        intent.putExtra(Intent.EXTRA_MIME_TYPES, ApplicationUtil.SUPPORTED_EXTENSIONS_PLAYLIST);
        intent.addCategory(Intent.CATEGORY_OPENABLE);
        try {
            startActivityForResult(Intent.createChooser(intent, getResources().getString(R.string.select_playlist)), PICK_REQUEST);
        } catch (android.content.ActivityNotFoundException ex) {
            Toasty.makeText(AddPlaylistActivity.this, "Please install a File Manager or a File Explorer app.", Toasty.ERROR);
        }
    }

    private void attemptLogin() {
        et_any_name.setError(null);
        String anyName = et_any_name.getText().toString();

        boolean cancel = false;
        View focusView = null;

        if (TextUtils.isEmpty(anyName)) {
            et_any_name.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_cannot_empty)));
            focusView = et_any_name;
            cancel = true;
        }

        if (Boolean.TRUE.equals(isFile)){
            if (filePath == null){
                Toasty.makeText(AddPlaylistActivity.this, getString(R.string.err_file_invalid), Toasty.ERROR);
                cancel = true;
            }
        } else {
            et_url.setError(null);
            String urlData = et_url.getText().toString();
            if (TextUtils.isEmpty(urlData)) {
                et_url.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_cannot_empty)));
                focusView = et_url;
                cancel = true;
            }
        }

        if (cancel) {
            if (focusView != null) {
                focusView.requestFocus();
            }
        } else {
            if (Boolean.TRUE.equals(isFile)){
                loadPlaylistData();
            } else {
                if (NetworkUtils.isConnected(this)){
                    loadPlaylistData();
                } else {
                    Toasty.makeText(AddPlaylistActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
                }
            }
        }
    }

    private void loadPlaylistData() {
        String finalUrl = isFile ? filePath : et_url.getText().toString().trim();
        LoadPlaylist playlist = new LoadPlaylist(this, isFile, finalUrl, new LoadPlaylistListener() {
            @Override
            public void onStart() {
                setEnabled(false);
                progressDialog.show();
            }

            @Override
            public void onEnd(String success, String msg , ArrayList<ItemPlaylist> arrayListPlaylist) {
                progressDialog.dismiss();
                if (!isFinishing()){
                    if (success.equals("1")) {
                        if (arrayListPlaylist.isEmpty()){
                            setEnabled(true);
                            Toasty.makeText(AddPlaylistActivity.this, getString(R.string.err_no_data_found), Toasty.ERROR);
                        } else {
                            jsHelper.addToPlaylistData(arrayListPlaylist);
                            if (Boolean.FALSE.equals(isFile)){
                                dbHelper.addToUserDB(new ItemUsersDB("", et_any_name.getText().toString(), et_any_name.getText().toString(),
                                        et_any_name.getText().toString(), et_url.getText().toString(),"playlist")
                                );
                            }
                            Toast.makeText(AddPlaylistActivity.this, "Add successfully.", Toast.LENGTH_SHORT).show();
                            openPlaylistActivity();
                        }
                    }  else {
                        setEnabled(true);
                        Toasty.makeText(AddPlaylistActivity.this, msg, Toasty.ERROR);
                    }
                }
            }
        });
        playlist.execute();
    }

    private void setEnabled(boolean is_enabled) {
        if (is_enabled){
            findViewById(R.id.iv_add).setVisibility(View.VISIBLE);
            findViewById(R.id.pb_add).setVisibility(View.GONE);
            if (ApplicationUtil.isTvBox(this)){
                findViewById(R.id.ll_btn_add).requestFocus();
            }
        } else {
            findViewById(R.id.iv_add).setVisibility(View.GONE);
            findViewById(R.id.pb_add).setVisibility(View.VISIBLE);
        }
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_add_playlist;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
    }

    @NonNull
    private Boolean checkPer() {
        if (Build.VERSION.SDK_INT >= 33){
            if ((ContextCompat.checkSelfPermission(AddPlaylistActivity.this, READ_MEDIA_AUDIO) != PackageManager.PERMISSION_GRANTED)) {
                requestPermissions(new String[]{READ_MEDIA_AUDIO}, MY_PERMISSIONS_REQUEST_WRITE_EXTERNAL_STORAGE);
                return false;
            } else {
                return true;
            }
        } if (Build.VERSION.SDK_INT >= 29) {
            if ((ContextCompat.checkSelfPermission(AddPlaylistActivity.this, READ_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED)) {
                requestPermissions(new String[]{READ_EXTERNAL_STORAGE}, MY_PERMISSIONS_REQUEST_WRITE_EXTERNAL_STORAGE);
                return false;
            } else {
                return true;
            }
        } else {
            if (ContextCompat.checkSelfPermission(AddPlaylistActivity.this, WRITE_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED) {
                requestPermissions(new String[]{WRITE_EXTERNAL_STORAGE}, MY_PERMISSIONS_REQUEST_WRITE_EXTERNAL_STORAGE);
                return false;
            }
            return true;
        }
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        boolean canUseExternalStorage = false;
        if (requestCode == MY_PERMISSIONS_REQUEST_WRITE_EXTERNAL_STORAGE) {
            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                canUseExternalStorage = true;
            }
            if (!canUseExternalStorage) {
                Toast.makeText(AddPlaylistActivity.this, getResources().getString(R.string.err_cannot_use_features), Toast.LENGTH_SHORT).show();
            }
        }
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        if (requestCode == PICK_REQUEST && resultCode == Activity.RESULT_OK && data != null && data.getData() != null) {
            try {
                Uri uri = data.getData();
                String pathAudio = ApplicationUtil.getPathAudio(AddPlaylistActivity.this, uri);
                if (pathAudio != null && pathAudio.contains(".m3u")){
                    filePath = String.valueOf(uri);
                    tv_browse.setText(ApplicationUtil.getPathAudio(AddPlaylistActivity.this, uri));
                    btn_browse.setBackgroundResource(R.drawable.focused_btn_success);
                    new Handler().postDelayed(() -> Toasty.makeText(AddPlaylistActivity.this, getString(R.string.added_success), Toasty.SUCCESS), 0);
                } else {
                    errorData();
                }
            } catch (Exception e) {
                errorData();
                e.printStackTrace();
            }
        }
    }

    private void errorData() {
        filePath = "";
        tv_browse.setText("");
        btn_browse.setBackgroundResource(R.drawable.focused_btn_danger);
        new Handler().postDelayed(() -> Toasty.makeText(AddPlaylistActivity.this, getString(R.string.err_file_invalid), Toasty.ERROR), 0);
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)){
            DialogUtil.ExitDialog(AddPlaylistActivity.this);
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }

    @SuppressLint("MissingSuperCall")
    @Override
    public void onBackPressed() {
        DialogUtil.ExitDialog(AddPlaylistActivity.this);
    }
}