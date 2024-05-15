package nemosofts.streambox.activity;

import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.view.KeyEvent;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.activity.UI.PlaylistActivity;
import nemosofts.streambox.asyncTask.LoadLogin;
import nemosofts.streambox.asyncTask.LoadPlaylist;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.Toasty;
import nemosofts.streambox.interfaces.LoadPlaylistListener;
import nemosofts.streambox.interfaces.LoginListener;
import nemosofts.streambox.item.ItemPlaylist;
import nemosofts.streambox.item.ItemUsersDB;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.SPHelper;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.view.NSoftsProgressDialog;

public class GetActivity extends AppCompatActivity {

    private Helper helper;
    private DBHelper dbHelper;
    private SPHelper spHelper;
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

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        helper = new Helper(this);
        spHelper = new SPHelper(this);
        dbHelper = new DBHelper(this);

        progressDialog = new NSoftsProgressDialog(GetActivity.this);

        Intent intent = getIntent();
        if (Intent.ACTION_SEARCH.equals(intent.getAction())) {
            String login_type = intent.getStringExtra("login_type");
            if (login_type != null){
                if (login_type.equals("xtream") || login_type.equals("stream")){
                    String any_name = intent.getStringExtra("any_name");
                    String user_name = intent.getStringExtra("user_name");
                    String user_pass = intent.getStringExtra("user_pass");
                    String dms_url = intent.getStringExtra("dms_url");
                    loadLogin(any_name, user_name, user_pass, dms_url, login_type);
                } else if (login_type.equals("playlist")){
                    String any_name = intent.getStringExtra("any_name");
                    String dms_url = intent.getStringExtra("dms_url");
                    loadLoginPlaylist(any_name, dms_url);
                } else {
                    Toasty.makeText(GetActivity.this, getString(R.string.err_no_data_found), Toasty.ERROR);
                }
            }
            try {
                intent.removeExtra("login_type");
            } catch (Exception e) {
                e.printStackTrace();
            }
            try {
                intent.removeExtra("any_name");
            } catch (Exception e) {
                e.printStackTrace();
            }
            try {
                intent.removeExtra("user_name");
            } catch (Exception e) {
                e.printStackTrace();
            }
            try {
                intent.removeExtra("user_pass");
            } catch (Exception e) {
                e.printStackTrace();
            }
            try {
                intent.removeExtra("dms_url");
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private void loadLoginPlaylist(String anyName, String userURL) {
        if (NetworkUtils.isConnected(this)){
            LoadPlaylist playlist = new LoadPlaylist(this,false, userURL, new LoadPlaylistListener() {
                @Override
                public void onStart() {
                    progressDialog.show();
                    if (spHelper.isLogged()) {
                        new JSHelper(GetActivity.this).removeAllData();
                        dbHelper.removeAllData();
                        spHelper.removeSignOut();
                    }
                }

                @Override
                public void onEnd(String success, ArrayList<ItemPlaylist> arrayListPlaylist) {
                    progressDialog.dismiss();
                    if (!isFinishing()){
                        if (success.equals("1")) {
                            if (arrayListPlaylist.isEmpty()){
                                Toast.makeText(GetActivity.this, getString(R.string.err_no_data_found), Toast.LENGTH_SHORT).show();
                            } else {

                                new JSHelper(GetActivity.this).addToPlaylistData(arrayListPlaylist);

                                Toast.makeText(GetActivity.this, "Login successfully.", Toast.LENGTH_SHORT).show();

                                spHelper.setLoginType(Callback.TAG_LOGIN_PLAYLIST);
                                spHelper.setAnyName(anyName);
                                Intent intent = new Intent(GetActivity.this, PlaylistActivity.class);
                                intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                                startActivity(intent);
                                finish();
                            }
                        }  else {
                            Toasty.makeText(GetActivity.this, getString(R.string.err_server_not_connected), Toasty.ERROR);
                        }
                    }
                }
            });
            playlist.execute();
        }  else {
            Toasty.makeText(GetActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
    }

    private void loadLogin(String any_name, String user_name, String user_pass, String dms_url, String login_type) {
        if (NetworkUtils.isConnected(this)){
            LoadLogin login = new LoadLogin(new LoginListener() {
                @Override
                public void onStart() {
                    progressDialog.show();
                    if (spHelper.isLogged()) {
                        new JSHelper(GetActivity.this).removeAllData();
                        dbHelper.removeAllData();
                        spHelper.removeSignOut();
                    }
                }

                @Override
                public void onEnd(String success, String username, String password, String message, int auth, String status, String exp_date, String is_trial, String active_cons, String created_at, String max_connections, String allowed_output_formats, boolean xui, String version, int revision, String url, String port, String https_port, String server_protocol, String rtmp_port, int timestamp_now, String time_now, String timezone) {
                    progressDialog.dismiss();
                    if (!isFinishing()){
                        if (success.equals("1")) {
                            try {
                                if (Boolean.TRUE.equals(login_type.equals("xtream"))){
                                    dbHelper.addToUserDB(new ItemUsersDB("", any_name, user_name, user_pass, dms_url,"xui")
                                    );
                                    spHelper.setLoginDetails(
                                            username,password,message,auth,status, exp_date, is_trial, active_cons,created_at,max_connections,
                                            xui,version,revision,url,port,https_port,server_protocol,rtmp_port,timestamp_now,time_now,timezone
                                    );
                                    spHelper.setLoginType(Callback.TAG_LOGIN_ONE_UI);
                                } else {
                                    dbHelper.addToUserDB(new ItemUsersDB("", any_name, user_name, user_pass, dms_url,"stream")
                                    );
                                    spHelper.setLoginDetails(
                                            username,password,message,auth,status, exp_date, is_trial, active_cons,created_at,max_connections,
                                            xui,version,revision,url,port,https_port,server_protocol,rtmp_port,timestamp_now,time_now,timezone
                                    );
                                    spHelper.setLoginType(Callback.TAG_LOGIN_STREAM);
                                }
                                if (!allowed_output_formats.isEmpty()){
                                    if (allowed_output_formats.contains("m3u8")){
                                        spHelper.setLiveFormat(2);
                                    } else {
                                        spHelper.setLiveFormat(1);
                                    }
                                } else {
                                    spHelper.setLiveFormat(0);
                                }

                                spHelper.setAnyName(any_name);
                                spHelper.setIsFirst(false);
                                spHelper.setIsLogged(true);
                                spHelper.setIsAutoLogin(true);

                                Callback.isCustomAds = false;
                                Callback.customAdCount = 0;
                                Callback.customAdShow = 15;
                                Callback.is_load_ads = true;

                                Toast.makeText(GetActivity.this, "Login successfully.", Toast.LENGTH_SHORT).show();
                            } catch (Exception e) {
                                e.printStackTrace();
                            }
                            ApplicationUtil.openThemeActivity(GetActivity.this);
                        }  else {
                            Toasty.makeText(GetActivity.this, getString(R.string.err_login_not_incorrect), Toasty.ERROR);
                        }
                    }
                }
            },dms_url, helper.getAPIRequestLogin(user_name, user_pass));
            login.execute();
        }  else {
            Toasty.makeText(GetActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
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