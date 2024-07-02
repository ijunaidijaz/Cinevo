package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.AsyncTask;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.activity.UI.PlaylistActivity;
import nemosofts.streambox.adapter.AdapterUsers;
import nemosofts.streambox.asyncTask.LoadLogin;
import nemosofts.streambox.asyncTask.LoadPlaylist;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
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

public class UsersListActivity extends AppCompatActivity {

    private Helper helper;
    private SPHelper spHelper;
    private RecyclerView rv;
    private ArrayList<ItemUsersDB> arrayList;
    private FrameLayout frameLayout;
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

        progressDialog = new NSoftsProgressDialog(UsersListActivity.this);

        arrayList = new ArrayList<>();

        frameLayout = findViewById(R.id.fl_empty);
        rv = findViewById(R.id.rv);
        GridLayoutManager grid = new GridLayoutManager(this, 2);
        grid.setSpanCount(2);
        rv.setLayoutManager(grid);
        rv.setItemAnimator(new DefaultItemAnimator());
        rv.setHasFixedSize(true);

        getUserData();

        findViewById(R.id.ll_user_add).setOnClickListener(v -> {
            @SuppressLint("UnsafeOptInUsageError") Intent intent = new Intent(UsersListActivity.this, SelectPlayerActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "");
            startActivity(intent);
            finish();
        });
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_users_list;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
    }

    @SuppressLint("StaticFieldLeak")
    private void getUserData() {
        new AsyncTask<String, String, String>() {

            @Override
            protected void onPreExecute() {
                super.onPreExecute();
            }

            @Override
            protected String doInBackground(String... strings) {
                try {
                    DBHelper dbHelper = new DBHelper(UsersListActivity.this);
                    arrayList.addAll(dbHelper.loadUsersDB());
                } catch (Exception e) {
                    e.printStackTrace();
                }
                return null;
            }

            @Override
            protected void onPostExecute(String s) {
                super.onPostExecute(s);
                if (!isFinishing()){
                    if (arrayList != null && !arrayList.isEmpty()){
                        setAdapter();
                    } else {
                        setEmpty();
                    }
                }
            }
        }.execute();
    }

    public void setAdapter() {
        AdapterUsers adapter = new AdapterUsers(this, arrayList, (itemCat, position) -> {
            if (position >= 0 && position < arrayList.size()) {
                if (arrayList.get(position).getUserType().equals("xui") || arrayList.get(position).getUserType().equals("stream")) {
                    loadLogin(arrayList.get(position));
                } else if (arrayList.get(position).getUserType().equals("playlist")) {
                    loadLoginPlaylist(arrayList.get(position).getAnyName(), arrayList.get(position).getUserURL());
                }
            } else {
                Toasty.makeText(UsersListActivity.this, "Position out of bounds: " + position, Toasty.ERROR);
            }
        });
        rv.setAdapter(adapter);
        if (ApplicationUtil.isTvBox(this)){
            rv.requestFocus();
        }
        setEmpty();
    }

    private void loadLoginPlaylist(String anyName, String userURL) {
        if (NetworkUtils.isConnected(this)){
            LoadPlaylist playlist = new LoadPlaylist(this,false, userURL, new LoadPlaylistListener() {
                @Override
                public void onStart() {
                    progressDialog.show();
                }

                @Override
                public void onEnd(String success, String msg, ArrayList<ItemPlaylist> arrayListPlaylist) {
                    progressDialog.dismiss();
                    if (!isFinishing()){
                        if (success.equals("1")) {
                            if (arrayListPlaylist.isEmpty()){
                                Toast.makeText(UsersListActivity.this, getString(R.string.err_no_data_found), Toast.LENGTH_SHORT).show();
                            } else {

                                new JSHelper(UsersListActivity.this).addToPlaylistData(arrayListPlaylist);

                                Toast.makeText(UsersListActivity.this, "Login successfully.", Toast.LENGTH_SHORT).show();

                                spHelper.setLoginType(Callback.TAG_LOGIN_PLAYLIST);
                                spHelper.setAnyName(anyName);
                                Intent intent = new Intent(UsersListActivity.this, PlaylistActivity.class);
                                intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                                startActivity(intent);
                                finish();
                            }
                        }  else {
                            Toasty.makeText(UsersListActivity.this, msg, Toasty.ERROR);
                        }
                    }
                }
            });
            playlist.execute();
        }  else {
            Toasty.makeText(UsersListActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
    }

    private void loadLogin(ItemUsersDB itemUsersDB) {
        if (NetworkUtils.isConnected(this)){
            LoadLogin login = new LoadLogin(new LoginListener() {
                @Override
                public void onStart() {
                    progressDialog.show();
                }

                @Override
                public void onEnd(String success, String username, String password, String message, int auth, String status, String exp_date, String is_trial, String active_cons, String created_at, String max_connections, String allowed_output_formats, boolean xui, String version, int revision, String url, String port, String https_port, String server_protocol, String rtmp_port, int timestamp_now, String time_now, String timezone) {
                    progressDialog.dismiss();
                    if (!isFinishing()){
                        if (success.equals("1")) {
                            try {
                                if (Boolean.TRUE.equals(itemUsersDB.getUserType().equals("xui"))){
                                    spHelper.setLoginDetails(username,password,message,auth,status, exp_date, is_trial, active_cons,created_at,max_connections,
                                            xui,version,revision,url,port,https_port,server_protocol,rtmp_port,timestamp_now,time_now,timezone
                                    );
                                    spHelper.setLoginType(Callback.TAG_LOGIN_ONE_UI);
                                } else {
                                    spHelper.setLoginDetails(username,password,message,auth,status, exp_date, is_trial, active_cons,created_at,max_connections,
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

                                spHelper.setAnyName(itemUsersDB.getAnyName());
                                spHelper.setIsFirst(false);
                                spHelper.setIsLogged(true);
                                spHelper.setIsAutoLogin(true);

                                Callback.isCustomAds = false;
                                Callback.customAdCount = 0;
                                Callback.customAdShow = 15;
                                Callback.is_load_ads = true;

                                Toast.makeText(UsersListActivity.this, "Login successfully.", Toast.LENGTH_SHORT).show();
                            } catch (Exception e) {
                                e.printStackTrace();
                            }
                            ApplicationUtil.openThemeActivity(UsersListActivity.this);
                        }  else {
                            Toasty.makeText(UsersListActivity.this, getString(R.string.err_server_not_connected), Toasty.ERROR);
                        }
                    }
                }

            },itemUsersDB.getUserURL(), helper.getAPIRequestLogin(itemUsersDB.getUseName(),itemUsersDB.getUserPass()));
            login.execute();
        }  else {
            Toasty.makeText(UsersListActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
    }

    @SuppressLint("InflateParams, UnsafeOptInUsageError")
    private void setEmpty() {
        if (!arrayList.isEmpty()) {
            rv.setVisibility(View.VISIBLE);
            frameLayout.setVisibility(View.GONE);
        } else {
            rv.setVisibility(View.GONE);
            frameLayout.setVisibility(View.VISIBLE);
            if (ApplicationUtil.isTvBox(this)){
                findViewById(R.id.ll_user_add).requestFocus();
            }

            frameLayout.removeAllViews();

            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

            View myView = inflater.inflate(R.layout.layout_add_user, null);

            frameLayout.addView(myView);
        }
    }

    @SuppressLint("UnsafeOptInUsageError")
    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)) {
            onBackPressed();
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public void onDestroy() {
        if (progressDialog != null && progressDialog.isShowing()){
            progressDialog.cancel();
        }
        super.onDestroy();
    }

    @SuppressLint("MissingSuperCall")
    @Override
    public void onBackPressed() {
        DialogUtil.ExitDialog(UsersListActivity.this);
    }
}