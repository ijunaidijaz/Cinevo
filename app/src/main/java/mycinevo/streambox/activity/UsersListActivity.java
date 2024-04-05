package mycinevo.streambox.activity;

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

import mycinevo.streambox.R;
import mycinevo.streambox.activity.UI.PlaylistActivity;
import mycinevo.streambox.adapter.AdapterUsers;
import mycinevo.streambox.asyncTask.LoadLogin;
import mycinevo.streambox.asyncTask.LoadPlaylist;
import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.dialog.ExitDialog;
import mycinevo.streambox.dialog.Toasty;
import mycinevo.streambox.interfaces.LoadPlaylistListener;
import mycinevo.streambox.interfaces.LoginListener;
import mycinevo.streambox.item.ItemPlaylist;
import mycinevo.streambox.item.ItemUsersDB;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.NetworkUtils;
import mycinevo.streambox.util.SharedPref;
import mycinevo.streambox.util.helper.DBHelper;
import mycinevo.streambox.util.helper.Helper;
import mycinevo.streambox.util.helper.JSHelper;
import mycinevo.streambox.view.NSoftsProgressDialog;

public class UsersListActivity extends AppCompatActivity {

    private Helper helper;
    private SharedPref sharedPref;
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
        sharedPref = new SharedPref(this);

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
            if (arrayList.get(position).getUserType().equals("xui") || arrayList.get(position).getUserType().equals("stream")){
                loadLogin(arrayList.get(position));
            } else if (arrayList.get(position).getUserType().equals("playlist")){
                loadLoginPlaylist(arrayList.get(position).getAnyName(), arrayList.get(position).getUserURL());
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
                public void onEnd(String success, ArrayList<ItemPlaylist> arrayListPlaylist) {
                    progressDialog.dismiss();
                    if (!isFinishing()){
                        if (success.equals("1")) {
                            if (arrayListPlaylist.isEmpty()){
                                Toast.makeText(UsersListActivity.this, getString(R.string.err_no_data_found), Toast.LENGTH_SHORT).show();
                            } else {

                                new JSHelper(UsersListActivity.this).addToPlaylistData(arrayListPlaylist);

                                Toast.makeText(UsersListActivity.this, "Login successfully.", Toast.LENGTH_SHORT).show();

                                sharedPref.setLoginType(Callback.TAG_LOGIN_PLAYLIST);
                                sharedPref.setAnyName(anyName);
                                Intent intent = new Intent(UsersListActivity.this, PlaylistActivity.class);
                                intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                                startActivity(intent);
                                finish();
                            }
                        }  else {
                            Toasty.makeText(UsersListActivity.this, getString(R.string.err_server_not_connected), Toasty.ERROR);
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
                                    sharedPref.setLoginDetails(username,password,message,auth,status, exp_date, is_trial, active_cons,created_at,max_connections,
                                            xui,version,revision,url,port,https_port,server_protocol,rtmp_port,timestamp_now,time_now,timezone
                                    );
                                    sharedPref.setLoginType(Callback.TAG_LOGIN_ONE_UI);
                                } else {
                                    sharedPref.setLoginDetails(username,password,message,auth,status, exp_date, is_trial, active_cons,created_at,max_connections,
                                            xui,version,revision,url,port,https_port,server_protocol,rtmp_port,timestamp_now,time_now,timezone
                                    );
                                    sharedPref.setLoginType(Callback.TAG_LOGIN_STREAM);
                                }

                                if (!allowed_output_formats.isEmpty()){
                                    if (allowed_output_formats.contains("m3u8")){
                                        sharedPref.setLiveFormat(2);
                                    } else {
                                        sharedPref.setLiveFormat(1);
                                    }
                                } else {
                                    sharedPref.setLiveFormat(0);
                                }

                                sharedPref.setAnyName(itemUsersDB.getAnyName());
                                sharedPref.setIsFirst(false);
                                sharedPref.setIsLogged(true);
                                sharedPref.setIsAutoLogin(true);

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
            new ExitDialog(UsersListActivity.this);
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
        new ExitDialog(UsersListActivity.this);
    }
}