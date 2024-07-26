package nemosofts.streambox.activity.setting;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompatActivity;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import org.apache.commons.io.FileUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.Objects;

import nemosofts.streambox.R;
import nemosofts.streambox.activity.AboutUsActivity;
import nemosofts.streambox.activity.NetworkSpeedActivity;
import nemosofts.streambox.activity.NotificationsActivity;
import nemosofts.streambox.activity.ProfileActivity;
import nemosofts.streambox.adapter.AdapterSetting;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.AdultsCountDialog;
import nemosofts.streambox.dialog.FeedBackDialog;
import nemosofts.streambox.item.ItemSetting;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.SPHelper;
import nemosofts.streambox.view.NSoftsProgressDialog;

public class SettingActivity extends AppCompatActivity {

    private SPHelper spHelper;
    private DBHelper dbHelper;
    private RecyclerView rv;
    private ArrayList<ItemSetting> arrayList;
    private AdapterSetting adapter;
    private NSoftsProgressDialog progressDialog;
    private String cacheSize;
    private Boolean isTvBox;

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

        isTvBox  = ApplicationUtil.isTvBox(this);

        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (Boolean.TRUE.equals(isTvBox)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        spHelper = new SPHelper(this);

        progressDialog = new NSoftsProgressDialog(SettingActivity.this);

        dbHelper = new DBHelper(this);

        arrayList = new ArrayList<>();

        initializeCache();

        rv = findViewById(R.id.rv);
        rv.setHasFixedSize(true);
        GridLayoutManager grid = new GridLayoutManager(this, 4);
        grid.setSpanCount(4);
        rv.setLayoutManager(grid);

        addData();
        setAdapterToListview();
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_setting;
    }

    private void setAdapterToListview() {
        adapter = new AdapterSetting(this, arrayList, (itemSerials, position) -> setOnClick(arrayList.get(position).getName()));
        rv.setAdapter(adapter);
        if (Boolean.TRUE.equals(isTvBox)){
            rv.requestFocus();
        }
    }

    private void setOnClick(String name) {
        if (name != null){
            if (name.equals(getResources().getString(R.string.wifi_setting))){
                startActivity(new Intent(android.provider.Settings.ACTION_WIFI_SETTINGS));
            } else if (name.equals(getResources().getString(R.string.stream_format))){
                startActivity(new Intent(SettingActivity.this, SettingFormatActivity.class));
            } else if (name.equals(getResources().getString(R.string.notifications))){
                startActivity(new Intent(SettingActivity.this, NotificationsActivity.class));
            } else if (name.equals(getResources().getString(R.string.post_notification))){
                notification();
            } else if (name.equals(getResources().getString(R.string.clear_cache))){
                clearCache();
            } else if (name.equals(getResources().getString(R.string.adults_content))){
                new AdultsCountDialog(this);
            } else if (name.equals(getResources().getString(R.string.profile))){
                startActivity(new Intent(SettingActivity.this, ProfileActivity.class));
            } else if (name.equals(getResources().getString(R.string.speed_test))){
                startActivity(new Intent(SettingActivity.this, NetworkSpeedActivity.class));
            } else if (name.equals(getResources().getString(R.string.multiple_screen))){
                startActivity(new Intent(SettingActivity.this, SettingMultiScreenActivity.class));
            } else if (name.equals(getResources().getString(R.string.feedback))){
                new FeedBackDialog(this).showDialog(getString(R.string.feedback));
            } else if (name.equals(getResources().getString(R.string.general_setting))){
                startActivity(new Intent(SettingActivity.this, SettingGeneralActivity.class));
            } else if (name.equals(getResources().getString(R.string.ui))){
                startActivity(new Intent(SettingActivity.this, SettingUIActivity.class));
            }  else if (name.equals(getResources().getString(R.string.about))){
                startActivity(new Intent(SettingActivity.this, AboutUsActivity.class));
            }
        }
    }

    private void notification() {
        try {
            Intent intent = new Intent();
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                intent.setAction("android.settings.APP_NOTIFICATION_SETTINGS");
                intent.putExtra("android.provider.extra.APP_PACKAGE", getPackageName());
            } else {
                intent.setAction("android.settings.APPLICATION_DETAILS_SETTINGS");
                intent.addCategory(Intent.CATEGORY_DEFAULT);
                intent.setData(android.net.Uri.parse("package:" + getPackageName()));
            }
            startActivity(intent);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void addData() {
        if (!arrayList.isEmpty()){
            arrayList.clear();
        }
        arrayList.add(new ItemSetting(getResources().getString(R.string.general_setting), "", R.drawable.ic_player_setting));
        arrayList.add(new ItemSetting(getResources().getString(R.string.ui), "", R.drawable.ic_pencil_ruler));
        if (!spHelper.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
            arrayList.add(new ItemSetting(getResources().getString(R.string.stream_format), "", R.drawable.ic_video_settings));
        }
        arrayList.add(new ItemSetting(getResources().getString(R.string.multiple_screen), "", R.drawable.ic_grid_view));
        arrayList.add(new ItemSetting(getResources().getString(R.string.wifi_setting), "", R.drawable.ic_wifi));
        if (!ApplicationUtil.isTvBox(this)){
            arrayList.add(new ItemSetting(getResources().getString(R.string.post_notification), "", R.drawable.ic_round_notification));
        }
        arrayList.add(new ItemSetting(getResources().getString(R.string.notifications), "", R.drawable.ic_round_notifications));
        arrayList.add(new ItemSetting(getResources().getString(R.string.clear_cache), cacheSize, R.drawable.ic_clean_code));
        arrayList.add(new ItemSetting(getResources().getString(R.string.adults_content), "", R.drawable.ic_player_lock));
        if (!spHelper.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
            arrayList.add(new ItemSetting(getResources().getString(R.string.profile), "", R.drawable.ic_profile));
        }
        arrayList.add(new ItemSetting(getResources().getString(R.string.speed_test), "", R.drawable.ic_speed));
        arrayList.add(new ItemSetting(getResources().getString(R.string.feedback), "", R.drawable.ic_feedback));
        arrayList.add(new ItemSetting(getResources().getString(R.string.about), "", R.drawable.ic_information));
    }

    private void initializeCache() {
        try {
            long size = 0;
            size += getDirSize(this.getCacheDir());
            size += getDirSize(this.getExternalCacheDir());
            cacheSize = ApplicationUtil.readableFileSize(size);
        } catch (Exception e) {
            cacheSize ="0 MB";
            e.printStackTrace();
        }
    }

    private long getDirSize(File dir) {
        long size = 0;
        try {
            for (File file : Objects.requireNonNull(dir.listFiles())) {
                if (file != null && file.isDirectory()) {
                    size += getDirSize(file);
                } else if (file != null && file.isFile()) {
                    size += file.length();
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return size;
    }

    @SuppressLint("StaticFieldLeak")
    private void clearCache() {
        if (!cacheSize.equals("0 MB")){
            new AsyncTask<String, String, String>() {
                @Override
                protected void onPreExecute() {
                    progressDialog.show();
                    super.onPreExecute();
                }

                @Override
                protected String doInBackground(String... strings) {
                    try {
                        FileUtils.deleteQuietly(getCacheDir());
                        FileUtils.deleteQuietly(getExternalCacheDir());
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                    return null;
                }

                @SuppressLint("NotifyDataSetChanged")
                @Override
                protected void onPostExecute(String s) {
                    progressDialog.dismiss();
                    cacheSize = "0 MB";
                    addData();
                    if (adapter != null){
                        adapter.notifyDataSetChanged();
                    }
                    super.onPostExecute(s);
                }
            }.execute();
        }
    }

    @Override
    public void onDestroy() {
        if (progressDialog != null && progressDialog.isShowing()){
            progressDialog.cancel();
        }
        try {
            dbHelper.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
        super.onDestroy();
    }

    @Override
    public void onResume() {
        if (Boolean.TRUE.equals(Callback.is_recreate_ui)) {
            Callback.is_recreate_ui = false;
            Callback.is_recreate = true;
            recreate();
        }
        super.onResume();
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