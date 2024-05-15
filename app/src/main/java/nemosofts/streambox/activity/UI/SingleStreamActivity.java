package mycinevo.streambox.activity.UI;

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
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;

import mycinevo.streambox.R;
import mycinevo.streambox.activity.DownloadActivity;
import mycinevo.streambox.activity.NotificationsActivity;
import mycinevo.streambox.activity.PlayerSingleURLActivity;
import mycinevo.streambox.activity.SelectPlayerActivity;
import mycinevo.streambox.adapter.AdapterSingleURL;
import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.dialog.DialogUtil;
import mycinevo.streambox.dialog.PopupAdsDialog;
import mycinevo.streambox.dialog.Toasty;
import mycinevo.streambox.item.ItemSingleURL;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.NetworkUtils;
import mycinevo.streambox.util.helper.SPHelper;
import mycinevo.streambox.util.helper.DBHelper;

@UnstableApi
public class SingleStreamActivity extends AppCompatActivity implements View.OnClickListener {

    private DBHelper dbHelper;
    private RecyclerView rv;
    private ArrayList<ItemSingleURL> arrayList;
    private FrameLayout frameLayout;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (Boolean.TRUE.equals(Callback.isLandscape)){
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        }
        IfSupported.IsRTL(this);
        IfSupported.IsScreenshot(this);
        IfSupported.hideStatusBar(this);

        Callback.isAppOpen = true;
        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        dbHelper = new DBHelper(this);

        arrayList = new ArrayList<>();

        frameLayout = findViewById(R.id.fl_empty);
        rv = findViewById(R.id.rv);

        GridLayoutManager grid = new GridLayoutManager(this, 2);
        grid.setSpanCount(2);
        rv.setLayoutManager(grid);
        rv.setItemAnimator(new DefaultItemAnimator());
        rv.setHasFixedSize(true);

        getData();
        setListener();
        getNetworkInfo();

        new PopupAdsDialog(this);
    }

    private void setListener() {
        findViewById(R.id.iv_notifications).setOnClickListener(this);
        findViewById(R.id.iv_file_download).setOnClickListener(this);
        findViewById(R.id.ll_url_add).setOnClickListener(this);
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_ui_single_stream;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
    }

    private void getNetworkInfo() {
        ImageView iv_wifi = findViewById(R.id.iv_wifi);
        if (NetworkUtils.isConnected(this)) {
            if (NetworkUtils.isConnectedMobile(this)){
                iv_wifi.setImageResource(R.drawable.bar_selector_none);
            } else if (NetworkUtils.isConnectedWifi(this)){
                iv_wifi.setImageResource(R.drawable.ic_wifi);
            } else if (NetworkUtils.isConnectedEthernet(this)){
                iv_wifi.setImageResource(R.drawable.ic_ethernet);
            }
        } else {
            iv_wifi.setImageResource(R.drawable.ic_wifi_off);
        }
    }

    @SuppressLint("StaticFieldLeak")
    private void getData() {
        new AsyncTask<String, String, String>() {

            @Override
            protected void onPreExecute() {
                super.onPreExecute();
            }

            @Override
            protected String doInBackground(String... strings) {
                try {
                    arrayList.addAll(dbHelper.loadSingleURL());
                } catch (Exception e) {
                    e.printStackTrace();
                }
                return null;
            }

            @Override
            protected void onPostExecute(String s) {
                super.onPostExecute(s);
                if (!isFinishing()){
                    if (!arrayList.isEmpty()){
                        setAdapter();
                    } else {
                        setEmpty();
                    }
                }
            }
        }.execute();
    }

    public void setAdapter() {
        AdapterSingleURL adapter;
        adapter = new AdapterSingleURL(this,arrayList, (itemCat, position) -> {
            if (NetworkUtils.isConnected(this)){
                new SPHelper(this).setLoginType(Callback.TAG_LOGIN_SINGLE_STREAM);
                Intent intent = new Intent(SingleStreamActivity.this, PlayerSingleURLActivity.class);
                intent.putExtra("channel_title", arrayList.get(position).getAnyName());
                intent.putExtra("channel_url", arrayList.get(position).getSingleURL());
                startActivity(intent);
            } else {
                Toasty.makeText(SingleStreamActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            }
        });
        rv.setAdapter(adapter);
        if (ApplicationUtil.isTvBox(this)){
            rv.requestFocus();
        }
        setEmpty();
    }

    @SuppressLint("MissingInflatedId")
    private void setEmpty() {
        if (!arrayList.isEmpty()) {
            rv.setVisibility(View.VISIBLE);
            frameLayout.setVisibility(View.GONE);
        } else {
            if (ApplicationUtil.isTvBox(this)){
                findViewById(R.id.ll_url_add).requestFocus();
            }

            rv.setVisibility(View.GONE);
            frameLayout.setVisibility(View.VISIBLE);

            frameLayout.removeAllViews();

            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.row_empty, null);

            myView.findViewById(R.id.tv_empty_msg_sub).setVisibility(View.GONE);

            frameLayout.addView(myView);
        }
    }

    @SuppressLint({"NonConstantResourceId", "UnsafeOptInUsageError"})
    @Override
    public void onClick(@NonNull View id) {
        switch (id.getId()) {
            case R.id.iv_notifications ->
                    startActivity(new Intent(SingleStreamActivity.this, NotificationsActivity.class));
            case R.id.iv_file_download ->
                    startActivity(new Intent(SingleStreamActivity.this, DownloadActivity.class));
            case R.id.ll_url_add -> {
                new SPHelper(this).setLoginType(Callback.TAG_LOGIN);
                Intent intent = new Intent(SingleStreamActivity.this, SelectPlayerActivity.class);
                intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                intent.putExtra("from", "");
                startActivity(intent);
                finish();
            }
            default -> {
            }
        }
    }

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
        try {
            dbHelper.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
        super.onDestroy();
    }

    @Override
    public void onResume() {
        if (Boolean.TRUE.equals(Callback.is_recreate)) {
            Callback.is_recreate = false;
            recreate();
        }
        super.onResume();
    }

    @Override
    public void onBackPressed() {
        if (ApplicationUtil.isTvBox(SingleStreamActivity.this)) {
            super.onBackPressed();
        } else {
            DialogUtil.ExitDialog(SingleStreamActivity.this);
        }
    }
}