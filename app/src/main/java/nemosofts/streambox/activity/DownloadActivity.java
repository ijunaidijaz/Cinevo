package nemosofts.streambox.activity;

import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.READ_MEDIA_VIDEO;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.os.StatFs;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterDownload;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.item.ItemVideoDownload;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.view.NSoftsProgressDialog;

public class DownloadActivity extends AppCompatActivity {

    private DBHelper dbHelper;
    private RecyclerView rv;
    private ArrayList<ItemVideoDownload> arrayList;
    private FrameLayout frameLayout;
    private NSoftsProgressDialog progressDialog;
    private ProgressBar pb_data;
    private TextView tv_used_data, tv_total_data;
    private final Handler handlerSeries = new Handler();
    private int progressStatusOld = 0;

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

        progressDialog = new NSoftsProgressDialog(DownloadActivity.this);

        dbHelper = new DBHelper(this);

        arrayList = new ArrayList<>();

        pb_data = findViewById(R.id.pb_data);
        pb_data.setMax(100);
        tv_used_data = findViewById(R.id.tv_used_data);
        tv_total_data = findViewById(R.id.tv_total_data);

        frameLayout = findViewById(R.id.fl_empty);
        rv = findViewById(R.id.rv);
        GridLayoutManager grid = new GridLayoutManager(this, 6);
        grid.setSpanCount(6);
        rv.setLayoutManager(grid);
        rv.setItemAnimator(new DefaultItemAnimator());
        rv.setHasFixedSize(true);

        if(Boolean.TRUE.equals(checkPer())) {
            new LoadDownloadVideo().execute();
        }
    }

    @SuppressLint("StaticFieldLeak")
    class LoadDownloadVideo extends AsyncTask<String, String, String> {

        @Override
        protected void onPreExecute() {
            arrayList.clear();
            frameLayout.setVisibility(View.GONE);
            rv.setVisibility(View.GONE);
            progressDialog.show();
            super.onPreExecute();
        }

        @Override
        protected String doInBackground(String... strings) {
            try {
                List<ItemVideoDownload> tempArray = dbHelper.loadDataDownload(DBHelper.TABLE_DOWNLOAD_MOVIES);
                File fileRoot = new File(getExternalFilesDir("").getAbsolutePath() + File.separator + "temp");
                File[] files = fileRoot.listFiles();
                if (files != null) {
                    for (File file : files) {
                        for (int j = 0; j < tempArray.size(); j++) {
                            if (new File(file.getAbsolutePath()).getName().contains(tempArray.get(j).getTempName())) {
                                ItemVideoDownload itemVideo = tempArray.get(j);
                                itemVideo.setVideoURL(file.getAbsolutePath());
                                arrayList.add(itemVideo);
                                break;
                            }
                        }
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
            return null;
        }

        @Override
        protected void onPostExecute(String s) {
            progressDialog.dismiss();
            if (!isFinishing()){
                if (!arrayList.isEmpty()){
                    setAdapterToListview();
                } else {
                    setEmpty();
                }
            }
        }
    }

    @SuppressLint("UnsafeOptInUsageError")
    public void setAdapterToListview() {
        AdapterDownload adapter = new AdapterDownload(this, arrayList, new AdapterDownload.RecyclerItemClickListener() {
            @Override
            public void onClickListener(ItemVideoDownload itemVideo, int position) {
                Intent intent = new Intent(DownloadActivity.this, PlayerDownloadActivity.class);
                intent.putExtra("channel_title", arrayList.get(position).getName());
                intent.putExtra("channel_url", arrayList.get(position).getVideoURL());
                startActivity(intent);
            }

            @Override
            public void onDelete() {
                updateStorageInfo(true);
            }
        });
        rv.setAdapter(adapter);
        if (ApplicationUtil.isTvBox(this)){
            rv.requestFocus();
        }
        setEmpty();
    }

    @SuppressLint({"SetTextI18n", "DefaultLocale"})
    private void updateStorageInfo(Boolean is_delete) {
        try {
            StatFs stat = new StatFs(Environment.getExternalStorageDirectory().getPath());
            long availableBytes = stat.getAvailableBytes();
            long totalBytes = stat.getTotalBytes();

            int progress = (int) ((availableBytes * 100) / totalBytes);

            if (Boolean.FALSE.equals(is_delete)){
                progressStatusOld = 0;
                handlerSeries.postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        if (progressStatusOld < 100 - progress) {
                            progressStatusOld++;
                            pb_data.setProgress(progressStatusOld);
                            handlerSeries.postDelayed(this, 10);
                        }
                    }
                }, 10);
            } else {
                pb_data.setProgress(100 - progress);
            }

            double availableGB = availableBytes / (1024.0 * 1024.0 * 1024.0);
            double totalGB = totalBytes / (1024.0 * 1024.0 * 1024.0);

            tv_used_data.setText(String.format("%.2f GB", availableGB) + " Available Storage");
            tv_total_data.setText(String.format("%.2f GB", totalGB) + " total . Internal Storage");

            findViewById(R.id.ll_storage).setVisibility(View.VISIBLE);
        } catch (Exception e) {
            e.printStackTrace();
            findViewById(R.id.ll_storage).setVisibility(View.GONE);
        }
    }

    private void setEmpty() {
        updateStorageInfo(false);
        if (!arrayList.isEmpty()) {
            rv.setVisibility(View.VISIBLE);
            frameLayout.setVisibility(View.GONE);
        } else {
            rv.setVisibility(View.GONE);
            frameLayout.setVisibility(View.VISIBLE);

            frameLayout.removeAllViews();

            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.row_empty, null);

            myView.findViewById(R.id.tv_empty_msg_sub).setVisibility(View.GONE);

            frameLayout.addView(myView);
        }
    }

    public Boolean checkPer() {
        if (Build.VERSION.SDK_INT >= 33){
            if ((ContextCompat.checkSelfPermission(DownloadActivity.this, READ_MEDIA_VIDEO) != PackageManager.PERMISSION_GRANTED)) {
                requestPermissions(new String[]{READ_MEDIA_VIDEO}, 1);
                return false;
            } else {
                return true;
            }
        } else if (Build.VERSION.SDK_INT >= 29) {
            if ((ContextCompat.checkSelfPermission(DownloadActivity.this, READ_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED)) {
                requestPermissions(new String[]{READ_EXTERNAL_STORAGE}, 1);
                return false;
            } else {
                return true;
            }
        } else {
            if ((ContextCompat.checkSelfPermission(DownloadActivity.this, WRITE_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED) && (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M)) {
                requestPermissions(new String[]{WRITE_EXTERNAL_STORAGE}, 1);
                return false;
            }
            return true;
        }
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        boolean canUseExternalStorage = false;
        if (requestCode == 1) {
            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                canUseExternalStorage = true;
                new LoadDownloadVideo().execute();
            }

            if (!canUseExternalStorage) {
                Toast.makeText(DownloadActivity.this, getResources().getString(R.string.err_cannot_use_features), Toast.LENGTH_SHORT).show();
            }
        }
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_download;
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
}