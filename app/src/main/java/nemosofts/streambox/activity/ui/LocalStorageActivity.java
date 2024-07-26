package nemosofts.streambox.activity.ui;

import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.READ_MEDIA_VIDEO;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.provider.MediaStore;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.OptIn;
import androidx.core.content.ContextCompat;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.onesignal.Continue;
import com.onesignal.OneSignal;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.activity.SelectPlayerActivity;
import nemosofts.streambox.activity.player.PlayerLocalActivity;
import nemosofts.streambox.adapter.VideoAdapter;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.item.ItemVideo;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.SPHelper;
import nemosofts.streambox.view.NSoftsProgressDialog;

public class LocalStorageActivity extends AppCompatActivity {

    private NSoftsProgressDialog progressDialog;
    private FrameLayout frameLayout;
    private RecyclerView rv;
    private ArrayList<ItemVideo> itemVideoList;

    @OptIn(markerClass = UnstableApi.class)
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

        progressDialog = new NSoftsProgressDialog(LocalStorageActivity.this);

        itemVideoList = new ArrayList<>();

        frameLayout = findViewById(R.id.fl_empty);
        rv = findViewById(R.id.rv);
        GridLayoutManager grid = new GridLayoutManager(this, 2);
        grid.setSpanCount(2);
        rv.setLayoutManager(grid);
        rv.setItemAnimator(new DefaultItemAnimator());
        rv.setHasFixedSize(true);

        if(Boolean.TRUE.equals(checkPer(1))){
            new LoadDownloadVideo().execute();
        }

        findViewById(R.id.iv_picker_video).setOnClickListener(v -> {
            if(Boolean.TRUE.equals(checkPer(2))) {
                openVideoPicker();
            }
        });
        findViewById(R.id.iv_exit).setOnClickListener(v -> {
            new SPHelper(this).setLoginType(Callback.TAG_LOGIN);
            Intent intent = new Intent(LocalStorageActivity.this, SelectPlayerActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "");
            startActivity(intent);
            finish();
        });

        // requestPermission will show the native Android notification permission prompt.
        // NOTE: It's recommended to use a OneSignal In-App Message to prompt instead.
        OneSignal.getNotifications().requestPermission(false, Continue.none());
    }

    @SuppressLint("StaticFieldLeak")
    class LoadDownloadVideo extends AsyncTask<String, String, String> {

        @Override
        protected void onPreExecute() {
            rv.setVisibility(View.GONE);
            progressDialog.show();
            super.onPreExecute();
        }

        @Override
        protected String doInBackground(String... strings) {
            try {
                Uri uri = MediaStore.Video.Media.EXTERNAL_CONTENT_URI;
                String[] projection = { MediaStore.Video.Media._ID, MediaStore.Video.Media.TITLE, MediaStore.Video.Media.DATA };
                String selection = MediaStore.Video.Media.MIME_TYPE + "=? OR " + MediaStore.Video.Media.MIME_TYPE + "=?";
                String[] selectionArgs = { "video/mp4", "video/x-msvideo" }; // MIME type for mp4 and avi videos

                Cursor cursor = getContentResolver().query(uri, projection, selection, selectionArgs, null);
                if (cursor != null) {
                    while (cursor.moveToNext()) {
                        String id = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Video.Media._ID));
                        String title = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Video.Media.TITLE));
                        String path = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Video.Media.DATA));
                        String thumbnailPath = getThumbnailPath(id);
                        itemVideoList.add(new ItemVideo(title, path, thumbnailPath));
                    }
                    cursor.close();
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
                if (!itemVideoList.isEmpty()){
                    setAdapterToListview();
                } else {
                    setEmpty();
                }
            }
        }
    }

    @OptIn(markerClass = UnstableApi.class)
    private void setAdapterToListview() {
        VideoAdapter videoAdapter = new VideoAdapter(itemVideoList, (itemVideo, position) -> {
            Intent intent = new Intent(LocalStorageActivity.this, PlayerLocalActivity.class);
            intent.putExtra("channel_title", itemVideoList.get(position).getTitle());
            intent.putExtra("channel_url", itemVideoList.get(position).getPath());
            startActivity(intent);
        });
        rv.setAdapter(videoAdapter);
        setEmpty();
    }

    private void setEmpty() {
        if (!itemVideoList.isEmpty()) {
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

    @Override
    public int setContentViewID() {
        return R.layout.activity_ui_local_storage;
    }

    private String getThumbnailPath(String videoId) {
        Uri thumbnailUri = MediaStore.Video.Thumbnails.EXTERNAL_CONTENT_URI;
        String[] projection = { MediaStore.Video.Thumbnails.DATA };
        String selection = MediaStore.Video.Thumbnails.VIDEO_ID + "=?";
        String[] selectionArgs = { videoId };
        Cursor cursor = getContentResolver().query(thumbnailUri, projection, selection, selectionArgs, null);
        if (cursor != null && cursor.moveToFirst()) {
            String thumbnailPath = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Video.Thumbnails.DATA));
            cursor.close();
            return thumbnailPath;
        }
        return null;
    }

    public Boolean checkPer(int code) {
        if (Build.VERSION.SDK_INT >= 33){
            if ((ContextCompat.checkSelfPermission(LocalStorageActivity.this, READ_MEDIA_VIDEO) != PackageManager.PERMISSION_GRANTED)) {
                requestPermissions(new String[]{READ_MEDIA_VIDEO}, code);
                return false;
            } else {
                return true;
            }
        } else if (Build.VERSION.SDK_INT >= 29) {
            if ((ContextCompat.checkSelfPermission(LocalStorageActivity.this, READ_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED)) {
                requestPermissions(new String[]{READ_EXTERNAL_STORAGE}, code);
                return false;
            } else {
                return true;
            }
        } else {
            if (ContextCompat.checkSelfPermission(LocalStorageActivity.this, WRITE_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED) {
                requestPermissions(new String[]{WRITE_EXTERNAL_STORAGE}, code);
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
                Toast.makeText(LocalStorageActivity.this, getResources().getString(R.string.err_cannot_use_features), Toast.LENGTH_SHORT).show();
            }
        } else if (requestCode == 2) {
            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                canUseExternalStorage = true;
                openVideoPicker();
            }
            if (!canUseExternalStorage) {
                Toast.makeText(LocalStorageActivity.this, getResources().getString(R.string.err_cannot_use_features), Toast.LENGTH_SHORT).show();
            }
        }
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
    }

    private void openVideoPicker() {
        Intent intent = new Intent(Intent.ACTION_PICK, MediaStore.Video.Media.EXTERNAL_CONTENT_URI);
        startActivityForResult(intent, 12);
    }

    @OptIn(markerClass = UnstableApi.class)
    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        if (requestCode == 12 && resultCode == RESULT_OK && data.getData() != null) {
            Uri videoUri = data.getData();
            String videoPath = getRealPathFromURI(videoUri);
            if (videoPath != null){
                Intent intent = new Intent(LocalStorageActivity.this, PlayerLocalActivity.class);
                intent.putExtra("channel_title", "video");
                intent.putExtra("channel_url", videoPath);
                startActivity(intent);
            }
        }
    }

    @Nullable
    private String getRealPathFromURI(Uri contentUri) {
        try {
            Cursor cursor = getContentResolver().query(contentUri,null, null, null, null);
            if (cursor == null) {
                return contentUri.getPath();
            }
            cursor.moveToFirst();
            int idx = cursor.getColumnIndex(MediaStore.Video.Media.DATA);
            return cursor.getString(idx);
        } catch (Exception e) {
            return null;
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
    public void onResume() {
        if (Boolean.TRUE.equals(Callback.is_recreate)) {
            Callback.is_recreate = false;
            recreate();
        }
        super.onResume();
    }

    @Override
    public void onBackPressed() {
        if (ApplicationUtil.isTvBox(LocalStorageActivity.this)) {
            super.onBackPressed();
        } else {
            DialogUtil.ExitDialog(LocalStorageActivity.this);
        }
    }
}