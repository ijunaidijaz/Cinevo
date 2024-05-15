package mycinevo.streambox.activity;

import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.view.KeyEvent;

import androidx.annotation.NonNull;
import androidx.core.app.ActivityCompat;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;

import mycinevo.streambox.BuildConfig;
import mycinevo.streambox.R;
import mycinevo.streambox.adapter.AdapterSelect;
import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.dialog.DialogUtil;
import mycinevo.streambox.item.ItemSelect;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.helper.SPHelper;

@UnstableApi
public class SelectPlayerActivity extends AppCompatActivity {

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

        SPHelper spHelper = new SPHelper(this);

        RecyclerView rv = findViewById(R.id.rv_list);
        GridLayoutManager grid = new GridLayoutManager(this, 2);
        grid.setSpanCount(2);
        rv.setLayoutManager(grid);
        rv.setItemAnimator(new DefaultItemAnimator());
        rv.setHasFixedSize(true);

        ArrayList<ItemSelect> arrayList = new ArrayList<>();
        if (Boolean.TRUE.equals(spHelper.getIsSelect(SPHelper.TAG_SELECT_XUI))){
            arrayList.add(new ItemSelect(getString(R.string.login_with_xtream_codes), R.drawable.ic_folder_connection,false));
        }
        if (Boolean.TRUE.equals(spHelper.getIsSelect(SPHelper.TAG_SELECT_STREAM))){
            arrayList.add(new ItemSelect(getString(R.string._1_stream), R.drawable.ic_mist_line,false));
        }
        if (Boolean.TRUE.equals(spHelper.getIsSelect(SPHelper.TAG_SELECT_PLAYLIST))){
            arrayList.add(new ItemSelect(getString(R.string.m3u_playlist), R.drawable.ic_play_list,false));
        }
        if (Boolean.TRUE.equals(spHelper.getIsSelect(SPHelper.TAG_SELECT_DEVICE))){
            arrayList.add(new ItemSelect(getString(R.string.login_with_device_id), R.drawable.ic_devices,false));
        }
        if (Boolean.TRUE.equals(spHelper.getIsSelect(SPHelper.TAG_SELECT_SINGLE))){
            arrayList.add(new ItemSelect(getString(R.string.play_single_stream), R.drawable.ic_movie,false));
        }
        arrayList.add(new ItemSelect(getString(R.string.list_users), R.drawable.ic_user_octagon,true));
        arrayList.add(new ItemSelect(getString(R.string._downloads), R.drawable.iv_downloading,true));

//        if (Boolean.TRUE.equals(sharedPref.getIsSelect(SharedPref.TAG_SELECT_SUBSCRIPTION))){
//            arrayList.add(new ItemSelect(getString(R.string._subscription), R.drawable.ic_shopping_cart,true));
//        }

        AdapterSelect adapterSelect = new AdapterSelect(arrayList, (item, position) -> select(arrayList.get(position).getTitle()));
        rv.setAdapter(adapterSelect);
        if (ApplicationUtil.isTvBox(this)){
            rv.requestFocus();
        }

        findViewById(R.id.tv_terms).setOnClickListener(view -> {
            Intent intent = new Intent(SelectPlayerActivity.this, WebActivity.class);
            intent.putExtra("web_url", BuildConfig.BASE_URL+"terms.php");
            intent.putExtra("page_title", getResources().getString(R.string.terms_and_conditions));
            ActivityCompat.startActivity(SelectPlayerActivity.this, intent, null);
        });
    }

    private void select(@NonNull String title) {
        if (title.equals(getString(R.string.login_with_xtream_codes))){
            Intent intent = new Intent(SelectPlayerActivity.this, SignInActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "xtream");
            startActivity(intent);
            finish();
        } else if (title.equals(getString(R.string._1_stream))){
            Intent intent = new Intent(SelectPlayerActivity.this, SignInActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "stream");
            startActivity(intent);
            finish();
        } else if (title.equals(getString(R.string.m3u_playlist))){
            Intent intent = new Intent(SelectPlayerActivity.this, AddPlaylistActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "");
            startActivity(intent);
            finish();
        } else if (title.equals(getString(R.string.login_with_device_id))){
            Intent intent = new Intent(SelectPlayerActivity.this, SignInDeviceActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "");
            startActivity(intent);
            finish();
        } else if (title.equals(getString(R.string.play_single_stream))){
            Intent intent = new Intent(SelectPlayerActivity.this, AddSingleURLActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "");
            startActivity(intent);
            finish();
        } else if (title.equals(getString(R.string.list_users))){
            Intent intent = new Intent(SelectPlayerActivity.this, UsersListActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "");
            startActivity(intent);
            finish();
        } else if (title.equals(getString(R.string._downloads))){
            startActivity(new Intent(SelectPlayerActivity.this, DownloadActivity.class));
        } else if (title.equals(getString(R.string._subscription))){
            startActivity(new Intent(SelectPlayerActivity.this, SubscriptionActivity.class));
        }
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_select_player;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN) {
            if (keyCode == KeyEvent.KEYCODE_BACK){
                onBackPressed();
                return true;
            } else if (keyCode == KeyEvent.KEYCODE_HOME){
                ApplicationUtil.openHomeActivity(this);
                return true;
            }
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public void onBackPressed() {
        if (ApplicationUtil.isTvBox(SelectPlayerActivity.this)){
            super.onBackPressed();
        } else {
            DialogUtil.ExitDialog(SelectPlayerActivity.this);
        }
    }
}