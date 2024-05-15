package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.pm.ActivityInfo;
import android.os.AsyncTask;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.widget.ProgressBar;

import androidx.annotation.NonNull;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.net.CookieManager;
import java.net.CookiePolicy;
import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.epg.AdapterEpg;
import nemosofts.streambox.adapter.epg.AdapterLiveEpg;
import nemosofts.streambox.adapter.epg.ItemPost;
import nemosofts.streambox.asyncTask.LoadEpg;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.Toasty;
import nemosofts.streambox.interfaces.EpgListener;
import nemosofts.streambox.item.ItemEpg;
import nemosofts.streambox.item.ItemLive;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.SPHelper;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.helper.JSHelper;

@UnstableApi
public class EPGActivity extends AppCompatActivity {

    private Helper helper;
    private SPHelper spHelper;
    private JSHelper jsHelper;
    private String cat_id = "0";
    private RecyclerView rv_live;
    private ArrayList<ItemLive> arrayList;
    ArrayList<ItemPost> arrayListPost = new ArrayList<>();
    ArrayList<ItemEpg> arrayListEpg = new ArrayList<>();
    AdapterLiveEpg adapter;
    ProgressBar pb;

    private static final CookieManager DEFAULT_COOKIE_MANAGER;
    static {
        DEFAULT_COOKIE_MANAGER = new CookieManager();
        DEFAULT_COOKIE_MANAGER.setCookiePolicy(CookiePolicy.ACCEPT_ORIGINAL_SERVER);
    }

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

        cat_id = getIntent().getStringExtra("cat_id");

        jsHelper = new JSHelper(this);
        spHelper = new SPHelper(this);
        helper = new Helper(this);

        arrayList = new ArrayList<>();

        pb = findViewById(R.id.pb);
        rv_live = findViewById(R.id.rv_live);
        rv_live.setHasFixedSize(true);
        LinearLayoutManager llm = new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
        rv_live.setLayoutManager(llm);
        rv_live.setNestedScrollingEnabled(false);

        getData();
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_epg;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
    }

    @SuppressLint("StaticFieldLeak")
    private void getData() {
        new AsyncTask<String, String, String>() {

            final ArrayList<ItemLive> itemLives = new ArrayList<>();

            @Override
            protected void onPreExecute() {
                pb.setVisibility(View.VISIBLE);
                super.onPreExecute();
            }

            @Override
            protected String doInBackground(String... strings) {
                try {
                    itemLives.addAll(jsHelper.getLive(cat_id, false));
                } catch (Exception e) {
                    e.printStackTrace();
                }
                return null;
            }

            @Override
            protected void onPostExecute(String s) {
                super.onPostExecute(s);
                pb.setVisibility(View.GONE);
                if (!isFinishing()){
                    if (itemLives.isEmpty()) {
                        findViewById(R.id.ll_epg).setVisibility(View.GONE);
                        findViewById(R.id.ll_epg_empty).setVisibility(View.VISIBLE);
                    } else {
                        arrayList.addAll(itemLives);
                        setAdapterToListview();
                    }
                }
            }
        }.execute();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void setAdapterToListview() {
        adapter = new AdapterLiveEpg(this, arrayList, (itemCat, position) -> {
            adapter.select(position);
            setMediaSource(position);
        });
        rv_live.setAdapter(adapter);
    }

    private void setMediaSource(int playPos) {
        ItemPost itemPost = new ItemPost("1","logo");
        ArrayList<ItemLive> arrayListLive = new ArrayList<>();
        arrayListLive.add(arrayList.get(playPos));
        itemPost.setArrayListLive(arrayListLive);
        arrayListPost.add(itemPost);

        getEpgData(playPos);
    }

    private void getEpgData(int playPos) {
        if (NetworkUtils.isConnected(this)){
            LoadEpg loadSeriesID = new LoadEpg(this, new EpgListener() {
                @Override
                public void onStart() {
                    pb.setVisibility(View.VISIBLE);
                }

                @Override
                public void onEnd(String success, ArrayList<ItemEpg> epgArrayList) {
                    pb.setVisibility(View.GONE);
                    if (!isFinishing()){
                        if (!epgArrayList.isEmpty()){
                            arrayListEpg.addAll(epgArrayList);
                        }
                        setEpg();
                    }
                }
            }, helper.getAPIRequestID("get_simple_data_table","stream_id", arrayList.get(playPos).getStreamID(), spHelper.getUserName(), spHelper.getPassword()));
            loadSeriesID.execute();
        } else {
            Toasty.makeText(this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
    }

    private void setEpg() {
        ItemPost itemPost2 = new ItemPost("2","listings");
        if (!arrayListEpg.isEmpty()){
            itemPost2.setArrayListEpg(arrayListEpg);
        } else {
            ArrayList<ItemEpg> arrayListEp = new ArrayList<>();
            arrayListEp.add(new ItemEpg("","", ApplicationUtil.encodeBase64("No Data Found"),"",""));
            itemPost2.setArrayListEpg(arrayListEp);
        }
        arrayListPost.add(itemPost2);

        RecyclerView rv_home = findViewById(R.id.rv_epg);
        LinearLayoutManager llm = new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
        rv_home.setLayoutManager(llm);
        rv_home.setItemAnimator(new DefaultItemAnimator());
        rv_home.setHasFixedSize(true);
        AdapterEpg adapterHome = new AdapterEpg(this, arrayListPost);
        rv_home.setAdapter(adapterHome);
        rv_home.scrollToPosition(arrayListPost.size() - 1);
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