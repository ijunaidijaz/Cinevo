package nemosofts.streambox.activity.epg;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.media.metrics.PlaybackStateEvent;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.RequiresApi;
import androidx.media3.common.C;
import androidx.media3.common.MediaItem;
import androidx.media3.common.PlaybackException;
import androidx.media3.common.Player;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.common.util.Util;
import androidx.media3.datasource.DataSource;
import androidx.media3.datasource.DefaultDataSourceFactory;
import androidx.media3.datasource.DefaultHttpDataSource;
import androidx.media3.datasource.HttpDataSource;
import androidx.media3.exoplayer.SimpleExoPlayer;
import androidx.media3.exoplayer.dash.DashMediaSource;
import androidx.media3.exoplayer.dash.DefaultDashChunkSource;
import androidx.media3.exoplayer.hls.HlsMediaSource;
import androidx.media3.exoplayer.rtsp.RtspMediaSource;
import androidx.media3.exoplayer.smoothstreaming.DefaultSsChunkSource;
import androidx.media3.exoplayer.smoothstreaming.SsMediaSource;
import androidx.media3.exoplayer.source.MediaSource;
import androidx.media3.exoplayer.source.ProgressiveMediaSource;
import androidx.media3.exoplayer.upstream.DefaultBandwidthMeter;
import androidx.media3.ui.AspectRatioFrameLayout;
import androidx.media3.ui.PlayerView;
import androidx.nemosofts.AppCompatActivity;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.net.CookieHandler;
import java.net.CookieManager;
import java.net.CookiePolicy;
import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.activity.player.PlayerLiveActivity;
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
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.util.helper.SPHelper;

@UnstableApi
public class EPGTwoActivity extends AppCompatActivity {

    private Helper helper;
    private SPHelper spHelper;
    private JSHelper jsHelper;
    private String cat_id = "0";
    private RecyclerView rv_live;
    private ArrayList<ItemLive> arrayList;
    ArrayList<ItemPost> arrayListPost = new ArrayList<>();
    AdapterLiveEpg adapter;
    ProgressBar pb;
    private int position = 0;
    private TextView tv_title, tv_time;

    private SimpleExoPlayer exoPlayer;
    private DefaultBandwidthMeter BANDWIDTH_METER;
    private DataSource.Factory mediaDataSourceFactory;

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

        tv_title = findViewById(R.id.tv_title);
        tv_time = findViewById(R.id.tv_time);

        pb = findViewById(R.id.pb);
        rv_live = findViewById(R.id.rv_live);
        rv_live.setHasFixedSize(true);
        LinearLayoutManager llm = new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
        rv_live.setLayoutManager(llm);
        rv_live.setNestedScrollingEnabled(false);

        intPlayer();

        getData();
    }

    private void intPlayer() {
        BANDWIDTH_METER = new DefaultBandwidthMeter.Builder(this).build();
        mediaDataSourceFactory = buildDataSourceFactory(true);
        if (CookieHandler.getDefault() != DEFAULT_COOKIE_MANAGER) {
            CookieHandler.setDefault(DEFAULT_COOKIE_MANAGER);
        }
        exoPlayer = new SimpleExoPlayer.Builder(this).build();
        PlayerView playerView = findViewById(R.id.exoPlayerView);
        playerView.setPlayer(exoPlayer);
        playerView.setUseController(true);
        playerView.requestFocus();

        playerView.setResizeMode(AspectRatioFrameLayout.RESIZE_MODE_FILL);
        exoPlayer.setVideoScalingMode(C.VIDEO_SCALING_MODE_DEFAULT);

        exoPlayer.addListener(new Player.Listener(){

            @Override
            public void onIsPlayingChanged(boolean isPlaying) {
                Player.Listener.super.onIsPlayingChanged(isPlaying);
                playerView.setKeepScreenOn(isPlaying);
            }

            @Override
            public void onPlayerStateChanged(boolean playWhenReady, int playbackState) {
                Player.Listener.super.onPlayerStateChanged(playWhenReady, playbackState);
                if (playbackState == PlaybackStateEvent.STATE_PLAYING) {
                    findViewById(R.id.pb_player).setVisibility(View.GONE);
                } else if (playbackState == Player.STATE_BUFFERING) {
                    findViewById(R.id.pb_player).setVisibility(View.VISIBLE);
                }
            }

            @Override
            public void onPlayerError(@NonNull PlaybackException error) {
                exoPlayer.stop();
                findViewById(R.id.pb_player).setVisibility(View.GONE);
                Toast.makeText(EPGTwoActivity.this, error.getMessage(), Toast.LENGTH_SHORT).show();
                Player.Listener.super.onPlayerError(error);
            }
        });
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_epg_two;
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
                    itemLives.addAll(jsHelper.getLive(cat_id));
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

    public void setAdapterToListview() {
        adapter = new AdapterLiveEpg(this, arrayList, (itemCat, pos) -> {
            position = pos;
            adapter.select(pos);
            setMediaSource(pos);
        });
        rv_live.setAdapter(adapter);
        setMediaSource(position);
        adapter.select(position);
        findViewById(R.id.vw_player).setOnClickListener(v -> {
            try {
                if (!arrayList.isEmpty()){
                    Callback.playPosLive = position;
                    if (!Callback.arrayListLive.isEmpty()) {
                        Callback.arrayListLive.clear();
                    }
                    Callback.arrayListLive.addAll(arrayList);
                    startActivity(new Intent(EPGTwoActivity.this, PlayerLiveActivity.class));
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    private void setMediaSource(int playPos) {
        String channelUrl = getChannelUrl(playPos);
        Uri uri = Uri.parse(channelUrl);
        MediaSource mediaSource = buildMediaSource(uri);
        exoPlayer.setMediaSource(mediaSource);
        exoPlayer.prepare();
        exoPlayer.setPlayWhenReady(true);

        if (!arrayListPost.isEmpty()){
            arrayListPost.clear();
        }
        ItemPost itemPost = new ItemPost("1","logo");
        ArrayList<ItemLive> arrayListLive = new ArrayList<>();
        arrayListLive.add(arrayList.get(playPos));
        itemPost.setArrayListLive(arrayListLive);
        arrayListPost.add(itemPost);

        getEpgData(playPos);
    }

    private @NonNull String getChannelUrl(int playPos) {
        String channelUrl;
        String format = ".m3u8";
        if (spHelper.getLiveFormat() == 1){
            format = ".ts";
        }
        if (Boolean.TRUE.equals(spHelper.getIsXuiUser())){
            channelUrl = spHelper.getServerURL()+ spHelper.getUserName()+"/"+ spHelper.getPassword()+"/"+arrayList.get(playPos).getStreamID()+format;
        } else {
            channelUrl = spHelper.getServerURL()+"live/"+ spHelper.getUserName()+"/"+ spHelper.getPassword()+"/"+arrayList.get(playPos).getStreamID()+format;
        }
        return channelUrl;
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
                            setEpg(epgArrayList);
                        } else {
                            setEpg(null);
                        }
                    }
                }
            }, helper.getAPIRequestID("get_simple_data_table","stream_id", arrayList.get(playPos).getStreamID(), spHelper.getUserName(), spHelper.getPassword()));
            loadSeriesID.execute();
        } else {
            Toasty.makeText(this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
    }

    @SuppressLint("SetTextI18n")
    private void setEpg(ArrayList<ItemEpg> arrayListEpg) {
        ItemPost itemPost = new ItemPost("1","listings");
        if (arrayListEpg != null && !arrayListEpg.isEmpty()){
            itemPost.setArrayListEpg(arrayListEpg);
            loadTimeData();
        } else {
            ArrayList<ItemEpg> arrayListEp = new ArrayList<>();
            arrayListEp.add(new ItemEpg("","", ApplicationUtil.encodeBase64("No Data Found"),"",""));
            itemPost.setArrayListEpg(arrayListEp);
            tv_title.setText(R.string.err_no_data_found);
            tv_time.setText("-");
        }
        arrayListPost.add(itemPost);

        RecyclerView rv_home = findViewById(R.id.rv_epg);
        LinearLayoutManager llm = new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
        rv_home.setLayoutManager(llm);
        rv_home.setItemAnimator(new DefaultItemAnimator());
        rv_home.setHasFixedSize(true);
        AdapterEpg adapterHome = new AdapterEpg(this, arrayListPost);
        rv_home.setAdapter(adapterHome);
        rv_home.scrollToPosition(arrayListPost.size() - 1);
    }

    private void loadTimeData() {
        if (NetworkUtils.isConnected(this)){
            LoadEpg loadSeriesID = new LoadEpg(this, new EpgListener() {
                @Override
                public void onStart() {

                }

                @SuppressLint("SetTextI18n")
                @Override
                public void onEnd(String success, ArrayList<ItemEpg> epgArrayList) {
                    if (!isFinishing()){
                        if (!epgArrayList.isEmpty()){
                            tv_title.setText(ApplicationUtil.decodeBase64(epgArrayList.get(0).getTitle()));
                            tv_time.setText(ApplicationUtil.getTimestamp(epgArrayList.get(0).getStartTimestamp()) + " - " + ApplicationUtil.getTimestamp(epgArrayList.get(0).getStopTimestamp()));
                        } else {
                            tv_title.setText(R.string.err_no_data_found);
                            tv_time.setText("-");
                        }
                    }
                }
            }, helper.getAPIRequestID("get_short_epg","stream_id", arrayList.get(position).getStreamID(), spHelper.getUserName(), spHelper.getPassword()));
            loadSeriesID.execute();
        } else {
            Toasty.makeText(this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
    }

    @SuppressLint("SwitchIntDef")
    @NonNull
    private MediaSource buildMediaSource(Uri uri) {
        int contentType  = Util.inferContentType(uri);
        MediaItem mediaItem = new MediaItem.Builder()
                .setUri(uri)
                .build();
        return switch (contentType) {
            case C.TYPE_DASH ->
                    new DashMediaSource.Factory(new DefaultDashChunkSource.Factory(mediaDataSourceFactory), buildDataSourceFactory(false))
                            .createMediaSource(mediaItem);
            case C.TYPE_SS ->
                    new SsMediaSource.Factory(new DefaultSsChunkSource.Factory(mediaDataSourceFactory), buildDataSourceFactory(false))
                            .createMediaSource(mediaItem);
            case C.TYPE_HLS ->
                    new HlsMediaSource.Factory(mediaDataSourceFactory)
                            .createMediaSource(mediaItem);
            case C.TYPE_RTSP ->
                    new RtspMediaSource.Factory()
                            .createMediaSource(mediaItem);
            case C.TYPE_OTHER ->
                    new ProgressiveMediaSource.Factory(mediaDataSourceFactory)
                            .createMediaSource(mediaItem);
            default ->
                // This is the MediaSource representing the media to be played.
                    new ProgressiveMediaSource.Factory(mediaDataSourceFactory)
                            .createMediaSource(mediaItem);
        };
    }

    private DataSource.Factory buildDataSourceFactory(boolean useBandwidthMeter) {
        return buildDataSourceFactory(useBandwidthMeter ? BANDWIDTH_METER : null);
    }

    public DataSource.Factory buildDataSourceFactory(DefaultBandwidthMeter bandwidthMeter) {
        return new DefaultDataSourceFactory(EPGTwoActivity.this, bandwidthMeter,
                buildHttpDataSourceFactory(bandwidthMeter));
    }

    public HttpDataSource.Factory buildHttpDataSourceFactory(DefaultBandwidthMeter bandwidthMeter) {
        CookieManager cookieManager = new CookieManager();
        cookieManager.setCookiePolicy(CookiePolicy.ACCEPT_ORIGINAL_SERVER);
        CookieHandler.setDefault(cookieManager);
        return new DefaultHttpDataSource.Factory().setUserAgent(spHelper.getAgentName().isEmpty() ? Util.getUserAgent(EPGTwoActivity.this, "ExoPlayerDemo") : spHelper.getAgentName())
                .setTransferListener(bandwidthMeter)
                .setAllowCrossProtocolRedirects(true)
                .setKeepPostFor302Redirects(true);
    }

    @Override
    public void onStop() {
        super.onStop();
        playWhenReady(false);
    }

    @RequiresApi(api = Build.VERSION_CODES.N)
    @Override
    public void onPause() {
        super.onPause();
        playWhenReady(false);
    }

    @Override
    public void onResume() {
        super.onResume();
        playWhenReady(true);
    }

    @Override
    protected void onRestart() {
        super.onRestart();
        playWhenReady(true);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        releasePlayer();
    }

    private void releasePlayer() {
        try {
            if (exoPlayer != null) {
                exoPlayer.clearMediaItems();
                exoPlayer.release();
                exoPlayer = null;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void playWhenReady(boolean setPlayWhenReady) {
        try {
            if (exoPlayer != null) {
                if (setPlayWhenReady){
                    exoPlayer.setPlayWhenReady(true);
                    exoPlayer.getPlaybackState();
                } else {
                    if (exoPlayer.getPlayWhenReady()){
                        exoPlayer.setPlayWhenReady(false);
                        exoPlayer.getPlaybackState();
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        switch (keyCode) {
            case KeyEvent.KEYCODE_BACK:
                finish();
                break;
            case KeyEvent.KEYCODE_HOME:
                ApplicationUtil.openHomeActivity(this);
                break;
            case KeyEvent.KEYCODE_UNKNOWN:
                return super.onKeyDown(keyCode, event);
            default:
                break;
        }
        return super.onKeyDown(keyCode, event);
    }
}