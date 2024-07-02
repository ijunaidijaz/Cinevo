package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ActivityInfo;
import android.media.metrics.PlaybackStateEvent;
import android.net.Uri;
import android.os.BatteryManager;
import android.os.Build;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.view.accessibility.CaptioningManager;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.RequiresApi;
import androidx.media3.common.AudioAttributes;
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
import androidx.media3.exoplayer.DefaultRenderersFactory;
import androidx.media3.exoplayer.SimpleExoPlayer;
import androidx.media3.exoplayer.dash.DashMediaSource;
import androidx.media3.exoplayer.dash.DefaultDashChunkSource;
import androidx.media3.exoplayer.hls.HlsMediaSource;
import androidx.media3.exoplayer.rtsp.RtspMediaSource;
import androidx.media3.exoplayer.smoothstreaming.DefaultSsChunkSource;
import androidx.media3.exoplayer.smoothstreaming.SsMediaSource;
import androidx.media3.exoplayer.source.DefaultMediaSourceFactory;
import androidx.media3.exoplayer.source.MediaSource;
import androidx.media3.exoplayer.source.ProgressiveMediaSource;
import androidx.media3.exoplayer.trackselection.DefaultTrackSelector;
import androidx.media3.exoplayer.upstream.DefaultBandwidthMeter;
import androidx.media3.extractor.DefaultExtractorsFactory;
import androidx.media3.ui.AspectRatioFrameLayout;
import androidx.media3.ui.PlayerView;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;

import java.net.CookieHandler;
import java.net.CookieManager;
import java.net.CookiePolicy;
import java.util.ArrayList;
import java.util.Locale;

import nemosofts.streambox.R;
import nemosofts.streambox.asyncTask.LoadEpg;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.dialog.PlayerLiveListDialog;
import nemosofts.streambox.dialog.Toasty;
import nemosofts.streambox.interfaces.EpgListener;
import nemosofts.streambox.item.ItemEpg;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.NetworkUtils;
import nemosofts.streambox.util.helper.SPHelper;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.player.BrightnessVolumeControl;
import nemosofts.streambox.util.player.CustomPlayerView;

@UnstableApi
public class PlayerLiveActivity extends AppCompatActivity {

    private int playback = 0;
    private SimpleExoPlayer exoPlayer;
    private CustomPlayerView playerView;
    private DefaultBandwidthMeter BANDWIDTH_METER;
    private DataSource.Factory mediaDataSourceFactory;
    private ImageView iv_play, iv_player_fav, btnTryAgain;
    private Helper helper;
    private DBHelper dbHelper;
    private SPHelper spHelper;
    private TextView tv_player_title;
    private ProgressBar pb_player;
    private PlayerLiveListDialog listDialog;
    private BroadcastReceiver batteryReceiver;
    private LinearLayout exo_resize;

    private RelativeLayout rl_player_epg;
    private TextView tv_epg_title, tv_epg_time;

    private static final CookieManager DEFAULT_COOKIE_MANAGER;
    static {
        DEFAULT_COOKIE_MANAGER = new CookieManager();
        DEFAULT_COOKIE_MANAGER.setCookiePolicy(CookiePolicy.ACCEPT_ORIGINAL_SERVER);
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.IsRTL(this);
        IfSupported.IsScreenshot(this);
        IfSupported.hideBottomBar(this);
        IfSupported.statusBarBlackColor(this);

        helper = new Helper(this);
        spHelper = new SPHelper(this);
        dbHelper = new DBHelper(this);

        listDialog = new PlayerLiveListDialog(this, position -> {
            Callback.playPosLive = position;
            setMediaSource();
        });

        tv_player_title = findViewById(R.id.tv_player_title);
        pb_player = findViewById(R.id.pb_player);
        btnTryAgain = findViewById(R.id.iv_reset);
        iv_play = findViewById(R.id.iv_play);
        iv_player_fav = findViewById(R.id.iv_player_fav);
        rl_player_epg = findViewById(R.id.rl_player_epg);
        tv_epg_title = findViewById(R.id.tv_epg_title);
        tv_epg_time = findViewById(R.id.tv_epg_time);

        BANDWIDTH_METER = new DefaultBandwidthMeter.Builder(this).build();
        mediaDataSourceFactory = buildDataSourceFactory(true);

        // Set default cookie manager if not already set
        if (CookieHandler.getDefault() != DEFAULT_COOKIE_MANAGER) {
            CookieHandler.setDefault(DEFAULT_COOKIE_MANAGER);
        }

        // https://github.com/google/ExoPlayer/issues/8571
        DefaultExtractorsFactory extractorsFactory = ApplicationUtil.getDefaultExtractorsFactory();
        DefaultRenderersFactory renderersFactory = ApplicationUtil.getDefaultRenderersFactory(this);

        DefaultTrackSelector trackSelector = new DefaultTrackSelector(this);

        // Set captioning parameters if enabled
        final CaptioningManager captioningManager = (CaptioningManager) getSystemService(Context.CAPTIONING_SERVICE);
        if (!captioningManager.isEnabled()) {
            trackSelector.setParameters(trackSelector.buildUponParameters().setIgnoredTextSelectionFlags(C.SELECTION_FLAG_DEFAULT));
        }
        Locale locale = captioningManager.getLocale();
        if (locale != null) {
            trackSelector.setParameters(trackSelector.buildUponParameters().setPreferredTextLanguage(locale.getISO3Language()));
        }

        // Build ExoPlayer instance
        exoPlayer = new SimpleExoPlayer.Builder(this, renderersFactory)
                .setTrackSelector(trackSelector)
                .setMediaSourceFactory(new DefaultMediaSourceFactory(this, extractorsFactory))
                .build();

        // Set audio attributes for the player
        AudioAttributes audioAttributes = new AudioAttributes.Builder()
                .setUsage(C.USAGE_MEDIA)
                .setContentType(C.AUDIO_CONTENT_TYPE_MOVIE)
                .build();
        exoPlayer.setAudioAttributes(audioAttributes, true);

        // Attach ExoPlayer to the player view
        playerView = findViewById(R.id.nSoftsPlayerView);
        playerView.setPlayer(exoPlayer);
        playerView.setUseController(true);
        playerView.requestFocus();
        playerView.setControllerHideOnTouch(false);
        playerView.setControllerAutoShow(true);
        playerView.setBrightnessControl(new BrightnessVolumeControl(this));

        // Set controller visibility listener
        playerView.setControllerVisibilityListener((PlayerView.ControllerVisibilityListener) visibility -> {
            // https://developer.android.com/training/system-ui/immersive
            IfSupported.toggleSystemUi(PlayerLiveActivity.this, playerView, visibility == View.VISIBLE);
        });

        // Set media source
        setMediaSource();

        // Set player event listeners
        exoPlayer.addListener(new Player.Listener() {

            @Override
            public void onIsPlayingChanged(boolean isPlaying) {
                Player.Listener.super.onIsPlayingChanged(isPlaying);
                playerView.setKeepScreenOn(isPlaying);
            }

            @Override
            public void onPlayerStateChanged(boolean playWhenReady, int playbackState) {
                Player.Listener.super.onPlayerStateChanged(playWhenReady, playbackState);
                if (playbackState == PlaybackStateEvent.STATE_PLAYING) {
                    iv_play.setImageResource(R.drawable.ic_pause);
                    pb_player.setVisibility(View.GONE);
                    playback = 1;
                } else if (playbackState == Player.STATE_BUFFERING) {
                    pb_player.setVisibility(View.VISIBLE);
                }
            }

            @Override
            public void onPlayerError(@NonNull PlaybackException error) {
                Player.Listener.super.onPlayerError(error);
                if (playback < 5){
                    playback = playback + 1;
                    Toast.makeText(PlayerLiveActivity.this,"Playback error - "+ String.valueOf(playback)+"/5 " + error.getMessage(), Toast.LENGTH_SHORT).show();
                    setMediaSource();
                } else {
                    playback = 1;
                    exoPlayer.stop();
                    btnTryAgain.setVisibility(View.VISIBLE);
                    pb_player.setVisibility(View.GONE);
                    iv_play.setImageResource(R.drawable.ic_play);
                    iv_play.setVisibility(View.GONE);
                    Toasty.makeText(PlayerLiveActivity.this, "Failed : " + error.getErrorCodeName(), Toasty.ERROR);
                }
            }
        });

        btnTryAgain.setOnClickListener(v -> {
            btnTryAgain.setVisibility(View.GONE);
            pb_player.setVisibility(View.VISIBLE);
            setMediaSource();
        });

        exo_resize = findViewById(R.id.ll_aspect_ratio);
        exo_resize.setOnClickListener(firstListener);

        iv_play.setOnClickListener(v -> {
            exoPlayer.setPlayWhenReady(!exoPlayer.getPlayWhenReady());
            iv_play.setImageResource(Boolean.TRUE.equals(exoPlayer.getPlayWhenReady()) ? R.drawable.ic_pause : R.drawable.ic_play);
        });
        if (ApplicationUtil.isTvBox(this)){
            iv_play.requestFocus();
        }

        iv_player_fav.setOnClickListener(v -> {
            if (Boolean.TRUE.equals(dbHelper.checkLive(DBHelper.TABLE_FAV_LIVE, Callback.arrayListLive.get(Callback.playPosLive).getStreamID()))){
                dbHelper.removeLive(DBHelper.TABLE_FAV_LIVE, Callback.arrayListLive.get(Callback.playPosLive).getStreamID());
                iv_player_fav.setImageResource(R.drawable.ic_favorite_border);
                Toast.makeText(PlayerLiveActivity.this, getString(R.string.fav_remove_success), Toast.LENGTH_SHORT).show();
            } else {
                dbHelper.addToLive(DBHelper.TABLE_FAV_LIVE, Callback.arrayListLive.get(Callback.playPosLive), 0);
                iv_player_fav.setImageResource(R.drawable.ic_favorite);
                Toast.makeText(PlayerLiveActivity.this, getString(R.string.fav_success), Toast.LENGTH_SHORT).show();
            }
        });

        findViewById(R.id.iv_media_info).setOnClickListener(v -> {
            if (exoPlayer != null && exoPlayer.getPlayWhenReady() && exoPlayer.getVideoFormat() != null){
                playerView.hideController();
                DialogUtil.DialogPlayerInfo(this, exoPlayer, true);
            } else {
                Toasty.makeText(this,getString(R.string.please_wait_a_minute), Toasty.ERROR);
            }
        });


        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
            iv_player_fav.setVisibility(View.INVISIBLE);
        }
        findViewById(R.id.ll_multiple_screen).setOnClickListener(v -> {
            Intent intent = new Intent(PlayerLiveActivity.this, MultipleScreenActivity.class);
            intent.putExtra("is_player", true);
            intent.putExtra("stream_id", Callback.arrayListLive.get(Callback.playPosLive).getStreamID());
            startActivity(intent);
            finish();
        });
        findViewById(R.id.iv_previous).setOnClickListener(v -> previous());
        findViewById(R.id.iv_next).setOnClickListener(v -> next());

        ImageView battery_info = findViewById(R.id.iv_battery_info);
        if (Boolean.FALSE.equals(ApplicationUtil.isTvBox(this))){
            batteryReceiver = new BroadcastReceiver() {
                @Override
                public void onReceive(Context context, Intent intent) {
                    int status = intent.getIntExtra(BatteryManager.EXTRA_STATUS, -1);
                    int level = intent.getIntExtra(BatteryManager.EXTRA_LEVEL, -1);
                    int scale = intent.getIntExtra(BatteryManager.EXTRA_SCALE, -1);
                    battery_info.setImageResource(ApplicationUtil.getBatteryDrawable(status,level,scale));
                }
            };
            IntentFilter filter = new IntentFilter(Intent.ACTION_BATTERY_CHANGED);
            registerReceiver(batteryReceiver, filter);
        } else {
            battery_info.setVisibility(View.INVISIBLE);
        }

        findViewById(R.id.iv_back_player).setOnClickListener(v -> onBackPressed());
        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.iv_back_player).setVisibility(View.GONE);
        }
    }

    public void next() {
        if (!Callback.arrayListLive.isEmpty()) {
            if (NetworkUtils.isConnected(this)) {
                if (Callback.playPosLive < (Callback.arrayListLive.size() - 1)) {
                    Callback.playPosLive = Callback.playPosLive + 1;
                } else {
                    Callback.playPosLive = 0;
                }
                setMediaSource();
            } else {
                Toasty.makeText(PlayerLiveActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            }
        }
    }

    public void previous() {
        if (!Callback.arrayListLive.isEmpty()) {
            if (NetworkUtils.isConnected(this)) {
                if (Callback.playPosLive > 0) {
                    Callback.playPosLive = Callback.playPosLive - 1;
                } else {
                    Callback.playPosLive = Callback.arrayListLive.size() - 1;
                }
                setMediaSource();
            } else {
                Toasty.makeText(PlayerLiveActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            }
        }
    }

    private void setMediaSource() {
        if (NetworkUtils.isConnected(this)){
            if (!Callback.arrayListLive.isEmpty() && spHelper.isLogged() || spHelper.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){

                findViewById(R.id.ll_channels_list).setOnClickListener(view -> {
                    playerView.hideController();
                    listDialog.showDialog();
                });

                if (btnTryAgain.getVisibility() == View.VISIBLE){
                    btnTryAgain.setVisibility(View.GONE);
                }

                tv_player_title.setText(Callback.arrayListLive.get(Callback.playPosLive).getName());
                if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_ONE_UI) || spHelper.getLoginType().equals(Callback.TAG_LOGIN_STREAM)){
                    iv_player_fav.setImageResource(Boolean.TRUE.equals(dbHelper.checkLive(DBHelper.TABLE_FAV_LIVE, Callback.arrayListLive.get(Callback.playPosLive).getStreamID())) ? R.drawable.ic_favorite : R.drawable.ic_favorite_border);
                }

                String channelUrl;
                if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
                    channelUrl = Callback.arrayListLive.get(Callback.playPosLive).getStreamID();
                    rl_player_epg.setVisibility(View.GONE);
                } else {
                    String format = ".m3u8";
                    if (spHelper.getLiveFormat() == 1){
                        format = ".ts";
                    }
                    if (Boolean.TRUE.equals(spHelper.getIsXuiUser())){
                        channelUrl = spHelper.getServerURL()+ spHelper.getUserName()+"/"+ spHelper.getPassword()+"/"+Callback.arrayListLive.get(Callback.playPosLive).getStreamID()+format;
                    } else {
                        channelUrl = spHelper.getServerURL()+"live/"+ spHelper.getUserName()+"/"+ spHelper.getPassword()+"/"+Callback.arrayListLive.get(Callback.playPosLive).getStreamID()+format;
                    }
                    getEpgData();
                }

                Uri uri = Uri.parse(channelUrl);
                MediaSource mediaSource = buildMediaSource(uri);
                exoPlayer.setMediaSource(mediaSource);
                exoPlayer.prepare();
                exoPlayer.setPlayWhenReady(true);
                iv_play.setImageResource(R.drawable.ic_pause);
                iv_play.setVisibility(View.VISIBLE);

                if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_ONE_UI) || spHelper.getLoginType().equals(Callback.TAG_LOGIN_STREAM)){
                    try {
                        dbHelper.addToLive(DBHelper.TABLE_RECENT_LIVE, Callback.arrayListLive.get(Callback.playPosLive), spHelper.getLiveLimit());
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        } else {
            Toasty.makeText(PlayerLiveActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
    }

    private void getEpgData() {
        if (NetworkUtils.isConnected(this)){
            LoadEpg loadSeriesID = new LoadEpg(this, new EpgListener() {
                @Override
                public void onStart() {
                    rl_player_epg.setVisibility(View.GONE);
                    tv_epg_title.setText("");
                    tv_epg_time.setText("");
                }

                @SuppressLint("SetTextI18n")
                @Override
                public void onEnd(String success, ArrayList<ItemEpg> epgArrayList) {
                    if (!isFinishing()){
                        if (!epgArrayList.isEmpty()){
                            tv_epg_title.setText(ApplicationUtil.decodeBase64(epgArrayList.get(0).getTitle()));
                            tv_epg_time.setText(ApplicationUtil.getTimestamp(epgArrayList.get(0).getStartTimestamp()) + " - " + ApplicationUtil.getTimestamp(epgArrayList.get(0).getStopTimestamp()));
                            rl_player_epg.setVisibility(View.VISIBLE);
                        }
                    }
                }
            }, helper.getAPIRequestID("get_short_epg","stream_id", Callback.arrayListLive.get(Callback.playPosLive).getStreamID(), spHelper.getUserName(), spHelper.getPassword()));
            loadSeriesID.execute();
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
        return new DefaultDataSourceFactory(PlayerLiveActivity.this, bandwidthMeter,
                buildHttpDataSourceFactory(bandwidthMeter));
    }

    public HttpDataSource.Factory buildHttpDataSourceFactory(DefaultBandwidthMeter bandwidthMeter) {
        CookieManager cookieManager = new CookieManager();
        cookieManager.setCookiePolicy(CookiePolicy.ACCEPT_ORIGINAL_SERVER);
        CookieHandler.setDefault(cookieManager);
        return new DefaultHttpDataSource.Factory().setUserAgent(spHelper.getAgentName().isEmpty() ? Util.getUserAgent(PlayerLiveActivity.this, "ExoPlayerDemo") : spHelper.getAgentName())
                .setTransferListener(bandwidthMeter)
                .setAllowCrossProtocolRedirects(true)
                .setKeepPostFor302Redirects(true);
    }

    View.OnClickListener firstListener = new View.OnClickListener() {
        @Override
        public void onClick(View v) {
            playerView.setResizeMode(AspectRatioFrameLayout.RESIZE_MODE_FILL);
            exoPlayer.setVideoScalingMode(C.VIDEO_SCALING_MODE_DEFAULT);
            playerView.showController();
            ApplicationUtil.showText(playerView, "Full Scree");
            exo_resize.setOnClickListener(secondListener);
        }
    };
    View.OnClickListener secondListener = new View.OnClickListener() {
        @Override
        public void onClick(View v) {
            playerView.setResizeMode(AspectRatioFrameLayout.RESIZE_MODE_ZOOM);
            exoPlayer.setVideoScalingMode(C.VIDEO_SCALING_MODE_DEFAULT);
            playerView.showController();
            ApplicationUtil.showText(playerView, "Zoom");
            exo_resize.setOnClickListener(thirdListener);
        }
    };
    View.OnClickListener thirdListener = new View.OnClickListener() {
        @Override
        public void onClick(View v) {
            playerView.setResizeMode(AspectRatioFrameLayout.RESIZE_MODE_FIT);
            exoPlayer.setVideoScalingMode(C.VIDEO_SCALING_MODE_DEFAULT);
            playerView.showController();
            ApplicationUtil.showText(playerView, "Fit");
            exo_resize.setOnClickListener(firstListener);
        }
    };

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_player_live;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
    }

    @Override
    public void onStop() {
        super.onStop();
        try {
            if (exoPlayer != null && exoPlayer.getPlayWhenReady()) {
                exoPlayer.setPlayWhenReady(false);
                exoPlayer.getPlaybackState();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @RequiresApi(api = Build.VERSION_CODES.N)
    @Override
    public void onPause() {
        super.onPause();
        try {
            if (exoPlayer != null && exoPlayer.getPlayWhenReady()) {
                exoPlayer.setPlayWhenReady(false);
                exoPlayer.getPlaybackState();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public void onResume() {
        super.onResume();
        try {
            if (exoPlayer != null) {
                exoPlayer.setPlayWhenReady(true);
                exoPlayer.getPlaybackState();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    protected void onRestart() {
        super.onRestart();
        try {
            if (exoPlayer != null) {
                exoPlayer.setPlayWhenReady(true);
                exoPlayer.getPlaybackState();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        try {
            if (exoPlayer != null) {
                exoPlayer.setPlayWhenReady(false);
                exoPlayer.stop();
                exoPlayer.release();
            }
            if (batteryReceiver != null){
                unregisterReceiver(batteryReceiver);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN){
            if (keyCode == KeyEvent.KEYCODE_BACK){
                onBackPressed();
                return true;
            } else if (keyCode == KeyEvent.KEYCODE_HOME){
                ApplicationUtil.openHomeActivity(this);
                return true;
            } else if (keyCode == KeyEvent.KEYCODE_MEDIA_PLAY || keyCode == KeyEvent.KEYCODE_MEDIA_PAUSE || keyCode == KeyEvent.KEYCODE_MEDIA_PLAY_PAUSE) {
                if (exoPlayer != null) {
                    exoPlayer.setPlayWhenReady(!exoPlayer.getPlayWhenReady());
                    iv_play.setImageResource(Boolean.TRUE.equals(exoPlayer.getPlayWhenReady()) ? R.drawable.ic_pause : R.drawable.ic_play);
                }
                return true;
            } else if (keyCode == KeyEvent.KEYCODE_MEDIA_NEXT) {
                next();
                return true;
            } else if (keyCode == KeyEvent.KEYCODE_MEDIA_PREVIOUS) {
                previous();
                return true;
            } else if (keyCode == KeyEvent.KEYCODE_MEDIA_STOP) {
                if (exoPlayer != null && exoPlayer.getPlayWhenReady()) {
                    exoPlayer.setPlayWhenReady(false);
                    exoPlayer.getPlaybackState();
                }
                return true;
            } else {
                return super.onKeyDown(keyCode, event);
            }
        } else {
            return super.onKeyDown(keyCode, event);
        }
    }

    @Override
    public void onBackPressed() {
        if (listDialog.isShowing()) {
            listDialog.dismissDialog();
        } else {
            super.onBackPressed();
        }
    }
}