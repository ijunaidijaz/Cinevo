package mycinevo.streambox.activity;

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
import android.os.Handler;
import android.view.KeyEvent;
import android.view.View;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
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
import androidx.media3.extractor.ts.DefaultTsPayloadReaderFactory;
import androidx.media3.extractor.ts.TsExtractor;
import androidx.media3.ui.AspectRatioFrameLayout;
import androidx.media3.ui.PlayerView;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;

import java.net.CookieHandler;
import java.net.CookieManager;
import java.net.CookiePolicy;

import mycinevo.streambox.R;
import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.dialog.PlayerEpisodesListDialog;
import mycinevo.streambox.dialog.Toasty;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.NetworkUtils;
import mycinevo.streambox.util.SharedPref;
import mycinevo.streambox.util.player.BrightnessVolumeControl;
import mycinevo.streambox.util.player.CustomPlayerView;

@UnstableApi
public class PlayerEpisodesActivity extends AppCompatActivity {

    private int playback = 0;
    private SimpleExoPlayer exoPlayer;
    private CustomPlayerView playerView;
    private DefaultBandwidthMeter BANDWIDTH_METER;
    private DataSource.Factory mediaDataSourceFactory;
    private SharedPref sharedPref;
    private ProgressBar pb_player;
    private TextView tv_player_title;
    private LinearLayout ll_skip_next;
    private PlayerEpisodesListDialog listDialog;
    private BroadcastReceiver batteryReceiver;
    private ImageView exo_resize;

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
        IfSupported.hideBottomBar(this);

        sharedPref = new SharedPref(this);

        listDialog = new PlayerEpisodesListDialog(this, position -> {
            Callback.playPosEpisodes = position;
            setMediaSource();
        });

        pb_player = findViewById(R.id.pb_player);
        tv_player_title = findViewById(R.id.tv_player_title);
        ll_skip_next = findViewById(R.id.ll_skip_next);

        BANDWIDTH_METER = new DefaultBandwidthMeter.Builder(this).build();
        mediaDataSourceFactory = buildDataSourceFactory(true);
        if (CookieHandler.getDefault() != DEFAULT_COOKIE_MANAGER) {
            CookieHandler.setDefault(DEFAULT_COOKIE_MANAGER);
        }

        AudioAttributes audioAttributes = new AudioAttributes.Builder()
                .setUsage(C.USAGE_MEDIA)
                .setContentType(C.AUDIO_CONTENT_TYPE_MOVIE)
                .build();

        DefaultExtractorsFactory extractorsFactory = new DefaultExtractorsFactory()
                .setTsExtractorFlags(DefaultTsPayloadReaderFactory.FLAG_ENABLE_HDMV_DTS_AUDIO_STREAMS)
                .setTsExtractorTimestampSearchBytes(1500 * TsExtractor.TS_PACKET_SIZE);

        DefaultRenderersFactory renderersFactory = new DefaultRenderersFactory(this);
        renderersFactory.setExtensionRendererMode(DefaultRenderersFactory.EXTENSION_RENDERER_MODE_ON);

        exoPlayer = new SimpleExoPlayer.Builder(this, renderersFactory)
                .setTrackSelector(new DefaultTrackSelector(this))
                .setMediaSourceFactory(new DefaultMediaSourceFactory(this, extractorsFactory))
                .setAudioAttributes(audioAttributes, true)
                .build();

        playerView = findViewById(R.id.nSoftsPlayerView);
        playerView.setPlayer(exoPlayer);
        playerView.setShowVrButton(sharedPref.getIsVR());
        playerView.setShowSubtitleButton(sharedPref.getIsSubtitle());
        playerView.setShowFastForwardButton(true);
        playerView.setShowRewindButton(true);
        playerView.setShowNextButton(false);
        playerView.setShowPreviousButton(false);
        playerView.setControllerVisibilityListener((PlayerView.ControllerVisibilityListener) visibility -> {
            findViewById(R.id.rl_player_top).setVisibility(visibility);
            if (Callback.playPosEpisodes < (Callback.arrayListEpisodes.size())) {
                ll_skip_next.setVisibility(visibility);
            }
        });
        playerView.setControllerHideOnTouch(true);
        playerView.setControllerAutoShow(true);
        playerView.setBrightnessControl(new BrightnessVolumeControl(PlayerEpisodesActivity.this));

        setMediaSource();

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
                    pb_player.setVisibility(View.GONE);
                    playback = 1;
                } else if (playbackState == Player.STATE_BUFFERING) {
                    pb_player.setVisibility(View.VISIBLE);
                }
                else if (playbackState == Player.STATE_ENDED) {
                    onCompletion();
                }
            }
            @Override
            public void onPlayerError(@NonNull PlaybackException error) {
                Player.Listener.super.onPlayerError(error);
                if (playback < 5){
                    playback = playback + 1;
                    Toast.makeText(PlayerEpisodesActivity.this,"Playback error - "+ String.valueOf(playback)+"/5 " + error.getMessage(), Toast.LENGTH_SHORT).show();
                    setMediaSource();
                } else {
                    playback = 1;
                    exoPlayer.stop();
                    pb_player.setVisibility(View.GONE);
                    Toasty.makeText(PlayerEpisodesActivity.this, "Failed : " + error.getErrorCodeName(), Toasty.ERROR);
                }
            }
        });

        exo_resize = findViewById(R.id.exo_resize);
        exo_resize.setOnClickListener(firstListener);
        ll_skip_next.setOnClickListener(v -> next());

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

    private void onCompletion() {
        if (sharedPref.getIsAutoplayEpisode()){
            new Handler().postDelayed(this::next, 30);
        }
    }

    private void next() {
        if (Callback.playPosEpisodes < (Callback.arrayListEpisodes.size() - 1)) {
            Callback.playPosEpisodes = Callback.playPosEpisodes + 1;
            setMediaSource();
        } else {
            ll_skip_next.setVisibility(View.GONE);
        }
    }

    private void setMediaSource() {
        if (NetworkUtils.isConnected(this)){
            if (!Callback.arrayListEpisodes.isEmpty() && sharedPref.isLogged()){

                findViewById(R.id.exo_episodes).setOnClickListener(view -> {
                    playerView.hideController();
                    listDialog.showDialog();
                });

                tv_player_title.setText(Callback.arrayListEpisodes.get(Callback.playPosEpisodes).getTitle());
                String episodeUrl= sharedPref.getServerURL()+"series/"+sharedPref.getUserName()+"/"+sharedPref.getPassword()+"/"+Callback.arrayListEpisodes.get(Callback.playPosEpisodes).getId()+"."+Callback.arrayListEpisodes.get(Callback.playPosEpisodes).getContainerExtension();
                Uri uri = Uri.parse(episodeUrl);
                MediaSource mediaSource = buildMediaSource(uri);
                exoPlayer.setMediaSource(mediaSource);
                exoPlayer.seekTo(0);
                exoPlayer.prepare();
                exoPlayer.setPlayWhenReady(true);

                if (Callback.playPosEpisodes < (Callback.arrayListEpisodes.size())) {
                    ll_skip_next.setVisibility(View.VISIBLE);
                } else {
                    ll_skip_next.setVisibility(View.GONE);
                }
            }
        } else {
            Toasty.makeText(PlayerEpisodesActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
    }

    @SuppressLint("SwitchIntDef")
    @NonNull
    private MediaSource buildMediaSource(Uri uri) {
        int type = Util.inferContentType(uri);
        switch (type) {
            case C.TYPE_SS -> {
                return new SsMediaSource.Factory(new DefaultSsChunkSource.Factory(mediaDataSourceFactory), buildDataSourceFactory(false)).createMediaSource(MediaItem.fromUri(uri));
            }
            case C.TYPE_DASH -> {
                return new DashMediaSource.Factory(new DefaultDashChunkSource.Factory(mediaDataSourceFactory), buildDataSourceFactory(false)).createMediaSource(MediaItem.fromUri(uri));
            }
            case C.TYPE_HLS -> {
                return new HlsMediaSource.Factory(mediaDataSourceFactory).createMediaSource(MediaItem.fromUri(uri));
            }
            case C.TYPE_RTSP -> {
                return new RtspMediaSource.Factory().createMediaSource(MediaItem.fromUri(uri));
            }
            case C.TYPE_OTHER -> {
                return new ProgressiveMediaSource.Factory(mediaDataSourceFactory).createMediaSource(MediaItem.fromUri(uri));
            }
            default -> throw new IllegalStateException("Unsupported type: " + type);
        }
    }

    private DataSource.Factory buildDataSourceFactory(boolean useBandwidthMeter) {
        return buildDataSourceFactory(useBandwidthMeter ? BANDWIDTH_METER : null);
    }

    public DataSource.Factory buildDataSourceFactory(DefaultBandwidthMeter bandwidthMeter) {
        return new DefaultDataSourceFactory(this, bandwidthMeter,
                buildHttpDataSourceFactory(bandwidthMeter));
    }

    public HttpDataSource.Factory buildHttpDataSourceFactory(DefaultBandwidthMeter bandwidthMeter) {
        return new DefaultHttpDataSource.Factory().setUserAgent(sharedPref.getAgentName().isEmpty() ? Util.getUserAgent(PlayerEpisodesActivity.this, "ExoPlayerDemo") : sharedPref.getAgentName())
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
        return R.layout.activity_player_episodes;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
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
                }
                return true;
            } else if (keyCode == KeyEvent.KEYCODE_MEDIA_NEXT) {
                next();
                return true;
            } else if (keyCode == KeyEvent.KEYCODE_MEDIA_REWIND) {
                seekBy((long) -10 * 1000);
                return true;
            } else if (keyCode == KeyEvent.KEYCODE_MEDIA_FAST_FORWARD) {
                seekBy((long) 10 * 1000);
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

    private void seekBy(long positionMs) {
        try {
            if (exoPlayer != null) {
                long currentPosition = exoPlayer.getCurrentPosition();
                long newPosition = currentPosition + positionMs;
                // Ensure the new position is within the bounds of the media duration
                long duration = exoPlayer.getDuration();
                newPosition = Math.max(0, Math.min(newPosition, duration));
                // Seek to the new position
                exoPlayer.seekTo(newPosition);
            }
        } catch (Exception e) {
            e.printStackTrace();
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