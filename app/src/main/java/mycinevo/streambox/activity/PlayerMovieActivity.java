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
import android.view.KeyEvent;
import android.view.View;
import android.view.accessibility.CaptioningManager;
import android.widget.ImageView;
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
import androidx.media3.ui.AspectRatioFrameLayout;
import androidx.media3.ui.PlayerControlView;
import androidx.media3.ui.PlayerView;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;

import java.lang.reflect.Field;
import java.net.CookieHandler;
import java.net.CookieManager;
import java.net.CookiePolicy;
import java.util.Locale;

import mycinevo.streambox.R;
import mycinevo.streambox.dialog.DialogUtil;
import mycinevo.streambox.dialog.Toasty;
import mycinevo.streambox.item.ItemMovies;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.NetworkUtils;
import mycinevo.streambox.util.helper.DBHelper;
import mycinevo.streambox.util.helper.SPHelper;
import mycinevo.streambox.util.player.BrightnessVolumeControl;
import mycinevo.streambox.util.player.CustomDefaultTrackNameProvider;
import mycinevo.streambox.util.player.CustomPlayerView;

@UnstableApi
public class PlayerMovieActivity extends AppCompatActivity {

    private int playback = 0;
    private String stream_id="", container=".mp4", movie_name="", stream_rating="", stream_icon="";
    private SimpleExoPlayer exoPlayer;
    private CustomPlayerView playerView;
    private DefaultBandwidthMeter BANDWIDTH_METER;
    private DataSource.Factory mediaDataSourceFactory;
    private DBHelper dbHelper;
    private SPHelper spHelper;
    private ProgressBar pb_player;
    private TextView tv_player_title;
    private BroadcastReceiver batteryReceiver;
    private ImageView exo_resize;
    public static boolean controllerVisible;
    public static boolean controllerVisibleFully;

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

        stream_id = getIntent().getStringExtra("stream_id");
        container = getIntent().getStringExtra("container");
        movie_name = getIntent().getStringExtra("movie_name");
        stream_rating = getIntent().getStringExtra("stream_rating");
        stream_icon = getIntent().getStringExtra("stream_icon");

        dbHelper = new DBHelper(this);
        spHelper = new SPHelper(this);

        pb_player = findViewById(R.id.pb_player);
        tv_player_title = findViewById(R.id.tv_player_title);

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
        playerView.setShowVrButton(spHelper.getIsVR());
        playerView.setShowSubtitleButton(spHelper.getIsSubtitle());
        playerView.setShowFastForwardButton(true);
        playerView.setShowRewindButton(true);
        playerView.setShowNextButton(false);
        playerView.setShowPreviousButton(false);
        playerView.setControllerHideOnTouch(false);
        playerView.setControllerAutoShow(true);

        // Set controller visibility listener
        playerView.setControllerVisibilityListener((PlayerView.ControllerVisibilityListener) visibility -> {
            controllerVisible = visibility == View.VISIBLE;
            controllerVisibleFully = playerView.isControllerFullyVisible();

            findViewById(R.id.rl_player_movie_top).setVisibility(visibility);

            // https://developer.android.com/training/system-ui/immersive
            IfSupported.toggleSystemUi(PlayerMovieActivity.this, playerView, visibility == View.VISIBLE);
            if (visibility == View.VISIBLE) {
                // Because when using dpad controls, focus resets to first item in bottom controls bar
                findViewById(R.id.exo_play_pause).requestFocus();
            }
        });

        playerView.setBrightnessControl(new BrightnessVolumeControl(this));

        // Set custom track name provider for control view
        try {
            PlayerControlView controlView = playerView.findViewById(R.id.exo_controller);
            playerView.findViewById(R.id.exo_settings).setVisibility(View.GONE);
            CustomDefaultTrackNameProvider customDefaultTrackNameProvider = new CustomDefaultTrackNameProvider(getResources());
            final Field field = PlayerControlView.class.getDeclaredField("trackNameProvider");
            field.setAccessible(true);
            field.set(controlView, customDefaultTrackNameProvider);
        } catch (NoSuchFieldException | IllegalAccessException e) {
            e.printStackTrace();
        }

        // Set media source
        setMediaSource(dbHelper.getSeek(DBHelper.TABLE_SEEK_MOVIE,stream_id, movie_name));

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
                    pb_player.setVisibility(View.GONE);
                    playback = 1;
                } else if (playbackState == Player.STATE_BUFFERING) {
                    pb_player.setVisibility(View.VISIBLE);
                } else if (playbackState == Player.STATE_ENDED) {
                    removeSeekMovie();
                }
            }
            @Override
            public void onPlayerError(@NonNull PlaybackException error) {
                Player.Listener.super.onPlayerError(error);
                if (playback < 5){
                    playback = playback + 1;
                    Toast.makeText(PlayerMovieActivity.this,"Playback error - "+ String.valueOf(playback)+"/5 " + error.getMessage(), Toast.LENGTH_SHORT).show();
                    setMediaSource(dbHelper.getSeek(DBHelper.TABLE_SEEK_MOVIE,stream_id, movie_name));
                } else {
                    playback = 1;
                    exoPlayer.stop();
                    pb_player.setVisibility(View.GONE);
                    Toasty.makeText(PlayerMovieActivity.this, "Failed : " + error.getErrorCodeName(), Toasty.ERROR);
                }
            }
        });

        exo_resize = findViewById(R.id.exo_resize);
        exo_resize.setOnClickListener(firstListener);

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

        findViewById(R.id.iv_media_info).setOnClickListener(v -> {
            if (exoPlayer != null && exoPlayer.getPlayWhenReady() && exoPlayer.getVideoFormat() != null){
                playerView.hideController();
                DialogUtil.DialogPlayerInfo(this, exoPlayer, false);
            } else {
                Toasty.makeText(this,getString(R.string.please_wait_a_minute), Toasty.ERROR);
            }
        });

        findViewById(R.id.iv_back_player).setOnClickListener(v -> finish());
        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.iv_back_player).setVisibility(View.GONE);
        }
    }

    private void removeSeekMovie() {
        try {
            dbHelper.removeSeekID(DBHelper.TABLE_SEEK_MOVIE,stream_id, movie_name);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void setMediaSource(int currentPosition) {
        if (NetworkUtils.isConnected(this)){
            if (spHelper.isLogged()){
                tv_player_title.setText(movie_name);
                String channelUrl = spHelper.getServerURL()+"movie/"+ spHelper.getUserName()+"/"+ spHelper.getPassword()+"/"+stream_id+"."+container;
                Uri uri = Uri.parse(channelUrl);

                MediaSource mediaSource = buildMediaSource(uri);
                exoPlayer.setMediaSource(mediaSource);
                exoPlayer.seekTo(currentPosition);
                exoPlayer.prepare();
                exoPlayer.setPlayWhenReady(true);

                try {
                    ItemMovies itemMovies = new ItemMovies(movie_name,stream_id,stream_icon,stream_rating,"");
                    dbHelper.addToMovie(DBHelper.TABLE_RECENT_MOVIE, itemMovies, spHelper.getMovieLimit());
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        } else {
            Toasty.makeText(PlayerMovieActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
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
        return new DefaultDataSourceFactory(PlayerMovieActivity.this, bandwidthMeter, buildHttpDataSourceFactory(bandwidthMeter));
    }

    public HttpDataSource.Factory buildHttpDataSourceFactory(DefaultBandwidthMeter bandwidthMeter) {
        CookieManager cookieManager = new CookieManager();
        cookieManager.setCookiePolicy(CookiePolicy.ACCEPT_ORIGINAL_SERVER);
        CookieHandler.setDefault(cookieManager);
        return new DefaultHttpDataSource.Factory().setUserAgent(spHelper.getAgentName().isEmpty() ? Util.getUserAgent(PlayerMovieActivity.this, "ExoPlayerDemo") : spHelper.getAgentName())
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

    private long getCurrentSeekPosition() {
        return exoPlayer.getCurrentPosition();
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_player_movie;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
    }

    @Override
    public void onStop() {
        super.onStop();
        if (exoPlayer != null && exoPlayer.getPlayWhenReady()) {
            exoPlayer.setPlayWhenReady(false);
            exoPlayer.getPlaybackState();
        }
    }

    @RequiresApi(api = Build.VERSION_CODES.N)
    @Override
    public void onPause() {
        super.onPause();
        if (exoPlayer != null && exoPlayer.getPlayWhenReady()) {
            exoPlayer.setPlayWhenReady(false);
            exoPlayer.getPlaybackState();
        }
    }

    @Override
    public void onResume() {
        super.onResume();
        if (exoPlayer != null) {
            exoPlayer.setPlayWhenReady(true);
            exoPlayer.getPlaybackState();
        }
    }

    @Override
    protected void onRestart() {
        super.onRestart();
        if (exoPlayer != null) {
            exoPlayer.setPlayWhenReady(true);
            exoPlayer.getPlaybackState();
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        try {
            try {
                if (exoPlayer != null){
                    dbHelper.addToSeek(DBHelper.TABLE_SEEK_MOVIE, String.valueOf(getCurrentSeekPosition()), stream_id, movie_name);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
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
        switch (keyCode) {
            case KeyEvent.KEYCODE_MEDIA_PLAY:
            case KeyEvent.KEYCODE_MEDIA_PAUSE:
            case KeyEvent.KEYCODE_MEDIA_PLAY_PAUSE:
            case KeyEvent.KEYCODE_BUTTON_SELECT:
                if (exoPlayer == null)
                    break;
                if (keyCode == KeyEvent.KEYCODE_MEDIA_PAUSE) {
                    exoPlayer.pause();
                } else if (keyCode == KeyEvent.KEYCODE_MEDIA_PLAY) {
                    exoPlayer.play();
                } else if (exoPlayer.isPlaying()) {
                    exoPlayer.pause();
                } else {
                    exoPlayer.play();
                }
                return true;
            case KeyEvent.KEYCODE_VOLUME_UP:
            case KeyEvent.KEYCODE_VOLUME_DOWN:
                return true;
            case KeyEvent.KEYCODE_BUTTON_START:
            case KeyEvent.KEYCODE_BUTTON_A:
            case KeyEvent.KEYCODE_ENTER:
            case KeyEvent.KEYCODE_DPAD_CENTER:
            case KeyEvent.KEYCODE_NUMPAD_ENTER:
            case KeyEvent.KEYCODE_SPACE:
                if (exoPlayer == null)
                    break;
                if (!controllerVisibleFully) {
                    if (exoPlayer.isPlaying()) {
                        exoPlayer.pause();
                    } else {
                        exoPlayer.play();
                    }
                    return true;
                }
                break;
            case KeyEvent.KEYCODE_DPAD_LEFT:
            case KeyEvent.KEYCODE_BUTTON_L2:
            case KeyEvent.KEYCODE_MEDIA_REWIND:
                if (!controllerVisibleFully || keyCode == KeyEvent.KEYCODE_MEDIA_REWIND) {
                    if (exoPlayer == null)
                        break;
                    seekBy((long) -10 * 1000);
                    return true;
                }
                break;
            case KeyEvent.KEYCODE_DPAD_RIGHT:
            case KeyEvent.KEYCODE_BUTTON_R2:
            case KeyEvent.KEYCODE_MEDIA_FAST_FORWARD:
                if (!controllerVisibleFully || keyCode == KeyEvent.KEYCODE_MEDIA_FAST_FORWARD) {
                    if (exoPlayer == null)
                        break;
                    seekBy((long) 10 * 1000);
                    return true;
                }
                break;
            case KeyEvent.KEYCODE_BACK:
                if (ApplicationUtil.isTvBox(this)) {
                    if (controllerVisible && exoPlayer != null && exoPlayer.isPlaying()) {
                        playerView.hideController();
                        return true;
                    } else {
                        finish();
                    }
                }
                break;
            case KeyEvent.KEYCODE_UNKNOWN:
                return super.onKeyDown(keyCode, event);
            default:
                if (!controllerVisibleFully) {
                    playerView.showController();
                    return true;
                }
                break;
        }
        return super.onKeyDown(keyCode, event);
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
}