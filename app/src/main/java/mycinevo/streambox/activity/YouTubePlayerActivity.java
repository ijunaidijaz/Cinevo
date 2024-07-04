package mycinevo.streambox.activity;

import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;

import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.YouTubePlayer;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.AbstractYouTubePlayerListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.YouTubePlayerListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.options.IFramePlayerOptions;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.utils.YouTubePlayerUtils;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.views.YouTubePlayerView;

import mycinevo.streambox.R;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.player.CustomYouTubePlayerView;

public class YouTubePlayerActivity extends AppCompatActivity {

    YouTubePlayerView youTubePlayerView;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        IfSupported.IsRTL(this);
        IfSupported.IsScreenshot(this);
        IfSupported.hideStatusBar(this);

        String stream_id = getIntent().getStringExtra("stream_id");

        youTubePlayerView = findViewById(R.id.youtube_player_view);
        youTubePlayerView.setEnableAutomaticInitialization(false);
        getLifecycle().addObserver(youTubePlayerView);

        View customPlayerUi = youTubePlayerView.inflateCustomPlayerUi(R.layout.custom_player_ui);

        YouTubePlayerListener listener = new AbstractYouTubePlayerListener() {
            @Override
            public void onReady(@NonNull YouTubePlayer youTubePlayer) {
                CustomYouTubePlayerView customYouTubePlayerView = new CustomYouTubePlayerView(customPlayerUi, youTubePlayer, youTubePlayerView);
                youTubePlayer.addListener(customYouTubePlayerView);
                if (stream_id != null) {
                    YouTubePlayerUtils.loadOrCueVideo(youTubePlayer, getLifecycle(), stream_id, 0F);
                }
            }
        };

        IFramePlayerOptions options = new IFramePlayerOptions.Builder().controls(0).build();
        youTubePlayerView.initialize(listener, options);
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_youtube_player;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
    }
}