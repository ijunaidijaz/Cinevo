package mycinevo.streambox.util;

import android.app.Activity;
import android.os.Build;
import android.view.View;
import android.view.Window;
import android.view.WindowInsets;
import android.view.WindowInsetsController;
import android.view.WindowManager;

import androidx.annotation.OptIn;
import androidx.media3.common.util.UnstableApi;

import mycinevo.streambox.R;
import mycinevo.streambox.util.helper.SPHelper;
import mycinevo.streambox.util.player.CustomPlayerView;

public class IfSupported {

    private IfSupported() {
        throw new IllegalStateException("Utility class");
    }

    public static void IsRTL(Activity mContext) {
        if (Boolean.TRUE.equals(new SPHelper(mContext).getIsRTL())) {
            try {
                Window window = mContext.getWindow();
                window.getDecorView().setLayoutDirection(View.LAYOUT_DIRECTION_RTL);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public static void IsScreenshot(Activity mContext) {
        if (Boolean.TRUE.equals(new SPHelper(mContext).getIsScreenshot())) {
            try {
                Window window = mContext.getWindow();
                window.setFlags(WindowManager.LayoutParams.FLAG_SECURE, WindowManager.LayoutParams.FLAG_SECURE);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public static void keepScreenOn(Activity mContext) {
        try {
            Window window = mContext.getWindow();
            window.addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void hideStatusBar(Activity mContext) {
        try {
            View decorView = mContext.getWindow().getDecorView();
            int uiOptions = View.SYSTEM_UI_FLAG_FULLSCREEN;
            decorView.setSystemUiVisibility(uiOptions);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void hideBottomBar(Activity mContext) {
        try {
            View decorView = mContext.getWindow().getDecorView();
            int uiOptions = View.SYSTEM_UI_FLAG_HIDE_NAVIGATION | View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY | View.SYSTEM_UI_FLAG_FULLSCREEN;
            decorView.setSystemUiVisibility(uiOptions);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void hideStatusBarDialog(Window window) {
        try {
            View decorView = window.getDecorView();
            int uiOptions = View.SYSTEM_UI_FLAG_FULLSCREEN;
            decorView.setSystemUiVisibility(uiOptions);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @OptIn(markerClass = UnstableApi.class)
    public static void toggleSystemUi(final Activity activity, final CustomPlayerView playerView, final boolean show) {
        if (Build.VERSION.SDK_INT >= 31) {
            Window window = activity.getWindow();
            if (window != null) {
                WindowInsetsController windowInsetsController = window.getInsetsController();
                if (windowInsetsController != null) {
                    if (show) {
                        windowInsetsController.show(WindowInsets.Type.systemBars());
                    } else {
                        windowInsetsController.hide(WindowInsets.Type.systemBars());
                    }
                }
            }
        } else {
            if (show) {
                playerView.setSystemUiVisibility(View.SYSTEM_UI_FLAG_LAYOUT_STABLE
                        | View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION
                        | View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN);
            } else {
                playerView.setSystemUiVisibility(View.SYSTEM_UI_FLAG_LOW_PROFILE
                        | View.SYSTEM_UI_FLAG_FULLSCREEN
                        | View.SYSTEM_UI_FLAG_LAYOUT_STABLE
                        | View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY
                        | View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION
                        | View.SYSTEM_UI_FLAG_HIDE_NAVIGATION);
            }
        }
    }

    public static void statusBarBlackColor(Activity mContext) {
        try {
            Window window = mContext.getWindow();
            window.addFlags(WindowManager.LayoutParams.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS);
            window.clearFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS);
            window.setStatusBarColor(mContext.getResources().getColor(R.color.black));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
