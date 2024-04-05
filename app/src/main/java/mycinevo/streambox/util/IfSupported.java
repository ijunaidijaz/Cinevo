package mycinevo.streambox.util;

import android.app.Activity;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;

public class IfSupported {

    private IfSupported() {
        throw new IllegalStateException("Utility class");
    }

    public static void IsRTL(Activity mContext) {
        if (Boolean.TRUE.equals(new SharedPref(mContext).getIsRTL())) {
            try {
                Window window = mContext.getWindow();
                window.getDecorView().setLayoutDirection(View.LAYOUT_DIRECTION_RTL);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public static void IsScreenshot(Activity mContext) {
        if (Boolean.TRUE.equals(new SharedPref(mContext).getIsScreenshot())) {
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
}
