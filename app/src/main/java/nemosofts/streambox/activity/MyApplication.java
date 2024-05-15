package mycinevo.streambox.activity;

import android.content.Context;
import android.os.StrictMode;

import androidx.multidex.MultiDex;
import androidx.nemosofts.Application;

import com.google.firebase.analytics.FirebaseAnalytics;
import com.onesignal.OneSignal;

import mycinevo.streambox.BuildConfig;
import mycinevo.streambox.R;
import mycinevo.streambox.util.helper.DBHelper;

public class MyApplication extends Application {

    @Override
    public void onCreate() {
        super.onCreate();

        // Analytics Initialization
        FirebaseAnalytics.getInstance(getApplicationContext());

        StrictMode.VmPolicy.Builder builder = new StrictMode.VmPolicy.Builder();
        StrictMode.setVmPolicy(builder.build());

        try {
            DBHelper dbHelper = new DBHelper(getApplicationContext());
            dbHelper.onCreate(dbHelper.getWritableDatabase());
        } catch (Exception e) {
            e.printStackTrace();
        }

        // OneSignal Initialization
        OneSignal.initWithContext(this, getString(R.string.onesignal_app_id));
    }

    @Override
    public String setProductID() {
        return "47641297";
    }

    @Override
    public String setApplicationID() {
        return BuildConfig.APPLICATION_ID;
    }

    @Override
    protected void attachBaseContext(Context base) {
        super.attachBaseContext(base);
        MultiDex.install(this);
    }
}
