package nemosofts.streambox.util;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import java.util.Objects;

import nemosofts.streambox.activity.SplashActivity;

public class BootReceiver extends BroadcastReceiver {
    @Override
    public void onReceive(Context context, Intent intent) {
        if (intent != null && Objects.equals(intent.getAction(), Intent.ACTION_BOOT_COMPLETED) && (ApplicationUtil.isTvBox(context))){
            Intent activityIntent = new Intent(context, SplashActivity.class);
            activityIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            context.startActivity(activityIntent);
        }
    }
}
