package mycinevo.streambox.util;

import android.Manifest;
import android.content.Context;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.telephony.TelephonyManager;

import androidx.annotation.NonNull;
import androidx.annotation.RequiresPermission;

/**
 * The type Network utils.
 */
public class NetworkUtils {

    private NetworkUtils() {
        throw new UnsupportedOperationException("Should not create instance of Util class. Please use as static..");
    }

    /**
     * Get the network info
     *
     * @param context the context
     * @return network info
     */
    @RequiresPermission(Manifest.permission.ACCESS_NETWORK_STATE)
    private static NetworkInfo getNetworkInfo(@NonNull Context context) {
        ConnectivityManager cm = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
        assert cm != null;
        return cm.getActiveNetworkInfo();
    }

    /**
     * Check if there is any connectivity
     *
     * @param context the context
     * @return boolean boolean
     */
    @RequiresPermission(Manifest.permission.ACCESS_NETWORK_STATE)
    public static boolean isConnected(Context context) {
        NetworkInfo info = getNetworkInfo(context);
        return (info != null && info.isConnectedOrConnecting());
    }

    /**
     * Check if there is fast connectivity
     *
     * @param context the context
     * @return boolean boolean
     */
    @RequiresPermission(Manifest.permission.ACCESS_NETWORK_STATE)
    public static boolean isConnectedFast(Context context) {
        NetworkInfo info = getNetworkInfo(context);
        return (info != null && info.isConnectedOrConnecting() && isConnectionFast(info.getType(), info.getSubtype()));
    }

    /**
     * Check if there is any connectivity to a mobile network
     *
     * @param context the context
     * @return boolean boolean
     */
    @RequiresPermission(Manifest.permission.ACCESS_NETWORK_STATE)
    public static boolean isConnectedMobile(Context context) {
        NetworkInfo info = getNetworkInfo(context);
        return (info != null && info.isConnectedOrConnecting() && info.getType() == ConnectivityManager.TYPE_MOBILE);
    }

    /**
     * Check if there is any connectivity to a Wifi network
     *
     * @param context the context
     * @return boolean boolean
     */
    @RequiresPermission(Manifest.permission.ACCESS_NETWORK_STATE)
    public static boolean isConnectedWifi(Context context) {
        NetworkInfo info = getNetworkInfo(context);
        return (info != null && info.isConnectedOrConnecting() && info.getType() == ConnectivityManager.TYPE_WIFI);
    }

    /**
     * Check if there is any connectivity to a Ethernet network
     *
     * @param context the context
     * @return boolean boolean
     */
    @RequiresPermission(Manifest.permission.ACCESS_NETWORK_STATE)
    public static boolean isConnectedEthernet(Context context) {
        NetworkInfo info = getNetworkInfo(context);
        return (info != null && info.isConnectedOrConnecting() && info.getType() == ConnectivityManager.TYPE_ETHERNET);
    }

    /**
     * Check if the connection is fast
     *
     * @param type    the type
     * @param subType the sub type
     * @return boolean boolean
     */
    private static boolean isConnectionFast(int type, int subType) {
        if (type == ConnectivityManager.TYPE_WIFI) {
            return true;
        } else if (type == ConnectivityManager.TYPE_MOBILE) {
            return switch (subType) {
                case TelephonyManager.NETWORK_TYPE_1xRTT, TelephonyManager.NETWORK_TYPE_EDGE -> false; // ~ 50-100 kbps
                case TelephonyManager.NETWORK_TYPE_CDMA -> false; // ~ 14-64 kbps
                case TelephonyManager.NETWORK_TYPE_EVDO_0 -> true; // ~ 400-1000 kbps
                case TelephonyManager.NETWORK_TYPE_EVDO_A -> true; // ~ 600-1400 kbps
                case TelephonyManager.NETWORK_TYPE_GPRS -> false; // ~ 100 kbps
                case TelephonyManager.NETWORK_TYPE_HSDPA -> true; // ~ 2-14 Mbps
                case TelephonyManager.NETWORK_TYPE_HSPA -> true; // ~ 700-1700 kbps
                case TelephonyManager.NETWORK_TYPE_HSUPA -> true; // ~ 1-23 Mbps
                case TelephonyManager.NETWORK_TYPE_UMTS -> true; // ~ 400-7000 kbps
                /*
                 * Above API level 7, make sure to set android:targetSdkVersion
                 * to appropriate level to use these
                 */
                case TelephonyManager.NETWORK_TYPE_EHRPD -> // API level 11
                        true; // ~ 1-2 Mbps
                case TelephonyManager.NETWORK_TYPE_EVDO_B -> // API level 9
                        true; // ~ 5 Mbps
                case TelephonyManager.NETWORK_TYPE_HSPAP -> // API level 13
                        true; // ~ 10-20 Mbps
                case TelephonyManager.NETWORK_TYPE_IDEN -> // API level 8
                        false; // ~25 kbps
                case TelephonyManager.NETWORK_TYPE_LTE -> // API level 11
                        true; // ~ 10+ Mbps
                // Unknown
                default -> false;
            };
        } else {
            return false;
        }
    }

}
