package mycinevo.streambox.util.helper;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.view.View;
import android.view.Window;
import android.widget.Toast;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;

import java.io.File;
import java.util.Random;

import mycinevo.streambox.R;
import mycinevo.streambox.activity.DownloadService;
import mycinevo.streambox.activity.InterstitialActivity;
import mycinevo.streambox.asyncTask.LoadInterstitial;
import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.interfaces.InterAdListener;
import mycinevo.streambox.item.ItemVideoDownload;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.NetworkUtils;
import okhttp3.MultipartBody;
import okhttp3.RequestBody;

public class Helper {

    private final Context ctx;
    private InterAdListener interAdListener;

    public Helper(Context ctx) {
        this.ctx = ctx;
    }

    public Helper(Context ctx, InterAdListener interAdListener) {
        this.ctx = ctx;
        this.interAdListener = interAdListener;
    }

    public RequestBody getAPIRequestLogin(String username, String password) {
        return new MultipartBody.Builder()
            .setType(MultipartBody.FORM)
            .addFormDataPart("username", username)
            .addFormDataPart("password", password)
            .build();
    }

    public RequestBody getAPIRequest(String action, String username, String password) {
        return new MultipartBody.Builder()
            .setType(MultipartBody.FORM)
            .addFormDataPart("username", username)
            .addFormDataPart("password", password)
            .addFormDataPart("action", action)
            .build();
    }

    public RequestBody getAPIRequestID(String action,String type, String series_id, String username, String password) {
        return new MultipartBody.Builder()
            .setType(MultipartBody.FORM)
            .addFormDataPart("username", username)
            .addFormDataPart("password", password)
            .addFormDataPart("action", action)
            .addFormDataPart(type, series_id)
            .build();
    }

    public RequestBody getAPIRequestNSofts(String helper_name, String reportTitle, String reportMessages, String userName, String userPass) {
        Gson gson = new GsonBuilder().setDateFormat("yyyy-MM-dd' 'HH:mm:ss").create();
        JsonObject jsObj = (JsonObject) new Gson().toJsonTree(gson);
        jsObj.addProperty("helper_name", helper_name);
        jsObj.addProperty("application_id", ctx.getPackageName());
        if (Callback.METHOD_REPORT.equals(helper_name)) {
            jsObj.addProperty("user_name", userName);
            jsObj.addProperty("user_pass", userPass);
            jsObj.addProperty("report_title", reportTitle);
            jsObj.addProperty("report_msg", reportMessages);
        } else if (Callback.METHOD_GET_DEVICE_ID.equals(helper_name)) {
            jsObj.addProperty("device_id", userPass);
        }
        return new MultipartBody.Builder()
            .setType(MultipartBody.FORM)
            .addFormDataPart("data", ApplicationUtil.toBase64(jsObj.toString()))
            .build();
    }

    @SuppressLint("StaticFieldLeak")
    public void download(final ItemVideoDownload itemDownload, String table) {
        File root = new File(ctx.getExternalFilesDir("").getAbsolutePath() + File.separator + "/temp");
        if (!root.exists()) {
            root.mkdirs();
        }

        Random random = new Random();
        String a = String.valueOf(System.currentTimeMillis());
        String name = random.nextInt((999999 - 100000) + 100000) + a.substring(a.length() - 6, a.length() - 1);

        File file = new File(root, name + ApplicationUtil.containerExtension(itemDownload.getContainerExtension()));
        if (Boolean.FALSE.equals(new DBHelper(ctx).checkDownload(table, itemDownload.getStreamID(), ApplicationUtil.containerExtension(itemDownload.getContainerExtension())))) {
            if (Boolean.FALSE.equals(DownloadService.getInstance().isDownloading())) {
                Intent serviceIntent = new Intent(ctx, DownloadService.class);
                serviceIntent.setAction(DownloadService.ACTION_START);
                serviceIntent.putExtra("downloadUrl", itemDownload.getVideoURL());
                serviceIntent.putExtra("file_path", root.toString());
                serviceIntent.putExtra("file_name", file.getName());
                serviceIntent.putExtra("file_container", ApplicationUtil.containerExtension(itemDownload.getContainerExtension()));
                serviceIntent.putExtra("item", itemDownload);
                ctx.startService(serviceIntent);
            } else {
                Intent serviceIntent = new Intent(ctx, DownloadService.class);
                serviceIntent.setAction(DownloadService.ACTION_ADD);
                serviceIntent.putExtra("downloadUrl", itemDownload.getVideoURL());
                serviceIntent.putExtra("file_path", root.toString());
                serviceIntent.putExtra("file_name", file.getName());
                serviceIntent.putExtra("file_container", ApplicationUtil.containerExtension(itemDownload.getContainerExtension()));
                serviceIntent.putExtra("item", itemDownload);
                ctx.startService(serviceIntent);
            }
        } else {
            Toast.makeText(ctx, ctx.getResources().getString(R.string.already_download), Toast.LENGTH_SHORT).show();
        }
    }

    public void showInterAd(final int pos, final String type) {
        if (NetworkUtils.isConnected(ctx)){
            if (isCustomAd()){
                ctx.startActivity(new Intent(ctx, InterstitialActivity.class));
            } else {
                interAdListener.onClick(pos, type);
            }
        } else {
            interAdListener.onClick(pos, type);
        }
    }

    private boolean isCustomAd() {
        if (Boolean.TRUE.equals(Callback.isCustomAds)) {
            if (!Callback.interstitial_ads_image.isEmpty() && !Callback.interstitial_ds_redirect_url.isEmpty()){
                Callback.customAdCount = Callback.customAdCount + 1;
                return Callback.customAdCount % Callback.customAdShow == 0;
            } else {
                new LoadInterstitial(ctx).execute();
                return false;
            }
        } else {
            return false;
        }
    }
    public static void hideNavigationKeys(Window window) {
        // Hide the navigation bar
        View decorView = window.getDecorView();
        int uiOptions = View.SYSTEM_UI_FLAG_HIDE_NAVIGATION;
        decorView.setSystemUiVisibility(uiOptions);
    }
}
