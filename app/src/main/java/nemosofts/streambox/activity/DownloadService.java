package nemosofts.streambox.activity;

import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ServiceInfo;
import android.os.Build;
import android.os.Handler;
import android.os.IBinder;
import android.os.Message;
import android.util.Log;
import android.widget.RemoteViews;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;
import androidx.nemosofts.view.coreprogress.ProgressHelper;
import androidx.nemosofts.view.coreprogress.ProgressUIListener;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemVideoDownload;
import nemosofts.streambox.util.Encrypter.Encrypter;
import okhttp3.Call;
import okhttp3.Callback;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import okio.BufferedSource;

public class DownloadService extends Service {

    NotificationCompat.Builder myNotify;
    RemoteViews rv;
    OkHttpClient client;
    public static final String ACTION_STOP = "com.mydownload.action.STOP";
    public static final String ACTION_START = "com.mydownload.action.START";
    public static final String ACTION_ADD = "com.mydownload.action.ADD";
    private static final String CANCEL_TAG = "c_tag";
    NotificationManager mNotificationManager;
    public static DownloadService downloadService;

    Encrypter enc;
    Boolean isDownloaded = false;
    Thread thread;
    Call call;
    public static int count = 0;
    public static List<String> arrayListName = new ArrayList<>();
    public static List<String> arrayListContainer = new ArrayList<>();
    public static List<String> arrayListFilePath = new ArrayList<>();
    public static List<String> arrayListURL = new ArrayList<>();
    public static List<ItemVideoDownload> arrayListVideo = new ArrayList<>();
    static final int MY_NOTIFICATION_ID = 1002;

    private final Handler mHandler = new Handler(new Handler.Callback() {
        @Override
        public boolean handleMessage(@NonNull Message message) {
            switch (message.what) {
                case 1 -> {
                    int progress = Integer.parseInt(message.obj.toString());
                    arrayListVideo.get(0).setProgress(progress);
                    rv.setProgressBar(R.id.progress, 100, progress, false);
                    mNotificationManager.notify(MY_NOTIFICATION_ID, myNotify.build());
                }
                case 0 -> {
                    rv.setTextViewText(R.id.nf_title, arrayListVideo.get(0).getName());
                    rv.setTextViewText(R.id.nf_percentage, count - (arrayListURL.size() - 1) + "/" + count + " " + getString(R.string.downloading));
                    mNotificationManager.notify(MY_NOTIFICATION_ID, myNotify.build());
                }
                case 2 -> {
                    try {
                        Thread.sleep(500);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                    rv.setProgressBar(R.id.progress, 100, 100, false);
                    rv.setTextViewText(R.id.nf_percentage, count + "/" + count + " " + getString(R.string.downloaded));
                    mNotificationManager.notify(MY_NOTIFICATION_ID, myNotify.build());
                    count = 0;
                }
                default -> {
                }
            }
            return false;
        }
    });

    public static DownloadService getInstance() {
        if (downloadService == null) {
            downloadService = new DownloadService();
        }
        return downloadService;
    }

    public static Boolean isDownloading() {
        return !arrayListFilePath.isEmpty();
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public void onCreate() {
        super.onCreate();

        enc = Encrypter.GetInstance();
        enc.Init(this);

        mNotificationManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
        String NOTIFICATION_CHANNEL_ID = "download_ch_1";
        myNotify = new NotificationCompat.Builder(this, NOTIFICATION_CHANNEL_ID);
        myNotify.setChannelId(NOTIFICATION_CHANNEL_ID);
        myNotify.setSmallIcon(R.drawable.ic_file_download_not);
        myNotify.setTicker(getResources().getString(R.string.downloading));
        myNotify.setWhen(System.currentTimeMillis());
        myNotify.setOnlyAlertOnce(true);

        rv = new RemoteViews(getPackageName(), R.layout.row_custom_notification);
        rv.setTextViewText(R.id.nf_title, getString(R.string.app_name));
        rv.setProgressBar(R.id.progress, 100, 0, false);
        rv.setTextViewText(R.id.nf_percentage, getResources().getString(R.string.downloading) + " " + "(0%)");

        Intent closeIntent = new Intent(this, DownloadService.class);
        closeIntent.setAction(ACTION_STOP);
        PendingIntent pcloseIntent = PendingIntent.getService(this, 0,
                closeIntent, PendingIntent.FLAG_IMMUTABLE);
        rv.setOnClickPendingIntent(R.id.iv_stop_download, pcloseIntent);

        myNotify.setCustomContentView(rv);

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            CharSequence name = "Online Channel download";// The user-visible name of the channel.
            NotificationChannel mChannel = new NotificationChannel(NOTIFICATION_CHANNEL_ID, name, NotificationManager.IMPORTANCE_LOW);
            mNotificationManager.createNotificationChannel(mChannel);
        }
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            startForeground(MY_NOTIFICATION_ID, myNotify.build(), ServiceInfo.FOREGROUND_SERVICE_TYPE_DATA_SYNC);
        } else {
            startForeground(MY_NOTIFICATION_ID, myNotify.build());
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        try {
            stopForeground(true);
            stop(null);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        super.onStartCommand(intent, flags, startId);
        if (intent != null && intent.getAction() != null) {
            if (intent.getAction().equals(ACTION_START)) {
                arrayListURL.add(intent.getStringExtra("downloadUrl"));
                arrayListFilePath.add(intent.getStringExtra("file_path"));
                arrayListName.add(intent.getStringExtra("file_name"));
                arrayListContainer.add(intent.getStringExtra("file_container"));
                arrayListVideo.add((ItemVideoDownload) intent.getSerializableExtra("item"));
                count = count + 1;
                init();
            } else if (intent.getAction().equals(ACTION_STOP)) {
                stop(intent);
            } else if (intent.getAction().equals(ACTION_ADD)) {
                ItemVideoDownload itemVideoDownload = (ItemVideoDownload) intent.getSerializableExtra("item");
                boolean flag = false;
                if (itemVideoDownload != null){
                    for (int i = 0; i < arrayListVideo.size(); i++) {
                        if (itemVideoDownload.getStreamID().equals(arrayListVideo.get(i).getStreamID())) {
                            flag = true;
                            break;
                        }
                    }
                }

                if (!flag) {
                    count = count + 1;
                    arrayListURL.add(intent.getStringExtra("downloadUrl"));
                    arrayListFilePath.add(intent.getStringExtra("file_path"));
                    arrayListName.add(intent.getStringExtra("file_name"));
                    arrayListContainer.add(intent.getStringExtra("file_container"));
                    arrayListVideo.add(itemVideoDownload);

                    Message msg = mHandler.obtainMessage();
                    msg.what = 0;
                    mHandler.sendMessage(msg);
                }
            }
        }
        return START_STICKY;
    }

    private void stop(Intent intent) {
        try {
            count = 0;
            if (client != null) {
                for (Call call : client.dispatcher().runningCalls()) {
                    if (call.request().tag().equals(CANCEL_TAG))
                        call.cancel();
                }
            }
            if (thread != null) {
                thread.interrupt();
                thread = null;
            }
            try {
                new File(arrayListFilePath.get(0) + "/" + arrayListName.get(0).replace(arrayListContainer.get(0), "")).delete();
            } catch (Exception e) {
                e.printStackTrace();
            }
            arrayListVideo.clear();
            arrayListName.clear();
            arrayListContainer.clear();
            arrayListURL.clear();
            arrayListFilePath.clear();
            stopForeground(true);
            if (intent != null) {
                stopService(intent);
            } else {
                stopSelf();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void init() {
        thread = new Thread(new Runnable() {
            @Override
            public void run() {
                isDownloaded = false;

                client = new OkHttpClient();
                Request.Builder builder = new Request.Builder()
                        .url(arrayListURL.get(0))
                        .addHeader("Accept-Encoding", "identity")
                        .get()
                        .tag(CANCEL_TAG);

                call = client.newCall(builder.build());
                call.enqueue(new Callback() {
                    @Override
                    public void onFailure(@NonNull Call call, @NonNull IOException e) {
                        e.printStackTrace();
                    }

                    @Override
                    public void onResponse(@NonNull Call call, @NonNull Response response) {
                        ResponseBody responseBody = ProgressHelper.withProgress(response.body(), new ProgressUIListener() {

                            //if you don't need this method, don't override this methd. It isn't an abstract method, just an empty method.
                            @Override
                            public void onUIProgressStart(long totalBytes) {
                                super.onUIProgressStart(totalBytes);
                                Message msg = mHandler.obtainMessage();
                                msg.what = 0;
                                mHandler.sendMessage(msg);
                            }

                            @Override
                            public void onUIProgressChanged(long numBytes, long totalBytes, float percent, float speed) {
                                if (Boolean.FALSE.equals(isDownloaded)) {
                                    Message msg = mHandler.obtainMessage();
                                    msg.what = 1;
                                    msg.obj = (int) (100 * percent) + "";
                                    mHandler.sendMessage(msg);
                                }
                            }

                            @Override
                            public void onUIProgressFinish() {
                                super.onUIProgressFinish();
                            }
                        });

                        try {
                            BufferedSource source = responseBody.source();
                            enc.encrypt(arrayListFilePath.get(0) + "/" + arrayListName.get(0), source, arrayListVideo.get(0));
                        } catch (Exception e) {
                            Log.d("show_data", e.toString());
                        }

                        if (!arrayListURL.isEmpty()) {
                            arrayListVideo.remove(0);
                            arrayListName.remove(0);
                            arrayListContainer.remove(0);
                            arrayListFilePath.remove(0);
                            arrayListURL.remove(0);
                            if (!call.isCanceled() && !arrayListURL.isEmpty()) {
                                init();
                            } else {
                                Message msg = mHandler.obtainMessage();
                                msg.what = 2;
                                msg.obj = 0 + "";
                                mHandler.sendMessage(msg);
                                isDownloaded = true;
                            }
                        }
                    }
                });
            }
        });
        thread.start();
    }
}