package nemosofts.streambox.util;

import static android.content.Context.UI_MODE_SERVICE;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.UiModeManager;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.database.Cursor;
import android.graphics.Color;
import android.graphics.Point;
import android.net.Uri;
import android.os.BatteryManager;
import android.os.Build;
import android.provider.DocumentsContract;
import android.provider.MediaStore;
import android.provider.Settings;
import android.text.SpannableStringBuilder;
import android.text.style.ForegroundColorSpan;
import android.util.Base64;
import android.util.TypedValue;
import android.view.Display;
import android.view.WindowManager;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.media3.common.MimeTypes;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.exoplayer.DefaultRenderersFactory;
import androidx.media3.exoplayer.SimpleExoPlayer;
import androidx.media3.extractor.DefaultExtractorsFactory;
import androidx.media3.extractor.ts.DefaultTsPayloadReaderFactory;
import androidx.media3.extractor.ts.TsExtractor;
import androidx.nemosofts.theme.ThemeEngine;

import org.jetbrains.annotations.Contract;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.concurrent.TimeUnit;

import nemosofts.streambox.R;
import nemosofts.streambox.activity.SelectPlayerActivity;
import nemosofts.streambox.activity.UI.BlackPantherActivity;
import nemosofts.streambox.activity.UI.GlossyActivity;
import nemosofts.streambox.activity.UI.MovieUIActivity;
import nemosofts.streambox.activity.UI.OneUIActivity;
import nemosofts.streambox.activity.UI.PlaylistActivity;
import nemosofts.streambox.activity.UI.SingleStreamActivity;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.util.helper.SPHelper;
import nemosofts.streambox.util.player.CustomPlayerView;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.logging.HttpLoggingInterceptor;

public class ApplicationUtil {

    public static final String FEATURE_FIRE_TV = "amazon.hardware.fire_tv";
    public static final String[] SUPPORTED_EXTENSIONS_PLAYLIST = new String[] {
            "audio/mpegurl",
            "audio/x-mpegurl",
            "application/x-mpegurl"
    };

    private ApplicationUtil() {
        throw new IllegalStateException("Utility class");
    }

    @NonNull
    public static String responsePost(String url, RequestBody requestBody) {

        HttpLoggingInterceptor logging = new HttpLoggingInterceptor();
        logging.setLevel(HttpLoggingInterceptor.Level.BODY);

        OkHttpClient client = new OkHttpClient.Builder()
                .connectTimeout(5, TimeUnit.SECONDS)
                .writeTimeout(10, TimeUnit.SECONDS)
                .readTimeout(30, TimeUnit.SECONDS)
                .cache(null)
                .build();

        Request request = new Request.Builder()
                .url(url)
                .post(requestBody)
                .build();
        try {
            Response response = client.newCall(request).execute();
            return response.body() != null ? response.body().string() : "";
        } catch (Exception e) {
            e.printStackTrace();
            return "";
        }
    }

    @NonNull
    public static String okhttpGet(String movie_id, String Token) {
        OkHttpClient client = new OkHttpClient();
        Request request = new Request.Builder()
                .url("https://api.themoviedb.org/3/movie/"+ movie_id +"/credits?language=en-US")
                .get()
                .addHeader("accept", "application/json")
                .addHeader("Authorization", "Bearer "+Token)
                .build();
        try {
            Response response = client.newCall(request).execute();
            return response.body() != null ? response.body().string() : "";
        } catch (Exception e) {
            e.printStackTrace();
            return "";
        }
    }

    @NonNull
    public static String toBase64(@NonNull String input) {
        byte[] encodeValue = Base64.encode(input.getBytes(), Base64.DEFAULT);
        return new String(encodeValue);
    }

    @NonNull
    public static String decodeBase64(String encoded) {
        byte[] decodedBytes = Base64.decode(encoded, Base64.DEFAULT);
        return new String(decodedBytes);
    }

    @NonNull
    public static String encodeBase64(@NonNull String encoded) {
        byte[] decodedBytes = Base64.encode(encoded.getBytes(), Base64.DEFAULT);
        return new String(decodedBytes);
    }

    public static int getColumnWidth(@NonNull Context ctx, int column, int gridPadding) {
        Resources r = ctx.getResources();
        float padding = TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, gridPadding, r.getDisplayMetrics());
        return (int) ((getScreenWidth(ctx) - ((column + 1) * padding)) / column);
    }

    private static int getScreenWidth(@NonNull Context ctx) {
        int columnWidth;
        WindowManager wm = (WindowManager) ctx.getSystemService(Context.WINDOW_SERVICE);
        Display display = wm.getDefaultDisplay();
        final Point point = new Point();
        point.x = display.getWidth();
        point.y = display.getHeight();
        columnWidth = point.x;
        return columnWidth;
    }

    @NonNull
    public static String convertIntToDate(String convertDate, String pattern) {
        try {
            if (!convertDate.isEmpty()){
                if (convertDate.equals("null")){
                    return " Unlimited";
                } else {
                    long timestamp = Long.parseLong(convertDate);
                    Date date = new Date(timestamp * 1000);
                    SimpleDateFormat dateFormat = new SimpleDateFormat(pattern, Locale.getDefault());
                    return " "+dateFormat.format(date);
                }
            } else {
                return " none";
            }
        } catch (Exception e) {
            return " none";
        }
    }

    @NonNull
    public static String readableFileSize(long size) {
        if (size <= 0) return "0 Bytes";
        final String[] units = new String[]{"Bytes", "kB", "MB", "GB", "TB"};
        int digitGroups = (int) (Math.log10(size) / Math.log10(1024));
        return new DecimalFormat("#,##0.#").format(size / Math.pow(1024, digitGroups)) + " " + units[digitGroups];
    }

    @NonNull
    public static String TimeFormat(String time) {
        try {
            if (!time.isEmpty()){
                int totalMinutes = Integer.parseInt(time);
                int hours = totalMinutes / 60;
                int minutes = totalMinutes % 60;
                return formatTime(hours, minutes);
            } else {
                return "0";
            }
        } catch (Exception e) {
            e.printStackTrace();
            return "0";
        }
    }

    @NonNull
    private static String formatTime(int hours, int minutes) {
        if (hours != 0){
            return hours + "h " + minutes + "m";
        } else {
            if (minutes != 0){
                return minutes + "m";
            } else {
                return "0";
            }
        }
    }

    @NonNull
    public static String formatTimeToTime(String timeString) {
        try {
            String[] timeParts = timeString.split(":");
            int hours = Integer.parseInt(timeParts[0]);
            int minutes = Integer.parseInt(timeParts[1]);
            int seconds = Integer.parseInt(timeParts[2]);
            if (hours != 0){
                return hours + "h " + minutes + "m " + seconds + "s";
            } else {
                if (minutes != 0){
                    return minutes + "m "  + seconds + "s";
                } else {
                    return "0";
                }
            }
        } catch (Exception e) {
            return timeString;
        }
    }

    public static Boolean calculateUpdateHours(@NonNull String inputDateStr, int updateHours){
        boolean is_update = false;
        try {
            if (!inputDateStr.isEmpty()){
                @SuppressLint("SimpleDateFormat") SimpleDateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
                Date inputDate = dateFormat.parse(inputDateStr);
                Date currentDate = new Date();
                assert inputDate != null;
                long timeDifferenceInMillis = currentDate.getTime() - inputDate.getTime();
                long seconds = timeDifferenceInMillis / 1000;
                int hours = (int) (seconds / 3600);
                is_update = hours > updateHours;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return is_update;
    }

    public static String calculateTimeSpan(@NonNull String inputDateStr){
        String time = "not available";
        if (!inputDateStr.isEmpty()){
            try {
                @SuppressLint("SimpleDateFormat") SimpleDateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
                Date inputDate = dateFormat.parse(inputDateStr);
                Date currentDate = new Date();
                assert inputDate != null;
                long timeDifferenceInMillis = currentDate.getTime() - inputDate.getTime();
                long seconds = timeDifferenceInMillis / 1000;

                int year = (int) (seconds / 31556926);
                int months = (int) (seconds / 2629743);
                int week = (int) (seconds / 604800);
                int day = (int) (seconds / 86400);
                int hours = (int) (seconds / 3600);
                int min = (int) ((seconds - (hours * 3600)) / 60);
                int secs = (int) (seconds % 60);

                if (seconds < 60){
                    time = secs + " sec ago";
                } else if (seconds < 3600){
                    time = (min == 1) ? min+" min ago" : min + " mins ago";
                } else if (seconds < 86400){
                    time = (hours == 1) ? hours+" hour ago" : hours + " hours ago";
                } else if (seconds < 604800){
                    time = (day == 1) ? day+" day ago" : day + " days ago";
                } else if (seconds < 2629743){
                    time = (week == 1) ? week+" week ago" : week + " weeks ago";
                } else if (seconds < 31556926){
                    time = (months == 1) ? months+" month ago" : months + " months ago";
                }  else {
                    time = (year == 1) ? year+" year ago" : year + " years ago";
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        } else {
            time = "";
        }
        return time;
    }

    @NonNull
    @Contract(pure = true)
    public static String averageRating(String rating) {
        if (rating != null){
            return switch (rating) {
                case "1", "1.1", "1.2", "1.3", "1.4", "1.5", "1.6", "1.7", "1.8", "1.9" -> "1";
                case "2", "2.1", "2.2", "2.3", "2.4", "2.5", "2.6", "2.7", "2.8", "2.9" -> "2";
                case "3", "3.1", "3.2", "3.3", "3.4", "3.5", "3.6", "3.7", "3.8", "3.9" -> "3";
                case "4", "4.1", "4.2", "4.3", "4.4", "4.5", "4.6", "4.7", "4.8", "4.9" -> "4";
                case "5", "5.1", "5.2", "5.3", "5.4", "5.5", "5.6", "5.7", "5.8", "5.9" -> "5";
                case "6", "6.1", "6.2", "6.3", "6.4", "6.5", "6.6", "6.7", "6.8", "6.9" -> "6";
                case "7", "7.1", "7.2", "7.3", "7.4", "7.5", "7.6", "7.7", "7.8", "7.9" -> "7";
                case "8", "8.1", "8.2", "8.3", "8.4", "8.5", "8.6", "8.7", "8.8", "8.9" -> "8";
                case "9", "9.1", "9.2", "9.3", "9.4", "9.5", "9.6", "9.7", "9.8", "9.9" -> "9";
                case "10", "10.1", "10.2", "10.3", "10.4", "10.5", "10.6", "10.7", "10.8", "10.9" -> "10";
                default -> "0";
            };
        } else {
            return "0";
        }
    }

    // TvBox
    public static boolean isTvBox(@NonNull Context context) {
        final PackageManager pm = context.getPackageManager();

        // Check if the device is running on Android TV platform
        boolean hasFeature = pm.hasSystemFeature(PackageManager.FEATURE_TELEVISION);
        if (hasFeature) {
            // Additional checks for Android TV platform
            UiModeManager uiModeManager = (UiModeManager) context.getSystemService(UI_MODE_SERVICE);
            if (uiModeManager != null && uiModeManager.getCurrentModeType() == Configuration.UI_MODE_TYPE_TELEVISION) {
                return true;
            }
        }

        // Check if the device has the FEATURE_FIRE_TV system feature
        if (pm.hasSystemFeature(FEATURE_FIRE_TV)) {
            return true;
        }

        // Check for conditions indicating a TV box
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.R) { // Legacy storage no longer works on Android 11 (level 30)
            // (Some boxes still report touchscreen feature)
            if (!pm.hasSystemFeature(PackageManager.FEATURE_TOUCHSCREEN)) {
                return true;
            }
            if (pm.hasSystemFeature("android.hardware.hdmi.cec")) {
                return true;
            }
            if (Build.MANUFACTURER.equalsIgnoreCase("zidoo")) {
                return true;
            }
        }

        // Default: No TV box detected
        return false;
    }

    public static boolean isLandscape(@NonNull Context context) {
        int orientation = context.getResources().getConfiguration().orientation;
        return orientation == Configuration.ORIENTATION_LANDSCAPE;
    }

    public static boolean isTablet(@NonNull Context context) {
        return context.getResources().getConfiguration().smallestScreenWidthDp >= 720;
    }

    // Player
    @SuppressLint("UnsafeOptInUsageError")
    public static void showText(@NonNull final CustomPlayerView playerView, final String text, final long timeout) {
        playerView.removeCallbacks(playerView.textClearRunnable);
        playerView.clearIcon();
        playerView.setCustomErrorMessage(text);
        playerView.postDelayed(playerView.textClearRunnable, timeout);
    }

    public static void showText(@SuppressLint("UnsafeOptInUsageError") final CustomPlayerView playerView, final String text) {
        showText(playerView, text, 1200);
    }

    public static void openThemeActivity(Activity activity) {
        int theme = new SPHelper(activity).getIsTheme();
        Intent intent;
        if (theme == 2){
            intent = new Intent(activity, GlossyActivity.class);
        } else  if (theme == 3){
            intent = new Intent(activity, BlackPantherActivity.class);
        } else  if (theme == 4){
            intent = new Intent(activity, MovieUIActivity.class);
        } else {
            intent = new Intent(activity, OneUIActivity.class);
        }
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        activity.startActivity(intent);
        activity.finish();
    }

    @SuppressLint("UnsafeOptInUsageError")
    public static void openHomeActivity(Activity activity) {
        SPHelper spHelper = new SPHelper(activity);
        Intent intent;
        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_SINGLE_STREAM)){
            intent = new Intent(activity, SingleStreamActivity.class);
        } else if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
            intent = new Intent(activity, PlaylistActivity.class);
        } else if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_ONE_UI) || spHelper.getLoginType().equals(Callback.TAG_LOGIN_STREAM)){
            int theme = spHelper.getIsTheme();
            if (theme == 2){
                intent = new Intent(activity, GlossyActivity.class);
            }  else  if (theme == 3){
                intent = new Intent(activity, BlackPantherActivity.class);
            } else  if (theme == 4){
                intent = new Intent(activity, MovieUIActivity.class);
            } else {
                intent = new Intent(activity, OneUIActivity.class);
            }
        } else {
            intent = new Intent(activity, SelectPlayerActivity.class);
        }
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        intent.putExtra("from", "");
        activity.startActivity(intent);
        activity.finish();
    }

    public static int openThemeBg(Activity activity) {
        int theme = new SPHelper(activity).getIsTheme();
        int themePage = new ThemeEngine(activity).getThemePage();
        if (theme == 2){
            return R.drawable.bg_ui_glossy;
        } else if (theme == 3){
            return R.drawable.bg_dark_panther;
        } else {
            if (themePage == 0){
                return R.drawable.bg_dark;
            } else if (themePage == 1 || themePage == 2 || themePage == 3){
                return R.drawable.bg_classic;
            } else {
                return R.drawable.bg_dark;
            }
        }
    }

    @NonNull
    @Contract(pure = true)
    public static String containerExtension(String container) {
        if (container != null){
            if (container.contains(".")){
                return container;
            } else {
                return "."+container;
            }
        } else {
            return ".mp4";
        }
    }

    @NonNull
    @Contract(pure = true)
    public static Boolean geIsAdultsCount(@NonNull String count) {
        return count.contains("18+") || count.contains("[18+]");
    }

    @SuppressLint("HardwareIds")
    public static String getDeviceID(Context context) {
        try {
            return Settings.Secure.getString(context.getContentResolver(), Settings.Secure.ANDROID_ID);
        } catch (Exception e) {
            return "N/A";
        }
    }

    public static int getBatteryDrawable(int status, int level, int scale) {
        float batteryLevel = (level / (float) scale) * 100;
        boolean isCharging;
        if (status == BatteryManager.BATTERY_STATUS_CHARGING) {
            isCharging = true;
        } else if (status == BatteryManager.BATTERY_STATUS_FULL) {
            isCharging = false;
        } else {
            isCharging = false;
        }
        if (isCharging){
            return R.drawable.ic_battery_charging;
        } else if (batteryLevel < 10){
            return R.drawable.ic_battery_disable;
        } else if (batteryLevel < 20){
            return R.drawable.ic_battery_empty;
        } else if (batteryLevel < 30){
            return R.drawable.ic_battery_one;
        } else if (batteryLevel < 50){
            return R.drawable.ic_battery_two;
        } else {
            return R.drawable.ic_battery_full;
        }
    }

    @NonNull
    public static String getTimestamp(String data) {
        try {
            long timestamp = Long.parseLong(data);
            // Create a Date object using the timestamp
            Date date = new Date(timestamp * 1000);
            // Create a SimpleDateFormat object to define the desired date and time format
            SimpleDateFormat sdf = new SimpleDateFormat("H:mm", Locale.getDefault());
            // Format the date using the SimpleDateFormat
            return sdf.format(date);
        } catch (Exception e) {
            return "";
        }
    }

    @NonNull
    public static String formatFrameRate(float frameRate) {
        DecimalFormat decimalFormat = new DecimalFormat("#.#");
        return decimalFormat.format(frameRate);
    }

    public static String format(Number number) {
        if (number != null){
            char[] suffix = {' ', 'k', 'M', 'B', 'T', 'P', 'E'};
            long numValue = number.longValue();
            int value = (int) Math.floor(Math.log10(numValue));
            int base = value / 3;
            if (value >= 3 && base < suffix.length) {
                return new DecimalFormat("#0.0").format(numValue / Math.pow(10, base * 3)) + suffix[base];
            } else {
                return new DecimalFormat("#,##0").format(numValue);
            }
        } else {
            return String.valueOf(0);
        }
    }

    @NonNull
    @Contract(pure = true)
    public static String getVideoResolution(int height) {
        try {
            if (height >= 4320) {
                return "8k";
            } else if (height >= 2160) {
                return "4k";
            } else if (height >= 1440) {
                return "2k";
            } else if (height >= 1080) {
                return "1080p";
            } else if (height >= 720) {
                return "720p";
            } else if (height >= 480) {
                return "480p";
            } else if (height >= 360) {
                return "360p";
            } else if (height >= 240) {
                return "240p";
            } else if (height >= 140) {
                return "140p";
            } else {
                return "Unknown resolution";
            }
        } catch (Exception e) {
            return "Unknown resolution";
        }
    }

    @SuppressLint("Range")
    public static String getPathAudio(Context context, Uri uri) {
        try {
            String filePath = "";
            String wholeID = DocumentsContract.getDocumentId(uri);

            // Split at colon, use second item in the array
            String id = wholeID.split(":")[1];

            String[] column = {MediaStore.Audio.Media.DATA};

            // where id is equal to
            String sel = MediaStore.Audio.Media._ID + "=?";

            Cursor cursor = context.getContentResolver().query(MediaStore.Audio.Media.EXTERNAL_CONTENT_URI,
                    column, sel, new String[]{id}, null);

            int columnIndex = cursor.getColumnIndex(column[0]);

            if (cursor.moveToFirst()) {
                filePath = cursor.getString(columnIndex);
            }

            cursor.close();
            return filePath;
        } catch (Exception e) {
            e.printStackTrace();
            if (uri == null) {
                return null;
            }
            String[] projection = {MediaStore.MediaColumns.DATA};
            Cursor cursor = context.getContentResolver().query(uri, projection, null, null, null);
            if (cursor != null) {
                int columnIndex = cursor.getColumnIndexOrThrow(MediaStore.MediaColumns.DATA);
                cursor.moveToFirst();
                String returnn = cursor.getString(columnIndex);
                cursor.close();

                if (returnn == null) {
                    String path = null, image_id = null;
                    Cursor cursor2 = context.getContentResolver().query(uri, null, null, null, null);
                    if (cursor2 != null) {
                        cursor2.moveToFirst();
                        image_id = cursor2.getString(0);
                        image_id = image_id.substring(image_id.lastIndexOf(":") + 1);
                        cursor2.close();
                    }

                    Cursor cursor3 = context.getContentResolver().query(MediaStore.Audio.Media.EXTERNAL_CONTENT_URI, null, MediaStore.Audio.Media._ID + " = ? ", new String[]{image_id}, null);
                    if (cursor3 != null) {
                        cursor3.moveToFirst();
                        path = cursor3.getString(cursor3.getColumnIndex(MediaStore.MediaColumns.DATA));
                        cursor3.close();
                    }
                    return path;
                }
                return returnn;
            }
            // this is our fallback here
            return uri.getPath();
        }
    }


    public static void setRating(@NonNull String rating, ImageView iv_star_1, ImageView iv_star_2, ImageView iv_star_3, ImageView iv_star_4, ImageView iv_star_5) {
        try {
            String average = averageRating(rating);
            if (iv_star_1 != null && iv_star_2 != null && iv_star_3 != null && iv_star_4 != null && iv_star_5 != null) {
                switch (average) {
                    case "1" -> {
                        iv_star_1.setImageResource(R.drawable.ic_star);
                        iv_star_2.setImageResource(R.drawable.ic_star_border);
                        iv_star_3.setImageResource(R.drawable.ic_star_border);
                        iv_star_4.setImageResource(R.drawable.ic_star_border);
                        iv_star_5.setImageResource(R.drawable.ic_star_border);
                    }
                    case "2" -> {
                        iv_star_1.setImageResource(R.drawable.ic_star);
                        iv_star_2.setImageResource(R.drawable.ic_star);
                        iv_star_3.setImageResource(R.drawable.ic_star_border);
                        iv_star_4.setImageResource(R.drawable.ic_star_border);
                        iv_star_5.setImageResource(R.drawable.ic_star_border);
                    }
                    case "3" -> {
                        iv_star_1.setImageResource(R.drawable.ic_star);
                        iv_star_2.setImageResource(R.drawable.ic_star);
                        iv_star_3.setImageResource(R.drawable.ic_star);
                        iv_star_4.setImageResource(R.drawable.ic_star_border);
                        iv_star_5.setImageResource(R.drawable.ic_star_border);
                    }
                    case "4" -> {
                        iv_star_1.setImageResource(R.drawable.ic_star);
                        iv_star_2.setImageResource(R.drawable.ic_star);
                        iv_star_3.setImageResource(R.drawable.ic_star);
                        iv_star_4.setImageResource(R.drawable.ic_star);
                        iv_star_5.setImageResource(R.drawable.ic_star_border);
                    }
                    case "5" -> {
                        iv_star_1.setImageResource(R.drawable.ic_star);
                        iv_star_2.setImageResource(R.drawable.ic_star);
                        iv_star_3.setImageResource(R.drawable.ic_star);
                        iv_star_4.setImageResource(R.drawable.ic_star);
                        iv_star_5.setImageResource(R.drawable.ic_star);
                    }
                    default -> {
                        iv_star_1.setImageResource(R.drawable.ic_star_border);
                        iv_star_2.setImageResource(R.drawable.ic_star_border);
                        iv_star_3.setImageResource(R.drawable.ic_star_border);
                        iv_star_4.setImageResource(R.drawable.ic_star_border);
                        iv_star_5.setImageResource(R.drawable.ic_star_border);
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @NonNull
    public static CharSequence setErrorMsg(String errorMsg) {
        try {
            SpannableStringBuilder builder = new SpannableStringBuilder(errorMsg);
            builder.setSpan(new ForegroundColorSpan(Color.WHITE), 0, errorMsg.length(), 0);
            return builder;
        } catch (Exception e) {
            return errorMsg;
        }
    }

    @NonNull
    @OptIn(markerClass = androidx.media3.common.util.UnstableApi.class)
    public static DefaultExtractorsFactory getDefaultExtractorsFactory() {
        return new DefaultExtractorsFactory()
                .setTsExtractorFlags(DefaultTsPayloadReaderFactory.FLAG_ENABLE_HDMV_DTS_AUDIO_STREAMS
                        | DefaultTsPayloadReaderFactory.FLAG_IGNORE_AAC_STREAM
                        | DefaultTsPayloadReaderFactory.FLAG_IGNORE_H264_STREAM)
                .setTsExtractorTimestampSearchBytes(1500 * TsExtractor.TS_PACKET_SIZE);
    }

    @NonNull
    @OptIn(markerClass = androidx.media3.common.util.UnstableApi.class)
    public static DefaultRenderersFactory getDefaultRenderersFactory(Context context) {
        @SuppressLint("WrongConstant")
        DefaultRenderersFactory renderersFactory = new DefaultRenderersFactory(context);
        renderersFactory.setExtensionRendererMode(DefaultRenderersFactory.EXTENSION_RENDERER_MODE_ON);
        return renderersFactory;
    }

    @OptIn(markerClass = UnstableApi.class)
    @NonNull
    public static String getInfoAudio(@NonNull SimpleExoPlayer exoPlayer) {
        String info_audio;
        if (exoPlayer.getAudioFormat() != null){

            int audioSampleRate = exoPlayer.getAudioFormat().sampleRate;
            int audioChannels = exoPlayer.getAudioFormat().channelCount;
            String audioMimeType = exoPlayer.getAudioFormat().sampleMimeType;

            info_audio = "Audio Sample Rate: " + audioSampleRate + "\n\n"
                    + "Audio Channels: " + audioChannels + "\n\n"
                    + "Audio Type: "+ formatAudioFromMime(audioMimeType) +"\n\n"
                    + "Audio MIME Type: " + audioMimeType +"\n";

        } else {
            info_audio = "Audio Sample Rate: N/A" + "\n\n"
                    + "Audio Channels: N/A" + "\n\n"
                    + "Audio Type: N/A"+"\n\n"
                    + "Audio MIME Type: N/A"+"\n";

        }
        return info_audio;
    }

    @OptIn(markerClass = UnstableApi.class)
    @NonNull
    public static String getInfoVideo(@NonNull SimpleExoPlayer exoPlayer, boolean isLive) {
        String info_video;
        if (exoPlayer.getVideoFormat() != null){

            int videoWidth = exoPlayer.getVideoFormat().width;
            int videoHeight = exoPlayer.getVideoFormat().height;
            int videoBitrate = exoPlayer.getVideoFormat().bitrate;
            float frameRate = exoPlayer.getVideoFormat().frameRate;
            String finalRate = formatFrameRate(frameRate);

            if (isLive){
                info_video = "Video Quality: " + ApplicationUtil.getVideoResolution(videoHeight)+ "\n\n"
                        + "Video Width: " + videoWidth + "\n\n"
                        + "Video Height: " + videoHeight + "\n";
            } else {
                info_video = "Video Quality: " + ApplicationUtil.getVideoResolution(videoHeight)+ "\n\n"
                        + "Video Width: " + videoWidth + "\n\n"
                        + "Video Height: " + videoHeight + "\n\n"
                        + "Video Bitrate: " + videoBitrate + "\n\n"
                        + "Video Frame Rate: " + finalRate + "\n";
            }
        } else {
            info_video = "Video Quality : Unknown resolution" + "\n\n"
                    + "Video Width: N/A" + "\n\n"
                    + "Video Height: N/A" + "\n";
        }
        return info_video;
    }

    public static String formatAudioFromMime(final String mimeType) {
       if (mimeType == null){
           return "N/A";
       }
       return switch (mimeType) {
            case MimeTypes.AUDIO_DTS -> "DTS";
            case MimeTypes.AUDIO_DTS_HD -> "DTS-HD";
            case MimeTypes.AUDIO_DTS_EXPRESS -> "DTS Express";
            case MimeTypes.AUDIO_TRUEHD -> "TrueHD";
            case MimeTypes.AUDIO_AC3 -> "AC-3";
            case MimeTypes.AUDIO_E_AC3 -> "E-AC-3";
            case MimeTypes.AUDIO_E_AC3_JOC -> "E-AC-3-JOC";
            case MimeTypes.AUDIO_AC4 -> "AC-4";
            case MimeTypes.AUDIO_AAC -> "AAC";
            case MimeTypes.AUDIO_MPEG -> "MP3";
            case MimeTypes.AUDIO_MPEG_L2 -> "MP2";
            case MimeTypes.AUDIO_VORBIS -> "Vorbis";
            case MimeTypes.AUDIO_OPUS -> "Opus";
            case MimeTypes.AUDIO_FLAC -> "FLAC";
            case MimeTypes.AUDIO_ALAC -> "ALAC";
            case MimeTypes.AUDIO_WAV -> "WAV";
            case MimeTypes.AUDIO_AMR -> "AMR";
            case MimeTypes.AUDIO_AMR_NB -> "AMR-NB";
            case MimeTypes.AUDIO_AMR_WB -> "AMR-WB";
            case MimeTypes.APPLICATION_PGS -> "PGS";
            case MimeTypes.APPLICATION_SUBRIP -> "SRT";
            case MimeTypes.TEXT_SSA -> "SSA";
            case MimeTypes.TEXT_VTT -> "VTT";
            case MimeTypes.APPLICATION_TTML -> "TTML";
            case MimeTypes.APPLICATION_TX3G -> "TX3G";
            case MimeTypes.APPLICATION_DVBSUBS -> "DVB";
            default -> mimeType;
       };
    }
}
