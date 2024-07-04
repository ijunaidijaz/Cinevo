package mycinevo.streambox.asyncTask;

import android.annotation.SuppressLint;
import android.content.Context;
import android.net.Uri;
import android.os.AsyncTask;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import mycinevo.streambox.R;
import mycinevo.streambox.interfaces.LoadPlaylistListener;
import mycinevo.streambox.item.ItemPlaylist;
import mycinevo.streambox.util.HttpsTrustManager;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;

public class LoadPlaylist extends AsyncTask<String, String, String> {

    @SuppressLint("StaticFieldLeak")
    private final Context ctx;
    private final LoadPlaylistListener listener;
    private final ArrayList<ItemPlaylist> playlist = new ArrayList<>();
    private final Boolean isFile;
    private final String filePath;
    private String msg = "";

    public LoadPlaylist(Context ctx, Boolean isFile, String filePath, LoadPlaylistListener listener) {
        this.ctx = ctx;
        this.listener = listener;
        this.isFile = isFile;
        this.filePath = filePath;
    }

    @Override
    protected void onPreExecute() {
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String... strings) {
        BufferedReader reader = null;
        InputStream inputStream = null;
        try {
            if (Boolean.TRUE.equals(isFile)){
                inputStream = ctx.getContentResolver().openInputStream(Uri.parse(filePath));
                if (inputStream == null) {
                    msg = "File not found or unable to open";
                    return "0";
                }
                reader = new BufferedReader(new InputStreamReader(inputStream));
            }  else {
                HttpsTrustManager.allowAllSSL();
                OkHttpClient client = new OkHttpClient.Builder().build();
                Request request = new Request.Builder().url(filePath).build();
                Response response = client.newCall(request).execute();
                if (!response.isSuccessful()) {
                    msg = "HTTP request failed";
                    return "0";
                }
                if (response.body() == null) {
                    msg = "Response body is empty";
                    return "0";
                }
                reader = new BufferedReader(new InputStreamReader(response.body().byteStream()));
            }

            String line;
            String name = null;
            String logo = null;
            String group = null;

            while ((line = reader.readLine()) != null) {
                if (line.startsWith("#EXTINF:-1")) {
                    String data = line.substring("#EXTINF:-1,".length()).trim();

                    try {
                        Pattern pattern = Pattern.compile("tvg-name=\"(.*?)\"");
                        Matcher matcher = pattern.matcher(data);
                        if (matcher.find()) {
                            name = matcher.group(1);
                        } else {
                            Pattern pattern2 = Pattern.compile("group-title=\"([^\"]*)\",(.*?)$");
                            Matcher matcher2 = pattern2.matcher(line);
                            if (matcher2.find()) {
                                name = matcher2.group(2);
                            } else {
                                name = "";
                            }
                        }
                    } catch (Exception e) {
                        name = "";
                    }

                    try {
                        Pattern pattern_logo = Pattern.compile("tvg-logo=\"(.*?)\"");
                        Matcher matcher_logo = pattern_logo.matcher(data);
                        if (matcher_logo.find()) {
                            logo = matcher_logo.group(1);
                        } else {
                            logo = "";
                        }
                    } catch (Exception e) {
                        logo = "";
                    }

                    try {
                        Pattern pattern_group = Pattern.compile("group-title=\"(.*?)\"");
                        Matcher matcher_group = pattern_group.matcher(data);
                        if (matcher_group.find()) {
                            group = matcher_group.group(1);
                        } else {
                            group = "";
                        }
                    } catch (Exception e) {
                        group = "";
                    }

                } else if (line.startsWith("http") || line.startsWith("https") && (name != null && logo != null && group != null)) {
                    ItemPlaylist objItem = new ItemPlaylist(name, logo, group, line);
                    playlist.add(objItem);
                    name = null;
                    logo = null;
                    group = null;
                }
            }
            msg = "Successfully";
            return "1";
        } catch (Exception e) {
            e.printStackTrace();
            msg = ctx.getString(R.string.err_server_not_connected);
            return "0";
        } finally {
            try {
                if (inputStream != null) {
                    inputStream.close();
                }
                if (reader != null) {
                    reader.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s, msg, playlist);
        super.onPostExecute(s);
    }

}