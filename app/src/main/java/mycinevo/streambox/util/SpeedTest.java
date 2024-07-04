package mycinevo.streambox.util;

import android.os.AsyncTask;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;

public class SpeedTest extends AsyncTask<String, String, String> {

    private static long downloadSpeedKbps = 0;
    private final OnSpeedCheckListener listener;

    public SpeedTest(OnSpeedCheckListener listener) {
        this.listener = listener;
    }

    @Override
    protected void onPreExecute() {
        downloadSpeedKbps = 0;
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String... strings) {
        try {
            String downloadUrl = "https://download.envatonemosofts.com/sample.txt";
            downloadSpeedKbps = 0;
            long startTime = System.currentTimeMillis();

            URL url = new URL(downloadUrl);
            HttpURLConnection urlConnection = (HttpURLConnection) url.openConnection();

            try (InputStream in = new BufferedInputStream(urlConnection.getInputStream())) {
                byte[] buffer = new byte[1024];
                int bytesRead;
                long totalBytesRead = 0;
                while ((bytesRead = in.read(buffer)) != -1) {
                    totalBytesRead += bytesRead;
                }

                long endTime = System.currentTimeMillis();
                long totalTimeInMillis = endTime - startTime;

                // Calculate download speed in Kbps (kilobits per second)
                downloadSpeedKbps = (long) ((totalBytesRead * 8 / 1024.0) / (totalTimeInMillis / 1000.0));
            } finally {
                urlConnection.disconnect();
            }

            return "1";
        } catch (Exception e) {
            e.printStackTrace();
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String result) {
        listener.onEnd(result, "Download Speed: " + ApplicationUtil.readableFileSize(downloadSpeedKbps) + " Kbps");
        super.onPostExecute(result);
    }

    public interface OnSpeedCheckListener {
        void onStart();
        void onEnd(String success, String speedMbps);
    }
}