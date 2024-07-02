package mycinevo.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;

import mycinevo.streambox.interfaces.EpgListener;
import mycinevo.streambox.item.ItemEpg;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.helper.SPHelper;
import okhttp3.RequestBody;

public class LoadEpg extends AsyncTask<String, String, String> {

    private final SPHelper spHelper;
    private final RequestBody requestBody;
    private final EpgListener listener;
    private final ArrayList<ItemEpg> arrayList = new ArrayList<>();

    public LoadEpg(Context ctx, EpgListener listener, RequestBody requestBody) {
        this.listener = listener;
        this.requestBody = requestBody;
        spHelper = new SPHelper(ctx);
    }

    @Override
    protected void onPreExecute() {
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String... strings) {
        try {

            String json = ApplicationUtil.responsePost(spHelper.getAPI(), requestBody);
            JSONObject jsonObject = new JSONObject(json);

            if (jsonObject.has("epg_listings")) {

                JSONArray c =  jsonObject.getJSONArray("epg_listings");

                for (int i = 0; i < c.length(); i++) {
                    JSONObject jsonobject = c.getJSONObject(i);

                    String start = jsonobject.getString("start");
                    String end = jsonobject.getString("end");
                    String title = jsonobject.getString("title");
                    String start_timestamp = jsonobject.getString("start_timestamp");
                    String stop_timestamp = jsonobject.getString("stop_timestamp");

                    ItemEpg objItem = new ItemEpg(start, end, title, start_timestamp, stop_timestamp);
                    arrayList.add(objItem);
                }
            }

            return "1";
        } catch (Exception ee) {
            ee.printStackTrace();
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s, arrayList);
        super.onPostExecute(s);
    }

}