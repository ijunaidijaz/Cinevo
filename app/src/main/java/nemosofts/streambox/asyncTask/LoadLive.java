package nemosofts.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import org.json.JSONArray;

import nemosofts.streambox.interfaces.LoadSuccessListener;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.util.helper.SPHelper;

public class LoadLive extends AsyncTask<String, String, String> {

    private final JSHelper jsHelper;
    private final Helper helper;
    private final SPHelper spHelper;
    private final LoadSuccessListener listener;
    private String msg = "";

    public LoadLive(Context ctx, LoadSuccessListener listener) {
        this.listener = listener;
        spHelper = new SPHelper(ctx);
        helper = new Helper(ctx);
        jsHelper = new JSHelper(ctx);
    }

    @Override
    protected void onPreExecute() {
        jsHelper.removeAllLive(); // Clear existing live data before loading new data
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String... strings) {
        try {
            // Fetch live stream categories
            String jsonCategory = ApplicationUtil.responsePost(spHelper.getAPI(), helper.getAPIRequest("get_live_categories", spHelper.getUserName(), spHelper.getPassword()));
            JSONArray arrayCategory = new JSONArray(jsonCategory);
            if (arrayCategory.length() != 0){
                jsHelper.addToCatLiveList(jsonCategory);
            }  else {
                msg = "No live categories found";
                return "3";
            }

            String jsonLive = ApplicationUtil.responsePost(spHelper.getAPI(), helper.getAPIRequest("get_live_streams", spHelper.getUserName(), spHelper.getPassword()));
            JSONArray jsonarray = new JSONArray(jsonLive);
            if (jsonarray.length() != 0){
                jsHelper.setLiveSize(jsonarray.length());
                jsHelper.addToLiveData(jsonLive );
            } else {
                msg = "No series found";
                return "3";
            }
            return "1";
        } catch (Exception e) {
            e.printStackTrace();
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s, msg);
        super.onPostExecute(s);
    }

    @Override
    protected void onCancelled(String s) {
        listener.onEnd(s, msg);
        super.onCancelled(s);
    }
}