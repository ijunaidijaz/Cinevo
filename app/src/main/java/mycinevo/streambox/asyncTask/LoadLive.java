package mycinevo.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import org.json.JSONArray;

import mycinevo.streambox.interfaces.LiveListener;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.SharedPref;
import mycinevo.streambox.util.helper.Helper;
import mycinevo.streambox.util.helper.JSHelper;

public class LoadLive extends AsyncTask<String, String, String> {

    private final JSHelper jsHelper;
    private final Helper helper;
    private final SharedPref sharedPref;
    private final LiveListener listener;

    public LoadLive(Context ctx, LiveListener listener) {
        this.listener = listener;
        sharedPref = new SharedPref(ctx);
        helper = new Helper(ctx);
        jsHelper = new JSHelper(ctx);
    }

    @Override
    protected void onPreExecute() {
        jsHelper.removeAllLive();
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String... strings) {
        try {
            String json_category = ApplicationUtil.responsePost(sharedPref.getAPI(), helper.getAPIRequest("get_live_categories", sharedPref.getUserName(), sharedPref.getPassword()));
            if (!json_category.isEmpty()){
                JSONArray arrayCategory = new JSONArray(json_category);
                if (arrayCategory.length() > 0){
                    jsHelper.addToCatLiveList(json_category);
                }
            }

            String json = ApplicationUtil.responsePost(sharedPref.getAPI(), helper.getAPIRequest("get_live_streams",sharedPref.getUserName(), sharedPref.getPassword()));
            if (!json.isEmpty()){
                JSONArray jsonarray = new JSONArray(json);
                if (jsonarray.length() > 0){
                    jsHelper.setLiveSize(jsonarray.length());
                    jsHelper.addToLiveData(json);
                }
            }

            return "1";
        } catch (Exception e) {
            e.printStackTrace();
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s);
        super.onPostExecute(s);
    }

    @Override
    protected void onCancelled(String s) {
        listener.onCancel(s);
        super.onCancelled(s);
    }
}