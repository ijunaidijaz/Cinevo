package nemosofts.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import org.json.JSONArray;

import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.helper.SPHelper;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.interfaces.SuccessListener;

public class LoadSeries extends AsyncTask<String, String, String> {

    private final Helper helper;
    private final JSHelper jsHelper;
    private final SPHelper spHelper;
    private final SuccessListener listener;

    public LoadSeries(Context ctx, SuccessListener listener) {
        this.listener = listener;
        spHelper = new SPHelper(ctx);
        helper = new Helper(ctx);
        jsHelper = new JSHelper(ctx);
    }

    @Override
    protected void onPreExecute() {
        jsHelper.removeAllSeries();
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String... strings) {
        try {
            String json_category = ApplicationUtil.responsePost(spHelper.getAPI(), helper.getAPIRequest("get_series_categories", spHelper.getUserName(), spHelper.getPassword()));
            JSONArray arrayCategory = new JSONArray(json_category);
            if (arrayCategory.length() != 0){
                jsHelper.addToSeriesCatData(json_category);
            }  else {
                return "2";
            }

            String json = ApplicationUtil.responsePost(spHelper.getAPI(), helper.getAPIRequest("get_series", spHelper.getUserName(), spHelper.getPassword()));
            JSONArray jsonarray = new JSONArray(json);
            if (jsonarray.length() != 0){
                jsHelper.setSeriesSize(jsonarray.length());
                jsHelper.addToSeriesData(json);
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
}