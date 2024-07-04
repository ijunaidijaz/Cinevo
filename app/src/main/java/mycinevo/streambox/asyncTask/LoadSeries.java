package mycinevo.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import org.json.JSONArray;

import mycinevo.streambox.interfaces.LoadSuccessListener;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.helper.Helper;
import mycinevo.streambox.util.helper.JSHelper;
import mycinevo.streambox.util.helper.SPHelper;

public class LoadSeries extends AsyncTask<String, String, String> {

    private final Helper helper;
    private final JSHelper jsHelper;
    private final SPHelper spHelper;
    private final LoadSuccessListener listener;
    private String msg = "";

    public LoadSeries(Context ctx, LoadSuccessListener listener) {
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
            // Fetch series categories
            String jsonCategory  = ApplicationUtil.responsePost(spHelper.getAPI(), helper.getAPIRequest("get_series_categories", spHelper.getUserName(), spHelper.getPassword()));
            JSONArray arrayCategory = new JSONArray(jsonCategory);
            if (arrayCategory.length() != 0){
                jsHelper.addToSeriesCatData(jsonCategory);
            }  else {
                msg = "No series categories found";
                return "3";
            }

            // Fetch series data
            String jsonSeries  = ApplicationUtil.responsePost(spHelper.getAPI(), helper.getAPIRequest("get_series", spHelper.getUserName(), spHelper.getPassword()));
            JSONArray jsonarray = new JSONArray(jsonSeries);
            if (jsonarray.length() != 0){
                jsHelper.setSeriesSize(jsonarray.length());
                jsHelper.addToSeriesData(jsonSeries);
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
}