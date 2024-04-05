package mycinevo.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import org.json.JSONArray;

import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.SharedPref;
import mycinevo.streambox.util.helper.Helper;
import mycinevo.streambox.util.helper.JSHelper;
import mycinevo.streambox.interfaces.SuccessListener;

public class LoadMovies extends AsyncTask<String, String, String> {

    private final Helper helper;
    private final JSHelper jsHelper;
    private final SharedPref sharedPref;
    private final SuccessListener listener;

    public LoadMovies(Context ctx, SuccessListener listener) {
        this.listener = listener;
        sharedPref = new SharedPref(ctx);
        helper = new Helper(ctx);
        jsHelper = new JSHelper(ctx);
    }

    @Override
    protected void onPreExecute() {
        jsHelper.removeAllMovies();
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String... strings) {
        try {

            String json_category = ApplicationUtil.responsePost(sharedPref.getAPI(), helper.getAPIRequest("get_vod_categories", sharedPref.getUserName(), sharedPref.getPassword()));
            if (!json_category.isEmpty()){
                JSONArray arrayCategory = new JSONArray(json_category);
                if (arrayCategory.length() > 0){
                    jsHelper.addToMovieCatData(json_category);
                }
            }

            String json = ApplicationUtil.responsePost(sharedPref.getAPI(), helper.getAPIRequest("get_vod_streams",sharedPref.getUserName(), sharedPref.getPassword()));
            if (!json.isEmpty()){
                JSONArray jsonarray = new JSONArray(json);
                if (jsonarray.length() > 0){
                    jsHelper.setMovieSize(jsonarray.length());
                    jsHelper.addToMovieData(json);
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
}