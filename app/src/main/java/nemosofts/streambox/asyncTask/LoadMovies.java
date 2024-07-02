package nemosofts.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import org.json.JSONArray;

import nemosofts.streambox.interfaces.LoadSuccessListener;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.helper.JSHelper;
import nemosofts.streambox.util.helper.SPHelper;

public class LoadMovies extends AsyncTask<String, String, String> {

    private final Helper helper;
    private final JSHelper jsHelper;
    private final SPHelper spHelper;
    private final LoadSuccessListener listener;
    private String msg = "";

    public LoadMovies(Context ctx, LoadSuccessListener listener) {
        this.listener = listener;
        spHelper = new SPHelper(ctx);
        helper = new Helper(ctx);
        jsHelper = new JSHelper(ctx);
    }

    @Override
    protected void onPreExecute() {
        jsHelper.removeAllMovies(); // Clear existing movie data before loading new data
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String... strings) {
        try {
            // Fetch movie categories
            String jsonCategory = ApplicationUtil.responsePost(spHelper.getAPI(), helper.getAPIRequest("get_vod_categories", spHelper.getUserName(), spHelper.getPassword()));
            JSONArray arrayCategory = new JSONArray(jsonCategory);
            if (arrayCategory.length() != 0){
                jsHelper.addToMovieCatData(jsonCategory);
            }  else {
                msg = "No movie categories found";
                return "3";
            }

            // Fetch movie streams
            String jsonMovies = ApplicationUtil.responsePost(spHelper.getAPI(), helper.getAPIRequest("get_vod_streams", spHelper.getUserName(), spHelper.getPassword()));
            JSONArray jsonarray = new JSONArray(jsonMovies);
            if (jsonarray.length() != 0){
                jsHelper.setMovieSize(jsonarray.length());
                jsHelper.addToMovieData(jsonMovies);
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