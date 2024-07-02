package mycinevo.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import org.json.JSONArray;

import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.interfaces.DataListener;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.helper.SPHelper;
import mycinevo.streambox.util.helper.Helper;
import mycinevo.streambox.util.helper.JSHelper;

public class LoadData extends AsyncTask<String, String, String> {

    private final JSHelper jsHelper;
    private final Helper helper;
    private final SPHelper spHelper;
    private final DataListener listener;
    private JSONArray arrayLive, arraySeries, arrayMovies;

    public LoadData(Context ctx, DataListener listener) {
        this.listener = listener;
        helper = new Helper(ctx);
        spHelper = new SPHelper(ctx);
        jsHelper = new JSHelper(ctx);
    }

    @Override
    protected void onPreExecute() {
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String... strings) {
        try {
            if (jsHelper.getUpdateDate().isEmpty()){
                jsHelper.setUpdateDate();
                return "1";
            } else {
                // 5 Hours
                if (Boolean.TRUE.equals(ApplicationUtil.calculateUpdateHours(jsHelper.getUpdateDate(), spHelper.getAutoUpdate()))){
                    jsHelper.setUpdateDate();

                    try {
                        if (!spHelper.getCurrent(Callback.TAG_SERIES).isEmpty()){
                            String json_series = ApplicationUtil.responsePost(spHelper.getAPI(), helper.getAPIRequest("get_series", spHelper.getUserName(), spHelper.getPassword()));
                            if (!json_series.isEmpty()){
                                arraySeries = new JSONArray(json_series);
                                if (arraySeries.length() != 0 && arraySeries.length() != jsHelper.getSeriesSize()){
                                    jsHelper.setSeriesSize(arraySeries.length());
                                    jsHelper.addToSeriesData(json_series);
                                    Callback.successSeries = "1";
                                }
                            }
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                    }

                    try {
                        if (!spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty()){
                            String json_movie = ApplicationUtil.responsePost(spHelper.getAPI(), helper.getAPIRequest("get_vod_streams", spHelper.getUserName(), spHelper.getPassword()));
                            if (!json_movie.isEmpty()){
                                arrayMovies = new JSONArray(json_movie);
                                if (arrayMovies.length() != 0 && arrayMovies.length() != jsHelper.getMoviesSize()){
                                    jsHelper.setMovieSize(arrayMovies.length());
                                    jsHelper.addToMovieData(json_movie);
                                    Callback.successMovies = "1";
                                }
                            }
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                    }

                    try {
                        if (!spHelper.getCurrent(Callback.TAG_TV).isEmpty()){
                            String json_live = ApplicationUtil.responsePost(spHelper.getAPI(), helper.getAPIRequest("get_live_streams", spHelper.getUserName(), spHelper.getPassword()));
                            if (!json_live.isEmpty()){
                                arrayLive = new JSONArray(json_live);
                                if (arrayLive.length() != 0 && arrayLive.length() != jsHelper.getLiveSize()){
                                    jsHelper.setLiveSize(arrayLive.length());
                                    jsHelper.addToLiveData(json_live);
                                    Callback.successLive = "1";
                                }
                            }
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                    return "1";
                } else {
                    return "2";
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s, arrayLive, arraySeries, arrayMovies);
        super.onPostExecute(s);
    }
}