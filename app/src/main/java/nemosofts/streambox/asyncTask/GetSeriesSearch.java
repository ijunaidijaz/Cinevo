package nemosofts.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import java.util.ArrayList;
import java.util.Collections;

import nemosofts.streambox.interfaces.GetSeriesListener;
import nemosofts.streambox.item.ItemSeries;
import nemosofts.streambox.util.helper.JSHelper;

public class GetSeriesSearch extends AsyncTask<String, String, String> {

    private final JSHelper jsHelper;
    private final GetSeriesListener listener;
    private final ArrayList<ItemSeries> itemSeries = new ArrayList<>();
    private final String searchText;

    public GetSeriesSearch(Context ctx, String searchText, GetSeriesListener listener) {
        this.listener = listener;
        this.searchText = searchText;
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
            ArrayList<ItemSeries> arrayList = new ArrayList<>(jsHelper.getSeriesSearch(searchText));
            Collections.reverse(arrayList);
            int limit = Math.min(20, arrayList.size());
            for (int j = 0; j < limit; j++) {
                itemSeries.add(arrayList.get(j));
            }
            return "1";
        } catch (Exception e) {
            e.printStackTrace();
            return "0";
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s,itemSeries);
        super.onPostExecute(s);
    }
}