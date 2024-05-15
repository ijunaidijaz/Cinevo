package mycinevo.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import mycinevo.streambox.interfaces.GetSeriesListener;
import mycinevo.streambox.item.ItemSeries;
import mycinevo.streambox.util.helper.DBHelper;
import mycinevo.streambox.util.helper.JSHelper;

public class GetSeries extends AsyncTask<String, String, String> {

    private final DBHelper dbHelper;
    private final JSHelper jsHelper;
    private final GetSeriesListener listener;
    private final ArrayList<ItemSeries> itemSeries = new ArrayList<>();
    private final int is_page;
    private final String cat_id;
    private final int page;
    int itemsPerPage = 15;

    public GetSeries(Context ctx, int page, String cat_id, int is_page, GetSeriesListener listener) {
        this.listener = listener;
        this.is_page = is_page;
        this.cat_id = cat_id;
        this.page = page;
        jsHelper = new JSHelper(ctx);
        dbHelper = new DBHelper(ctx);
    }

    @Override
    protected void onPreExecute() {
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String... strings) {
        try {
            if (Boolean.TRUE.equals(is_page == 1)){
                itemSeries.addAll(dbHelper.getSeries(DBHelper.TABLE_FAV_SERIES, jsHelper.getIsSeriesOrder()));
            } else  if (Boolean.TRUE.equals(is_page == 2)){
                itemSeries.addAll(dbHelper.getSeries(DBHelper.TABLE_RECENT_SERIES, jsHelper.getIsSeriesOrder()));
            } else if (Boolean.TRUE.equals(is_page == 3)){
                final ArrayList<ItemSeries> arrayList = new ArrayList<>(jsHelper.getSeriesRe());
                if (!arrayList.isEmpty()){
                    Collections.sort(arrayList, new Comparator<ItemSeries>() {
                        @Override
                        public int compare(ItemSeries o1, ItemSeries o2) {
                            return Integer.compare(Integer.parseInt(o1.getSeriesID()), Integer.parseInt(o2.getSeriesID()));
                        }
                    });
                    Collections.reverse(arrayList);
                    for (int i = 0; i < arrayList.size(); i++) {
                        itemSeries.add(arrayList.get(i));
                        if (i == 49){
                            break;
                        }
                    }
                    if (Boolean.TRUE.equals(jsHelper.getIsSeriesOrder()) && !itemSeries.isEmpty()){
                        Collections.reverse(itemSeries);
                    }
                }
            } else {
                final ArrayList<ItemSeries> arrayList = new ArrayList<>(jsHelper.getSeries(cat_id));
                if (Boolean.TRUE.equals(jsHelper.getIsSeriesOrder())){
                    Collections.reverse(arrayList);
                }
                if (!arrayList.isEmpty()){
                    int startIndex = (page - 1) * itemsPerPage;
                    int endIndex = Math.min(startIndex + itemsPerPage, arrayList.size());
                    for (int i = startIndex; i < endIndex; i++) {
                        itemSeries.add(arrayList.get(i));
                    }
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
        listener.onEnd(s,itemSeries);
        super.onPostExecute(s);
    }
}