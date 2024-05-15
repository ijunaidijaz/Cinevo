package mycinevo.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import mycinevo.streambox.interfaces.GetLiveListener;
import mycinevo.streambox.item.ItemLive;
import mycinevo.streambox.util.helper.DBHelper;
import mycinevo.streambox.util.helper.JSHelper;

public class GetLive extends AsyncTask<String, String, String> {

    private final DBHelper dbHelper;
    private final JSHelper jsHelper;
    private final GetLiveListener listener;
    private final ArrayList<ItemLive> itemLives = new ArrayList<>();
    private final int is_page;
    private final String cat_id;
    private final int page;
    int itemsPerPage = 15;

    public GetLive(Context ctx, int page, String cat_id, int is_page, GetLiveListener listener) {
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
                itemLives.addAll(dbHelper.getLive(DBHelper.TABLE_FAV_LIVE, jsHelper.getIsLiveOrder()));
            } else if (Boolean.TRUE.equals(is_page == 2)){
                itemLives.addAll(dbHelper.getLive(DBHelper.TABLE_RECENT_LIVE, jsHelper.getIsLiveOrder()));
            } else if (Boolean.TRUE.equals(is_page == 3)){
                final ArrayList<ItemLive> arrayList = new ArrayList<>(jsHelper.getLiveRe());
                if (!arrayList.isEmpty()){
                    Collections.sort(arrayList, new Comparator<ItemLive>() {
                        @Override
                        public int compare(ItemLive o1, ItemLive o2) {
                            return Integer.compare(Integer.parseInt(o1.getStreamID()), Integer.parseInt(o2.getStreamID()));
                        }
                    });
                    Collections.reverse(arrayList);
                    for (int i = 0; i < arrayList.size(); i++) {
                        itemLives.add(arrayList.get(i));
                        if (i == 49){
                            break;
                        }
                    }
                    if (Boolean.TRUE.equals(jsHelper.getIsLiveOrder()) && !itemLives.isEmpty()){
                        Collections.reverse(itemLives);
                    }
                }
            } else {
                final ArrayList<ItemLive> arrayList = new ArrayList<>(jsHelper.getLive(cat_id, false));
                if (!arrayList.isEmpty()){
                    if (Boolean.TRUE.equals(jsHelper.getIsLiveOrder())){
                        Collections.reverse(arrayList);
                    }
                    if (!arrayList.isEmpty()){
                        int startIndex = (page - 1) * itemsPerPage;
                        int endIndex = Math.min(startIndex + itemsPerPage, arrayList.size());
                        for (int i = startIndex; i < endIndex; i++) {
                            itemLives.add(arrayList.get(i));
                        }
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
        listener.onEnd(s,itemLives);
        super.onPostExecute(s);
    }
}