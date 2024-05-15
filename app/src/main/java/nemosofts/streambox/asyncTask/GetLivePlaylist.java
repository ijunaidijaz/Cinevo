package nemosofts.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.Collections;

import nemosofts.streambox.interfaces.GetLiveListener;
import nemosofts.streambox.item.ItemLive;
import nemosofts.streambox.util.helper.JSHelper;

public class GetLivePlaylist extends AsyncTask<String, String, String> {

    private final JSHelper jsHelper;
    private final GetLiveListener listener;
    private final ArrayList<ItemLive> itemLives = new ArrayList<>();
    private final String cat_name;
    private final int page;
    int itemsPerPage = 10;

    public GetLivePlaylist(Context ctx, int page, String cat_name, GetLiveListener listener) {
        this.listener = listener;
        this.cat_name = cat_name;
        this.page = page;
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
            final ArrayList<ItemLive> arrayList = new ArrayList<>();
            final ArrayList<ItemLive> arrayListAll = new ArrayList<>(jsHelper.getLivePlaylist());
            for (int i = 0; i < arrayListAll.size(); i++) {
                addOrUpdateItem(arrayList, cat_name, arrayListAll.get(i));
            }

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
            return "1";
        } catch (Exception e) {
            e.printStackTrace();
            return "0";
        }
    }

    private void addOrUpdateItem(ArrayList<ItemLive> arrayList, String catName, @NonNull ItemLive itemLive) {
        boolean idExists = itemLive.getCatName().equals(catName);
        if (idExists) {
            arrayList.add(itemLive);
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s,itemLives);
        super.onPostExecute(s);
    }
}