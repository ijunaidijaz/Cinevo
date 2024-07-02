package mycinevo.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import androidx.annotation.NonNull;

import java.util.ArrayList;

import mycinevo.streambox.interfaces.GetLiveListener;
import mycinevo.streambox.item.ItemLive;
import mycinevo.streambox.util.helper.JSHelper;

public class GetLiveSearch extends AsyncTask<String, String, String> {

    private final JSHelper jsHelper;
    private final GetLiveListener listener;
    private final ArrayList<ItemLive> itemLives = new ArrayList<>();
    private final String searchText;
    private final Boolean isPlaylist;

    public GetLiveSearch(Context ctx, Boolean isPlaylist, String searchText, GetLiveListener listener) {
        this.listener = listener;
        this.isPlaylist = isPlaylist;
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
            ArrayList<ItemLive> arrayList = new ArrayList<>();
            if (Boolean.TRUE.equals(isPlaylist)){
                final ArrayList<ItemLive> arrayListAll = new ArrayList<>(jsHelper.getLivePlaylist());
                for (int i = 0; i < arrayListAll.size(); i++) {
                    addOrUpdateItem(arrayList, arrayListAll.get(i));
                }
            } else {
                arrayList.addAll(jsHelper.getLivesSearch(searchText));
            }

            int limit = Math.min(20, arrayList.size());
            for (int j = 0; j < limit; j++) {
                itemLives.add(arrayList.get(j));
            }
            return "1";
        } catch (Exception e) {
            e.printStackTrace();
            return "0";
        }
    }

    private void addOrUpdateItem(ArrayList<ItemLive> arrayList, @NonNull ItemLive itemLive) {
        boolean idExists = itemLive.getName().toLowerCase().contains(searchText);
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