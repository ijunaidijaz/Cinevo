package nemosofts.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import java.util.ArrayList;

import nemosofts.streambox.interfaces.GetLiveListener;
import nemosofts.streambox.item.ItemLive;
import nemosofts.streambox.util.helper.JSHelper;

public class GetLiveSearch extends AsyncTask<String, String, String> {

    private final JSHelper jsHelper;
    private final GetLiveListener listener;
    private final ArrayList<ItemLive> itemLives = new ArrayList<>();
    private final String searchText;
    private final Boolean isPlaylist;
    private static final int MAX_RESULTS = 20;

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
            ArrayList<ItemLive> resultList = new ArrayList<>();

            if (Boolean.TRUE.equals(isPlaylist)) {
                // Fetch and filter the live playlist
                ArrayList<ItemLive> allItems = new ArrayList<>(jsHelper.getLivePlaylist());
                for (ItemLive item : allItems) {
                    if (item.getName().toLowerCase().contains(searchText.toLowerCase())) {
                        resultList.add(item);
                    }
                }
            } else {
                // Fetch search results directly from the helper
                resultList.addAll(jsHelper.getLivesSearch(searchText));
            }

            // Limit the results to the first 20 items
            int limit = Math.min(MAX_RESULTS, resultList.size());
            for (int i = 0; i < limit; i++) {
                itemLives.add(resultList.get(i));
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