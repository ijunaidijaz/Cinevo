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
    private final String categoryName;
    private final int page;
    private static final int ITEMS_PER_PAGE = 10;

    public GetLivePlaylist(Context ctx, int page, String categoryName, GetLiveListener listener) {
        this.listener = listener;
        this.categoryName = categoryName;
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
            // Retrieve the entire live playlist
            ArrayList<ItemLive> allItems = new ArrayList<>(jsHelper.getLivePlaylist());

            // Filter the items by the specified category
            ArrayList<ItemLive> filteredItems = new ArrayList<>();
            for (ItemLive item : allItems) {
                if (item.getCatName().equals(categoryName)) {
                    filteredItems.add(item);
                }
            }

            // Reverse the order if required
            if (Boolean.TRUE.equals(jsHelper.getIsLiveOrder())) {
                Collections.reverse(filteredItems);
            }

            // Apply pagination
            int startIndex = (page - 1) * ITEMS_PER_PAGE;
            int endIndex = Math.min(startIndex + ITEMS_PER_PAGE, filteredItems.size());
            for (int i = startIndex; i < endIndex; i++) {
                itemLives.add(filteredItems.get(i));
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