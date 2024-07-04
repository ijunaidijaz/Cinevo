package mycinevo.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import androidx.annotation.NonNull;

import java.util.ArrayList;

import mycinevo.streambox.interfaces.GetCategoryListener;
import mycinevo.streambox.item.ItemCat;
import mycinevo.streambox.util.helper.JSHelper;

public class GetCategory extends AsyncTask<String, String, String> {

    private static final int PAGE_TYPE_LIVE = 1;
    private static final int PAGE_TYPE_MOVIE = 2;
    private static final int PAGE_TYPE_SERIES = 3;
    private static final int PAGE_TYPE_PLAYLIST_4 = 4;
    private static final int PAGE_TYPE_PLAYLIST_5 = 5;

    private final JSHelper jsHelper;
    private final GetCategoryListener listener;
    private final ArrayList<ItemCat> itemCat = new ArrayList<>();
    private final int pageType;

    public GetCategory(Context ctx, int pageType, GetCategoryListener listener) {
        this.listener = listener;
        this.pageType = pageType;
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
            switch (pageType) {
                case PAGE_TYPE_LIVE:
                    itemCat.addAll(jsHelper.getCategoryLive());
                    break;
                case PAGE_TYPE_MOVIE:
                    itemCat.addAll(jsHelper.getCategoryMovie());
                    break;
                case PAGE_TYPE_SERIES:
                    itemCat.addAll(jsHelper.getCategorySeries());
                    break;
                case PAGE_TYPE_PLAYLIST_4:
                case PAGE_TYPE_PLAYLIST_5:
                    ArrayList<ItemCat> arrayList = new ArrayList<>(jsHelper.getCategoryPlaylist(pageType));
                    for (int i = 0; i < arrayList.size(); i++) {
                        addOrUpdateItem(itemCat, String.valueOf(i), arrayList.get(i).getName());
                    }
                    break;
                default:
                    return "0"; // Error
            }
            return "1"; // Success
        } catch (Exception e) {
            e.printStackTrace();
            return "0"; // Error
        }
    }

    protected void addOrUpdateItem(@NonNull ArrayList<ItemCat> itemList, String id, String title) {
        boolean idExists = false;
        for (ItemCat item : itemList) {
            if (item.getName().equals(title)) {
                idExists = true;
                break;
            }
        }
        if (!idExists) {
            itemList.add(new ItemCat(id,title,""));
        }
    }

    @Override
    protected void onPostExecute(String s) {
        boolean success = s.equals("1");
        listener.onEnd(success, itemCat);
        super.onPostExecute(s);
    }
}