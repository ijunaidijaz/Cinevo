package nemosofts.streambox.asyncTask;

import android.content.Context;
import android.os.AsyncTask;

import androidx.annotation.NonNull;

import java.util.ArrayList;

import nemosofts.streambox.interfaces.GetCategoryListener;
import nemosofts.streambox.item.ItemCat;
import nemosofts.streambox.util.helper.JSHelper;

public class GetCategory extends AsyncTask<String, String, String> {

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
            if (Boolean.TRUE.equals(pageType == 1)){
                itemCat.addAll(jsHelper.getCategoryLive());
            } else if (Boolean.TRUE.equals(pageType == 2)){
                itemCat.addAll(jsHelper.getCategoryMovie());
            } else if (Boolean.TRUE.equals(pageType == 3)){
                itemCat.addAll(jsHelper.getCategorySeries());
            } else if (Boolean.TRUE.equals(pageType == 4) || Boolean.TRUE.equals(pageType == 5)){
                ArrayList<ItemCat> arrayList = new ArrayList<>(jsHelper.getCategoryPlaylist(pageType));
                for (int i = 0; i < arrayList.size(); i++) {
                    addOrUpdateItem(itemCat, String.valueOf(i) , arrayList.get(i).getName());
                }
            }
            return "1";
        } catch (Exception e) {
            e.printStackTrace();
            return "0";
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
        listener.onEnd(s,itemCat);
        super.onPostExecute(s);
    }
}