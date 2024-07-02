package mycinevo.streambox.asyncTask;

import android.os.AsyncTask;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;

import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.interfaces.UsersListener;
import mycinevo.streambox.item.ItemUsers;
import mycinevo.streambox.util.ApplicationUtil;
import okhttp3.RequestBody;

public class LoadUsers extends AsyncTask<String, String, String> {

    private final RequestBody requestBody;
    private final UsersListener listener;
    private final ArrayList<ItemUsers> arrayList = new ArrayList<>();
    private String verifyStatus = "0", message = "";

    public LoadUsers(UsersListener usersListener, RequestBody requestBody) {
        this.listener = usersListener;
        this.requestBody = requestBody;
    }

    @Override
    protected void onPreExecute() {
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String... strings) {
        try {
            String json = ApplicationUtil.responsePost(Callback.API_URL, requestBody);
            JSONObject mainJson = new JSONObject(json);
            JSONArray jsonArray = mainJson.getJSONArray(Callback.TAG_ROOT);

            for (int i = 0; i < jsonArray.length(); i++) {
                JSONObject obj = jsonArray.getJSONObject(i);

                if (!obj.has(Callback.TAG_SUCCESS)) {
                    String id = obj.getString("id");
                    String user_type = obj.getString("user_type");
                    String user_name = obj.getString("user_name");
                    String user_password = obj.getString("user_password");
                    String dns_base = obj.getString("dns_base");
                    String device_id = obj.getString("device_id");

                    ItemUsers objItem = new ItemUsers(id, user_type, user_name, user_password, dns_base, device_id);
                    arrayList.add(objItem);
                } else {
                    verifyStatus = obj.getString(Callback.TAG_SUCCESS);
                    message = obj.getString(Callback.TAG_MSG);
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
        listener.onEnd(s, verifyStatus, message, arrayList);
        super.onPostExecute(s);
    }
}