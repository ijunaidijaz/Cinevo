package nemosofts.streambox.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.item.ItemUsersDB;
import nemosofts.streambox.util.helper.DBHelper;

public class AdapterUsers extends RecyclerView.Adapter<AdapterUsers.ViewHolder> {

    private final Context ctx;
    private final List<ItemUsersDB> arrayList;
    private final RecyclerItemClickListener listener;
    private final DBHelper dbHelper;

    public AdapterUsers(Context ctx, List<ItemUsersDB> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
        this.ctx = ctx;
        dbHelper = new DBHelper(ctx);
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        private final RelativeLayout rl_users_list;
        private final TextView tv_any_name;
        private final TextView tv_users_url;
        private final TextView tv_users_name;

        public ViewHolder(View itemView) {
            super(itemView);
            rl_users_list = itemView.findViewById(R.id.rl_users_list);
            tv_any_name = itemView.findViewById(R.id.tv_users_any_name);
            tv_users_url = itemView.findViewById(R.id.tv_users_url);
            tv_users_name = itemView.findViewById(R.id.tv_users_name);
        }
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_users_list,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {

        String any_name;
        String users_name;
        String usersUrl;
        switch (arrayList.get(position).getUserType()) {
            case "xui" -> {
                any_name = arrayList.get(position).getAnyName();
                users_name = ctx.getString(R.string.user_list_user_name) + " " + arrayList.get(position).getUseName();
                usersUrl =  "Login Type:  Xtream Codes / Xui";
            }
            case "stream" -> {
                any_name = arrayList.get(position).getAnyName();
                users_name = ctx.getString(R.string.user_list_user_name) + " " + arrayList.get(position).getUseName();
                usersUrl =  "Login Type:  1-stream";
            }
            case "playlist" -> {
                any_name = arrayList.get(position).getAnyName();
                users_name = "Login Type:  M3U Playlist";
                usersUrl =  ctx.getString(R.string.user_list_url)+" " + arrayList.get(position).getUserURL();
            }
            default -> {
                any_name = arrayList.get(position).getAnyName();
                users_name = ctx.getString(R.string.user_list_user_name) + " " + arrayList.get(position).getUseName();
                usersUrl =  ctx.getString(R.string.user_list_url)+" " + arrayList.get(position).getUserURL();
            }
        }

        holder.tv_any_name.setText(any_name);
        holder.tv_users_url.setText(usersUrl);
        holder.tv_users_name.setText(users_name);

        holder.rl_users_list.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()), position));
        holder.rl_users_list.setOnLongClickListener(v -> {
            DialogUtil.DeleteDialog(ctx, () -> {
                try {
                    dbHelper.removeFromUser(arrayList.get(holder.getAbsoluteAdapterPosition()).getId());
                    arrayList.remove(holder.getAbsoluteAdapterPosition());
                    notifyItemRemoved(holder.getAbsoluteAdapterPosition());
                    Toast.makeText(ctx, ctx.getString(R.string.delete), Toast.LENGTH_SHORT).show();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            });
            return false;
        });
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemUsersDB itemUsers, int position);
    }
}
