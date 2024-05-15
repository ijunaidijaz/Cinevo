package nemosofts.streambox.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemUsers;

public class AdapterDeviceID extends RecyclerView.Adapter<AdapterDeviceID.ViewHolder> {

    private final Context ctx;
    private final List<ItemUsers> arrayList;
    private final RecyclerItemClickListener listener;

    public AdapterDeviceID(Context ctx, List<ItemUsers> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
        this.ctx = ctx;
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        private final RelativeLayout rlUsersList;
        private final TextView tvUsersName;

        public ViewHolder(View itemView) {
            super(itemView);
            rlUsersList = itemView.findViewById(R.id.rl_users_list);
            tvUsersName = itemView.findViewById(R.id.tv_users_name);
        }
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_users_list_d,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        String usersName = ctx.getString(R.string.user_list_user_name)+" " + arrayList.get(position).getUserName();
        holder.tvUsersName.setText(usersName);
        holder.rlUsersList.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()), position));
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemUsers itemUsers, int position);
    }
}
