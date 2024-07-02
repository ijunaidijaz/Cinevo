package mycinevo.streambox.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import mycinevo.streambox.R;
import mycinevo.streambox.item.ItemNotification;

public class AdapterNotification extends RecyclerView.Adapter<AdapterNotification.ViewHolder> {

    private final List<ItemNotification> arrayList;
    private final RecyclerItemClickListener listener;

    public AdapterNotification(List<ItemNotification> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.item_notification ,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {

        holder.tv_notify_title.setText(arrayList.get(position).getTitle());
        holder.tv_notify_sub_title.setText(arrayList.get(position).getMsg());
        holder.tv_notify_on.setText(arrayList.get(position).getDate());
        holder.rl_notify.setOnClickListener(v -> {
            try {
                listener.onClickListener(arrayList.get(position), holder.getAbsoluteAdapterPosition());
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        TextView tv_notify_title;
        TextView tv_notify_sub_title;
        TextView tv_notify_on;
        RelativeLayout rl_notify;

        public ViewHolder(View view) {
            super(view);
            tv_notify_title = view.findViewById(R.id.tv_notify_title);
            tv_notify_sub_title = view.findViewById(R.id.tv_notify_sub_title);
            tv_notify_on = view.findViewById(R.id.tv_notify_on);
            rl_notify = view.findViewById(R.id.rl_notify);
        }
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemNotification notification, int position);
    }
}
