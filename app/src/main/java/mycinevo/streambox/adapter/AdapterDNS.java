package mycinevo.streambox.adapter;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import mycinevo.streambox.R;
import mycinevo.streambox.item.ItemDns;
import mycinevo.streambox.util.ApplicationUtil;

public class AdapterDNS extends RecyclerView.Adapter<AdapterDNS.MyViewHolder> {

    private int row_index = 0;
    private final List<ItemDns> arrayList;
    private final RecyclerItemClickListener listener;
    private Boolean isFindFocus = false;
    private final Boolean isTvBox;

    public static class MyViewHolder extends RecyclerView.ViewHolder {

        private final ImageView iv_tick;
        private final TextView tv_dns;
        private final LinearLayout ll_dns_list;

        private MyViewHolder(View view) {
            super(view);
            iv_tick = view.findViewById(R.id.iv_tick);
            tv_dns = view.findViewById(R.id.tv_dns);
            ll_dns_list = view.findViewById(R.id.ll_dns_list);
        }
    }

    public AdapterDNS(Context context, List<ItemDns> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
        isTvBox  = ApplicationUtil.isTvBox(context);
    }

    @NonNull
    @Override
    public MyViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_dns_list, parent, false);
        return new MyViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(@NonNull MyViewHolder holder, int position) {

        holder.tv_dns.setText(arrayList.get(position).getTitle());

        if (row_index > -1) {
            if (row_index == position) {
                if (isFindFocus && isTvBox){
                    holder.ll_dns_list.requestFocus();
                }
                holder.iv_tick.setVisibility(View.VISIBLE);
            } else {
                holder.iv_tick.setVisibility(View.GONE);
            }
        } else {
            holder.iv_tick.setVisibility(View.GONE);
        }

        holder.ll_dns_list.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()),holder.getAbsoluteAdapterPosition()));
    }

    @SuppressLint("NotifyDataSetChanged")
    public void setSelected(int position) {
        row_index = position;
        notifyDataSetChanged();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void setSelectedFocus(int position) {
        isFindFocus = true;
        row_index = position;
        notifyDataSetChanged();
    }

    public String getSelectedBase() {
        return arrayList.get(row_index).getBase();
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemDns itemDns, int position);
    }
}