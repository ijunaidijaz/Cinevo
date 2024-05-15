package nemosofts.streambox.adapter.player;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.nemosofts.view.RoundedImageView;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemLive;
import nemosofts.streambox.util.ApplicationUtil;

public class AdapterLivePlayer extends RecyclerView.Adapter<AdapterLivePlayer.ViewHolder> {

    private final List<ItemLive> arrayList;
    private final Context context;
    private final RecyclerItemClickListener listener;
    private int row_index = 0;
    private final Boolean isTvBox;

    public AdapterLivePlayer(Context context, List<ItemLive> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.context = context;
        this.listener = listener;
        isTvBox = ApplicationUtil.isTvBox(context);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.item_player_live,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        try {
            Picasso.get()
                    .load(arrayList.get(position).getStreamIcon().isEmpty() ? "null" : arrayList.get(position).getStreamIcon())
                    .resize(300, 300)
                    .centerCrop()
                    .noFade()
                    .placeholder(R.color.bg_color_load)
                    .into(holder.iv_logo);
        } catch (Exception e) {
            e.printStackTrace();
        }

        int step = 1;
        for (int i = 1; i < position + 1; i++) {
            step++;
        }
        holder.tv_num.setText(String.valueOf(step));
        holder.tv_name.setText(arrayList.get(position).getName());

        holder.ll_channels_list.setOnClickListener(v -> {
            try {
                listener.onClickListener(arrayList.get(position), holder.getAbsoluteAdapterPosition());
                select(holder.getAbsoluteAdapterPosition());
            } catch (Exception e) {
                e.printStackTrace();
            }
        });

        if (row_index > -1) {
            if (row_index == position) {
                if (Boolean.TRUE.equals(isTvBox)){
                    holder.ll_channels_list.requestFocus();
                }
                holder.tv_num.setTextColor(ContextCompat.getColor(context, R.color.color_select));
                holder.tv_name.setTextColor(ContextCompat.getColor(context, R.color.color_select));
            } else {
                holder.tv_num.setTextColor(ContextCompat.getColor(context, R.color.white));
                holder.tv_name.setTextColor(ContextCompat.getColor(context, R.color.white));
            }
        } else {
            holder.tv_num.setTextColor(ContextCompat.getColor(context, R.color.white));
            holder.tv_name.setTextColor(ContextCompat.getColor(context, R.color.white));
        }
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        RoundedImageView iv_logo;
        TextView tv_name;
        TextView tv_num;
        LinearLayout ll_channels_list;

        public ViewHolder(View view) {
            super(view);
            tv_num = view.findViewById(R.id.tv_num);
            iv_logo = view.findViewById(R.id.iv_live_list_logo);
            tv_name = view.findViewById(R.id.tv_live_list_name);
            ll_channels_list = view.findViewById(R.id.ll_channels_list);
        }
    }

    @SuppressLint("NotifyDataSetChanged")
    public void select(int position) {
        row_index = position;
        notifyDataSetChanged();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemLive itemLive, int position);
    }
}
