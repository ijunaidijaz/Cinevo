package mycinevo.streambox.adapter.epg;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import org.jetbrains.annotations.NotNull;

import java.util.List;

import mycinevo.streambox.R;
import mycinevo.streambox.item.ItemLive;
import mycinevo.streambox.util.ApplicationUtil;

public class AdapterLiveEpg extends RecyclerView.Adapter<AdapterLiveEpg.ViewHolder> {

    private final Context context;
    private final List<ItemLive> arrayList;
    private final RecyclerItemClickListener listener;
    private int row_index = 0;
    private final Boolean isTvBox;

    public AdapterLiveEpg(Context context, List<ItemLive> arrayList, RecyclerItemClickListener listener) {
        this.context = context;
        this.arrayList = arrayList;
        this.listener = listener;
        isTvBox = ApplicationUtil.isTvBox(context);
    }

    @NotNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_epg_live_list,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {

        try {
            Picasso.get()
                    .load(arrayList.get(position).getStreamIcon().isEmpty() ? "null" : arrayList.get(position).getStreamIcon())
                    .resize(300, 300)
                    .centerCrop()
                    .placeholder(R.color.bg_color_load)
                    .into(holder.iv_logo);
        } catch (Exception e) {
            e.printStackTrace();
        }

        holder.tv_name.setText(arrayList.get(position).getName());
        holder.rl_epg_live.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()), holder.getAbsoluteAdapterPosition()));

        if (row_index > -1) {
            if (row_index == position) {
                if (Boolean.TRUE.equals(isTvBox)){
                    holder.rl_epg_live.requestFocus();
                }
                holder.tv_name.setTextColor(ContextCompat.getColor(context, R.color.color_select));
                holder.vw_live.setVisibility(View.VISIBLE);
            } else {
                holder.tv_name.setTextColor(ContextCompat.getColor(context, R.color.white));
                holder.vw_live.setVisibility(View.GONE);
            }
        } else {
            holder.tv_name.setTextColor(ContextCompat.getColor(context, R.color.white));
            holder.vw_live.setVisibility(View.GONE);
        }
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        private final RelativeLayout rl_epg_live;
        private final ImageView iv_logo;
        private final TextView tv_name;
        private final View vw_live;

        public ViewHolder(View view) {
            super(view);
            iv_logo = view.findViewById(R.id.iv_live_logo);
            tv_name = view.findViewById(R.id.tv_live_name);
            rl_epg_live = view.findViewById(R.id.rl_epg_live);
            vw_live = itemView.findViewById(R.id.vw_live);
        }
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemLive itemLive, int position);
    }

    @SuppressLint("NotifyDataSetChanged")
    public void select(int position) {
        row_index = position;
        notifyDataSetChanged();
    }
}
