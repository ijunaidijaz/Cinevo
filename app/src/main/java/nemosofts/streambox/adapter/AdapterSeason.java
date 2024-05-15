package nemosofts.streambox.adapter;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemSeasons;
import nemosofts.streambox.util.ApplicationUtil;

public class AdapterSeason extends RecyclerView.Adapter<AdapterSeason.MyViewHolder> {

    private int row_index = 0;
    private final Context context;
    private final List<ItemSeasons> arrayList;
    private final RecyclerItemClickListener listener;
    private final Boolean isTvBox;

    public static class MyViewHolder extends RecyclerView.ViewHolder {

        private final TextView tv_season_name;
        private final RelativeLayout rl_season;

        private MyViewHolder(View view) {
            super(view);
            tv_season_name = view.findViewById(R.id.tv_season_name);
            rl_season = view.findViewById(R.id.rl_season);
        }
    }

    public AdapterSeason(Context context, List<ItemSeasons> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.context = context;
        this.listener = listener;
        isTvBox  = ApplicationUtil.isTvBox(context);
    }

    @NonNull
    @Override
    public MyViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_seasons_list, parent, false);
        return new MyViewHolder(itemView);
    }

    @SuppressLint("NotifyDataSetChanged")
    @Override
    public void onBindViewHolder(@NonNull MyViewHolder holder, int position) {

        holder.tv_season_name.setText(arrayList.get(position).getName());

        holder.rl_season.setOnClickListener(v -> {
            if (!arrayList.get(holder.getAbsoluteAdapterPosition()).getSeasonNumber().equals("0")){
                listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()), holder.getAbsoluteAdapterPosition());
                row_index = holder.getAbsoluteAdapterPosition();
                notifyDataSetChanged();
            }
        });

        if (row_index > -1) {
            if (row_index == position) {
                if (Boolean.TRUE.equals(isTvBox)){
                    holder.rl_season.requestFocus();
                }
                holder.tv_season_name.setTextColor(ContextCompat.getColor(context, R.color.color_select));
            } else {
                holder.tv_season_name.setTextColor(ContextCompat.getColor(context, R.color.white));
            }
        } else {
            holder.tv_season_name.setTextColor(ContextCompat.getColor(context, R.color.white));
        }
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
        void onClickListener(ItemSeasons itemSeasons, int position);
    }
}