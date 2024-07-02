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
import nemosofts.streambox.item.ItemEpisodes;
import nemosofts.streambox.util.ApplicationUtil;

public class AdapterEpisodesPlayer extends RecyclerView.Adapter<AdapterEpisodesPlayer.ViewHolder> {

    private final List<ItemEpisodes> arrayList;
    private final Context context;
    private final RecyclerItemClickListener listener;
    private int rowIndex = 0;
    private final Boolean isTvBox;

    public AdapterEpisodesPlayer(Context context, List<ItemEpisodes> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.context = context;
        this.listener = listener;
        isTvBox = ApplicationUtil.isTvBox(context);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.item_player_epi,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        try {
            Picasso.get()
                    .load(arrayList.get(position).getCoverBig().isEmpty() ? "null" : arrayList.get(position).getCoverBig())
                    .resize(300, 300)
                    .centerCrop()
                    .noFade()
                    .placeholder(R.color.bg_color_load)
                    .into(holder.iv_episodes_logo);
        } catch (Exception e) {
            e.printStackTrace();
        }

        int step = 1;
        for (int i = 1; i < position + 1; i++) {
            step++;
        }
        holder.episodes_num.setText(String.valueOf(step));
        holder.episodes_name.setText(arrayList.get(position).getTitle());
        holder.ll_episodes_list.setOnClickListener(v -> {
            try {
                listener.onClickListener(arrayList.get(position), holder.getAbsoluteAdapterPosition());
                select(holder.getAbsoluteAdapterPosition());
            } catch (Exception e) {
                e.printStackTrace();
            }
        });

        if (rowIndex > -1) {
            if (rowIndex == position) {
                if (Boolean.TRUE.equals(isTvBox)){
                    holder.episodes_name.requestFocus();
                }
                holder.episodes_num.setTextColor(ContextCompat.getColor(context, R.color.color_select));
                holder.episodes_name.setTextColor(ContextCompat.getColor(context, R.color.color_select));
            } else {
                holder.episodes_num.setTextColor(ContextCompat.getColor(context, R.color.white));
                holder.episodes_name.setTextColor(ContextCompat.getColor(context, R.color.white));
            }
        } else {
            holder.episodes_num.setTextColor(ContextCompat.getColor(context, R.color.white));
            holder.episodes_name.setTextColor(ContextCompat.getColor(context, R.color.white));
        }
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        RoundedImageView iv_episodes_logo;
        LinearLayout ll_episodes_list;
        TextView episodes_num;
        TextView episodes_name;

        public ViewHolder(View view) {
            super(view);
            iv_episodes_logo = view.findViewById(R.id.iv_episodes_logo);
            ll_episodes_list = view.findViewById(R.id.ll_episodes_list);
            episodes_num = view.findViewById(R.id.episodes_num);
            episodes_name = view.findViewById(R.id.tv_episodes_name);
        }
    }

    @SuppressLint("NotifyDataSetChanged")
    public void select(int position) {
        rowIndex = position;
        notifyDataSetChanged();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemEpisodes itemEpisodes, int position);
    }
}
