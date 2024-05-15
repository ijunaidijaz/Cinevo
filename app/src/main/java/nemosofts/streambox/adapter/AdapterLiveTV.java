package nemosofts.streambox.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.nemosofts.view.RoundedImageView;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemLive;
import nemosofts.streambox.util.ApplicationUtil;

public class AdapterLiveTV extends RecyclerView.Adapter<AdapterLiveTV.MyViewHolder> {

    private final List<ItemLive> arrayList;
    private final RecyclerItemClickListener listener;
    private final Boolean isTvBox;
    private final int columnWidth;

    public static class MyViewHolder extends RecyclerView.ViewHolder {

        private final View fd_movie_card;
        private final RoundedImageView iv_movie;
        private final TextView tv_movie_title;
        private final LinearLayout ll_card_star;

        public MyViewHolder(View view) {
            super(view);
            ll_card_star = view.findViewById(R.id.ll_card_star);
            fd_movie_card = view.findViewById(R.id.fd_movie_card);
            iv_movie = view.findViewById(R.id.iv_movie);
            tv_movie_title = view.findViewById(R.id.tv_movie_title);
        }
    }

    public AdapterLiveTV(Context context, List<ItemLive> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
        isTvBox  = ApplicationUtil.isTvBox(context);
        columnWidth = ApplicationUtil.getColumnWidth(context, Boolean.TRUE.equals(isTvBox)? 8 : 7, 0);
    }

    @NonNull
    @Override
    public MyViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View  itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.item_movie, parent, false);
        return new MyViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(@NonNull MyViewHolder holder, int position) {

        holder.tv_movie_title.setText(arrayList.get(position).getName());
        holder.ll_card_star.setVisibility(View.GONE);

        holder.iv_movie.setScaleType(ImageView.ScaleType.CENTER_CROP);
        holder.iv_movie.setLayoutParams(new RelativeLayout.LayoutParams(columnWidth, columnWidth));
        holder.fd_movie_card.setLayoutParams(new RelativeLayout.LayoutParams(columnWidth, columnWidth));

        try {
            Picasso.get()
                    .load(arrayList.get(position).getStreamIcon().isEmpty() ? "null" : arrayList.get(position).getStreamIcon())
                    .resize(Boolean.TRUE.equals(isTvBox) ? columnWidth : 300, Boolean.TRUE.equals(isTvBox) ? columnWidth : 300)
                    .centerCrop()
                    .placeholder(R.color.bg_color_load)
                    .into(holder.iv_movie);
        } catch (Exception e) {
            e.printStackTrace();
        }

        holder.fd_movie_card.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()), holder.getAbsoluteAdapterPosition()));
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
        void onClickListener(ItemLive itemLive, int position);
    }
}
