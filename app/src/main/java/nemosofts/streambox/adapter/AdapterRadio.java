package nemosofts.streambox.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;
import androidx.nemosofts.view.RoundedImageView;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemLive;
import nemosofts.streambox.util.ApplicationUtil;

public class AdapterRadio extends RecyclerView.Adapter<AdapterRadio.MyViewHolder> {

    private final List<ItemLive> arrayList;
    private final RecyclerItemClickListener listener;
    private final int columnWidth;
    private final Boolean isTvBox;

    public static class MyViewHolder extends RecyclerView.ViewHolder{

        private final View fdRadioCard;
        private final RoundedImageView ivRadioList;

        public MyViewHolder(View view) {
            super(view);
            ivRadioList = view.findViewById(R.id.iv_radio_list);
            fdRadioCard = view.findViewById(R.id.fd_radio_card);
        }
    }

    public AdapterRadio(Context context, List<ItemLive> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
        columnWidth = ApplicationUtil.getColumnWidth(context, ApplicationUtil.isTvBox(context)? 8 : 6, 0);
        isTvBox = ApplicationUtil.isTvBox(context);
    }

    @NonNull
    @Override
    public MyViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View  itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.item_radio, parent, false);
        return new MyViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(@NonNull MyViewHolder holder, int position) {

        holder.ivRadioList.setScaleType(ImageView.ScaleType.CENTER_CROP);
        holder.ivRadioList.setLayoutParams(new RelativeLayout.LayoutParams(columnWidth, columnWidth));
        holder.fdRadioCard.setLayoutParams(new RelativeLayout.LayoutParams(columnWidth, columnWidth));

        try {
            Picasso.get()
                    .load(arrayList.get(position).getStreamIcon().isEmpty() ? "null" : arrayList.get(position).getStreamIcon())
                    .resize(Boolean.TRUE.equals(isTvBox) ? columnWidth : 300, Boolean.TRUE.equals(isTvBox) ? columnWidth : 300)
                    .centerCrop()
                    .noFade()
                    .placeholder(R.color.bg_color_load)
                    .into(holder.ivRadioList);
        } catch (Exception e) {
            e.printStackTrace();
        }

        holder.fdRadioCard.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()), holder.getAbsoluteAdapterPosition()));
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
