package nemosofts.streambox.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.RatingBar;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Callback;
import com.squareup.picasso.Picasso;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemEpisodes;
import nemosofts.streambox.util.ApplicationUtil;

public class AdapterEpisodes extends RecyclerView.Adapter<AdapterEpisodes.MyViewHolder> {

    private final List<ItemEpisodes> arrayList;
    private final RecyclerItemClickListener listener;
    private final String seriesCover;

    public static class MyViewHolder extends RecyclerView.ViewHolder {

        private final ImageView iv_episodes;
        private final TextView tv_episodes;
        private final TextView tv_duration;
        private final TextView tv_plot;
        private final RatingBar rb_episodes;
        private final RelativeLayout rl_episodes;

        private MyViewHolder(View view) {
            super(view);
            iv_episodes = view.findViewById(R.id.iv_episodes);
            tv_episodes = view.findViewById(R.id.tv_episodes);
            rb_episodes = view.findViewById(R.id.rb_episodes_list);
            tv_duration = view.findViewById(R.id.tv_duration);
            tv_plot = view.findViewById(R.id.tv_plot);
            rl_episodes = view.findViewById(R.id.rl_episodes);
        }
    }

    public AdapterEpisodes(List<ItemEpisodes> arrayList, String cover ,RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
        this.seriesCover = cover;
    }

    @NonNull
    @Override
    public MyViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_episodes_list, parent, false);
        return new MyViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(@NonNull MyViewHolder holder, int position) {
        try {
            Picasso.get()
                    .load(arrayList.get(position).getCoverBig().isEmpty() ? "null" : arrayList.get(position).getCoverBig())
                    .resize(450, 300)
                    .centerCrop()
                    .placeholder(R.color.bg_color_load)
                    .into(holder.iv_episodes, new Callback() {
                        @Override
                        public void onSuccess() {

                        }
                        @Override
                        public void onError(Exception e) {
                            try {
                                Picasso.get()
                                        .load(seriesCover)
                                        .resize(450, 300)
                                        .centerCrop()
                                        .placeholder(R.color.bg_color_load)
                                        .into(holder.iv_episodes);
                            } catch (Exception ex) {
                                e.printStackTrace();
                            }
                        }
                    });
        } catch (Exception e) {
            e.printStackTrace();
        }

        holder.tv_episodes.setText(arrayList.get(position).getTitle());

        try {
            // Adding null check before parsing the rating
            String ratingString = arrayList.get(position).getRating().isEmpty() ? "0" : arrayList.get(position).getRating();
            double newRating = 0.0; // default value
            if (ratingString != null && !ratingString.isEmpty()&& !ratingString.equals("null")) {
                newRating = convertToFiveRating(Double.parseDouble(ratingString));
            }
            holder.rb_episodes.setRating((float) newRating);
        } catch (Exception e) {
            holder.rb_episodes.setRating(0);
        }

        try {
            holder.tv_duration.setText(ApplicationUtil.formatTimeToTime(arrayList.get(position).getDuration()));
        } catch (Exception e) {
            holder.tv_duration.setText("0");
        }

        holder.tv_plot.setText(arrayList.get(position).getPlot());
        holder.rl_episodes.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()), holder.getAbsoluteAdapterPosition()));
    }

    public static double convertToFiveRating(double oldRating) {
        return (oldRating - 1) * 4 / 9 + 1;
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
        void onClickListener(ItemEpisodes itemEpisodes, int position);
    }
}