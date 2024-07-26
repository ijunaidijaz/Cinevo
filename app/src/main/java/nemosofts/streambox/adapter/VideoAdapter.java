package nemosofts.streambox.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.nemosofts.view.RoundedImageView;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemVideo;

public class VideoAdapter extends RecyclerView.Adapter<VideoAdapter.VideoViewHolder> {

    private final RecyclerItemClickListener listener;
    private final ArrayList<ItemVideo> itemVideos;

    public VideoAdapter(ArrayList<ItemVideo> itemVideos, RecyclerItemClickListener listener) {
        this.itemVideos = itemVideos;
        this.listener = listener;
    }

    @NonNull
    @Override
    public VideoViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_url_list,parent, false);
        return new VideoViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull VideoViewHolder holder, int position) {
        ItemVideo itemVideo = itemVideos.get(position);

        if (itemVideo.getPath().contains("/storage/emulated/0/")){
            holder.tv_video_url.setText(itemVideo.getTitle().replace("/storage/emulated/0/",""));
        } else {
            holder.tv_video_url.setText(itemVideo.getPath());
        }

        holder.tv_any_name.setText(itemVideo.getTitle());

        int step = 1;
        for (int i = 1; i < position + 1; i++) {
            step++;
            if (step > 7) {
                step = 1;
            }
        }

        switch (step) {
            case 2 -> holder.iv_color_bg.setImageResource(R.color.color_setting_2);
            case 3 -> holder.iv_color_bg.setImageResource(R.color.color_setting_3);
            case 4 -> holder.iv_color_bg.setImageResource(R.color.color_setting_4);
            case 5 -> holder.iv_color_bg.setImageResource(R.color.color_setting_5);
            case 6 -> holder.iv_color_bg.setImageResource(R.color.color_setting_6);
            case 7 -> holder.iv_color_bg.setImageResource(R.color.color_setting_7);
            default -> holder.iv_color_bg.setImageResource(R.color.color_setting_1);
        }

        holder.ll_single_list.setOnClickListener(v -> listener.onClickListener(itemVideo, holder.getAbsoluteAdapterPosition()));
    }

    @Override
    public int getItemCount() {
        return itemVideos.size();
    }

    public static class VideoViewHolder extends RecyclerView.ViewHolder {

        private final LinearLayout ll_single_list;
        private final TextView tv_any_name;
        private final TextView tv_video_url;
        private final RoundedImageView iv_color_bg;

        public VideoViewHolder(@NonNull View itemView) {
            super(itemView);
            ll_single_list = itemView.findViewById(R.id.ll_single_list);
            tv_any_name = itemView.findViewById(R.id.tv_any_name);
            tv_video_url = itemView.findViewById(R.id.tv_video_url);
            iv_color_bg = itemView.findViewById(R.id.iv_color_bg);
        }
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemVideo itemVideo, int position);
    }
}