package nemosofts.streambox.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.nemosofts.view.RoundedImageView;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.item.ItemSingleURL;
import nemosofts.streambox.util.helper.DBHelper;

public class AdapterSingleURL extends RecyclerView.Adapter<AdapterSingleURL.ViewHolder> {

    private final Context ctx;
    private final List<ItemSingleURL> arrayList;
    private final RecyclerItemClickListener listener;
    private final DBHelper dbHelper;

    public AdapterSingleURL(Context ctx, List<ItemSingleURL> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
        this.ctx = ctx;
        dbHelper = new DBHelper(ctx);
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        private final LinearLayout ll_single_list;
        private final TextView tv_any_name;
        private final TextView tv_video_url;
        private final RoundedImageView iv_color_bg;

        public ViewHolder(View itemView) {
            super(itemView);
            ll_single_list = itemView.findViewById(R.id.ll_single_list);
            tv_any_name = itemView.findViewById(R.id.tv_any_name);
            tv_video_url = itemView.findViewById(R.id.tv_video_url);
            iv_color_bg = itemView.findViewById(R.id.iv_color_bg);
        }
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_url_list,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        String anyName =   arrayList.get(position).getAnyName();
        String usersUrl =  ctx.getString(R.string.user_list_url)+" " + arrayList.get(position).getSingleURL();
        holder.tv_any_name.setText(anyName);
        holder.tv_video_url.setText(usersUrl);
        holder.ll_single_list.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()), position));

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

        holder.ll_single_list.setOnLongClickListener(v -> {
            DialogUtil.DeleteDialog(ctx, () -> {
                try {
                    dbHelper.removeFromSingleURL(arrayList.get(holder.getAbsoluteAdapterPosition()).getId());
                    arrayList.remove(holder.getAbsoluteAdapterPosition());
                    notifyItemRemoved(holder.getAbsoluteAdapterPosition());
                    Toast.makeText(ctx, ctx.getString(R.string.delete), Toast.LENGTH_SHORT).show();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            });
            return false;
        });
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemSingleURL itemSingleURL, int position);
    }
}
