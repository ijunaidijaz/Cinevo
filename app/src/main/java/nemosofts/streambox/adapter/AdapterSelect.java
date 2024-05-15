package nemosofts.streambox.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemSelect;

public class AdapterSelect extends RecyclerView.Adapter<AdapterSelect.ViewHolder> {

    private final List<ItemSelect> arrayList;
    private final RecyclerItemClickListener listener;

    public static class ViewHolder extends RecyclerView.ViewHolder{

        private final TextView iv_title;
        private final RelativeLayout rl_login_list;
        private final ImageView iv_logo;
        private final ImageView iv_more;

        public ViewHolder(View itemView) {
            super(itemView);
            iv_title = itemView.findViewById(R.id.iv_list_title);
            rl_login_list = itemView.findViewById(R.id.rl_login_list);
            iv_logo = itemView.findViewById(R.id.iv_list_logo);
            iv_more = itemView.findViewById(R.id.iv_more);
        }
    }

    public AdapterSelect(List<ItemSelect> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_login_list,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {

        holder.iv_title.setText(arrayList.get(position).getTitle());

        holder.iv_more.setVisibility(Boolean.TRUE.equals(arrayList.get(position).getIsMore()) ? View.VISIBLE : View.GONE);

        try {
            holder.iv_logo.setImageResource(arrayList.get(position).getLogoResId());
        } catch (Exception e) {
            e.printStackTrace();
        }

        holder.rl_login_list.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()), holder.getAbsoluteAdapterPosition()));
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemSelect item, int position);
    }
}
