package mycinevo.streambox.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import mycinevo.streambox.R;
import mycinevo.streambox.item.ItemCat;

public class AdapterCat extends RecyclerView.Adapter<AdapterCat.ViewHolder> {

    private final List<ItemCat> arrayList;
    private final RecyclerItemClickListener listener;

    public static class ViewHolder extends RecyclerView.ViewHolder{

        private final TextView tvCategory;
        private final RelativeLayout rlCategory;

        public ViewHolder(View itemView) {
            super(itemView);
            tvCategory = itemView.findViewById(R.id.tv_cat);
            rlCategory = itemView.findViewById(R.id.rl_cat);
        }
    }

    public AdapterCat(List<ItemCat> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.item_cat,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        holder.tvCategory.setText(arrayList.get(position).getName());
        holder.rlCategory.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()), holder.getAbsoluteAdapterPosition()));
        holder.rlCategory.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()), holder.getAbsoluteAdapterPosition()));
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemCat item, int position);
    }
}
