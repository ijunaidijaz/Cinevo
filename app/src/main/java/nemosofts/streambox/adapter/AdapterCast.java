package mycinevo.streambox.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.nemosofts.view.RoundedImageView;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import java.util.List;

import mycinevo.streambox.R;
import mycinevo.streambox.item.ItemCast;

public class AdapterCast extends RecyclerView.Adapter<AdapterCast.MyViewHolder> {

    private final List<ItemCast> arrayList;
    private final RecyclerItemClickListener listener;

    public AdapterCast(List<ItemCast> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
    }

    static class MyViewHolder extends RecyclerView.ViewHolder {

        private final LinearLayout ll_cast;
        private final RoundedImageView iv_cast;
        private final TextView tv_cast;

        MyViewHolder(View view) {
            super(view);
            ll_cast = view.findViewById(R.id.ll_cast);
            iv_cast = view.findViewById(R.id.iv_cast);
            tv_cast = view.findViewById(R.id.tv_cast);
        }
    }

    @NonNull
    @Override
    public MyViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_cast, parent, false);
        return new MyViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(@NonNull MyViewHolder holder, int position) {

        holder.tv_cast.setText(arrayList.get(position).getName());
        Picasso.get()
                .load("https://image.tmdb.org/t/p/w300"+arrayList.get(position).getProfile())
                .placeholder(R.drawable.ic_credits)
                .into(holder.iv_cast);

        holder.ll_cast.setOnClickListener(v -> listener.onClickListener(arrayList.get(holder.getAbsoluteAdapterPosition()), holder.getAbsoluteAdapterPosition()));
    }

    @Override
    public long getItemId(int id) {
        return id;
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemCast itemCast, int position);
    }
}