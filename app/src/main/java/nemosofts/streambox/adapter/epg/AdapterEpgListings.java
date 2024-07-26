package nemosofts.streambox.adapter.epg;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import org.jetbrains.annotations.NotNull;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemEpg;
import nemosofts.streambox.util.ApplicationUtil;

public class AdapterEpgListings extends RecyclerView.Adapter<AdapterEpgListings.ViewHolder> {

    private final List<ItemEpg> arrayList;

    public AdapterEpgListings(List<ItemEpg> arrayList) {
        this.arrayList = arrayList;
    }

    @NotNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.row_epg_listings,parent, false);
        return new ViewHolder(v);
    }

    @SuppressLint("SetTextI18n")
    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        ItemEpg itemEpg = arrayList.get(position);
        holder.tvEpg.setText(ApplicationUtil.decodeBase64(itemEpg.getTitle()));
        holder.tvEpgTime.setText(ApplicationUtil.getTimestamp(itemEpg.getStartTimestamp()) + " - " + ApplicationUtil.getTimestamp(itemEpg.getStopTimestamp()));
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{

        TextView tvEpg;
        TextView tvEpgTime;

        public ViewHolder(View view) {
            super(view);
            tvEpg = view.findViewById(R.id.iv_epg_test);
            tvEpgTime = view.findViewById(R.id.iv_epg_start_time);
        }
    }
}
