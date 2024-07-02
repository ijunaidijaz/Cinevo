package mycinevo.streambox.adapter;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Filter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import mycinevo.streambox.R;
import mycinevo.streambox.item.ItemCat;
import mycinevo.streambox.util.ApplicationUtil;

public class AdapterCategory extends RecyclerView.Adapter<AdapterCategory.ViewHolder> {

    private final Context context;
    private List<ItemCat> arrayList;
    private final List<ItemCat> filteredArrayList;
    private final RecyclerItemClickListener listener;
    private int row_index = 0;
    private NameFilter filter;
    private final Boolean isTvBox;

    public static class ViewHolder extends RecyclerView.ViewHolder{

        private final TextView tv_cat;
        private final View vw_cat;

        public ViewHolder(View itemView) {
            super(itemView);
            tv_cat = itemView.findViewById(R.id.tv_cat);
            vw_cat = itemView.findViewById(R.id.vw_cat);
        }
    }

    public AdapterCategory(Context context,List<ItemCat> arrayList, RecyclerItemClickListener listener) {
        this.context = context;
        this.arrayList = arrayList;
        this.filteredArrayList = arrayList;
        this.listener = listener;
        isTvBox  = ApplicationUtil.isTvBox(context);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.item_category,parent, false);
        return new ViewHolder(v);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        ItemCat currentItem = arrayList.get(position);

        holder.tv_cat.setText(currentItem.getName());
        holder.tv_cat.setOnClickListener(v -> listener.onClickListener(getPosition(currentItem.getId())));

        if (row_index > -1) {
            if (row_index == position) {
                if (Boolean.TRUE.equals(isTvBox)){
                    holder.tv_cat.requestFocus();
                }
                holder.tv_cat.setTextColor(ContextCompat.getColor(context, R.color.color_select));
                holder.vw_cat.setVisibility(View.VISIBLE);
            } else {
                holder.tv_cat.setTextColor(ContextCompat.getColor(context, R.color.white));
                holder.vw_cat.setVisibility(View.GONE);
            }
        } else {
            holder.tv_cat.setTextColor(ContextCompat.getColor(context, R.color.white));
            holder.vw_cat.setVisibility(View.GONE);
        }

    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(int position);
    }

    @SuppressLint("NotifyDataSetChanged")
    public void select(int position) {
        row_index = position;
        notifyDataSetChanged();
    }

    private int getPosition(String id) {
        int count = 0;
        for (int i = 0; i < filteredArrayList.size(); i++) {
            if (id.equals(filteredArrayList.get(i).getId())) {
                count = i;
                break;
            }
        }
        return count;
    }

    public Filter getFilter() {
        if (filter == null) {
            filter = new NameFilter();
        }
        return filter;
    }


    private class NameFilter extends Filter {

        @NonNull
        @Override
        protected FilterResults performFiltering(CharSequence constraint) {
            constraint = constraint.toString().toLowerCase();
            FilterResults result = new FilterResults();
            if (!constraint.toString().isEmpty()) {
                ArrayList<ItemCat> filteredItems = new ArrayList<>();
                for (int i = 0, l = filteredArrayList.size(); i < l; i++) {
                    String nameList = filteredArrayList.get(i).getName();
                    if (nameList.toLowerCase().contains(constraint))
                        filteredItems.add(filteredArrayList.get(i));
                }
                result.count = filteredItems.size();
                result.values = filteredItems;
            } else {
                synchronized (this) {
                    result.values = filteredArrayList;
                    result.count = filteredArrayList.size();
                }
            }
            return result;
        }

        @SuppressLint("NotifyDataSetChanged")
        @Override
        protected void publishResults(CharSequence constraint, @NonNull FilterResults results) {
            @SuppressWarnings("unchecked")
            ArrayList<ItemCat> filteredItems = (ArrayList<ItemCat>) results.values;
            arrayList = filteredItems;
            notifyDataSetChanged();
        }
    }
}