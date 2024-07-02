package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;
import android.widget.FrameLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.Objects;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterCategory;
import nemosofts.streambox.adapter.AdapterMovie;
import nemosofts.streambox.asyncTask.GetCategory;
import nemosofts.streambox.asyncTask.GetMoviesPlaylist;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.ChildCountDialog;
import nemosofts.streambox.dialog.FilterDialog;
import nemosofts.streambox.interfaces.GetCategoryListener;
import nemosofts.streambox.interfaces.GetMovieListener;
import nemosofts.streambox.item.ItemCat;
import nemosofts.streambox.item.ItemMovies;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.Helper;
import nemosofts.streambox.util.recycler.EndlessRecyclerViewScrollListener;
import nemosofts.streambox.view.NSoftsProgressDialog;

public class PlaylistMovieActivity extends AppCompatActivity {

    private Helper helper;
    private NSoftsProgressDialog progressDialog;
    // Category
    private AdapterCategory adapter_category;
    private RecyclerView rv_cat;
    private ArrayList<ItemCat> arrayListCat;
    // Movies
    private FrameLayout frameLayout;
    private Boolean isOver = false, isScroll = false, isLoading = false;
    private int page = 1;
    private String cat_name = "";
    private AdapterMovie adapter;
    private ArrayList<ItemMovies> arrayList;
    private RecyclerView rv;
    private ProgressBar pb;
    private GetMoviesPlaylist loadMovies;
    private int pos = 0;

    @OptIn(markerClass = UnstableApi.class)
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (Boolean.TRUE.equals(Callback.isLandscape)){
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        }
        IfSupported.IsRTL(this);
        IfSupported.IsScreenshot(this);
        IfSupported.hideStatusBar(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));
        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (ApplicationUtil.isTvBox(this)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        TextView page_title = findViewById(R.id.tv_page_title);
        page_title.setText(getString(R.string.movies_home));

        // Initialize UI components
        pb = findViewById(R.id.pb);
        frameLayout = findViewById(R.id.fl_empty);
        rv = findViewById(R.id.rv);
        rv_cat = findViewById(R.id.rv_cat);

        // Initialize helpers and dialogs
        progressDialog = new NSoftsProgressDialog(PlaylistMovieActivity.this);
        helper = new Helper(this, (position, type) -> {
            Intent intent = new Intent(PlaylistMovieActivity.this, PlayerSingleURLActivity.class);
            intent.putExtra("channel_title", arrayList.get(0).getName());
            intent.putExtra("channel_url", arrayList.get(0).getStreamID());
            startActivity(intent);
        });

        // Initialize RecyclerViews
        initRecyclerViews();

        // Initialize listeners
        initListeners();

        // Initialize data lists
        arrayList = new ArrayList<>();
        arrayListCat = new ArrayList<>();

        // Fetch initial data
        new Handler().postDelayed(this::getDataCat, 0);
    }

    private void initRecyclerViews() {
        GridLayoutManager grid = new GridLayoutManager(this, 1);
        grid.setSpanCount(ApplicationUtil.isTvBox(this) ? 6 : 5);
        rv.setLayoutManager(grid);
        rv.setItemAnimator(new DefaultItemAnimator());
        rv.setHasFixedSize(true);
        rv.addOnScrollListener(new EndlessRecyclerViewScrollListener(grid) {
            @Override
            public void onLoadMore(int p, int totalItemsCount) {
                if ((Boolean.FALSE.equals(isOver) && (Boolean.FALSE.equals(isLoading)))) {
                    isLoading = true;
                    new Handler().postDelayed(() -> {
                        isScroll = true;
                        getData();
                    }, 0);
                }
            }
        });

        LinearLayoutManager llm = new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
        rv_cat.setLayoutManager(llm);
        rv_cat.setItemAnimator(new DefaultItemAnimator());
        rv_cat.setHasFixedSize(true);
    }

    private void initListeners() {
        findViewById(R.id.iv_filter).setOnClickListener(v -> new FilterDialog(this, 2, () -> new Handler().postDelayed(() -> recreate_data(pos), 0)));
        findViewById(R.id.iv_search).setOnClickListener(view -> {
            Intent intent = new Intent(PlaylistMovieActivity.this, SearchActivity.class);
            intent.putExtra("page", "MoviePlaylist");
            startActivity(intent);
        });
    }

    private void getDataCat() {
        GetCategory category = new GetCategory(this, 5, new GetCategoryListener() {
            @Override
            public void onStart() {
                progressDialog.show();
            }

            @Override
            public void onEnd(boolean success, ArrayList<ItemCat> itemCat) {
                progressDialog.dismiss();
                if (success && !itemCat.isEmpty()) {
                    if (!arrayListCat.isEmpty()){
                        arrayListCat.clear();
                    }
                    arrayListCat.addAll(itemCat);
                    cat_name = itemCat.get(0).getName();
                    setAdapterToCatListview();
                } else {
                    setEmpty();
                }
            }
        });
        category.execute();
    }

    private void getData() {
        loadMovies = new GetMoviesPlaylist(this, page, cat_name, new GetMovieListener() {
            @Override
            public void onStart() {
                if (arrayList.isEmpty()){
                    pb.setVisibility(View.VISIBLE);
                    frameLayout.setVisibility(View.GONE);
                }
            }

            @Override
            public void onEnd(String success, ArrayList<ItemMovies> arrayListMovies) {
                if (!isFinishing()){
                    if (Boolean.FALSE.equals(isOver)){
                        pb.setVisibility(View.GONE);
                        if (success.equals("1")) {
                            if (arrayListMovies.isEmpty()) {
                                isOver = true;
                                setEmpty();
                            } else {
                                page = page + 1;
                                arrayList.addAll(arrayListMovies);
                                setAdapterToListview();
                            }
                        } else {
                            setEmpty();
                        }
                        isLoading = false;
                    }
                }
            }
        });
        loadMovies.execute();
    }

    public void setAdapterToCatListview() {
        adapter_category = new AdapterCategory(this, arrayListCat, position -> new Handler().postDelayed(() -> recreate_data(position), 0));
        rv_cat.setAdapter(adapter_category);
        adapter_category.select(0);
        cat_name = arrayListCat.get(0).getName();
        pos = 0;

        // Handle adult content verification dialog or immediate data fetch
        handleAdultContentVerification();

        // Set up search functionality
        setupSearchFunctionality();
    }

    private void setupSearchFunctionality() {
        EditText edt_search = findViewById(R.id.edt_search);
        edt_search.setOnEditorActionListener((v, actionId, event) -> {
            if (actionId == EditorInfo.IME_ACTION_SEARCH) {
                InputMethodManager inputManager = (InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE);
                inputManager.hideSoftInputFromWindow(Objects.requireNonNull(this.getCurrentFocus()).getWindowToken(), InputMethodManager.HIDE_NOT_ALWAYS);
            }
            return true;
        });
        edt_search.addTextChangedListener(searchWatcher);
    }

    TextWatcher searchWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {
            // Not used, can be left empty
        }

        @SuppressLint("NotifyDataSetChanged")
        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            if (adapter_category != null) {
                adapter_category.getFilter().filter(s.toString());
                adapter_category.notifyDataSetChanged();
            }
        }

        @Override
        public void afterTextChanged(Editable s) {
            // Not used, can be left empty
        }
    };

    @SuppressLint("NotifyDataSetChanged")
    private void recreate_data(int position) {
        if (position >= 0 && position < arrayListCat.size()) {
            pos = position;
            cat_name = arrayListCat.get(position).getName();
            adapter_category.select(position);

            // Cancel any ongoing task
            if (loadMovies != null) {
                loadMovies.cancel(true);
            }
            isOver = true;

            // Clear the list
            if (!arrayList.isEmpty()){
                arrayList.clear();
            }

            // Notify adapter of data change
            if (adapter != null){
                adapter.notifyDataSetChanged();
            }

            // Reset pagination and fetch new data
            new Handler().postDelayed(this::resetPaginationAndFetchData, 0);
        }
    }

    private void resetPaginationAndFetchData() {
        isOver = false;
        isScroll = false;
        isLoading = false;
        page = 1;

        // Handle adult content verification dialog or immediate data fetch
        handleAdultContentVerification();
    }

    private void handleAdultContentVerification() {
        if (ApplicationUtil.geIsAdultsCount(arrayListCat.get(pos).getName())) {
            new ChildCountDialog(this, pos, position -> getData());
        } else {
            // Delayed data fetch if not adult content
            new Handler().postDelayed(this::getData, 0);
        }
    }

    public void setAdapterToListview() {
        if(Boolean.FALSE.equals(isScroll)) {
            adapter = new AdapterMovie(this, arrayList, (itemCat, position) -> helper.showInterAd(position,""));
            rv.setAdapter(adapter);
            setEmpty();
        } else {
            adapter.notifyItemInserted(arrayList.size()-1);
        }
    }

    private void setEmpty() {
        if (!arrayList.isEmpty()) {
            rv.setVisibility(View.VISIBLE);
            frameLayout.setVisibility(View.GONE);
        } else {
            rv.setVisibility(View.GONE);
            frameLayout.setVisibility(View.VISIBLE);

            frameLayout.removeAllViews();

            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.row_empty, null);

            frameLayout.addView(myView);
        }
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_live_tv;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN) {
            if (keyCode == KeyEvent.KEYCODE_BACK){
                finish();
                return true;
            } else if (keyCode == KeyEvent.KEYCODE_HOME){
                ApplicationUtil.openHomeActivity(this);
                return true;
            }
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public void onDestroy() {
        if (progressDialog != null && progressDialog.isShowing()){
            progressDialog.cancel();
        }
        super.onDestroy();
    }
}