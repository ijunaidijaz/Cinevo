package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;
import android.widget.FrameLayout;
import android.widget.ProgressBar;

import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.Objects;

import nemosofts.streambox.R;
import nemosofts.streambox.activity.player.PlayerLiveActivity;
import nemosofts.streambox.activity.player.PlayerSingleURLActivity;
import nemosofts.streambox.adapter.AdapterLiveTV;
import nemosofts.streambox.adapter.AdapterMovie;
import nemosofts.streambox.adapter.AdapterSeries;
import nemosofts.streambox.asyncTask.GetLiveSearch;
import nemosofts.streambox.asyncTask.GetMovieSearch;
import nemosofts.streambox.asyncTask.GetSeriesSearch;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.Toasty;
import nemosofts.streambox.interfaces.GetLiveListener;
import nemosofts.streambox.interfaces.GetMovieListener;
import nemosofts.streambox.interfaces.GetSeriesListener;
import nemosofts.streambox.item.ItemLive;
import nemosofts.streambox.item.ItemMovies;
import nemosofts.streambox.item.ItemSeries;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.SPHelper;

public class SearchActivity extends AppCompatActivity {

    private EditText edt_search;
    private FrameLayout frameLayout;
    private RecyclerView rv;
    private ProgressBar pb;
    private String page;

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

        page = getIntent().getStringExtra("page");

        pb = findViewById(R.id.pb);
        frameLayout = findViewById(R.id.fl_empty);
        rv = findViewById(R.id.rv);
        GridLayoutManager grid = new GridLayoutManager(this, 1);
        grid.setSpanCount(6);
        rv.setLayoutManager(grid);
        rv.setItemAnimator(new DefaultItemAnimator());
        rv.setHasFixedSize(true);

        edt_search = findViewById(R.id.edt_search);
        edt_search.setOnEditorActionListener((v, actionId, event) -> {
            if (actionId == EditorInfo.IME_ACTION_SEARCH){
                InputMethodManager inputManager = (InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE);
                inputManager.hideSoftInputFromWindow(Objects.requireNonNull(this.getCurrentFocus()).getWindowToken(),InputMethodManager.HIDE_NOT_ALWAYS);
                searchData();
            }
            return true;
        });
        if (ApplicationUtil.isTvBox(this)){
            edt_search.requestFocus();
        }

        findViewById(R.id.iv_search).setOnClickListener(view -> searchData());
    }

    private void searchData() {
        switch (page) {
            case "Movie" -> searchMovie(false);
            case "MoviePlaylist" -> searchMovie(true);
            case "Live" -> searchLive(false);
            case "LivePlaylist" -> searchLive(true);
            case "Series" -> searchSeries();
            default -> {
            }
        }
    }

    private void searchSeries() {
        new GetSeriesSearch(this, edt_search.getText().toString(), new GetSeriesListener() {
            @Override
            public void onStart() {
                pb.setVisibility(View.VISIBLE);
                frameLayout.setVisibility(View.GONE);
                rv.setVisibility(View.GONE);
            }

            @Override
            public void onEnd(String success, ArrayList<ItemSeries> arrayListSeries) {
                pb.setVisibility(View.GONE);
                if (!isFinishing()){
                    if (success.equals("1")) {
                        if (arrayListSeries.isEmpty()) {
                            setEmpty(true);
                            Toasty.makeText(SearchActivity.this, getString(R.string.err_no_data_found), Toasty.ERROR);
                        } else {
                            setAdapterToSeries(arrayListSeries);
                        }
                    } else {
                        Toasty.makeText(SearchActivity.this, getString(R.string.err_server_not_connected), Toasty.ERROR);
                        setEmpty(true);
                    }
                }
            }
        }).execute();
    }

    private void searchMovie(boolean isPlaylist) {
        new GetMovieSearch(this, isPlaylist, edt_search.getText().toString(), new GetMovieListener() {
            @Override
            public void onStart() {
                pb.setVisibility(View.VISIBLE);
                frameLayout.setVisibility(View.GONE);
                rv.setVisibility(View.GONE);
            }

            @Override
            public void onEnd(String success, ArrayList<ItemMovies> arrayListMovies) {
                pb.setVisibility(View.GONE);
                if (!isFinishing()){
                    if (success.equals("1")) {
                        if (arrayListMovies.isEmpty()) {
                            setEmpty(true);
                            Toasty.makeText(SearchActivity.this, getString(R.string.err_no_data_found), Toasty.ERROR);
                        } else {
                            setAdapterToMovies(arrayListMovies);
                        }
                    } else {
                        Toasty.makeText(SearchActivity.this, getString(R.string.err_server_not_connected), Toasty.ERROR);
                        setEmpty(true);
                    }
                }
            }
        }).execute();
    }

    private void searchLive(boolean isPlaylist) {
        new GetLiveSearch(this, isPlaylist, edt_search.getText().toString(), new GetLiveListener() {
            @Override
            public void onStart() {
                pb.setVisibility(View.VISIBLE);
                frameLayout.setVisibility(View.GONE);
                rv.setVisibility(View.GONE);
            }

            @Override
            public void onEnd(String success, ArrayList<ItemLive> arrayListLive) {
                pb.setVisibility(View.GONE);
                if (!isFinishing()){
                    if (success.equals("1")) {
                        if (arrayListLive.isEmpty()) {
                            setEmpty(true);
                            Toasty.makeText(SearchActivity.this, getString(R.string.err_no_data_found), Toasty.ERROR);
                        } else {
                            setAdapterToLive(arrayListLive);
                        }
                    } else {
                        Toasty.makeText(SearchActivity.this, getString(R.string.err_server_not_connected), Toasty.ERROR);
                        setEmpty(true);
                    }
                }
            }
        }).execute();
    }

    @OptIn(markerClass = UnstableApi.class)
    private void setAdapterToMovies(ArrayList<ItemMovies> arrayListMovies) {
        AdapterMovie adapter = new AdapterMovie(this, arrayListMovies, (itemCat, position) -> {
            Intent intent;
            if (new SPHelper(this).getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
                intent = new Intent(SearchActivity.this, PlayerSingleURLActivity.class);
                intent.putExtra("channel_title", arrayListMovies.get(0).getName());
                intent.putExtra("channel_url", arrayListMovies.get(0).getStreamID());
                startActivity(intent);
            } else {
                intent = new Intent(this, DetailsMovieActivity.class);
                intent.putExtra("stream_id", arrayListMovies.get(position).getStreamID());
                intent.putExtra("stream_name", arrayListMovies.get(position).getName());
                intent.putExtra("stream_icon", arrayListMovies.get(position).getStreamIcon());
                intent.putExtra("stream_rating", arrayListMovies.get(position).getRating());
                startActivity(intent);
            }
        });
        rv.setAdapter(adapter);
        setEmpty(false);
    }

    @OptIn(markerClass = UnstableApi.class)
    private void setAdapterToLive(ArrayList<ItemLive> arrayListLive) {
        AdapterLiveTV adapter = new AdapterLiveTV(this, arrayListLive, (itemCat, position) -> {
            Intent intent = new Intent(SearchActivity.this, PlayerLiveActivity.class);
            Callback.playPosLive = position;
            if (!Callback.arrayListLive.isEmpty()) {
                Callback.arrayListLive.clear();
            }
            Callback.arrayListLive.addAll(arrayListLive);
            startActivity(intent);
        });
        rv.setAdapter(adapter);
        setEmpty(false);
    }

    private void setAdapterToSeries(ArrayList<ItemSeries> arrayListSeries) {
        AdapterSeries adapter = new AdapterSeries(this, arrayListSeries, (itemCat, position) -> {
            Intent intent = new Intent(this, DetailsSeriesActivity.class);
            intent.putExtra("series_id", arrayListSeries.get(position).getSeriesID());
            intent.putExtra("series_name", arrayListSeries.get(position).getName());
            intent.putExtra("series_rating", arrayListSeries.get(position).getRating());
            intent.putExtra("series_cover", arrayListSeries.get(position).getCover());
            startActivity(intent);
        });
        rv.setAdapter(adapter);
        setEmpty(false);
    }

    private void setEmpty(Boolean isEmpty) {
        if (Boolean.FALSE.equals(isEmpty)) {
            rv.setVisibility(View.VISIBLE);
            frameLayout.setVisibility(View.GONE);
        } else {
            rv.setVisibility(View.GONE);
            frameLayout.setVisibility(View.VISIBLE);

            frameLayout.removeAllViews();

            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.row_empty, null);

            myView.findViewById(R.id.tv_empty_msg_sub).setVisibility(View.GONE);

            frameLayout.addView(myView);
        }
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_search;
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN) {
            if (keyCode == KeyEvent.KEYCODE_BACK){
                onBackPressed();
                return true;
            } else if (keyCode == KeyEvent.KEYCODE_HOME){
                ApplicationUtil.openHomeActivity(this);
                return true;
            }
        }
        return super.onKeyDown(keyCode, event);
    }
}