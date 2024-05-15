package mycinevo.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.text.TextUtils;
import android.text.method.HideReturnsTransformationMethod;
import android.text.method.PasswordTransformationMethod;
import android.view.KeyEvent;
import android.view.View;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.nemosofts.AppCompat;
import androidx.nemosofts.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import org.jetbrains.annotations.Contract;

import java.util.ArrayList;

import mycinevo.streambox.R;
import mycinevo.streambox.adapter.AdapterDNS;
import mycinevo.streambox.asyncTask.LoadLogin;
import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.dialog.DialogUtil;
import mycinevo.streambox.dialog.Toasty;
import mycinevo.streambox.interfaces.LoginListener;
import mycinevo.streambox.item.ItemDns;
import mycinevo.streambox.item.ItemUsersDB;
import mycinevo.streambox.util.ApplicationUtil;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.NetworkUtils;
import mycinevo.streambox.util.helper.SPHelper;
import mycinevo.streambox.util.helper.DBHelper;
import mycinevo.streambox.util.helper.Helper;
import mycinevo.streambox.view.NSoftsProgressDialog;

public class SignInActivity extends AppCompatActivity {

    private DBHelper dbHelper;
    private Helper helper;
    private SPHelper spHelper;
    private EditText et_any_name;
    private EditText et_user_name;
    private EditText et_login_password;
    private EditText et_url;
    private Boolean isVisibility = false;
    private LinearLayout ll_url;
    private AdapterDNS adapter;
    private Boolean isXui = true;
    private NSoftsProgressDialog progressDialog;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (Boolean.TRUE.equals(Callback.isLandscape)){
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        }
        IfSupported.IsRTL(this);
        IfSupported.IsScreenshot(this);
        IfSupported.hideStatusBar(this);
        IfSupported.keepScreenOn(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ApplicationUtil.openThemeBg(this));

        String from = getIntent().getStringExtra("from");
        if (from != null && from.equals("stream")) {
            isXui = false;
        }

        progressDialog = new NSoftsProgressDialog(SignInActivity.this);

        helper = new Helper(this);
        spHelper = new SPHelper(this);
        dbHelper = new DBHelper(this);

        ll_url  = findViewById(R.id.ll_url);
        et_any_name = findViewById(R.id.et_any_name);
        et_user_name = findViewById(R.id.et_user_name);
        et_login_password = findViewById(R.id.et_login_password);
        et_url = findViewById(R.id.et_url);

        ImageView iv_visibility = findViewById(R.id.iv_visibility);
        iv_visibility.setImageResource(Boolean.TRUE.equals(isVisibility) ? R.drawable.ic_login_visibility : R.drawable.ic_login_visibility_off);
        iv_visibility.setOnClickListener(v -> {
            isVisibility = !isVisibility;
            iv_visibility.setImageResource(Boolean.TRUE.equals(isVisibility) ? R.drawable.ic_login_visibility : R.drawable.ic_login_visibility_off);
            et_login_password.setTransformationMethod(Boolean.TRUE.equals(isVisibility) ? HideReturnsTransformationMethod.getInstance()  : PasswordTransformationMethod.getInstance());
        });

        findViewById(R.id.ll_btn_add).setOnClickListener(v -> attemptLogin());
        findViewById(R.id.rl_list_users).setOnClickListener(view -> {
            Intent intent = new Intent(SignInActivity.this, UsersListActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "");
            startActivity(intent);
            finish();
        });

        RecyclerView rv_dns = findViewById(R.id.rv_dns);
        rv_dns.setHasFixedSize(true);
        LinearLayoutManager llm = new LinearLayoutManager(this, LinearLayoutManager.HORIZONTAL, false);
        rv_dns.setLayoutManager(llm);
        rv_dns.setNestedScrollingEnabled(false);

        ArrayList<ItemDns> arrayList = new ArrayList<>();
        arrayList.add(new ItemDns("External",""));
        try {
            if (Boolean.TRUE.equals(isXui)){
                if (Boolean.TRUE.equals(spHelper.getIsXUI_DNS())) {
                    ArrayList<ItemDns> arrayList2 = new ArrayList<>(dbHelper.loadDNS(DBHelper.TABLE_DNS_XUI));
                    if (!arrayList2.isEmpty()) {
                        arrayList.addAll(arrayList2);
                    }
                } else {
                    rv_dns.setVisibility(View.GONE);
                }
            } else {
                if (Boolean.TRUE.equals(spHelper.getIssStreamDNS())){
                    ArrayList<ItemDns> arrayList2 = new ArrayList<>(dbHelper.loadDNS(DBHelper.TABLE_DNS_STREAM));
                    if (!arrayList2.isEmpty()){
                        arrayList.addAll(arrayList2);
                    }
                } else {
                    rv_dns.setVisibility(View.GONE);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        adapter = new AdapterDNS(this, arrayList, (itemDns, position) -> {
            ll_url.setVisibility(position == 0 ? View.VISIBLE : View.GONE);
            adapter.setSelectedFocus(position);
        });
        rv_dns.setAdapter(adapter);
        adapter.setSelected(0);

        if (ApplicationUtil.isTvBox(this)){
            et_any_name.requestFocus();
        }
    }

    @Override
    public int setLayoutResourceId() {
        return R.layout.activity_sign_in;
    }

    @Override
    public int setAppCompat() {
        return AppCompat.COMPAT();
    }

    @SuppressLint("SetTextI18n")
    private void attemptLogin() {
        et_user_name.setError(null);
        et_login_password.setError(null);
        et_any_name.setError(null);

        if (ll_url.getVisibility() == View.GONE){
            if (adapter.getSelectedBase().isEmpty()){
                et_url.setText("https://nemosofts.com");
            } else {
                et_url.setText(adapter.getSelectedBase());
            }
        }

        // Store values at the time of the login attempt.
        String any_name = et_any_name.getText().toString();
        String user_name = et_user_name.getText().toString();
        String password = et_login_password.getText().toString();
        String url_data = et_url.getText().toString();

        boolean cancel = false;
        View focusView = null;

        // Check for a valid password, if the user entered one.
        if (TextUtils.isEmpty(password)) {
            et_login_password.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_cannot_empty)));
            focusView = et_login_password;
            cancel = true;
        }

        if (!TextUtils.isEmpty(password) && !isPasswordValid(password)) {
            et_login_password.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_password_sort)));
            focusView = et_login_password;
            cancel = true;
        }

        if (et_login_password.getText().toString().endsWith(" ")) {
            et_login_password.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_pass_end_space)));
            focusView = et_login_password;
            cancel = true;
        }

        // Check for a valid user name.
        if (TextUtils.isEmpty(user_name)) {
            et_user_name.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_cannot_empty)));
            focusView = et_user_name;
            cancel = true;
        } else {
            if (TextUtils.isEmpty(any_name)) {
                et_any_name.setText(et_user_name.getText().toString());
            }
        }

        if (TextUtils.isEmpty(url_data)) {
            et_url.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_cannot_empty)));
            focusView = et_url;
            cancel = true;
        }

        boolean flag = false;
        if (!Callback.arrayBlacklist.isEmpty()){
            for (int i = 0; i < Callback.arrayBlacklist.size(); i++) {
                if (et_url.getText().toString().contains(Callback.arrayBlacklist.get(i).getBase())) {
                    flag = true;
                    break;
                }
            }
        }

        if (flag) {
            cancel = true;
            Toasty.makeText(SignInActivity.this, "Blacklist", Toasty.ERROR);
        }

        if (cancel) {
            if (focusView != null) {
                focusView.requestFocus();
            }
        } else {
            loadLogin();
        }
    }

    private void loadLogin() {
        if (NetworkUtils.isConnected(this)){
            LoadLogin login = new LoadLogin(new LoginListener() {
                @Override
                public void onStart() {
                    setEnabled(false);
                    progressDialog.show();
                }

                @Override
                public void onEnd(String success, String username, String password, String message, int auth, String status, String exp_date, String is_trial, String active_cons, String created_at, String max_connections, String allowed_output_formats, boolean xui, String version, int revision, String url, String port, String https_port, String server_protocol, String rtmp_port, int timestamp_now, String time_now, String timezone) {
                    progressDialog.dismiss();
                    if (!isFinishing()){
                        if (success.equals("1")) {
                            try {
                                if (Boolean.TRUE.equals(isXui)){
                                    dbHelper.addToUserDB(new ItemUsersDB("", et_any_name.getText().toString(), et_user_name.getText().toString(),
                                            et_login_password.getText().toString(), et_url.getText().toString(),"xui")
                                    );
                                    spHelper.setLoginDetails(
                                            username,password,message,auth,status, exp_date, is_trial, active_cons,created_at,max_connections,
                                            xui,version,revision,url,port,https_port,server_protocol,rtmp_port,timestamp_now,time_now,timezone
                                    );
                                    spHelper.setLoginType(Callback.TAG_LOGIN_ONE_UI);
                                } else {
                                    dbHelper.addToUserDB(new ItemUsersDB("", et_any_name.getText().toString(), et_user_name.getText().toString(),
                                            et_login_password.getText().toString(), et_url.getText().toString(),"stream")
                                    );
                                    spHelper.setLoginDetails(
                                            username,password,message,auth,status, exp_date, is_trial, active_cons,created_at,max_connections,
                                            xui,version,revision,url,port,https_port,server_protocol,rtmp_port,timestamp_now,time_now,timezone
                                    );
                                    spHelper.setLoginType(Callback.TAG_LOGIN_STREAM);
                                }
                                if (!allowed_output_formats.isEmpty()){
                                    if (allowed_output_formats.contains("m3u8")){
                                        spHelper.setLiveFormat(2);
                                    } else {
                                        spHelper.setLiveFormat(1);
                                    }
                                } else {
                                    spHelper.setLiveFormat(0);
                                }

                                spHelper.setAnyName(et_any_name.getText().toString());
                                spHelper.setIsFirst(false);
                                spHelper.setIsLogged(true);
                                spHelper.setIsAutoLogin(true);

                                Callback.isCustomAds = false;
                                Callback.customAdCount = 0;
                                Callback.customAdShow = 15;
                                Callback.is_load_ads = true;

                                Toast.makeText(SignInActivity.this, "Login successfully.", Toast.LENGTH_SHORT).show();
                            } catch (Exception e) {
                                e.printStackTrace();
                            }
                            ApplicationUtil.openThemeActivity(SignInActivity.this);
                        }  else {
                            setEnabled(true);
                            Toasty.makeText(SignInActivity.this, getString(R.string.err_login_not_incorrect), Toasty.ERROR);
                        }
                    }
                }
            },et_url.getText().toString(), helper.getAPIRequestLogin(et_user_name.getText().toString(), et_login_password.getText().toString()));
            login.execute();
        }  else {
            Toasty.makeText(SignInActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
        }
    }

    private void setEnabled(boolean is_enabled) {
        if (is_enabled){
            findViewById(R.id.iv_add).setVisibility(View.VISIBLE);
            findViewById(R.id.pb_add).setVisibility(View.GONE);
            if (ApplicationUtil.isTvBox(this)){
                findViewById(R.id.ll_btn_add).requestFocus();
            }
        } else {
            findViewById(R.id.iv_add).setVisibility(View.GONE);
            findViewById(R.id.pb_add).setVisibility(View.VISIBLE);
        }
    }

    @Contract(pure = true)
    private boolean isPasswordValid(@NonNull String password) {
        return !password.isEmpty();
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)){
            DialogUtil.ExitDialog(SignInActivity.this);
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }

    @SuppressLint("MissingSuperCall")
    @Override
    public void onBackPressed() {
        DialogUtil.ExitDialog(SignInActivity.this);
    }
}