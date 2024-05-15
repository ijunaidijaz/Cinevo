package mycinevo.streambox.dialog;

import static android.view.WindowManager.LayoutParams.MATCH_PARENT;
import static android.view.WindowManager.LayoutParams.WRAP_CONTENT;

import android.app.Activity;
import android.app.Dialog;
import android.view.Window;
import android.widget.EditText;
import android.widget.Toast;

import androidx.annotation.NonNull;

import java.util.Objects;

import mycinevo.streambox.R;
import mycinevo.streambox.asyncTask.LoadStatus;
import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.interfaces.StatusListener;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.NetworkUtils;
import mycinevo.streambox.util.helper.SPHelper;
import mycinevo.streambox.util.helper.Helper;
import mycinevo.streambox.view.NSoftsProgressDialog;

public class FeedBackDialog {

    private final Helper helper;
    private final SPHelper spHelper;
    private Dialog dialog;
    private final Activity ctx;
    private final NSoftsProgressDialog progressDialog;

    public FeedBackDialog(@NonNull Activity ctx) {
        this.ctx = ctx;
        helper = new Helper(ctx);
        spHelper = new SPHelper(ctx);
        progressDialog = new NSoftsProgressDialog(ctx);
    }

    public void showDialog(String title) {
        if(spHelper.isLogged()) {
            dialog = new Dialog(ctx);
            dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
            dialog.setContentView(R.layout.dialog_feed_back);
            EditText et_messages = dialog.findViewById(R.id.et_messages);
            dialog.findViewById(R.id.tv_cancel).setOnClickListener(view -> dismissDialog());
            dialog.findViewById(R.id.iv_close).setOnClickListener(view -> dismissDialog());
            dialog.findViewById(R.id.tv_submit).setOnClickListener(view -> {
                if(et_messages.getText().toString().trim().isEmpty()) {
                    et_messages.setError(ctx.getString(R.string.please_describe_the_problem));
                    et_messages.requestFocus();
                } else {
                    loadReportSubmit(et_messages.getText().toString(), title);
                }
            });
            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.DialogAnimation;
            dialog.show();
            Window window = dialog.getWindow();
            IfSupported.hideStatusBarDialog(window);
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    private void loadReportSubmit(String reportMessages, String reportTitle) {
        if (NetworkUtils.isConnected(ctx)){
            LoadStatus loadFav = new LoadStatus(new StatusListener() {
                @Override
                public void onStart() {
                    progressDialog.show();
                }

                @Override
                public void onEnd(String success, String reportSuccess, String message) {
                    if (success.equals("1")) {
                        if (reportSuccess.equals("1")) {
                            Toast.makeText(ctx, message, Toast.LENGTH_SHORT).show();
                        }
                    } else {
                        Toast.makeText(ctx, ctx.getString(R.string.err_server_not_connected), Toast.LENGTH_SHORT).show();
                    }
                    dismissDialog();
                }
            }, helper.getAPIRequestNSofts(Callback.METHOD_REPORT, reportTitle, reportMessages, spHelper.getUserName(), spHelper.getPassword()));
            loadFav.execute();
        } else {
            Toast.makeText(ctx, ctx.getString(R.string.err_internet_not_connected), Toast.LENGTH_SHORT).show();
        }
    }

    private void dismissDialog() {
        if (dialog != null && dialog.isShowing()){
            dialog.dismiss();
        }
        if (progressDialog.isShowing()) {
            progressDialog.dismiss();
        }
    }
}
