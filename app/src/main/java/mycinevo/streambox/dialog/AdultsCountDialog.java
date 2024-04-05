package mycinevo.streambox.dialog;

import static android.view.WindowManager.LayoutParams.MATCH_PARENT;
import static android.view.WindowManager.LayoutParams.WRAP_CONTENT;

import android.app.Dialog;
import android.content.Context;
import android.view.View;
import android.view.Window;
import android.widget.EditText;

import java.util.Objects;

import mycinevo.streambox.R;
import mycinevo.streambox.util.IfSupported;
import mycinevo.streambox.util.SharedPref;

public class AdultsCountDialog {

    private final SharedPref sharedPref;
    private final Dialog dialog;

    public AdultsCountDialog(Context context) {
        sharedPref = new SharedPref(context);
        dialog = new Dialog(context);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_adults_count);
        dialog.findViewById(R.id.iv_close_adult).setOnClickListener(view -> dismissDialog());
        dialog.findViewById(R.id.tv_cancel_adult).setOnClickListener(view -> dismissDialog());

        EditText etPasswordOld = dialog.findViewById(R.id.et_password_1);
        EditText etPassword = dialog.findViewById(R.id.et_password_2);
        EditText etConfirmPassword = dialog.findViewById(R.id.et_password_3);

        if (sharedPref.getAdultPassword().isEmpty()){
            etPasswordOld.setVisibility(View.GONE);
        } else {
            etPasswordOld.setVisibility(View.VISIBLE);
        }

        dialog.findViewById(R.id.tv_submit_adult).setOnClickListener(view -> {
            if (etPasswordOld.getVisibility() == View.VISIBLE){
                if(etPasswordOld.getText().toString().trim().isEmpty()) {
                    etPasswordOld.setError(context.getResources().getString(R.string.err_cannot_empty));
                    etPasswordOld.requestFocus();
                }
                else if(etPasswordOld.getText().toString().endsWith(" ")) {
                    etPasswordOld.setError(context.getResources().getString(R.string.error_pass_end_space));
                    etPasswordOld.requestFocus();
                }
                else if (sharedPref.getAdultPassword().equals(etPasswordOld.getText().toString())){
                    if(etPassword.getText().toString().trim().isEmpty()) {
                        etPassword.setError(context.getResources().getString(R.string.err_cannot_empty));
                        etPassword.requestFocus();
                    } else if(etPassword.getText().toString().endsWith(" ")) {
                        etPassword.setError(context.getResources().getString(R.string.error_pass_end_space));
                        etPassword.requestFocus();
                    } else if(etConfirmPassword.getText().toString().trim().isEmpty()) {
                        etConfirmPassword.setError(context.getResources().getString(R.string.err_cannot_empty));
                        etConfirmPassword.requestFocus();
                    } else if(etConfirmPassword.getText().toString().endsWith(" ")) {
                        etConfirmPassword.setError(context.getResources().getString(R.string.error_pass_end_space));
                        etConfirmPassword.requestFocus();
                    } else if (!etConfirmPassword.getText().toString().equals(etPassword.getText().toString())){
                        etConfirmPassword.setError(context.getResources().getString(R.string.error_pass_not_match));
                        etConfirmPassword.requestFocus();
                    } else {
                        sharedPref.setAdultPassword(etPassword.getText().toString());
                        dismissDialog();
                    }
                } else {
                    etPasswordOld.setError(context.getString(R.string.error_old_pass_not_match));
                    etPasswordOld.requestFocus();
                }
            } else {
                if(etPassword.getText().toString().trim().isEmpty()) {
                    etPassword.setError(context.getResources().getString(R.string.err_cannot_empty));
                    etPassword.requestFocus();
                } else if(etPassword.getText().toString().endsWith(" ")) {
                    etPassword.setError(context.getResources().getString(R.string.error_pass_end_space));
                    etPassword.requestFocus();
                } else if(etConfirmPassword.getText().toString().trim().isEmpty()) {
                    etConfirmPassword.setError(context.getResources().getString(R.string.err_cannot_empty));
                    etConfirmPassword.requestFocus();
                } else if(etConfirmPassword.getText().toString().endsWith(" ")) {
                    etConfirmPassword.setError(context.getResources().getString(R.string.error_pass_end_space));
                    etConfirmPassword.requestFocus();
                } else if (!etConfirmPassword.getText().toString().equals(etPassword.getText().toString())){
                    etConfirmPassword.setError(context.getResources().getString(R.string.error_pass_not_match));
                    etConfirmPassword.requestFocus();
                } else {
                    sharedPref.setAdultPassword(etPassword.getText().toString());
                    dismissDialog();
                }
            }
        });
        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.DialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        IfSupported.hideStatusBarDialog(window);
        window.setLayout(MATCH_PARENT, WRAP_CONTENT);
    }

    public void dismissDialog() {
        if (dialog != null && dialog.isShowing()){
            dialog.dismiss();
        }
    }

    public boolean isShowing() {
        return dialog != null && dialog.isShowing();
    }

}
