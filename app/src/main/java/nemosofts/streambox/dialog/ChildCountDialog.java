package nemosofts.streambox.dialog;

import static android.view.WindowManager.LayoutParams.MATCH_PARENT;
import static android.view.WindowManager.LayoutParams.WRAP_CONTENT;

import android.app.Dialog;
import android.content.Context;
import android.view.Window;
import android.widget.EditText;

import java.util.Objects;

import nemosofts.streambox.R;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.SPHelper;

public class ChildCountDialog {

    private Dialog dialog;
    private final SPHelper spHelper;
    private final ChildCountListener countListener;

    public ChildCountDialog(Context context, int pos,  ChildCountListener listener) {
        this.countListener = listener;
        spHelper = new SPHelper(context);
        if (spHelper.getAdultPassword().isEmpty()){
            countListener.onUnLock(pos);
        } else {
            dialog = new Dialog(context);
            dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
            dialog.setContentView(R.layout.dialog_child_count);
            dialog.findViewById(R.id.iv_close_adult).setOnClickListener(view -> dismissDialog());
            dialog.findViewById(R.id.tv_cancel_adult).setOnClickListener(view -> dismissDialog());
            EditText et_password = dialog.findViewById(R.id.et_password_adult);
            dialog.findViewById(R.id.tv_unlock_adult).setOnClickListener(view -> {
                if(et_password.getText().toString().trim().isEmpty()) {
                    et_password.setError(context.getString(R.string.err_cannot_empty));
                    et_password.requestFocus();
                } else {
                    if (spHelper.getAdultPassword().equals(et_password.getText().toString())){
                        countListener.onUnLock(pos);
                        dismissDialog();
                    } else {
                        et_password.setError(context.getString(R.string.err_password));
                        et_password.requestFocus();
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
    }

    public void dismissDialog() {
        if (dialog != null && dialog.isShowing()){
            dialog.dismiss();
        }
    }

    public boolean isShowing() {
        return dialog != null && dialog.isShowing();
    }

    public interface ChildCountListener {
        void onUnLock(int pos);
    }
}
