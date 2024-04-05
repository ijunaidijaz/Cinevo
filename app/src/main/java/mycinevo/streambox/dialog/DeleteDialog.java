package mycinevo.streambox.dialog;

import static android.view.WindowManager.LayoutParams.MATCH_PARENT;
import static android.view.WindowManager.LayoutParams.WRAP_CONTENT;

import android.app.Dialog;
import android.content.Context;
import android.view.Window;

import java.util.Objects;

import mycinevo.streambox.R;
import mycinevo.streambox.util.IfSupported;

public class DeleteDialog {

    private final Dialog dialog;
    private final DeleteListener deleteListener;

    public DeleteDialog(Context context, DeleteListener listener) {
        this.deleteListener = listener;
        dialog = new Dialog(context);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_delete);
        dialog.findViewById(R.id.iv_close_delete).setOnClickListener(view -> dismissDialog());
        dialog.findViewById(R.id.tv_cancel_delete).setOnClickListener(view -> dismissDialog());
        dialog.findViewById(R.id.tv_do_delete).setOnClickListener(view -> {
            deleteListener.onDelete();
            dismissDialog();
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

    public interface DeleteListener {
        void onDelete();
    }
}
