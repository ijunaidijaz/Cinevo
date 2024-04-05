package mycinevo.streambox.dialog;

import static android.view.WindowManager.LayoutParams.MATCH_PARENT;
import static android.view.WindowManager.LayoutParams.WRAP_CONTENT;

import android.app.Activity;
import android.app.Dialog;
import android.view.KeyEvent;
import android.view.Window;

import java.util.Objects;

import mycinevo.streambox.R;
import mycinevo.streambox.util.IfSupported;

public class ScreenDialog {

    private Dialog dialog;
    private final Activity activity;
    private final ScreenDialogListener listener;

    public ScreenDialog(Activity activity, ScreenDialogListener filterListener) {
        this.listener = filterListener;
        this.activity = activity;
    }

    public void showDialog() {
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_screen);
        dialog.setCancelable(false);
        dialog.findViewById(R.id.iv_screen_one).setOnClickListener(v -> {
            listener.onSubmit(1);
            dismissDialog();
        });
        dialog.findViewById(R.id.iv_screen_two).setOnClickListener(v -> {
            listener.onSubmit(2);
            dismissDialog();
        });
        dialog.findViewById(R.id.iv_screen_three).setOnClickListener(v -> {
            listener.onSubmit(3);
            dismissDialog();
        });
        dialog.findViewById(R.id.iv_screen_four).setOnClickListener(v -> {
            listener.onSubmit(4);
            dismissDialog();
        });
        dialog.findViewById(R.id.iv_screen_five).setOnClickListener(v -> {
            listener.onSubmit(5);
            dismissDialog();
        });
        dialog.setOnKeyListener((dialog, keyCode, event) -> {
            if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)) {
                dismissDialog();
                return true;
            }
            return false;
        });
        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.DialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        IfSupported.hideStatusBarDialog(window);
        window.setLayout(MATCH_PARENT, WRAP_CONTENT);
    }

    public boolean isShowing() {
        return dialog != null && dialog.isShowing();
    }

    public void dismissDialog() {
        if (dialog != null && dialog.isShowing()){
            dialog.dismiss();
        }
    }

    public interface ScreenDialogListener {
        void onSubmit(int screen);
    }
}
