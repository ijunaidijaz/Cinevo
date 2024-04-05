package mycinevo.streambox.dialog;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
import static android.view.ViewGroup.LayoutParams.WRAP_CONTENT;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.Dialog;
import android.os.Handler;
import android.view.Window;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import java.util.Objects;

import mycinevo.streambox.R;
import mycinevo.streambox.util.IfSupported;

public class Toasty {

    public static final int ERROR = 0;
    public static final int SUCCESS = 1;

    private Toasty() {
        throw new IllegalStateException("Utility class");
    }

    public static void makeText(Activity activity, String message) {
        makeText(activity, message, SUCCESS);
    }

    @SuppressLint("SetTextI18n")
    public static void makeText(Activity activity, String message, int toastType) {
        try {
            if (!activity.isFinishing()) {
                Dialog dialog = new Dialog(activity);
                dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
                dialog.setContentView(R.layout.dialog_toast);
                RelativeLayout ll_toast_bg = dialog.findViewById(R.id.ll_toast_bg);
                ImageView toast_icon = dialog.findViewById(R.id.iv_toast_icon);
                TextView toast_title = dialog.findViewById(R.id.tv_toast_title);
                TextView toast_message = dialog.findViewById(R.id.tv_toast_message);
                ImageView closeIcon = dialog.findViewById(R.id.iv_toast_close);

                if (closeIcon != null) {
                    closeIcon.setOnClickListener(view -> {
                        if (dialog.isShowing()) {
                            dialog.dismiss();
                        }
                    });
                }

                if (ll_toast_bg != null && toast_icon != null && toast_title != null && toast_message != null) {
                    if (toastType == ERROR) {
                        toast_title.setText("Error!");
                        toast_icon.setImageResource(R.drawable.ic_error_toast);
                        toast_icon.setBackgroundResource(R.drawable.toast_icon_error_bg);
                        ll_toast_bg.setBackgroundResource(R.drawable.toast_error_bg);
                    } else {
                        toast_title.setText("Success!");
                        toast_icon.setImageResource(R.drawable.ic_success_toast);
                        toast_icon.setBackgroundResource(R.drawable.toast_icon_success_bg);
                        ll_toast_bg.setBackgroundResource(R.drawable.toast_success_bg);
                    }
                    if (!message.isEmpty()){
                        toast_message.setText(message);
                    } else {
                        if (toastType == ERROR){
                            toast_message.setText("This is a error message.");
                        } else {
                            toast_message.setText("This is a success message.");
                        }
                    }
                    Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
                    dialog.getWindow().getAttributes().windowAnimations = R.style.DialogAnimation;
                    dialog.show();
                    Window window = dialog.getWindow();
                    IfSupported.hideStatusBarDialog(window);
                    window.setLayout(MATCH_PARENT, WRAP_CONTENT);
                    new Handler().postDelayed(() -> {
                        if (dialog.isShowing()) {
                            dialog.dismiss();
                        }
                    }, 1800);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
