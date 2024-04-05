package mycinevo.streambox.dialog;

import static android.view.WindowManager.LayoutParams.MATCH_PARENT;
import static android.view.WindowManager.LayoutParams.WRAP_CONTENT;

import android.app.Activity;
import android.app.Dialog;
import android.view.Window;

import java.util.Objects;

import mycinevo.streambox.R;
import mycinevo.streambox.util.IfSupported;

public class DownloadDataDialog {

    public DownloadDataDialog(Activity activity, String type, DownloadListener listener) {
        Dialog dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_download_data);
        dialog.findViewById(R.id.iv_close).setOnClickListener(view -> dialog.dismiss());
        dialog.findViewById(R.id.tv_do_downloaded).setOnClickListener(view -> {
            dialog.dismiss();
            listener.onDownload(type);
        });
        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.DialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        IfSupported.hideStatusBarDialog(window);
        window.setLayout(MATCH_PARENT, WRAP_CONTENT);
    }

    public interface DownloadListener {
        void onDownload(String type);
    }

}
