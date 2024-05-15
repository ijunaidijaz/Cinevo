package nemosofts.streambox.dialog;

import static android.view.WindowManager.LayoutParams.MATCH_PARENT;
import static android.view.WindowManager.LayoutParams.WRAP_CONTENT;

import android.app.Dialog;
import android.content.Context;
import android.view.Window;
import android.widget.RadioGroup;

import java.util.Objects;

import nemosofts.streambox.R;
import nemosofts.streambox.util.IfSupported;
import nemosofts.streambox.util.helper.SPHelper;

public class HoursDialog {

    private final Dialog dialog;
    private final HoursListener listener;
    private int limit;

    public HoursDialog(Context context, HoursListener limitListener) {
        this.listener = limitListener;
        SPHelper spHelper = new SPHelper(context);
        dialog = new Dialog(context);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_hours);
        dialog.findViewById(R.id.iv_close_limit).setOnClickListener(view -> dismissDialog());
        dialog.findViewById(R.id.tv_cancel_limit).setOnClickListener(view -> dismissDialog());

        RadioGroup rg =  dialog.findViewById(R.id.rg);

        limit = spHelper.getAutoUpdate();
        if (limit == 1){
            rg.check(R.id.rd_1);
        } else if (limit == 3){
            rg.check(R.id.rd_2);
        } else if (limit == 5){
            rg.check(R.id.rd_3);
        } else if (limit == 7){
            rg.check(R.id.rd_4);
        } else if (limit == 10){
            rg.check(R.id.rd_5);
        }

        dialog.findViewById(R.id.rd_1).setOnClickListener(view -> limit = 1);
        dialog.findViewById(R.id.rd_2).setOnClickListener(view -> limit = 3);
        dialog.findViewById(R.id.rd_3).setOnClickListener(view -> limit = 5);
        dialog.findViewById(R.id.rd_4).setOnClickListener(view -> limit = 7);
        dialog.findViewById(R.id.rd_5).setOnClickListener(view -> limit = 10);
        dialog.findViewById(R.id.tv_submit_limit).setOnClickListener(view -> {
            listener.onSetLimit(limit);
            dismissDialog();
        });

        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.DialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        IfSupported.hideStatusBarDialog(window);
        window.setLayout(MATCH_PARENT, WRAP_CONTENT);
    }

    private void dismissDialog() {
        if (dialog != null && dialog.isShowing()){
            dialog.dismiss();
        }
    }

    public interface HoursListener {
        void onSetLimit(int hours);
    }
}
