package mycinevo.streambox.interfaces;

public interface LoadSuccessListener {
    void onStart();
    void onEnd(String success, String msg);
}