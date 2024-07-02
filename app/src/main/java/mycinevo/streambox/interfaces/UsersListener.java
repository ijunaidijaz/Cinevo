package mycinevo.streambox.interfaces;

import java.util.ArrayList;

import mycinevo.streambox.item.ItemUsers;

public interface UsersListener {
    void onStart();
    void onEnd(String success, String verifyStatus, String message, ArrayList<ItemUsers> arrayListUsers);
}