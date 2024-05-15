package mycinevo.streambox.util.helper;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.SharedPreferences;

import androidx.annotation.NonNull;

import java.text.SimpleDateFormat;
import java.util.Calendar;

import mycinevo.streambox.callback.Callback;
import mycinevo.streambox.util.Encrypter.EncryptData;


public class SPHelper {

    private final EncryptData encryptData;
    private final SharedPreferences sharedPreferences;
    private final SharedPreferences.Editor editor;

    public static final String TAG_SELECT_XUI = "select_xui",
            TAG_SELECT_STREAM = "select_stream",
            TAG_SELECT_PLAYLIST = "select_playlist",
            TAG_SELECT_DEVICE = "select_device_id",
            TAG_SELECT_SINGLE = "select_single",
            TAG_SELECT_SUBSCRIPTION = "select_single";
    private static final String TAG_ABOUT = "is_about",
            TAG_ABOUT_EMAIL = "app_email",
            TAG_ABOUT_AUTHOR = "app_author",
            TAG_ABOUT_CONTACT = "app_contact",
            TAG_ABOUT_WEB = "app_website",
            TAG_ABOUT_DES = "app_description",
            TAG_ABOUT_DEV = "app_developedBy";
    private static final String TAG_IS_SUPP_RTL = "is_rtl",
            TAG_IS_SUPP_MAINTENANCE = "is_maintenance",
            TAG_IS_SUPP_SCREEN = "is_screenshot",
            TAG_IS_SUPP_APK = "is_apk",
            TAG_IS_SUPP_VPN = "is_vpn",
            TAG_IS_SUPP_XUI_DNS = "is_xui_dns",
            TAG_IS_SUPP_XUI_RADIO = "is_xui_radio",
            TAG_IS_SUPP_STREAM_DNS = "is_stream_dns",
            TAG_IS_SUPP_STREAM_RADIO = "is_stream_radio";
    private static final String TAG_FIRST_OPEN = "first_open";
    private static final String SHARED_PREF_AUTOLOGIN = "autologin";
    private static final String TAG_IS_LOGGED = "islogged";

    public SPHelper(@NonNull Context ctx) {
        encryptData = new EncryptData(ctx);
        sharedPreferences = ctx.getSharedPreferences("setting_streambox", Context.MODE_PRIVATE);
        editor = sharedPreferences.edit();
    }

    public Boolean getIsFirst() {
        return sharedPreferences.getBoolean(TAG_FIRST_OPEN, true);
    }
    public void setIsFirst(Boolean flag) {
        editor.putBoolean(TAG_FIRST_OPEN, flag);
        editor.apply();
    }

    public boolean isLogged() {
        return sharedPreferences.getBoolean(TAG_IS_LOGGED, false);
    }
    public void setIsLogged(Boolean isLogged) {
        editor.putBoolean(TAG_IS_LOGGED, isLogged);
        editor.apply();
    }

    public Boolean getIsAutoLogin() { return sharedPreferences.getBoolean(SHARED_PREF_AUTOLOGIN, false); }
    public void setIsAutoLogin(Boolean isAutoLogin) {
        editor.putBoolean(SHARED_PREF_AUTOLOGIN, isAutoLogin);
        editor.apply();
    }

    public void setLoginDetails(
            // user_info
            String username, String password, String message, int auth,
            String status, String exp_date, String is_trial, String active_cons, String created_at, String max_connections,
            // server_info
            boolean xui, String version, int revision, String url, String port, String https_port,
            String server_protocol, String rtmp_port, int timestamp_now, String time_now, String timezone) {

        // user_info
        editor.putString("username", encryptData.encrypt(username));
        editor.putString("password", encryptData.encrypt(password));
        editor.putString("message", message);
        editor.putInt("auth", auth);
        editor.putString("status",status);
        editor.putString("exp_date", exp_date);
        editor.putString("is_trial", is_trial);
        editor.putString("active_cons", active_cons);
        editor.putString("created_at", created_at);
        editor.putString("max_connections", max_connections);

        // server_info
        editor.putBoolean("is_xui", xui);
        editor.putString("version", version);
        editor.putInt("revision", revision);
        editor.putString("url_data", url);
        editor.putString("port", port);
        editor.putString("https_port", https_port);
        editor.putString("server_protocol", server_protocol);
        editor.putString("rtmp_port", rtmp_port);
        editor.putInt("timestamp_now", timestamp_now);
        editor.putString("time_now", time_now);
        editor.putString("timezone", timezone);

        editor.apply();
    }

    public Boolean getIsXuiUser() {
        return sharedPreferences.getBoolean("is_xui", true);
    }

    public String getIsStatus() {
        return sharedPreferences.getString("status", "");
    }

    public void setAnyName(String any_name){
        editor.putString("any_name", encryptData.encrypt(any_name));
        editor.apply();
    }
    public String getAnyName() {
        return encryptData.decrypt(sharedPreferences.getString("any_name", ""));
    }

    public String getActiveConnections() {
        return sharedPreferences.getString("active_cons", "");
    }

    public String getMaxConnections() {
        return sharedPreferences.getString("max_connections", "");
    }

    public String getUserName() {
        return encryptData.decrypt(sharedPreferences.getString("username", ""));
    }

    public String getPassword() {
        return encryptData.decrypt(sharedPreferences.getString("password",""));
    }

    public String getExpDate() {
        return sharedPreferences.getString("exp_date","0");
    }

    public String getServerURL() {
        String server_protocol =  sharedPreferences.getString("server_protocol","http");
        String url =  sharedPreferences.getString("url_data","");
        String http_port = sharedPreferences.getString("port","");
        String https_port = sharedPreferences.getString("https_port","");
        if (server_protocol.equals("http")){
            return server_protocol+"://"+url+ ":"+http_port+"/";
        } else {
            return server_protocol+"://"+url+ ":"+https_port+"/";
        }
    }

    public String getAPI() {
        String server_protocol =  sharedPreferences.getString("server_protocol","http");
        String url =  sharedPreferences.getString("url_data","");
        String http_port = sharedPreferences.getString("port","");
        String https_port = sharedPreferences.getString("https_port","");
        if (server_protocol.equals("http")){
            return server_protocol+"://"+url+ ":"+http_port+"/player_api.php";
        } else {
            return server_protocol+"://"+url+ ":"+https_port+"/player_api.php";
        }
    }

    public void setCurrentDate(String type){
        Calendar calendar = Calendar.getInstance();
        @SuppressLint("SimpleDateFormat") SimpleDateFormat simpleDateFormat = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
        String currentDateTime = simpleDateFormat.format(calendar.getTime());
        editor.putString(type, currentDateTime);
        editor.apply();
    }

    public void setCurrentDateEmpty(String type){
        editor.putString(type, "");
        editor.apply();
    }

    @NonNull
    public String getCurrent(String type) {
        return sharedPreferences.getString(type, "");
    }


    public String getLoginType() {
        return sharedPreferences.getString("login_type", Callback.TAG_LOGIN);
    }
    public void setLoginType(String type){
        editor.putString("login_type", type);
        editor.apply();
    }

    // AboutDetails --------------------------------------------------------------------------------
    public void setAboutDetails(String email, String author, String contact, String website, String description, String developed) {
        editor.putString(TAG_ABOUT_EMAIL, email);
        editor.putString(TAG_ABOUT_AUTHOR, author);
        editor.putString(TAG_ABOUT_CONTACT, contact);
        editor.putString(TAG_ABOUT_WEB, website);
        editor.putString(TAG_ABOUT_DES, description);
        editor.putString(TAG_ABOUT_DEV, developed);
        editor.apply();
    }
    public String getAppEmail() {
        return sharedPreferences.getString(TAG_ABOUT_EMAIL, "");
    }
    public String getAppAuthor() {
        return sharedPreferences.getString(TAG_ABOUT_AUTHOR, "");
    }
    public String getAppContact() {
        return sharedPreferences.getString(TAG_ABOUT_CONTACT, "");
    }
    public String getAppWebsite() {
        return sharedPreferences.getString(TAG_ABOUT_WEB, "");
    }
    public String getAppDescription() {
        return sharedPreferences.getString(TAG_ABOUT_DES, "");
    }
    public String getAppDevelopedBy() {
        return sharedPreferences.getString(TAG_ABOUT_DEV, "");
    }
    public Boolean getIsAboutDetails() {
        return sharedPreferences.getBoolean(TAG_ABOUT, false);
    }
    public void setAboutDetails(Boolean flag){
        editor.putBoolean(TAG_ABOUT, flag);
        editor.apply();
    }

    // isSupported ---------------------------------------------------------------------------------
    public void setIsSupported(Boolean isRtl, Boolean isMaintenance, Boolean isScreenshot, Boolean isApk, Boolean isVpn, Boolean isXuiDns, Boolean isRadio, Boolean is_stream_dns, Boolean is_stream_radio) {
        editor.putBoolean(TAG_IS_SUPP_RTL, isRtl);
        editor.putBoolean(TAG_IS_SUPP_MAINTENANCE, isMaintenance);
        editor.putBoolean(TAG_IS_SUPP_SCREEN, isScreenshot);
        editor.putBoolean(TAG_IS_SUPP_APK, isApk);
        editor.putBoolean(TAG_IS_SUPP_VPN, isVpn);
        editor.putBoolean(TAG_IS_SUPP_XUI_DNS, isXuiDns);
        editor.putBoolean(TAG_IS_SUPP_XUI_RADIO, isRadio);
        editor.putBoolean(TAG_IS_SUPP_STREAM_DNS, is_stream_dns);
        editor.putBoolean(TAG_IS_SUPP_STREAM_RADIO, is_stream_radio);
        editor.apply();
    }
    public Boolean getIsRTL() {
        return sharedPreferences.getBoolean(TAG_IS_SUPP_RTL, false);
    }
    public Boolean getIsMaintenance() {
        return sharedPreferences.getBoolean(TAG_IS_SUPP_MAINTENANCE, false);
    }
    public Boolean getIsScreenshot() {
        return sharedPreferences.getBoolean(TAG_IS_SUPP_SCREEN, false);
    }
    public Boolean getIsAPK() {
        return sharedPreferences.getBoolean(TAG_IS_SUPP_APK, false);
    }
    public Boolean getIsVPN() {
        return sharedPreferences.getBoolean(TAG_IS_SUPP_VPN, false);
    }
    public Boolean getIsXUI_DNS(){
        return sharedPreferences.getBoolean(TAG_IS_SUPP_XUI_DNS, false);
    }
    public Boolean getIssStreamDNS(){
        return sharedPreferences.getBoolean(TAG_IS_SUPP_STREAM_DNS, false);
    }
    public Boolean getIsRadio(){
        if (getLoginType().equals(Callback.TAG_LOGIN_ONE_UI)){
            return sharedPreferences.getBoolean(TAG_IS_SUPP_XUI_RADIO, false);
        } else {
            return sharedPreferences.getBoolean(TAG_IS_SUPP_STREAM_RADIO, false);
        }
    }

    public int getIsTheme() {
        return sharedPreferences.getInt("is_theme", 0);
    }
    public void setIsTheme(int flag){
        editor.putInt("is_theme", flag);
        editor.apply();
    }

    public boolean getIsDownload() {
        return sharedPreferences.getBoolean("is_download", false);
    }
    public void setIsDownload(Boolean flag){
        editor.putBoolean("is_download", flag);
        editor.apply();
    }

    public String getAdultPassword() {
        if (sharedPreferences.getString("adult_password","").isEmpty()){
            return "";
        } else {
            return encryptData.decrypt(sharedPreferences.getString("adult_password",""));
        }
    }
    public void setAdultPassword(String password){
        editor.putString("adult_password", encryptData.encrypt(password));
        editor.apply();
    }

    public int getScreen() {
        return sharedPreferences.getInt("screen_data", 5);
    }
    public void setScreen(int screen) {
        editor.putInt("screen_data", screen);
        editor.apply();
    }

    public Boolean getIsScreen() {
        return sharedPreferences.getBoolean("is_screen", true);
    }
    public void setIsScreen(Boolean is_screen) {
        editor.putBoolean("is_screen", is_screen);
        editor.apply();
    }

    public int getLiveLimit() {
        return sharedPreferences.getInt("live_limit", 20);
    }
    public void setLiveLimit(int state) {
        editor.putInt("live_limit", state);
        editor.apply();
    }

    public int getMovieLimit() {
        return sharedPreferences.getInt("movie_limit", 20);
    }
    public void setMovieLimit(int state) {
        editor.putInt("movie_limit", state);
        editor.apply();
    }

    public boolean getIsAutoplayEpisode() {
        return sharedPreferences.getBoolean("is_autoplay_epg", false);
    }
    public void setIsAutoplayEpisode(Boolean flag){
        editor.putBoolean("is_autoplay_epg", flag);
        editor.apply();
    }

    public String getAgentName() {
        return sharedPreferences.getString("agent_name","");
    }
    public void setAgentName(String agent){
        editor.putString("agent_name", agent);
        editor.apply();
    }

    public void removeSignOut() {
        editor.putString("login_type", Callback.TAG_LOGIN);
        editor.putBoolean(SHARED_PREF_AUTOLOGIN, false);
        editor.putBoolean(TAG_IS_LOGGED, false);

        editor.putString(Callback.TAG_TV, "");
        editor.putString(Callback.TAG_MOVIE, "");
        editor.putString(Callback.TAG_SERIES, "");

        // User
        editor.putString("username", encryptData.encrypt(""));
        editor.putString("password", encryptData.encrypt(""));
        editor.putString("message", "");
        editor.putInt("auth", 0);
        editor.putString("status","");
        editor.putString("exp_date", "");
        editor.putString("is_trial", "");
        editor.putString("active_cons", "");
        editor.putString("created_at", "");
        editor.putString("max_connections", "");

        // server_info
        editor.putBoolean("is_xui", false);
        editor.putString("version", "0");
        editor.putInt("revision", 0);
        editor.putString("url_data", "");
        editor.putString("port", "");
        editor.putString("https_port", "");
        editor.putString("server_protocol", "");
        editor.putString("rtmp_port", "");
        editor.putInt("timestamp_now", 0);
        editor.putString("time_now", "");
        editor.putString("timezone", "");

        editor.apply();

        if (!Callback.arrayListNotify.isEmpty()){
            Callback.arrayListNotify.clear();
        }
        if (!Callback.arrayList_play.isEmpty()){
            Callback.arrayList_play.clear();
        }
        if (!Callback.arrayListEpisodes.isEmpty()){
            Callback.arrayListEpisodes.clear();
        }
        if (!Callback.arrayListLive.isEmpty()){
            Callback.arrayListLive.clear();
        }
    }

    public Boolean getIsSelect(String type) {
        return sharedPreferences.getBoolean(type, true);
    }
    public void setIsSelect(Boolean xui, Boolean stream, Boolean playlist, Boolean device_id, Boolean single) {
        editor.putBoolean(TAG_SELECT_XUI, xui);
        editor.putBoolean(TAG_SELECT_STREAM, stream);
        editor.putBoolean(TAG_SELECT_PLAYLIST, playlist);
        editor.putBoolean(TAG_SELECT_DEVICE, device_id);
        editor.putBoolean(TAG_SELECT_SINGLE, single);
        editor.apply();
    }

    public int getLiveFormat() {
        return sharedPreferences.getInt("live_format", 0);
    }
    public void setLiveFormat(int state) {
        editor.putInt("live_format", state);
        editor.apply();
    }

    public int getAutoUpdate() {
        return sharedPreferences.getInt("add_data", 5);
    }
    public void setAutoUpdate(int state) {
        editor.putInt("add_data", state);
        editor.apply();
    }

    public String getTmdbKEY() {
        return sharedPreferences.getString("tmdb_key","");
    }
    public void setTmdbKEY(String api_key){
        editor.putString("tmdb_key", api_key);
        editor.apply();
    }

    public Boolean getIsSplashAudio() {
        return sharedPreferences.getBoolean("splash_audio", true);
    }
    public void setIsAudio(boolean is_audio) {
        editor.putBoolean("splash_audio", is_audio);
        editor.apply();
    }

    // Shimmer -------------------------------------------------------------------------------------
    public void setIsShimmering(boolean is_home, boolean is_details) {
        editor.putBoolean("shimmer_home", is_home);
        editor.putBoolean("shimmer_details", is_details);
        editor.apply();
    }

    public Boolean getIsShimmeringHome() {
        return sharedPreferences.getBoolean("shimmer_home", true);
    }
    public Boolean getIsShimmeringDetails() {
        return sharedPreferences.getBoolean("shimmer_details", true);
    }

    // UI ------------------------------------------------------------------------------------------
    public void setIsUI(boolean is_name, boolean is_download, boolean is_cast) {
        editor.putBoolean("ui_card_title", is_name);
        editor.putBoolean("ui_download", is_download);
        editor.putBoolean("ui_cast", is_cast);
        editor.apply();
    }

    public boolean getUICardTitle() {
        return sharedPreferences.getBoolean("ui_card_title", true);
    }
    public boolean getIsCast() {
        return sharedPreferences.getBoolean("ui_cast", true);
    }
    public boolean getIsDownloadUser() {
        return sharedPreferences.getBoolean("ui_download", true);
    }

    public void setIsPlayerUI(boolean is_subtitle, boolean is_vr) {
        editor.putBoolean("ui_player_subtitle", is_subtitle);
        editor.putBoolean("ui_player_vr", is_vr);
        editor.apply();
    }
    public boolean getIsSubtitle() {
        return sharedPreferences.getBoolean("ui_player_subtitle", true);
    }
    public boolean getIsVR() {
        return sharedPreferences.getBoolean("ui_player_vr", true);
    }
}