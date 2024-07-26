package nemosofts.streambox.util.Encrypter;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.os.AsyncTask;

import androidx.annotation.NonNull;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;

import javax.crypto.Cipher;
import javax.crypto.CipherOutputStream;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import javax.net.ssl.HttpsURLConnection;

import nemosofts.streambox.BuildConfig;
import nemosofts.streambox.util.ApplicationUtil;
import nemosofts.streambox.util.helper.DBHelper;
import nemosofts.streambox.item.ItemVideoDownload;
import okio.BufferedSource;

public class Encrypter {

    private DBHelper dbHelper;
    private Context context;
    private Boolean isEncrypt = false;
    private static Encrypter instance = null;

    private Encrypter() {
        isEncrypt = false;
    }

    public static Encrypter GetInstance() {
        if (instance == null) {
            instance = new Encrypter();
        }
        return instance;
    }

    public void Init(Context context) {
        this.context = context;
        isEncrypt = true;
        dbHelper = new DBHelper(context);
    }

    public String GetEditedFileName(@NonNull File file, String token) {
        String path = file.getAbsolutePath();
        String extension;
        String fname;
        int i = path.lastIndexOf('.');
        if (i > 0) {
            extension = path.substring(i);
            fname = path.substring(0, i) + token;
        } else {
            fname = path + token;
        }
        return fname;
    }

    @SuppressLint("StaticFieldLeak")
    public void encrypt(String fileName, BufferedSource bufferedSource, final ItemVideoDownload itemDownload) {
        try {
            final long a = System.currentTimeMillis();

            File file_encypt = new File(GetEditedFileName(new File(fileName.concat(ApplicationUtil.containerExtension(itemDownload.getContainerExtension()))), ""));
            final String fileSavedName = file_encypt.getName().replace(ApplicationUtil.containerExtension(itemDownload.getContainerExtension()), "");
            itemDownload.setTempName(fileSavedName);

            OutputStream fileStream = new BufferedOutputStream(new FileOutputStream(file_encypt));

            Cipher encryptionCipher = Cipher.getInstance("AES/CTR/NoPadding");
            String AES_ALGORITHM = "AES";

            byte[] secretKey = BuildConfig.ENC_KEY.getBytes();
            byte[] initialIv = BuildConfig.IV.getBytes();

            SecretKeySpec secretKeySpec = new SecretKeySpec(secretKey, AES_ALGORITHM);
            encryptionCipher.init(Cipher.ENCRYPT_MODE, secretKeySpec, new IvParameterSpec(initialIv));

            OutputStream outputStream = new CipherOutputStream(fileStream, encryptionCipher);

            InputStream fis = bufferedSource.inputStream();
            int len;
            byte[] buffer = new byte[2048];
            while ((len = fis.read(buffer)) != -1) {
                outputStream.write(buffer, 0, len);
            }

            fis.close();
            outputStream.close();
            bufferedSource.close();

            new AsyncTask<String, String, String>() {
                String imageName;

                @Override
                protected String doInBackground(String... strings) {
                    imageName = getBitmapFromURL(itemDownload.getStreamIcon(), fileSavedName);
                    if (!imageName.equals("0")) {
                        return "1";
                    } else {
                        return "0";
                    }
                }

                @Override
                protected void onPostExecute(String s) {
                    super.onPostExecute(s);
                    if (!s.equals("1")) {
                        imageName = "null";
                    }
                    itemDownload.setStreamIcon(imageName);
                    itemDownload.setTempName(fileSavedName);
                    dbHelper.addToDownloads(DBHelper.TABLE_DOWNLOAD_MOVIES, itemDownload);
                }
            }.execute();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public String getBitmapFromURL(String src, String name) {
        try {
            if (src != null && src.equals("")) {
                src = "null";
            }
            URL url = new URL(src);
            InputStream input;
            if(src.contains("https://")) {
                HttpsURLConnection connection = (HttpsURLConnection) url.openConnection();
                connection.setDoInput(true);
                connection.connect();
                input = connection.getInputStream();
            } else {
                HttpURLConnection connection = (HttpURLConnection) url.openConnection();
                connection.setDoInput(true);
                connection.connect();
                input = connection.getInputStream();
            }
            Bitmap myBitmap = BitmapFactory.decodeStream(input);

            ByteArrayOutputStream bytes = new ByteArrayOutputStream();
            myBitmap.compress(Bitmap.CompressFormat.JPEG, 80, bytes);

            File f_root = new File(context.getExternalFilesDir("").getAbsolutePath() + File.separator + "/tempim/");
            if (!f_root.exists()) {
                f_root.mkdirs();
            }

            File f = new File(f_root, name + ".jpg");

            f.createNewFile();

            //write the bytes in file
            FileOutputStream fo = new FileOutputStream(f);
            fo.write(bytes.toByteArray());

            // remember close de FileOutput
            fo.close();

            return f.getAbsolutePath();
        } catch (IOException e) {
            // Log exception
            return "0";
        }
    }
}
