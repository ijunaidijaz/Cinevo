package nemosofts.streambox.item;

public class ItemVideo {

    private String title;
    private String path;
    private String thumbnailPath;

    public ItemVideo(String title, String path, String thumbnailPath) {
        this.title = title;
        this.path = path;
        this.thumbnailPath = thumbnailPath;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public String getThumbnailPath() {
        return thumbnailPath;
    }

    public void setThumbnailPath(String thumbnailPath) {
        this.thumbnailPath = thumbnailPath;
    }
}
