package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemInfoMovies implements Serializable {

	private final String tmdb_id;
	private final String name;
	private final String movie_image;
	private final String release_date;
	private final String episode_run_time;
	private final String youtube_trailer;
	private final String director;
	private final String cast;
	private final String plot;
	private final String genre;
	private final String rating;

	public ItemInfoMovies(String tmdb_id, String name, String movie_image, String release_date, String episode_run_time, String youtube_trailer, String director, String cast, String plot, String genre, String rating) {
		this.tmdb_id = tmdb_id;
		this.name = name;
		this.movie_image = movie_image;
		this.release_date = release_date;
		this.episode_run_time = episode_run_time;
		this.youtube_trailer = youtube_trailer;
		this.director = director;
		this.cast = cast;
		this.plot = plot;
		this.genre = genre;
		this.rating = rating;
	}

	public String getTmdbID() {
		return tmdb_id;
	}

	public String getName() {
		return name;
	}

	public String getMovieImage() {
		return movie_image;
	}

	public String getReleaseDate() {
		return release_date;
	}

	public String getEpisodeRunTime() {
		return episode_run_time;
	}

	public String getYoutubeTrailer() {
		return youtube_trailer;
	}

	public String getDirector() {
		return director;
	}

	public String getCast() {
		return cast;
	}

	public String getPlot() {
		return plot;
	}

	public String getGenre() {
		return genre;
	}

	public String getRating() {
		return rating;
	}
}
