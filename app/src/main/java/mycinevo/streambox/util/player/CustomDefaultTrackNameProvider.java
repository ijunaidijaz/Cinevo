package mycinevo.streambox.util.player;

import android.content.res.Resources;

import androidx.annotation.OptIn;
import androidx.media3.common.Format;
import androidx.media3.common.MimeTypes;
import androidx.media3.ui.DefaultTrackNameProvider;

import mycinevo.streambox.BuildConfig;

@OptIn(markerClass = androidx.media3.common.util.UnstableApi.class)
public class CustomDefaultTrackNameProvider extends DefaultTrackNameProvider {

    public CustomDefaultTrackNameProvider(Resources resources) {
        super(resources);
    }

    @Override
    public String getTrackName(Format format) {
        String trackName = super.getTrackName(format);

        if (format.sampleMimeType != null) {
            // Get a human-readable format name from the MIME type
            String sampleFormat = formatNameFromMime(format.sampleMimeType);

            // If in DEBUG mode and the sample format is null, use the MIME type as the format name
            if (BuildConfig.DEBUG && sampleFormat == null) {
                sampleFormat = format.sampleMimeType;
            }

            // Append the format name to the track name
            trackName += " (" + sampleFormat + ")";
        }

        // Append the label to the track name if it's not already included
        if (format.label != null) {
            if (!trackName.startsWith(format.label)) { // HACK
                trackName += " - " + format.label;
            }
        }
        return trackName;
    }

    private String formatNameFromMime(final String mimeType) {
        return switch (mimeType) {
            case MimeTypes.AUDIO_DTS -> "DTS";
            case MimeTypes.AUDIO_DTS_HD -> "DTS-HD";
            case MimeTypes.AUDIO_DTS_EXPRESS -> "DTS Express";
            case MimeTypes.AUDIO_TRUEHD -> "TrueHD";
            case MimeTypes.AUDIO_AC3 -> "AC-3";
            case MimeTypes.AUDIO_E_AC3 -> "E-AC-3";
            case MimeTypes.AUDIO_E_AC3_JOC -> "E-AC-3-JOC";
            case MimeTypes.AUDIO_AC4 -> "AC-4";
            case MimeTypes.AUDIO_AAC -> "AAC";
            case MimeTypes.AUDIO_MPEG -> "MP3";
            case MimeTypes.AUDIO_MPEG_L2 -> "MP2";
            case MimeTypes.AUDIO_VORBIS -> "Vorbis";
            case MimeTypes.AUDIO_OPUS -> "Opus";
            case MimeTypes.AUDIO_FLAC -> "FLAC";
            case MimeTypes.AUDIO_ALAC -> "ALAC";
            case MimeTypes.AUDIO_WAV -> "WAV";
            case MimeTypes.AUDIO_AMR -> "AMR";
            case MimeTypes.AUDIO_AMR_NB -> "AMR-NB";
            case MimeTypes.AUDIO_AMR_WB -> "AMR-WB";
            case MimeTypes.APPLICATION_PGS -> "PGS";
            case MimeTypes.APPLICATION_SUBRIP -> "SRT";
            case MimeTypes.TEXT_SSA -> "SSA";
            case MimeTypes.TEXT_VTT -> "VTT";
            case MimeTypes.APPLICATION_TTML -> "TTML";
            case MimeTypes.APPLICATION_TX3G -> "TX3G";
            case MimeTypes.APPLICATION_DVBSUBS -> "DVB";
            default -> null;
        };
    }
}
