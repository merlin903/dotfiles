function flac_to_mp3 -d "Convert FLAC files to mp3 files"
  for file in *.flac
    set -l base (basename $file .flac)
    ffmpeg -i "$file" -vb 320k -vsync 2 -map_metadata 0 "$base.mp3"
  end
end
