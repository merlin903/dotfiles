function flac_to_m4a -d "Convert FLAC files to AAC m4a files"
  for file in *.flac
    set -l base (basename $file .flac)
    ffmpeg -i "$file" -vn -acodec aac -b:a 320k -f mp4 -y "$base.m4a"
  end
end
