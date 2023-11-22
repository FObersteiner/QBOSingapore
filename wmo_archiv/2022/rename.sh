for file in */afw2ha.*; do
 extension="${file##*.}" ; file_path="${file%%/*}"
 cp $file "$file_path/afw2hd.$extension" ; cp "$file" "$file_path/afw2hc.$extension"
done
