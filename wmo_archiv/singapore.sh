for file in TEMP*2023-10/*txt; do
    ofile=$file
    file=${file#* }
    file=${file%%_*}
    year=${file:8:4}
    month=${file:12:2}
    day_hour=${file:14:4}
    hour=${file:16:2}
    new_a_filename="afw2ha.${day_hour}"
    new_c_filename="afw2hc.${day_hour}"
    new_d_filename="afw2hd.${day_hour}"
    dir=$year/M$month

    # Create the directory if it doesn't exist
    mkdir -p "$dir"

    # Copy the file to the new directory with the new filename
    cp "$ofile" "$dir/$new_a_filename"
    cp "$ofile" "$dir/$new_c_filename"
    cp "$ofile" "$dir/$new_d_filename"

    # for file in */afw2ha.*; do
	# extension="${file##*.}" ; file_path="${file%%/*}"
    # cp $file "$file_path/afw2hd.$extension" ; cp "$file" "$file_path/afw2hc.$extension"
    # done
done

