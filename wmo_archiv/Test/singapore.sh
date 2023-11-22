for file in TEMP*/*txt; do
    ofile=$file
    file=${file#* }
    file=${file%%_*}
    year=${file:8:4}
    month=${file:12:2}
    day_hour=${file:14:4}
    hour=${file:16:2}
    new_filename="afw2ha.${day_hour}"
    dir=$year/M$month

    # Create the directory if it doesn't exist
    mkdir -p "$dir"

    # Copy the file to the new directory with the new filename
    cp "$ofile" "$dir/$new_filename"
done

