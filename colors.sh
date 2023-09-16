for i in {0..255}; do
    print -Pn "%K{$i}  %k%F{$i}${(l:3::0:)i}%f "
    (( ($i + 1) % 6 )) || print
done

