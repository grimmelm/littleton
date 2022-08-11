for i in {1..13}
do
    csplit --suppress-matched test$i.convey /-----/ {*}
    mv xx00 test$i.convey
    mv xx01 test$i.sexp
    mv xx02 test$i.human
done
