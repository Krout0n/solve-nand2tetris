cargo build --release
path="./target/release"

token_test() {
  cmd="$path/token-test"
  $cmd "$1.jack" > test.xml
  diff -w "$1T.xml" test.xml
  if [ $? -eq "0" ] ; then
    printf  "[\e[32mSUCCESS\e[37m] $1.jack Token XML!\n"
  else
    printf "[\e[31mFAILED!!\e[37m] $1.jack Token XML is Failed!\n"
  fi
  rm -rf test.xml
}

token_test "ArrayTest/Main"
token_test "ExpressionLessSquare/Main"
token_test "ExpressionLessSquare/Square"
token_test "ExpressionLessSquare/SquareGame"
token_test "Square/Main"
token_test "Square/Square"
token_test "Square/SquareGame"
