clear
echo "-----------------------COMPILING PS1 & PS1_TEST---------------------------"
cs3110 compile ps2.ml &&
cs3110 compile ps2_test.ml &&
echo "-------------------------RUNNING UNIT TESTS NOW---------------------------" &&
cs3110 test ps2_test.ml
