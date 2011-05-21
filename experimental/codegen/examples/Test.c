// c1 = $out1 = f1($in1);
// c2 = $out1 = f2($in1);
// c3 = $out1 = f3($in1,$in2);
// [c1,c2] ; c3

foo(a) {
  b = f1(a);
  c = f2(a);
  d = f3(b,c);
  return d;
}
