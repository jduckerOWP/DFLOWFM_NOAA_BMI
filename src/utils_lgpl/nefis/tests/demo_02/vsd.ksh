vs <<+++++
!rm -f demo2.res
!rm -f dispstat
use demo2.dat def demo2.def
disp stat to dispstat
let a = ElmName from GrpNaam(1,3,1;1,1,1;1,6,1)
write a to demo2.res X00A
let b = ElmName from GrpNaam(1,3,1;2,2,2;1,6,1)
write b to demo2.res X00B
let c = ElmName from GrpNaam(1,3,1;3,3,3;1,6,1)
write c to demo2.res X00C
let d = ElmName from GrpNaam(1,3,1;4,4,4;1,6,1)
write d to demo2.res X00D
let e = ElmName from GrpNaam(1,3,1;5,5,5;1,6,1)
write e to demo2.res X00E
+++++