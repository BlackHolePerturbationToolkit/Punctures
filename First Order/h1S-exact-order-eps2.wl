(* ::Package:: *)

(* ::Section::Closed:: *)
(*Definitions*)


h1PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List]:=Developer`ToPackedArray@N@Table[h1PMemo[l,m,r0,M,\[Delta]r],{\[Delta]r,\[CapitalDelta]r}];


dh1PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List,n_:1]:=Developer`ToPackedArray@N@Table[dh1PMemo[l,m,r0,M,\[Delta]r,n],{\[Delta]r,\[CapitalDelta]r}];


h2PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List]:=Developer`ToPackedArray@N@Table[h2PMemo[l,m,r0,M,\[Delta]r],{\[Delta]r,\[CapitalDelta]r}];


dh2PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List,n_:1]:=Developer`ToPackedArray@N@Table[dh2PMemo[l,m,r0,M,\[Delta]r,n],{\[Delta]r,\[CapitalDelta]r}];


h3PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List]:=Developer`ToPackedArray@N@Table[h3PMemo[l,m,r0,M,\[Delta]r],{\[Delta]r,\[CapitalDelta]r}];


dh3PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List,n_:1]:=Developer`ToPackedArray@N@Table[dh3PMemo[l,m,r0,M,\[Delta]r,n],{\[Delta]r,\[CapitalDelta]r}];


h4PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List]:=Developer`ToPackedArray@N@Table[h4PMemo[l,m,r0,M,\[Delta]r],{\[Delta]r,\[CapitalDelta]r}];


dh4PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List,n_:1]:=Developer`ToPackedArray@N@Table[dh4PMemo[l,m,r0,M,\[Delta]r,n],{\[Delta]r,\[CapitalDelta]r}];


h5PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List]:=Developer`ToPackedArray@N@Table[h5PMemo[l,m,r0,M,\[Delta]r],{\[Delta]r,\[CapitalDelta]r}];


dh5PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List,n_:1]:=Developer`ToPackedArray@N@Table[dh5PMemo[l,m,r0,M,\[Delta]r,n],{\[Delta]r,\[CapitalDelta]r}];


h6PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List]:=Developer`ToPackedArray@N@Table[h6PMemo[l,m,r0,M,\[Delta]r],{\[Delta]r,\[CapitalDelta]r}];


dh6PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List,n_:1]:=Developer`ToPackedArray@N@Table[dh6PMemo[l,m,r0,M,\[Delta]r,n],{\[Delta]r,\[CapitalDelta]r}];


h7PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List]:=Developer`ToPackedArray@N@Table[h7PMemo[l,m,r0,M,\[Delta]r],{\[Delta]r,\[CapitalDelta]r}];


dh7PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List,n_:1]:=Developer`ToPackedArray@N@Table[dh7PMemo[l,m,r0,M,\[Delta]r,n],{\[Delta]r,\[CapitalDelta]r}];


h8PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List]:=Developer`ToPackedArray@N@Table[h8PMemo[l,m,r0,M,\[Delta]r],{\[Delta]r,\[CapitalDelta]r}];


dh8PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List,n_:1]:=Developer`ToPackedArray@N@Table[dh8PMemo[l,m,r0,M,\[Delta]r,n],{\[Delta]r,\[CapitalDelta]r}];


h9PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List]:=Developer`ToPackedArray@N@Table[h9PMemo[l,m,r0,M,\[Delta]r],{\[Delta]r,\[CapitalDelta]r}];


dh9PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List,n_:1]:=Developer`ToPackedArray@N@Table[dh9PMemo[l,m,r0,M,\[Delta]r,n],{\[Delta]r,\[CapitalDelta]r}];


h10PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List]:=Developer`ToPackedArray@N@Table[h10PMemo[l,m,r0,M,\[Delta]r],{\[Delta]r,\[CapitalDelta]r}];


dh10PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_List,n_:1]:=Developer`ToPackedArray@N@Table[dh10PMemo[l,m,r0,M,\[Delta]r,n],{\[Delta]r,\[CapitalDelta]r}];


h1PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


dh1PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_,n_:1]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


h2PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


dh2PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_,n_:1]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


h3PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


dh3PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_,n_:1]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


h4PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


dh4PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_,n_:1]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


h5PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


dh5PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_,n_:1]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


h6PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


dh6PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_,n_:1]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


h7PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


dh7PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_,n_:1]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


h8PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


dh8PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_,n_:1]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


h9PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


dh9PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_,n_:1]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


h10PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


dh10PMemo[l_,m_,r0_,M_,\[CapitalDelta]r_,n_:1]/;Abs[\[CapitalDelta]r]>\[CapitalDelta]rmax:=0.;


hPfields={h1PMemo,h2PMemo,h3PMemo,h4PMemo,h5PMemo,h6PMemo,h7PMemo,h8PMemo,h9PMemo,h10PMemo};
dhPfields={dh1PMemo,dh2PMemo,dh3PMemo,dh4PMemo,dh5PMemo,dh6PMemo,dh7PMemo,dh8PMemo,dh9PMemo,dh10PMemo};


h[i_][l_,m_,mmax_,r0_,M_,\[CapitalDelta]r_]/;MemberQ[{1,3,5,6,7},i]:=Module[{hPMemo=hPfields[[i]]},WignerD[{l,m,0},\[Pi],\[Pi]/2,\[Pi]/2]hPMemo[l,0,r0,M,\[CapitalDelta]r]+Sum[(WignerD[{l,m,mp},\[Pi],\[Pi]/2,\[Pi]/2]+WignerD[{l,m,-mp},\[Pi],\[Pi]/2,\[Pi]/2])hPMemo[l,mp,r0,M,\[CapitalDelta]r],{mp,2,Min[l,mmax],2}]];


dh[i_][l_,m_,mmax_,r0_,M_,\[CapitalDelta]r_,n_:1]/;MemberQ[{1,3,5,6,7},i]:=Module[{dhPMemo=dhPfields[[i]]},WignerD[{l,m,0},\[Pi],\[Pi]/2,\[Pi]/2]dhPMemo[l,0,r0,M,\[CapitalDelta]r,n]+Sum[(WignerD[{l,m,mp},\[Pi],\[Pi]/2,\[Pi]/2]+WignerD[{l,m,-mp},\[Pi],\[Pi]/2,\[Pi]/2])dhPMemo[l,mp,r0,M,\[CapitalDelta]r,n],{mp,2,Min[l,mmax],2}]];


h[i_][l_,m_,mmax_,r0_,M_,\[CapitalDelta]r_]/;MemberQ[{2,4},i]:=Module[{hPMemo=hPfields[[i]]},Sum[(WignerD[{l,m,mp},\[Pi],\[Pi]/2,\[Pi]/2]-WignerD[{l,m,-mp},\[Pi],\[Pi]/2,\[Pi]/2])hPMemo[l,mp,r0,M,\[CapitalDelta]r],{mp,1,Min[l,mmax],2}]];


dh[i_][l_,m_,mmax_,r0_,M_,\[CapitalDelta]r_,n_:1]/;MemberQ[{2,4},i]:=Module[{dhPMemo=dhPfields[[i]]},Sum[(WignerD[{l,m,mp},\[Pi],\[Pi]/2,\[Pi]/2]-WignerD[{l,m,-mp},\[Pi],\[Pi]/2,\[Pi]/2])dhPMemo[l,mp,r0,M,\[CapitalDelta]r,n],{mp,1,Min[l,mmax],2}]];


h[i_][l_,m_,mmax_,r0_,M_,\[CapitalDelta]r_]/;MemberQ[{8},i]:=Module[{hPMemo=hPfields[[i]]},Sum[(WignerD[{l,m,mp},\[Pi],\[Pi]/2,\[Pi]/2]+WignerD[{l,m,-mp},\[Pi],\[Pi]/2,\[Pi]/2])hPMemo[l,mp,r0,M,\[CapitalDelta]r],{mp,1,Min[l,mmax],2}]];


dh[i_][l_,m_,mmax_,r0_,M_,\[CapitalDelta]r_,n_:1]/;MemberQ[{8},i]:=Module[{dhPMemo=dhPfields[[i]]},Sum[(WignerD[{l,m,mp},\[Pi],\[Pi]/2,\[Pi]/2]+WignerD[{l,m,-mp},\[Pi],\[Pi]/2,\[Pi]/2])dhPMemo[l,mp,r0,M,\[CapitalDelta]r,n],{mp,1,Min[l,mmax],2}]];


h[i_][l_,m_,mmax_,r0_,M_,\[CapitalDelta]r_]/;MemberQ[{9,10},i]:=Module[{hPMemo=hPfields[[i]]},Sum[(WignerD[{l,m,mp},\[Pi],\[Pi]/2,\[Pi]/2]-WignerD[{l,m,-mp},\[Pi],\[Pi]/2,\[Pi]/2])hPMemo[l,mp,r0,M,\[CapitalDelta]r],{mp,2,Min[l,mmax],2}]];


dh[i_][l_,m_,mmax_,r0_,M_,\[CapitalDelta]r_,n_:1]/;MemberQ[{9,10},i]:=Module[{dhPMemo=dhPfields[[i]]},Sum[(WignerD[{l,m,mp},\[Pi],\[Pi]/2,\[Pi]/2]-WignerD[{l,m,-mp},\[Pi],\[Pi]/2,\[Pi]/2])dhPMemo[l,mp,r0,M,\[CapitalDelta]r,n],{mp,2,Min[l,mmax],2}]];


h[i_][l_,m_,r0_,M_,\[CapitalDelta]r_]:=h[i][l,m,l,r0,M,\[CapitalDelta]r]


dh[i_][l_,m_,r0_,M_,\[CapitalDelta]r_,n_:1]:=dh[i][l,m,l,r0,M,\[CapitalDelta]r,n]


(* ::Section::Closed:: *)
(*Load precalculated data*)


M=1;
(*r0=710/100M;
r0S=ToString[If[IntegerQ[r0],r0,N[r0]]];*)
(*mmax=10;*)(* Number of m' modes - leave this at 10 *)
(*lmax=40;*)(* Number of l modes for puncture and residual fields *)
(*lmaxret=50;*)(* Number of l modes for retarded fields *)
(*\[CapitalDelta]rmax=2;*)(* Worldtube size*)


Print["Generating h1 files:
r0="<>ToString[r0]<>"
mmax="<>ToString[mmax]<>"
lmax="<>ToString[lmax]<>"
lmaxret="<>ToString[lmaxret]<>"
\[CapitalDelta]rmax="<>\[CapitalDelta]rmax<>"
h1P="<>h1PFile<>"
h1dir="<>h1dir];


grid=Import[FileNameJoin[{h1dir,"no-ddh/h1ret/h1-l0m0.h5"}],{"Datasets","grid"}]/.{Sequence[N@r0,N@r0]->Sequence[N@r0,N@r0], N@r0->Sequence[N@r0,N@r0]};


\[CapitalDelta]rgrid=Rationalize[grid-r0,10^-10]/.{{a___,0,0,b___}:>{a,0,0,b}, 0->Sequence[0,0]};


\[CapitalDelta]rderivgrid=\[CapitalDelta]rgrid/.{a___,0,0,b___}:>{a,Left,Right,b};


r0i=Position[\[CapitalDelta]rgrid,0];


fields={1,2,3,4,5,6,7,8,9,10};


Get[h1PFile];


{rLi,rRi}={Position[\[CapitalDelta]rgrid,-\[CapitalDelta]rmax][[1,1]],Position[\[CapitalDelta]rgrid,\[CapitalDelta]rmax][[1,1]]}


\[CapitalDelta]rgridP=\[CapitalDelta]rgrid[[rLi;;rRi]];


r0iP=Position[\[CapitalDelta]rgridP,0][[1,1]]


\[CapitalDelta]rderivgridP=\[CapitalDelta]rderivgrid[[rLi;;rRi]];


With[{l=2,m=2},{sizel,sizer}=(Dimensions/@Import[FileNameJoin[{h1dir, "no-ddh","h1ret","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{"Datasets",{"inhom_left","inhom_right"}}])[[All,1]]];


r0i[[1,1]]+1==r0i[[2,1]]&&Dimensions[r0i]=={2,1}&&sizel==r0i[[1,1]]&&sizel+sizer==Length[\[CapitalDelta]rgrid]


ClearAll[data]


data[l_,m_]:=data[l,m]=Check[Join@@ImportHDF5[FileNameJoin[{h1dir, "no-ddh","h1ret","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{"Datasets",{"inhom_left","inhom_right"}}],Print[{l,m}]];


hret[1,l_,m_]:=hret[1,l,m]=Complex@@@data[l,m][[All,{1,2}]];


dhret[1,l_,m_]:=dhret[1,l,m]=Complex@@@data[l,m][[All,{1,2}+2]];


hret[2,l_,m_]/;l>1:=hret[2,l,m]=Complex@@@data[l,m][[All,{21,22}]];


dhret[2,l_,m_]/;l>1:=dhret[2,l,m]=Complex@@@data[l,m][[All,{21,22}+2]];


hret[2,1,m_]:=hret[2,1,m]=Complex@@@data[1,m][[All,{17,18}]]


dhret[2,1,m_]:=dhret[2,1,m]=Complex@@@data[1,m][[All,{17,18}+2]]


hret[2,0,m_]:=hret[2,0,m]=Complex@@@data[0,m][[All,{13,14}]]


dhret[2,0,m_]:=dhret[2,0,m]=Complex@@@data[0,m][[All,{13,14}+2]]


hret[3,l_,m_]:=hret[3,l,m]=Complex@@@data[l,m][[All,{5,6}]];


dhret[3,l_,m_]:=dhret[3,l,m]=Complex@@@data[l,m][[All,{5,6}+2]];


hret[4,l_,m_]:=hret[4,l,m]=Complex@@@data[l,m][[All,{25,26}]];


dhret[4,l_,m_]:=dhret[4,l,m]=Complex@@@data[l,m][[All,{25,26}+2]];


hret[4,1,m_]:=hret[4,1,m]=Complex@@@data[1,m][[All,{21,22}]];


dhret[4,1,m_]:=dhret[4,1,m]=Complex@@@data[1,m][[All,{21,22}+2]];


hret[5,l_,m_]:=hret[5,l,m]=Complex@@@data[l,m][[All,{9,10}]];


dhret[5,l_,m_]:=dhret[5,l,m]=Complex@@@data[l,m][[All,{9,10}+2]];


hret[6,l_,m_]:=hret[6,l,m]=Complex@@@data[l,m][[All,{13,14}]];


dhret[6,l_,m_]:=dhret[6,l,m]=Complex@@@data[l,m][[All,{13,14}+2]];


hret[6,0,m_]:=hret[6,0,m]=Complex@@@data[0,m][[All,{9,10}]]


dhret[6,0,m_]:=dhret[6,0,m]=Complex@@@data[0,m][[All,{9,10}+2]]


hret[7,l_,m_]:=hret[7,l,m]=Complex@@@data[l,m][[All,{17,18}]];


dhret[7,l_,m_]:=dhret[7,l,m]=Complex@@@data[l,m][[All,{17,18}+2]];


hret[8,l_,m_]:=hret[8,l,m]=Complex@@@data[l,m][[All,{9,10}]];


dhret[8,l_,m_]:=dhret[8,l,m]=Complex@@@data[l,m][[All,{9,10}+2]];


hret[8,1,m_]:=hret[8,1,m]=Complex@@@data[1,m][[All,{5,6}]];


dhret[8,1,m_]:=dhret[8,1,m]=Complex@@@data[1,m][[All,{5,6}+2]];


hret[9,l_,m_]:=hret[9,l,m]=Complex@@@data[l,m][[All,{1,2}]];


dhret[9,l_,m_]:=dhret[9,l,m]=Complex@@@data[l,m][[All,{1,2}+2]];


hret[10,l_,m_]:=hret[10,l,m]=Complex@@@data[l,m][[All,{5,6}]];


dhret[10,l_,m_]:=dhret[10,l,m]=Complex@@@data[l,m][[All,{5,6}+2]];


Monitor[
Do[hret[i,l,m],{i,{1,2,3,6}},{l,(*1*)0,lmaxret},{m,l,1(*0*),-2}];
Do[hret[i,l,m],{i,{4,5}},{l,1,lmaxret},{m,l,1(*0*),-2}];
Do[hret[i,l,m],{i,{7}},{l,2,lmaxret},{m,l,1(*0*),-2}];
Do[hret[i,l,m],{i,{8,9}},{l,1,lmaxret},{m,l-1,0,-2}];
Do[hret[i,l,m],{i,{10}},{l,2,lmaxret},{m,l-1,0,-2}];,{i,l,m}]


Monitor[
Do[dhret[i,l,m],{i,{1,2,3,6}},{l,0,(*1,*)lmaxret},{m,l,1(*0*),-2}];
Do[dhret[i,l,m],{i,{4,5}},{l,1,lmaxret},{m,l,1(*0*),-2}];
Do[dhret[i,l,m],{i,{7}},{l,2,lmaxret},{m,l,1(*0*),-2}];
Do[dhret[i,l,m],{i,{8,9}},{l,1,lmaxret},{m,l-1,0,-2}];
Do[dhret[i,l,m],{i,{10}},{l,2,lmaxret},{m,l-1,0,-2}];,{i,l,m}]


Monitor[
Do[hret[i,l,m]=0.+0.I,{i,{1,2,3,6}},{l,(*1*)2,lmaxret,2},{m,{0}}];
Do[hret[i,l,m]=0.+0.I,{i,{4,5}},{l,2,lmaxret,2},{m,{0}}];
Do[hret[i,l,m]=0.+0.I,{i,{7}},{l,2,lmaxret,2},{m,{0}}];,{i,l,m}]


Monitor[
Do[dhret[i,l,m]=0.+0.I,{i,{1,2,3,6}},{l,(*1*)2,lmaxret,2},{m,{0}}];
Do[dhret[i,l,m]=0.+0.I,{i,{4,5}},{l,2,lmaxret,2},{m,{0}}];
Do[dhret[i,l,m]=0.+0.I,{i,{7}},{l,2,lmaxret,2},{m,{0}}];,{i,l,m}]


(* ::Section::Closed:: *)
(*Compute second derivatives of retarded field using field equations*)


Block[
{r=grid,f=1-(2M)/r,fp=(2M)/r^2,fpp=(4M)/r^3,
h1:=hret[1,l,m],h2:=hret[2,l,m],h3:=hret[3,l,m],h4:=hret[4,l,m],h5:=hret[5,l,m],h6:=hret[6,l,m],h7:=hret[7,l,m],h8:=hret[8,l,m],h9:=hret[9,l,m],h10:=hret[10,l,m],dt=(-I m Sqrt[M/r0^3]),
dth1:=dt hret[1,l,m],dth2:=dt hret[2,l,m],dth3:=dt hret[3,l,m],dth4:=dt hret[4,l,m],dth5:=dt hret[5,l,m],dth6:=dt hret[6,l,m],dth7:=dt hret[7,l,m],dth8:=dt hret[8,l,m],dth9:=dt hret[9,l,m],th10:=dt hret[10,l,m],
drh1:=dhret[1,l,m],drh2:=dhret[2,l,m],drh3:=dhret[3,l,m],drh4:=dhret[4,l,m],drh5:=dhret[5,l,m],drh6:=dhret[6,l,m],drh7:=dhret[7,l,m],drh8:=dhret[8,l,m],drh9:=dhret[9,l,m],drh10:=dhret[10,l,m]},
Table[ddhret[1,l,m]=Simplify[-(fp/f)drh1+1/f^2 dt^2 h1+1/f ((2M)/r^3+(l(l+1))/r^2)h1+4/f^2 (1/2 f fp drh1-1/2 fp dth2+f^2/(2r^2) (h1-f h3-If[l>=1,h5,0]-f h6))],{l,0,lmaxret},{m,l,0,-2}];
Table[ddhret[2,l,m]=Simplify[-(fp/f)drh2+1/f^2 dt^2 h2+1/f ((2M)/r^3+(l(l+1))/r^2)h2+4/f^2 (1/2 f fp drh2-1/2 fp dth1+f^2/(2r^2) (h2-If[l>=1,h4,0]))],{l,0,lmaxret},{m,l,0,-2}];
(*Table[ddhret[3,l,m]=Simplify[(-(fp/f)(f drh3+h3 fp)+1/f^2dt^2f h3+1/f((2M)/r^3+(l(l+1))/r^2)f h3+4/f^2(1/2f fp (f drh3+h3 fp)+1/(2r^2)(1-8M/r+10(M/r)^2)f h3 -f^2/(2r^2)(h1-h5-(1-4M/r)h6))-2fp drh3-h3 fpp)/f],{l,0,lmaxret},{m,l,0,-2}]; - Use Akcay, Warburton & Barack instead of Barack & Lousto *)
Table[ddhret[3,l,m]=Simplify[-(fp/f) drh3+1/f^2 dt^2 h3+1/f ((2M)/r^3+(l(l+1))/r^2)h3+4/f^2 (-f/(2r^2))(h1-If[l>=1,h5,0]-(1-4M/r)(h3+h6))],{l,0,lmaxret},{m,l,0,-2}];Table[ddhret[4,l,m]=Simplify[-(fp/f)drh4+1/f^2 dt^2 h4+1/f ((2M)/r^3+(l(l+1))/r^2)h4+4/f^2 (1/4 f fp drh4-1/4 fp dth5-3/4 fp f/r h4-1/2 l(l+1)f/r^2h2)],{l,1,lmaxret},{m,l,0,-2}];
Table[ddhret[5,l,m]=Simplify[-(fp/f)drh5+1/f^2 dt^2 h5+1/f ((2M)/r^3+(l(l+1))/r^2)h5+4/f^2 (1/4 f fp drh5-1/4 fp dth4+f/r^2 (1-7/2M/r)h5-f/(2r^2) l(l+1)(h1-f h3-f h6)-f^2/(2r^2) If[l>=2,h7,0])],{l,1,lmaxret},{m,l,0,-2}];
Table[ddhret[6,l,m]=Simplify[-(fp/f)drh6+1/f^2 dt^2 h6+1/f ((2M)/r^3+(l(l+1))/r^2)h6+4/f^2 (-f/(2r^2))(h1-If[l>=1,h5,0]-(1-4M/r)(h3+h6))],{l,0,lmaxret},{m,l,0,-2}];
Table[ddhret[7,l,m]=Simplify[-(fp/f)drh7+1/f^2 dt^2 h7+1/f ((2M)/r^3+(l(l+1))/r^2)h7+4/f^2 (-f/(2r^2))(h7+(l-1)(l+2)h5)],{l,2,lmaxret},{m,l,0,-2}];
Table[ddhret[8,l,m]=Simplify[-(fp/f)drh8+1/f^2 dt^2 h8+1/f ((2M)/r^3+(l(l+1))/r^2)h8+4/f^2 (1/4 f fp drh8-1/4 fp dth9-3/4 fp f/r h8)],{l,1,lmaxret},{m,l-1,0,-2}];
Table[ddhret[9,l,m]=Simplify[-(fp/f)drh9+1/f^2 dt^2 h9+1/f ((2M)/r^3+(l(l+1))/r^2)h9+4/f^2 (1/4 fp(f drh9-dth8)+f/r^2 ((1-7/2M/r)h9-f/2If[l>=2,h10,0]))],{l,1,lmaxret},{m,l-1,0,-2}];
Table[ddhret[10,l,m]=Simplify[-(fp/f)drh10+1/f^2 dt^2 h10+1/f ((2M)/r^3+(l(l+1))/r^2)h10+4/f^2 (-f/(2r^2))(h10+(l-1)(l+2)h9)],{l,2,lmaxret},{m,l-1,0,-2}];
];


(* ::Section::Closed:: *)
(*Read even static modes*)


evenstaticdata[l_,m_]:=evenstaticdata[l,m]=Check[Join@@Import[FileNameJoin[{h1dir, "EvenStatic","h1ret","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{"Datasets",{"inhom_left","inhom_right"}}],Print[{l,m}]];


Do[
hret[1,l,0]=Complex@@@evenstaticdata[l,0][[All,{1,2}]];
dhret[1,l,0]=Complex@@@evenstaticdata[l,0][[All,{1,2}+2]];
ddhret[1,l,0]=Complex@@@evenstaticdata[l,0][[All,{1,2}+4]];
hret[3,l,0]=Complex@@@evenstaticdata[l,0][[All,{7,8}]];
dhret[3,l,0]=Complex@@@evenstaticdata[l,0][[All,{7,8}+2]];
ddhret[3,l,0]=Complex@@@evenstaticdata[l,0][[All,{7,8}+4]];
hret[5,l,0]=Complex@@@evenstaticdata[l,0][[All,{13,14}]];
dhret[5,l,0]=Complex@@@evenstaticdata[l,0][[All,{13,14}+2]];
ddhret[5,l,0]=Complex@@@evenstaticdata[l,0][[All,{13,14}+4]];
hret[6,l,0]=Complex@@@evenstaticdata[l,0][[All,{19,20}]];
dhret[6,l,0]=Complex@@@evenstaticdata[l,0][[All,{19,20}+2]];
ddhret[6,l,0]=Complex@@@evenstaticdata[l,0][[All,{19,20}+4]];
hret[7,l,0]=Complex@@@evenstaticdata[l,0][[All,{25,26}]];
dhret[7,l,0]=Complex@@@evenstaticdata[l,0][[All,{25,26}+2]];
ddhret[7,l,0]=Complex@@@evenstaticdata[l,0][[All,{25,26}+4]];
hret[2,l,0]=Complex@@@evenstaticdata[l,0][[All,{31,32}]];
dhret[2,l,0]=Complex@@@evenstaticdata[l,0][[All,{31,32}+2]];
ddhret[2,l,0]=Complex@@@evenstaticdata[l,0][[All,{31,32}+4]];
hret[4,l,0]=Complex@@@evenstaticdata[l,0][[All,{37,38}]];
dhret[4,l,0]=Complex@@@evenstaticdata[l,0][[All,{37,38}+2]];
ddhret[4,l,0]=Complex@@@evenstaticdata[l,0][[All,{37,38}+4]];,{l,2,lmaxret,2}]


(* ::Section::Closed:: *)
(*h1S*)


(*Monitor[Do[hS[i,l,m]=Table[h[i][l,m,mmax,r0,M,\[CapitalDelta]r],{\[CapitalDelta]r,grid-r0}],{i,{1,2,3,4,5,6,8,9}},{l,0,40},{m,l,0,-2}],{i,l,m}]//Timing*)


Monitor[Do[hS[i,l,m]=h[i][l,m,mmax,r0,M,\[CapitalDelta]rgridP],{i,{1,2,3,6}},{l,0,lmax},{m,l,0,-2}],{i,l,m}]//Timing


Monitor[Do[hS[i,l,m]=h[i][l,m,mmax,r0,M,\[CapitalDelta]rgridP],{i,{4,5}},{l,1,lmax},{m,l,0,-2}],{i,l,m}]//Timing


Monitor[Do[hS[i,l,m]=h[i][l,m,mmax,r0,M,\[CapitalDelta]rgridP],{i,{7}},{l,2,lmax},{m,l,0,-2}],{i,l,m}]//Timing


Monitor[Do[hS[i,l,m]=h[i][l,m,mmax,r0,M,\[CapitalDelta]rgridP],{i,{8,9}},{l,1,lmax},{m,l-1,0,-2}],{i,l,m}]//Timing


Monitor[Do[hS[i,l,m]=h[i][l,m,mmax,r0,M,\[CapitalDelta]rgridP],{i,{10}},{l,2,lmax},{m,l-1,0,-2}],{i,l,m}]//Timing


Monitor[Do[dhS[i,l,m]=dh[i][l,m,mmax,r0,M,\[CapitalDelta]rderivgridP],{i,{1,2,3,6}},{l,0,lmax},{m,l,0,-2}],{i,l,m}]//Timing


Monitor[Do[dhS[i,l,m]=dh[i][l,m,mmax,r0,M,\[CapitalDelta]rderivgridP],{i,{4,5}},{l,1,lmax},{m,l,0,-2}],{i,l,m}]//Timing


Monitor[Do[dhS[i,l,m]=dh[i][l,m,mmax,r0,M,\[CapitalDelta]rderivgridP],{i,{7}},{l,2,lmax},{m,l,0,-2}],{i,l,m}]//Timing


Monitor[Do[dhS[i,l,m]=dh[i][l,m,mmax,r0,M,\[CapitalDelta]rderivgridP],{i,{8,9}},{l,1,lmax},{m,l-1,0,-2}],{i,l,m}]//Timing


Monitor[Do[dhS[i,l,m]=dh[i][l,m,mmax,r0,M,\[CapitalDelta]rderivgridP],{i,{10}},{l,2,lmax},{m,l-1,0,-2}],{i,l,m}]//Timing


Monitor[Do[ddhS[i,l,m]=dh[i][l,m,mmax,r0,M,\[CapitalDelta]rderivgridP,2],{i,{1,2,3,6}},{l,0,lmax},{m,l,0,-2}],{i,l,m}]//Timing


Monitor[Do[ddhS[i,l,m]=dh[i][l,m,mmax,r0,M,\[CapitalDelta]rderivgridP,2],{i,{4,5}},{l,1,lmax},{m,l,0,-2}],{i,l,m}]//Timing


Monitor[Do[ddhS[i,l,m]=dh[i][l,m,mmax,r0,M,\[CapitalDelta]rderivgridP,2],{i,{7}},{l,2,lmax},{m,l,0,-2}],{i,l,m}]//Timing


Monitor[Do[ddhS[i,l,m]=dh[i][l,m,mmax,r0,M,\[CapitalDelta]rderivgridP,2],{i,{8,9}},{l,1,lmax},{m,l-1,0,-2}],{i,l,m}]//Timing


Monitor[Do[ddhS[i,l,m]=dh[i][l,m,mmax,r0,M,\[CapitalDelta]rderivgridP,2],{i,{10}},{l,2,lmax},{m,l-1,0,-2}],{i,l,m}]//Timing


hS[2,0,0]=ConstantArray[0.,Length[\[CapitalDelta]rgridP]];


hS[9,1,0]=ConstantArray[0.,Length[\[CapitalDelta]rgridP]];


dhS[2,0,0]=ConstantArray[0.,Length[\[CapitalDelta]rgridP]];


dhS[9,1,0]=ConstantArray[0.,Length[\[CapitalDelta]rgridP]];


ddhS[2,0,0]=ConstantArray[0.,Length[\[CapitalDelta]rgridP]];


ddhS[9,1,0]=ConstantArray[0.,Length[\[CapitalDelta]rgridP]];


(* ::Section::Closed:: *)
(*h1R*)


Monitor[Do[hR[i,l,m]=hret[i,l,m][[rLi;;rRi]]-hS[i,l,m],{i,{1,2,3,6}},{l,0,lmax},{m,l,0,-2}],{l,m}]//Timing


Monitor[Do[hR[i,l,m]=hret[i,l,m][[rLi;;rRi]]-hS[i,l,m],{i,{4,5}},{l,1,lmax},{m,l,0,-2}],{l,m}]//Timing


Monitor[Do[hR[i,l,m]=hret[i,l,m][[rLi;;rRi]]-hS[i,l,m],{i,{7}},{l,2,lmax},{m,l,0,-2}],{l,m}]//Timing


Monitor[Do[hR[i,l,m]=hret[i,l,m][[rLi;;rRi]]-hS[i,l,m],{i,{8,9}},{l,1,lmax},{m,l-1,0,-2}],{l,m}]//Timing


Monitor[Do[hR[i,l,m]=hret[i,l,m][[rLi;;rRi]]-hS[i,l,m],{i,{10}},{l,2,lmax},{m,l-1,0,-2}],{l,m}]//Timing


Monitor[Do[dhR[i,l,m]=dhret[i,l,m][[rLi;;rRi]]-dhS[i,l,m],{i,{1,2,3,6}},{l,0,lmax},{m,l,0,-2}],{l,m}]//Timing


Monitor[Do[dhR[i,l,m]=dhret[i,l,m][[rLi;;rRi]]-dhS[i,l,m],{i,{4,5}},{l,1,lmax},{m,l,0,-2}],{l,m}]//Timing


Monitor[Do[dhR[i,l,m]=dhret[i,l,m][[rLi;;rRi]]-dhS[i,l,m],{i,{7}},{l,2,lmax},{m,l,0,-2}],{l,m}]//Timing


Monitor[Do[dhR[i,l,m]=dhret[i,l,m][[rLi;;rRi]]-dhS[i,l,m],{i,{8,9}},{l,1,lmax},{m,l-1,0,-2}],{l,m}]//Timing


Monitor[Do[dhR[i,l,m]=dhret[i,l,m][[rLi;;rRi]]-dhS[i,l,m],{i,{10}},{l,2,lmax},{m,l-1,0,-2}],{l,m}]//Timing


Monitor[Do[ddhR[i,l,m]=ddhret[i,l,m][[rLi;;rRi]]-ddhS[i,l,m],{i,{1,2,3,6}},{l,0,lmax},{m,l,0,-2}],{l,m}]//Timing


Monitor[Do[ddhR[i,l,m]=ddhret[i,l,m][[rLi;;rRi]]-ddhS[i,l,m],{i,{4,5}},{l,1,lmax},{m,l,0,-2}],{l,m}]//Timing


Monitor[Do[ddhR[i,l,m]=ddhret[i,l,m][[rLi;;rRi]]-ddhS[i,l,m],{i,{7}},{l,2,lmax},{m,l,0,-2}],{l,m}]//Timing


Monitor[Do[ddhR[i,l,m]=ddhret[i,l,m][[rLi;;rRi]]-ddhS[i,l,m],{i,{8,9}},{l,1,lmax},{m,l-1,0,-2}],{l,m}]//Timing


Monitor[Do[ddhR[i,l,m]=ddhret[i,l,m][[rLi;;rRi]]-ddhS[i,l,m],{i,{10}},{l,2,lmax},{m,l-1,0,-2}],{l,m}]//Timing


(* ::Section::Closed:: *)
(*Export data*)


gridout=Join[grid[[;;sizel-1]],grid[[sizel+1;;]]];


gridoutP=Join[grid[[rLi;;sizel-1]],grid[[sizel+1;;rRi]]];


(* ::Subsection::Closed:: *)
(*h^(1S)*)


CreateDirectory[FileNameJoin[{h1dir,"h1S"}]];


hSLeft[i_,l_,m_]:=hS[i,l,m][[;;r0iP]](*/._[___,Left,___]\[Rule]0*);
dhSLeft[i_,l_,m_]:=dhS[i,l,m][[;;r0iP]](*/._[___,Left,___]\[Rule]0*);
ddhSLeft[i_,l_,m_]:=ddhS[i,l,m][[;;r0iP]](*/._[___,Left,___]\[Rule]0*);
hSRight[i_,l_,m_]:=hS[i,l,m][[r0iP+1;;]](*/._[___,Right,___]\[Rule]0*);
dhSRight[i_,l_,m_]:=dhS[i,l,m][[r0iP+1;;]](*/._[___,Right,___]\[Rule]0*);
ddhSRight[i_,l_,m_]:=ddhS[i,l,m][[r0iP+1;;]](*/._[___,Right,___]\[Rule]0*);


Table[
Export[FileNameJoin[{h1dir,"h1S","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hSLeft[1,l,m]],Im[hSLeft[1,l,m]],Re[dhSLeft[1,l,m]],Im[dhSLeft[1,l,m]],Re[ddhSLeft[1,l,m]],Im[ddhSLeft[1,l,m]],
Re[hSLeft[3,l,m]],Im[hSLeft[3,l,m]],Re[dhSLeft[3,l,m]],Im[dhSLeft[3,l,m]],Re[ddhSLeft[3,l,m]],Im[ddhSLeft[3,l,m]],
Re[hSLeft[5,l,m]],Im[hSLeft[5,l,m]],Re[dhSLeft[5,l,m]],Im[dhSLeft[5,l,m]],Re[ddhSLeft[5,l,m]],Im[ddhSLeft[5,l,m]],
Re[hSLeft[6,l,m]],Im[hSLeft[6,l,m]],Re[dhSLeft[6,l,m]],Im[dhSLeft[6,l,m]],Re[ddhSLeft[6,l,m]],Im[ddhSLeft[6,l,m]],
Re[hSLeft[7,l,m]],Im[hSLeft[7,l,m]],Re[dhSLeft[7,l,m]],Im[dhSLeft[7,l,m]],Re[ddhSLeft[7,l,m]],Im[ddhSLeft[7,l,m]],
Re[hSLeft[2,l,m]],Im[hSLeft[2,l,m]],Re[dhSLeft[2,l,m]],Im[dhSLeft[2,l,m]],Re[ddhSLeft[2,l,m]],Im[ddhSLeft[2,l,m]],
Re[hSLeft[4,l,m]],Im[hSLeft[4,l,m]],Re[dhSLeft[4,l,m]],Im[dhSLeft[4,l,m]],Re[ddhSLeft[4,l,m]],Im[ddhSLeft[4,l,m]]}],Transpose[{
Re[hSRight[1,l,m]],Im[hSRight[1,l,m]],Re[dhSRight[1,l,m]],Im[dhSRight[1,l,m]],Re[ddhSRight[1,l,m]],Im[ddhSRight[1,l,m]],
Re[hSRight[3,l,m]],Im[hSRight[3,l,m]],Re[dhSRight[3,l,m]],Im[dhSRight[3,l,m]],Re[ddhSRight[3,l,m]],Im[ddhSRight[3,l,m]],
Re[hSRight[5,l,m]],Im[hSRight[5,l,m]],Re[dhSRight[5,l,m]],Im[dhSRight[5,l,m]],Re[ddhSRight[5,l,m]],Im[ddhSRight[5,l,m]],
Re[hSRight[6,l,m]],Im[hSRight[6,l,m]],Re[dhSRight[6,l,m]],Im[dhSRight[6,l,m]],Re[ddhSRight[6,l,m]],Im[ddhSRight[6,l,m]],
Re[hSRight[7,l,m]],Im[hSRight[7,l,m]],Re[dhSRight[7,l,m]],Im[dhSRight[7,l,m]],Re[ddhSRight[7,l,m]],Im[ddhSRight[7,l,m]],
Re[hSRight[2,l,m]],Im[hSRight[2,l,m]],Re[dhSRight[2,l,m]],Im[dhSRight[2,l,m]],Re[ddhSRight[2,l,m]],Im[ddhSRight[2,l,m]],
Re[hSRight[4,l,m]],Im[hSRight[4,l,m]],Re[dhSRight[4,l,m]],Im[dhSRight[4,l,m]],Re[ddhSRight[4,l,m]],Im[ddhSRight[4,l,m]]}],gridoutP},
{"Datasets",{"inhom_left","inhom_right","grid"}}];,{l,2,lmax},{m,l,0,-2}];


Table[
Export[FileNameJoin[{h1dir,"h1S","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hSLeft[9,l,m]],Im[hSLeft[9,l,m]],Re[dhSLeft[9,l,m]],Im[dhSLeft[9,l,m]],Re[ddhSLeft[9,l,m]],Im[ddhSLeft[9,l,m]],
Re[hSLeft[10,l,m]],Im[hSLeft[10,l,m]],Re[dhSLeft[10,l,m]],Im[dhSLeft[10,l,m]],Re[ddhSLeft[10,l,m]],Im[ddhSLeft[10,l,m]],
Re[hSLeft[8,l,m]],Im[hSLeft[8,l,m]],Re[dhSLeft[8,l,m]],Im[dhSLeft[8,l,m]],Re[ddhSLeft[8,l,m]],Im[ddhSLeft[8,l,m]]}],Transpose[{
Re[hSRight[9,l,m]],Im[hSRight[9,l,m]],Re[dhSRight[9,l,m]],Im[dhSRight[9,l,m]],Re[ddhSRight[9,l,m]],Im[ddhSRight[9,l,m]],
Re[hSRight[10,l,m]],Im[hSRight[10,l,m]],Re[dhSRight[10,l,m]],Im[dhSRight[10,l,m]],Re[ddhSRight[10,l,m]],Im[ddhSRight[10,l,m]],
Re[hSRight[8,l,m]],Im[hSRight[8,l,m]],Re[dhSRight[8,l,m]],Im[dhSRight[8,l,m]],Re[ddhSRight[8,l,m]],Im[ddhSRight[8,l,m]]}],gridoutP},
{"Datasets",{"inhom_left","inhom_right","grid"}}];,{l,2,lmax},{m,l-1,0,-2}];


(* ::Text:: *)
(*l=1*)


Table[
Export[FileNameJoin[{h1dir,"h1S","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hSLeft[1,l,m]],Im[hSLeft[1,l,m]],Re[dhSLeft[1,l,m]],Im[dhSLeft[1,l,m]],Re[ddhSLeft[1,l,m]],Im[ddhSLeft[1,l,m]],
Re[hSLeft[3,l,m]],Im[hSLeft[3,l,m]],Re[dhSLeft[3,l,m]],Im[dhSLeft[3,l,m]],Re[ddhSLeft[3,l,m]],Im[ddhSLeft[3,l,m]],
Re[hSLeft[5,l,m]],Im[hSLeft[5,l,m]],Re[dhSLeft[5,l,m]],Im[dhSLeft[5,l,m]],Re[ddhSLeft[5,l,m]],Im[ddhSLeft[5,l,m]],
Re[hSLeft[6,l,m]],Im[hSLeft[6,l,m]],Re[dhSLeft[6,l,m]],Im[dhSLeft[6,l,m]],Re[ddhSLeft[6,l,m]],Im[ddhSLeft[6,l,m]],
Re[hSLeft[2,l,m]],Im[hSLeft[2,l,m]],Re[dhSLeft[2,l,m]],Im[dhSLeft[2,l,m]],Re[ddhSLeft[2,l,m]],Im[ddhSLeft[2,l,m]],
Re[hSLeft[4,l,m]],Im[hSLeft[4,l,m]],Re[dhSLeft[4,l,m]],Im[dhSLeft[4,l,m]],Re[ddhSLeft[4,l,m]],Im[ddhSLeft[4,l,m]]}],Transpose[{
Re[hSRight[1,l,m]],Im[hSRight[1,l,m]],Re[dhSRight[1,l,m]],Im[dhSRight[1,l,m]],Re[ddhSRight[1,l,m]],Im[ddhSRight[1,l,m]],
Re[hSRight[3,l,m]],Im[hSRight[3,l,m]],Re[dhSRight[3,l,m]],Im[dhSRight[3,l,m]],Re[ddhSRight[3,l,m]],Im[ddhSRight[3,l,m]],
Re[hSRight[5,l,m]],Im[hSRight[5,l,m]],Re[dhSRight[5,l,m]],Im[dhSRight[5,l,m]],Re[ddhSRight[5,l,m]],Im[ddhSRight[5,l,m]],
Re[hSRight[6,l,m]],Im[hSRight[6,l,m]],Re[dhSRight[6,l,m]],Im[dhSRight[6,l,m]],Re[ddhSRight[6,l,m]],Im[ddhSRight[6,l,m]],
Re[hSRight[2,l,m]],Im[hSRight[2,l,m]],Re[dhSRight[2,l,m]],Im[dhSRight[2,l,m]],Re[ddhSRight[2,l,m]],Im[ddhSRight[2,l,m]],
Re[hSRight[4,l,m]],Im[hSRight[4,l,m]],Re[dhSRight[4,l,m]],Im[dhSRight[4,l,m]],Re[ddhSRight[4,l,m]],Im[ddhSRight[4,l,m]]}],gridoutP},
{"Datasets",{"inhom_left","inhom_right","grid"}}];,{l,{1}},{m,l,0,-2}];


Table[
Export[FileNameJoin[{h1dir,"h1S","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hSLeft[9,l,m]],Im[hSLeft[9,l,m]],Re[dhSLeft[9,l,m]],Im[dhSLeft[9,l,m]],Re[ddhSLeft[9,l,m]],Im[ddhSLeft[9,l,m]],
Re[hSLeft[8,l,m]],Im[hSLeft[8,l,m]],Re[dhSLeft[8,l,m]],Im[dhSLeft[8,l,m]],Re[ddhSLeft[8,l,m]],Im[ddhSLeft[8,l,m]]}],Transpose[{
Re[hSRight[9,l,m]],Im[hSRight[9,l,m]],Re[dhSRight[9,l,m]],Im[dhSRight[9,l,m]],Re[ddhSRight[9,l,m]],Im[ddhSRight[9,l,m]],
Re[hSRight[8,l,m]],Im[hSRight[8,l,m]],Re[dhSRight[8,l,m]],Im[dhSRight[8,l,m]],Re[ddhSRight[8,l,m]],Im[ddhSRight[8,l,m]]}],gridoutP},
{"Datasets",{"inhom_left","inhom_right","grid"}}];,{l,{1}},{m,l-1,0,-2}];


(* ::Text:: *)
(*l=0*)


Table[
Export[FileNameJoin[{h1dir,"h1S","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hSLeft[1,l,m]],Im[hSLeft[1,l,m]],Re[dhSLeft[1,l,m]],Im[dhSLeft[1,l,m]],Re[ddhSLeft[1,l,m]],Im[ddhSLeft[1,l,m]],
Re[hSLeft[3,l,m]],Im[hSLeft[3,l,m]],Re[dhSLeft[3,l,m]],Im[dhSLeft[3,l,m]],Re[ddhSLeft[3,l,m]],Im[ddhSLeft[3,l,m]],
Re[hSLeft[6,l,m]],Im[hSLeft[6,l,m]],Re[dhSLeft[6,l,m]],Im[dhSLeft[6,l,m]],Re[ddhSLeft[6,l,m]],Im[ddhSLeft[6,l,m]],
Re[hSLeft[2,l,m]],Im[hSLeft[2,l,m]],Re[dhSLeft[2,l,m]],Im[dhSLeft[2,l,m]],Re[ddhSLeft[2,l,m]],Im[ddhSLeft[2,l,m]]}],Transpose[{
Re[hSRight[1,l,m]],Im[hSRight[1,l,m]],Re[dhSRight[1,l,m]],Im[dhSRight[1,l,m]],Re[ddhSRight[1,l,m]],Im[ddhSRight[1,l,m]],
Re[hSRight[3,l,m]],Im[hSRight[3,l,m]],Re[dhSRight[3,l,m]],Im[dhSRight[3,l,m]],Re[ddhSRight[3,l,m]],Im[ddhSRight[3,l,m]],
Re[hSRight[6,l,m]],Im[hSRight[6,l,m]],Re[dhSRight[6,l,m]],Im[dhSRight[6,l,m]],Re[ddhSRight[6,l,m]],Im[ddhSRight[6,l,m]],
Re[hSRight[2,l,m]],Im[hSRight[2,l,m]],Re[dhSRight[2,l,m]],Im[dhSRight[2,l,m]],Re[ddhSRight[2,l,m]],Im[ddhSRight[2,l,m]]}],gridoutP},
{"Datasets",{"inhom_left","inhom_right","grid"}}];
,{l,{0}},{m,l,0,-2}];


(* ::Subsection::Closed:: *)
(*h^(1R)*)


CreateDirectory[FileNameJoin[{h1dir,"h1R"}]];


hRLeft[i_,l_,m_]:=hR[i,l,m][[;;r0iP]](*/._[___,Left,___]\[Rule]0*);
hRRight[i_,l_,m_]:=hR[i,l,m][[r0iP+1;;]](*/._[___,Right,___]\[Rule]0*);
dhRLeft[i_,l_,m_]:=dhR[i,l,m][[;;r0iP]](*/._[___,Left,___]\[Rule]0*);
dhRRight[i_,l_,m_]:=dhR[i,l,m][[r0iP+1;;]](*/._[___,Right,___]\[Rule]0*);
ddhRLeft[i_,l_,m_]:=ddhR[i,l,m][[;;r0iP]](*/._[___,Left,___]\[Rule]0*);
ddhRRight[i_,l_,m_]:=ddhR[i,l,m][[r0iP+1;;]](*/._[___,Right,___]\[Rule]0*);


Table[
Export[FileNameJoin[{h1dir,"h1R","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hRLeft[1,l,m]],Im[hRLeft[1,l,m]],Re[dhRLeft[1,l,m]],Im[dhRLeft[1,l,m]],Re[ddhRLeft[1,l,m]],Im[ddhRLeft[1,l,m]],
Re[hRLeft[3,l,m]],Im[hRLeft[3,l,m]],Re[dhRLeft[3,l,m]],Im[dhRLeft[3,l,m]],Re[ddhRLeft[3,l,m]],Im[ddhRLeft[3,l,m]],
Re[hRLeft[5,l,m]],Im[hRLeft[5,l,m]],Re[dhRLeft[5,l,m]],Im[dhRLeft[5,l,m]],Re[ddhRLeft[5,l,m]],Im[ddhRLeft[5,l,m]],
Re[hRLeft[6,l,m]],Im[hRLeft[6,l,m]],Re[dhRLeft[6,l,m]],Im[dhRLeft[6,l,m]],Re[ddhRLeft[6,l,m]],Im[ddhRLeft[6,l,m]],
Re[hRLeft[7,l,m]],Im[hRLeft[7,l,m]],Re[dhRLeft[7,l,m]],Im[dhRLeft[7,l,m]],Re[ddhRLeft[7,l,m]],Im[ddhRLeft[7,l,m]],
Re[hRLeft[2,l,m]],Im[hRLeft[2,l,m]],Re[dhRLeft[2,l,m]],Im[dhRLeft[2,l,m]],Re[ddhRLeft[2,l,m]],Im[ddhRLeft[2,l,m]],
Re[hRLeft[4,l,m]],Im[hRLeft[4,l,m]],Re[dhRLeft[4,l,m]],Im[dhRLeft[4,l,m]],Re[ddhRLeft[4,l,m]],Im[ddhRLeft[4,l,m]]}],Transpose[{
Re[hRRight[1,l,m]],Im[hRRight[1,l,m]],Re[dhRRight[1,l,m]],Im[dhRRight[1,l,m]],Re[ddhRRight[1,l,m]],Im[ddhRRight[1,l,m]],
Re[hRRight[3,l,m]],Im[hRRight[3,l,m]],Re[dhRRight[3,l,m]],Im[dhRRight[3,l,m]],Re[ddhRRight[3,l,m]],Im[ddhRRight[3,l,m]],
Re[hRRight[5,l,m]],Im[hRRight[5,l,m]],Re[dhRRight[5,l,m]],Im[dhRRight[5,l,m]],Re[ddhRRight[5,l,m]],Im[ddhRRight[5,l,m]],
Re[hRRight[6,l,m]],Im[hRRight[6,l,m]],Re[dhRRight[6,l,m]],Im[dhRRight[6,l,m]],Re[ddhRRight[6,l,m]],Im[ddhRRight[6,l,m]],
Re[hRRight[7,l,m]],Im[hRRight[7,l,m]],Re[dhRRight[7,l,m]],Im[dhRRight[7,l,m]],Re[ddhRRight[7,l,m]],Im[ddhRRight[7,l,m]],
Re[hRRight[2,l,m]],Im[hRRight[2,l,m]],Re[dhRRight[2,l,m]],Im[dhRRight[2,l,m]],Re[ddhRRight[2,l,m]],Im[ddhRRight[2,l,m]],
Re[hRRight[4,l,m]],Im[hRRight[4,l,m]],Re[dhRRight[4,l,m]],Im[dhRRight[4,l,m]],Re[ddhRRight[4,l,m]],Im[ddhRRight[4,l,m]]}],gridoutP},
{"Datasets",{"inhom_left","inhom_right","grid"}}];,{l,2,lmax},{m,l,0,-2}];


Table[
Export[FileNameJoin[{h1dir,"h1R","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hRLeft[9,l,m]],Im[hRLeft[9,l,m]],Re[dhRLeft[9,l,m]],Im[dhRLeft[9,l,m]],Re[ddhRLeft[9,l,m]],Im[ddhRLeft[9,l,m]],
Re[hRLeft[10,l,m]],Im[hRLeft[10,l,m]],Re[dhRLeft[10,l,m]],Im[dhRLeft[10,l,m]],Re[ddhRLeft[10,l,m]],Im[ddhRLeft[10,l,m]],
Re[hRLeft[8,l,m]],Im[hRLeft[8,l,m]],Re[dhRLeft[8,l,m]],Im[dhRLeft[8,l,m]],Re[ddhRLeft[8,l,m]],Im[ddhRLeft[8,l,m]]}],Transpose[{
Re[hRRight[9,l,m]],Im[hRRight[9,l,m]],Re[dhRRight[9,l,m]],Im[dhRRight[9,l,m]],Re[ddhRRight[9,l,m]],Im[ddhRRight[9,l,m]],
Re[hRRight[10,l,m]],Im[hRRight[10,l,m]],Re[dhRRight[10,l,m]],Im[dhRRight[10,l,m]],Re[ddhRRight[10,l,m]],Im[ddhRRight[10,l,m]],
Re[hRRight[8,l,m]],Im[hRRight[8,l,m]],Re[dhRRight[8,l,m]],Im[dhRRight[8,l,m]],Re[ddhRRight[8,l,m]],Im[ddhRRight[8,l,m]]}],gridoutP},
{"Datasets",{"inhom_left","inhom_right","grid"}}];,{l,2,lmax},{m,l-1,0,-2}];


(* ::Text:: *)
(*l=1*)


Table[
Export[FileNameJoin[{h1dir,"h1R","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hRLeft[1,l,m]],Im[hRLeft[1,l,m]],Re[dhRLeft[1,l,m]],Im[dhRLeft[1,l,m]],Re[ddhRLeft[1,l,m]],Im[ddhRLeft[1,l,m]],
Re[hRLeft[3,l,m]],Im[hRLeft[3,l,m]],Re[dhRLeft[3,l,m]],Im[dhRLeft[3,l,m]],Re[ddhRLeft[3,l,m]],Im[ddhRLeft[3,l,m]],
Re[hRLeft[5,l,m]],Im[hRLeft[5,l,m]],Re[dhRLeft[5,l,m]],Im[dhRLeft[5,l,m]],Re[ddhRLeft[5,l,m]],Im[ddhRLeft[5,l,m]],
Re[hRLeft[6,l,m]],Im[hRLeft[6,l,m]],Re[dhRLeft[6,l,m]],Im[dhRLeft[6,l,m]],Re[ddhRLeft[6,l,m]],Im[ddhRLeft[6,l,m]],
Re[hRLeft[2,l,m]],Im[hRLeft[2,l,m]],Re[dhRLeft[2,l,m]],Im[dhRLeft[2,l,m]],Re[ddhRLeft[2,l,m]],Im[ddhRLeft[2,l,m]],
Re[hRLeft[4,l,m]],Im[hRLeft[4,l,m]],Re[dhRLeft[4,l,m]],Im[dhRLeft[4,l,m]],Re[ddhRLeft[4,l,m]],Im[ddhRLeft[4,l,m]]}],Transpose[{
Re[hRRight[1,l,m]],Im[hRRight[1,l,m]],Re[dhRRight[1,l,m]],Im[dhRRight[1,l,m]],Re[ddhRRight[1,l,m]],Im[ddhRRight[1,l,m]],
Re[hRRight[3,l,m]],Im[hRRight[3,l,m]],Re[dhRRight[3,l,m]],Im[dhRRight[3,l,m]],Re[ddhRRight[3,l,m]],Im[ddhRRight[3,l,m]],
Re[hRRight[5,l,m]],Im[hRRight[5,l,m]],Re[dhRRight[5,l,m]],Im[dhRRight[5,l,m]],Re[ddhRRight[5,l,m]],Im[ddhRRight[5,l,m]],
Re[hRRight[6,l,m]],Im[hRRight[6,l,m]],Re[dhRRight[6,l,m]],Im[dhRRight[6,l,m]],Re[ddhRRight[6,l,m]],Im[ddhRRight[6,l,m]],
Re[hRRight[2,l,m]],Im[hRRight[2,l,m]],Re[dhRRight[2,l,m]],Im[dhRRight[2,l,m]],Re[ddhRRight[2,l,m]],Im[ddhRRight[2,l,m]],
Re[hRRight[4,l,m]],Im[hRRight[4,l,m]],Re[dhRRight[4,l,m]],Im[dhRRight[4,l,m]],Re[ddhRRight[4,l,m]],Im[ddhRRight[4,l,m]]}],gridoutP},
{"Datasets",{"inhom_left","inhom_right","grid"}}];,{l,{1}},{m,l,0,-2}];


Table[
Export[FileNameJoin[{h1dir,"h1R","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hRLeft[9,l,m]],Im[hRLeft[9,l,m]],Re[dhRLeft[9,l,m]],Im[dhRLeft[9,l,m]],Re[ddhRLeft[9,l,m]],Im[ddhRLeft[9,l,m]],
Re[hRLeft[8,l,m]],Im[hRLeft[8,l,m]],Re[dhRLeft[8,l,m]],Im[dhRLeft[8,l,m]],Re[ddhRLeft[8,l,m]],Im[ddhRLeft[8,l,m]]}],Transpose[{
Re[hRRight[9,l,m]],Im[hRRight[9,l,m]],Re[dhRRight[9,l,m]],Im[dhRRight[9,l,m]],Re[ddhRRight[9,l,m]],Im[ddhRRight[9,l,m]],
Re[hRRight[8,l,m]],Im[hRRight[8,l,m]],Re[dhRRight[8,l,m]],Im[dhRRight[8,l,m]],Re[ddhRRight[8,l,m]],Im[ddhRRight[8,l,m]]}],gridoutP},
{"Datasets",{"inhom_left","inhom_right","grid"}}];,{l,{1}},{m,l-1,0,-2}];


(* ::Text:: *)
(*l=0*)


Table[
Export[FileNameJoin[{h1dir,"h1R","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hRLeft[1,l,m]],Im[hRLeft[1,l,m]],Re[dhRLeft[1,l,m]],Im[dhRLeft[1,l,m]],Re[ddhRLeft[1,l,m]],Im[ddhRLeft[1,l,m]],
Re[hRLeft[3,l,m]],Im[hRLeft[3,l,m]],Re[dhRLeft[3,l,m]],Im[dhRLeft[3,l,m]],Re[ddhRLeft[3,l,m]],Im[ddhRLeft[3,l,m]],
Re[hRLeft[6,l,m]],Im[hRLeft[6,l,m]],Re[dhRLeft[6,l,m]],Im[dhRLeft[6,l,m]],Re[ddhRLeft[6,l,m]],Im[ddhRLeft[6,l,m]],
Re[hRLeft[2,l,m]],Im[hRLeft[2,l,m]],Re[dhRLeft[2,l,m]],Im[dhRLeft[2,l,m]],Re[ddhRLeft[2,l,m]],Im[ddhRLeft[2,l,m]]}],Transpose[{
Re[hRRight[1,l,m]],Im[hRRight[1,l,m]],Re[dhRRight[1,l,m]],Im[dhRRight[1,l,m]],Re[ddhRRight[1,l,m]],Im[ddhRRight[1,l,m]],
Re[hRRight[3,l,m]],Im[hRRight[3,l,m]],Re[dhRRight[3,l,m]],Im[dhRRight[3,l,m]],Re[ddhRRight[3,l,m]],Im[ddhRRight[3,l,m]],
Re[hRRight[6,l,m]],Im[hRRight[6,l,m]],Re[dhRRight[6,l,m]],Im[dhRRight[6,l,m]],Re[ddhRRight[6,l,m]],Im[ddhRRight[6,l,m]],
Re[hRRight[2,l,m]],Im[hRRight[2,l,m]],Re[dhRRight[2,l,m]],Im[dhRRight[2,l,m]],Re[ddhRRight[2,l,m]],Im[ddhRRight[2,l,m]]}],gridoutP},
{"Datasets",{"inhom_left","inhom_right","grid"}}];
,{l,{0}},{m,l,0,-2}];


(* ::Subsection::Closed:: *)
(*h^(1ret)*)


CreateDirectory[FileNameJoin[{h1dir,"h1ret"}]];


hretLeft[i_,l_,m_]:=hret[i,l,m][[;;sizel]](*/._[___,Left,___]\[Rule]0*);
hretRight[i_,l_,m_]:=hret[i,l,m][[sizel+1;;]](*/._[___,Right,___]\[Rule]0*);
dhretLeft[i_,l_,m_]:=dhret[i,l,m][[;;sizel]](*/._[___,Left,___]\[Rule]0*);
dhretRight[i_,l_,m_]:=dhret[i,l,m][[sizel+1;;]](*/._[___,Right,___]\[Rule]0*);
ddhretLeft[i_,l_,m_]:=ddhret[i,l,m][[;;sizel]](*/._[___,Left,___]\[Rule]0*);
ddhretRight[i_,l_,m_]:=ddhret[i,l,m][[sizel+1;;]](*/._[___,Right,___]\[Rule]0*);


Table[
Export[FileNameJoin[{h1dir,"h1ret","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hretLeft[1,l,m]],Im[hretLeft[1,l,m]],Re[dhretLeft[1,l,m]],Im[dhretLeft[1,l,m]],Re[ddhretLeft[1,l,m]],Im[ddhretLeft[1,l,m]],
Re[hretLeft[3,l,m]],Im[hretLeft[3,l,m]],Re[dhretLeft[3,l,m]],Im[dhretLeft[3,l,m]],Re[ddhretLeft[3,l,m]],Im[ddhretLeft[3,l,m]],
Re[hretLeft[5,l,m]],Im[hretLeft[5,l,m]],Re[dhretLeft[5,l,m]],Im[dhretLeft[5,l,m]],Re[ddhretLeft[5,l,m]],Im[ddhretLeft[5,l,m]],
Re[hretLeft[6,l,m]],Im[hretLeft[6,l,m]],Re[dhretLeft[6,l,m]],Im[dhretLeft[6,l,m]],Re[ddhretLeft[6,l,m]],Im[ddhretLeft[6,l,m]],
Re[hretLeft[7,l,m]],Im[hretLeft[7,l,m]],Re[dhretLeft[7,l,m]],Im[dhretLeft[7,l,m]],Re[ddhretLeft[7,l,m]],Im[ddhretLeft[7,l,m]],
Re[hretLeft[2,l,m]],Im[hretLeft[2,l,m]],Re[dhretLeft[2,l,m]],Im[dhretLeft[2,l,m]],Re[ddhretLeft[2,l,m]],Im[ddhretLeft[2,l,m]],
Re[hretLeft[4,l,m]],Im[hretLeft[4,l,m]],Re[dhretLeft[4,l,m]],Im[dhretLeft[4,l,m]],Re[ddhretLeft[4,l,m]],Im[ddhretLeft[4,l,m]]}],Transpose[{
Re[hretRight[1,l,m]],Im[hretRight[1,l,m]],Re[dhretRight[1,l,m]],Im[dhretRight[1,l,m]],Re[ddhretRight[1,l,m]],Im[ddhretRight[1,l,m]],
Re[hretRight[3,l,m]],Im[hretRight[3,l,m]],Re[dhretRight[3,l,m]],Im[dhretRight[3,l,m]],Re[ddhretRight[3,l,m]],Im[ddhretRight[3,l,m]],
Re[hretRight[5,l,m]],Im[hretRight[5,l,m]],Re[dhretRight[5,l,m]],Im[dhretRight[5,l,m]],Re[ddhretRight[5,l,m]],Im[ddhretRight[5,l,m]],
Re[hretRight[6,l,m]],Im[hretRight[6,l,m]],Re[dhretRight[6,l,m]],Im[dhretRight[6,l,m]],Re[ddhretRight[6,l,m]],Im[ddhretRight[6,l,m]],
Re[hretRight[7,l,m]],Im[hretRight[7,l,m]],Re[dhretRight[7,l,m]],Im[dhretRight[7,l,m]],Re[ddhretRight[7,l,m]],Im[ddhretRight[7,l,m]],
Re[hretRight[2,l,m]],Im[hretRight[2,l,m]],Re[dhretRight[2,l,m]],Im[dhretRight[2,l,m]],Re[ddhretRight[2,l,m]],Im[ddhretRight[2,l,m]],
Re[hretRight[4,l,m]],Im[hretRight[4,l,m]],Re[dhretRight[4,l,m]],Im[dhretRight[4,l,m]],Re[ddhretRight[4,l,m]],Im[ddhretRight[4,l,m]]}],gridout},
{"Datasets",{"inhom_left","inhom_right","grid"}}];,{l,2,lmaxret},{m,l,0,-2}];


Table[
Export[FileNameJoin[{h1dir,"h1ret","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hretLeft[9,l,m]],Im[hretLeft[9,l,m]],Re[dhretLeft[9,l,m]],Im[dhretLeft[9,l,m]],Re[ddhretLeft[9,l,m]],Im[ddhretLeft[9,l,m]],
Re[hretLeft[10,l,m]],Im[hretLeft[10,l,m]],Re[dhretLeft[10,l,m]],Im[dhretLeft[10,l,m]],Re[ddhretLeft[10,l,m]],Im[ddhretLeft[10,l,m]],
Re[hretLeft[8,l,m]],Im[hretLeft[8,l,m]],Re[dhretLeft[8,l,m]],Im[dhretLeft[8,l,m]],Re[ddhretLeft[8,l,m]],Im[ddhretLeft[8,l,m]]}],Transpose[{
Re[hretRight[9,l,m]],Im[hretRight[9,l,m]],Re[dhretRight[9,l,m]],Im[dhretRight[9,l,m]],Re[ddhretRight[9,l,m]],Im[ddhretRight[9,l,m]],
Re[hretRight[10,l,m]],Im[hretRight[10,l,m]],Re[dhretRight[10,l,m]],Im[dhretRight[10,l,m]],Re[ddhretRight[10,l,m]],Im[ddhretRight[10,l,m]],
Re[hretRight[8,l,m]],Im[hretRight[8,l,m]],Re[dhretRight[8,l,m]],Im[dhretRight[8,l,m]],Re[ddhretRight[8,l,m]],Im[ddhretRight[8,l,m]]}],gridout},
{"Datasets",{"inhom_left","inhom_right","grid"}}];,{l,2,lmaxret},{m,l-1,0,-2}];


(* ::Text:: *)
(*l=1*)


Table[
Export[FileNameJoin[{h1dir,"h1ret","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hretLeft[1,l,m]],Im[hretLeft[1,l,m]],Re[dhretLeft[1,l,m]],Im[dhretLeft[1,l,m]],Re[ddhretLeft[1,l,m]],Im[ddhretLeft[1,l,m]],
Re[hretLeft[3,l,m]],Im[hretLeft[3,l,m]],Re[dhretLeft[3,l,m]],Im[dhretLeft[3,l,m]],Re[ddhretLeft[3,l,m]],Im[ddhretLeft[3,l,m]],
Re[hretLeft[5,l,m]],Im[hretLeft[5,l,m]],Re[dhretLeft[5,l,m]],Im[dhretLeft[5,l,m]],Re[ddhretLeft[5,l,m]],Im[ddhretLeft[5,l,m]],
Re[hretLeft[6,l,m]],Im[hretLeft[6,l,m]],Re[dhretLeft[6,l,m]],Im[dhretLeft[6,l,m]],Re[ddhretLeft[6,l,m]],Im[ddhretLeft[6,l,m]],
Re[hretLeft[2,l,m]],Im[hretLeft[2,l,m]],Re[dhretLeft[2,l,m]],Im[dhretLeft[2,l,m]],Re[ddhretLeft[2,l,m]],Im[ddhretLeft[2,l,m]],
Re[hretLeft[4,l,m]],Im[hretLeft[4,l,m]],Re[dhretLeft[4,l,m]],Im[dhretLeft[4,l,m]],Re[ddhretLeft[4,l,m]],Im[ddhretLeft[4,l,m]]}],Transpose[{
Re[hretRight[1,l,m]],Im[hretRight[1,l,m]],Re[dhretRight[1,l,m]],Im[dhretRight[1,l,m]],Re[ddhretRight[1,l,m]],Im[ddhretRight[1,l,m]],
Re[hretRight[3,l,m]],Im[hretRight[3,l,m]],Re[dhretRight[3,l,m]],Im[dhretRight[3,l,m]],Re[ddhretRight[3,l,m]],Im[ddhretRight[3,l,m]],
Re[hretRight[5,l,m]],Im[hretRight[5,l,m]],Re[dhretRight[5,l,m]],Im[dhretRight[5,l,m]],Re[ddhretRight[5,l,m]],Im[ddhretRight[5,l,m]],
Re[hretRight[6,l,m]],Im[hretRight[6,l,m]],Re[dhretRight[6,l,m]],Im[dhretRight[6,l,m]],Re[ddhretRight[6,l,m]],Im[ddhretRight[6,l,m]],
Re[hretRight[2,l,m]],Im[hretRight[2,l,m]],Re[dhretRight[2,l,m]],Im[dhretRight[2,l,m]],Re[ddhretRight[2,l,m]],Im[ddhretRight[2,l,m]],
Re[hretRight[4,l,m]],Im[hretRight[4,l,m]],Re[dhretRight[4,l,m]],Im[dhretRight[4,l,m]],Re[ddhretRight[4,l,m]],Im[ddhretRight[4,l,m]]}],gridout},
{"Datasets",{"inhom_left","inhom_right","grid"}}];,{l,{1}},{m,l,0,-2}];


Table[
Export[FileNameJoin[{h1dir,"h1ret","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hretLeft[9,l,m]],Im[hretLeft[9,l,m]],Re[dhretLeft[9,l,m]],Im[dhretLeft[9,l,m]],Re[ddhretLeft[9,l,m]],Im[ddhretLeft[9,l,m]],
Re[hretLeft[8,l,m]],Im[hretLeft[8,l,m]],Re[dhretLeft[8,l,m]],Im[dhretLeft[8,l,m]],Re[ddhretLeft[8,l,m]],Im[ddhretLeft[8,l,m]]}],Transpose[{
Re[hretRight[9,l,m]],Im[hretRight[9,l,m]],Re[dhretRight[9,l,m]],Im[dhretRight[9,l,m]],Re[ddhretRight[9,l,m]],Im[ddhretRight[9,l,m]],
Re[hretRight[8,l,m]],Im[hretRight[8,l,m]],Re[dhretRight[8,l,m]],Im[dhretRight[8,l,m]],Re[ddhretRight[8,l,m]],Im[ddhretRight[8,l,m]]}],gridout},
{"Datasets",{"inhom_left","inhom_right","grid"}}];,{l,{1}},{m,l-1,0,-2}];


(* ::Text:: *)
(*l=0*)


Table[
Export[FileNameJoin[{h1dir,"h1ret","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hretLeft[1,l,m]],Im[hretLeft[1,l,m]],Re[dhretLeft[1,l,m]],Im[dhretLeft[1,l,m]],Re[ddhretLeft[1,l,m]],Im[ddhretLeft[1,l,m]],
Re[hretLeft[3,l,m]],Im[hretLeft[3,l,m]],Re[dhretLeft[3,l,m]],Im[dhretLeft[3,l,m]],Re[ddhretLeft[3,l,m]],Im[ddhretLeft[3,l,m]],
Re[hretLeft[6,l,m]],Im[hretLeft[6,l,m]],Re[dhretLeft[6,l,m]],Im[dhretLeft[6,l,m]],Re[ddhretLeft[6,l,m]],Im[ddhretLeft[6,l,m]],
Re[hretLeft[2,l,m]],Im[hretLeft[2,l,m]],Re[dhretLeft[2,l,m]],Im[dhretLeft[2,l,m]],Re[ddhretLeft[2,l,m]],Im[ddhretLeft[2,l,m]]}],Transpose[{
Re[hretRight[1,l,m]],Im[hretRight[1,l,m]],Re[dhretRight[1,l,m]],Im[dhretRight[1,l,m]],Re[ddhretRight[1,l,m]],Im[ddhretRight[1,l,m]],
Re[hretRight[3,l,m]],Im[hretRight[3,l,m]],Re[dhretRight[3,l,m]],Im[dhretRight[3,l,m]],Re[ddhretRight[3,l,m]],Im[ddhretRight[3,l,m]],
Re[hretRight[6,l,m]],Im[hretRight[6,l,m]],Re[dhretRight[6,l,m]],Im[dhretRight[6,l,m]],Re[ddhretRight[6,l,m]],Im[ddhretRight[6,l,m]],
Re[hretRight[2,l,m]],Im[hretRight[2,l,m]],Re[dhretRight[2,l,m]],Im[dhretRight[2,l,m]],Re[ddhretRight[2,l,m]],Im[ddhretRight[2,l,m]]}],gridout},
{"Datasets",{"inhom_left","inhom_right","grid"}}];
,{l,{0}},{m,l,0,-2}];
