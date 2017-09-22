{$codepage utf8}

unit catalan_monoid_utils;
{ Catalan Monoid Generator, Utils
  first release: 02/12/2016 05:16:38 PM
  updated: 01/10/2017 02:18:55 PM
  updated: 03/12/2017 09:18:11 PM

}

{$mode objfpc}{$H+}

interface

type
  BinaryDigit=0..1;
  NaturalNumber=QWord;
  TDyckWord= array of BinaryDigit;
  TOPNDFunction=array of NaturalNumber;
  TDyckPath= array of string;

const
  PADDING_SPACE=' ';
  DYCK_PATH_CHAR:array[BinaryDigit] of ansistring=('/','\');

{Calculates C(n) using recurrence relation}
function CalculateCatalanNumber(n:integer):integer;

{Creates the trivial word 010101...01}
function NewTrivialDyckWord(const semilength:NaturalNumber):TDyckWord;
function NewIdentityNDPF(const length:NaturalNumber):TOPNDFunction;//identity function

{Copies the source word into the destination word. Clearly the words must have the same semi-length}
function CopyDyckWord(var srcWord:TDyckWord):TDyckWord;
function CopyOPNDFunc(var srcFunc:TOPNDFunction):TOPNDFunction;

{Outputs the Dyck word as a binary digit}
procedure PrintDyckWord(var dw:TDyckWord);
procedure PrintOPNDFunc(var func:TOPNDFunction);

procedure PrintDyckPaths(prefix,diafix:ansistring; var dw1,dw2:TDyckWord);

implementation

function NewIdentityNDPF(const length:NaturalNumber):TOPNDFunction;//identity function
var
	j:NaturalNumber;
begin
  SetLength(Result,length);
  for j:=LOW(Result)+1 to HIGH(Result)+1 do Result[j-1]:=j;
end;

function NewTrivialDyckWord(const semilength:NaturalNumber):TDyckWord;
var
  i:integer;
begin
  SetLength(Result,2*semilength);
  for i:=0 to semilength-1 do
  begin
    Result[2*i]:=0;
    Result[2*i+1]:=1;
  end;
end;

function CopyOPNDFunc(var srcFunc:TOPNDFunction):TOPNDFunction;
var
  i:integer;
begin
  SetLength(Result,Length(srcFunc));
  for i:=LOW(srcFunc) to HIGH(srcFunc) do Result[i]:=srcFunc[i]
end;

function CopyDyckWord(var srcWord:TDyckWord):TDyckWord;
var
  i:integer;
begin
  SetLength(Result,Length(srcWord));
  for i:=LOW(srcWord) to HIGH(srcWord) do Result[i]:=srcWord[i]
end;

procedure PrintDyckWord(var dw:TDyckWord);
var
  i:integer;
begin
  Write('   ');
  for i:=LOW(dw) to HIGH(dw) do Write(dw[i]);
end;

procedure PrintOPNDFunc(var func:TOPNDFunction);
var
  i:integer;
begin
  Write('[');
  for i:=LOW(func) to HIGH(func)-1 do Write(func[i],',');
  Write(func[HIGH(func)],']');
end;

function MakeDyckPath(var dw:TDyckWord):TDyckPath;
var
  i,j,k:integer;
begin
  SetLength(Result,Length(dw) div 2);
  for j:=HIGH(Result) downto LOW(Result) do Result[j]:='';
  j:=LOW(Result);
  for i:=LOW(dw) to HIGH(dw) do
  begin
    if dw[i]=0 then
    begin
      if (i>LOW(dw)) and (dw[i-1]=0) then
      begin
        Inc(j);
        for k:=Length(Result[j]) to i-1 do Result[j]+=PADDING_SPACE;
      end;
    end
    else
    begin
      if (i>Low(dw)) and (dw[i-1]=1) then
      begin
        Dec(j);
        for k:=Length(Result[j]) to i-1 do Result[j]+=PADDING_SPACE;
      end;
    end;
    Result[j]+=DYCK_PATH_CHAR[dw[i]];
  end;
end;

procedure PrintDyckPaths(prefix,diafix:ansistring; var dw1,dw2:TDyckWord);
var
  i,j:integer;
  path1,path2:TDyckPath;
begin
  path1:=MakeDyckPath(dw1);
  path2:=MakeDyckPath(dw2);
  for i:=HIGH(path1) downto LOW(path1) do
  begin
    Write(prefix,path1[i]);
    if i>0 then for j:=1 to Length(path1[0])+Length(diafix)-Length(path1[i]) do Write(PADDING_SPACE)
    else Write(diafix);
    WriteLn(path2[i]);
  end;
  Finalize(path1);
  Finalize(path2);
end;

function CalculateCatalanNumber(n:integer):integer;
var
  k:  integer;
begin
  if n=0 then Result:=1
  else if n=1 then Result:=1
  else
  begin
    Result:=0;
    for k:=1 to n do
    begin
      Result+=CalculateCatalanNumber(k-1)*CalculateCatalanNumber(n-k);
    end;
  end;
end;

end.

