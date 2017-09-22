program generate_catalan_monoid;
{ Catalan Monoid Generator
  first release: 02/12/2016 05:16:38 PM
  updated: 01/10/2017 02:18:55 PM
  updated: 03/12/2017 09:18:11 PM
  S.V.Pantazi (svpantazi@gmail.com)}

uses sysutils,catalan_monoid_utils;

type

  TCatalanMonoidNode=record
    parent,
    gen_label: integer;
    idempotent: boolean;

    //optional
    func: 	TOPNDFunction;
    dyckw: TDyckWord;
  end;

{global variables}
var
  idempotent_node_count,
  index_g:integer;
  node_g: array of TCatalanMonoidNode;

function GetNodePath(idx:integer):ansistring;
 var
    p:integer;
 begin
   if idx=0 then Result:='0'
   else Result:='';
   p:=idx;
   while p>0 do
   begin
     Result:=IntToStr(node_g[p].gen_label)+Result;
     p:=node_g[p].parent;
   end;
 end;

procedure PrintASCIITree();
var
  depth:integer;
const
   //UTF-8 codes for extended ASCII
   //http://ascii-table.com/ascii-extended-pc-list.php
   //https://en.wikipedia.org/wiki/List_of_Unicode_characters
   FIRST_CHILD:UTF8String=#$2500#$252C#$2500;{#196#194#196}
   FIRST_CHILD_ONLY:UTF8String=#$2500#$2500#$2500;{#196#196#196}
   VERTICAL_LINE:UTF8String=#$00A0#$2502#$00A0;{#32#179#32}
   MIDDLE_CHILD:UTF8String=#$00A0#$251C#$2500;{#32#195#196}
   LAST_CHILD:UTF8String=#$00A0#$2514#$2500{#32#192#196};
   MAX_CHILD=1024;

  procedure PrintNode(idx:integer);
  var
     childCount,
     i,j,k: integer;
     childIdx:array[0..MAX_CHILD] of integer;
  begin
    Write(GetNodePath(idx));
    Inc(depth);
    childCount:=0;
    // node_g index is the index of the last node_g created
    for i:=0 to index_g do
    begin
      if node_g[i].parent=idx then
      begin
        childIdx[childCount]:=i;
        Inc(childCount);
      end;
    end;
    for i:=0 to childCount-1 do
    begin
      if i=0 then
      begin
        if childCount=1 then Write(FIRST_CHILD_ONLY)
        else Write(FIRST_CHILD);
      end
      else
      begin
        WriteLn();
        Write(' ');
        for j:=1 to depth-1 do
        begin
         Write(VERTICAL_LINE);
         for k:=1 to j do Write(' ');//make up for label size
        end;
        if i<childCount-1 then Write(MIDDLE_CHILD)
        else Write(LAST_CHILD);
      end;
      PrintNode(childIdx[i]);
    end;
    Dec(depth);
  end;

begin
  depth:=0;
  PrintNode(0);
  WriteLn();
end;

procedure PrintNodeList(tree_n:integer);
var
  i:integer;
begin
  WriteLn('Node list---------------------------------------------');
  WriteLn(#13#10'C(n)=C('+ IntToStr(tree_n+1)+')='+IntToStr(index_g+1));
  WriteLn('Idempotent count: ',idempotent_node_count);
  WriteLn('------------------------------------------------------');
  WriteLn('Index',#9,'id',#9,'Dyck word',#9,'func'#9#9,'idempotent?');
  WriteLn('------------------------------------------------------');
  for i:=0 to index_g do
  begin
   Write(i,#9,GetNodePath(i),#9);
   PrintDyckWord(node_g[i].dyckw);
   Write(#9);
   PrintOPNDFunc(node_g[i].func);
   if node_g[i].idempotent then WriteLn(#9,'    yes   ')
   else WriteLn();
  end;
  WriteLn('------------------------------------------------------');
end;

procedure PrintNodeLabel(parent_node_idx,node_label,child_node_idx:Integer);
begin
  PrintDyckPaths('                        ','  ------------------------------>   ',node_g[parent_node_idx].dyckw,node_g[child_node_idx].dyckw);
  Write(Format('%2d',[child_node_idx-1]),',     parent idx:',Format('%2d',[parent_node_idx]));
  PrintDyckWord(node_g[parent_node_idx].dyckw); Write('   right comp f=',node_label);
  Write(',   child idx:',Format('%2d',[child_node_idx]));
  PrintDyckWord(node_g[child_node_idx].dyckw);
  WriteLn();
  Write('                        ');
  PrintOPNDFunc(node_g[parent_node_idx].func);
  Write(#9#9#9#9'    ');
  PrintOPNDFunc(node_g[child_node_idx].func);
  WriteLn();
  WriteLn('                        ',GetNodePath(parent_node_idx),#9#9#9#9#9#9, GetNodePath(child_node_idx));
end;

procedure FUNCTION_COMPOSITION_OP();
var
  parent_idx:integer;
begin
  parent_idx:=node_g[index_g].parent;
 //before composition, the new function is just a copy of the parent word
  node_g[index_g].func{child}:=CopyOPNDFunc(node_g[parent_idx].func{parent});
  node_g[index_g].func[node_g[index_g].gen_label-1]:=node_g[index_g].func[node_g[index_g].gen_label];
end;

procedure DYCK_WORD_MULTIFLIP_OP();
var
  j:integer;
  valley_idx,parent_idx:integer;
begin
  parent_idx:=node_g[index_g].parent;
 //before any flip, the new (child) word is just a copy of the parent word
  node_g[index_g].dyckw{child}:=CopyDyckWord(node_g[parent_idx].dyckw{parent});
 //finds the valley located at the n-th 1 in the parent word
//this should be always true: 0 < valley_idx < n-1
  valley_idx:=0;
  j:=node_g[index_g].gen_label;
  repeat
    if (node_g[index_g].dyckw[valley_idx]=1) then Dec(j);
    if j>0 then Inc(valley_idx);
  until (j=0);
{checks to see if the new word will be idempotent}
  if node_g[parent_idx].idempotent and (node_g[index_g].dyckw[valley_idx-1]=0) then
  begin
    node_g[index_g].idempotent:=true;
    Inc(idempotent_node_count);
  end;
{the Dyck word modification of the new (child) word occurs at index_g in the node_g list;
transformation is through one or more flip-up operations, depending on the parent word}
  repeat
    node_g[index_g].dyckw[valley_idx]:=0;
    node_g[index_g].dyckw[valley_idx+1]:=1;
    Inc(valley_idx);
  until (node_g[index_g].dyckw[valley_idx+1]=1);
end;

procedure UPDATE_AND_DISPLAY_NODE(nodeIdx:integer);
begin
	Assert(nodeIdx>0,'Cannot update 0 node');
//optional for display
  FUNCTION_COMPOSITION_OP();
  DYCK_WORD_MULTIFLIP_OP();
  WriteLn();
//each node_g addition is a parent->child link in the resulting tree
  PrintNodeLabel(node_g[nodeIdx].parent,node_g[nodeIdx].gen_label,index_g);
end;


procedure Print_Tree(tree_n:Integer);
begin
  //optional for logging, display, debug
  WriteLn();
  WriteLn();
  PrintASCIITree();
  WriteLn();
  WriteLn('DONE generate tree T',tree_n,', current index=',index_g);
  PrintNodeList(tree_n);
end;

{When adding a vertex to the Tn tree, a parent Dyck word is transformed in a new child word of the same length through
a right composition with function f; the parent Dyck word undergoes one or more flip-up operations,
meaning the modification of a 10 (valley) into a 01 (mountain)}
function ADD_NODE(parent_node_idx,node_label:integer):integer;
begin
//advances index_g
  Inc(index_g);
//storing the entire path would be redundant; it can be inferred by looking at the parent valley and parent indices
  node_g[index_g].parent:=parent_node_idx;
  node_g[index_g].gen_label:= node_label;
  Result:=index_g;//returns the current index

//optional for logging, display and debug purposes
  UPDATE_AND_DISPLAY_NODE(index_g);
end;

procedure GENERATE_TREE(tree_n:integer);
var
  j,k:  integer;
  parentIdx:integer;
  parentLabel:NaturalNumber;
begin
  WriteLn('INIT generate tree T',tree_n,', current index=',index_g);
  if tree_n>0 then
  begin
   //R1 - generate T(n-1)
    GENERATE_TREE(tree_n-1);
   //after generating the tree T(n-1), there should be C(n) nodes in the vertex array
    Assert(index_g=catalan_monoid_utils.CalculateCatalanNumber(tree_n)-1,'there should be C_n nodes in T(n-1)');
   //R2 - attach new branch to every vertex in T(n-1)
    for j:=0 to index_g do
    begin
   //grow new branch from node with index j in T(n-1)
     	parentIdx:=j;
      parentLabel:=node_g[j].gen_label;
		//since this is a branch, the index of the parent node becomes the index of the node just added
      for k:=tree_n downto parentLabel+1 do parentIdx:=ADD_NODE(parentIdx,k);
    end;
  end;
  Print_Tree(tree_n);
end;

procedure GENERATE_MONOID(monoid_n:integer);

var
  nodeArraySize:dword;
  i:integer;
begin
  nodeArraySize:= CalculateCatalanNumber(monoid_n);
  SetLength(node_g,nodeArraySize);
  index_g:=0;

//T0
  node_g[index_g].parent:=-1;
  node_g[index_g].gen_label := 0;
  node_g[index_g].idempotent:=true;
  idempotent_node_count:=1;

//update root element dyck word and function
  node_g[index_g].dyckw:=NewTrivialDyckWord(monoid_n);
  node_g[index_g].func:=NewIdentityNDPF(monoid_n);

  Print_Tree(0);

  GENERATE_TREE(monoid_n-1);//generate tree Tn

{dynamic array finalization}
  WriteLn();
  for i:=LOW(node_g) to HIGH(node_g) do
  begin
	 	Finalize(node_g[i].dyckw);
  	Finalize(node_g[i].func);
  	Finalize(node_g[i]);
  end;
  Finalize(node_g);
end;


procedure Help(msg:string);
begin
  WriteLn('Usage: ',argv[0],' n',msg);
end;

var
  n:  Integer=4;
begin
  if argc=0 then Help('')
  else
  begin
    if TryStrToInt(argv[1],n) then GENERATE_MONOID(n)
    else
    begin
      Help(', where n is an integer > 0');
    end;
  end;
  WriteLn();
  WriteLn('Press enter to finish...');
  ReadLn();
end.


