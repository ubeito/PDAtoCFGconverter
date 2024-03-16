# seq[seq[string]] matrix representation of DPA for Json array
import tables, sugar, sequtils, strutils #, macros
type
  PDAmtx* = seq[seq[string]]
proc show*(mtx: PDAmtx) =
  for i, row in mtx:
    var line = "[" & $i & "]"
    for j, col in row:
      line &= " [" & $j & "]->" & col
    echo line
proc sshow*(mtx: PDAmtx): string =
  for i, row in mtx:
    var line = "[" & $i & "]"
    for j, col in row:
      line &= " [" & $j & "]->" & col
    result &= line & "\p" # \p: platform specific newline: CRLF on Windows, LF on Unix
proc newPDAmtx*(n: int): PDAmtx =  # same as newPDAmtx in PDAmtxSeqtoCFGmtrxSEep.nim
  for i in 0..<n:
    var row: seq[string]
    for j in 0..<n:
      row &= ""
    result &= row
proc makeIdentmtx*(n: int): PDAmtx = # add int arg makeIdentmtx in PDAmtxSeqtoCFGmtrxSEep.nim
  result = newPDAmtx(n)
  for i in 0..<n:
    for j in 0..<n:
      if i != j: result[i][j] = " " else: result[i][j] = "ε"
proc makeNpairmtx*(n: int): PDAmtx = # add int arg makeNpairmtx in PDAmtxSeqtoCFGmtrxSEep.nim
  result = newPDAmtx(n)
  for i in 0..<n:
    for j in 0..<n:
      result[i][j] = $i & $j
proc makeZeromtx*(n: int): PDAmtx = # to avoid to fold empty sequences
  result = newPDAmtx(n)
  for i in 0..<n:
    for j in 0..<n:
      result[i][j] = " "

type
  PDAmtxs* = tuple
    A: Table[string, PDAmtx]
    B: Table[string, PDAmtx]
    C: PDAmtx
    Gamma: seq[string]
    iniStates: seq[int]  
    accStates: seq[int]
proc show*(m: PDAmtxs) =
  for s in m.Gamma: echo "A[" & s & "]="; m.A[s].show
  for s in m.Gamma: echo "B[" & s & "]="; m.B[s].show
  echo "C="; m.C.show
  echo "initial states = ", m.iniStates
  echo "accepting states = ", m.accStates
proc sshow*(m: PDAmtxs): string = # \p: platform specific newline: CRLF on Windows, LF on Unix
  for s in m.Gamma: result &= "A[" & s & "]=\p" & m.A[s].sshow
  for s in m.Gamma: result &= "B[" & s & "]=\p" & m.B[s].sshow
  result &= "C=\p" & m.C.sshow
  result &= "initial states = " & $(m.iniStates) & "\p"
  result &= "accepting states = " & $(m.accStates) & "\p"
#[
A[Z0]=
[0] [0]->  [1]->ε [2]->  [3]-> 
[1] [0]->  [1]->  [2]->  [3]-> 
[2] [0]->  [1]->  [2]->  [3]-> 
[3] [0]->  [1]->ε [2]->  [3]-> 
A[X]=
[0] [0]->  [1]->  [2]->  [3]-> 
[1] [0]->  [1]->1 [2]->  [3]-> 
[2] [0]->  [1]->  [2]->  [3]-> 
[3] [0]->  [1]->  [2]->  [3]-> 
B[Z0]=
[0] [0]->  [1]->  [2]->  [3]-> 
[1] [0]->  [1]->ε [2]->  [3]-> 
[2] [0]->  [1]->  [2]->  [3]->0
[3] [0]->  [1]->  [2]->  [3]-> 
B[X]=
[0] [0]->  [1]->  [2]->  [3]-> 
[1] [0]->  [1]->  [2]->  [3]-> 
[2] [0]->  [1]->  [2]->1 [3]-> 
[3] [0]->  [1]->  [2]->  [3]-> 
C=
[0] [0]->  [1]->  [2]->  [3]-> 
[1] [0]->  [1]->  [2]->0 [3]-> 
[2] [0]->  [1]->  [2]->  [3]-> 
[3] [0]->  [1]->  [2]->  [3]-> 
initial states = @[0]
accepting states = @[1]
]#
#import CFGstrParenthExpand
import regex #import re replaced for js 
proc CFGstrParenExp*(b: string): string =
  #let b0 = "12*(0+1+ε)*21+(0+1+ε)*12*1*22+ε+1"
  var imap = initTable[string,string]()
  proc parenthtoTable(b: string): string =
    let iseq = b.findall(re"\([^\(\)]*\)").map(x => b[x.boundaries]) #replaced for js 
#    let iseq = b.findall(re"\([^\(\)]*\)") # @["(0+1+ε)", "(0+1+ε)"]. If it does not match, @[] is returned. 
    let oseq = b.split(re"\([^\(\)]*\)") # @["12*", "*21+", "*12*1*22+ε+1"]. "".split(re"\(") == @[]
    var b_P = "" # b => b_P = 12*_P0_*21+_P1_*12*1*22+ε+1
    if oseq != @[]: 
      b_P = oseq[0]
      for k, i in iseq:
        let Pk = "_P" & $k & "_"
        var Is = iseq[k]; Is.removePrefix("("); Is.removeSuffix(")")
        imap[Pk] = Is # {"_P0_": "0+1+ε", "_P1_": "0+1+ε"}
        b_P &= Pk & oseq[k+1]
    result = b_P
  proc `*`(S1: seq[string], S2: seq[string]): seq[string] =
    var prodseq: seq[string]
    for s1 in S1:
      for s2 in S2:
        var s = ""
        if s1 == "": s = s2 # either s2 = ε or s2 != ε
        else: # s1 != ""
          if s1 == "ε" and s2 != "ε": s = s2
          elif s1 != "ε" and s2 == "ε": s = s1
          # elif s1 == "ε" and s2 == "ε": s = "ε" # not occur
          else: s = s1 & "*" & s2
        prodseq &= @[s]
    result = prodseq
  proc `*`(S1: seq[string], s2: string): seq[string] =
    var S2: seq[string]
    if s2.match(re"_P\d+_"): S2 = imap[s2].split("+") # replaced for js
#    if s2 =~ re"_P\d+_": S2 = imap[s2].split("+") # @["0","1","ε"]
    else: S2 = @[s2] # @["12"]
    result = S1 * S2
  proc substituteTatP(Pstr: string): string =
    var sumseq: seq[string]
    for p in Pstr.split("+"): # @["12*_P0_*21", "_P1_*12*1*22", "ε", "1"]. "".split("+") == @[]
      var prodseq = @[""]
      for u in p.split("*"): # @["12", "_P0_", "21"]
        prodseq = prodseq * u # @["12*0*21", "12*1*21", "12*21", "0*12*1*22", "1*12*1*22", "12*1*22", "ε", "1"]
      sumseq &= prodseq
    result = sumseq.foldl(a & "+" & b)
  result = b.parenthtoTable.substituteTatP
proc `*`*(s1,s2: string): string =
  if s1 != " " and s2 != " ":
    if s1 == "ε" and s2 != "ε": result = s2
    elif s1 != "ε" and s2 == "ε": result = s1
    elif s1 == "ε" and s2 == "ε": result = "ε"
    else: result = ("(" & s1 & ")" & "*" & "(" & s2 & ")").CFGstrParenExp # s1=a+b,s2=c => s1 * s2 => ac+ab
  else: result = " "
proc `+`*(s1,s2: string): string =
  if s1 == " " and s2 == " ": result = " "
  elif s1 != " " and s2 == " ": result = s1
  elif s1 == " " and s2 != " ": result = s2
  else: result = s1 & "+" & s2
type
  CFGmtx* = PDAmtx
type
  Rulesflagged* = OrderedTable[Symbol, Flag_Bodies]
  Flag_Bodies* = tuple[flag: bool, bodies: seq[Body]]
  Body* = seq[Symbol]
  Symbol* = string
proc show*(rsf: Rulesflagged) =
  for k, v in rsf:
    var line = k & " [" & $v.flag & "]" & " -> "
    if v.bodies != @[]: # Can't fold empty sequences
      line &= v.bodies.map(bd => bd.foldl(a & " * " & b)).foldl(a & " | " & b)
    echo line
proc sshow*(rsf: Rulesflagged): string =
  for k, v in rsf:
    var line = k & " [" & $v.flag & "]" & " -> "
    if v.bodies != @[]: # Can't fold empty sequences
      line &= v.bodies.map(bd => bd.foldl(a & " * " & b)).foldl(a & " | " & b)
    result &= line & "\p" # \p: platform specific newline: CRLF on Windows, LF on Unix
type
  Symbkind = enum Variable, Terminal, EmptyString #, Null
proc kind(s: Symbol): Symbkind = # if s == "": result = Null # for "".split('+')
  if s.allCharsInSet(Digits) and s.len >= 2: result = Variable # s is a pair of non-neagtive integer
  elif s == "ε": result = EmptyString
  else: result = Terminal
proc toRulesflagged*(mtx: CFGmtx): Rulesflagged =
  result = initOrderedTable[Symbol, Flag_Bodies]()
  for i, row in mtx:
    for j, col in row:
      var BS: seq[Body] = @[]
      for b in col.split('+'): # col non => b = "", s = ""
        if b == "": break
        var B: Body = @[]
        for s in b.split('*'): # echo "s=" , s
          case s.kind # of Null: B = @[] # do nothing
          of Variable: B &= @["V" & s]
          else: B &= @[s]
        BS &= B
      result["V" & $i & $j] = (flag: false, bodies: BS)
#[
V00 [false] -> V11 * V10 | V12 * 0 * V30 | ε
V01 [false] -> V11 * V11 | V12 * 0 * V31
V02 [false] -> V11 * V12 | V12 * 0 * V32
V03 [false] -> V11 * V13 | V12 * 0 * V33
V10 [false] -> 1 * V12 * 1 * V20
V11 [false] -> 1 * V12 * 1 * V21 | ε
V12 [false] -> 1 * V12 * 1 * V22 | 0
V13 [false] -> 1 * V12 * 1 * V23
V20 [false] -> 
V21 [false] -> 
V22 [false] -> ε
V23 [false] -> 
V30 [false] -> V11 * V10 | V12 * 0 * V30
V31 [false] -> V11 * V11 | V12 * 0 * V31
V32 [false] -> V11 * V12 | V12 * 0 * V32
V33 [false] -> V11 * V13 | V12 * 0 * V33 | ε
]#
proc isVariable*(s: string): bool = #echo "avcd".isVariable # change arg Symbol to string for CFGtoDot
  result = s[0].isUpperAscii
#proc isTerminal(s: Symbol): bool = # not used
#  result = not s.isVariable and s != "ε"
proc containsVariable(b: Body): bool = #echo  @["V_11","b"].containsVariable
  result = b.any(proc (x: Symbol): bool = return x.isVariable)
proc isEmptyBody(b: Body): bool =
  result = b.len == 0
proc isGenerativeSymb(rsf: Rulesflagged, s: Symbol): bool = #echo grsf.isGenerativeSymb("V_00")
  result = rsf[s].flag == true  
proc isGenerativeBody(rsf: Rulesflagged, b: Body): bool = #echo grsf.isGenerativeBody(@["V_00","V_01","a"]) #(@["a","b"])
  if not b.containsVariable:
    result = not b.isEmptyBody
  else:
    result = b.filter(x => x.isVariable).all(proc (x: Symbol): bool = return rsf.isGenerativeSymb(x))
proc isGenerativeSeqB(rsf: Rulesflagged, sb: seq[Body]): bool = #echo grsf.isGenerativeSeqB(@[@["a","V_01"],@["V_02"]])
  result = sb.any(proc (X: Body): bool = return rsf.isGenerativeBody(X))
# fixed point computation
proc compGenerative*(rsf: var Rulesflagged) =  #ref => Error: type mismatch: got <ref Rulesflagged, Symbol>
  var isChanging = true
  while isChanging:
    isChanging = false
    for h, f_bs in rsf.pairs:
      if f_bs.flag == false and rsf.isGenerativeSeqB(f_bs.bodies):
        rsf[h].flag = true #'f_bs.flag' cannot be assigned to
        isChanging = true
#[
V00 [true] -> V11 * V10 | V12 * 0 * V30 | ε
V01 [true] -> V11 * V11 | V12 * 0 * V31
V02 [true] -> V11 * V12 | V12 * 0 * V32
V03 [true] -> V11 * V13 | V12 * 0 * V33
V10 [false] -> 1 * V12 * 1 * V20
V11 [true] -> 1 * V12 * 1 * V21 | ε
V12 [true] -> 1 * V12 * 1 * V22 | 0
V13 [false] -> 1 * V12 * 1 * V23
V20 [false] -> 
V21 [false] -> 
V22 [true] -> ε
V23 [false] -> 
V30 [false] -> V11 * V10 | V12 * 0 * V30
V31 [true] -> V11 * V11 | V12 * 0 * V31
V32 [true] -> V11 * V12 | V12 * 0 * V32
V33 [true] -> V11 * V13 | V12 * 0 * V33 | ε
]#
proc delNongeneBody*(rsf: Rulesflagged) : Rulesflagged = #var => cannot be captured as it would violate memory safety
  var newrsf: Rulesflagged = initOrderedTable[Symbol, Flag_Bodies]()
  for k, v in rsf.pairs:
    newrsf[k] = (v.flag, v.bodies.filter(x => rsf.isGenerativeBody(x)))
  result = newrsf
#        rsf[k].bodies.del(i) #the length of the seq changed while iterating 
#[
V00 [true] -> ε
V01 [true] -> V11 * V11 | V12 * 0 * V31
V02 [true] -> V11 * V12 | V12 * 0 * V32
V03 [true] -> V12 * 0 * V33
V10 [false] -> 
V11 [true] -> ε
V12 [true] -> 1 * V12 * 1 * V22 | 0
V13 [false] -> 
V20 [false] -> 
V21 [false] -> 
V22 [true] -> ε
V23 [false] -> 
V30 [false] -> 
V31 [true] -> V11 * V11 | V12 * 0 * V31
V32 [true] -> V11 * V12 | V12 * 0 * V32
V33 [true] -> V12 * 0 * V33 | ε
]#
proc delNongeneSymb*(rsf: Rulesflagged) : Rulesflagged = 
  var newrsf: Rulesflagged = initOrderedTable[Symbol, Flag_Bodies]()
  for k, v in rsf.pairs:
    if v.flag == true:
       newrsf[k] = (v.flag, v.bodies)
  result = newrsf
#[
V00 [true] -> ε
V01 [true] -> V11 * V11 | V12 * 0 * V31
V02 [true] -> V11 * V12 | V12 * 0 * V32
V03 [true] -> V12 * 0 * V33
V11 [true] -> ε
V12 [true] -> 1 * V12 * 1 * V22 | 0
V22 [true] -> ε
V31 [true] -> V11 * V11 | V12 * 0 * V31
V32 [true] -> V11 * V12 | V12 * 0 * V32
V33 [true] -> V12 * 0 * V33 | ε
]#
proc isTerminal*(s: Symbol): bool =
  result = not s.isVariable and s != "ε"
proc isEpshilonSymb*(rsf: Rulesflagged, s: Symbol): bool =
  result = rsf[s].bodies == @[@["ε"]]
####################
#[
Internal epsilon-contraced CFG rules form:
V00 [true] -> ε
V01 [true] -> ε | V12 * 0 * V31
V02 [true] -> V12 | V12 * 0 * V32
V03 [true] -> V12 * 0 * V33
V11 [true] -> ε
V12 [true] -> 1 * V12 * 1 | 0
V22 [true] -> ε
V31 [true] -> ε | V12 * 0 * V31
V32 [true] -> V12 | V12 * 0 * V32
V33 [true] -> V12 * 0 * V33 | ε
]#
proc setflagsfalse*(rsf: var Rulesflagged): Rulesflagged =
  var newrsf: Rulesflagged = initOrderedTable[Symbol, Flag_Bodies]()
  for k, v in rsf.pairs:
    newrsf[k] = (false, v.bodies)
  result = newrsf
#[
V00 [false] -> ε
V01 [false] -> ε | V12 * 0 * V31
V02 [false] -> V12 | V12 * 0 * V32
V03 [false] -> V12 * 0 * V33
V11 [false] -> ε
V12 [false] -> 1 * V12 * 1 | 0
V22 [false] -> ε
V31 [false] -> ε | V12 * 0 * V31
V32 [false] -> V12 | V12 * 0 * V32
V33 [false] -> V12 * 0 * V33 | ε
]#
proc startSyms*(iniSs, accSs: seq[int]): seq[Symbol] =
  for i in iniSs:
    for a in accSs:
      result &= "V" & $i & $a
proc DFSfrom*(rsf: var Rulesflagged, start: Symbol) =
  if rsf[start].flag == true: return
  rsf[start].flag = true # else
  for b in rsf[start].bodies:
    for s in b.filter(x => x.isVariable):
      rsf.DFSfrom(s)
proc delFalseflagSymb*(rsf: Rulesflagged) : Rulesflagged = 
  var newrsf: Rulesflagged = initOrderedTable[Symbol, Flag_Bodies]()
  for k, v in rsf.pairs:
    if v.flag == true:
       newrsf[k] = (v.flag, v.bodies)
  result = newrsf
#[
V01 [true] -> ε | V12 * 0 * V31
V12 [true] -> 1 * V12 * 1 | 0
V31 [true] -> ε | V12 * 0 * V31
]#
type
  Rules* = OrderedTable[Symbol, Bodies]
  Bodies* = seq[Body]
proc show*(rs: Rules) =
  for k, v in rs:
    var line = k & " -> "
    if v != @[]: # Can't fold empty sequences
      line &= v.map(bd => bd.foldl(a & " * " & b)).foldl(a & " | " & b)
    echo line
proc sshow*(rs: Rules): string =
  for k, v in rs:
    var line = k & " -> "
    if v != @[]: # Can't fold empty sequences
      line &= v.map(bd => bd.foldl(a & " * " & b)).foldl(a & " | " & b)
    result &= line & "\p" # \p: platform specific newline: CRLF on Windows, LF on Unix
proc numVartoAlphaVar*(rsf: Rulesflagged, ssyms: seq[Symbol]): Rules = # change ssyms from gloval to arg var
#  echo "start states= ", ssyms
  result = initOrderedTable[Symbol, Bodies]()
  var oldnew = initTable[Symbol, Symbol]()
  var oldSyms = toSeq(rsf.keys) #(a.keys).toSeq => Error: undeclared field: 'keys' 
#  echo "old symbols = ", oldSyms
#  let AlphSet = {'A'..'Z'} - {'S'}
  let alphSymSet = @["A","B","C","D","E","F","G","H","I","J","K","L",
                     "M","N","O","P","Q","R","T","U","V","W","X","Y","Z"]
  if ssyms.len == 0:
    for i, s in oldSyms: oldnew[s] = alphSymSet[i] # $(('A'.ord + i).chr)
  elif ssyms.len == 1:
    oldnew[ssyms[0]] = "S"
    oldSyms = oldSyms.filter(s => s notin ssyms)
    for i, s in oldSyms: oldnew[s] = alphSymSet[i]
  else: # elif ssyms.len > 2
    var bs: Bodies # seq[seq[string]]
    for i, s in ssyms:
      oldnew[s] = "S" & $(i+1) # S1,S2,...
      bs &= @[@["S" & $(i+1)]]
    result["S"] = bs
    oldSyms = oldSyms.filter(s => s notin ssyms)
    for i, s in oldSyms: oldnew[s] = alphSymSet[i]
#  echo "old new = ", oldnew
  for k, v in rsf.pairs:
#    result[k] = v.bodies.map(b => b.filter(s => s.isVariable).map(s => oldnew[s]))
    var newbs: seq[seq[string]]
    for b in v.bodies:
      var newb: seq[string]
      for s in b:
        if s.isVariable: newb &= oldnew[s]
        else: newb &= s
      newbs &= newb
    result[oldnew[k]] = newbs
#  echo result
#[
S -> ε | A * 0 * B
A -> 1 * A * 1 | 0
B -> ε | A * 0 * B
]#
import algorithm
proc symCmp(x, y: tuple[k: Symbol, v: Bodies]): int = # not in CFGreachableVars.nim
  if (x.k).startsWith("S") and (y.k).startsWith("S"): return 0
  elif (x.k).startsWith("S") and not (y.k).startsWith("S"): return -1
  elif not (x.k).startsWith("S") and (y.k).startsWith("S"): return 1
  elif not (x.k).startsWith("S") and not (y.k).startsWith("S"): return 0
proc sortRules*(rs: Rules): Rules = # not in CFGreachableVars.nim
  result = initOrderedTable[Symbol, Bodies]()
  var tmp: seq[tuple[k: Symbol, v: Bodies]]
  for k, v in rs: tmp.add((k, v))
  tmp.sort(symCmp)
  for x in tmp: result[x.k] = x.v
#[
S -> ε | A * 0 * B
A -> 1 * A * 1 | 0
B -> ε | A * 0 * B
]#
proc sortBodies*(rs: Rules): Rules =
  result = initOrderedTable[Symbol, Bodies]()
  for k, bs in rs:
    var newbs = bs
    newbs.sort(proc (x,y: Body): int = 
                  if x.len < y.len: 1 else: -1)
    result[k] = newbs
#[
S -> A * 0 * B | ε
A -> 1 * A * 1 | 0
B -> A * 0 * B | ε
]#
proc extractVerTer*(rs: Rules): (seq[string], seq[string]) =
  let V = toSeq(rs.keys)
  let bodies = toSeq(rs.values)
  var T: seq[string]
  for bs in bodies:
    for b in bs:
      for s in b:
        if (s notin V) and s != "ε": T &= s
  result = (V, T.deduplicate) # (isSorted = true) => remain dumpulicated bug!
type
  CFG* = tuple
    V: seq[Symbol]
    T: seq[Symbol]
    # Rules => json to CFG error # Table[Symbol, Bodies] => JSON doesn't support keys of type Symbol
    P: OrderedTable[string, seq[seq[string]]]
    S: seq[Symbol] # for multiple start symbols
proc show*(cfg: CFG) =
  echo "V={" & cfg.V.foldl(a & "," & b) & "}"
  echo "T={" & cfg.T.foldl(a & "," & b) & "}"
  echo "P={"
  for k, v in cfg.P:
    var line = "    " & k & " -> "
    if v != @[]: # Can't fold empty sequences
      line &= v.map(bd => bd.foldl(a & " * " & b)).foldl(a & " | " & b)
    echo line
  echo "  }"
  echo "S={" & cfg.S.foldl(a & "," & b) & "}"
proc sshow*(cfg: CFG): string =
  result &= "V={" & cfg.V.foldl(a & "," & b) & "}" & "\p"
  result &= "T={" & cfg.T.foldl(a & "," & b) & "}" & "\p"
  result &= "P={" & "\p"
  for k, v in cfg.P:
    var line = "    " & k & " -> "
    if v != @[]: # Can't fold empty sequences
      line &= v.map(bd => bd.foldl(a & " * " & b)).foldl(a & " | " & b)
    result &= line & "\p"
  result &= "  }" & "\p"
  result &= "S={" & cfg.S.foldl(a & "," & b) & "}" & "\p"
proc startSyms*(rs: Rules): seq[Symbol] = # not in RulestoCFG.nim
  for k in rs.keys:
    if k.startsWith("S"): result.add(k)
#[
V={S,A,B}
T={0,1}
P={
    S -> A * 0 * B | ε
    A -> 1 * A * 1 | 0
    B -> A * 0 * B | ε
  }
S={S}
]#
proc longestBody(gr: CFG): int =
  result = 0
  for v in gr.V:
    for b in gr.P[v]:
      result = if b.len > result: b.len else: result
proc countBodies(gr: CFG): int =
  result = 0
  for v in gr.V:
    for b in gr.P[v]:
      result += 1
#import strutils # for align, for parseInt
type Grpmtx* = seq[seq[string]]
proc newGrpmtx(m, n: int): Grpmtx =
  for i in 0..<m:
    var row: seq[string]
    for j in 0..<n:
      row &= ""
    result &= row
proc sizes*(mtx: Grpmtx): (int, int) =
  result = (mtx.len, mtx[0].len)
const maruMax = 49 # max number of CFGed PDA states to be drawn in graphviz image
proc makeGrpmtx*(gr: CFG): Grpmtx =
  let maxsklnum = countBodies(gr) # echo maxsklnum
  let maxgrnum = longestBody(gr) + 1 # echo maxgrnum
  let m = maxsklnum + 1; let n = maxgrnum + 1
  result = newGrpmtx(m, n)
  for j in 0..<n: result[0][j] &= "g" & $j # group name
  var q = 0 # state index
  var i = 1 # current row
  for v in gr.V:
    for nb1, b in gr.P[v]: # nb1 = number of bodies of each rule - 1
      if nb1 == 0: # first skelton 
        result[i][0] = v # title head symbol of each rule automaton
        result[i][1] = $q; inc(q) # initial state of automaton
        for j, s in b:
          result[i][j + 2] = $q; inc(q)
      else: # 2nd or later skelton
        for j, s in b:
          result[i][j + 2] = $q; inc(q)
      inc(i)
      if q > maruMax: raise newException(IOError, "too many states in CFG transition diagram to draw")
proc findAccIndex*(row: seq[string]): int = # accepting state = last non-empty component
  for j, s in row:
    if s != "": result = j
#proc isVariable*(s: string): bool =
#  result = s[0].isUpperAscii
proc makeHeadtable*(gm: Grpmtx): auto =
  result = initTable[string, tuple[hr: int, aj: int]]()
  for i, row in gm:
    if row[0] != "": #.isVariable: #the container is empty
      let aj = findAccIndex(row)
      result[row[0]] = (i, aj)
proc makeElblmtx*(gm: Grpmtx, gr: CFG): Grpmtx =
  let (m, n) = gm.sizes
  result = newGrpmtx(m, n)
  var headrow: int; var head: string
  for i in 1..<m:
    let aj = findAccIndex(gm[i])
    if gm[i][0] != "": # 1st skelton row
      headrow = i; head = gm[i][0]
      result[i][0] = head # head symbol
      for j in 2..aj:
        result[i][j] = gr.P[head][0][j - 2]
    else: # 2nd or later skelton row
      for j in 2..aj:
        result[i][j] = gr.P[head][i - headrow][j - 2]
const maru*: Table[int, string] =
  {0:"⓪",1:"①",2:"②",3:"③",4:"④",5:"⑤",6:"⑥",7:"⑦",8:"⑧",9:"⑨",
   10:"⑩",11:"⑪",12:"⑫",13:"⑬",14:"⑭",15:"⑮",16:"⑯",17:"⑰",18:"⑱",19:"⑲",
   20:"⑳",21:"21",22:"22",23:"23",24:"24",25:"25",26:"26",27:"27",28:"28",29:"29",
   30:"30",31:"31",32:"32",33:"33",34:"34",35:"35",36:"36",37:"37",38:"38",39:"39", # added 
   40:"40",41:"41",42:"42",43:"43",44:"44",45:"45",46:"46",47:"47",48:"48",49:"49", # added
   50:"50",51:"51",52:"52",53:"53",54:"54",55:"55",56:"56",57:"57",58:"58",59:"59", # added
   60:"60",61:"61",62:"62",63:"63",64:"64",65:"65",66:"66",67:"67",68:"68",69:"69", # added
   70:"70",71:"71",72:"72",73:"73",74:"74",75:"75",76:"76",77:"77",78:"78",79:"79", # added
   80:"80",81:"81",82:"82",83:"83",84:"84",85:"85",86:"86",87:"87",88:"88",89:"89", # added
   90:"90",91:"91",92:"92",93:"93",94:"94",95:"95",96:"96",97:"97",98:"98",99:"99" # added
   }.toTable



when isMainModule:
  let ms: PDAmtxs = (A: {"Z0": @[@[" ", " ", " "],
                                @[" ", "a", " "],
                                @[" ", " ", " "]],
                         "X": @[@[" ", "a", " "],
                              @[" ", " ", " "],
                              @[" ", " ", " "]]}.toTable,
                    B: {"Z0": @[@[" ", " ", " "],
                              @[" ", "b", " "],
                              @[" ", " ", " "]],
                        "X": @[@[" ", " ", " "],
                              @["b", " ", " "],
                              @[" ", " ", " "]]}.toTable,
                    C:        @[@[" ", " ", "b"],
                              @[" ", " ", " "],
                              @[" ", " ", " "]],
                    Gamma: @["Z0","X"],
                    iniStates: @[0],
                    accStates: @[1]) # .toTable non ==> Error: type mismatch
#  ms.show
#  echo ms.sshow
  let mI = makeIdentmtx(3)
  let mX = makeNpairmtx(3)
  const gmtx: CFGmtx = @[@["11*10+12*0*30+ε", "11*11+12*0*31", "11*12+12*0*32", "11*13+12*0*33"],
                       @["1*12*1*20", "1*12*1*21+ε", "1*12*1*22+0", "1*12*1*23"],
                       @["", "", "ε", ""],
                       @["11*10+12*0*30", "11*11+12*0*31", "11*12+12*0*32", "11*13+12*0*33+ε"]]
#  gmtx.show
#  echo ""
  var grsf: Rulesflagged = gmtx.toRulesflagged
#  echo grsf.sshow & "\p"
  grsf = grsf.setflagsfalse
#  echo grsf.sshow & "\p"
  grsf.compGenerative
#  echo grsf.sshow & "\p"
  grsf = grsf.delNongeneBody
#  echo grsf.sshow & "\p"
  grsf = grsf.delNongeneSymb
#  echo grsf.sshow & "\p"
#  grsf.compEpshilonContract
#  echo grsf.sshow & "\p"
  #############################
  grsf = grsf.setflagsfalse
#  echo grsf.sshow & "\p"
  let ssyms = startSyms(ms.iniStates, ms.accStates).filter(s => grsf.hasKey(s)) ##  if not hasKey: delete start state
#  echo "start states = ", ssyms
#  @["V00", "V01"] => @["V00"]
  for st in ssyms: grsf.DFSfrom(st)
#  echo grsf.sshow & "\p"
  grsf = grsf.delFalseflagSymb
#  echo grsf.sshow & "\p"
  var grs = grsf.numVartoAlphaVar(ssyms)
#  echo grs.sshow & "\p"
  grs = grs.sortRules # not in CFGreachableVars.nim
#  echo grs.sshow & "\p"
  grs = grs.sortBodies
#  echo grs.sshow & "\p"
  let (grsV, grsT) = extractVerTer(grs)
  let gcfg: CFG =
    (
      V: grsV,
      T: grsT,
      P: grs,
      S: startSyms(grs) # @[" (unfixed) "]
    )
  gcfg.show

  let grm = gcfg.makeGrpmtx
  echo grm.sshow & "\p"
  let headtbl = makeHeadtable(grm)
  let grmlbl = grm.makeElblmtx(gcfg)




