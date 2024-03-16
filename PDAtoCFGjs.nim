# nim js PDAtoCFGjs.nim => PDAtoCFGjs.js
import tables, sugar, sequtils, strutils #, osproc
import PDA2CFG_separated_re # import PDA2CFG_separated replaced for js
import json

let tPDAmtxs = """
{
    "A": {"Z0": [[" ", "ε", " ", " "],
                 [" ", " ", " ", " "],
                 [" ", " ", " ", " "],
                 [" ", "ε", " ", " "]],
          "X" : [[" ", " ", " ", " "],
                 [" ", "1", " ", " "],
                 [" ", " ", " ", " "],
                 [" ", " ", " ", " "]]},
    "B": {"Z0": [[" ", " ", " ", " "],
                 [" ", "ε", " ", " "],
                 [" ", " ", " ", "0"],
                 [" ", " ", " ", " "]],
          "X":  [[" ", " ", " ", " "],
                 [" ", " ", " ", " "],
                 [" ", " ", "1", " "],
                 [" ", " ", " ", " "]]},
    "C":        [[" ", " ", " ", " "],
                 [" ", " ", "0", " "],
                 [" ", " ", " ", " "],
                 [" ", " ", " ", " "]],
    "Gamma": ["Z0","X"],
    "iniStates": [0],
    "accStates": [1]
}
"""

var N: int # PDA matrix size
var jfname: string
var gvsize = "6,6" #"7.5,7.5" # 最大幅＆高さ（インチ, 最大"7.75,10.25"）
var gvnodesep1 = "0.4" #0.5" # 1.00" # ノード間の最少間隔
var gvranksep1 = "0.6" #1.5" #2.00" # 異なるランクのノード間の最少距離
var gvnodesep2 = "0.4" #0.5" # 1.00" # ノード間の最少間隔
var gvranksep2 = "0.6" #1.5" #2.00" # 異なるランクのノード間の最少距離
var m: PDAmtxs
var gcfg: CFG

#window.width = 850.scaleToDpi # 640
#window.height = 800.scaleToDpi # 480
proc PDAjsontoPDAmtx(readresult: cstring): cstring {.exportc.} = # PDAjsontoPDAmtx(reader.result)
  let pdamtxs: string = $readresult # OK with strMw = cstrMw?
  var textarea = ""
  m = parseJson(pdamtxs).to(PDAmtxs)
  textarea &= "Internal PDA matrices:" & "\p"  # \p: platform specific newline: CRLF on Windows, LF on Unix
  textarea &= m.sshow & "\p"
  return textarea
echo PDAjsontoPDAmtx(tPDAmtxs)

proc toPDAgv(): cstring {.exportc.} = #
  N = m.C.len # m already exists once PDAjsontoPDAmtx called
  ################################
  ## content of PDAmtxtoDot.nim ##
  ################################
  let accStatesStr = (m.accStates.map(x => "q" & $x & ";") & @["\n"]).foldl(a & b) # can't fold empty sequences
  let toIniStatesStr = (m.iniStates.map(x => "s" & $x & ";") & @["\n"]).foldl(a & b)
  let toIniArrlines = m.iniStates.map(x => "  s" & $x & " -> " & "q" & $x & " [ label = \"\" ];\n")
  var edgeArrlines = ""
  for i in 0..N-1:
    for j in 0..N-1:
      var labelElems: seq[string]
      for s in m.Gamma:
        let ss = s.string # Error: 's' is of type <lent TaintedString> which cannot be captured as it would violate memory safety
        labelElems &= m.A[s][i][j].split("+").filter(x => not x.allCharsInSet(Whitespace)).map(x => x & "/" & ss)
        labelElems &= m.B[s][i][j].split("+").filter(x => not x.allCharsInSet(Whitespace)).map(x => x & "," & ss & "/")
      labelElems &= m.C[i][j].split("+").filter(x => not x.allCharsInSet(Whitespace))
      if labelElems != @[]:
        let labelStr = labelElems.foldl(a & "\\n" & b)
        edgeArrlines &= "  q" & $i & " -> " & "q" & $j & " [ label = \"" & labelStr & "\" ];\n"
  let dotstr: string = """
digraph PDAmtxs {
  rankdir=LR;
	size = """" & gvsize & """"
  nodesep = """ & gvnodesep1 & """
  ranksep = """ & gvranksep1 & """
  node [shape = doublecircle]; """ & accStatesStr & """
  node [shape = point, height = 0.0001]; """ & toIniStatesStr & """
  node [shape = circle]; """ & "\n" &
  toIniArrlines.foldl(a & b) & # Error: type mismatch: got 'seq[string]' => .foldl(a & b)
  edgeArrlines & """
}"""
  return dotstr
echo toPDAgv()

#[
let err = execCmd("dot -Tpng -Gdpi=200 " & jfname & ".gv -o" & jfname & ".png")
  textArea.addLine("Converted to png file: " & jfname & ".png" & "\p")
  pwindow.width = pimage.width
  pwindow.height = pimage.height + 25 # 20 = window title approx height
    pcanvas.drawImage(pimage) #, 0, 0, pimage.width, pimage.height) #, 10, 120)
]#

proc toCFGtxt(): cstring {.exportc.} = #
  #########################################
  ## content of PDAmtxSeqtoCFGmtxSeq.nim ##
  #########################################
  proc `*`(m1,m2: PDAmtx): PDAmtx =
    result = newPDAmtx(N)
    for i in 0..<N:
      for j in 0..<N:
        result[i][j] = " "
        for k in 0..<N:
          result[i][j] = result[i][j] + (m1[i][k] * m2[k][j])
# show ms.A["Z0"] * mX
  proc `+`(m1,m2: PDAmtx): PDAmtx =
    result = newPDAmtx(N)
    for i in 0..<N:
      for j in 0..<N:
        result[i][j] = m1[i][j] + m2[i][j]
# show ms.C + mI
  let mI = makeIdentmtx(N)
  let mX = makeNpairmtx(N)
  let mZ = makeZeromtx(N) # starting para in foldl: even if A = B = zero natrices (<=> m.Gamma == @[]) foldl does not error
  var mXrhs = (m.Gamma.map(s => m.A[s] * mX * m.B[s]).foldl(a + b, mZ) + m.C) * mX + mI # <= m.Gamma.map(s => m.A[s] * mX * m.B[s] * mX).foldl(a + b) + m.C + mI: simplified ANF
#  @[" ", " ", "ε", " "],
#  echo mXrhs
  proc whitestoDoubleQuotes(m: var PDAmtx): PDAmtx =
    result = newPDAmtx(N)
    for i in 0..<N:
      for j in 0..<N:
        if m[i][j].contains("+") and not (m[i][j].startsWith("(") and m[i][j].endsWith(")")):
          result[i][j] = "(" & m[i][j] & ")" else: result[i][j] = m[i][j]
        if m[i][j].allCharsInSet(Whitespace): result[i][j] = "" else: result[i][j] = m[i][j]
  mXrhs = mXrhs.whitestoDoubleQuotes
# echo mXrhs
#  @["", "", "ε", ""],
#  mXrhs.show
  var textarea = ""
  textarea &= "Internal multiplied & added CFG matrix form:" & "\p"
  textarea &= mXrhs.sshow & "\p"
  ############################################
  ## content of CFGmtxSeqtoRulesflagged.nim ##
  ############################################
  var grsf: Rulesflagged = mXrhs.toRulesflagged
#  echo grsf.sshow
  textarea &= "Internal flag-appended CFG matrix form:" & "\p"
  textArea &= grsf.sshow & "\p"
  ####################################
  ## content of CFGtoGenerative.nim ##
  ####################################
  grsf.compGenerative
  textarea &= "Internal generative-flagged CFG matrix form:" & "\p"
  textarea &= grsf.sshow & "\p"
  grsf = grsf.delNongeneBody
  textarea &= "Internal generative-body only CFG rules form:" & "\p"
  textarea &= grsf.sshow & "\p"
  grsf = grsf.delNongeneSymb
  textarea &= "Internal generative-symbol only CFG rules form:" & "\p"
  textarea &= grsf.sshow & "\p"
  ######################################
  ## content of GenetoEpshiContra.nim ##
  ######################################
#echo "fixed point computation"
#proc compEpshilonContract(rsf: var Rulesflagged) = # Error: 'rsf' is of type <var Rulesflagged> which cannot be captured as it would violate memory safety
  var isChanging = true
  while isChanging:
    isChanging = false
    var newrsf: Rulesflagged = initOrderedTable[Symbol, Flag_Bodies]()
    for h, fbs in grsf.pairs: # contract h's bodies with ε-rules
      var newfbs: Flag_Bodies
      for b in fbs.bodies: # substitute bodies' variables with ε's & simplify bodies
        var newb = b.filter(x => x.isTerminal or x == "ε" or not grsf.isEpshilonSymb(x)) # X->αYβ, Y->ε => X->αβ
        if newb == @[]: newb = @["ε"] # all rules are generative forms => each body is not empty
        newfbs.bodies &= @[newb]
      if newfbs.bodies.count(@["ε"]) >= 2:
        newfbs.bodies = newfbs.bodies.filter(x => x != @["ε"]) & @[@["ε"]]
        isChanging = true
      newrsf[h] = (grsf[h].flag, newfbs.bodies)
    grsf = newrsf # indent position must parallel with outermost for, not inner for!
  textarea &= "Internal epsilon-contraced CFG rules form:" & "\p"
  textarea &= grsf.sshow & "\p"
  #####################################
  ## content of CFGreachableVars.nim ##
  #####################################
  grsf = grsf.setflagsfalse
  textarea &= "Internal flags-falsed CFG rules form:" & "\p"
  textarea &= grsf.sshow & "\p"
  let ssyms = startSyms(m.iniStates, m.accStates).filter(s => grsf.hasKey(s)) ##  if not hasKey: delete start state
#  echo "start states = ", ssyms
#  @["V00", "V01"] => @["V00"]
  for st in ssyms: grsf.DFSfrom(st)
#  echo grsf.sshow & "\p"
  grsf = grsf.delFalseflagSymb
  textarea &= "Internal reachable symbol only CFG rules form:" & "\p"
  textarea &= grsf.sshow & "\p"
  var grs = grsf.numVartoAlphaVar(ssyms)
  textarea &= "Internal alphabetical nonterminal CFG rules form:" & "\p"
  textarea &= grs.sshow & "\p"
  grs = grs.sortRules # not in CFGreachableVars.nim
  textarea &= "Internal start-symbol-first CFG rules form:" & "\p"
  textarea &= grs.sshow & "\p"
#  echo grs.sshow & "\p"
  grs = grs.sortBodies
  textarea &= "Internal longer-body-first CFG rules form:" & "\p"
  textarea &= grs.sshow & "\p"
  ###############################
  ## content of RulestoCFG.nim ##
  ###############################
  let (grsV, grsT) = extractVerTer(grs)
  gcfg =
    (
      V: grsV,
      T: grsT,
      P: grs,
      S: startSyms(grs) # @[" (unfixed) "]
    )
  textarea &= "Internal ordinary CFG form:" & "\p"
  textarea &= gcfg.sshow & "\p"
  return textarea
echo toCFGtxt()

proc toCFGjson(): cstring {.exportc.} = #
  ##############################
  ## content of CFGtojson.nim ##
  ##############################
  let gj = %[("V",%gcfg.V),("T",%gcfg.T),("P",%gcfg.P),("S",%gcfg.S)] # %g, %*g # but expected one of: proc `%`(o: enum): JsonNode
  return $gj
echo toCFGjson()

proc toCFGgv(): cstring {.exportc.} = #
  #############################
  ## content of CFGtoDot.nim ##
  #############################
  let grm = gcfg.makeGrpmtx
  let headtbl = makeHeadtable(grm)
  let grmlbl = grm.makeElblmtx(gcfg)
# dot specification for nodes
  var spc = "digraph CFG_transition_diagram {\nsize = \"" & gvsize & "\"\nnodesep = " & gvnodesep2 & "\nranksep = " & gvranksep2 & "\n"
  #proc nodesSpec(grm: Grpmtx): string =
  let (m, n) = grm.sizes
  for i in 1..<m:
    if grm[i][0] != "": # = title head symbol => 1st skelton head
      spc &= grm[i][0] & " [shape = plaintext]\n"
    let aj = findAccIndex(grm[i])
    for j in 1..<aj:
      if grm[i][j] != "": spc &= grm[i][j] & " [shape = circle]\n"
    spc &= grm[i][aj] & " [shape = doublecircle]\n" # for aj
    spc &= "{rank = same;"
    for j in 0..aj:
      if grm[i][j] != "": spc &= " " & grm[i][j] & "[group = g" & $j & "];"
    spc &= "}\n"
# proc ordEdgesSpec(grm: Grpmtx): string =
  var headrow: int; var head: string;
  var headrowini: string; var headrowaj: int
  for i in 1..<m:
    if grm[i][0] != "": # 1st skelton head
      headrow = i
      head = grm[i][0]
      headrowini = grm[headrow][1] # init state of head row
      let aj = findAccIndex(grm[i]); headrowaj = aj
      for j in 1..aj:
        spc &= grm[i][j - 1] & " -> " & grm[i][j] & " [weight = 2"
        if j == 1: spc &= "]\n" # inia
        else: # j >= 2
          let elabel = grmlbl[i][j]
          spc &= ", label = \""  & elabel & "\""
          if elabel.isVariable: spc &= ", style = dotted"
          spc &= "]\n"
    else: # 2nd or later skelton
      let aj = findAccIndex(grm[i]) # acc state of this row
      for j in 2..aj: # exclude init state
        spc &= (if j == 2: headrowini else: grm[i][j - 1]) &
              " -> " & grm[i][j] & " [weight = 2"
        let elabel = grmlbl[i][j]
        spc &= ", label = \""  & elabel & "\""
        if elabel.isVariable: spc &= ", style = dotted"
        spc &= "]\n"
      spc &= grm[i][aj] & " -> " & grm[headrow][headrowaj] & " [weight = 1, color=\"black:invis:black\", arrowhead = onormal]\n" # eqiv
# proc calretEdgesSpec(grm: Grpmtx): string =
  for i in 1..<m:
    if grm[i][0] != "": # 1st skelton head
      headrow = i
    for j in 2..findAccIndex(grm[i]):
      let s = grmlbl[i][j]
      if s.isVariable:
        # call edge
        spc &= (if grm[i][j-1] == "": grm[headrow][j-1] else: grm[i][j-1]) & " -> " & grm[headtbl[s].hr][1] & # 1st symbol of 2nd or later skelton is variable
              " [fontname=\"MS UI Gothic\", taillabel = \"/" & maru[grm[i][j].parseInt] & "\""
        # call from (i,2) to (i,1)
        if j-1 == 2 and headtbl[s].hr == i:
          spc &= ", headport = n, tailport = n"
        spc &= "]\n"
        # return edge
        spc &= grm[headtbl[s].hr][headtbl[s].aj] & " -> " & grm[i][j] &
              " [fontname=\"MS UI Gothic\", taillabel = \"" & maru[grm[i][j].parseInt] & "/\"" &
              ", style = dashed, arrowhead = vee"
        # return from (i,j+1) to (i,j)
        if (headtbl[s].hr, headtbl[s].aj) == (i, j+1):
          spc &= ", headport = n, tailport = n"
        spc &= "]\n"
# proc veralignSpec(grm: Grpmtx): string =
  spc &= "edge[style = invis];\n"
  for j in 0..<n:
    var prv = ""
    for i in 1..<m:
      if grm[i][j] != "":
        if prv != "": spc &= prv & " -> " & grm[i][j] & "\n"
        prv = grm[i][j]
  spc &= "}"
  return spc
echo toCFGgv()

#  p2window.width = p2image.width
#  p2window.height = p2image.height + 25 # 20 = window title approx height
