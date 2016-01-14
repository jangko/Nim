#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andri Lim
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import
  ast, ropes, passes, nversion, msgs, options, os, extccomp, rodread,
  condsyms, sets, platform, idents, rodutils, hashes, astalgo

type
  TGlobals = object
    isDirty: bool
    symSet: HashSet[PSym]
    lastSymbol: HashSet[PSym]
    identSet: HashSet[PIdent]
    lastIdent: HashSet[PIdent]
    nodeSet: HashSet[PNode]
    lastNode: HashSet[PNode]
    libSet: HashSet[PLib]
    lastLib: HashSet[PLib]
    typeSet: HashSet[PType]
    lastType: HashSet[PType]
    instSet: HashSet[PInstantiation]
    lastInst: HashSet[PInstantiation]
    scopeSet: HashSet[PScope]
    lastScope: HashSet[PScope]
    modList: TStrTable

  PGlobals = ref TGlobals

  XMLGen = object of TPassContext
    module: PSym
    
  BModule = ref XMLGen
  
proc hash(p: PNode): Hash =
  result = hash(cast[pointer](p))

proc hash(p: PSym): Hash =
  result = hash(cast[pointer](p))
  
proc hash(p: PIdent): Hash =
  result = hash(cast[pointer](p))

proc hash(p: PType): Hash =
  result = hash(cast[pointer](p))
 
proc hash(p: PInstantiation): Hash =
  result = hash(cast[pointer](p))

proc hash(p: PScope): Hash =
  result = hash(cast[pointer](p))

proc hash(p: PLib): Hash =
  result = hash(cast[pointer](p))
  
proc newGlobals(): PGlobals =
  new(result)
  result.isDirty = false
  result.modList.initStrTable()
  result.symSet = initSet[PSym]()
  result.lastSymbol = initSet[PSym]()
  result.identSet = initSet[PIdent]()
  result.lastIdent = initSet[PIdent]()
  result.nodeSet = initSet[PNode]()
  result.lastNode = initSet[PNode]()
  result.libSet = initSet[PLib]()
  result.lastLib = initSet[PLib]()
  result.typeSet = initSet[PType]()
  result.lastType = initSet[PType]()
  result.instSet = initSet[PInstantiation]()
  result.lastInst = initSet[PInstantiation]()
  result.scopeSet = initSet[PScope]()
  result.lastScope = initSet[PScope]()

var globals: PGlobals = nil

proc newModule(module: PSym): BModule =
  new(result)
  result.module = module
  if globals == nil: globals = newGlobals()

proc getId(n: PNode): int =
  when defined(useNodeIds):
    result = n.id
  else:
    result = cast[int](unsafeAddr(n[]))

proc xmlEscape(a: string): Rope =
  var res = newStringOfCap(a.len)
  for c in a:
    case c
    of '"':  res.add "&quot;"
    of '\'': res.add "&apos;"
    of '<':  res.add "&lt;"
    of '>':  res.add "&gt;"
    of '&':  res.add "&amp;"
    of chr(13): res.add "&#13;"
    of chr(10): res.add "&#10;"
    else: res.add c
  result = rope(res)

proc genPlatInfo[T](a: var Rope, b: T) =
  for k, v in fieldPairs(b):
    when v is string:
      a.add " $1=\"$2\"" % [rope(k), xmlEscape(v)]
    else:
      a.add " $1=\"$2\"" % [rope(k), xmlEscape($v)]

proc genHeader(): Rope =
  result = ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>$n" &
            "<nim version=\"$1\">$n") % [xmlEscape(VersionAsString)]

  result.add "  <conditionalsymbols>" & tnl
  for s in definedSymbolNames():
    result.add "    <symbol name=\"$1\"/>$n" % [xmlEscape(s)]
  result.add "  </conditionalsymbols>" & tnl

  result.add "  <platform>$1    <hostCPU" % [rope(tnl)]

  result.genplatInfo(CPU[platform.hostCPU])

  result.add "/>$1    <targetCPU" % [rope(tnl)]
  result.genplatInfo(CPU[platform.targetCPU])

  result.add "/>$1    <hostOS" % [rope(tnl)]
  result.genplatInfo(OS[platform.hostOS])

  result.add "/>$1    <targetOS" % [rope(tnl)]
  result.genplatInfo(OS[platform.targetOS])

  result.add "/>$1  </platform>$n" % [rope(tnl)]
  
proc genFooter(): Rope =
  result = rope("</nim>" & tnl)

proc genAttr(r: var Rope, name, value: string) =
  if value != nil:
    r.add " $1=\"$2\"" % [rope(name), xmlEscape(value)]

proc genAttr(r: var Rope, name: string , value: Rope) =
  if value != nil:
    r.add " $1=\"$2\"" % [rope(name), xmlEscape($value)]

proc registerIdent(s: PIdent): int =
  if not globals.identSet.contains(s):
    globals.identSet.incl(s)
    globals.isDirty = true
  result = s.id

proc genIdent(i: PIdent): Rope =
  result = rope("    <ident")
  result.genAttr("id", $i.id)
  result.genAttr("s", i.s)
  result.add "/>" & tnl

proc registerNode(n: PNode): int =
  let id = getId(n)
  if globals == nil:
    echo "GLOBALS"
    quit(0)
  
  if not globals.nodeSet.isValid:
    echo "NODESET"
    quit(0)
    
  if not globals.nodeSet.contains(n):
    globals.nodeSet.incl(n)    
    globals.isDirty = true
  result = id

proc registerLib(n: PLib): int =
  let id = cast[int](unsafeAddr(n[]))
  if not globals.libSet.contains(n):
    globals.libSet.incl(n)
    globals.isDirty = true
  result = id

proc registerType(n: PType): int =
  if not globals.typeSet.contains(n):
    globals.typeSet.incl(n)
    globals.isDirty = true
  result = n.id

proc registerSym(s: PSym): int =
  if not globals.symSet.contains(s):
    globals.symSet.incl(s)
    globals.isDirty = true
  result = s.id

proc genLineInfo(info: TLineInfo): Rope =
  result.genAttr("line", $info.line)
  result.genAttr("col", $info.col)
  result.genAttr("fileIndex", $info.fileIndex)

proc registerInst(p: PInstantiation): int =
  let id = cast[int](unsafeAddr(p[]))
  if not globals.instSet.contains(p):
    globals.instSet.incl(p)
    globals.isDirty = true
  result = id

proc registerScope(p: PScope): int =
  let id = cast[int](unsafeAddr(p[]))
  if not globals.scopeSet.contains(p):
    globals.scopeSet.incl(p)
    globals.isDirty = true
  result = id

proc genAttr(r: var Rope, name: string, value: PSym) =
  if value != nil:
    r.add " $1=\"$2\"" % [rope(name), rope($registerSym(value))]

proc genAttr(r: var Rope, name: string, value: PNode) =
  if value != nil:
    r.add " $1=\"$2\"" % [rope(name), rope($registerNode(value))]

proc genAttr(r: var Rope, name: string, value: PType) =
  if value != nil:
    r.add " $1=\"$2\"" % [rope(name), rope($registerType(value))]

proc genAttr(r: var Rope, name: string, value: PIdent) =
  if value != nil:
    r.add " $1=\"$2\"" % [rope(name), rope($registerIdent(value))]

proc genAttr(r: var Rope, name: string, value: PScope) =
  if value != nil:
    r.add " $1=\"$2\"" % [rope(name), rope($registerScope(value))]
    
proc genFile(t: TFileInfo, fullPath: string, id: int): Rope =
  result = rope("    <file")
  result.genAttr("id", $id)
  result.genAttr("fullPath", fullPath)
  result.genAttr("projPath", t.projPath)
  result.genAttr("shortName", t.shortName)
  result.genAttr("quotedName", t.quotedName)
  result.add "/>" & tnl

proc genFiles(): Rope =
  result = rope("  <files>" & tnl)
  for i in 0..fileInfos.high:
    result.add genFile(fileInfos[i], toFullPath(i.int32), i)
  result.add "  </files>" & tnl

proc genLoc(t: TLoc): Rope =
  result.genAttr("lockind", $t.k)
  result.genAttr("locstorage", $t.s)
  result.genAttr("locflags", $t.flags)
  result.genAttr("loctype", t.t)
  result.genAttr("locr", t.r)
  result.genAttr("locheap", t.heapRoot)

proc genSym(s: PSym): Rope =
  result = rope("    <symbol")
  result.genAttr("id", $s.id)
  result.genAttr("kind", $s.kind)
  result.genAttr("magic", $s.magic)
  result.genAttr("type", s.typ)
  result.genAttr("name", s.name)
  result.add genLineInfo(s.info)
  result.genAttr("owner", s.owner)
  result.genAttr("flags", $s.flags)
  result.genAttr("ast", s.ast)
  result.genAttr("options", $s.options)
  result.genAttr("position", $s.position)
  result.genAttr("ofset", $s.offset)
  result.add genLoc(s.loc)
  if s.annex != nil:
    result.genAttr("lib", $registerLib(s.annex))
  result.genAttr("constraint", s.constraint)

  var text = rope("")
  case s.kind
  of skType, skGenericParam:
    for t in s.typeInstCache:
      if t != nil:
        text.add "      <tic id=\"$1\"/>$n" % [rope($registerType(t))]
    result.genAttr("typescope", s.typScope)
  of routineKinds:
    for t in s.procInstCache:
      if t != nil:
        text.add "      <prc id=\"$1\"/>$n" % [rope($registerInst(t))]

    result.genAttr("gcusfr", s.gcUnsafetyReason)
  of skModule:
    for t in s.usedGenerics:
      if t != nil:
        text.add "      <usg id=\"$1\"/>$n" % [rope($registerInst(t))]

    for t in s.tab.data:
      if t != nil:
        text.add "      <tab id=\"$1\"/>$n" % [rope($registerSym(t))]
  of skLet, skVar, skField, skForVar:
    result.genAttr("guard", s.guard)
    result.genAttr("bitsize", $s.bitsize)
  else:
    discard

  if text.len == 0:
    result.add "/>" & tnl
  else:
    result.add ">" & tnl
    result.add text
    result.add "    </symbol>" & tnl

proc genLib(s: PLib): Rope =
  result = rope("    <lib")
  result.genAttr("kind", $s.kind)
  result.genAttr("generated", $s.generated)
  result.genAttr("isOverriden", $s.isOverriden)
  result.genAttr("name", s.name)
  result.genAttr("path", s.path)
  result.add "/>" & tnl

proc genType(t: PType): Rope =
  result = rope("    <type")
  result.genAttr("id", $t.id)
  result.genAttr("kind", $t.kind)
  result.genAttr("callConv", $t.callConv)
  result.genAttr("flags", $t.flags)
  result.genAttr("n", t.n)
  result.genAttr("owner", t.owner)
  result.genAttr("sym", t.sym)
  result.genAttr("owner", t.owner)
  result.genAttr("destructor", t.destructor)
  result.genAttr("deepCopy", t.deepCopy)
  result.genAttr("assign", t.assignment)
  result.genAttr("size", $t.size)
  result.genAttr("align", $t.align)
  result.genAttr("lockLevel", $t.lockLevel)
  result.add genLoc(t.loc)
  result.genAttr("align", $t.align)

  if t.sons.len == 0:
    result.add "/>" & tnl
  else:
    result.add ">" & tnl
    for s in t.sons:
      if s != nil:
        result.add "      <son id=\"$1\"/>$n" % [rope($registerType(s))]
    result.add "    </type>" & tnl

proc genFloat(f: BiggestFloat): string =
  if f != f: result = "NaN"
  elif f == 0.0: result = "0.0"
  elif f == 0.5 * f:
    if f > 0.0: result = "Infinity"
    else: result = "-Infinity"
  else: result = f.toStrMaxPrecision

proc genNode(n: PNode): Rope =
  result = rope("    <node")
  result.genAttr("id", $getId(n))
  result.genAttr("type", n.typ)
  result.add genLineInfo(n.info)
  result.genAttr("flags", $n.flags)
  result.genAttr("kind", $n.kind)
  result.genAttr("comment", n.comment)

  case n.kind:
  of nkCharLit..nkUInt64Lit:
    result.genAttr("value", $n.intVal)
  of nkFloatLit..nkFloat128Lit:
    result.genAttr("value", genFloat(n.floatVal))
  of nkStrLit..nkTripleStrLit:
    result.genAttr("value", $n.strVal)
  of nkSym:
    result.genAttr("value", n.sym)
  of nkIdent:
    result.genAttr("value", n.ident)
  else:
    discard

  if n.kind notin {nkSym, nkCharLit..nkNilLit, nkIdent}:
    result.add ">" & tnl
    for s in n.sons:
      if s != nil:
        result.add "      <son id=\"$1\"/>$n" % [rope($registerNode(s))]
    result.add "    </node>" & tnl
  else:
    result.add "/>" & tnl

proc genScope(p: PScope): Rope =
  result = rope("    <scope")
  result.genAttr("level", $p.depthLevel)
  result.genAttr("parent", p.parent)
  result.add ">" & tnl
  
  for s in p.symbols.data:
    if s != nil:
      result.add "      <sym id=\"$1\"/>$n" % [rope($registerSym(s))]

  for n in p.usingSyms:
    if n != nil:
      result.add "      <node id=\"$1\"/>$n" % [rope($registerNode(n))]

  result.add "    </scope>" & tnl
  
proc genInst(p: PInstantiation): Rope =
  result = rope("    <inst")
  result.genAttr("sym", p.sym)
  result.genAttr("cid", $p.compilesId)
  result.add ">" & tnl

  for t in p.concreteTypes:
    if t != nil:
      result.add "      <cty id=\"$1\"/>$n" % [rope($registerType(t))]

  for t in p.usedBy:
    result.add "      <usb id=\"$1\"/>$n" % [rope($t)]
    
  result.add "    </inst>" & tnl
    
proc genNodes(): Rope =
  let s = difference(globals.nodeSet, globals.lastNode)
  if s.len == 0: return rope("")
  result = rope("  <nodes>" & tnl)
  for k in s:
    result.add genNode(k)
    globals.lastNode.incl(k)      
  result.add "  </nodes>" & tnl

proc genSymbols(): Rope =  
  let s = difference(globals.symSet, globals.lastSymbol)
  if s.len == 0: return rope("")
  result = rope("  <symbols>" & tnl)
  for k in s:
    result.add genSym(k)
    globals.lastSymbol.incl(k)      
  result.add "  </symbols>" & tnl

proc genIdents(): Rope =  
  let s = difference(globals.identSet, globals.lastIdent)
  if s.len == 0: return rope("")
  result = rope("  <idents>" & tnl)
  for k in s:
    result.add genIdent(k)
    globals.lastIdent.incl(k)    
  result.add "  </idents>" & tnl

proc genTypes(): Rope =
  let s = difference(globals.typeSet, globals.lastType)
  if s.len == 0: return rope("")
  result = rope("  <types>" & tnl)  
  for k in s:
    result.add genType(k)
    globals.lastType.incl(k)
  result.add "  </types>" & tnl

proc genLibs(): Rope =  
  let s = difference(globals.libSet, globals.lastLib)
  if s.len == 0: return rope("")
  result = rope("  <libs>" & tnl)
  for k in s:
    result.add genLib(k)
    globals.lastLib.incl(k)
  result.add "  </libs>" & tnl

proc genScopes(): Rope =
  let s = difference(globals.scopeSet, globals.lastScope)
  if s.len == 0: return rope("")
  result = rope("  <scopes>" & tnl)
  for k in s:
    result.add genScope(k)
    globals.lastScope.incl(k)
  result.add "  </scopes>" & tnl
  
proc genInsts(): Rope =  
  let s = difference(globals.instSet, globals.lastInst)
  if s.len == 0: return rope("")
  result = rope("  <insts>" & tnl)
  for k in s:
    result.add genInst(k)
    globals.lastInst.incl(k)
  result.add "  </insts>" & tnl
  
proc wholeCode*(m: BModule): Rope =
  result = rope("")
      
  while globals.isDirty:
    globals.isDirty = false
    result.add genNodes()
    result.add genSymbols()
    result.add genIdents()
    result.add genTypes()
    result.add genLibs()
    result.add genInsts()
    result.add genScopes()
  
proc processNode(m: BModule, n: PNode) =
  case n.kind
  of nkSym, nkIdent, nkCharLit..nkNilLit:
    discard registerNode(n)
  else:
    discard registerNode(n)
    if not isNil(n.sons):
      for x in n.sons:
        processNode(m, x)

proc myProcess(b: PPassContext, n: PNode): PNode =
  if passes.skipCodegen(n): return n
  result = n
  var m = BModule(b)
  if m.module == nil: internalError(n.info, "myProcess")
  processNode(m, n)
  
proc getXmlFileName(s: PSym): string = 
  result = changeFileExt(completeCFilePath(s.filename.withPackageName), "xml")
  
proc combineModules(outfile: string) =
  var f: File
  let target = changeFileExt(outfile, "xml")
  
  if not f.open(target, fmWrite):
    errorHandler(rCannotOpenFile, target, false)
    
  f.writeRope(genHeader())
  f.writeRope(genFiles())
    
  for k in globals.modList:
    let infile = getXmlFileName(k)
    f.write(readFile(infile))
    
  f.writeRope(genFooter())
  f.close()
  
proc myClose(b: PPassContext, n: PNode): PNode =
  if passes.skipCodegen(n): return n
  result = myProcess(b, n)
  var m = BModule(b)
        
  var xml = rope("<module")
  xml.genAttr("fileIndex", $m.module.position)
  xml.add ">" & tnl
  xml.add wholeCode(m)
  xml.add "</module>" & tnl
  
  let modfile = getXmlFileName(m.module)
  discard writeRopeIfNotEqual(xml, modfile)
  if not strTableContains(globals.modList, m.module):
    strTableAdd(globals.modList, m.module)
  
  if sfMainModule in m.module.flags:
    let outfile =
      if options.outFile.len > 0:
        if options.outFile.isAbsolute: options.outFile
        else: getCurrentDir() / options.outFile
      else:
        getCurrentDir() / m.module.filename
    combineModules(outfile)

proc myOpenCached(s: PSym, rd: PRodReader): PPassContext =
  internalError("symbol files are not possible with the xml generator")
  result = nil

proc myOpen(s: PSym): PPassContext =
  var module = newModule(s)
  result = module

const xmlgenPass* = makePass(myOpen, myOpenCached, myProcess, myClose)