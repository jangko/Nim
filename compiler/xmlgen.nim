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
  condsyms, intsets, platform, idents

type
  XMLGen = object of TPassContext
    module: PSym
    fileName: string

  BModule = ref XMLGen
  
  TGlobals = object
    code: Rope
    generatedSyms: IntSet
    generatedIdents: IntSet
    symTab: TSymSeq
    identTab: seq[PIdent]
    
  PGlobals = ref TGlobals
  
proc newGlobals(): PGlobals =
  new(result)
  result.generatedSyms = initIntSet()
  result.symTab = @[]
  result.generatedIdents = initIntSet()
  result.identTab = @[]
  
var globals: PGlobals

proc newModule(module: PSym): BModule =
  new(result)
  result.module = module
  result.fileName = module.position.int32.toFullPath
  
  if globals == nil: globals = newGlobals()
 
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
    
proc registerIdent(s: PIdent) =
  if not globals.generatedIdents.contains(s.id):
    globals.generatedIdents.incl(s.id)
    globals.identTab.add(s)

proc genIdent(i: PIdent): Rope =
  result = rope("    <ident")
  result.genAttr("id", $i.id)
  result.genAttr("s", i.s)
  result.add "/>" & tnl

proc genSym(s: PSym): Rope =
  result = rope("    <symbol")
  result.genAttr("id", $s.id)
  result.genAttr("kind", $s.kind)
  result.genAttr("magic", $s.magic)
  
  if s.typ != nil:
    result.genAttr("type", $s.typ.id)
  else:
    result.add " type=\"nil\""
    
  registerIdent(s.name)
  result.genAttr("name", $s.name.id)
  result.genAttr("line", $s.info.line)
  result.genAttr("col", $s.info.col)
  result.genAttr("fileIndex", $s.info.fileIndex)
    
  if s.owner != nil:
    result.genAttr("owner", $s.owner.id)
  else:
    result.add " owner=\"nil\""
  
  result.genAttr("flags", $s.flags)
  result.genAttr("options", $s.options)
  result.genAttr("position", $s.position)
  result.genAttr("ofset", $s.offset)
    
  result.add "/>" & tnl
                   
proc registerSym(s: PSym) =
  if not globals.generatedSyms.contains(s.id):
    globals.generatedSyms.incl(s.id)
    globals.symTab.add(s)

proc genFile(t: TFileInfo, fullPath: string): Rope =
  result = rope("    <symbol")
  result.genAttr("fullPath", fullPath)
  result.genAttr("projPath", t.projPath)
  result.genAttr("shortName", t.shortName)
  result.genAttr("quotedName", t.quotedName)
  #result.genAttr("dirtyFile", t.dirtyfile)
  
  result.add "/>" & tnl

proc genFiles(): Rope =
  result = rope("  <files>" & tnl)
  for i in 0..fileInfos.high:
    result.add genFile(fileInfos[i], toFullPath(i.int32))
  result.add "  </files>" & tnl
  
proc genSymbols(): Rope =
  result = rope("  <symbols>" & tnl)
  for s in globals.symTab:
    result.add genSym(s)
  result.add "  </symbols>" & tnl
  
proc genIdents(): Rope =
  result = rope("  <idents>" & tnl)
  for i in globals.identTab:
    result.add genIdent(i)
  result.add "  </idents>" & tnl
    
proc wholeCode*(m: BModule): Rope =
  result = rope("")
  result.add genFiles()
  result.add genIdents()
  result.add genSymbols()  
  
proc processNode(m: BModule, n: PNode) =
  case n.kind
  of nkSym:
    registerSym(n.sym)
  of nkCharLit..nkUInt64Lit:
    #r.res = rope(n.intVal)
    #r.kind = resExpr
    discard
  of nkNilLit:
    #if isEmptyType(n.typ):
    #  discard
    #elif mapType(n.typ) == etyBaseIndex:
    #  r.typ = etyBaseIndex
    #  r.address = rope"null" | rope"nil"
    #  r.res = rope"0"
    #  r.kind = resExpr
    #else:
    #  r.res = rope"null" | rope"nil"
    #  r.kind = resExpr
    discard
  of nkStrLit..nkTripleStrLit:
    discard
  of nkFloatLit..nkFloat64Lit:
    #let f = n.floatVal
    #if f != f: r.res = rope"NaN"
    #elif f == 0.0: r.res = rope"0.0"
    #elif f == 0.5 * f:
    #  if f > 0.0: r.res = rope"Infinity"
    #  else: r.res = rope"-Infinity"
    #else: r.res = rope(f.toStrMaxPrecision)
    #r.kind = resExpr
    discard
  of nkIdent:
    registerIdent(n.ident)
  else:
    if not isNil(n.sons):
      for x in n.sons:
        processNode(m, x)
        
proc myProcess(b: PPassContext, n: PNode): PNode =
  if passes.skipCodegen(n): return n
  result = n
  var m = BModule(b)
  if m.module == nil: internalError(n.info, "myProcess")
  processNode(m, n)
  
proc myClose(b: PPassContext, n: PNode): PNode =
  if passes.skipCodegen(n): return n
  result = myProcess(b, n)
  var m = BModule(b)
  if sfMainModule in m.module.flags:
    let code = wholeCode(m)
    let outfile =
      if options.outFile.len > 0:
        if options.outFile.isAbsolute: options.outFile
        else: getCurrentDir() / options.outFile
      else:
       changeFileExt(getCurrentDir() / m.module.filename, "xml")
    discard writeRopeIfNotEqual(genHeader() & code & genFooter(), outfile)
    
proc myOpenCached(s: PSym, rd: PRodReader): PPassContext =
  internalError("symbol files are not possible with the xml generator")
  result = nil
  
proc myOpen(s: PSym): PPassContext =
  result = newModule(s)

const xmlgenPass* = makePass(myOpen, myOpenCached, myProcess, myClose)