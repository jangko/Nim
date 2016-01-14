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
  condsyms, intsets, platform

type
  XMLGen = object of TPassContext
    module: PSym

  BModule = ref XMLGen
  
  TGlobals = object
    code: Rope
    generatedSyms: IntSet
    
  PGlobals = ref TGlobals
  
proc newGlobals(): PGlobals =
  new(result)
  result.generatedSyms = initIntSet()
  
var globals: PGlobals

proc newModule(module: PSym): BModule =
  new(result)
  result.module = module
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

proc wholeCode*(m: BModule): Rope =
  result = rope""

proc myProcess(b: PPassContext, n: PNode): PNode =
  if passes.skipCodegen(n): return n
  result = n
  var m = BModule(b)
  if m.module == nil: internalError(n.info, "myProcess")
  
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