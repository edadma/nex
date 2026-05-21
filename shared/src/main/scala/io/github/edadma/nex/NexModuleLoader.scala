package io.github.edadma.nex

import scala.collection.mutable

/** A single source file inside a module: its absolute path and parsed AST. */
case class FileEntry(absPath: String, ast: ProgramAST)

/** A module loaded from disk. `path` is the dotted name (empty for the root
  * module — the directory the project entry file lives in). `files` are all
  * `.nex` files directly in the module's directory (subdirs are submodules,
  * loaded as separate `LoadedModule`s). `isTestOnly` is true iff ANY file
  * in the module declares `@test module ...`; it's the union, not a per-
  * file flag, since the spec treats test/non-test as a module-wide property.
  * `imports` is the union of imports across all files, used by the
  * topological sort.
  */
case class LoadedModule(
    path:       List[String],
    files:      List[FileEntry],
    isTestOnly: Boolean,
    imports:    List[List[String]],
)

/** Walk the import graph from an entry file and return all reachable
  * modules in topological order (dependencies before dependents). Each
  * module is the union of all `.nex` files in one directory; subdirectories
  * are separate modules.
  *
  *  - `projectRoot`: directory treated as the root of the project. The
  *    root module lives here; `import a.b` resolves to `projectRoot/a/b/`.
  *  - On any parse error, IO error, missing module, or import cycle, an
  *    error message is returned (the caller decides how to surface it).
  *
  * The loader doesn't *resolve* import selectors (the elaborator does
  * that); it only chases the import paths to discover which directories
  * contain source.
  */
class NexModuleLoader(projectRoot: String):

  /** Result of a load: either a list of error messages, or modules in
    * topological order. Both the root module and any imported modules
    * are present; the root module is keyed by `List()`.
    */
  def loadFrom(entryFile: String): Either[List[String], List[LoadedModule]] =
    val errors  = mutable.ListBuffer.empty[String]
    val visited = mutable.Set.empty[List[String]]
    val onStack = mutable.Set.empty[List[String]]
    val ordered = mutable.ListBuffer.empty[LoadedModule]

    // The root module's directory contains the entry file; even if the
    // entry file isn't in `projectRoot`, we honour the user's choice.
    // (Most invocations have `projectRoot == pathDirname(entryFile)`.)
    def loadModule(path: List[String]): Unit =
      if visited(path) then return
      if onStack(path) then
        errors += s"import cycle through module `${path.mkString(".")}`"
        return
      onStack += path

      val dirPath = if path.isEmpty then projectRoot else pathJoin((projectRoot :: path)*)
      if !pathIsDirectory(dirPath) then
        errors += s"module `${path.mkString(".")}` not found (looked in `$dirPath`)"
        onStack -= path; visited += path
        return

      val files = listNexFiles(dirPath)
      if files.isEmpty then
        errors += s"module `${path.mkString(".")}` has no `.nex` files (in `$dirPath`)"
        onStack -= path; visited += path
        return

      val parsed     = mutable.ListBuffer.empty[FileEntry]
      var testOnly   = false
      val modImports = mutable.LinkedHashSet.empty[List[String]]

      for f <- files do
        val src = readNexSource(f)
        new NexParser().parseProgram(src) match
          case Left(err) =>
            errors += s"$f: parse error: $err"
          case Right(ast) =>
            // Validate any module declaration against the directory path
            // and harvest imports. Then strip module decls before handing
            // the file to the elaborator — when multiple files in one
            // module each declare `module foo`, concatenating them would
            // make every secondary `module foo` look like a mid-file
            // declaration to the elaborator's "module must come first"
            // check. The loader has already done the validation.
            val keptDecls = ast.decls.flatMap {
              case m: ModuleDeclAST =>
                if m.path != path then
                  errors += s"$f: module declaration `${m.path.mkString(".")}` does not match directory path `${path.mkString(".")}`"
                if m.isTestOnly then testOnly = true
                None
              case i: ImportDeclAST =>
                modImports += i.path
                Some(i)
              case other =>
                Some(other)
            }
            parsed += FileEntry(f, ast.copy(decls = keptDecls))

      // Recurse into imports BEFORE recording this module, so the resulting
      // `ordered` list has every dependency strictly before its dependent.
      for impPath <- modImports do loadModule(impPath)

      ordered += LoadedModule(path, parsed.toList, testOnly, modImports.toList)
      onStack -= path
      visited += path

    loadModule(Nil)

    if errors.nonEmpty then Left(errors.toList)
    else Right(ordered.toList)

object NexModuleLoader:

  /** Load the source-form prelude living at `preludeRoot` as a single
    * `LoadedModule` whose path is `["prelude"]`. The directory is treated
    * as flat: every `.nex` file directly in `preludeRoot` is parsed and
    * merged. Sub-directories under `preludeRoot` are *not* picked up here
    * — they're separate modules (`prelude.array` etc.) that, when they
    * exist, users will import explicitly per the roadmap's shallow
    * auto-import policy.
    *
    * The prelude module is not allowed to declare its own `import`s
    * pointing back into user code (which would create a cycle); any
    * `import prelude.foo.*` cross-references inside prelude files
    * resolve through `elaborateProject`'s normal import machinery and
    * are caller-driven.
    */
  def loadPreludeAsModule(preludeRoot: String): Either[List[String], LoadedModule] =
    if !pathIsDirectory(preludeRoot) then
      Left(List(s"prelude directory `$preludeRoot` not found"))
    else
      val files = listNexFiles(preludeRoot)
      if files.isEmpty then
        Right(LoadedModule(List("prelude"), Nil, isTestOnly = false, imports = Nil))
      else
        val errors  = scala.collection.mutable.ListBuffer.empty[String]
        val parsed  = scala.collection.mutable.ListBuffer.empty[FileEntry]
        val imports = scala.collection.mutable.LinkedHashSet.empty[List[String]]
        for f <- files do
          val src = readNexSource(f)
          new NexParser().parseProgram(src) match
            case Left(err) =>
              errors += s"$f: parse error: $err"
            case Right(ast) =>
              val keptDecls = ast.decls.flatMap {
                case m: ModuleDeclAST =>
                  if m.path != List("prelude") then
                    errors += s"$f: prelude file declares `module ${m.path.mkString(".")}` (expected `module prelude`)"
                  None
                case i: ImportDeclAST =>
                  imports += i.path
                  Some(i)
                case other =>
                  Some(other)
              }
              parsed += FileEntry(f, ast.copy(decls = keptDecls))
        if errors.nonEmpty then Left(errors.toList)
        else Right(LoadedModule(List("prelude"), parsed.toList, isTestOnly = false, imports.toList))
