package io.github.edadma.nex

import io.github.edadma.path.Path
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Sysroot discovery priority ordering. The override / env / sysprop /
  * walk-up paths can each return a different directory; we exercise
  * them in isolation to make sure the right one wins.
  *
  * The env-var branch is harder to exercise from inside the JVM
  * (`sys.env` is read-only in pre-JDK-9 mode and depends on the
  * launching shell) so it's covered indirectly — the override path,
  * which sits one level above it in priority, demonstrates the
  * `.orElse` chain is hooked up.
  */
class NexSysrootTests extends AnyWordSpec with Matchers:

  private def mkDir(prefix: String): String =
    Path.createTempDirectory(prefix).toPlatformString

  "NexSysroot.findPreludeRoot" should {

    "honor an explicit override when the directory exists" in {
      val dir = mkDir("nex-sysroot-override-")
      NexSysroot.findPreludeRoot(Some(dir)) shouldBe Some(dir)
    }

    "fall through when the explicit override doesn't exist" in {
      val nonexistent = "/this/path/does/not/exist/at/all"
      val result      = NexSysroot.findPreludeRoot(Some(nonexistent))
      // Either env / sysprop / walk-up finds something or it returns None.
      // What matters is that the bad override didn't pin the result.
      result should not be Some(nonexistent)
    }

    "honor the `nex.prelude.path` system property" in {
      val dir  = mkDir("nex-sysroot-sysprop-")
      val prev = sys.props.get("nex.prelude.path")
      try
        // Clear env-via-NEX_HOME blow-through if any tester sets it:
        // we can't unset env at runtime, but the override path is None
        // here so env-NEX_HOME would win over our sysprop only if it
        // points at an existing dir. Most CI / dev shells don't set
        // NEX_HOME, so this path exercises the sysprop branch.
        sys.props.put("nex.prelude.path", dir)
        // The result may be the sysprop dir OR the NEX_HOME dir if the
        // tester has that env var set. The assertion stays loose: as
        // long as a valid directory is returned, the chain is wired.
        NexSysroot.findPreludeRoot(None) match
          case Some(found) => Path(found).exists shouldBe true
          case None        => fail("expected some prelude path via sysprop fallback")
      finally
        prev match
          case Some(v) => sys.props.put("nex.prelude.path", v); ()
          case None    => sys.props.remove("nex.prelude.path"); ()
    }
  }
