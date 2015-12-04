package scala.coroutines.common



import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



/** Contains extra tree traversal methods.
 */
class TraverserUtil[C <: Context](val c: C) {
  import c.universe._

  /** Traverse equivalent parts of two trees in parallel,
   *  applying the specified function to parts of the tree with the same shape.
   */
  def traverseByShape(t0: Tree, t1: Tree)(f: (Tree, Tree) => Unit) = {
    def traverse(t0: Tree, t1: Tree): Unit = {
      f(t0, t1)
      (t0, t1) match {
        case (q"(..$params0) => $body0", q"(..$params1) => $body1") =>
          // function
          for ((p0, p1) <- params0 zip params1) traverse(p0, p1)
          traverse(body0, body1)
        case (q"{ case ..$cs0 }", q"{ case ..$cs1 }") =>
          // partial function
          for ((c0, c1) <- cs0 zip cs1) traverse(c0, c1)
        case (q"if ($c0) $t0 else $e0", q"if ($c1) $t1 else $e1") =>
          // if
          traverse(c0, c1)
          traverse(t0, t1)
          traverse(e0, e1)
        case (q"while ($c0) $b0", q"while ($c1) $b1") =>
          // while loop
          traverse(c0, c1)
          traverse(b0, b1)
        case (q"do $b0 while ($c0)", q"do $b1 while ($c1)") =>
          // while loop
          traverse(b0, b1)
          traverse(c0, c1)
        case (q"for (..$enums0) $b0", q"for (..$enums1) $b1") =>
          // for loop
          for ((e0, e1) <- enums0 zip enums1) traverse(e0, e1)
          traverse(b0, b1)
        case (q"for (..$enums0) yield $b0", q"for (..$enums1) yield $b1") =>
          // for-yield loop
          for ((e0, e1) <- enums0 zip enums1) traverse(e0, e1)
          traverse(b0, b1)
        case (
          q"new { ..$eds0 } with ..$ps0 { $self0 => ..$stats0 }",
          q"new { ..$eds1 } with ..$ps1 { $self1 => ..$stats1 }"
        ) =>
          // new
          for ((e0, e1) <- eds0 zip eds1) traverse(e0, e1)
          for ((p0, p1) <- ps0 zip ps1) traverse(p0, p1)
          traverse(self0, self1)
          for ((s0, s1) <- stats0 zip stats1) traverse(s0, s1)
        case (q"$a0[$t0]", q"$a1[$t1]") =>
          // type application
          traverse(a0, a1)
          traverse(t0, t1)
        case (q"$lhs0 = $rhs0", q"$lhs1 = $rhs1") =>
          // update
          traverse(lhs0, lhs1)
          traverse(rhs0, rhs1)
        case (q"return $r0", q"return $r1") =>
          // return
          traverse(r0, r1)
        case (q"throw $e0", q"throw $e1") =>
          // throw
          traverse(e0, e1)
        case (q"$e0: $tpt0", q"$e1: $tpt1") =>
          // ascription
          traverse(e0, e1)
          traverse(tpt0, tpt1)
        case (q"$e0: @$a0", q"$e1: @$a1") =>
          // annotated
          traverse(e0, e1)
          traverse(a0, a1)
        case (q"(..$exprs0)", q"(..$exprs1)") if exprs1.length > 1 =>
          // tuple
          for ((e0, e1) <- exprs0 zip exprs1) traverse(e0, e1)
        case (q"$e0 match { case ..$cs0 }", q"$e1 match { case ..$cs1 }") =>
          // pattern match
          traverse(e0, e1)
          for ((c0, c1) <- cs0 zip cs1) traverse(c0, c1)
        case (
          q"try $b0 catch { case ..$cs0 } finally $f0",
          q"try $b1 catch { case ..$cs1 } finally $f1"
        ) =>
          // try
          traverse(b0, b1)
          for ((c0, c1) <- cs0 zip cs1) traverse(c0, c1)
          traverse(f0, f1)
        case (q"$a0[..$tpts0](...$paramss0)", q"$a1[..$tpts1](...$paramss1)")
          if tpts0.length > 0 || paramss0.length > 0 =>
          // application
          traverse(a0, a1)
          for ((t0, t1) <- tpts0 zip tpts1) traverse(t0, t1)
          for ((ps0, ps1) <- paramss0 zip paramss1; (p0, p1) <- ps0 zip ps1) {
            traverse(p0, p1)
          }
        case (q"$r0.$m0", q"$r1.$m1") =>
          // selection
          traverse(r0, r1)
        case (q"$q0.super[$s0].$n0", q"$q1.super[$s1].$n1") =>
          // super selection
        case (q"$q0.this", q"$q1.this") =>
          // this
        case (q"{ ..$ss0 }", q"{ ..$ss1 }") if ss0.length > 1 && ss1.length > 1 =>
          // stats
          for ((a, b) <- ss0 zip ss1) traverse(a, b)
        case (Block(List(s0), e0), Block(List(s1), e1)) =>
          // stats, single
          traverse(s0, s1)
          traverse(e0, e1)
        case (tq"$tpt0.type", tq"$tpt1.type") =>
          // singleton type
          traverse(tpt0, tpt1)
        case (tq"$r0#$nme0", tq"$r1#$nme1") =>
          // type projection
          traverse(r0, r1)
        case (tq"$r0#$nme0", tq"$r1#$nme1") =>
          // type selection
          traverse(r0, r1)
        case (tq"$p0.super[$s0].$q0", tq"$p1.super[$s1].$q1") =>
          // super type selection
        case (tq"this.$n0", tq"this.$n1") =>
          // this type projection
        case (tq"$tpt0[..$tps0]", tq"$tpt1[..$tps1]") if tps0.length > 0 =>
          // applied type
          traverse(tpt0, tpt1)
          for ((tp0, tp1) <- tps0 zip tps1) traverse(tp0, tp1)
        case (tq"$tpt0 @$annots0", tq"$tpt1 @$annots1") =>
          // annotated type
          traverse(tpt0, tpt1)
          traverse(annots0, annots1)
        case (tq"..$ps0 { ..$defs0 }", tq"..$ps1 { ..$defs1 }") =>
          // compound type
          for ((p0, p1) <- ps0 zip ps1) traverse(p0, p1)
          for ((d0, d1) <- defs0 zip defs1) traverse(d0, d1)
        case (tq"$tp0 forSome { ..$defs0 }", tq"$tp1 forSome { ..$defs1 }") =>
          // existential type
          traverse(tp0, tp1)
          for ((d0, d1) <- defs0 zip defs1) traverse(d0, d1)
        case (tq"(..$tps0)", tq"(..$tps1)") if tps0.length > 1 =>
          // tuple type
          for ((tp0, tp1) <- tps0 zip tps1) traverse(tp0, tp1)
        case (tq"(..$tps0) => $rt0", tq"(..$tps1) => $rt1") =>
          // function type
          for ((tp0, tp1) <- tps0 zip tps1) traverse(tp0, tp1)
          traverse(rt0, rt1)
        case (pq"_", pq"_") =>
          // wildcard pattern
        case (pq"$n0 @ $p0", pq"$n1 @ $p1") =>
          // binding pattern
          traverse(p0, p1)
        case (pq"$e0(..$pats0)", pq"$e1(..$pats1)") =>
          // extractor pattern
          traverse(e0, e1)
          for ((p0, p1) <- pats0 zip pats1) traverse(p0, p1)
        case (pq"$p0: $tp0", pq"$p1: $tp1") =>
          // type pattern
          traverse(tp0, tp1)
        case (pq"$first0 | ..$rest0", pq"$first1 | ..$rest1") =>
          // alternative pattern
          traverse(first0, first1)
          for ((r0, r1) <- rest0 zip rest1) traverse(r0, r1)
        case (pq"(..$pats0)", pq"(..$pats1)") if pats0.length > 1 =>
          // tuple pattern
          for ((p0, p1) <- pats0 zip pats1) traverse(p0, p1)
        case (q"$_ val $_: $tp0 = $rhs0", q"$_ val $_: $tp1 = $rhs1") =>
          // val
          traverse(tp0, tp1)
          traverse(rhs0, rhs1)
        case (q"$_ var $_: $tp0 = $rhs0", q"$_ var $_: $tp1 = $rhs1") =>
          // var
          traverse(tp0, tp1)
          traverse(rhs0, rhs1)
        case (
          q"$_ def $_[..$tps0](...$pss0): $tp0 = $b0",
          q"$_ def $_[..$tps1](...$pss1): $tp1 = $b1"
        ) =>
          // method
          for ((tp0, tp1) <- tps0 zip tps1) traverse(tp0, tp1)
          for ((ps0, ps1) <- pss0 zip pss1; (p0, p1) <- ps0 zip ps1) traverse(p0, p1)
          traverse(tp0, tp1)
          traverse(b0, b1)
        case (
          q"$_ def this(...$pss0) = this(..$as0)",
          q"$_ def this(...$pss1) = this(..$as1)"
        ) =>
          // secondary constructor
          for ((ps0, ps1) <- pss0 zip pss1; (p0, p1) <- ps0 zip ps1) traverse(p0, p1)
          for ((a0, a1) <- as0 zip as1) traverse(a0, a1)
        case (q"$_ type $_[..$tps0] = $tp0", q"$_ type $_[..$tps1] = $tp1") =>
          // type
          for ((tp0, tp1) <- tps0 zip tps1) traverse(tp0, tp1)
          traverse(tp0, tp1)
        case (
          q"""
            $_ class $_[..$tps0] $_(...$pss0)
            extends { ..$eds0 } with ..$ps0 { $self0 => ..$ss0 }
          """,
          q"""
            $_ class $_[..$tps1] $_(...$pss1)
            extends { ..$eds1 } with ..$ps1 { $self1 => ..$ss1 }
          """
        ) =>
          // class
          for ((tp0, tp1) <- tps0 zip tps1) traverse(tp0, tp1)
          for ((ps0, ps1) <- pss0 zip pss1; (p0, p1) <- ps0 zip ps1) traverse(p0, p1)
          for ((e0, e1) <- eds0 zip eds1) traverse(e0, e1)
          for ((p0, p1) <- ps0 zip ps1) traverse(p0, p1)
          traverse(self0, self1)
          for ((a, b) <- ss0 zip ss1) traverse(a, b)
        case (
          q"""
            $_ trait $_[..$tps0] extends { ..$eds0 } with ..$ps0 { $self0 => ..$ss0 }
          """,
          q"""
            $_ trait $_[..$tps1] extends { ..$eds1 } with ..$ps1 { $self1 => ..$ss1 }
          """
        ) =>
          // trait
          for ((tp0, tp1) <- tps0 zip tps1) traverse(tp0, tp1)
          for ((e0, e1) <- eds0 zip eds1) traverse(e0, e1)
          for ((p0, p1) <- ps0 zip ps1) traverse(p0, p1)
          traverse(self0, self1)
          for ((a, b) <- ss0 zip ss1) traverse(a, b)
        case (
          q"""
            $_ object $_ extends { ..$eds0 } with ..$ps0 { $self0 => ..$ss0 }
          """,
          q"""
            $_ object $_ extends { ..$eds1 } with ..$ps1 { $self1 => ..$ss1 }
          """
        ) =>
          // object
          for ((e0, e1) <- eds0 zip eds1) traverse(e0, e1)
          for ((p0, p1) <- ps0 zip ps1) traverse(p0, p1)
          traverse(self0, self1)
          for ((a, b) <- ss0 zip ss1) traverse(a, b)
        case (
          q"""
            package object $_ extends { ..$eds0 } with ..$ps0 { $self0 => ..$ss0 }
          """,
          q"""
            package object $_ extends { ..$eds1 } with ..$ps1 { $self1 => ..$ss1 }
          """
        ) =>
          // package object
          for ((e0, e1) <- eds0 zip eds1) traverse(e0, e1)
          for ((p0, p1) <- ps0 zip ps1) traverse(p0, p1)
          traverse(self0, self1)
          for ((a, b) <- ss0 zip ss1) traverse(a, b)
        case (q"package $r0 { ..$tops0 }", q"package $r1 { ..$tops1 }") =>
          // package
          traverse(r0, r1)
          for ((t0, t1) <- tops0 zip tops1) traverse(t0, t1)
        case (q"import $r0.{..$ss0}", q"import $r1.{..$ss1}") =>
          // import
          traverse(r0, r1)
          for ((s0, s1) <- ss0 zip ss1) traverse(s0, s1)
        case (cq"$p0 if $c0 => $b0", cq"$p1 if $c1 => $b1") =>
          // case clause
          traverse(p0, p1)
          traverse(c0, c1)
          traverse(b0, b1)
        case (fq"$p0 <- $e0", fq"$p1 <- $e1") =>
          // generator enumerator
          traverse(e0, e1)
          traverse(p0, p1)
        case (fq"$p0 = $e0", fq"$p1 = $e1") =>
          // generator value definition
          traverse(e0, e1)
          traverse(p0, p1)
        case (fq"if $e0", fq"if $e1") =>
          // guard enumerator
          traverse(e0, e1)
        case _ =>
          // identifier
          // literal
          // literal pattern
          // type identifier
      }
    }
    traverse(t0, t1)
  }
}
