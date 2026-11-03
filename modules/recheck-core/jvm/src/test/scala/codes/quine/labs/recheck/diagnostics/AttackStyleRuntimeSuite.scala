package codes.quine.labs.recheck.diagnostics

import java.nio.file.Path
import scala.concurrent.duration.*

import codes.quine.labs.recheck.RuntimeHelper
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

/** Evaluates the attack strings written in each style on the runtimes. */
class AttackStyleRuntimeSuite extends munit.FunSuite with RuntimeHelper:

  override val munitTimeout: Duration = 120.seconds

  def attack(pumps: Seq[(String, String, Int)], suffix: String, n: Int): AttackPattern =
    val ps = pumps.map((s, t, m) => (UnitMode.CodePoint.decode(s), UnitMode.CodePoint.decode(t), m))
    AttackPattern(ps, UnitMode.CodePoint.decode(suffix), n)

  /** Attack strings of code points. */
  val texts: Seq[AttackPattern] = Seq(
    attack(Seq(("", "a", 0)), "\u0000", 31),
    attack(Seq(("'\"\\`", "#{x}#$y#@z$@{$a}", 0)), "\t\n\r\u000b\u000c", 2),
    attack(Seq(("", "\u0001\u0008\u001b\u007f\u0085   ​﻿", 0)), "1", 3),
    attack(Seq(("éあ", "́", 0)), "😀Ω", 2),
    attack(Seq(("a\u00a0b", "\u3000", 0)), "\u2000\u202f\u205f\u1680", 2),
    attack(Seq(("", "😀", 0)), "", 2),
    attack(Seq(("x", "\ud800", 0)), "-\udfff\udbff", 3),
    attack(Seq(("p", "ab", 1), ("q", "c", 0)), "0", 2),
    attack(Seq(("\\u0022\\u000a", "\\x41\\101", 0)), "%s{0}\\", 2),
    attack(Seq(("", "a", -5)), "", 2),
    AttackPattern(Seq.empty, UString.empty, 0)
  )

  /** Attack strings of UTF-16 code units, which encode [[texts]]. */
  val utf16Texts: Seq[AttackPattern] = texts.map: a =>
    def encode(s: UString): UString = UnitMode.UTF16.fromCodePoints(s)

    AttackPattern(a.pumps.map((s, t, m) => (encode(s), encode(t), m)), encode(a.suffix), a.n)

  /** Attack strings of bytes. */
  val bytes: Seq[AttackPattern] = Seq(
    AttackPattern(Seq((UString.empty, UString('a'), 0)), UString(0), 31),
    AttackPattern(Seq((UString.from("'\"\\$@#{".map(_.toInt)), UString(0x00, 0x80, 0xff), 1)), UString(0x7f, 0xc3), 3),
    AttackPattern(Seq.empty, UString.empty, 0)
  )

  def units(a: AttackPattern): Seq[Int] = a.asUString.iterator.toSeq

  def utf16(a: AttackPattern): Seq[Int] = UnitMode.UTF16.fromCodePoints(a.asUString).iterator.toSeq

  def utf8(a: AttackPattern): Seq[Int] = UnitMode.Byte.fromCodePoints(a.asUString).iterator.toSeq

  def hex(values: Seq[Int]): String = values.map(Integer.toHexString).mkString(" ")

  /** Runs the command with the input, and returns the lines of the output marked by `case:`. */
  def cases(command: Seq[String], input: String = "", dir: Option[Path] = None): Seq[String] =
    run(command, input, dir).linesIterator.collect { case s"case:$line" => line }.toSeq

  def check(attacks: Seq[AttackPattern], style: AttackPatternStyle, actual: Seq[String])(
      expected: AttackPattern => String
  )(using munit.Location): Unit =
    assertEquals(actual.size, attacks.size)
    for (a, line) <- attacks.zip(actual) do assertEquals(line, expected(a), clue(a.toString(style)))

  def javascript(attacks: Seq[AttackPattern], style: AttackPatternStyle): Seq[String] =
    val script =
      s"""|for (const s of [${attacks.map(_.toString(style)).mkString(", ")}]) {
          |  console.log('case:' + Array.from({ length: s.length }, (_, i) => s.charCodeAt(i).toString(16)).join(' '));
          |}
          |""".stripMargin
    cases(Seq("node", "-"), script)

  runtimeTest("JavaScript: unicode", "node"):
    val style = AttackPatternStyle.JavaScript(unicode = true)
    check(texts, style, javascript(texts, style))(a => hex(utf16(a)))

  runtimeTest("JavaScript: UTF-16", "node"):
    val style = AttackPatternStyle.JavaScript(unicode = false)
    check(utf16Texts, style, javascript(utf16Texts, style))(a => hex(units(a)))

  def python(attacks: Seq[AttackPattern], style: AttackPatternStyle): Seq[String] =
    val script =
      s"""|for s in [${attacks.map(_.toString(style)).mkString(", ")}]:
          |    print('case:' + ' '.join('%x' % (c if isinstance(c, int) else ord(c)) for c in s))
          |""".stripMargin
    cases(Seq("python3", "-I", "-"), script)

  runtimeTest("Python: str", "python3"):
    val style = AttackPatternStyle.Python(bytes = false)
    check(texts, style, python(texts, style))(a => hex(units(a)))

  runtimeTest("Python: bytes", "python3"):
    val style = AttackPatternStyle.Python(bytes = true)
    check(bytes, style, python(bytes, style))(a => hex(units(a)))

  def ruby(attacks: Seq[AttackPattern], style: AttackPatternStyle): Seq[String] =
    val script =
      s"""|[${attacks.map(_.toString(style)).mkString(", ")}].each do |s|
          |  puts "case:#{s.encoding} " + s.bytes.map { |b| b.to_s(16) }.join(' ')
          |end
          |""".stripMargin
    cases(Seq("ruby", "-"), script)

  runtimeTest("Ruby: UTF-8", "ruby"):
    val style = AttackPatternStyle.Ruby(bytes = false)
    check(texts, style, ruby(texts, style))(a => s"UTF-8 ${hex(utf8(a))}")

  runtimeTest("Ruby: ASCII-8BIT", "ruby"):
    val style = AttackPatternStyle.Ruby(bytes = true)
    check(bytes, style, ruby(bytes, style))(a => s"ASCII-8BIT ${hex(units(a))}")

  runtimeTest("Java", "java"):
    val style = AttackPatternStyle.Java
    val program =
      s"""|class Main {
          |  public static void main(String[] args) {
          |    String[] cases = { ${texts.map(_.toString(style)).mkString(", ")} };
          |    for (String s : cases) {
          |      StringBuilder b = new StringBuilder("case:");
          |      for (int i = 0; i < s.length(); i++) b.append(i == 0 ? "" : " ").append(Integer.toHexString(s.charAt(i)));
          |      System.out.println(b);
          |    }
          |  }
          |}
          |""".stripMargin
    val path = createFile("recheck-style", "Main.java", program)
    check(texts, style, cases(Seq("java", path.toString)))(a => hex(utf16(a)))

  runtimeTest("C#", "dotnet"):
    val style = AttackPatternStyle.CSharp
    val program =
      s"""|using System;
          |using System.Linq;
          |
          |string[] cases = { ${utf16Texts.map(_.toString(style)).mkString(", ")} };
          |foreach (var s in cases) Console.WriteLine("case:" + string.Join(" ", s.Select(c => ((int)c).ToString("x"))));
          |""".stripMargin
    val path = createFile("recheck-style", "Main.cs", program)
    check(utf16Texts, style, cases(Seq("dotnet", "run", path.getFileName.toString), dir = Some(path.getParent)))(a =>
      hex(units(a))
    )

  def perl(attacks: Seq[AttackPattern]): Seq[String] =
    val script =
      s"""|for my $$s (${attacks.map(_.toString(AttackPatternStyle.Perl)).mkString(", ")}) {
          |  print 'case:' . join(' ', map { sprintf '%x', ord } split //, $$s) . "\\n";
          |}
          |""".stripMargin
    cases(Seq("perl", "-"), script)

  runtimeTest("Perl: characters", "perl"):
    check(texts, AttackPatternStyle.Perl, perl(texts))(a => hex(units(a)))

  runtimeTest("Perl: bytes", "perl"):
    check(bytes, AttackPatternStyle.Perl, perl(bytes))(a => hex(units(a)))

  def php(attacks: Seq[AttackPattern], style: AttackPatternStyle): Seq[String] =
    val script =
      s"""|<?php
          |foreach ([${attacks.map(_.toString(style)).mkString(", ")}] as $$s) {
          |  echo 'case:', implode(' ', array_map('dechex', array_values(unpack('C*', $$s) ?: []))), "\\n";
          |}
          |""".stripMargin
    cases(Seq("php"), script)

  runtimeTest("PHP: UTF-8", "php"):
    val style = AttackPatternStyle.PHP(bytes = false)
    check(texts, style, php(texts, style))(a => hex(utf8(a)))

  runtimeTest("PHP: bytes", "php"):
    val style = AttackPatternStyle.PHP(bytes = true)
    check(bytes, style, php(bytes, style))(a => hex(units(a)))
