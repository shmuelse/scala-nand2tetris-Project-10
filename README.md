# Jack Analyzer

**A syntax analyzer for the Jack programming language, written in Scala.**

The front end of a Jack compiler, built for [nand2tetris](https://www.nand2tetris.org/) Project 10. Point it at a directory of `.jack` source files and it emits a tokenized stream and a full parse tree for each one, as XML.

---

## What it does

For every `Foo.jack` file in the input directory, the analyzer produces:

| Output | Stage | Contents |
| --- | --- | --- |
| `FooT.xml` | Tokenizer | Flat token stream, each token tagged with its type — `keyword`, `symbol`, `identifier`, `integerConstant`, `stringConstant` |
| `Foo.xml` | Parser | Nested parse tree following the Jack grammar — `<class>`, `<subroutineDec>`, `<letStatement>`, `<expression>`, … |

Both files are written alongside the source and match the reference output format expected by the nand2tetris `TextComparer` tool.

---

## Example

Input — `Main.jack`:

```jack
class Main {
    function void main() {
        do Output.printString("Hello");
        return;
    }
}
```

Output — `MainT.xml` (tokens):

```xml
<tokens>
<keyword> class </keyword>
<identifier> Main </identifier>
<symbol> { </symbol>
<keyword> function </keyword>
<keyword> void </keyword>
<identifier> main </identifier>
...
</tokens>
```

Output — `Main.xml` (parse tree):

```xml
<class>
  <keyword> class </keyword>
  <identifier> Main </identifier>
  <symbol> { </symbol>
  <subroutineDec>
    <keyword> function </keyword>
    <keyword> void </keyword>
    <identifier> main </identifier>
    <symbol> ( </symbol>
    <parameterList>
    </parameterList>
    ...
  </subroutineDec>
</class>
```

---

## How it works

The analyzer runs in two stages, chained through the file system:

```
Foo.jack  ──[ Tokenizer ]──▶  FooT.xml  ──[ Parser ]──▶  Foo.xml
```

**Tokenizer** — splits each source file into tokens with a single delimiter regex, classifies each one, escapes XML entities (`<`, `>`, `&`, `"`, `'`), and writes the token stream.

**Parser** — a recursive-descent parser with one method per grammar rule. It reads the token stream back in, walks it with a shared cursor, and writes an indented parse tree. Nesting depth is tracked as the recursion enters and exits each rule.

### Code structure

Everything lives in a single object, `EX_04`, organized into three inner classes:

| Class | Responsibility |
| --- | --- |
| `HelpFunctions` | Token classification, XML escaping, tag-content extraction, indented writing |
| `Tokenizing` | Source → token stream (`FooT.xml`) |
| `Parsing` | Token stream → parse tree (`Foo.xml`) |

### Grammar coverage

The full Jack grammar as specified by the course:

- `class`, `classVarDec` (`static` / `field`, comma-separated names)
- `subroutineDec` (`constructor` / `function` / `method`), `parameterList`, `subroutineBody`, `varDec`
- Statements: `let` (including array indexing), `if` / `else`, `while`, `do`, `return`
- `expression` / `term` / `expressionList` — parenthesized expressions, unary `-` and `~`, array access, and both call forms (`foo(...)` and `Bar.foo(...)`)

---

## Project layout

```
.
├── src/
│   └── EX_04.scala      # Tokenizer, parser, helpers, entry point
├── ex4.iml              # IntelliJ module definition
└── .idea/               # IntelliJ project settings
```

---

## Requirements

- **Scala 2.13**
- **JDK 8**
- IntelliJ IDEA with the Scala plugin — or a command-line Scala installation

## Build & run

**IntelliJ IDEA** — open the project folder, attach a Scala 2.13 SDK to the `ex4` module (`File → Project Structure → Modules → Dependencies`), and run the `EX_04` object.

**Command line:**

```bash
scalac -d out/production/ex4 src/EX_04.scala
scala -cp out/production/ex4 EX_04
```

## Usage

The program takes no command-line arguments. On start it prompts for the **directory** containing your `.jack` files:

```
Enter file path:
C:\nand2tetris\projects\10\Square
path is:
C:\nand2tetris\projects\10\Square\
```

Every `.jack` file in that directory is processed; anything else is skipped.

---

## Scope

Built to the course specification: inputs are assumed to be well-formed Jack, so the parser reports no syntax errors. Paths currently use the Windows separator. The natural next step is Project 11 — extending the parser into a full code generator that emits VM code instead of XML.

---

## Credits

Course project by Yonatan Friedman, Isaias Mola, and Shmuel Segal.

Based on *The Elements of Computing Systems* (Nisan & Schocken), Project 10.
