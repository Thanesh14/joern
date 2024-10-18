package io.joern.console.cpgcreation

import better.files.File
import io.joern.console.workspacehandling.Project
import io.joern.console.{ConsoleException, FrontendConfig, Reporting}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import flatgraph.help.Table
import flatgraph.help.Table.AvailableWidthProvider

import java.nio.file.Path
import scala.util.{Failure, Success, Try}

class ImportCode[T <: Project](console: io.joern.console.Console[T])(implicit
  availableWidthProvider: AvailableWidthProvider
) extends Reporting {
  import io.joern.console.Console.*

  private val config = console.config
  private val workspace = console.workspace
  protected val generatorFactory = new CpgGeneratorFactory(config)

  /** Validates the input path, ensuring it exists and is not a directory unless allowed */
  private def checkInputPath(inputPath: String, allowDirectory: Boolean = false): Unit = {
    val file = File(inputPath)
    if (!file.exists) throw new ConsoleException(s"Input path does not exist: '$inputPath'")
    if (file.isDirectory && !allowDirectory) throw new ConsoleException(s"Input path is a directory: '$inputPath'")
  }

  /** Main method to import code and generate the CPG, selecting frontend by language or input path content */
  def apply(inputPath: String, projectName: String = "", language: String = ""): Cpg = {
    checkInputPath(inputPath, allowDirectory = true)
    val frontendOpt = if (language.nonEmpty) {
      generatorFactory.forLanguage(language)
    } else {
      generatorFactory.forCodeAt(inputPath)
    }

    frontendOpt match {
      case Some(frontend) => apply(frontend, inputPath, projectName)
      case None => throw new ConsoleException(s"No suitable CPG generator found for: $inputPath with language: $language")
    }
  }

  // Frontend definitions for supported languages
  def c: SourceBasedFrontend = new CFrontend("c")
  def cpp: SourceBasedFrontend = new CFrontend("cpp", extension = "cpp")
  def java: SourceBasedFrontend = new SourceBasedFrontend("java", Languages.JAVASRC, "Java Source Frontend", "java")
  def python: SourceBasedFrontend = new SourceBasedFrontend("python", Languages.PYTHONSRC, "Python Source Frontend", "py")
  def javascript: SourceBasedFrontend = new JsFrontend("javascript", Languages.JAVASCRIPT, "Javascript Frontend", "js")
  def php: SourceBasedFrontend = new SourceBasedFrontend("php", Languages.PHP, "PHP Source Frontend", "php")

  private def allFrontends: List[Frontend] = List(c, cpp, java, python, javascript, php)

  // Abstract class for all frontends
  abstract class Frontend(val name: String, val language: String, val description: String = "")(implicit
    availableWidthProvider: AvailableWidthProvider
  ) {
    def cpgGeneratorForLanguage(language: String, config: FrontendConfig, rootPath: Path, args: List[String]): Option[CpgGenerator] =
      io.joern.console.cpgcreation.cpgGeneratorForLanguage(language, config, rootPath, args)

    def isAvailable: Boolean =
      cpgGeneratorForLanguage(language, config.frontend, config.install.rootPath.path, args = Nil).exists(_.isAvailable)

    protected def fromFile(inputPath: String, projectName: String = "", args: List[String] = List()): Cpg = {
      checkInputPath(inputPath)
      withFileInTmpFile(inputPath) { dir =>
        this.apply(dir.pathAsString, projectName, args)
      }.getOrElse(throw new ConsoleException(s"Unable to generate CPG from file: $inputPath"))
    }

    def apply(inputPath: String, projectName: String = "", args: List[String] = List()): Cpg = {
      val frontend = cpgGeneratorForLanguage(language, config.frontend, config.install.rootPath.path, args)
        .getOrElse(throw new ConsoleException(s"No CPG generator for language=$language available!"))
      new ImportCode(console).apply(frontend, inputPath, projectName)
    }
  }

  // Frontend for source code
  class SourceBasedFrontend(name: String, language: String, description: String, extension: String)
      extends Frontend(name, language, description) {

    def fromString(str: String, args: List[String] = List()): Cpg = {
      withCodeInTmpFile(str, s"tmp.$extension") { dir =>
        super.apply(dir.pathAsString, args = args)
      }.getOrElse(throw new ConsoleException(s"Unable to generate CPG from given String"))
    }
  }

  /** Creates a temporary directory, writes code into a file, and executes a function with that temporary file */
  private def withCodeInTmpFile(str: String, filename: String)(f: File => Cpg): Try[Cpg] = {
    val dir = File.newTemporaryDirectory("console")
    val result = Try {
      (dir / filename).write(str)
      f(dir)
    }
    dir.deleteOnExit(swallowIOExceptions = true)
    result
  }

  /** Copies the input file to a temporary directory and applies the given function */
  private def withFileInTmpFile(inputPath: String)(f: File => Cpg): Try[Cpg] = {
    val dir = File.newTemporaryDirectory("console")
    val result = Try {
      File(inputPath).copyToDirectory(dir)
      f(dir)
    }
    dir.deleteOnExit(swallowIOExceptions = true)
    result
  }

  /** Apply the CPG generator to create a CPG, associate it with a project, and apply default overlays */
  private def apply(generator: CpgGenerator, inputPath: String, projectName: String): Cpg = {
    checkInputPath(inputPath, allowDirectory = true)
    val name = Option(projectName).filter(_.nonEmpty).getOrElse(deriveNameFromInputPath(inputPath, workspace))
    report(s"Creating project `$name` for code at `$inputPath`")

    workspace.createProject(inputPath, name).flatMap { pathToProject =>
      val frontendCpgOutFile = pathToProject.resolve(nameOfLegacyCpgInProject)
      generatorFactory.runGenerator(generator, inputPath, frontendCpgOutFile.toString) match {
        case Success(_) => console.open(name).flatMap(_.cpg)
        case Failure(exception) => throw new ConsoleException(s"Error creating project for input path: `$inputPath`", exception)
      }
    }.map { cpg =>
      report("Code successfully imported. You can now query it using `cpg`.")
      console.applyDefaultOverlays(cpg)
      generator.applyPostProcessingPasses(cpg)
    }.getOrElse(throw new ConsoleException(s"Error creating project for input path: `$inputPath`"))
  }

  // Method to render the available frontends in a table format
  override def toString: String = {
    val cols = List("name", "description", "available")
    val rows = allFrontends.map { frontend =>
      List(frontend.name, frontend.description, frontend.isAvailable.toString)
    }
    "Type `importCode.<language>` to run a specific language frontend\n" + Table(cols, rows).render
  }
}
