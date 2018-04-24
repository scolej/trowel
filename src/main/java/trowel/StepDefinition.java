package trowel;

import java.nio.file.Path;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

public final class StepDefinition {
  public static Optional<StepDefinition> of(
      final String aRegex, final Path aFile, final int aLineNumber) {
    try {
      return Optional.of(new StepDefinition(aRegex, aFile, aLineNumber));
    } catch (final PatternSyntaxException e) {
      e.printStackTrace();
      return Optional.empty();
    }
  }

  private final String regex;

  private final Path file;

  private final int lineNumber;

  private final Pattern pattern;

  private StepDefinition(final String aRegex, final Path aFile, final int aLineNumber) {
    regex = aRegex;
    pattern = Pattern.compile(regex);
    file = aFile;
    lineNumber = aLineNumber;
  }

  public Path file() {
    return file;
  }

  public int lineNumber() {
    return lineNumber;
  }

  public Pattern pattern() {
    return pattern;
  }

  public String regex() {
    return regex;
  }
}
