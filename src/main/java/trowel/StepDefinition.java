package trowel;

import java.nio.file.Path;
import java.util.regex.Pattern;

public final class StepDefinition {
	private final String regex;

	private final Path file;

	private final int lineNumber;

	private final Pattern pattern;

	public StepDefinition(final String aRegex, final Path aFile, final int aLineNumber) {
		regex = aRegex;
		pattern = Pattern.compile(regex); /* FIXME Can fail. */
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
