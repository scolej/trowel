package trowel;

import java.nio.file.Path;

public final class StepDefinition {
	private final String regex;

	private final Path file;

	private final int lineNumber;

	public StepDefinition(final String aRegex, final Path aFile, final int aLineNumber) {
		regex = aRegex;
		file = aFile;
		lineNumber = aLineNumber;
	}

	public Path file() {
		return file;
	}

	public String getRegex() {
		return regex;
	}

	public int lineNumber() {
		return lineNumber;
	}
}
